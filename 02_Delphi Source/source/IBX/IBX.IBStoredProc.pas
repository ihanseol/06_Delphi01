{$A8} {$R-}
{*************************************************************}
{                                                             }
{       Embarcadero Delphi Visual Component Library           }
{       InterBase Express core components                     }
{                                                             }
{       Copyright (c) 1998-2024 Embarcadero Technologies, Inc.}
{              All rights reserved                            }
{                                                             }
{    InterBase Express is based in part on the product        }
{    Free IB Components, written by Gregory H. Deatz for      }
{    Hoagland, Longo, Moran, Dunst & Doukas Company.          }
{    Free IB Components is used under license.                }
{                                                             }
{    Additional code created by Jeff Overcash and used        }
{    with permission.                                         }
{*************************************************************}

unit IBX.IBStoredProc;

interface

uses System.Classes, Data.DB, IBX.IB, IBX.IBDatabase, IBX.IBCustomDataSet,
     IBX.IBHeader;

{ TIBStoredProc }
type

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TIBStoredProc = class(TIBCustomDataSet)
  private
    FStmtHandle: TISC_STMT_HANDLE;
    FProcName: string;
    FParams: TParams;
    FPrepared: Boolean;
    FNameList: TStrings;
    procedure SetParamsList(Value: TParams);
    procedure FreeStatement;
    function GetStoredProcedureNames: TStrings;
    procedure GetStoredProcedureNamesFromServer;
    procedure CreateParamDesc;
    procedure SetParams;
    procedure SetParamsFromCursor;
    procedure GenerateSQL;
    procedure FetchDataIntoOutputParams;
    procedure ReadParamData(Reader: TReader);
    procedure WriteParamData(Writer: TWriter);
    function GetParams: TParams;

  protected
    { IProviderSupport }
    procedure PSExecute; override;
    function PSGetTableName: String; override;
    function PSGetParams: TParams; override;
    procedure PSSetCommandText(const CommandText: string); override;
    procedure PSSetParams(AParams: TParams); override;
    function PSGetCommandText: String; override;
    function PSGetCommandType: TPSCommandType; override;

    procedure DefineProperties(Filer: TFiler); override;
    procedure SetFiltered(Value: Boolean); override;
    function GetParamsCount: Word;
    procedure SetPrepared(Value: Boolean);
    procedure SetPrepare(Value: Boolean);
    procedure SetProcName(Value: string);
    procedure Disconnect; override;
    procedure InternalOpen; override;
    function GetParamsClass: TParamsClass; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopyParams(Value: TParams);
    procedure ExecProc;
    function ParamByName(const Value: string): TParam;
    procedure Prepare;
    procedure UnPrepare;
    property ParamCount: Word read GetParamsCount;
    property StmtHandle: TISC_STMT_HANDLE read FStmtHandle;
    property Prepared: Boolean read FPrepared write SetPrepare;
    property StoredProcedureNames: TStrings read GetStoredProcedureNames;

  published
    property StoredProcName: string read FProcName write SetProcName;
    property Params: TParams read GetParams write SetParamsList stored false;
    property Filtered;

    property BeforeDatabaseDisconnect;
    property AfterDatabaseDisconnect;
    property DatabaseFree;
    property BeforeTransactionEnd;
    property AfterTransactionEnd;
    property TransactionFree;
    property OnFilterRecord;
  end;

implementation

uses
  System.SysUtils, IBX.IBUtils, IBX.IBSQL, IBX.IBQuery;

{ TIBStoredProc }

constructor TIBStoredProc.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParams := TIBXParams.Create(self);
  FNameList := TStringList.Create;
end;

destructor TIBStoredProc.Destroy;
begin
  Destroying;
  Disconnect;
  FParams.Free;
  FNameList.Free;
  inherited Destroy;
end;

procedure TIBStoredProc.Disconnect;
begin
  Close;
  UnPrepare;
end;

procedure TIBStoredProc.ExecProc;
var
  DidActivate: Boolean;
begin
  CheckInActive;
  if StoredProcName = '' then
    IBError(ibxeNoStoredProcName, [nil]);
  ActivateConnection;
  DidActivate := ActivateTransaction;
  try
    SetPrepared(True);
    if DataSource <> nil then
      SetParamsFromCursor;
    if FParams.Count > 0 then
      SetParams;
    InternalExecQuery;
    FetchDataIntoOutputParams;
    SetPrepared(false);  // Unprepare the statement due to a bug in GDS32
  finally
    if DidActivate then
      DeactivateTransaction;
  end;
end;

procedure TIBStoredProc.SetProcName(Value: string);
begin
  if not (csReading in ComponentState) then
  begin
    CheckInactive;
    if Value <> FProcName then
    begin
      FProcName := Value;
      FreeStatement;
      FParams.Clear;
      if (Value <> '') and
         (Database <> nil) then
        GenerateSQL;
    end;
  end
  else
  begin
    FProcName := Value;
    if (Value <> '') and (Database <> nil) then
      GenerateSQL;
  end;
end;

function TIBStoredProc.GetParams: TParams;
begin
  if (FParams.Count = 0)  then
    FParams.ParseSQL(SelectSQL.Text, True);
  Result := FParams;
end;

function TIBStoredProc.GetParamsClass: TParamsClass;
begin
  Result := DefaultIBXParamsClass;
end;

function TIBStoredProc.GetParamsCount: Word;
begin
  if (not (csDesigning in ComponentState)) and
     (FProcName <> '') and (Database <> nil) and (not Prepared) then
    SetPrepared(true);
  Result := FParams.Count;
end;

procedure TIBStoredProc.SetFiltered(Value: Boolean);
begin
  if(Filtered <> Value) then
  begin
    inherited SetFiltered(value);
    if Active then
    begin
      Close;
      Open;
    end;
  end
  else
    inherited SetFiltered(value);
end;

procedure TIBStoredProc.GenerateSQL;
var
  Query : TIBDataset;
  input : string;
begin
  ActivateConnection;
  Query := nil;
  try
    Query := TIBDataset.Create(self);
    Query.Database := DataBase;
    Query.Transaction := Database.PrecommittedTransaction;
    Query.SelectSQL.Text := 'SELECT RDB$PARAMETER_NAME,  RDB$PARAMETER_TYPE ' + {do not localize}
                       'FROM RDB$PROCEDURE_PARAMETERS ' + {do not localize}
                       'WHERE RDB$PROCEDURE_NAME = ' + {do not localize}
                       '''' + FormatIdentifierValue(Database.SQLDialect,
                               QuoteIdentifier(Database.SQLDialect, FProcName)) + '''' +
                       ' ORDER BY RDB$PARAMETER_NUMBER'; {do not localize}
    Query.Open;
    while (not Query.EOF) do
    begin
      if (Query.FieldByName('RDB$PARAMETER_TYPE').AsInteger = 0) then {do not localize}
      begin
        if (input <> '') then
          input := input + ', :' +
            QuoteIdentifier(Database.SQLDialect, Query.FieldByName('RDB$PARAMETER_NAME').AsString.Trim) {do not localize}
        else
          input := ':' +
            QuoteIdentifier(Database.SQLDialect, Query.FieldByName('RDB$PARAMETER_NAME').AsString.Trim); {do not localize}
      end;
      Query.Next;
    end;
    if Input <> '' then
      SelectSQL.Text := 'Execute Procedure ' + {do not localize}
          QuoteIdentifier(Database.SQLDialect, FProcName) + '(' + input + ')'
    else
      SelectSQL.Text := 'Execute Procedure ' + {do not localize}
          QuoteIdentifier(Database.SQLDialect, FProcName);
  finally
    Query.Free;
  end;
end;

procedure TIBStoredProc.CreateParamDesc;
var
  i : integer;
  DataType : TFieldType;
begin
  DataType := ftUnknown;
  for i := 0 to QSelect.Current.Count - 1 do
  begin
    case QSelect.Fields[i].SQLtype of
      SQL_TYPE_DATE: DataType := ftDate;
      SQL_TYPE_TIME: DataType := ftTime;
      SQL_TIMESTAMP: DataType := ftDateTime;
      SQL_SHORT:
        if (QSelect.Fields[i].SQLVar.sqlscale = 0) then
          DataType := ftSmallInt
        else
          DataType := ftBCD;
      SQL_LONG:
        if (QSelect.Fields[i].SQLVar.sqlscale = 0) then
          DataType := ftInteger
        else if (QSelect.Fields[i].SQLVar.sqlscale >= (-4)) then
          DataType := ftBCD
        else
          DataType := ftFloat;
      SQL_INT64:
        if (QSelect.Fields[i].SQLVar.sqlscale = 0) then
          DataType := ftLargeInt
        else if (QSelect.Fields[i].SQLVar.sqlscale >= (-4)) then
          DataType := ftBCD
        else
          DataType := ftFloat;
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT: DataType := ftFloat;
      SQL_TEXT, SQL_VARYING :
      begin
        if ((QSelect.Fields[i].SQLVar.SQLSubtype and $FF) = 1) then
        begin
          if (QSelect.Fields[i].Data.sqltype and not 1) = SQL_VARYING then
            DataType := ftVarBytes
          else
            DataType := ftBytes;
        end
        else
         DataType := ftWideString;
      end;
      SQL_BLOB, SQL_ARRAY, SQL_QUAD: DataType := ftBlob;
      SQL_BOOLEAN : DataType := ftBoolean;
    end;
    FParams.CreateParam(DataType, Trim(QSelect.Fields[i].Name), ptOutput);
  end;

  DataType := ftUnknown;
  for i := 0 to QSelect.Params.Count - 1 do
  begin
    case QSelect.Params[i].SQLtype of
      SQL_TYPE_DATE: DataType := ftDate;
      SQL_TYPE_TIME: DataType := ftTime;
      SQL_TIMESTAMP: DataType := ftDateTime;
      SQL_SHORT:
        if (QSelect.Params[i].SQLVar.sqlscale = 0) then
          DataType := ftSmallInt
        else
          DataType := ftBCD;
      SQL_LONG:
        if (QSelect.Params[i].SQLVar.sqlscale = 0) then
          DataType := ftInteger
        else if (QSelect.Params[i].SQLVar.sqlscale >= (-4)) then
          DataType := ftBCD
        else DataType := ftFloat;
      SQL_INT64:
        if (QSelect.Params[i].SQLVar.sqlscale = 0) then
          DataType := ftLargeInt
        else if (QSelect.Params[i].SQLVar.sqlscale >= (-4)) then
          DataType := ftBCD
        else DataType := ftFloat;
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT: DataType := ftFloat;
      SQL_TEXT, SQL_VARYING :
      begin
        if ((QSelect.Params[i].SQLVar.SQLSubtype and $FF) = 1) then
        begin
          if (QSelect.Params[i].Data.sqltype and not 1) = SQL_VARYING then
            DataType := ftVarBytes
          else
            DataType := ftBytes;
        end
        else
         DataType := ftWideString;
      end;
      SQL_BLOB, SQL_ARRAY, SQL_QUAD: DataType := ftBlob;
      SQL_BOOLEAN : DataType := ftBoolean;
      end;
    FParams.CreateParam(DataType, Trim(QSelect.Params[i].Name), ptInput);
  end;
end;

procedure TIBStoredProc.SetPrepared(Value: Boolean);
begin
  if Prepared <> Value then
  begin
    if Value then
      try
        if SelectSQL.Text = '' then
          GenerateSQL;
        InternalPrepare;
        if FParams.Count = 0 then
          CreateParamDesc;
        FPrepared := True;
      except
        FreeStatement;
        raise;
      end
    else
      FreeStatement;
  end;

end;

procedure TIBStoredProc.Prepare;
begin
  SetPrepared(True);
end;

procedure TIBStoredProc.UnPrepare;
begin
  SetPrepared(False);
end;

procedure TIBStoredProc.FreeStatement;
begin
  InternalUnPrepare;
  QSelect.FreeHandle;
  FPrepared := False;
end;

procedure TIBStoredProc.SetPrepare(Value: Boolean);
begin
  if Value then
    Prepare
  else
    UnPrepare;
end;

procedure TIBStoredProc.CopyParams(Value: TParams);
begin
  if not Prepared and (FParams.Count = 0) then
  try
    Prepare;
    Value.Assign(FParams);
  finally
    UnPrepare;
  end
  else
    Value.Assign(FParams);
end;

procedure TIBStoredProc.SetParamsList(Value: TParams);
begin
  CheckInactive;
  if Prepared then
  begin
    SetPrepared(False);
    FParams.Assign(Value);
    SetPrepared(True);
  end else
    FParams.Assign(Value);
end;

function TIBStoredProc.ParamByName(const Value: string): TParam;
begin
  if not Prepared and (FParams.Count = 0) then
    Prepare;
  Result := FParams.ParamByName(Value);
end;

function TIBStoredProc.GetStoredProcedureNames: TStrings;
begin
  FNameList.clear;
  GetStoredProcedureNamesFromServer;
  Result := FNameList;
end;

procedure TIBStoredProc.GetStoredProcedureNamesFromServer;
var
  Query : TIBSQL;
  InternalTransaction : TIBTransaction;
begin
  if not (csReading in ComponentState) then
  begin
    ActivateConnection;
    Query := nil; 
    InternalTransaction := TIBTransaction.Create(nil);
    try
      InternalTransaction.DefaultDatabase := Database;
      InternalTransaction.SetTransactionLevel(ibtrPrecommitted);
      Query := TIBSQL.Create(self);
      InternalTransaction.StartTransaction;
      Query.GoToFirstRecordOnExecute := False;
      Query.Database := DataBase;
      Query.Transaction := InternalTransaction;
      Query.SQL.Text := 'Select RDB$PROCEDURE_NAME from RDB$PROCEDURES'; {do not localize}
      Query.Prepare;
      Query.ExecQuery;
      while (not Query.EOF) and (Query.Next <> nil) do
        FNameList.Add(TrimRight(Query.Current.ByName('RDB$PROCEDURE_NAME').AsString)); {do not localize}
    finally
      Query.Free;
      InternalTransaction.Commit;
      InternalTransaction.Free;
    end;
  end;
end;

procedure TIBStoredProc.SetParams;
var
  i : integer;
  j : integer;
begin
  i := 0;
  for j := 0 to FParams.Count - 1 do
  begin
    if (Params[j].ParamType <> ptInput) then
      continue;
    if not Params[j].Bound then
      IBError(ibxeRequiredParamNotSet, [nil]);
    // Set the Origin relationaship information is the TParam has it set
    if (Params[j] is TIBXParam) then
    begin
      if TIBXParam(Params[j]).RelationName <> '' then
        SQLParams[i].Data.RelName := TIBXParam(Params[j]).RelationName;
      if TIBXParam(Params[j]).ColumnName <> '' then
        SQLParams[i].Data.SqlName := TIBXParam(Params[j]).ColumnName;
    end;

    if Params[j].IsNull then
      SQLParams[i].IsNull := True
    else
    begin
      SQLParams[i].IsNull := False;
      case Params[j].DataType of
        ftString, ftFixedChar, ftFixedWideChar, ftWideString:
          SQLParams[i].AsString := Params[j].AsString;
        ftShortInt, ftSmallint, ftWord, ftByte:
          SQLParams[i].AsShort := Params[j].AsSmallInt;
        ftInteger:
          SQLParams[i].AsLong := Params[j].AsInteger;
        ftLargeInt:
          SQLParams[i].AsInt64 := Params[j].Value;
        ftFloat, ftCurrency:
         SQLParams[i].AsDouble := Params[j].AsFloat;
        ftBCD:
          SQLParams[i].AsCurrency := Params[j].AsCurrency;
        ftDate:
          SQLParams[i].AsDate := Params[j].AsDateTime;
        ftTime:
          SQLParams[i].AsTime := Params[j].AsDateTime;
        ftDateTime:
          SQLParams[i].AsDateTime := Params[j].AsDateTime;
        ftMemo, ftWideMemo:
          SQLParams[i].AsString := Params[j].AsString;
        ftBlob, ftVarBytes, ftBytes :
          SQLParams[i].AsBytes := Params[j].AsBytes;
        ftBoolean :
          SQLParams[i].AsBoolean := Params[j].AsBoolean;
        else
          IBError(ibxeNotSupported, [nil]);
      end;
    end;
    Inc(i);
  end;
end;

procedure TIBStoredProc.SetParamsFromCursor;
var
  I: Integer;
  DataSet: TDataSet;
begin
  if DataSource <> nil then
  begin
    DataSet := DataSource.DataSet;
    if DataSet <> nil then
    begin
      DataSet.FieldDefs.Update;
      for I := 0 to FParams.Count - 1 do
        if (not FParams[I].Bound) and
           ((FParams[I].ParamType = ptInput) or
            (FParams[I].ParamType = ptInputOutput)) then
          FParams[I].AssignField(DataSet.FieldByName(Name));
    end;
  end;
end;

procedure TIBStoredProc.FetchDataIntoOutputParams;
var
i,j : Integer;
begin
  j := 0;
  for i := 0 to FParams.Count - 1 do
    if Params[I].ParamType = ptOutput then
    begin
       if QSelect.Fields[j].SQLType = SQL_BLOB then
       begin
         if QSelect.Fields[j].Data.SqlSubtype = 1 then
           Params[i].Value := QSelect.Fields[j].AsString
         else
         Params[I].Value := QSelect.Fields[j].AsBytes
       end
       else
         Params[I].Value := QSelect.Fields[j].Value;
       Inc(j);
    end;
end;

procedure TIBStoredProc.InternalOpen;
begin
  IBError(ibxeIsAExecuteProcedure,[nil]);
end;

procedure TIBStoredProc.DefineProperties(Filer: TFiler);

  function WriteData: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not FParams.IsEqual(TIBStoredProc(Filer.Ancestor).FParams) else
      Result := FParams.Count > 0;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData); {do not localize}
end;

procedure TIBStoredProc.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;

procedure TIBStoredProc.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(Params);
end;

{ TIBStoredProc IProviderSupport }

function TIBStoredProc.PSGetParams: TParams;
begin
  Result := Params;
end;

procedure TIBStoredProc.PSSetParams(AParams: TParams);
begin
  if AParams.Count > 0 then
    Params.Assign(AParams);
  Close;
end;

function TIBStoredProc.PSGetTableName: String;
begin
  { ! }
end;

procedure TIBStoredProc.PSExecute;
begin
  ExecProc;
end;

procedure TIBStoredProc.PSSetCommandText(const CommandText: string);
begin
  if CommandText <> '' then
    StoredProcName := CommandText;
end;

function TIBStoredProc.PSGetCommandText: String;
begin
  Result := StoredProcName;
end;

function TIBStoredProc.PSGetCommandType: TPSCommandType;
begin
  Result := ctStoredProc;
end;

end.
