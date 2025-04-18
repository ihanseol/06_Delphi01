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

unit IBX.IBUpdateSQL;

interface

uses
  System.Classes, Data.DB, IBX.IB, IBX.IBCustomDataSet, IBX.IBQuery;

type
{ TIBUpdateSQL }

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TIBUpdateSQL = class(TIBDataSetUpdateObject)
  private
    FDataSet: TIBCustomDataSet;
    FLiveMode : TLiveModes;
    FQueries: array[TUpdateKind] of TIBQuery;
    FSQLText: array[TUpdateKind] of TStrings;
    function GetQuery(UpdateKind: TUpdateKind): TIBQuery;
    function GetSQLIndex(Index: Integer): TStrings;
    procedure SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
    procedure SetSQLIndex(Index: Integer; Value: TStrings);
  protected
    function GetSQL(UpdateKind: TUpdateKind): TStrings; override;
    function GetDataSet: TIBCustomDataSet; override;
    procedure SetDataSet(ADataSet: TIBCustomDataSet); override;
    procedure SQLChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Apply(UpdateKind: TUpdateKind); override;
    procedure ExecSQL(UpdateKind: TUpdateKind);
    procedure SetParams(UpdateKind: TUpdateKind);
    function GetLiveModes : TLiveModes; override;

    property DataSet;
    property Query[UpdateKind: TUpdateKind]: TIBQuery read GetQuery;
    property SQL[UpdateKind: TUpdateKind]: TStrings read GetSQL write SetSQL;
  published
    property ModifySQL: TStrings index 0 read GetSQLIndex write SetSQLIndex;
    property InsertSQL: TStrings index 1 read GetSQLIndex write SetSQLIndex;
    property DeleteSQL: TStrings index 2 read GetSQLIndex write SetSQLIndex;
  end;

implementation

uses
  System.SysUtils, System.Variants, IBX.IBUtils;

{ TIBUpdateSQL }

constructor TIBUpdateSQL.Create(AOwner: TComponent);
var
  UpdateKind: TUpdateKind;
begin
  inherited Create(AOwner);
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
  begin
    FSQLText[UpdateKind] := TStringList.Create;
    TStringList(FSQLText[UpdateKind]).OnChange := SQLChanged;
  end;
end;

destructor TIBUpdateSQL.Destroy;
var
  UpdateKind: TUpdateKind;
begin
  if Assigned(FDataSet) and (FDataSet.UpdateObject = Self) then
    FDataSet.UpdateObject := nil;
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
    FSQLText[UpdateKind].Free;
  inherited Destroy;
end;

procedure TIBUpdateSQL.ExecSQL(UpdateKind: TUpdateKind);
begin
  Query[UpdateKind].Prepare;
  Query[UpdateKind].ExecSQL;
  if Query[UpdateKind].RowsAffected <> 1 then
    IBError(ibxeUpdateFailed, [nil]);
end;

function TIBUpdateSQL.GetQuery(UpdateKind: TUpdateKind): TIBQuery;
begin
  if not Assigned(FQueries[UpdateKind]) then
  begin
    FQueries[UpdateKind] := TIBQuery.Create(Self);
    FQueries[UpdateKind].SQL.Assign(FSQLText[UpdateKind]);
    if (FDataSet is TIBCustomDataSet) then
    begin
      FQueries[UpdateKind].Database := TIBCustomDataSet(FDataSet).DataBase;
      FQueries[UpdateKind].Transaction := TIBCustomDataSet(FDataSet).Transaction;
    end;
  end;
  Result := FQueries[UpdateKind];
end;

function TIBUpdateSQL.GetSQL(UpdateKind: TUpdateKind): TStrings;
begin
  Result := FSQLText[UpdateKind];
end;

function TIBUpdateSQL.GetSQLIndex(Index: Integer): TStrings;
begin
  Result := FSQLText[TUpdateKind(Index)];
end;

function TIBUpdateSQL.GetDataSet: TIBCustomDataSet;
begin
  Result := FDataSet;
end;

function TIBUpdateSQL.GetLiveModes: TLiveModes;
var
  qry : TIBQuery;

  procedure PrepareSQL(iSQL: TIBQuery; lm: TLiveMode);
  begin
    try
      if Trim(iSQL.SQL.Text) <> '' then
      begin
        if not iSQL.Prepared then
          iSQL.Prepare;
        Include(FLiveMode, lm);
      end;
    except
     on E: Exception do
       if not (E is EIBInterbaseRoleError) then
         Raise;
    end;
  end;

begin
  qry := TIBQuery.Create(FDataset.Database);
  try
    qry.SQL.Assign(RefreshSQL);
    PrepareSQL(qry, lmRefresh);
  finally
    qry.Free;
  end;
  PrepareSQL(Query[ukInsert], lmInsert);
  PrepareSQL(Query[ukModify], lmModify);
  PrepareSQL(Query[ukDelete], lmDelete);
  Result := FLiveMode;
end;

procedure TIBUpdateSQL.SetDataSet(ADataSet: TIBCustomDataSet);
begin
  FDataSet := ADataSet;
end;

procedure TIBUpdateSQL.SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
begin
  FSQLText[UpdateKind].Assign(Value);
end;

procedure TIBUpdateSQL.SetSQLIndex(Index: Integer; Value: TStrings);
begin
  SetSQL(TUpdateKind(Index), Value);
end;

procedure TIBUpdateSQL.SQLChanged(Sender: TObject);
var
  UpdateKind: TUpdateKind;
begin
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
    if Sender = FSQLText[UpdateKind] then
    begin
      if Assigned(FQueries[UpdateKind]) then
      begin
        FQueries[UpdateKind].Params.Clear;
        FQueries[UpdateKind].SQL.Assign(FSQLText[UpdateKind]);
      end;
      Break;
    end;
end;

procedure TIBUpdateSQL.SetParams(UpdateKind: TUpdateKind);
var
  I: Integer;
  Old: Boolean;
  Param: TParam;
  PName: string;
  Field: TField;
  Value: Variant;
  rel, col : String;
begin
  if not Assigned(FDataSet) then
    Exit;
  for I := 0 to Query[UpdateKind].Params.Count - 1 do
  begin
    Param := Query[UpdateKind].Params[I];
    PName := Param.Name;
    Old := PName.ToUpperInvariant.StartsWith('OLD_'); {do not localize}
    if Old then
      PName := PName.Remove(0, 4);
    Field := FDataSet.FindField(PName);
    if not Assigned(Field) then
      Continue;
    if Old then
      Param.AssignFieldValue(Field, Field.OldValue)
    else
    begin
      Value := Field.NewValue;
      if VarIsEmpty(Value) then
        Value := Field.OldValue;
      Param.AssignFieldValue(Field, Value);
    end;
    if Field is TBlobField then
    begin
      SplitToRelCol(FDataset.Database.SQLDialect, Field.Origin, Rel, Col);
      Query[UpdateKind].SetBlobParamOrigin(Param.Name, Rel, Col);
    end;
  end;
end;

procedure TIBUpdateSQL.Apply(UpdateKind: TUpdateKind);
begin
  SetParams(UpdateKind);
  ExecSQL(UpdateKind);
end;

end.
