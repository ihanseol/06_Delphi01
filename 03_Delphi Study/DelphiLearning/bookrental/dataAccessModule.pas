unit dataAccessModule;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.IB,
  FireDAC.Phys.IBDef, FireDAC.VCLUI.Wait, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client;

type
  TdmDataAccess = class(TDataModule)
    conBookRental: TFDConnection;
    qryBook: TFDQuery;
    qryBookBOOK_SEQ: TIntegerField;
    qryBookBOOK_TITLE: TWideStringField;
    qryBookBOOK_ISBN: TStringField;
    qryBookBOOK_AUTHOR: TWideStringField;
    qryBookBOOK_PRICE: TIntegerField;
    qryBookBOOK_LINK: TWideStringField;
    qryBookBOOK_RENT_YN: TStringField;
    qryBookBOOK_IMAGE: TBlobField;
    qryBookBOOK_DESCRIPTION: TWideMemoField;
    qryBookBOOK_RENT: TStringField;
    qryBookBOOK_RENT1: TStringField;
    qryDuplicatedBook: TFDQuery;
    qryUser: TFDQuery;
    qryUserUSER_SEQ: TIntegerField;
    qryUserUSER_NAME: TWideStringField;
    qryUserUSER_BIRTH: TDateField;
    qryUserUSER_SEX: TStringField;
    qryUserUSER_PHONE: TStringField;
    qryUserUSER_MAIL: TWideStringField;
    qryUserUSER_IMAGE: TBlobField;
    qryUserUSER_REG_DATE: TDateField;
    qryUserUSER_OUT_YN: TStringField;
    qryUserUSER_OUT_DATE: TDateField;
    qryUserUSER_RENT_COUNT: TIntegerField;
    qryUserUSER_SEX_STR: TStringField;
    qryUserUSER_OUT: TStringField;
    procedure qryBookCalcFields(DataSet: TDataSet);
    procedure qryUserCalcFields(DataSet: TDataSet);
  private
    { Private declarations }
  public
    { Public declarations }
    function DuplicatedISBN(ASeq, AISBN: string): Boolean;
  end;

var
  dmDataAccess: TdmDataAccess;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}

function TdmDataAccess.DuplicatedISBN(ASeq, AISBN: string): Boolean;
begin
  qryDuplicatedBook.close;
  qryDuplicatedBook.ParamByName('ISBN').AsString := AISBN;
  qryDuplicatedBook.open;

  if (qryDuplicatedBook.RecordCount > 0) and
    (qryDuplicatedBook.Fields[0].AsString <> ASeq) then
    Result := True;

end;

// event -> OnCalcFields
procedure TdmDataAccess.qryBookCalcFields(DataSet: TDataSet);
var
  RentYN: string;
begin
  RentYN := qryBook.FieldByName('BOOK_RENT_YN').AsString;
  if RentYN = 'Y' then
  begin
    qryBook.FieldByName('BOOK_RENT').AsString := '대여중';
  end
  else
  begin
    qryBook.FieldByName('BOOK_RENT').AsString := '대여가능';
  end;
end;

procedure TdmDataAccess.qryUserCalcFields(DataSet: TDataSet);
begin
  //
  if qryUser.FieldByName('USER_SEX').AsString = 'M' then
    qryUser.FieldByName('USER_SEX_STR').AsString := '남자'
  else
    qryUser.FieldByName('USER_SEX_STR').AsString := '여자';

  if qryUserUSER_OUT_YN.AsString = 'Y' then
    qryUserUSER_OUT.AsString := '탈퇴'
  else
    qryUserUSER_OUT.AsString := '회원';

end;

end.
