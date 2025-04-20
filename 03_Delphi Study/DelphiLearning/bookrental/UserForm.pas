unit UserForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Data.DB,
  Vcl.Mask, Vcl.DBCtrls, Vcl.Grids, Vcl.DBGrids, Vcl.WinXCalendars, Vcl.Buttons;

type
  TfrmUser = class(TForm)
    pnlHeader: TPanel;
    pnlContent: TPanel;
    lblCaption: TLabel;
    btnAdd: TButton;
    btnClose: TButton;
    pnlGrid: TPanel;
    pnlInput: TPanel;
    Splitter1: TSplitter;
    pnlGridHeader: TPanel;
    dbgList: TDBGrid;
    Label1: TLabel;
    edtSearch: TEdit;
    chkSearchName: TCheckBox;
    chkSearchPhone: TCheckBox;
    Label2: TLabel;
    dbeName: TDBEdit;
    Label3: TLabel;
    dpBirth: TCalendarPicker;
    grpSex: TDBRadioGroup;
    Label4: TLabel;
    SpeedButton1: TSpeedButton;
    dbePhone: TDBEdit;
    Label5: TLabel;
    dbeMail: TDBEdit;
    GroupBox1: TGroupBox;
    btnClearImage: TButton;
    btnLoadImage: TButton;
    imgUser: TImage;
    dlgLoadImage: TOpenDialog;
    btnCancel: TButton;
    btnSave: TButton;
    btnDelete: TButton;
    dbUser: TDataSource;
    procedure btnLoadImageClick(Sender: TObject);
    procedure btnClearImageClick(Sender: TObject);
    procedure dbUserDataChange(Sender: TObject; Field: TField);
    procedure btnCloseClick(Sender: TObject);
    procedure dpBirthCloseUp(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure dbUserStateChange(Sender: TObject);
    procedure dbeNameExit(Sender: TObject);
    procedure edtSearchKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    function DuplicatedUser: Boolean;
    // Ctrl+Shift_C  , code gen for this function and procedure;
    // Ctrl+Shift+Up, Down goto implementation ...
  public
    { Public declarations }

  end;

var
  frmUser: TfrmUser;

implementation

{$R *.dfm}

uses dataAccessModule, CommonFunctions;

procedure TfrmUser.btnAddClick(Sender: TObject);
begin
  //
  dmDataAccess.qryUser.Append;
  dbeName.SetFocus;

end;

procedure TfrmUser.btnCancelClick(Sender: TObject);
begin
  dmDataAccess.qryUser.Cancel;
end;

procedure TfrmUser.btnClearImageClick(Sender: TObject);

var
  Field: TField;
begin
  //
  imgUser.Picture.Assign(nil);
  Field := dmDataAccess.qryUser.FieldByName('USER_IMAGE');
  if dmDataAccess.qryUser.State <> dsEdit then // if current state is not modify
    dmDataAccess.qryUser.Edit;

  Field.Assign(nil);
end;

procedure TfrmUser.btnCloseClick(Sender: TObject);
begin
  close;
end;

procedure TfrmUser.btnDeleteClick(Sender: TObject);
// 회원탈퇴
// 먼저 대여권수가 있으면, 처리
var
  Field: TField;
  RentCount: Integer;
  Name, OutYn, Msg: String;

begin
  RentCount := dmDataAccess.qryUser.FieldByName('USER_RENT_COUNT').AsInteger;
  Name := dmDataAccess.qryUser.FieldByName('USER_NAME').AsString;
  OutYn := dmDataAccess.qryUser.FieldByName('USER_OUT_YN').AsString;

  if OutYn = 'Y' then
  begin
    ShowMessage('이미 탈퇴한 회원입니다. ');
    Exit;
  end;

  if RentCount > 0 then
  begin
    ShowMessage(Format('현재 대여중인 도서가 %d권 있습니다. 반납후 회원탈퇴 가능합니다.', [RentCount]));
  end;

  Msg := Format('정말로 [%s]님의 탈퇴처리 하시겠습니까 ?', [Name]);
  if MessageDlg(Msg, mtInformation, [mbYes, mbNo], 0) = mrNo then
  begin
    Exit;
  end;

  // 탈퇴처리
  if dmDataAccess.qryUser.State <> dsEdit then
  begin
    dmDataAccess.qryUser.Edit;
  end;

  dmDataAccess.qryUser.FieldByName('USER_OUT_YN').AsString := 'Y';
  dmDataAccess.qryUser.FieldByName('USER_OUT_DATE').AsDateTime := now;
  dmDataAccess.qryUser.Post;
  dmDataAccess.qryUser.Refresh;
end;

procedure TfrmUser.btnLoadImageClick(Sender: TObject);
var
  Field: TField;
begin
  if dlgLoadImage.Execute then
  begin
    LoadImageFromFile(imgUser, dlgLoadImage.FileName);

    Field := dmDataAccess.qryUser.FieldByName('USER_IMAGE');
    SaveImageToBlobField(imgUser, Field as TBlobField);
  end;

end;

procedure TfrmUser.btnSaveClick(Sender: TObject);
begin
  // in here , name and date_birth field is mandatory

  if dbeName.Text = '' then
  begin
    ShowMessage('이름을 입력해 주세요 ...');
    dbeName.SetFocus;
    Exit;
  end;

  if dpBirth.IsEmpty then
  begin
    ShowMessage('생년월일을 입력해 주세요 ...');
    dpBirth.SetFocus;
    Exit;
  end;

  dmDataAccess.qryUser.Post;
  dmDataAccess.qryUser.Refresh;
end;

procedure TfrmUser.dbeNameExit(Sender: TObject);
begin
  //
  DuplicatedUser;
end;

procedure TfrmUser.dbUserDataChange(Sender: TObject; Field: TField);
var
  LField: TField;

begin

  if dmDataAccess.qryUser.State = dsEdit then
    Exit;

  LField := dmDataAccess.qryUser.FieldByName('USER_IMAGE');
  LoadImageFromBlobField(imgUser, LField as TBlobField);

  LField := dmDataAccess.qryUser.FieldByName('USER_BIRTH');
  if LField.AsDateTime = 0 then
    dpBirth.IsEmpty := True // dp --> date time picker
  else
    dpBirth.Date := LField.AsDateTime;

end;

procedure TfrmUser.dbUserStateChange(Sender: TObject);
var
  State: TDataSetState;
  OutYn: string;
begin
  //
  State := dmDataAccess.qryUser.State;
  OutYn := dmDataAccess.qryUser.FieldByName('USER_OUT_YN').AsString;

  btnAdd.Enabled := (State = dsBrowse);
  btnSave.Enabled := (State <> dsBrowse);
  btnDelete.Enabled := (State = dsBrowse);
  btnCancel.Enabled := (State <> dsBrowse);
end;

procedure TfrmUser.dpBirthCloseUp(Sender: TObject);
var
  Field: TField;
begin
  Field := dmDataAccess.qryUser.FieldByName('USER_BIRTH');

  if Field.AsDateTime <> dpBirth.Date then
  begin
    if dmDataAccess.qryUser.State = dsBrowse then
    begin
      if dmDataAccess.qryUser.RecNo > 0 then
        dmDataAccess.qryUser.Edit
      else
        dmDataAccess.qryUser.Append;
    end;

    if dpBirth.IsEmpty then
      Field.Assign(nil)
    else
      Field.AsDateTime := dpBirth.Date;
  end;
   DuplicatedUser;
end;

function TfrmUser.DuplicatedUser(): Boolean;
var
  seq: Integer;
  aName: String;
  aBirth: TDateTime;
begin
  seq := dmDataAccess.qryUser.FieldByName('USER_SEQ').AsInteger;
  aName := dmDataAccess.qryUser.FieldByName('USER_NAME').AsString;
  aBirth := dmDataAccess.qryUser.FieldByName('USER_BIRTH').AsDateTime;

  if (aName = '') or (aBirth = 0) then
    Exit;

  if dmDataAccess.DuplicatedUser(seq, aName, aBirth) then
    ShowMessage('이미 등록된 회원입니다. (이름과 생년월일이 중복됩니다.)');

end;

procedure TfrmUser.edtSearchKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
Filter : string;
begin
//
  Filter := '';
  if edtSearch.Text <> '' then
  begin
      if chkSearchName.Checked then
       Filter := Format('USER_NAME LIKE ''%%%S%%'' ',[edtSearch.Text]);
      if chkSearchPhone.Checked then
      begin
        if Filter  <> '' then
             Filter := Filter + ' OR ' ;

         Filter := Filter + Format('USER_PHONE LIKE ''%%%S%%'' ',[edtSearch.Text]);
      end;
  end;

  dmDataAccess.qryUser.Filter := Filter;
  dmDataAccess.qryUser.Filtered :=  (Filter <> '');
end;

end.
