unit BookForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Grids, Vcl.DBGrids,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.DBCtrls, Vcl.Mask;

type
  TfrmBook = class(TForm)
    pnlHeader: TPanel;
    pnlContent: TPanel;
    pnlMain: TPanel;
    pnlInput: TPanel;
    pnlMainHeader: TPanel;
    gridBook: TDBGrid;
    lblCaption: TLabel;
    btnAdd: TButton;
    btnClose: TButton;
    lblSearch: TLabel;
    edtSearch: TEdit;
    chkSearchTitle: TCheckBox;
    chkSearchAuthor: TCheckBox;
    lblTitle: TLabel;
    dbeTitle: TDBEdit;
    lblISBN: TLabel;
    dbeISBN: TDBEdit;
    lblAuthor: TLabel;
    dbeAuthor: TDBEdit;
    Label2: TLabel;
    dbePrice: TDBEdit;
    Label3: TLabel;
    dbeLink: TDBEdit;
    btnImageLoad: TButton;
    GroupBox1: TGroupBox;
    btnImageClear: TButton;
    Label4: TLabel;
    dbmDescription: TDBMemo;
    btnDelete: TButton;
    btnSave: TButton;
    btnCancel: TButton;
    imgBook: TImage;
    Splitter1: TSplitter;
    dsBook: TDataSource;
    dlgLoadImage: TOpenDialog;
    lblLink: TLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure btnImageLoadClick(Sender: TObject);
    procedure btnImageClearClick(Sender: TObject);
    procedure dsBookDataChange(Sender: TObject; Field: TField);
    procedure btnAddClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure dsBookStateChange(Sender: TObject);
    procedure dbeISBNExit(Sender: TObject);
    procedure edtSearchKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lblLinkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmBook: TfrmBook;

implementation

{$R *.dfm}

uses
  dataAccessModule, CommonFunctions, WinAPI.ShellAPI;
  // WinAPI.ShellAPI - for this  ShellExecute(Handle, 'open', PChar(dbeLink.Text), nil, nil, SW_SHOW);

procedure TfrmBook.btnAddClick(Sender: TObject);
begin
  dmDataAccess.qryBook.Append;
  dbeTitle.SetFocus;
  Exit;

end;

procedure TfrmBook.btnCancelClick(Sender: TObject);
begin
  dmDataAccess.qryBook.Cancel;
end;

procedure TfrmBook.btnCloseClick(Sender: TObject);
begin
  close;
end;

procedure TfrmBook.btnDeleteClick(Sender: TObject);
var
  RentYN, Title, strMessage: string;
begin
  RentYN := dmDataAccess.qryBook.FieldByName('BOOK_RENT_YN').AsString;
  if RentYN = 'Y' then
  begin
    ShowMessage('�뿩���ε����� �����Ҽ������ϴ�. �����ݳ��� ������ �����մϴ�. ');
    Exit;
  end;

  Title := dmDataAccess.qryBook.FieldByName('BOOK_TITLE').AsString;
  strMessage := Format('[%s] ������ �����Ͻðڽ��ϱ� ...', [Title]);
  if MessageDlg(strMessage, TMsgDlgType.mtInformation, [mbYes, mbNo], 0) = mrNo
  then
  begin
    Exit;
  end
  else
  begin
    dmDataAccess.qryBook.Delete;
  end;

end;

procedure TfrmBook.btnImageClearClick(Sender: TObject);
var
  Field: TField;
begin
  imgBook.Picture.Assign(nil);
  Field := dmDataAccess.qryBook.FieldByName('BOOK_IMAGE');
  if dmDataAccess.qryBook.State <> dsEdit then // not in state edit mode, dsEdit
    dmDataAccess.qryBook.Edit;
  Field.Assign(nil);
end;

procedure TfrmBook.btnImageLoadClick(Sender: TObject);
var
  Field: TField;
begin
  if dlgLoadImage.Execute then
  begin
    LoadImageFromFile(imgBook, dlgLoadImage.FileName);
    Field := dmDataAccess.qryBook.FieldByName('BOOK_IMAGE');
    SaveImageToBlobField(imgBook, Field as TBlobField)
  end;

end;

procedure TfrmBook.btnSaveClick(Sender: TObject);
begin
  if dbeTitle.Text = '' then
  begin
    ShowMessage('������ �Է��ϼ��� ..');
    dbeTitle.SetFocus;
    Exit;
  end;

  if dbeAuthor.Text = '' then
  begin
    ShowMessage('���ڸ� �Է��ϼ��� ...');
    dbeAuthor.SetFocus;
    Exit;
  end;

  dmDataAccess.qryBook.Post;
  dmDataAccess.qryBook.Refresh;
end;

procedure TfrmBook.dbeISBNExit(Sender: TObject);
var
  Seq, ISBN: string;
begin
  Seq := dmDataAccess.qryBook.FieldByName('BOOK_SEQ').AsString;
  ISBN := dmDataAccess.qryBook.FieldByName('BOOK_ISBN').AsString;

  if dmDataAccess.qryBook.State = dsBrowse then
    Exit;

  if dmDataAccess.DuplicatedISBN(Seq, ISBN) then
  begin
    ShowMessage('�̵̹�ϵ� ISBN�Դϴ�. ');
    dbeISBN.Text := '';
    dbeISBN.SetFocus;
  end;

end;

procedure TfrmBook.dsBookDataChange(Sender: TObject; Field: TField);
var
  LField: TField;
begin

  if dmDataAccess.qryBook.State = dsEdit then
    Exit;

  LField := dmDataAccess.qryBook.FieldByName('BOOK_IMAGE');
  if LField is TBlobField then
    LoadImageFromBlobField(imgBook, LField as TBlobField);

end;

procedure TfrmBook.dsBookStateChange(Sender: TObject);
var
  State: TDataSetState;
begin
  State := dmDataAccess.qryBook.State;
  btnAdd.Enabled := (State = dsBrowse);
  btnSave.Enabled := (State <> dsBrowse); // State is not browse ...
  btnCancel.Enabled := (State <> dsBrowse);

  btnDelete.Enabled := (State = dsBrowse) and
    (dmDataAccess.qryBook.RecordCount > 0);

  //
end;

procedure TfrmBook.edtSearchKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Filter: String;
begin
  Filter := '';
  if edtSearch.Text <> '' then // if search text is not blank
  begin
    if chkSearchTitle.Checked then
      Filter := Format('Book_TITLE LIKE ''%%%S%%''', [edtSearch.Text]);
    // '' --> ' , %% --> %
    // BOOK_TITLE like '%AAA%' --> SQL Query Statement
    if chkSearchAuthor.Checked then
    begin
      if Filter <> '' then
        Filter := Filter + ' OR ';
      Filter := Filter + Format('Book_AUTHOR LIKE ''%%%S%%''',
        [edtSearch.Text]);
    end;
  end;

  dmDataAccess.qryBook.Filter := Filter;
  dmDataAccess.qryBook.Filtered := (Filter <> '');

end;

procedure TfrmBook.lblLinkClick(Sender: TObject);
begin
  //  open web browser
  ShellExecute(Handle, 'open', PChar(dbeLink.Text), nil, nil, SW_SHOW);
end;

end.
