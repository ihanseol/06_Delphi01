unit mainform;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ToolWin, Vcl.ComCtrls, Vcl.ExtCtrls,
  System.ImageList, Vcl.ImgList;

type
  TfrmMain = class(TForm)
    pnlLayout: TPanel;
    tbMainMenu: TToolBar;
    btnRent: TToolButton;
    btnMenuBook: TToolButton;
    btnMenuUser: TToolButton;
    ToolButton4: TToolButton;
    btnClose: TToolButton;
    ilToolbar: TImageList;
    procedure btnCloseClick(Sender: TObject);
    procedure btnMenuBookClick(Sender: TObject);
    procedure btnMenuUserClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}
uses
BookForm, UserForm;

procedure TfrmMain.btnCloseClick(Sender: TObject);
begin
  close;
end;

procedure TfrmMain.btnMenuBookClick(Sender: TObject);
begin
 if not Assigned(frmBook) then
   frmBook := TfrmBook.Create(self);
  frmBook.Parent := pnlLayout;
  frmBook.BorderStyle := bsNone;
  frmBook.Align := alClient;
  frmBook.show;

end;

procedure TfrmMain.btnMenuUserClick(Sender: TObject);
begin
   //

   if not Assigned(frmUser) then            // if not assigned user form then create self (mainform)
      frmUser.Create(Self);

   frmUser.Parent := pnlLayout;
   frmUser.BorderStyle := bsNone;
   frmUser.Align := alClient;
   frmUser.Show;
end;

end.
