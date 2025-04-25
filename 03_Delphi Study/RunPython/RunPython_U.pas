unit RunPython_U;

interface

uses
  Winapi.Windows, Winapi.ShellAPI, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmRunPython = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmRunPython: TfrmRunPython;

implementation

{$R *.dfm}

procedure TfrmRunPython.Button1Click(Sender: TObject);
var
  PythonScriptPath: string;
  PythonInterpreterPath: string;
begin
  PythonInterpreterPath := 'c:\Users\minhwasoo\AppData\Local\Programs\Python\Python313\python.exe';
  PythonScriptPath := IncludeTrailingPathDelimiter(GetCurrentDir) +
    'aqt_projectInfo_Excel.py';

  Memo1.Lines.Add('Creating Python script: ' + PythonScriptPath);

  ShellExecute(0, nil, PChar(PythonInterpreterPath),
    PChar('"' + PythonScriptPath + '"'), nil, SW_SHOWNORMAL);
  Memo1.Lines.Add('Python script executed.');
end;

end.
