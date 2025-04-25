unit unit_RunPythonExample01;

interface

uses
  Winapi.Windows, Winapi.ShellAPI, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  PythonScriptPath: string;
  PythonInterpreterPath: string;
begin
  PythonInterpreterPath := 'C:\Python312\python.exe';
  // Replace with your Python interpreter path
  PythonScriptPath := IncludeTrailingPathDelimiter(GetCurrentDir) +
    'my_python_script.py';

  // Create a simple Python script for demonstration
  Memo1.Lines.Add('Creating Python script: ' + PythonScriptPath);
  TStringList.Create.SaveToFile(PythonScriptPath,
    ['print("Hello from Python!")', 'result = 5 + 3',
    'print("Result:", result)']);

  // Execute the Python script
  ShellExecute(0, nil, PChar(PythonInterpreterPath),
    PChar('"' + PythonScriptPath + '"'), nil, SW_SHOWNORMAL);
  Memo1.Lines.Add('Python script executed.');

  // Note: Getting direct output back to Delphi requires more complex methods
end;

end.
