unit Python4Delphi;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  PythonEngine, PythonGUIInputOutput, Vcl.StdCtrls;

type
  TfrmPython4Delphi = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
//    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ReadXLSXDataWithPython(const FileName: string);
    procedure TestPython(const FileName: string);
  end;

var
  frmPython4Delphi: TfrmPython4Delphi;

implementation

{$R *.dfm}
// Requires Python4Delphi (P4D) to be installed and configured

{ TfrmPython4Delphi }


//procedure TfrmPython4Delphi.FormCreate(Sender: TObject);
//begin
//  PythonEngine1.DllName := 'python38.dll';
//  PythonEngine1.DllPath := 'c:\Users\minhwasoo\AppData\Local\Programs\Python\Python38\'; // Update this path!
//  PythonEngine1.RegVersion := '3.8'; // Match your Python version
//  PythonEngine1.LoadDll; // Explicitly load the DLL
//end;


procedure TfrmPython4Delphi.Button1Click(Sender: TObject);
begin
  // ReadXLSXDataWithPython('d:\05_Send\YanSoo_Spec.xlsx');
//  TestPython('');
  PythonEngine1.ExecStrings( Memo2.Lines );
end;


procedure TfrmPython4Delphi.ReadXLSXDataWithPython(const FileName: string);
var
  pythonString: TStringList;
  Output: PPyObject;
begin
  if not Assigned(PythonEngine1) then
    PythonEngine1 := TPythonEngine.Create(Self);

  pythonString := TStringList.Create;
  try
    pythonString.Add('import pandas as pd');
    pythonString.Add('df = pd.read_excel("' + StringReplace(FileName, '\', '\\',
      [rfReplaceAll]) + '")');
    pythonString.Add('df.to_csv(index=False)');
    Output := PythonEngine1.EvalStrings(pythonString);
    try
      Memo1.Lines.Text := PythonEngine1.PyObjectAsString(Output);
    finally
      PythonEngine1.Py_XDECREF(Output);
    end;
  finally
    pythonString.Free;
  end;
end;

procedure TfrmPython4Delphi.TestPython(const FileName: string);
var
  pythonString: TStringList;
  Output: PPyObject;
begin
  if not Assigned(PythonEngine1) then
    PythonEngine1 := TPythonEngine.Create(Self);

  pythonString := TStringList.Create;
  try
    pythonString.Add('3+4');
    Output := PythonEngine1.EvalStrings(pythonString);
    try
      Memo1.Lines.Text := PythonEngine1.PyObjectAsString(Output);
    finally
      PythonEngine1.Py_XDECREF(Output);
    end;
  finally
    pythonString.Free;
  end;
end;

end.
