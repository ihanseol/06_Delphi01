unit Python4Delphi;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  PythonEngine, PythonGUIInputOutput, Vcl.StdCtrls, System.Math;

type
  TfrmPython4Delphi = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    Memo2: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    // procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ReadXLSXDataWithPython(const FileName: string);
    procedure TestPython(const FileName: string);
    procedure GetMemoData;
    procedure TestEvalString(Sender: TObject);
  end;

var
  frmPython4Delphi: TfrmPython4Delphi;

implementation

{$R *.dfm}
// Requires Python4Delphi (P4D) to be installed and configured

{ TfrmPython4Delphi }


// procedure TfrmPython4Delphi.FormCreate(Sender: TObject);
// begin
// PythonEngine1.DllName := 'python38.dll';
// PythonEngine1.DllPath := 'c:\Users\minhwasoo\AppData\Local\Programs\Python\Python38\'; // Update this path!
// PythonEngine1.RegVersion := '3.8'; // Match your Python version
// PythonEngine1.LoadDll; // Explicitly load the DLL
// end;

procedure TfrmPython4Delphi.Button1Click(Sender: TObject);
var
  getPandasData: TSTringList;
  i: Integer;
begin
  // ReadXLSXDataWithPython('d:\05_Send\YanSoo_Spec.xlsx');
  // TestPython('');
  // PythonEngine1.ExecStrings( Memo2.Lines );

  // getPandasData := TSTringList.Create; // Create an instance of TStringList
  // try
  // getPandasData.Add('import pandas as pd');
  // getPandasData.Add('df = pd.read_excel(r''d:\05_Send\Yansoo_Spec.xlsx'')');
  // getPandasData.Add('print(df[''Company''].iloc[-1])');
  // getPandasData.Add('print(df[''address''].iloc[-1])');
  // getPandasData.Add('print(len(df))');
  // PythonEngine1.ExecStrings(getPandasData);
  // finally
  // getPandasData.Free; // Free the object when done
  // end;

  // GetMemoData;
  TestPython('');
end;

procedure TfrmPython4Delphi.Button2Click(Sender: TObject);
begin
  GetMemoData;
end;

procedure TfrmPython4Delphi.GetMemoData;
var
  i: Integer;
  lineCount: Integer;
  linesToDisplay: Integer;
begin
  lineCount := Memo1.Lines.Count;
  linesToDisplay := 3;
  for i := Max(0, lineCount - linesToDisplay) to lineCount - 1 do
  begin
    ShowMessage(Memo1.Lines[i]);
  end;
end;

procedure TfrmPython4Delphi.ReadXLSXDataWithPython(const FileName: string);
var
  pythonString: TSTringList;
  Output: PPyObject;
begin
  if not Assigned(PythonEngine1) then
    PythonEngine1 := TPythonEngine.Create(Self);

  pythonString := TSTringList.Create;
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
  pythonString: TSTringList;
  Output: PPyObject;
begin
  if not Assigned(PythonEngine1) then
    PythonEngine1 := TPythonEngine.Create(Self);

  pythonString := TSTringList.Create;
  try
    pythonString.Add('3+4');

    try
      // Memo1.Lines.Text := PythonEngine1.PyObjectAsString(Output);
      // PythonEngine1.ExecStrings(pythonString);
      Output := PythonEngine1.EvalStrings(pythonString);
      if Assigned(Output) then
      begin
        ShowMessage(Format(' Eval : %s',
          [PythonEngine1.PyObjectAsString(Output)]));
      end;

    finally
      PythonEngine1.Py_XDECREF(Output);
    end;
  finally
    pythonString.Free;
  end;
end;

procedure TfrmPython4Delphi.TestEvalString(Sender: TObject);
var
  Result: PPyObject;
begin
  with PythonEngine1 do
  begin
    Result := EvalStrings(Memo1.Lines);
    if Assigned(Result) then
    begin
      ShowMessage(Format('Eval: %s', [PyObjectAsString(Result)]));
      Py_DECREF(Result);
    end
    else
      ShowMessage('Could not evaluate the script');
  end;
end;

end.
