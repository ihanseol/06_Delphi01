program RunPython;

uses
  Vcl.Forms,
  RunPython_U in 'RunPython_U.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmRunPython, frmRunPython);
  Application.Run;
end.
