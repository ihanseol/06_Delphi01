program Python4DelphiExam01;

uses
  Vcl.Forms,
  Python4Delphi in 'Python4Delphi.pas' {frmPython4Delphi};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmPython4Delphi, frmPython4Delphi);
  Application.Run;
end.
