program TransparentFormDemo;

uses
  Forms,
  FrmMain in 'FrmMain.pas' {FormDemo};

{$R *.res}

begin
  Application.Initialize;
  //Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormDemo, FormDemo);
  Application.Run;
end.

