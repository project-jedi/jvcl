program TestDBGridLookup;

uses
  Vcl.Forms,
  MainU in 'MainU.pas' {Form1},
  JvDBGrid in '..\..\run\JvDBGrid.pas',
  JvDBGridSelectColumnForm in '..\..\run\JvDBGridSelectColumnForm.pas' {frmSelectColumn};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
