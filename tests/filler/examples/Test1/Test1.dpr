program Test1;

uses
  Forms,
  MainTest1 in 'MainTest1.pas' {Form1},
  JvFillerEditor in '..\..\source\JvFillerEditor.pas' {frmFillerEditor};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.                         
