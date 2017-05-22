program JvDesignerDemo;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  JvDesignSurface in '..\..\run\JvDesignSurface.pas',
  JvDesignImp in '..\..\run\JvDesignImp.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
