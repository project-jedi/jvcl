program JvCheckBoxDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {Form1},
  JvLinkedControlsEditorForm in '..\..\design\JvLinkedControlsEditorForm.pas' {frmLinkedControlsEditor};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
