program JvLinkLabelDemo;

uses
  Forms, JvLinkLabelMainFormU,
  InfoStrings in 'InfoStrings.pas',
  Play in 'Play.pas' {frmPlay};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TJvLinkLabel Demo';
  Application.CreateForm(TJvLinkLabelMainForm, JvLinkLabelMainForm);
  Application.Run;
end.
