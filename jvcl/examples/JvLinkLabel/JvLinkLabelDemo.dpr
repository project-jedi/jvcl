program JvLinkLabelDemo;

uses
  Forms,
  InfoStrings in 'InfoStrings.pas',
  Play in 'Play.pas' {frmPlay},
  JvLinkLabelMainFormU in 'JvLinkLabelMainFormU.pas' {JvLinkLabelMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TJvLinkLabel Demo';
  Application.CreateForm(TJvLinkLabelMainForm, JvLinkLabelMainForm);
  Application.Run;
end.
