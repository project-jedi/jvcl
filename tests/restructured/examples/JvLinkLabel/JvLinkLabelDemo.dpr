program JvLinkLabelDemo;

uses
  Forms,
  Main in 'Main.pas' {frmMain},
  InfoStrings in 'InfoStrings.pas',
  Play in 'Play.pas' {frmPlay};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TJvLinkLabel Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
