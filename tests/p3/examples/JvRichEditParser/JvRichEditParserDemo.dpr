program JvRichEditParserDemo;

uses
  Forms,
  JvRichEditParserFrm in 'JvRichEditParserFrm.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
