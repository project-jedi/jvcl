program SystemPopupTest;

uses
  Forms,
  JvSystemPopup2MainFormU in 'JvSystemPopup2MainFormU.pas' {JvSystemPopup2MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvSystemPopup2MainForm, JvSystemPopup2MainForm);
  Application.Run;
end.
