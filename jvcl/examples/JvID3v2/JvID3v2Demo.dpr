program JvID3v2Demo;

uses
  Forms,
  JvID3v2MainFormU in 'JvID3v2MainFormU.pas' {JvID3v2MainForm},
  JvID3v2EditFormU in 'JvID3v2EditFormU.pas' {JvID3v2EditForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvID3v2MainForm, JvID3v2MainForm);
  Application.Run;
end.
