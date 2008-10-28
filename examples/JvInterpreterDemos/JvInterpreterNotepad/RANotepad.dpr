program RANotepad;

uses
  Forms,
  fMain in 'fMain.pas' {Main};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
