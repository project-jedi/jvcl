program JvPlayListProj;

uses
  Forms,
  fPlayList in 'fPlayList.pas' {Form1},
  JvPropAutoSave in '..\..\Source\JvPropAutoSave.pas',
  JvMaxPixel in '..\..\Source\JvMaxPixel.pas',
  JvItemsSearchs in '..\..\Source\JvItemsSearchs.pas';

{$R *.RES}

 begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
