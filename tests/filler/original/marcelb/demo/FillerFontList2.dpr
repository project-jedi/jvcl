program FillerFontList2;

uses
  Forms,
  JvFillFontLstMain in 'JvFillFontLstMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
