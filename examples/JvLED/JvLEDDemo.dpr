program JvLEDDemo;

uses
  Forms,
  LEDMain in 'LEDMain.pas' {LEDDemoMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TLEDDemoMain, LEDDemoMain);
  Application.Run;
end.
