program RegEditDemo;

uses
  Forms,
  Main in 'Main.pas' {Form1}
// , StrStore in '..\..\..\Add\Utils\StrStore.pas'
;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
