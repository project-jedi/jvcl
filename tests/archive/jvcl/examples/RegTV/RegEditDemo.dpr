program RegEditDemo;

uses
  Forms,
  RegTVMainFormU in 'RegTVMainFormU.pas' {RegTVMainForm};

// , StrStore in '..\..\..\Add\Utils\StrStore.pas'

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRegTVMainForm, RegTVMainForm);
  Application.Run;
end.
