{**************************************************************************************************}
{                                                                                                  }
{ Ray's Jedi Projects                                                                              }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit is part of EDI SDK demos.                                                              }
{                                                                                                  }
{ Unit owner: Raymond Alexander                                                                    }
{ Date created: Before October, 1, 2003                                                            }
{ Last modified: October 26, 2003                                                                  }
{ Additional Info:                                                                                 }
{   E-Mail at RaysDelphiBox3@hotmail.com                                                           }
{   For latest EDI specific updates see http://sourceforge.net/projects/edisdk                     }
{   See home page for latest news & events and online help.                                        }
{                                                                                                  }
{**************************************************************************************************}
program EDIDBBuffering;

uses
  Forms,
  UnitMain in 'UnitMain.pas' {FormMain},
  ADS70_DM in 'ADS70_DM.pas' {ADS70_Data: TDataModule},
  Interbase6_IBX_DM in 'Interbase6_IBX_DM.pas' {Interbase6_IBX_Data: TDataModule};

{$E exe}

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'EDI Database Buffering (Demo)';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
