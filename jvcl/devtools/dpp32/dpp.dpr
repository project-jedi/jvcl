{**************************************************************************************************}
{                                                                                                  }
{ Delphi language Preprocessor (dpp32)                                                             }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is dpp.dpr                                                                     }
{                                                                                                  }
{ The Initial Developer of the Original Code is Andreas Hausladen                                  }
{ Portions created by these individuals are Copyright (C) of these individuals.                    }
{                                                                                                  }
{**************************************************************************************************}
program dpp;
{$APPTYPE CONSOLE}
uses                                            
  Main in 'Main.pas',
  dpp_FileInfos in 'dpp_FileInfos.pas',
  dpp_Macros in 'dpp_Macros.pas',
  dpp_PascalParser in 'dpp_PascalParser.pas',
  dpp_PreProcess in 'dpp_PreProcess.pas',
  dpp_Utils in 'dpp_Utils.pas';

begin
  Halt(EntryPoint);
end.
