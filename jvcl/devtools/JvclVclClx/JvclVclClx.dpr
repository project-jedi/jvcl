{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvclVclClx.pas, released on 2004-05-19.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

program JvclVclClx;

uses
  Forms,
  Main in 'Main.pas' {FormMain},
  JvclVclClxCvt in 'JvclVclClxCvt.pas',
  Utils in 'Utils.pas',
  VclClxCvtUtils in 'VclClxCvtUtils.pas',
  VclClxCvt in 'VclClxCvt.pas',
  PackageInformation in '..\Common\PackageInformation.pas',
  dpp_PascalParser in '..\Common\dpp_PascalParser.pas',
  PackageModels in '..\Common\PackageModels.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
