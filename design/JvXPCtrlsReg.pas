{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvXPCtrlsReg.PAS, released on 2004-01-01.

The Initial Developer of the Original Code is Marc Hoffman.
Portions created by Marc Hoffman are Copyright (C) 2002 APRIORI business solutions AG.
Portions created by APRIORI business solutions AG are Copyright (C) 2002 APRIORI business solutions AG
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvXPCtrlsReg;

interface

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvXPCtrlsReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvXPCtrlsReg.dcr}
{$ENDIF LINUX}

procedure Register;

implementation

uses
  Classes, ImgList,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, VCLEditors,
  {$ELSE}
  Contnrs, DsgnIntf,
  {$ENDIF COMPILER6_UP}
  {$IFDEF USEJVCL}
  JvDsgnConsts,
  {$ENDIF USEJVCL}
  JvXPCore, JvXPPropertyEditors, JvXPBar, JvXPContainer,
  JvXPButtons, JvXPCheckCtrls;

{$IFNDEF USEJVCL}
resourcestring
  RsPaletteXPControls = 'Jv XP Controls';
{$ENDIF USEJVCL}

procedure Register;
begin
  RegisterComponents(RsPaletteXPControls, [TJvXPStyleManager, TJvXPBar, TJvXPContainer,
    TJvXPButton, TJvXPToolButton, TJvXPCheckBox]);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvXPBarItem, 'ImageIndex',
    TJvXPItemImageIndexPropertyEditor);
  RegisterComponentEditor(TJvXPBar, TJvXPBarItemEditor);
end;

end.

