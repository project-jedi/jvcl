{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExDBCtrls.pas, released on 2004-01-04

The Initial Developer of the Original Code is Andreas Hausladen [Andreas dott Hausladen att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvExDBCtrls;

{$I jvcl.inc}
{MACROINCLUDE JvExControls.macros}

WARNINGHEADER

interface

uses
  Windows, Messages, Types,
  SysUtils, Classes, Graphics, Controls, Forms, Buttons, DBCtrls,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvTypes, JvThemes, JVCLVer, JvExControls, JvExButtons;

type
  WINCONTROL_DECL_DEFAULT(DBEdit)

  CONTROL_DECL_DEFAULT(DBText)

  WINCONTROL_DECL_DEFAULT(DBCheckBox)

  WINCONTROL_DECL_DEFAULT(DBComboBox)

  WINCONTROL_DECL_DEFAULT(DBListBox)

  WINCONTROL_DECL_DEFAULT(DBRadioGroup)

  WINCONTROL_DECL_DEFAULT(DBMemo)

  WINCONTROL_DECL_DEFAULT(DBImage)

  WINCONTROL_DECL_DEFAULT(DBNavigator)

  WINCONTROL_DECL_DEFAULT(DBLookupControl)

  WINCONTROL_DECL_DEFAULT(DBLookupListBox)

  WINCONTROL_DECL_DEFAULT(PopupDataList)

  WINCONTROL_DECL_DEFAULT(DBLookupComboBox)

  WINCONTROL_DECL_DEFAULT(DBRichEdit)

  CONTROL_DECL_DEFAULT(NavButton)

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

WINCONTROL_IMPL_DEFAULT(DBEdit)

CONTROL_IMPL_DEFAULT(DBText)

WINCONTROL_IMPL_DEFAULT(DBCheckBox)

WINCONTROL_IMPL_DEFAULT(DBComboBox)

WINCONTROL_IMPL_DEFAULT(DBListBox)

WINCONTROL_IMPL_DEFAULT(DBRadioGroup)

WINCONTROL_IMPL_DEFAULT(DBMemo)

WINCONTROL_IMPL_DEFAULT(DBImage)

WINCONTROL_IMPL_DEFAULT(DBNavigator)

WINCONTROL_IMPL_DEFAULT(DBLookupControl)

WINCONTROL_IMPL_DEFAULT(DBLookupListBox)

WINCONTROL_IMPL_DEFAULT(PopupDataList)

WINCONTROL_IMPL_DEFAULT(DBLookupComboBox)

WINCONTROL_IMPL_DEFAULT(DBRichEdit)

CONTROL_IMPL_DEFAULT(NavButton)

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.