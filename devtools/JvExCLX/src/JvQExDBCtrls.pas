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
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQExDBCtrls;

{$I jvcl.inc}
{MACROINCLUDE JvQExControls.macros}

WARNINGHEADER

interface

uses
  QGraphics, QControls, QForms, QButtons, QExtCtrls, QDBCtrls,
  Qt, QWindows, QMessages,
  Classes, SysUtils,
  JvQTypes, JvQThemes, JVCLXVer, JvQExControls, JvQExButtons;

type
  JV_WINCONTROL(DBEdit)
  JV_CONTROL(DBText)
  JV_WINCONTROL(DBCheckBox)
  JV_WINCONTROL(DBComboBox)
  JV_WINCONTROL(DBListBox)
  JV_WINCONTROL(DBRadioGroup)
  JV_WINCONTROL(DBMemo)
  JV_WINCONTROL(DBImage)
  JV_WINCONTROL(DBNavigator)
  JV_WINCONTROL(DBLookupControl)
  JV_WINCONTROL(DBLookupListBox)
  JV_WINCONTROL(PopupDataList)
  JV_WINCONTROL(DBLookupComboBox)
  JV_CONTROL(NavButton)

implementation

JV_WINCONTROL_IMPL(DBEdit)
JV_CONTROL_EVENTS_IMPL(DBText)
JV_WINCONTROL_IMPL(DBCheckBox)
JV_WINCONTROL_IMPL(DBComboBox)
JV_WINCONTROL_IMPL(DBListBox)
JV_WINCONTROL_IMPL(DBRadioGroup)
JV_WINCONTROL_IMPL(DBMemo)
JV_WINCONTROL_IMPL(DBImage)
JV_WINCONTROL_IMPL(DBNavigator)
JV_WINCONTROL_IMPL(DBLookupControl)
JV_WINCONTROL_IMPL(DBLookupListBox)
JV_WINCONTROL_IMPL(PopupDataList)
JV_WINCONTROL_IMPL(DBLookupComboBox)
JV_CONTROL_IMPL(NavButton)

end.
