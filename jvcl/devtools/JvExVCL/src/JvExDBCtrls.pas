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

unit JvExDBCtrls;

{$I jvcl.inc}
{MACROINCLUDE JvExControls.macros}

WARNINGHEADER

interface

uses
  Windows, Messages, Graphics, Controls, Forms, Buttons, DBCtrls,
  Classes, SysUtils,
  JvTypes, JvThemes, JVCLVer, JvExControls, JvExButtons;

 {$DEFINE NeedMouseEnterLeave}

type
  JV_WINCONTROL_EVENTS(DBEdit)
  JV_CONTROL_EVENTS(DBText)
  JV_WINCONTROL_EVENTS(DBCheckBox)
  JV_WINCONTROL_EVENTS(DBComboBox)
  JV_WINCONTROL_EVENTS(DBListBox)
  JV_WINCONTROL_EVENTS(DBRadioGroup)
  JV_WINCONTROL_EVENTS(DBMemo)
  JV_WINCONTROL_EVENTS(DBImage)
  JV_WINCONTROL_EVENTS(DBNavigator)
  JV_WINCONTROL_EVENTS(DBLookupControl)
  JV_WINCONTROL_EVENTS(DBLookupListBox)
  JV_WINCONTROL_EVENTS(PopupDataList)
  JV_WINCONTROL_EVENTS(DBLookupComboBox)
  JV_WINCONTROL_EVENTS(DBRichEdit)
  JV_CONTROL_EVENTS_BEGIN(NavButton)

implementation

JV_WINCONTROL_EVENTS_IMPL(DBEdit)
JV_CONTROL_EVENTS_IMPL(DBText)
JV_WINCONTROL_EVENTS_IMPL(DBCheckBox)
JV_WINCONTROL_EVENTS_IMPL(DBComboBox)
JV_WINCONTROL_EVENTS_IMPL(DBListBox)
JV_WINCONTROL_EVENTS_IMPL(DBRadioGroup)
JV_WINCONTROL_EVENTS_IMPL(DBMemo)
JV_WINCONTROL_EVENTS_IMPL(DBImage)
JV_WINCONTROL_EVENTS_IMPL(DBNavigator)
JV_WINCONTROL_EVENTS_IMPL(DBLookupControl)
JV_WINCONTROL_EVENTS_IMPL(DBLookupListBox)
JV_WINCONTROL_EVENTS_IMPL(PopupDataList)
JV_WINCONTROL_EVENTS_IMPL(DBLookupComboBox)
JV_WINCONTROL_EVENTS_IMPL(DBRichEdit)
JV_CONTROL_EVENTS_IMPL(NavButton)

end.
