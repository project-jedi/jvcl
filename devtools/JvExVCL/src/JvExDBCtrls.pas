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

{$I jvcl.inc}
{MACROINCLUDE JvExControls.macros}

WARNINGHEADER

unit JvExDBCtrls;
interface
uses
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, Buttons, DBCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Qt, QGraphics, QControls, QForms, QButtons, QDBCtrls, Types, QWindows,
  {$ENDIF VisualCLX}
  Classes, SysUtils,
  JvTypes, JvThemes, JVCLVer, JvExControls, JvExButtons;

{$IFDEF VCL}
 {$DEFINE NeedMouseEnterLeave}
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
  {$DEFINE NeedMouseEnterLeave}
 {$IFEND}
{$ENDIF VisualCLX}

type
  JV_WINCONTROL_EVENTS(DBEdit)
  JV_CONTROL_EVENTS(DBText)
  JV_WINCONTROL_EVENTS(DBCheckBox)
  JV_WINCONTROL_EVENTS(DBComboBox)
  JV_WINCONTROL_EVENTS(DBListBox)
  JV_WINCONTROL_EVENTS(DBRadioGroup)
  JV_WINCONTROL_EVENTS(DBMemo)
  JV_CUSTOMCONTROL_EVENTS(DBImage)
  JV_CUSTOMCONTROL_EVENTS(DBNavigator)
  JV_CUSTOMCONTROL_EVENTS(DBLookupControl)
  JV_CUSTOMCONTROL_EVENTS(DBLookupListBox)
  JV_CUSTOMCONTROL_EVENTS(PopupDataList)
  JV_CUSTOMCONTROL_EVENTS(DBLookupComboBox)
{$IFDEF VCL}
  JV_WINCONTROL_EVENTS(DBRichEdit)
{$ENDIF VCL}
  JV_CONTROL_EVENTS_BEGIN(NavButton)

implementation

JV_WINCONTROL_EVENTS_IMPL(DBEdit)
JV_CONTROL_EVENTS_IMPL(DBText)
JV_WINCONTROL_EVENTS_IMPL(DBCheckBox)
JV_WINCONTROL_EVENTS_IMPL(DBComboBox)
JV_WINCONTROL_EVENTS_IMPL(DBListBox)
JV_WINCONTROL_EVENTS_IMPL(DBRadioGroup)
JV_WINCONTROL_EVENTS_IMPL(DBMemo)
JV_CUSTOMCONTROL_EVENTS_IMPL(DBImage)
JV_CUSTOMCONTROL_EVENTS_IMPL(DBNavigator)
JV_CUSTOMCONTROL_EVENTS_IMPL(DBLookupControl)
JV_CUSTOMCONTROL_EVENTS_IMPL(DBLookupListBox)
JV_CUSTOMCONTROL_EVENTS_IMPL(PopupDataList)
JV_CUSTOMCONTROL_EVENTS_IMPL(DBLookupComboBox)
{$IFDEF VCL}
JV_WINCONTROL_EVENTS_IMPL(DBRichEdit)
{$ENDIF VCL}
JV_CONTROL_EVENTS_IMPL(NavButton)

end.
