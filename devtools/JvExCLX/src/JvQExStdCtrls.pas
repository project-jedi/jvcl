{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvQExStdCtrls.pas, released on 2004-09-21

The Initial Developer of the Original Code is André Snepvangers [asn att xs4all dott nl]
Portions created by André Snepvangers are Copyright (C) 2004 André Snepvangers.
All Rights Reserved.

Contributor(s): -


You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQExStdCtrls;

{$I jvcl.inc}
{MACROINCLUDE JvQExControls.macros}

WARNINGHEADER

interface

uses
  QGraphics, QControls, QForms, QStdCtrls, QExtCtrls,
  Qt, QWindows, QMessages,
  Classes, SysUtils,
  JvQTypes, JvQThemes, JVCLXVer, JvQExControls;

type
  JV_WINCONTROL(ButtonControl)
  JV_WINCONTROL(Button)
  JV_WINCONTROL(CustomCheckBox)
  JV_WINCONTROL(CheckBox)
  JV_WINCUSTOMCONTROL(CustomComboBox)
  JV_WINCUSTOMCONTROL(ComboBox)
  JV_EDITCONTROL(CustomEdit)
  JV_EDITCONTROL(Edit)
  JV_WINCONTROL(CustomGroupBox)
  JV_WINCONTROL(GroupBox)
  JV_WINCONTROL(CustomLCDNumber)
  JV_WINCONTROL(LCDNumber)
  JV_WINCUSTOMCONTROL(CustomListBox)
  JV_WINCUSTOMCONTROL(ListBox)
  JV_WINCONTROL(CustomMemo)
  JV_WINCONTROL(Memo)
  JV_WINCONTROL(RadioButton)
  JV_WINCONTROL(ScrollBar)
  JV_WINCONTROL(CustomLabel)
  JV_WINCONTROL(Label)

implementation

JV_WINCONTROL_IMPL(ButtonControl)
JV_WINCONTROL_IMPL(Button)
JV_WINCONTROL_IMPL(CustomCheckBox)
JV_WINCONTROL_IMPL(CheckBox)
JV_WINCUSTOMCONTROL_IMPL(CustomComboBox)
JV_WINCUSTOMCONTROL_IMPL(ComboBox)
JV_EDITCONTROL_IMPL(CustomEdit)
JV_EDITCONTROL_IMPL(Edit)
JV_WINCONTROL_IMPL(CustomMemo)
JV_WINCONTROL_IMPL(Memo)
JV_WINCONTROL_IMPL(CustomGroupBox)
JV_WINCONTROL_IMPL(GroupBox)
JV_WINCONTROL_IMPL(CustomLCDNumber)
JV_WINCONTROL_IMPL(LCDNumber)
JV_WINCUSTOMCONTROL_IMPL(CustomListBox)
JV_WINCUSTOMCONTROL_IMPL(ListBox)
JV_WINCONTROL_IMPL(RadioButton)
JV_WINCONTROL_IMPL(ScrollBar)
JV_WINCONTROL_IMPL(CustomLabel)
JV_WINCONTROL_IMPL(Label)

{$DEFINE UnitName 'JvQExStdCtrls.pas'}

UNITVERSION

end.
