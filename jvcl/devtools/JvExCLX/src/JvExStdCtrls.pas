{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExStdCtrls.pas, released on 2004-10-21

The Initial Developer of the Original Code is André Snepvangers [asn att xs4all dott nl]
Portions created by André Snepvangers are Copyright (C) 2004 André Snepvangers.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvExStdCtrls;

{$I jvcl.inc}
{MACROINCLUDE JvExControls.macros}

WARNINGHEADER

interface

uses
  Windows, Messages, Graphics, Controls, Forms, StdCtrls,
  Classes, SysUtils,
  JvTypes, JvThemes, JVCLVer, JvExControls;

type
  JV_WINCONTROL(ButtonControl)
  JV_WINCONTROL(Button)
  JV_WINCONTROL(CustomCheckBox)
  JV_WINCONTROL(CheckBox)
  JV_CUSTOMCONTROL(CustomComboBox)
  JV_CUSTOMCONTROL(ComboBox)
  JV_EDITCONTROL(CustomEdit)
{$UNDEF PUBLISHAUTOSIZE}
{$DEFINE PUBLISHAUTOSIZE published}
  JV_EDITCONTROL(Edit)
{$UNDEF PUBLISHAUTOSIZE}
{$DEFINE PUBLISHAUTOSIZE}
  JV_WINCONTROL(CustomGroupBox)
  JV_WINCONTROL(GroupBox)
  JV_CONTROL(CustomLabel)
{$UNDEF PUBLISHAUTOSIZE}
{$DEFINE PUBLISHAUTOSIZE published}
  JV_CONTROL(Label)
{$UNDEF PUBLISHAUTOSIZE}
{$DEFINE PUBLISHAUTOSIZE}
  JV_WINCONTROL(CustomListBox)
  JV_EDITCONTROL(CustomMemo)
  JV_EDITCONTROL(Memo)
  JV_WINCONTROL(RadioButton)
  JV_WINCONTROL(ScrollBar)
  JV_WINCONTROL(CustomStaticText)
(*$UNDEF PUBLISHAUTOSIZE *)
(*$DEFINE PUBLISHAUTOSIZE published *)
  JV_WINCONTROL(StaticText)
(*$UNDEF PUBLISHAUTOSIZE *)
(*$DEFINE PUBLISHAUTOSIZE *)
  {$IFDEF COMPILER6_UP}
  JV_CUSTOMCONTROL(CustomCombo)
  {$ENDIF COMPILER6_UP}

implementation

  JV_WINCONTROL_IMPL(ButtonControl)
  JV_WINCONTROL_IMPL(Button)
  JV_WINCONTROL_IMPL(CustomCheckBox)
  JV_WINCONTROL_IMPL(CheckBox)
  JV_CUSTOMCONTROL_IMPL(CustomComboBox)
  JV_CUSTOMCONTROL_IMPL(ComboBox)
  JV_EDITCONTROL_IMPL(CustomEdit)
  JV_EDITCONTROL_IMPL(Edit)
  JV_WINCONTROL_IMPL(CustomGroupBox)
  JV_WINCONTROL_IMPL(GroupBox)
  JV_CONTROL_IMPL(CustomLabel)
  JV_CONTROL_IMPL(Label)
  JV_WINCONTROL_IMPL(CustomListBox)
  JV_EDITCONTROL_IMPL(CustomMemo)
  JV_EDITCONTROL_IMPL(Memo)
  JV_WINCONTROL_IMPL(RadioButton)
  JV_WINCONTROL_IMPL(ScrollBar)
  JV_WINCONTROL_IMPL(CustomStaticText)
  JV_WINCONTROL_IMPL(StaticText)
  {$IFDEF COMPILER6_UP}
  JV_CUSTOMCONTROL_IMPL(CustomCombo)
  {$ENDIF COMPILER6_UP}

end.
