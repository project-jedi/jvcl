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
  JV_WINCONTROL(CustomGroupBox)
{$DEFINE HASAUTOSIZE}
  JV_CONTROL(CustomLabel)
  JV_CONTROL(Label)
{$UNDEF HASAUTOSIZE}
  JV_EDITCONTROL(CustomEdit)
  JV_EDITCONTROL(CustomMemo)
  {$IFDEF COMPILER6_UP}
  JV_WINCONTROL(CustomCombo)
  {$ENDIF COMPILER6_UP}
  JV_WINCONTROL(CustomComboBox)
  JV_WINCONTROL(ButtonControl)
  JV_WINCONTROL(Button)
  JV_WINCONTROL(CustomCheckBox)
  JV_WINCONTROL(RadioButton)
  JV_WINCONTROL(CustomListBox)
  JV_WINCONTROL(ScrollBar)
  JV_WINCONTROL(GroupBox)
  JV_WINCONTROL(CheckBox)
  JV_WINCONTROL(CustomStaticText)
  JV_WINCONTROL(StaticText)

function DoClipBoardCommands(Msg: Integer; ClipBoardCommands: TJvClipBoardCommands): Boolean;

implementation

function DoClipBoardCommands(Msg: Integer; ClipBoardCommands: TJvClipBoardCommands): Boolean;
begin
  case Msg of
    WM_COPY          : Result := caCopy in ClipBoardCommands;
    WM_CUT           : Result := caCut in ClipBoardCommands;
    WM_PASTE         : Result := caPaste in ClipBoardCommands;
    WM_UNDO, EM_UNDO : Result := caUndo in ClipBoardCommands;
  else
    Result := False;
  end;
end;


JV_WINCONTROL_IMPL(CustomGroupBox)
{$DEFINE HASAUTOSIZE}
JV_CONTROL_IMPL(CustomLabel)
JV_CONTROL_IMPL(Label)
{$UNDEF HASAUTOSIZE}

{$UNDEF CONSTRUCTOR_CODE}
{$DEFINE CONSTRUCTOR_CODE
  FClipboardCommands := [caCopy..caUndo];
}
JV_EDITCONTROL_IMPL(CustomEdit)
JV_EDITCONTROL_IMPL(CustomMemo)
{$UNDEF CONSTRUCTOR_CODE}
{$DEFINE CONSTRUCTOR_CODE}

{$IFDEF COMPILER6_UP}
JV_WINCONTROL_IMPL(CustomCombo)
{$ENDIF COMPILER6_UP}
JV_WINCONTROL_IMPL(CustomComboBox)
JV_WINCONTROL_IMPL(ButtonControl)
JV_WINCONTROL_IMPL(Button)
JV_WINCONTROL_IMPL(CustomCheckBox)
JV_WINCONTROL_IMPL(RadioButton)
JV_WINCONTROL_IMPL(CustomListBox)
JV_WINCONTROL_IMPL(ScrollBar)
JV_WINCONTROL_IMPL(GroupBox)
JV_WINCONTROL_IMPL(CheckBox)
JV_WINCONTROL_IMPL(CustomStaticText)
JV_WINCONTROL_IMPL(StaticText)

{$UNDEF CONSTRUCTOR_CODE} // undefine at file end

end.
