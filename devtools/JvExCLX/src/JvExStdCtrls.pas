{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExStdCtrls.pas, released on 2004-01-04

The Initial Developer of the Original Code is Andreas Hausladen [Andreas dott Hausladen att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2003 Andreas Hausladen.
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
  JV_WINCONTROL_EVENTS(CustomGroupBox)
{$DEFINE HASAUTOSIZE}
  JV_CONTROL_EVENTS(CustomLabel)
  JV_CONTROL_EVENTS(Label)
{$UNDEF HASAUTOSIZE}
  JV_EDITCONTROL_EVENTS(CustomEdit)
  JV_EDITCONTROL_EVENTS(CustomMemo)
  {$IFDEF COMPILER6_UP}
  JV_WINCONTROL_EVENTS(CustomCombo)
  {$ENDIF COMPILER6_UP}
  JV_WINCONTROL_EVENTS(CustomComboBox)
  JV_WINCONTROL_EVENTS(ButtonControl)
  JV_WINCONTROL_EVENTS(Button)
  JV_WINCONTROL_EVENTS(CustomCheckBox)
  JV_WINCONTROL_EVENTS(RadioButton)
  JV_WINCONTROL_EVENTS(CustomListBox)
  JV_WINCONTROL_EVENTS(ScrollBar)
  JV_WINCONTROL_EVENTS(GroupBox)
  JV_WINCONTROL_EVENTS(CheckBox)
  JV_WINCONTROL_EVENTS(CustomStaticText)
  JV_WINCONTROL_EVENTS(StaticText)

function DoClipBoardCommands(Msg: Integer; ClipBoardCommands: TJvClipBoardCommands): boolean;

implementation

function DoClipBoardCommands(Msg: Integer; ClipBoardCommands: TJvClipBoardCommands): boolean;
begin
  case Msg of
    WM_CLEAR         : Result := caClear in ClipBoardCommands;
    WM_COPY          : Result := caCopy in ClipBoardCommands;
    WM_CUT           : Result := caCut in ClipBoardCommands;
    WM_PASTE         : Result := caPaste in ClipBoardCommands;
    WM_UNDO, EM_UNDO : Result := caUndo in ClipBoardCommands;
  else
    Result := False;
  end;
end;


JV_WINCONTROL_EVENTS_IMPL(CustomGroupBox)
{$DEFINE HASAUTOSIZE}
JV_CONTROL_EVENTS_IMPL(CustomLabel)
JV_CONTROL_EVENTS_IMPL(Label)
{$UNDEF HASAUTOSIZE}

{$UNDEF CONSTRUCTOR_CODE}
{$DEFINE CONSTRUCTOR_CODE
  FClipboardCommands := [caCopy..caUndo];
}
JV_EDITCONTROL_EVENTS_IMPL(CustomEdit)
JV_EDITCONTROL_EVENTS_IMPL(CustomMemo)
{$UNDEF CONSTRUCTOR_CODE}
{$DEFINE CONSTRUCTOR_CODE}

{$IFDEF COMPILER6_UP}
JV_WINCONTROL_EVENTS_IMPL(CustomCombo)
{$ENDIF COMPILER6_UP}
JV_WINCONTROL_EVENTS_IMPL(CustomComboBox)
JV_WINCONTROL_EVENTS_IMPL(ButtonControl)
JV_WINCONTROL_EVENTS_IMPL(Button)
JV_WINCONTROL_EVENTS_IMPL(CustomCheckBox)
JV_WINCONTROL_EVENTS_IMPL(RadioButton)
JV_WINCONTROL_EVENTS_IMPL(CustomListBox)
JV_WINCONTROL_EVENTS_IMPL(ScrollBar)
JV_WINCONTROL_EVENTS_IMPL(GroupBox)
JV_WINCONTROL_EVENTS_IMPL(CheckBox)
JV_WINCONTROL_EVENTS_IMPL(CustomStaticText)
JV_WINCONTROL_EVENTS_IMPL(StaticText)

{$UNDEF CONSTRUCTOR_CODE} // undefine at file end

end.
