{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExComboEdits.pas, released on 2004-01-19

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

{$IFDEF VCL}
Sorry this file is for VCLX only
{$ENDIF VCL}

unit JvExComboEdits;

interface

uses
  Qt, QGraphics, QControls, QForms, Types, QComboEdits, QWindows,
  Classes, SysUtils,
  JvTypes, JvThemes, JVCLVer, JvExControls;

{$IFDEF VCL}
 {$DEFINE NeedMouseEnterLeave}
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
  {$DEFINE NeedMouseEnterLeave}
 {$IFEND}
{$ENDIF VisualCLX}

type
  JV_EDITCONTROL_EVENTS(CustomComboEdit)
  JV_EDITCONTROL_EVENTS(ComboEdit)

  JV_EDITCONTROL_EVENTS_BEGIN(CustomComboMaskEdit)
  private
    FBeepOnError: Boolean;
  protected
    procedure DoBeepOnError; dynamic;
    procedure SetBeepOnError(Value: Boolean); virtual;
    property BeepOnError: Boolean read FBeepOnError write SetBeepOnError default True;
  JV_EDITCONTROL_EVENTS_END(CustomComboMaskEdit)

  JV_EDITCONTROL_EVENTS_BEGIN(ComboMaskEdit)
  private
    FBeepOnError: Boolean;
  protected
    procedure DoBeepOnError; dynamic;
    procedure SetBeepOnError(Value: Boolean); virtual;
    property BeepOnError: Boolean read FBeepOnError write SetBeepOnError default True;
  JV_EDITCONTROL_EVENTS_END(ComboMaskEdit)

implementation

{$UNDEF CONSTRUCTOR_CODE}
{$DEFINE CONSTRUCTOR_CODE
  FClipboardCommands := [caCopy..caUndo];
}
JV_EDITCONTROL_EVENTS_IMPL(CustomComboEdit)
JV_EDITCONTROL_EVENTS_IMPL(ComboEdit)

{ The CONSTRUCTOR_CODE macro is used to extend the constructor by the macro
  content. }
{$UNDEF CONSTRUCTOR_CODE}
{$DEFINE CONSTRUCTOR_CODE
  FBeepOnError := True;
  FClipboardCommands := [caCopy..caUndo];
}
JV_EDITCONTROL_EVENTS_IMPL_BEGIN(CustomComboMaskEdit)
procedure TJvExCustomComboMaskEdit.DoBeepOnError;
begin
  if BeepOnError then
    SysUtils.Beep;
end;

procedure TJvExCustomComboMaskEdit.SetBeepOnError(Value: Boolean);
begin
  FBeepOnError := Value;
end;
JV_EDITCONTROL_EVENTS_IMPL_END(CustomComboMaskEdit)


JV_EDITCONTROL_EVENTS_IMPL_BEGIN(ComboMaskEdit)
procedure TJvExComboMaskEdit.DoBeepOnError;
begin
  if BeepOnError then
    SysUtils.Beep;
end;

procedure TJvExComboMaskEdit.SetBeepOnError(Value: Boolean);
begin
  FBeepOnError := Value;
end;
JV_EDITCONTROL_EVENTS_IMPL_END(ComboMaskEdit)

{$UNDEF CONSTRUCTOR_CODE} // undefine at file end
end.
