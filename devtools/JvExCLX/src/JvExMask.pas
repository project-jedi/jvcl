{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvQExStdCtrls.pas, released on 2004-10-21

The Initial Developer of the Original Code is André Snepvangers [asn att xs4all dott nl]
Portions created by André Snepvangers are Copyright (C) 2004 André Snepvangers.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvExMask;

{$I jvcl.inc}

{MACROINCLUDE JvExControls.macros}

WARNINGHEADER

interface

uses
  Windows, Messages, Graphics, Controls, Forms, Mask,
  Classes, SysUtils,
  JvTypes, JvThemes, JVCLVer, JvExControls;

type
  JV_EDITCONTROL_BEGIN(CustomMaskEdit)
  private
    FBeepOnError: Boolean;
  protected
    procedure DoBeepOnError; dynamic;
    procedure SetBeepOnError(Value: Boolean); virtual;
    property BeepOnError: Boolean read FBeepOnError write SetBeepOnError default True;
  JV_EDITCONTROL_END(CustomMaskEdit)

  JV_EDITCONTROL_BEGIN(MaskEdit)
  private
    FBeepOnError: Boolean;
  protected
    procedure DoBeepOnError; dynamic;
    procedure SetBeepOnError(Value: Boolean); virtual;
    property BeepOnError: Boolean read FBeepOnError write SetBeepOnError default True;
  JV_EDITCONTROL_END(MaskEdit)

implementation

{ The CONSTRUCTOR_CODE macro is used to extend the constructor by the macro
  content. }
{$UNDEF CONSTRUCTOR_CODE}
{$DEFINE CONSTRUCTOR_CODE
  FBeepOnError := True;
  FClipboardCommands := [caCopy..caUndo];
}

JV_EDITCONTROL_IMPL(CustomMaskEdit)

procedure TJvExCustomMaskEdit.DoBeepOnError;
begin
  if FBeepOnError then
    SysUtils.Beep;
end;

procedure TJvExCustomMaskEdit.SetBeepOnError(Value: Boolean);
begin
  FBeepOnError := Value;
end;

JV_EDITCONTROL_IMPL(MaskEdit)

procedure TJvExMaskEdit.DoBeepOnError;
begin
  if FBeepOnError then
    SysUtils.Beep;
end;

procedure TJvExMaskEdit.SetBeepOnError(Value: Boolean);
begin
  FBeepOnError := Value;
end;
{$UNDEF CONSTRUCTOR_CODE} // undefine at file end
end.
