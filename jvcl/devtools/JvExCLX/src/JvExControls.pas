{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExControls.pas, released on 2004-01-04

The Initial Developer of the Original Code is Andreas Hausladen [Andreas dott Hausladen att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:  UNDER CONSTRUCTION !!!
-----------------------------------------------------------------------------}
// $Id$

unit JvExControls;

{$I jvcl.inc}

{MACROINCLUDE JvExControls.macros}

WARNINGHEADER

interface

uses
  Windows, Messages, Graphics, Controls, Forms,
  Classes, SysUtils,
  JvTypes, JvThemes, JVCLVer;


const
  CM_DENYSUBCLASSING = JvThemes.CM_DENYSUBCLASSING;

type
  { Add IJvDenySubClassing to the base class list if the control should not
    be themed by the ThemeManager (www.delphi-gems.de).
    This only works with JvExVCL derived classes. }
  IJvDenySubClassing = interface
    ['{76942BC0-2A6E-4DC4-BFC9-8E110DB7F601}']
  end;

type
  JV_CONTROL(Control)
  JV_WINCONTROL(WinControl)
  JV_CONTROL(GraphicControl)
  JV_WINCONTROL(CustomControl)
  JV_WINCONTROL(HintWindow)

function GetCanvas(Instance: TWinControl): TControlCanvas;
function GetHintColor(Instance: TWinControl): TColor;
function InputKeysToDlgCodes(InputKeys: TInputKeys): Integer;
function ShiftStateToKeyData(Shift: TShiftState): Longint;
function DoClipBoardCommands(Msg: Integer; ClipBoardCommands: TJvClipBoardCommands): Boolean;

implementation

uses
  TypInfo;

function ShiftStateToKeyData(Shift: TShiftState): Longint;
const
  AltMask = $20000000;
begin
  Result := 0;
  if ssAlt in Shift then
    Result := Result or AltMask;
end;

function GetHintColor(Instance: TWinControl): TColor;
var
  PI: PPropInfo;
begin
  Result := clDefault;  { = parent HintColor }
  while (Result = clDefault) and (Instance <> nil) do
  begin
    PI := GetPropInfo(Instance, 'HintColor');
    if PI <> nil then
      Result := TColor(GetOrdProp(Instance, PI));
    Instance := Instance.Parent;
  end;
  case Result of
  clNone, clDefault: Result := Application.HintColor;
  end;
end;

function GetCanvas(Instance: TWinControl): TControlCanvas;
var
  PI: PPropInfo;
begin
  Result := nil;
  if Assigned(Instance) then
  begin
    PI := GetPropInfo(Instance, 'Canvas');
    if PI <> nil then
      Result := TControlCanvas(GetOrdProp(Instance, PI));
  end;
end;

function InputKeysToDlgCodes(InputKeys: TInputKeys): Integer;
begin
  Result := 0;
  if ikAll in InputKeys then
    inc(Result, DLGC_WANTALLKEYS);
  if ikArrows in InputKeys then
    inc(Result, DLGC_WANTARROWS);
  if ikChars in InputKeys then
    inc(Result, DLGC_WANTCHARS);
  if ikEdit in InputKeys then
    inc(Result, DLGC_HASSETSEL);
  if ikTabs in InputKeys then
    inc(Result, DLGC_WANTTAB);
  if ikButton in InputKeys then
    inc(Result, DLGC_BUTTON);
end;

function DlgCodesToInputKeys(DlgCodes: Integer): TInputKeys;
begin
  Result := [];
  if DlgCodes and DLGC_WANTALLKEYS <> 0 then
    Include(Result, ikAll);
  if DlgCodes and DLGC_WANTARROWS <> 0 then
    Include(Result, ikArrows);
  if DlgCodes and DLGC_WANTCHARS <> 0 then
    Include(Result, ikChars);
  if DlgCodes and DLGC_HASSETSEL <> 0 then
    Include(Result, ikEdit);
  if DlgCodes and DLGC_WANTTAB <> 0 then
    Include(Result, ikTabs);
  if DlgCodes and DLGC_BUTTON <> 0 then
    Include(Result, ikButton);
end;

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


JV_CONTROL_IMPL(Control)
JV_WINCONTROL_IMPL(WinControl)
JV_CONTROL_IMPL(GraphicControl)
JV_WINCONTROL_IMPL(CustomControl)
JV_WINCONTROL_IMPL(HintWindow)

end.

