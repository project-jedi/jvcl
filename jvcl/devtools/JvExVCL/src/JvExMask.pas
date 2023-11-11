{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExMask.pas, released on 2004-01-04

The Initial Developer of the Original Code is Andreas Hausladen [Andreas dott Hausladen att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvExMask;

{$I jvcl.inc}
{MACROINCLUDE JvExControls.macros}

WARNINGHEADER

interface

uses
  Windows, Messages, Types,
  SysUtils, Classes, Graphics, Controls, Forms, Mask,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvTypes, JvThemes, JVCLVer, JvExControls;

type
  TJvExCustomMaskEdit = class(TCustomMaskEdit, IJvExControl)
  EDITCONTROL_DECL
  private
    FBeepOnError: Boolean;
    FRaiseException: Boolean;
    {$IFNDEF COMPILER12_UP}
    FTextHint: string;
    procedure SetTextHint(const Value: string);
    {$ENDIF ~COMPILER12_UP}
    function UserTextHint: Boolean;
  protected
    procedure DoBeepOnError; dynamic;
    procedure SetBeepOnError(Value: Boolean); virtual;
    procedure ValidateError; override;
    property BeepOnError: Boolean read FBeepOnError write SetBeepOnError default True;
    property RaiseException: Boolean read FRaiseException write FRaiseException default True;

    procedure DoSetTextHint(const Value: string); {$IFDEF COMPILER12_UP}override;{$ELSE}virtual;{$ENDIF}
    procedure PaintWindow(DC: HDC); override;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;

    {$IFNDEF COMPILER12_UP}
    procedure CreateWnd; override;
    property TextHint: string read FTextHint write SetTextHint;
    {$ENDIF ~COMPILER12_UP}
  end;

  TJvExMaskEdit = class(TMaskEdit, IJvExControl)
  EDITCONTROL_DECL
  private
    FBeepOnError: Boolean;
    FRaiseException: Boolean;
    {$IFNDEF COMPILER12_UP}
    FTextHint: string;
    procedure SetTextHint(const Value: string);
    {$ENDIF ~COMPILER12_UP}
    function UserTextHint: Boolean;
  protected
    procedure DoBeepOnError; dynamic;
    procedure SetBeepOnError(Value: Boolean); virtual;
    procedure ValidateError; override;

    procedure DoSetTextHint(const Value: string); {$IFDEF COMPILER12_UP}override;{$ELSE}virtual;{$ENDIF}
    procedure PaintWindow(DC: HDC); override;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;

    {$IFNDEF COMPILER12_UP}
    procedure CreateWnd; override;
    {$ENDIF ~COMPILER12_UP}
  published
    {$IFNDEF COMPILER12_UP}
    property TextHint: string read FTextHint write SetTextHint;
    {$ENDIF ~COMPILER12_UP}
    property BeepOnError: Boolean read FBeepOnError write SetBeepOnError default True;
    property RaiseException: Boolean read FRaiseException write FRaiseException default True;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  JclSysInfo;

{$IFNDEF COMPILER12_UP}
const
  ECM_FIRST       = $1500;
  EM_SETCUEBANNER = ECM_FIRST + 1;   // Set the cue banner with the lParam = LPCWSTR

  CBM_FIRST       = $1700;
  CB_SETCUEBANNER = CBM_FIRST + 3;

type
  UnicodeString = WideString;
{$ENDIF ~COMPILER12_UP}

BEGIN_EDITCONTROL_CONSTRUCTOR(CustomMaskEdit)
  FBeepOnError := True;
  FRaiseException := True;
  if UserTextHint then
    ControlState := ControlState + [csCustomPaint]; // needed for PaintWindow
END_CONSTRUCTOR

EDITCONTROL_IMPL(CustomMaskEdit)

EDITCONTROL_WNDPROC(CustomMaskEdit)

procedure TJvExCustomMaskEdit.DoBeepOnError;
begin
  if FBeepOnError then
    SysUtils.Beep;
end;

procedure TJvExCustomMaskEdit.SetBeepOnError(Value: Boolean);
begin
  FBeepOnError := Value;
end;

procedure TJvExCustomMaskEdit.ValidateError;
begin
  if FRaiseException then
    inherited ValidateError;
end;

{$IFNDEF COMPILER12_UP}
procedure TJvExCustomMaskEdit.SetTextHint(const Value: string);
begin
  if FTextHint <> Value then
  begin
    FTextHint := Value;
    if not (csLoading in ComponentState) then
      DoSetTextHint(FTextHint);
  end;
end;

procedure TJvExCustomMaskEdit.CreateWnd;
begin
  inherited CreateWnd;
  DoSetTextHint(FTextHint);
end;
{$ENDIF ~COMPILER12_UP}

function TJvExCustomMaskEdit.UserTextHint: Boolean;
begin
  {$IFDEF JVCLThemesEnabled}
  Result := not (JclCheckWinVersion(5, 1) and StyleServices.Enabled);
  {$ELSE}
  Result := True;
  {$ENDIF JVCLThemesEnabled}
end;

procedure TJvExCustomMaskEdit.DoSetTextHint(const Value: string);
begin
  {$IFDEF COMPILER12_UP}
  inherited DoSetTextHint(Value);
  {$ELSE}
  {$IFDEF JVCLThemesEnabled}
  if JclCheckWinVersion(5, 1) and StyleServices.Enabled and HandleAllocated then
    SendMessage(Handle, EM_SETCUEBANNER, WPARAM(0), LPARAM(PWideChar(UnicodeString(Value))));
  {$ENDIF JVCLThemesEnabled}
  {$ENDIF COMPILER12_UP}
  if UserTextHint and HandleAllocated and
     not Focused and Enabled and not ReadOnly and (Text = '') then
    Invalidate;
end;

procedure TJvExCustomMaskEdit.PaintWindow(DC: HDC);
var
  R: TRect;
  OldFont: HFONT;
  OldTextColor: TColorRef;
begin
  inherited PaintWindow(DC);

  if UserTextHint and (TextHint <> '') and
     Enabled and not ReadOnly and not Focused and (Text = '') then
  begin
    SendMessage(Handle, EM_GETRECT, 0, LPARAM(@R));

    OldFont := SelectObject(DC, Font.Handle);
    OldTextColor := SetTextColor(DC, ColorToRGB(clGrayText));
    DrawText(DC, PChar(TextHint), Length(TextHint), R, DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);

    SetTextColor(DC, OldTextColor);
    SelectObject(DC, OldFont);
  end;
end;

procedure TJvExCustomMaskEdit.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  if UserTextHint and (TextHint <> '') then
    Invalidate;
end;

procedure TJvExCustomMaskEdit.WMSetFocus(var Msg: TWMSetFocus);
begin
  if UserTextHint and (TextHint <> '') then
    Invalidate;
  inherited;
end;

BEGIN_EDITCONTROL_CONSTRUCTOR(MaskEdit)
  FBeepOnError := True;
  FRaiseException := True;
  if UserTextHint then
    ControlState := ControlState + [csCustomPaint]; // needed for PaintWindow
END_CONSTRUCTOR

EDITCONTROL_IMPL(MaskEdit)

EDITCONTROL_WNDPROC(MaskEdit)

procedure TJvExMaskEdit.DoBeepOnError;
begin
  if FBeepOnError then
    SysUtils.Beep;
end;

procedure TJvExMaskEdit.SetBeepOnError(Value: Boolean);
begin
  FBeepOnError := Value;
end;

procedure TJvExMaskEdit.ValidateError;
begin
  if FRaiseException then
    inherited ValidateError;
end;

{$IFNDEF COMPILER12_UP}
procedure TJvExMaskEdit.SetTextHint(const Value: string);
begin
  if FTextHint <> Value then
  begin
    FTextHint := Value;
    if not (csLoading in ComponentState) then
      DoSetTextHint(FTextHint);
  end;
end;

procedure TJvExMaskEdit.CreateWnd;
begin
  inherited CreateWnd;
  DoSetTextHint(FTextHint);
end;
{$ENDIF ~COMPILER12_UP}

function TJvExMaskEdit.UserTextHint: Boolean;
begin
  {$IFDEF JVCLThemesEnabled}
  Result := not (JclCheckWinVersion(5, 1) and StyleServices.Enabled);
  {$ELSE}
  Result := True;
  {$ENDIF JVCLThemesEnabled}
end;

procedure TJvExMaskEdit.DoSetTextHint(const Value: string);
begin
  {$IFDEF COMPILER12_UP}
  inherited DoSetTextHint(Value);
  {$ELSE}
  {$IFDEF JVCLThemesEnabled}
  if JclCheckWinVersion(5, 1) and StyleServices.Enabled and HandleAllocated then
    SendMessage(Handle, EM_SETCUEBANNER, WPARAM(0), LPARAM(PWideChar(UnicodeString(Value))));
  {$ENDIF JVCLThemesEnabled}
  {$ENDIF COMPILER12_UP}
  if UserTextHint and HandleAllocated and
     not Focused and Enabled and not ReadOnly and (Text = '') then
    Invalidate;
end;

procedure TJvExMaskEdit.PaintWindow(DC: HDC);
var
  R: TRect;
  OldFont: HFONT;
  OldTextColor: TColorRef;
begin
  inherited PaintWindow(DC);

  if UserTextHint and (TextHint <> '') and
     Enabled and not ReadOnly and not Focused and (Text = '') then
  begin
    SendMessage(Handle, EM_GETRECT, 0, LPARAM(@R));

    OldFont := SelectObject(DC, Font.Handle);
    OldTextColor := SetTextColor(DC, ColorToRGB(clGrayText));
    DrawText(DC, PChar(TextHint), Length(TextHint), R, DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);

    SetTextColor(DC, OldTextColor);
    SelectObject(DC, OldFont);
  end;
end;

procedure TJvExMaskEdit.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  if UserTextHint and (TextHint <> '') then
    Invalidate;
end;

procedure TJvExMaskEdit.WMSetFocus(var Msg: TWMSetFocus);
begin
  if UserTextHint and (TextHint <> '') then
    Invalidate;
  inherited;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

{$UNDEF CONSTRUCTOR_CODE} // undefine at file end

end.