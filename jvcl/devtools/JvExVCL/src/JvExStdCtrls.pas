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
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvExStdCtrls;

{$I jvcl.inc}
{MACROINCLUDE JvExControls.macros}

WARNINGHEADER

interface

uses
  Windows, Messages, Types,
  SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvTypes, JvThemes, JVCLVer, JvExControls;

type
  WINCONTROL_DECL_DEFAULT(CustomGroupBox)

  {$DEFINE HASAUTOSIZE}

  CONTROL_DECL_DEFAULT(CustomLabel)

  CONTROL_DECL_DEFAULT(Label)

  {$UNDEF HASAUTOSIZE}

  TJvExCustomEdit = class(TCustomEdit, IJvExControl)
  EDITCONTROL_DECL
  private
    {$IFNDEF COMPILER12_UP}
    FTextHint: string;
    procedure SetTextHint(const Value: string);
    {$ENDIF ~COMPILER12_UP}
    function UserTextHint: Boolean;
  protected
    procedure DoSetTextHint(const Value: string); {$IFDEF COMPILER12_UP}override;{$ELSE}virtual;{$ENDIF}
    procedure PaintWindow(DC: HDC); override;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;

    {$IFNDEF COMPILER12_UP}
    procedure CreateWnd; override;
    property TextHint: string read FTextHint write SetTextHint;
    {$ENDIF ~COMPILER12_UP}
  end;

  EDITCONTROL_DECL_DEFAULT(CustomMemo)

  WINCONTROL_DECL_DEFAULT(CustomCombo)

  TJvExCustomComboBox = class(TCustomComboBox, IJvExControl)
  WINCONTROL_DECL
  private
    {$IFNDEF COMPILER12_UP}
    FTextHint: string;
    procedure SetTextHint(const Value: string);
    {$ENDIF ~COMPILER12_UP}
    function UserTextHint: Boolean;
    function IsEditReadOnly: Boolean;
  protected
    procedure DoSetTextHint; {$IFDEF COMPILER12_UP}override;{$ELSE}virtual;{$ENDIF}
    procedure ComboWndProc(var Message: TMessage; ComboWnd: HWND; ComboProc: Pointer); override;

    {$IFNDEF COMPILER12_UP}
    procedure CreateWnd; override;
    property TextHint: string read FTextHint write SetTextHint;
    {$ENDIF ~COMPILER12_UP}
  end;

  WINCONTROL_DECL_DEFAULT(ButtonControl)

  WINCONTROL_DECL_DEFAULT(Button)

  WINCONTROL_DECL_DEFAULT(CustomCheckBox)

  WINCONTROL_DECL_DEFAULT(RadioButton)

  WINCONTROL_DECL_DEFAULT(CustomListBox)

  WINCONTROL_DECL_DEFAULT(ScrollBar)

  WINCONTROL_DECL_DEFAULT(GroupBox)

  WINCONTROL_DECL_DEFAULT(CheckBox)

  WINCONTROL_DECL_DEFAULT(CustomStaticText)

  WINCONTROL_DECL_DEFAULT(StaticText)

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

WINCONTROL_IMPL_DEFAULT(CustomGroupBox)

{$DEFINE HASAUTOSIZE}

CONTROL_IMPL_DEFAULT(CustomLabel)

CONTROL_IMPL_DEFAULT(Label)

{$UNDEF HASAUTOSIZE}

BEGIN_EDITCONTROL_CONSTRUCTOR(CustomEdit)
  if UserTextHint then
    ControlState := ControlState + [csCustomPaint]; // needed for PaintWindow
END_CONSTRUCTOR

EDITCONTROL_IMPL(CustomEdit)

EDITCONTROL_WNDPROC(CustomEdit)

{$IFNDEF COMPILER12_UP}
procedure TJvExCustomEdit.SetTextHint(const Value: string);
begin
  if FTextHint <> Value then
  begin
    FTextHint := Value;
    if not (csLoading in ComponentState) then
      DoSetTextHint(FTextHint);
  end;
end;

procedure TJvExCustomEdit.CreateWnd;
begin
  inherited CreateWnd;
  DoSetTextHint(FTextHint);
end;
{$ENDIF ~COMPILER12_UP}

function TJvExCustomEdit.UserTextHint: Boolean;
begin
  {$IFDEF JVCLThemesEnabled}
  Result := not (JclCheckWinVersion(5, 1) and StyleServices.Enabled);
  {$ELSE}
  Result := True;
  {$ENDIF JVCLThemesEnabled}
end;

procedure TJvExCustomEdit.DoSetTextHint(const Value: string);
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

procedure TJvExCustomEdit.PaintWindow(DC: HDC);
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

procedure TJvExCustomEdit.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  if UserTextHint and (TextHint <> '') then
    Invalidate;
end;

procedure TJvExCustomEdit.WMSetFocus(var Msg: TWMSetFocus);
begin
  if UserTextHint and (TextHint <> '') then
    Invalidate;
  inherited;
end;

EDITCONTROL_IMPL_DEFAULT(CustomMemo)

WINCONTROL_IMPL_DEFAULT(CustomCombo)

BEGIN_WINCONTROL_CONSTRUCTOR(CustomComboBox)
  if UserTextHint then
    ControlState := ControlState + [csCustomPaint]; // needed for PaintWindow
END_CONSTRUCTOR

WINCONTROL_IMPL(CustomComboBox)

WINCONTROL_WNDPROC(CustomComboBox)

{$IFNDEF COMPILER12_UP}
procedure TJvExCustomComboBox.SetTextHint(const Value: string);
begin
  if FTextHint <> Value then
  begin
    FTextHint := Value;
    if not (csLoading in ComponentState) then
      DoSetTextHint;
  end;
end;

procedure TJvExCustomComboBox.CreateWnd;
begin
  inherited CreateWnd;
  DoSetTextHint;
end;
{$ENDIF ~COMPILER12_UP}

function TJvExCustomComboBox.UserTextHint: Boolean;
begin
  {$IFDEF JVCLThemesEnabled}
  Result := not (JclCheckWinVersion(5, 1) and StyleServices.Enabled);
  {$ELSE}
  Result := True;
  {$ENDIF JVCLThemesEnabled}
end;

function TJvExCustomComboBox.IsEditReadOnly: Boolean;
begin
  Result := HandleAllocated and (GetWindowLong(FEditHandle, GWL_STYLE) and ES_READONLY <> 0);
end;

procedure TJvExCustomComboBox.DoSetTextHint;
begin
  {$IFDEF COMPILER12_UP}
  inherited DoSetTextHint;
  {$ELSE}
  {$IFDEF JVCLThemesEnabled}
  if StyleServices.Enabled and HandleAllocated then
  begin
    if JclCheckWinVersion(6, 0) then
      SendMessage(Handle, CB_SETCUEBANNER, WPARAM(0), LPARAM(PWideChar(UnicodeString(FTextHint))))
    else if JclCheckWinVersion(5, 1) then
      SendMessage(FEditHandle, EM_SETCUEBANNER, WPARAM(0), LPARAM(PWideChar(UnicodeString(FTextHint))));
  end;
  {$ENDIF JVCLThemesEnabled}
  {$ENDIF COMPILER12_UP}
  if UserTextHint and HandleAllocated and
     not Focused and Enabled and not IsEditReadOnly and (Text = '') then
    Invalidate;
end;

procedure TJvExCustomComboBox.ComboWndProc(var Message: TMessage; ComboWnd: HWND; ComboProc: Pointer);

  procedure EditPaintWindow(DC: HDC);
  var
    R: TRect;
    OldFont: HFONT;
    OldTextColor: TColorRef;
  begin
    if UserTextHint and (TextHint <> '') and
       Enabled and not IsEditReadOnly and not Focused and (Text = '') then
    begin
      SendMessage(FEditHandle, EM_GETRECT, 0, LPARAM(@R));

      OldFont := SelectObject(DC, Font.Handle);
      OldTextColor := SetTextColor(DC, ColorToRGB(clGrayText));
      DrawText(DC, PChar(TextHint), Length(TextHint), R, DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);

      SetTextColor(DC, OldTextColor);
      SelectObject(DC, OldFont);
    end;
  end;

var
  DC: HDC;
  PaintStruct: TPaintStruct;
begin
  DC := 0;
  try
    if (ComboWnd = FEditHandle) and UserTextHint and (Message.Msg = WM_PAINT) then
    begin
      if TWMPaint(Message).DC = 0 then
      begin
        DC := BeginPaint(ComboWnd, PaintStruct);
        TWMPaint(Message).DC := DC;
      end;
    end;

    inherited ComboWndProc(Message, ComboWnd, ComboProc);

    if ComboWnd = FEditHandle then
    begin
      case Message.Msg of
        WM_PAINT:
          EditPaintWindow(DC);

        WM_PRINTCLIENT:
          EditPaintWindow(HDC(Message.WParam));

        WM_KILLFOCUS, WM_SETFOCUS:
          if UserTextHint and (TextHint <> '') then
            Invalidate;
      end;
    end;
  finally
    if DC <> 0 then
    begin
      EndPaint(ComboWnd, PaintStruct);
      TWMPaint(Message).DC := 0;
    end;
  end;
end;

WINCONTROL_IMPL_DEFAULT(ButtonControl)

WINCONTROL_IMPL_DEFAULT(Button)

WINCONTROL_IMPL_DEFAULT(CustomCheckBox)

WINCONTROL_IMPL_DEFAULT(RadioButton)

WINCONTROL_IMPL_DEFAULT(CustomListBox)

WINCONTROL_IMPL_DEFAULT(ScrollBar)

WINCONTROL_IMPL_DEFAULT(GroupBox)

WINCONTROL_IMPL_DEFAULT(CheckBox)

WINCONTROL_IMPL_DEFAULT(CustomStaticText)

WINCONTROL_IMPL_DEFAULT(StaticText)

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.