{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDropDownForm, released on 2002-10-04.

The Initial Developer of the Original Code is Oliver Giesen [giesen att lucatec dott com]
Portions created by Oliver Giesen are Copyright (C) 2002 Lucatec GmbH.
All Rights Reserved.

Contributor(s):
  Andreas Hausladen

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  A generic container form to be displayed as dropdown below a TCustomEdit
  descendant.

  There's still plenty of room for improvement here.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDropDownForm;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages,
  Classes, Controls, StdCtrls, Forms,
  JvTypes, JvExForms;

type
  TJvCustomDropDownForm = class(TJvExCustomForm)
  private
    FEntering: Boolean;
    FCloseOnLeave: Boolean;
    FLeaving: Boolean;
    FOnKillFocus: TJvFocusChangeEvent;
    FOnSetFocus: TJvFocusChangeEvent;
  protected
    function GetEdit: TCustomEdit;
    procedure FocusSet(PrevWnd: THandle); override;
    procedure FocusKilled(NextWnd: THandle); override;
    procedure DoFocusSet(const APreviousControl: TWinControl); dynamic;
    procedure DoFocusKilled(const ANextControl: TWinControl); dynamic;
    procedure DoClose(var Action: TCloseAction); override;
    procedure DoShow; override;
    procedure CreateParams(var AParams: TCreateParams); override;
    property Edit: TCustomEdit read GetEdit;
  public
    constructor Create(AOwner: TComponent); override;
    property CloseOnLeave: Boolean read FCloseOnLeave write FCloseOnLeave; // (ahuser) meight have no function under VisualCLX
    property Entering: Boolean read FEntering;
    property Leaving: Boolean read FLeaving;
    property OnSetFocus: TJvFocusChangeEvent read FOnSetFocus write FOnSetFocus;
    property OnKillFocus: TJvFocusChangeEvent read FOnKillFocus write FOnKillFocus;
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
  SysUtils,
  JvResources, JvJVCLUtils, JclSysInfo;

type
  TCustomEditAccessProtected = class(TCustomEdit);

constructor TJvCustomDropDownForm.Create(AOwner: TComponent);
begin
  if not (AOwner is TCustomEdit) then
    raise EJVCLException.CreateRes(@RsETJvCustomDropDownFormCreateOwnerMus);

  inherited CreateNew(AOwner);

  BorderIcons := [];
  BorderStyle := bsNone;
  Font := TCustomEditAccessProtected(AOwner).Font;

  Position := poDesigned; // required for D2005
  FEntering := True;
  FLeaving := False;
  FCloseOnLeave := True;
end;

procedure TJvCustomDropDownForm.CreateParams(var AParams: TCreateParams);
{$IFDEF COMPILER6}
const
  CS_DROPSHADOW = $20000;
{$ENDIF COMPILER6}
begin
  inherited CreateParams(AParams);
  AParams.Style := AParams.Style or WS_BORDER;
  AParams.ExStyle := AParams.ExStyle or WS_EX_TOOLWINDOW;
  AddBiDiModeExStyle(AParams.ExStyle);

  AParams.WindowClass.style := AParams.WindowClass.style or CS_SAVEBITS;
  if JclCheckWinVersion(5, 1) then // Windows XP+
    AParams.WindowClass.style := AParams.WindowClass.style or CS_DROPSHADOW;

  // Fixing the Window Ghosting "bug"
  // This also fixes mantis 3409 where the popup would not appear if its
  // associated control was placed on a form with fsStayOnTop form style.
  if Assigned(Screen.ActiveForm) then
    AParams.WndParent := Screen.ActiveForm.Handle
  else
  if Assigned (Application.MainForm) then
    AParams.WndParent := Application.MainForm.Handle
  else
    AParams.WndParent := Application.Handle;
end;

procedure TJvCustomDropDownForm.DoClose(var Action: TCloseAction);
begin
  Action := caFree;
  inherited DoClose(Action);
end;

procedure TJvCustomDropDownForm.DoShow;
var
  WR, LScreenRect: TRect;
  X, Y: Integer;
begin
  inherited DoShow;

  // Mantis 3357: Always reposition ourselves with respect to the owner
  // as it may have moved between two of our apparitions.
  GetWindowRect(TWinControl(Owner).Handle, WR);
  X := WR.Left;
  Y := WR.Bottom;

  if Screen.MonitorCount > 0 then
  begin
    LScreenRect := Monitor.WorkareaRect;
    if X + Width > LScreenRect.Right then
      X := LScreenRect.Right - Width;
    if Y + Height > LScreenRect.Bottom then
      Y := WR.Top - Height;
  end
  else
  begin
    if not SystemParametersInfo(SPI_GETWORKAREA, 0, @LScreenRect, 0) then
      LScreenRect := Rect(0, 0, Screen.Width, Screen.Height);
    if Left + Width > LScreenRect.Right then
      X := LScreenRect.Right - Width;
    if Top + Height > LScreenRect.Bottom then
      Y := WR.Top - Height;
  end;
  SetBounds(X, Y, Width, Height);
end;

function TJvCustomDropDownForm.GetEdit: TCustomEdit;
begin
  Result := TCustomEdit(Owner);
end;

procedure TJvCustomDropDownForm.FocusKilled(NextWnd: THandle);
begin
  if IsChildWindow(NextWnd, Self.Handle) then
    inherited FocusKilled(NextWnd)
  else
  begin
    FLeaving := True;
    try
      inherited FocusKilled(NextWnd);
      DoFocusKilled(FindControl(NextWnd));
    finally
      FLeaving := False;
    end;
  end;
end;

procedure TJvCustomDropDownForm.DoFocusKilled(const ANextControl: TWinControl);
begin
  if Assigned(FOnKillFocus) then
    FOnKillFocus(Self, ANextControl);
  if CloseOnLeave then
    Close;
end;

procedure TJvCustomDropDownForm.FocusSet(PrevWnd: THandle);
begin
  if IsChildWindow(PrevWnd, Self.Handle) then
    inherited FocusSet(PrevWnd)
  else
  begin
    FEntering := True;
    try
      inherited FocusSet(PrevWnd);
      DoFocusSet(FindControl(PrevWnd));
    finally
      FEntering := False;
    end;
  end;
end;

procedure TJvCustomDropDownForm.DoFocusSet(const APreviousControl: TWinControl);
begin
  if Assigned(FOnSetFocus) then
    FOnSetFocus(Self, APreviousControl);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
