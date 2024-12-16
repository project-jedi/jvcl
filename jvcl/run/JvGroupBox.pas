{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvGroupBox.PAS, released on 2000-11-22.

The Initial Developer of the Original Code is Peter Below <100113 dott 1101 att compuserve dott com>
Portions created by Peter Below are Copyright (C) 2000 Peter Below.
All Rights Reserved.

Contributor(s):
  Roman Ganz
  Robert Marquardt

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvGroupBox;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Windows, Messages, Graphics, Controls, Forms, StdCtrls,
  JvThemes, JvExControls, JvExStdCtrls, JvCheckBox, JvJCLUtils;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvGroupBox = class(TJvExGroupBox, IJvDenySubClassing)
  private
    FCheckBox: TJvCheckBox;
    FOnHotKey: TNotifyEvent;
    FPropagateEnable: Boolean;
    FCheckable: Boolean;
    FOnCheckBoxClick: TNotifyEvent;
    procedure SetPropagateEnable(const Value: Boolean);
    procedure SetCheckable(const Value: Boolean);
    function GetCaption: TCaption;
    procedure SetCaption(const Value: TCaption);
    function GetChecked: Boolean;
    procedure SetChecked(const Value: Boolean);
    function StoredCheckable: Boolean;
    procedure CheckBoxClick(Sender: TObject);
  protected
    function WantKey(Key: Integer; Shift: TShiftState): Boolean; override;
    procedure EnabledChanged; override;
    procedure DoHotKey; dynamic;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Canvas;
  published
    property HintColor;
    {$IFDEF JVCLThemesEnabledD6}
    property ParentBackground default True;
    {$ENDIF JVCLThemesEnabledD6}
    property Caption: TCaption read GetCaption write SetCaption;
    property Checkable: Boolean read FCheckable write SetCheckable default False;
    property Checked: Boolean read GetChecked write SetChecked stored StoredCheckable;
    property PropagateEnable: Boolean read FPropagateEnable write SetPropagateEnable default False;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property OnHotKey: TNotifyEvent read FOnHotKey write FOnHotKey;
    property OnCheckBoxClick: TNotifyEvent read FOnCheckBoxClick write FOnCheckBoxClick; 
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
  Math;

constructor TJvGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPropagateEnable := False;
  FCheckable := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
  {$IFDEF JVCLThemesEnabledD6}
  IncludeThemeStyle(Self, [csParentBackground]);
  {$ENDIF JVCLThemesEnabledD6}
end;

procedure TJvGroupBox.Paint;
var
  H: Integer;
  R: TRect;
  Flags: Longint;
  {$IFDEF JVCLThemesEnabledD6}
  Details: TThemedElementDetails;
  CaptionRect: TRect;
  {$ENDIF JVCLThemesEnabledD6}
  LastBkMode: Integer;
begin
  {$IFDEF JVCLThemesEnabled}
  if StyleServices.Enabled then
  begin
    {$IFDEF COMPILER7_UP}
    inherited Paint;
    {$ELSE}
    if Enabled then
      Details := StyleServices.GetElementDetails(tbGroupBoxNormal)
    else
      Details := StyleServices.GetElementDetails(tbGroupBoxDisabled);
    R := ClientRect;
    Inc(R.Top, Canvas.TextHeight('0') div 2);
    StyleServices.DrawElement(Canvas.Handle, Details, R);

    CaptionRect := Rect(8, 0, Min(Canvas.TextWidth(Caption) + 8, ClientWidth - 8),
      Canvas.TextHeight(Caption));

    Canvas.Brush.Color := Self.Color;
    DrawThemedBackground(Self, Canvas, CaptionRect);
    StyleServices.DrawText(Canvas.Handle, Details, Caption, CaptionRect, DT_LEFT, 0);
    {$ENDIF COMPILER7_UP}
    Exit;
  end;
  {$ENDIF JVCLThemesEnabled}
  with Canvas do
  begin
    LastBkMode := GetBkMode(Handle);
    try
      Font := Self.Font;
      H := TextHeight('0');
      R := Rect(0, H div 2 - 1, Width, Height);
      if Ctl3D then
      begin
        Inc(R.Left);
        Inc(R.Top);
        Brush.Color := clBtnHighlight;
        FrameRect( R);
        OffsetRect(R, -1, -1);
        Brush.Color := clBtnShadow;
      end
      else
        Brush.Color := clWindowFrame;
      FrameRect( R);
      if Text <> '' then
      begin
        if not UseRightToLeftAlignment then
          R := Rect(8, 0, 0, H)
        else
          R := Rect(R.Right - Canvas.TextWidth(Text) - 8, 0, 0, H);
        Flags := DrawTextBiDiModeFlags(DT_SINGLELINE);
        // calculate text rect
        SetBkMode(Handle, OPAQUE);
        DrawText(Handle, Text, Length(Text), R, Flags or DT_CALCRECT);
        Brush.Color := Color;
        if not Enabled then
        begin
          OffsetRect(R, 1, 1);
          Font.Color := clBtnHighlight;
          DrawText(Canvas, Text, Length(Text), R, Flags);
          OffsetRect(R, -1, -1);
          Font.Color := clBtnShadow;
          SetBkMode(Handle, TRANSPARENT);
          DrawText(Canvas, Text, Length(Text), R, Flags);
        end
        else
          DrawText(Canvas, Text, Length(Text), R, Flags);
      end;
    finally
      SetBkMode(Handle, LastBkMode);
    end;
  end;
end;

function TJvGroupBox.WantKey(Key: Integer; Shift: TShiftState): Boolean;
begin
  Result := inherited WantKey(Key, Shift);
  if Result then
    DoHotKey;
end;

procedure TJvGroupBox.EnabledChanged;
var
  I: Integer;
begin
  inherited EnabledChanged;
  if PropagateEnable then
    for I := 0 to ControlCount - 1 do
      if Checkable then
        if Enabled then
          if Controls[I] = FCheckBox then
            Controls[I].Enabled := True
          else
            Controls[I].Enabled := Checked
        else
          Controls[I].Enabled := False
      else
        Controls[I].Enabled := Enabled;
  Invalidate;
end;

procedure TJvGroupBox.DoHotKey;
begin
  if Assigned(FOnHotKey) then
    FOnHotKey(Self);
end;

function TJvGroupBox.GetCaption: TCaption;
begin
  if FCheckable then
    Result := FCheckBox.Caption
  else
    Result := inherited Caption;
end;

function TJvGroupBox.GetChecked: Boolean;
begin
  if FCheckable then
    Result := FCheckBox.Checked
  else
    Result := False;
end;

procedure TJvGroupBox.SetCaption(const Value: TCaption);
begin
  if FCheckable then
    FCheckBox.Caption := Value
  else
    inherited Caption := Value;
end;

procedure TJvGroupBox.SetCheckable(const Value: Boolean);
begin
  if FCheckable <> Value then
  begin
    if Value then
    begin
      FCheckBox := TJvCheckBox.Create(Self);
      FCheckBox.Parent := Self;
      FCheckBox.Top := 0;
      FCheckBox.Left := 8;
      FCheckBox.Caption := Caption;
      PropagateEnable := True;
      FCheckBox.OnClick := CheckBoxClick;
      FCheckBox.Checked := True;
      inherited Caption := '';
    end
    else
    begin
      inherited Caption := FCheckBox.Caption;
      FreeAndNil(FCheckBox);
    end;
    FCheckable := Value;
  end;
end;

procedure TJvGroupBox.SetChecked(const Value: Boolean);
begin
  if Checkable then
    FCheckBox.Checked := Value;
end;

procedure TJvGroupBox.CheckBoxClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
    if Controls[I] <> FCheckBox then
      Controls[I].Enabled := FCheckBox.Checked;
      
  if Assigned(FOnCheckBoxClick) then
    FOnCheckBoxClick(Self);
end;

procedure TJvGroupBox.SetPropagateEnable(const Value: Boolean);
var
  I: Integer;
begin
  FPropagateEnable := Value;
  for I := 0 to ControlCount - 1 do
    Controls[I].Enabled := Enabled;
end;

function TJvGroupBox.StoredCheckable: Boolean;
begin
  { Write "False" to the DFM file because the checkbox is initialized with "True" }
  Result := FCheckable and not Checked;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
