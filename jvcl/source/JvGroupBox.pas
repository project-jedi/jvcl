{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAlignListbox.PAS, released on 2000-11-22.

The Initial Developer of the Original Code is Peter Below <100113.1101@compuserve.com>
Portions created by Peter Below are Copyright (C) 2000 Peter Below.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2000-mm-dd

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvGroupBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  JVCLVer;

type
  TJvGroupBox = class(TGroupBox)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FOnHotKey: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FHintColor, FSaved: TColor;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChange: TNotifyEvent;
    FOver: Boolean;
    FPropagateEnable: Boolean;
    procedure SetPropagateEnable(const Value: Boolean);
    procedure CMDialogChar(var Msg: TCMDialogChar);
      message CM_DIALOGCHAR;
    procedure CMEnabledChanged(var Msg: TMsg); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Msg: TMsg); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMsg); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMsg); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMsg); message CM_PARENTCOLORCHANGED;
  protected
    procedure DoHotKey; dynamic;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Canvas;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property PropagateEnable: Boolean read FPropagateEnable write SetPropagateEnable default False;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChange write FOnParentColorChange;
    property OnHotKey: TNotifyEvent read FOnHotKey write FOnHotKey;
  end;

implementation

procedure TJvGroupBox.CMCtl3DChanged(var Msg: TMsg);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TJvGroupBox.CMDialogChar(var Msg: TCMDialogChar);
begin
  inherited;
  if Msg.Result <> 0 then
    DoHotKey;
end;

procedure TJvGroupBox.CMEnabledChanged(var Msg: TMsg);
var
  I: Integer;
begin
  inherited;
  if PropagateEnable then
    for I := 0 to ControlCount - 1 do
      Controls[I].Enabled := Enabled;
  Invalidate;
end;

procedure TJvGroupBox.CMMouseEnter(var Msg: TMsg);
begin
  if not FOver then
  begin
    FSaved := Application.HintColor;
    // for D7...
    if csDesigning in ComponentState then
      Exit;
    Application.HintColor := FHintColor;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvGroupBox.CMMouseLeave(var Msg: TMsg);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvGroupBox.CMParentColorChanged(var Msg: TMsg);
begin
  inherited;
  if Assigned(FOnParentColorChange) then
    FOnParentColorChange(Self);
end;

constructor TJvGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHintColor := clInfoBk;
  FOver := False;
  FPropagateEnable := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

procedure TJvGroupBox.DoHotKey;
begin
  if Assigned(FOnHotKey) then
    FOnHotKey(Self);
end;

procedure TJvGroupBox.Paint;
var
  H: Integer;
  R: TRect;
  Flags: Longint;
begin
  with Canvas do
  begin
    Font := Self.Font;
    H := TextHeight('0');
    R := Rect(0, H div 2 - 1, Width, Height);
    if Ctl3D then
    begin
      Inc(R.Left);
      Inc(R.Top);
      Brush.Color := clBtnHighlight;
      FrameRect(R);
      OffsetRect(R, -1, -1);
      Brush.Color := clBtnShadow;
    end
    else
      Brush.Color := clWindowFrame;
    FrameRect(R);
    if Text <> '' then
    begin
      if not UseRightToLeftAlignment then
        R := Rect(8, 0, 0, H)
      else
        R := Rect(R.Right - Canvas.TextWidth(Text) - 8, 0, 0, H);
      Flags := DrawTextBiDiModeFlags(DT_SINGLELINE);
      // calculate text rect
      DrawText(Handle, PChar(Text), Length(Text), R, Flags or DT_CALCRECT);
      Brush.Color := Color;
      if not Enabled then
      begin
        OffsetRect(R, 1, 1);
        Font.Color := clBtnHighlight;
        DrawText(Handle, PChar(Text), Length(Text), R, Flags);
        OffsetRect(R, -1, -1);
        Font.Color := clBtnShadow;
        SetBkMode(Handle, Windows.TRANSPARENT);
        DrawText(Handle, PChar(Text), Length(Text), R, Flags);
      end
      else
        DrawText(Handle, PChar(Text), Length(Text), R, Flags);
    end;
  end;
end;

procedure TJvGroupBox.SetPropagateEnable(const Value: Boolean);
var
  I: Integer;
begin
  FPropagateEnable := Value;
  for I := 0 to ControlCount - 1 do
    Controls[I].Enabled := Enabled;
end;

end.

