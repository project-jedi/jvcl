{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvScrollBox.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvScrollBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  JVCLVer;

type
  TJvScrollBox = class(TScrollBox)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FHintColor: TColor;
    FSaved: TColor;
    FHotTrack: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOver: Boolean;
    FOnHorizontalScroll: TNotifyEvent;
    FOnVerticalScroll: TNotifyEvent;
    procedure SetHotTrack(const Value: Boolean);
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
  protected
    procedure WndProc(var Msg: TMessage); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnVerticalScroll: TNotifyEvent read FOnVerticalScroll write FOnVerticalScroll;
    property OnHorizontalScroll: TNotifyEvent read FOnHorizontalScroll write FOnHorizontalScroll;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyup;
    property TabStop;
  end;

implementation

constructor TJvScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHintColor := clInfoBk;
  FHotTrack := False;
  FOver := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

procedure TJvScrollBox.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TJvScrollBox.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvScrollBox.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  if Assigned(FOnHorizontalScroll) then
    FOnHorizontalScroll(Self);
end;

procedure TJvScrollBox.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  if Assigned(FOnVerticalScroll) then
    FOnVerticalScroll(Self);
end;

procedure TJvScrollBox.CMMouseEnter(var Msg: TMessage);
begin
  if not FOver then
  begin
    FSaved := Application.HintColor;
    // for D7...
    if csDesigning in ComponentState then
      Exit;
    Application.HintColor := FHintColor;
    if FHotTrack then
      Ctl3D := True;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvScrollBox.CMMouseLeave(var Msg: TMessage);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    if FHotTrack then
      Ctl3D := False;
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvScrollBox.SetHotTrack(const Value: Boolean);
begin
  FHotTrack := Value;
  if Value then
    Ctl3D := False;
end;

procedure TJvScrollBox.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS;
end;

procedure TJvScrollBox.WndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_LBUTTONDOWN then
    if not Focused and not (csDesigning in ComponentState) then
      SetFocus;
  inherited WndProc(Msg);
end;

procedure TJvScrollBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key <> 0 then
    case Key of
      VK_UP:
        Perform(WM_VSCROLL, SB_LINEUP, 0);
      VK_DOWN:
        Perform(WM_VSCROLL, SB_LINEDOWN, 0);
      VK_LEFT:
        Perform(WM_HSCROLL, SB_LINELEFT, 0);
      VK_RIGHT:
        Perform(WM_HSCROLL, SB_LINERIGHT, 0);
      VK_NEXT:
        if ssShift in Shift then
          Perform(WM_HSCROLL, SB_PAGERIGHT, 0)
        else
          Perform(WM_VSCROLL, SB_PAGEDOWN, 0);
      VK_PRIOR:
        if ssShift in Shift then
          Perform(WM_HSCROLL, SB_PAGELEFT, 0)
        else
          Perform(WM_VSCROLL, SB_PAGEUP, 0);
      VK_HOME:
        if ssCtrl in Shift then
          Perform(WM_VSCROLL, SB_TOP, 0)
        else
          Perform(WM_HSCROLL, SB_LEFT, 0);
      VK_END:
        if ssCtrl in Shift then
          Perform(WM_VSCROLL, SB_BOTTOM, 0)
        else
          Perform(WM_HSCROLL, SB_RIGHT, 0);
    end;
end;

end.

