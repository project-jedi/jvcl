{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvScrollBox.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I vclonly.inc}

unit JvScrollBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  JvExControls, JvExForms;

type
  TJvScrollBox = class(TJvExScrollBox)
  private
    FHotTrack: Boolean;
    FOnHorizontalScroll: TNotifyEvent;
    FOnVerticalScroll: TNotifyEvent;
    procedure SetHotTrack(const Value: Boolean);
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure WndProc(var Msg: TMessage); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property HintColor;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property OnVerticalScroll: TNotifyEvent read FOnVerticalScroll write FOnVerticalScroll;
    property OnHorizontalScroll: TNotifyEvent read FOnHorizontalScroll write FOnHorizontalScroll;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property TabStop;
  end;

implementation

uses
  JvThemes;

constructor TJvScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHotTrack := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
  IncludeThemeStyle(Self, [csNeedsBorderPaint]);
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

procedure TJvScrollBox.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not MouseOver then
  begin
    if FHotTrack then
      Ctl3D := True;
    inherited MouseEnter(Control);
  end;
end;

procedure TJvScrollBox.MouseLeave(Control: TControl);
begin
  if MouseOver then
  begin
    if FHotTrack then
      Ctl3D := False;
    inherited MouseLeave(Control);
  end;
end;

procedure TJvScrollBox.SetHotTrack(const Value: Boolean);
begin
  FHotTrack := Value;
  if Value then
    Ctl3D := False;
end;

procedure TJvScrollBox.DoGetDlgCode(var Code: TDlgCodes);
begin
  Code := [dcWantAllKeys, dcWantArrows];
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

