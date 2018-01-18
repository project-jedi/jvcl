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
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvScrollBox;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  JvExControls, JvExForms, JvJVCLUtils;

type
  TEraseBackgroundEvent = procedure(Sender: TObject; Canvas: TCanvas; var Result: Boolean) of object;

  TJvScrollBoxFillMode = (sfmTile, sfmStretch, sfmNone);

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvScrollBox = class(TJvExScrollBox)
  private
    FHotTrack: Boolean;
    FOnHorizontalScroll: TNotifyEvent;
    FOnVerticalScroll: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    FCanvas: TCanvas;
    FOnEraseBackground: TEraseBackgroundEvent;
    FBackground: TJvPicture;
    FBackgroundFillMode: TJvScrollBoxFillMode;
    FLockRefreshCount : Integer;

    procedure SetHotTrack(const Value: Boolean);
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure SetBackground(const Value: TPicture);
    procedure SetBackgroundFillMode(const Value: TJvScrollBoxFillMode);
    function GetBackground: TPicture;
  protected
    procedure BoundsChanged; override;
    procedure GetDlgCode(var Code: TDlgCodes); override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure WndProc(var Msg: TMessage); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure PaintWindow(DC: HDC); override;
    procedure Paint; virtual;
    function DoEraseBackground(ACanvas: TCanvas; Param: LPARAM): Boolean; override;
    procedure PaintBackground(ACanvas: TCanvas);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    property Canvas: TCanvas read FCanvas;
  published
    property Background: TPicture read GetBackground write SetBackground;
    property BackgroundFillMode: TJvScrollBoxFillMode read FBackgroundFillMode write SetBackgroundFillMode default sfmTile;
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
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnEraseBackground: TEraseBackgroundEvent read FOnEraseBackground write FOnEraseBackground;
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
  JvThemes;

procedure TJvScrollBox.BoundsChanged;
var R: TRect;
begin
  inherited BoundsChanged;
  R := ClientRect;
  InvalidateRect(self.Handle, @R, false);
end;

constructor TJvScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHotTrack := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
  IncludeThemeStyle(Self, [csNeedsBorderPaint]);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;

  // We use a TJvPicture to allow silent migration from TJvgScrollBox
  // where background was a TBitmap.
  FBackground := TJvPicture.Create;
  FBackgroundFillMode := sfmTile;

  FLockRefreshCount := 0;
end;

destructor TJvScrollBox.Destroy;
begin
  FCanvas.Free;
  FBackground.Free;
  inherited Destroy;
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

procedure TJvScrollBox.SetBackground(const Value: TPicture);
begin
  FBackground.Assign(Value);
  Invalidate;
end;

procedure TJvScrollBox.SetBackgroundFillMode(const Value: TJvScrollBoxFillMode);
begin
  if FBackgroundFillMode <> Value then
  begin
    FBackgroundFillMode := Value;
    Invalidate;
  end;
end;

function TJvScrollBox.GetBackground: TPicture;
begin
  // Required because FBackground is a TJvPicture and as such cannot be
  // used directly in the property declaration.
  Result := FBackground;
end;

procedure TJvScrollBox.GetDlgCode(var Code: TDlgCodes);
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

procedure TJvScrollBox.PaintWindow(DC: HDC);
begin
  FCanvas.Lock;
  try
    FCanvas.Handle := DC;
    try
      TControlCanvas(FCanvas).UpdateTextFlags;
      Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
end;

procedure TJvScrollBox.WMPaint(var Msg: TWMPaint);
begin
  ControlState := ControlState + [csCustomPaint];
  inherited;
  ControlState := ControlState - [csCustomPaint];
end;

function TJvScrollBox.DoEraseBackground(ACanvas: TCanvas; Param: LPARAM): Boolean;
begin
  Result := False;
  if Assigned(FOnEraseBackground) then
    FOnEraseBackground(Self, ACanvas, Result);
  if not Result then
    Result := inherited DoEraseBackground(ACanvas, Param);

  PaintBackground(ACanvas);
end;

procedure TJvScrollBox.Paint;
begin
  if Assigned(FOnPaint) then
    FOnPaint(Self);
end;

procedure TJvScrollBox.PaintBackground(ACanvas: TCanvas);
var
  R: TRect;
  X: Integer;
  Y: Integer;
  BackgroundHeight: Integer;
  BackgroundWidth: Integer;
  XOffset: Integer;
  YOffset: Integer;
  SavedYOffset: Integer;
begin
  if Assigned(Background.Graphic) and not Background.Graphic.Empty then
  begin
    case BackgroundFillMode of
      sfmTile:
        begin
          R := ClientRect;
          BackgroundHeight := FBackground.Height;
          BackgroundWidth := FBackground.Width;

          XOffset := HorzScrollBar.Position - Trunc(HorzScrollBar.Position / BackgroundWidth) * BackgroundWidth;
          YOffset := VertScrollBar.Position - Trunc(VertScrollBar.Position / BackgroundHeight) * BackgroundHeight;
          SavedYOffset := YOffset;
          X := R.Left;
          while X < R.Right do
          begin
            Y := R.Top;
            while Y < R.Bottom do
            begin
              ACanvas.Draw(X - XOffset, Y - YOffset, Background.Graphic);

              Inc(Y, BackgroundHeight - YOffset);
              YOffset := 0;
            end;
            Inc(X, BackgroundWidth - XOffset);
            XOffset := 0;
            YOffset := SavedYOffset;
          end;
        end;
      sfmStretch:
        begin
          R := ClientRect;
          if HorzScrollBar.Range > R.Right then
            R.Right := HorzScrollBar.Range - R.Left;
          if VertScrollBar.Range > R.Bottom then
            R.Bottom := VertScrollBar.Range - R.Top;
          OffsetRect(R, -HorzScrollBar.Position, -VertScrollBar.Position);

          ACanvas.StretchDraw(R, Background.Graphic);
        end;
      sfmNone:
        begin
          ACanvas.Draw(0, 0, Background.Graphic);
        end;
    end;
  end;
end;

procedure TJvScrollBox.BeginUpdate;
begin
  if FLockRefreshCount = 0 then
    SendMessage(Handle, WM_SETREDRAW, Ord(False), 0);

  Inc(FLockRefreshCount);
end;

procedure TJvScrollBox.EndUpdate;
begin
  Dec(FLockRefreshCount);
  if FLockRefreshCount = 0 then
  begin
    SendMessage(Handle, WM_SETREDRAW, Ord(True), 0);
    RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME or RDW_ALLCHILDREN);
  end;
end;

{$IFDEF UNITVERSIONING}

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
