{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCaptionPanel.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thrnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thrnqvist are Copyright  1997-2002 Peter Thrnqvist.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  TJvCaptionPanel is a panel that looks like a form, with a Caption area,
  system buttons but is derived from a normal panel.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvCaptionPanel;

{$I jvcl.inc}

interface

// Define JVCAPTIONPANEL_STD_BEHAVE to not use the previous undocumented WM_SYSCOMMAND with SC_DRAGMOVE but instead handle
// the dragging "manually" within the control. Defining this means that you actually get the Mouse events
// and the OnEndAutoDrag event. Additionally, the form displays scrollbars as expected when the component is dragged
// The downside is that the control "flashes" more when it's dragged
{$DEFINE JVCAPTIONPANEL_STD_BEHAVE}



uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages,
  SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  JvComponent, JvExtComponent, JvExControls;

type
  TJvCapBtnStyle = (capClose, capMax, capMin, capRestore, capHelp);
  TJvCapBtnStyles = set of TJvCapBtnStyle;
  TJvDrawPosition = (dpLeft, dpTop, dpRight, dpBottom);
  TJvCapBtnEvent = procedure(Sender: TObject; Button: TJvCapBtnStyle) of object;
  TJvAutoDragStartEvent = procedure(Sender: TObject; var AllowDrag: Boolean) of object;
  { internal class }

  TJvCapBtn = class(TJvGraphicControl)
  private
    FOwner: TComponent;
    FStyle: TJvCapBtnStyle;
    FMouseDown: Boolean;
    FDown: Boolean;
    FFlat: Boolean;
    FOver: Boolean;
    procedure SetFlat(Value: Boolean);
    procedure SetStyle(Value: TJvCapBtnStyle);
    procedure BtnClick;
  protected
    procedure Click; override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Style: TJvCapBtnStyle read FStyle write SetStyle default capClose;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Visible default False;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvCaptionPanel = class(TJvCustomPanel, IJvDenySubClassing)
  private
    FButtonArray: array [TJvCapBtnStyle] of TJvCapBtn;
    FButtonClick: TJvCapBtnEvent;
    FCaptionPosition: TJvDrawPosition;
    FOffset: Integer;
    FButtons: TJvCapBtnStyles;
    FAutoDrag: Boolean;
    FMouseDown: Boolean;
    FCaptionRect: TRect;
    FCaption: string;
    FCaptionColor: TColor;
    FFlat: Boolean;
    FBevel: Integer;
    FDragging: Boolean;
    FEndDrag: TNotifyEvent;
    FCaptionFont: TFont;
    FOnStartAutoDrag: TJvAutoDragStartEvent;
    FOutlookLook: Boolean;
    FCaptionOffsetSmall: Integer;
    FCaptionOffsetLarge: Integer;
    FIcon: TIcon;
    {$IFDEF JVCAPTIONPANEL_STD_BEHAVE}
    FAnchorPos: TPoint;
    {$ENDIF JVCAPTIONPANEL_STD_BEHAVE}
    FResizable: Boolean;
    FCaptionHeight: Integer;
    procedure SetIcon(Value: TIcon);
    procedure SetCaptionFont(Value: TFont);
    procedure SetCaptionColor(Value: TColor);
    procedure SetFlat(Value: Boolean);
    procedure SetButtons(Value: TJvCapBtnStyles);
    procedure SetCaption(Value: string);
    procedure SetCaptionPosition(Value: TJvDrawPosition);
    procedure DrawRotatedText(Rotation: Integer);
    procedure DrawButtons;
    procedure WMNCLButtonUp(var Msg: TWMNCLButtonUp); message WM_NCLBUTTONUP;
    procedure SetResizable(const Value: Boolean);
    procedure SetOutlookLook(const Value: Boolean);
    procedure DoCaptionFontChange(Sender: TObject);
    procedure SetCaptionHeight(const Value: Integer);
  protected
    procedure Paint; override;
    procedure Resize; override;
    function GetEffectiveCaptionHeight: Integer;

    procedure AlignControls(AControl: TControl; var R: TRect); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure ClickButton(Button: TJvCapBtnStyle); virtual;
    function CanStartDrag: Boolean; virtual;
    procedure DoLeaveDrag; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property AutoDrag: Boolean read FAutoDrag write FAutoDrag default True;
    property Buttons: TJvCapBtnStyles read FButtons write SetButtons;
    property BorderStyle default bsSingle;
    property Caption: string read FCaption write SetCaption;
    property CaptionColor: TColor read FCaptionColor write SetCaptionColor default clActiveCaption;
    property CaptionPosition: TJvDrawPosition read FCaptionPosition write SetCaptionPosition default dpLeft;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property CaptionHeight: Integer read FCaptionHeight write SetCaptionHeight default 0;
    property Color;
    property Cursor;
    property DragCursor;
    property FullRepaint;
    property Locked;
    property DragMode;
    property Enabled;
    property FlatButtons: Boolean read FFlat write SetFlat default False;
    property Font;
    property Hint;
    property Icon: TIcon read FIcon write SetIcon;
    property OutlookLook: Boolean read FOutlookLook write SetOutlookLook;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Resizable:Boolean read FResizable write SetResizable default True;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnButtonClick: TJvCapBtnEvent read FButtonClick write FButtonClick;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartAutoDrag: TJvAutoDragStartEvent read FOnStartAutoDrag write FOnStartAutoDrag;
    property OnEndAutoDrag: TNotifyEvent read FEndDrag write FEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnResize;
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
  JvJCLUtils;

//=== { TJvCapBtn } ==========================================================

constructor TJvCapBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
  Width := GetSystemMetrics(SM_CYCAPTION) - 3;
  Height := Width - 2;
  FStyle := capClose;
  Visible := False;
  FFlat := False;
end;

procedure TJvCapBtn.BtnClick;
begin
  if FOwner is TJvCaptionPanel then
    TJvCaptionPanel(FOwner).ClickButton(Style);
end;

procedure TJvCapBtn.SetFlat(Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TJvCapBtn.SetStyle(Value: TJvCapBtnStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Invalidate;
  end;
end;

procedure TJvCapBtn.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not Enabled then
    Exit;
  inherited MouseDown(Button, Shift, X, Y);
  if not FMouseDown then
  begin
    FMouseDown := True;
    FDown := True;
    Repaint;
  end;
end;

procedure TJvCapBtn.Click;
begin
  inherited Click;
  BtnClick;
end;

procedure TJvCapBtn.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not Enabled then
    Exit;
  inherited MouseUp(Button, Shift, X, Y);
  if FMouseDown then
  begin
    FMouseDown := False;
    FDown := False;
    Repaint;
  end;
end;

procedure TJvCapBtn.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if FMouseDown then
  begin
    FOver := PtInRect(ClientRect, Point(X, Y));
    if not FOver then
    begin
      if FDown then { mouse has slid off, so release }
      begin
        FDown := False;
        Repaint;
      end;
    end
    else
    begin
      if not FDown then { mouse has slid back on, so push }
      begin
        FDown := True;
        Repaint;
      end;
    end;
  end;
end;

procedure TJvCapBtn.MouseEnter(Control: TControl);
var
  R: TRect;
begin
  if csDesigning in ComponentState then
    Exit;
  if not FOver then
  begin
    FOver := True;
    if FFlat then
    begin
      R := ClientRect;
      if FDown then
        Frame3D(Canvas, R, clBtnShadow, clBtnHighLight, 1)
      else
        Frame3D(Canvas, R, clBtnHighLight, clBtnShadow, 1);
    end;
    inherited MouseEnter(Control);
  end;
end;

procedure TJvCapBtn.MouseLeave(Control: TControl);
var
  R: TRect;
begin
  if FOver then
  begin
    FOver := False;
    if FFlat then
    begin
      R := ClientRect;
      Frame3D(Canvas, R, clBtnFace, clBtnFace, 1);
    end;
    inherited MouseLeave(Control);
  end;
end;

procedure TJvCapBtn.Paint;
var
  Flags: Integer;
  R: TRect;
begin
  if not Visible then
    Exit;
  Flags := 0;
  case FStyle of
    capClose:
      Flags := DFCS_CAPTIONCLOSE;
    capMax:
      Flags := DFCS_CAPTIONMAX;
    capMin:
      Flags := DFCS_CAPTIONMIN;
    capRestore:
      Flags := DFCS_CAPTIONRESTORE;
    capHelp:
      Flags := DFCS_CAPTIONHELP;
  end;

  if not Enabled then
    Flags := Flags or DFCS_INACTIVE
  else
  if FDown and FMouseDown and Enabled then
    Flags := Flags or DFCS_PUSHED;
  if FFlat then
    Flags := Flags or DFCS_FLAT;

  Canvas.Brush.Color := Color;
    SetBkMode(Canvas.Handle, TRANSPARENT);
    DrawFrameControl(Canvas.Handle, ClientRect, DFC_CAPTION, Flags);
    if FFlat then
    begin
      R := ClientRect;
      if FDown and FMouseDown then
        Frame3D(Canvas, R, clBtnShadow, clBtnHighLight, 1)
      else
      if FOver then
        Frame3D(Canvas, R, clBtnHighLight, clBtnShadow, 1)
      else
        Frame3D(Canvas, R, clBtnFace, clBtnFace, 1);
    end;
end;

//=== { TJvCaptionPanel } ====================================================

constructor TJvCaptionPanel.Create(AOwner: TComponent);
var
  I: TJvCapBtnStyle;
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  FCaptionFont := TFont.Create;
  FIcon := TIcon.Create;
  // (rom) Warning! This seems no standard Windows font
//  FCaptionFont.Name := 'MS Shell Dlg 2';
  FCaptionFont.Size := 10;
  FCaptionFont.Style := [fsBold];
  FCaptionFont.Color := clWhite;
  FCaptionFont.OnChange := DoCaptionFontChange;
  FCaptionPosition := dpLeft;
  FAutoDrag := True;
  FOffset := 8;
  FCaptionColor := clActiveCaption;
  FFlat := False;
  for I := Low(FButtonArray) to High(FButtonArray) do //Iterate
  begin
    FButtonArray[I] := TJvCapBtn.Create(Self);
    FButtonArray[I].Parent := Self;
    FButtonArray[I].Style := I;
    FButtonArray[I].Flat := FFlat;
  end;
  FButtons := [];
  BorderStyle := bsSingle;

  FCaptionOffsetSmall := 2;
  FCaptionOffsetLarge := 3;
  FOutlookLook := False;
  FResizable := True;
end;

destructor TJvCaptionPanel.Destroy;
begin
  FIcon.Free;
  FCaptionFont.Free;
  inherited Destroy;
end;

procedure TJvCaptionPanel.SetCaptionFont(Value: TFont);
begin
  FCaptionFont.Assign(Value);
  Invalidate;
end;

procedure TJvCaptionPanel.SetCaptionHeight(const Value: Integer);
begin
  if FCaptionHeight <> Value then
  begin
    FCaptionHeight := Value;
    Invalidate;
    ReAlign;
  end;
end;

procedure TJvCaptionPanel.SetCaption(Value: string);
begin
  FCaption := Value;
  inherited Caption := '';
  Invalidate;
end;

procedure TJvCaptionPanel.SetCaptionColor(Value: TColor);
begin
  if FCaptionColor <> Value then
  begin
    FCaptionColor := Value;
    Invalidate;
  end;
end;

procedure TJvCaptionPanel.SetFlat(Value: Boolean);
var
  I: TJvCapBtnStyle;
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    for I := Low(FButtonArray) to High(FButtonArray) do
      FButtonArray[I].Flat := FFlat;
  end;
end;

procedure TJvCaptionPanel.SetButtons(Value: TJvCapBtnStyles);
var
  I: TJvCapBtnStyle;
begin
  if FButtons <> Value then
  begin
    FButtons := Value;
    for I := Low(FButtonArray) to High(FButtonArray) do
      FButtonArray[I].Visible := (I in FButtons);
    Invalidate;
  end;
end;

procedure TJvCaptionPanel.SetCaptionPosition(Value: TJvDrawPosition);
begin
  if FCaptionPosition <> Value then
  begin
    FCaptionPosition := Value;
    RecreateWnd;
  end;
end;

procedure TJvCaptionPanel.SetIcon(Value: TIcon);
begin
  FIcon.Assign(Value);
  Invalidate;
end;

procedure TJvCaptionPanel.AlignControls(AControl: TControl; var R: TRect);
begin
  case FCaptionPosition of
    dpLeft:
      R := Rect(GetEffectiveCaptionHeight + FCaptionOffsetSmall, 0, ClientWidth, ClientHeight);
    dpTop:
      R := Rect(0, GetEffectiveCaptionHeight + FCaptionOffsetSmall, ClientWidth, ClientHeight);
    dpRight:
      R := Rect(0, 0, ClientWidth - GetEffectiveCaptionHeight - FCaptionOffsetSmall, ClientHeight);
    dpBottom:
      R := Rect(0, 0, ClientWidth, ClientHeight - GetEffectiveCaptionHeight - FCaptionOffsetSmall);
  end;
  inherited AlignControls(AControl, R);
end;


procedure TJvCaptionPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if BorderStyle = bsSingle then
    with Params do
    begin
      if Resizable then
        Style := Style or WS_THICKFRAME
      else
        Style := Style or WS_DLGFRAME;
      ExStyle := ExStyle and not WS_EX_CLIENTEDGE;
    end;
end;


procedure TJvCaptionPanel.Paint;
var
  Rotation: Integer;
  R: TRect;
  FlatOffset: Integer;
  AdjustedCaptionHeight: Integer;
begin
  R := ClientRect;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(R);
  Canvas.Brush.Color := FCaptionColor;
  FBevel := FCaptionOffsetSmall;
  Rotation := 0;

  FlatOffset := Ord(FlatButtons);

  AdjustedCaptionHeight := GetEffectiveCaptionHeight;
  if FOutlookLook then
  begin
    if CaptionPosition = dpLeft then
      AdjustedCaptionHeight := AdjustedCaptionHeight - 3 + FlatOffset
    else
    if CaptionPosition = dpRight then
      AdjustedCaptionHeight := AdjustedCaptionHeight - 4 + FlatOffset
    else
      AdjustedCaptionHeight := AdjustedCaptionHeight - 5 + FlatOffset
  end;

  case FCaptionPosition of
    dpLeft:
      begin
        FCaptionRect := Rect(FBevel, FBevel, AdjustedCaptionHeight + FBevel, ClientHeight - FBevel);
        Rotation := 90;
      end;
    dpTop:
      FCaptionRect := Rect(FBevel, FBevel, ClientWidth - FBevel, AdjustedCaptionHeight + FBevel);
    dpRight:
      begin
        FCaptionRect := Rect(ClientWidth - AdjustedCaptionHeight - FBevel, FBevel, ClientWidth - FBevel, ClientHeight - FBevel);
        Rotation := -90;
      end;
    dpBottom:
      FCaptionRect := Rect(FBevel, ClientHeight - AdjustedCaptionHeight - FBevel, ClientWidth - FBevel, ClientHeight - FBevel);
  end; //case
  Canvas.FillRect(FCaptionRect);
  if not FIcon.Empty then
  begin
    case FCaptionPosition of
      dpRight:
        Canvas.Draw((FCaptionRect.Left + FCaptionRect.Right - FIcon.Width) div 2, FCaptionRect.Top + 1, FIcon);
      dpLeft:
        Canvas.Draw((FCaptionRect.Left + FCaptionRect.Right - FIcon.Width) div 2, FCaptionRect.Bottom - 1 - FIcon.Height, FIcon);
      dpBottom, dpTop:
        Canvas.Draw(FCaptionRect.Left + 1, (FCaptionRect.Top + FCaptionRect.Bottom - FIcon.Height) div 2 , FIcon);
    end;
  end;
  DrawRotatedText(Rotation);
  DrawButtons;
end;

procedure TJvCaptionPanel.DrawRotatedText(Rotation: Integer);
var
  tH: Integer;
  tW: Integer;
  Lf: TLogFont;
  Tf: TFont;
  Flags: Integer;
  R: TRect;
begin
  if FCaption = '' then
    Exit;
    SetBkMode(Canvas.Handle, TRANSPARENT);
    with Canvas do
    begin
      Tf := TFont.Create;
      try
        Tf.Assign(CaptionFont);
        GetObject(Tf.Handle, SizeOf(Lf), @Lf);
        Lf.lfEscapement := Rotation * 10;
        Lf.lfOrientation := Rotation * 10;
        Lf.lfOutPrecision := OUT_TT_PRECIS;
        Tf.Handle := CreateFontIndirect(Lf);
        Canvas.Font.Assign(Tf);
      finally
        Tf.Free;
      end;
      R := FCaptionRect;
      tH := ((R.Bottom - R.Top) - Canvas.TextHeight(FCaption)) div 2;
      tW := ((R.Right - R.Left) - Canvas.TextHeight(FCaption)) div 2;
      if FOutlookLook then
      begin
        Dec(tH);
        Dec(tW);
      end;
      case FCaptionPosition of
        dpLeft:
          begin
            if not FIcon.Empty then
              Dec(R.Bottom, FIcon.Height + 2);
            R := Rect(R.Left, R.Bottom, R.Right, R.Top);
            OffsetRect(R, tW, -FOffset);
          end;
        dpTop, dpBottom:
          begin
            OffsetRect(R, FOffset, tH);
            if not FIcon.Empty then
              Inc(R.Left, FIcon.Width + 2);
          end;
        dpRight:
          begin
            if not FIcon.Empty then
              Inc(R.Top, FIcon.Height + 2);
            R := Rect(R.Right, R.Top, R.Left, R.Bottom);
            OffsetRect(R, -tW, FOffset);
          end;
      end;
      Flags := DT_NOPREFIX;
      if FCaptionPosition in [dpTop, dpBottom] then
        Flags := Flags or DT_VCENTER;
      if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
        Flags := Flags or DT_NOCLIP; { bug or feature? }
      DrawText(Canvas, Caption, -1, R, Flags);
    end;
end;

function TJvCaptionPanel.GetEffectiveCaptionHeight: Integer;
begin
  if FCaptionHeight = 0 then
    Result := GetSystemMetrics(SM_CYCAPTION)
  else
    Result := FCaptionHeight;
end;

procedure TJvCaptionPanel.DrawButtons;
var
  R: TRect;
  FWidth, FHeight: Integer;
begin
  if FButtons = [] then
    Exit;

  FWidth := FButtonArray[capClose].Width;
  FHeight := FButtonArray[capClose].Height;
  if FFlat then
  begin
    Inc(FWidth);
    Inc(FHeight);
  end;

  case FCaptionPosition of
    dpLeft:
      R := Rect(FCaptionRect.Left + FCaptionOffsetSmall, FCaptionRect.Top + FCaptionOffsetSmall, 0, 0);
    dpTop:
      R := Rect(FCaptionRect.Right - FWidth - FCaptionOffsetSmall, FCaptionRect.Top + FCaptionOffsetLarge, 0, 0);
    dpRight:
      R := Rect(FCaptionRect.Left + FCaptionOffsetSmall, FCaptionRect.Bottom - FHeight - FCaptionOffsetSmall, 0, 0);
    dpBottom:
      R := Rect(FCaptionRect.Right - FWidth - FCaptionOffsetSmall, FCaptionRect.Top + FCaptionOffsetLarge, 0, 0);
  end;

  if capClose in FButtons then
  begin
    FButtonArray[capClose].Top := R.Top;
    FButtonArray[capClose].Left := R.Left;
    FButtonArray[capClose].Visible := True;
    case FCaptionPosition of
      dpLeft:
        OffsetRect(R, 0, FHeight + FCaptionOffsetSmall);
      dpTop:
        OffsetRect(R, -FWidth - FCaptionOffsetSmall, 0);
      dpRight:
        OffsetRect(R, 0, -FHeight - FCaptionOffsetSmall);
      dpBottom:
        OffsetRect(R, -FWidth - FCaptionOffsetSmall, 0);
    end;
  end
  else
    FButtonArray[capClose].Visible := False;

  if (capMax in FButtons) then
  begin
    FButtonArray[capMax].Top := R.Top;
    FButtonArray[capMax].Left := R.Left;
    FButtonArray[capMax].Visible := True;
    case FCaptionPosition of
      dpLeft:
        OffsetRect(R, 0, FHeight);
      dpTop:
        OffsetRect(R, -FWidth, 0);
      dpRight:
        OffsetRect(R, 0, -FHeight);
      dpBottom:
        OffsetRect(R, -FWidth, 0);
    end;
  end
  else
    FButtonArray[capMax].Visible := False;

  if capRestore in FButtons then
  begin
    FButtonArray[capRestore].Top := R.Top;
    FButtonArray[capRestore].Left := R.Left;
    FButtonArray[capRestore].Visible := True;
    case FCaptionPosition of
      dpLeft:
        OffsetRect(R, 0, FHeight);
      dpTop:
        OffsetRect(R, -FWidth, 0);
      dpRight:
        OffsetRect(R, 0, -FHeight);
      dpBottom:
        OffsetRect(R, -FWidth, 0);
    end;
  end
  else
    FButtonArray[capRestore].Visible := False;

  if capMin in FButtons then
  begin
    FButtonArray[capMin].Top := R.Top;
    FButtonArray[capMin].Left := R.Left;
    FButtonArray[capMin].Visible := True;
    case FCaptionPosition of
      dpLeft:
        OffsetRect(R, 0, FHeight);
      dpTop:
        OffsetRect(R, -FWidth, 0);
      dpRight:
        OffsetRect(R, 0, -FHeight);
      dpBottom:
        OffsetRect(R, -FWidth, 0);
    end;
  end
  else
    FButtonArray[capMin].Visible := False;

  if capHelp in FButtons then
  begin
    FButtonArray[capHelp].Top := R.Top;
    FButtonArray[capHelp].Left := R.Left;
    FButtonArray[capHelp].Visible := True;
  end
  else
    FButtonArray[capHelp].Visible := False;
end;

{ this method is called only by the caption buttons }

procedure TJvCaptionPanel.ClickButton(Button: TJvCapBtnStyle);
begin
  if Assigned(FButtonClick) then
    FButtonClick(Self, Button);
end;

procedure TJvCaptionPanel.DoLeaveDrag;
begin
  if Assigned(FEndDrag) then
    FEndDrag(Self);
end;

procedure TJvCaptionPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then
  begin
    {$IFDEF JVCAPTIONPANEL_STD_BEHAVE}
    ReleaseCapture;
    {$ENDIF JVCAPTIONPANEL_STD_BEHAVE}
    DoLeaveDrag;
  end;
  FDragging := False;
end;

procedure TJvCaptionPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  {$IFDEF JVCAPTIONPANEL_STD_BEHAVE}
  if FDragging then
    SetBounds(Left + X - FAnchorPos.X, Top + Y - FAnchorPos.Y, Width, Height);
  {$ENDIF JVCAPTIONPANEL_STD_BEHAVE}
end;

procedure TJvCaptionPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  FMouseDown := True;
  if not PtInRect(FCaptionRect, Point(X, Y)) then
    Exit;

  if FAutoDrag and CanStartDrag then
  begin
    SetZOrder(True);
    FDragging := True;
    ReleaseCapture;
    {$IFDEF JVCAPTIONPANEL_STD_BEHAVE}
    SetCapture(Handle);
    FAnchorPos := Point(X, Y);
    {$ELSE}
    Perform(WM_SYSCOMMAND, SC_DRAGMOVE, 0);
    {$ENDIF JVCAPTIONPANEL_STD_BEHAVE}
  end;
end;

procedure TJvCaptionPanel.Resize;
begin
  inherited Resize;
  Repaint;
end;

function TJvCaptionPanel.CanStartDrag: Boolean;
begin
  Result := Align = alNone;
  if Assigned(FOnStartAutoDrag) then
    FOnStartAutoDrag(Self, Result);
end;


procedure TJvCaptionPanel.WMNCLButtonUp(var Msg: TWMNCLButtonUp);
begin
  inherited;
  if FDragging then
    MouseUp(mbLeft, [], Msg.XCursor, Msg.YCursor);
end;

procedure TJvCaptionPanel.SetResizable(const Value: Boolean);
begin
  if FResizable <> Value then
  begin
    FResizable := Value;
    RecreateWnd;
  end;
end;


procedure TJvCaptionPanel.SetOutlookLook(const Value: Boolean);
begin
  FOutlookLook := Value;
  if FOutlookLook then
  begin
    FCaptionOffsetSmall := 0;
    FCaptionOffsetLarge := 0;
  end
  else
  begin
    FCaptionOffsetSmall := 2;
    FCaptionOffsetLarge := 3;
  end;
  Invalidate;
end;

procedure TJvCaptionPanel.DoCaptionFontChange(Sender: TObject);
begin
  Invalidate;
end;


{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.