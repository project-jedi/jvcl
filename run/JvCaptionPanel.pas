{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCaptionPanel.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright © 1997-2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

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

{$IFDEF VisualCLX}
 {$DEFINE JVCAPTIONPANEL_STD_BEHAVE}
{$ENDIF VisualCLX}

uses
  Windows, Messages,
  {$IFDEF VisualCLX}
  Qt,
  {$ENDIF VisualCLX}
  Classes, Graphics, Controls, Forms,
  JvComponent, JvExControls;

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

  TJvCaptionPanel = class(TJvCustomPanel, IJvDenySubClassing)
  private
    FButtonArray: array [TJvCapBtnStyle] of TJvCapBtn;
    FButtonClick: TJvCapBtnEvent;
    FCaptionPosition: TJvDrawPosition;
    FCaptionWidth: Integer;
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
    procedure SetIcon(Value: TIcon);
    procedure SetCaptionFont(Value: TFont);
    procedure SetCaptionColor(Value: TColor);
    procedure SetFlat(Value: Boolean);
    procedure SetButtons(Value: TJvCapBtnStyles);
    procedure SetCaption(Value: string);
    procedure SetCaptionPosition(Value: TJvDrawPosition);
    procedure DrawRotatedText(Rotation: Integer);
    procedure DrawButtons;
    {$IFDEF VCL}
    procedure WMNCLButtonUp(var Msg: TWMNCLButtonUp); message WM_NCLBUTTONUP;
    {$ENDIF VCL}
    procedure SetOutlookLook(const Value: Boolean);
    procedure DoCaptionFontChange(Sender: TObject);
  protected
    procedure Paint; override;
    procedure Resize; override;

    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    {$IFDEF VCL}
    procedure CreateParams(var Params: TCreateParams); override;
    {$ENDIF VCL}
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
    property Color;
    property Cursor;
    {$IFDEF VCL}
    property DragCursor;
    property FullRepaint;
    property Locked;
    {$ENDIF VCL}
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

implementation

uses
  SysUtils, ExtCtrls;

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
  {$IFDEF VisualCLX}
  Canvas.Start;
  try
  {$ENDIF VisualCLX}
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
  {$IFDEF VisualCLX}
  finally
    Canvas.Stop;
  end;
  {$ENDIF VisualCLX}
end;

//=== { TJvCaptionPanel } ====================================================

constructor TJvCaptionPanel.Create(AOwner: TComponent);
var
  I: TJvCapBtnStyle;
begin
  inherited Create(AOwner);
  {$IFDEF VCL}
  DoubleBuffered := True;
  {$ENDIF VCL}
  FCaptionFont := TFont.Create;
  FIcon := TIcon.Create;
  {$IFDEF VCL}
  // (rom) Warning! This seems no standard Windows font
//  FCaptionFont.Name := 'MS Shell Dlg 2';
  FCaptionFont.Size := 10;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  FCaptionFont.Name := 'Helvetica';
  FCaptionFont.Height := 13;
  {$ENDIF VisualCLX}
  FCaptionFont.Style := [fsBold];
  FCaptionFont.Color := clWhite;
  FCaptionFont.OnChange := DoCaptionFontChange;
  FCaptionPosition := dpLeft;
  FCaptionWidth := GetSystemMetrics(SM_CYCAPTION);
  FAutoDrag := True;
  {$IFDEF VCL}
  FOffset := 8;
  FCaptionColor := clActiveCaption;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  FOffset := 3;
  FCaptionColor := clActiveHighlight;
  {$ENDIF VisualCLX}
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
end;

destructor TJvCaptionPanel.Destroy;
begin
  FCaptionFont.Free;
  inherited Destroy;
end;

procedure TJvCaptionPanel.SetCaptionFont(Value: TFont);
begin
  FCaptionFont.Assign(Value);
  Invalidate;
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
    {$IFDEF VCL}
    RecreateWnd;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    RecreateWidget;
    {$ENDIF VisualCLX}
  end;
end;

procedure TJvCaptionPanel.SetIcon(Value: TIcon);
begin
  FIcon.Assign(Value);
  Invalidate;
end;

procedure TJvCaptionPanel.AlignControls(AControl: TControl; var Rect: TRect);
begin
  case FCaptionPosition of
    dpLeft:
      Rect := Classes.Rect(FCaptionWidth + FCaptionOffsetSmall, 0, ClientWidth, ClientHeight);
    dpTop:
      Rect := Classes.Rect(0, FCaptionWidth + FCaptionOffsetSmall, ClientWidth, ClientHeight);
    dpRight:
      Rect := Classes.Rect(0, 0, ClientWidth - FCaptionWidth - FCaptionOffsetSmall, ClientHeight);
    dpBottom:
      Rect := Classes.Rect(0, 0, ClientWidth, ClientHeight - FCaptionWidth - FCaptionOffsetSmall);
  end;
  inherited AlignControls(AControl, Rect);
end;

{$IFDEF VCL}
procedure TJvCaptionPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if BorderStyle = bsSingle then
    with Params do
    begin
      Style := Style or WS_THICKFRAME;
      ExStyle := ExStyle and not WS_EX_CLIENTEDGE;
    end;
end;
{$ENDIF VCL}

procedure TJvCaptionPanel.Paint;
var
  Rotation: Integer;
  R: TRect;
  FlatOffset: Integer;
begin
  R := ClientRect;
  {$IFDEF VisualCLX}
  if BorderStyle = bsSingle then
  begin
    DrawShadePanel(Canvas, R, False, 1, nil);
    InflateRect(R, -2, -2);
  end;
  {$ENDIF VisualCLX}
  with Canvas do
  begin
    Brush.Color := Color;
    FillRect(R);
    Brush.Color := FCaptionColor;
  end;
  FBevel := FCaptionOffsetSmall;
  Rotation := 0;

  FlatOffset := Ord(FlatButtons);

  if FOutlookLook then
  begin
    if CaptionPosition = dpLeft then
      FCaptionWidth := GetSystemMetrics(SM_CYCAPTION) - 3 + FlatOffset
    else
    if CaptionPosition = dpRight then
      FCaptionWidth := GetSystemMetrics(SM_CYCAPTION) - 4 + FlatOffset
    else
      FCaptionWidth := GetSystemMetrics(SM_CYCAPTION) - 5 + FlatOffset
  end
  else
    FCaptionWidth := GetSystemMetrics(SM_CYCAPTION);

  case FCaptionPosition of
    dpLeft:
      begin
        FCaptionRect := Rect(FBevel, FBevel, FCaptionWidth + FBevel, ClientHeight - FBevel);
        Rotation := 90;
      end;
    dpTop:
      FCaptionRect := Rect(FBevel, FBevel, ClientWidth - FBevel, FCaptionWidth + FBevel);
    dpRight:
      begin
        FCaptionRect := Rect(ClientWidth - FCaptionWidth - FBevel, FBevel, ClientWidth - FBevel, ClientHeight - FBevel);
        Rotation := -90;
      end;
    dpBottom:
      FCaptionRect := Rect(FBevel, ClientHeight - FCaptionWidth - FBevel, ClientWidth - FBevel, ClientHeight - FBevel);
  end; //case
  Canvas.FillRect(FCaptionRect);
  if not FIcon.Empty then
  begin
    with FCaptionRect do
      case FCaptionPosition of
      dpRight:
        Canvas.Draw( (Left + Right - FIcon.Width) div 2, Top + 1, FIcon);
      dpLeft:
        Canvas.Draw( (Left + Right - FIcon.Width) div 2, Bottom - 1 - FIcon.Height, FIcon);
      dpBottom, dpTop:
        Canvas.Draw(Left + 1, (Top + Bottom - FIcon.Height) div 2 , FIcon );
      end; //case
  end;
  DrawRotatedText(Rotation);
  DrawButtons;
end;

procedure TJvCaptionPanel.DrawRotatedText(Rotation: Integer);
var
  tH: Integer;
  {$IFDEF VCL}
  tW: Integer;
  Lf: TLogFont;
  Tf: TFont;
  Flags: Integer;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  X, Y: Integer;
  {$ENDIF VisualCLX}
  R: TRect;
begin
  if FCaption = '' then
    Exit;
  {$IFDEF VisualCLX}
  Canvas.Start;
  try
    QPainter_save(Canvas.Handle);
    Canvas.Font.Assign(CaptionFont);
  {$ENDIF VisualCLX}
    SetBkMode(Canvas.Handle, TRANSPARENT);
    with Canvas do
    begin
      {$IFDEF VCL}
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
      {$ENDIF VCL}
      R := FCaptionRect;
      tH := ((R.Bottom - R.Top) - Canvas.TextHeight(FCaption)) div 2;
      {$IFDEF VCL}
      tW := ((R.Right - R.Left) - Canvas.TextHeight(FCaption)) div 2;
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      X := FCaptionRect.Left;
      Y := FCaptionRect.Top;
      {$ENDIF VisualCLX}
      if FOutlookLook then
      begin
        Dec(tH);
        {$IFDEF VCL}
        Dec(tW);
        {$ENDIF VCL}
      end;
      case FCaptionPosition of
        dpLeft:
          begin
            {$IFDEF VCL}
            if not FIcon.Empty then
              Dec(R.Bottom, FIcon.Height + 2);
            with R do
              R := Rect(Left, Bottom, Right, Top);
            OffsetRect(R, tW, -FOffset);
            {$ENDIF VCL}
            {$IFDEF VisualCLX}
            X := R.Left + (FCaptionWidth + Canvas.TextHeight(Caption)) div 2 - FOffset;
            Y := R.Bottom - 1;
            if not FIcon.Empty then
              Dec(Y, FIcon.Height + 3)
            else
              Dec(Y);
            {$ENDIF VisualCLX}
          end;
        dpTop, dpBottom:
          begin
            {$IFDEF VisualCLX}
            X := R.Left;
            Y := R.Top + tH + Canvas.TextHeight(Caption) - FOffset;
            if not FIcon.Empty then
              Inc(X, FIcon.Width + 3)
            else
              Inc(X);
            {$ENDIF VisualCLX}
            {$IFDEF VCL}
            OffsetRect(R, FOffset, tH);
            if not FIcon.Empty then
              Inc(R.Left, FIcon.Width + 2);
            {$ENDIF VCL}
          end;
        dpRight:
          begin
            {$IFDEF VisualCLX}
            X := R.Left + (FCaptionWidth - Canvas.TextHeight(Caption)) div 2 + FOffset;
            Y := R.Top;
            if not FIcon.Empty then
              Inc(Y, FIcon.Height + 3)
            else
              Inc(Y);
            {$ENDIF VisualCLX}
            {$IFDEF VCL}
            if not FIcon.Empty then
              Inc(R.Top, FIcon.Height + 2);
            with R do
              R := Rect(Right, Top, Left, Bottom);
            OffsetRect(R, -tW, FOffset);
            {$ENDIF VCL}
          end;
      end;
      {$IFDEF VCL}
      Flags := DT_NOPREFIX;
      if FCaptionPosition in [dpTop, dpBottom] then
        Flags := Flags or DT_VCENTER;
      if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
        Flags := Flags or DT_NOCLIP; { bug or feature? }
      DrawText(Canvas.Handle, PChar(Caption), -1, R, Flags);
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      TextOutAngle(Canvas, Rotation, X, Y, Caption);
      {$ENDIF VisualCLX}
    end;
  {$IFDEF VisualCLX}
  finally
    QPainter_restore(Canvas.Handle);
    Canvas.Stop;
  end;
  {$ENDIF VisualCLX}
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
    DoLeaveDrag;
  FDragging := False;
end;

procedure TJvCaptionPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  {$IFDEF JVCAPTIONPANEL_STD_BEHAVE}
  if FDragging then
  begin
    Left := Left + X - FAnchorPos.X;
    Top := Top + Y - FAnchorPos.Y;
  end;
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
  Result := True;
  if Assigned(FOnStartAutoDrag) then
    FOnStartAutoDrag(Self, Result);
end;

{$IFDEF VCL}
procedure TJvCaptionPanel.WMNCLButtonUp(var Msg: TWMNCLButtonUp);
begin
  inherited;
  if FDragging then
    MouseUp(mbLeft, [], Msg.XCursor, Msg.YCursor);
end;
{$ENDIF VCL}

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

end.

