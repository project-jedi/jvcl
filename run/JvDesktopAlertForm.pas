{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDesktopAlertForm.PAS, released on 2004-03-24.

The Initial Developer of the Original Code is Peter Thornqvist <peter3 at sourceforge dot net>
Portions created by Peter Thornqvist are Copyright (C) 2004 Peter Thornqvist.
All Rights Reserved.

Contributor(s):
Hans-Eric Grönlund (stack logic)
Olivier Sannier (animation styles logic)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
* This form is used by the TJvDesktopAlert component

-----------------------------------------------------------------------------}
// $Id$

unit JvDesktopAlertForm;

{$I jvcl.inc}

interface

uses
  Windows,
  {$IFDEF VCL}
  Messages,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Types,
  {$ENDIF VisualCLX}
  Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls,
  ImgList, ActnList,
  JvButton, JvLabel, JvComponent;

const
  cDefaultAlertFormWidth = 329;
  cDefaultAlertFormHeight = 76;

type
  TJvDesktopAlertButtonType = (abtArrowLeft, abtArrowRight, abtClose, abtMaximize,
    abtMinimize, abtDropDown, abtDropDownChevron, abtRestore, abtImage);

  TJvDesktopAlertButton = class(TJvCustomGraphicButton)
  private
    FChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FImageIndex: TImageIndex;
    FToolType: TJvDesktopAlertButtonType;
    procedure SetImages(const Value: TCustomImageList);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure DoImagesChange(Sender: TObject);
    procedure SetToolType(const Value: TJvDesktopAlertButtonType);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ToolType: TJvDesktopAlertButtonType read FToolType write SetToolType;
    property DropDownMenu;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex;
    property Width default 21;
    property Height default 21;
    property OnClick;
  end;

  TJvFormDesktopAlert = class(TJvForm)
  private
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnUserMove: TNotifyEvent;
    acClose: TAction;
    MouseTimer: TTimer;

    {$IFDEF VCL}
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    {$ENDIF VCL}

    procedure DoMouseTimer(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  protected
    procedure DoShow; override;
    procedure DoClose(var Action: TCloseAction); override;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;

    {$IFDEF VCL}
    procedure WMMove(var Msg: TWMMove); message WM_MOVE;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    {$ENDIF VCL}
  public
    imIcon: TImage;
    lblText: TJvLabel;
    lblHeader: TLabel;
    tbDropDown: TJvDesktopAlertButton;
    tbClose: TJvDesktopAlertButton;

    Moveable: Boolean;
    MoveAnywhere: Boolean;
    Closeable: Boolean;
    ClickableMessage: Boolean;
    MouseInControl: Boolean;
    WindowColorFrom: TColor;
    WindowColorTo: TColor;
    CaptionColorFrom: TColor;
    CaptionColorTo: TColor;
    FrameColor: TColor;
    constructor Create(AOwner: TComponent); override;
    procedure acCloseExecute(Sender: TObject);
    procedure SetNewTop(const Value: Integer);
    procedure SetNewLeft(const Value: Integer);
    procedure SetNewOrigin(ALeft, ATop: Integer);
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnUserMove: TNotifyEvent read FOnUserMove write FOnUserMove;

  end;

implementation

uses
  Menus,
  SysUtils,
  JvJVCLUtils,
  JvDesktopAlert;

{.$R *.dfm}// not needed

const
  cAlphaIncrement = 5;
  cCaptionHeight = 8;

  JvDefaultCaptionDotColor = TColor($00F8FCF8);
  JvDefaultCaptionDotShadowColor = TColor($00B8BCB8);
  JvDefaultTrackBorderColor = TColor($00663300);
  JvDefaultHotTrackColor = TColor($00CC9999);
  JvDefaultTrackColor = TColor($00D6BEB5);

procedure DrawDesktopAlertCaption(Canvas: TCanvas; ARect: TRect; ColorFrom, ColorTo: TColor; DrawDots: Boolean);
var
  I: Integer;
  R: TRect;
begin
  GradientFillRect(Canvas, ARect, ColorFrom, ColorTo, fdTopToBottom, cCaptionHeight);
  R := ARect;
  Inc(R.Left, (R.Right - R.Left) div 2 - 20);
  Inc(R.Top, 3);
  R.Right := R.Left + 2;
  R.Bottom := R.Top + 2;
  if DrawDots then
    for I := 0 to 9 do // draw the dots
    begin
      Canvas.Brush.Color := clGray;
      Canvas.FillRect(R);
      OffsetRect(R, 1, 1);
      Canvas.Brush.Color := JvDefaultCaptionDotColor;
      Canvas.FillRect(R);
      Canvas.Brush.Color := JvDefaultCaptionDotShadowColor;
      Canvas.FillRect(Rect(R.Left, R.Top, R.Left + 1, R.Top + 1));
      OffsetRect(R, 3, -1);
    end;
end;

procedure DrawDesktopAlertWindow(Canvas: TCanvas; WindowRect: TRect;
  FrameColor: TColor; WindowColorFrom, WindowColorTo, CaptionColorFrom, CaptionColorTo: TColor; DrawDots: Boolean);
var
  CaptionRect: TRect;
  ATop: Integer;
  AColors: Byte;
begin
  CaptionRect := WindowRect;
  CaptionRect.Bottom := CaptionRect.Top + cCaptionHeight;
  DrawDesktopAlertCaption(Canvas, CaptionRect, CaptionColorFrom, CaptionColorTo, DrawDots);
  ATop := WindowRect.Top;
  WindowRect.Top := CaptionRect.Bottom + 1;
  Dec(WindowRect.Bottom);
  if WindowRect.Bottom - WindowRect.Top < 255 then
    AColors := WindowRect.Bottom - WindowRect.Top
  else
    AColors := 32;
  GradientFillRect(Canvas, WindowRect, WindowColorFrom, WindowColorTo, fdTopToBottom, AColors);
  WindowRect.Top := ATop;
  Inc(WindowRect.Bottom);
  Canvas.Brush.Color := clGray;
  {$IFDEF VCL}
  Canvas.FrameRect(WindowRect);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  FrameRect(Canvas, WindowRect);
  {$ENDIF VisualCLX}
end;

//=== { TJvFormDesktopAlert } ================================================

constructor TJvFormDesktopAlert.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner, 1);

  {$IFDEF VCL}
  Font.Assign(Screen.IconFont);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Font.Assign(Application.Font);
  {$ENDIF VisualCLX}
  MouseTimer := TTimer.Create(Self);
  MouseTimer.Enabled := False;
  MouseTimer.Interval := 200;
  MouseTimer.OnTimer := DoMouseTimer;
  MouseTimer.Enabled := True;

  {$IFDEF VCL}
  BorderStyle := bsNone;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  BorderStyle := fbsNone;
  {$ENDIF VisualCLX}
  BorderIcons := [];
  FormStyle := fsStayOnTop;
  Scaled := False;
  Height := cDefaultAlertFormHeight;
  Width := cDefaultAlertFormWidth;
  OnPaint := FormPaint;

  imIcon := TImage.Create(Self);
  imIcon.Parent := Self;
  imIcon.SetBounds(8, 11, 32, 32);
  imIcon.AutoSize := True;
  imIcon.Transparent := True;

  lblHeader := TLabel.Create(Self);
  lblHeader.Parent := Self;
  lblHeader.SetBounds(48, 11, 71, 13);
  lblHeader.Font.Style := [fsBold];
  lblHeader.Transparent := True;

  lblText := TJvLabel.Create(Self);
  lblText.Parent := Self;
  lblText.SetBounds(56, 24, 67, 13);
  lblText.Transparent := True;
  lblText.WordWrap := True;
  lblText.Anchors := [akLeft..akBottom];

  acClose := TAction.Create(Self);
  acClose.Caption := 'Close';

  acClose.ShortCut := ShortCut(VK_F4, [ssAlt]); // 32883
  acClose.OnExecute := acCloseExecute;

  tbClose := TJvDesktopAlertButton.Create(Self);
  tbClose.ToolType := abtClose;
  tbClose.Parent := Self;
  tbClose.SetBounds(Width - 17, cCaptionHeight + 2, 15, 15);
  tbClose.Anchors := [akRight, akTop];

  tbDropDown := TJvDesktopAlertButton.Create(Self);
  tbDropDown.ToolType := abtDropDown;
  tbDropDown.Parent := Self;
  tbDropDown.BoundsRect := tbClose.BoundsRect;
  tbDropDown.Left := tbDropDown.Left - 16;
  tbDropDown.Anchors := [akRight, akTop];
end;

procedure TJvFormDesktopAlert.FormPaint(Sender: TObject);
begin
  DrawDesktopAlertWindow(Canvas, ClientRect, FrameColor, WindowColorFrom, WindowColorTo, CaptionColorFrom, CaptionColorTo, Moveable or MoveAnywhere);
end;

{$IFDEF VCL}
procedure TJvFormDesktopAlert.WMNCHitTest(var Msg: TWMNCHitTest);
var
  P: TPoint;
begin
  with Msg do
    P := ScreenToClient(Point(XPos, YPos));
  if ((P.Y <= cCaptionHeight) and Moveable) or (MoveAnywhere and (ControlAtPos(P, False) = nil)) then
  begin
    TJvDesktopAlert(Owner).StyleHandler.AbortAnimation;
    Msg.Result := HTCAPTION;
  end
  else
    inherited;
end;
{$ENDIF VCL}

procedure TJvFormDesktopAlert.acCloseExecute(Sender: TObject);
begin
  if Closeable then
    Close;
end;

procedure TJvFormDesktopAlert.MouseEnter(AControl: TControl);
begin
  inherited MouseEnter(AControl);
  MouseInControl := True;
  //  SetFocus;
  TJvDesktopAlert(Owner).StyleHandler.AbortAnimation;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvFormDesktopAlert.MouseLeave(AControl: TControl);
var
  P: TPoint;
begin
  inherited MouseLeave(AControl);
  // make sure the mouse actually left the outer boundaries
  GetCursorPos(P);
  if not PtInRect(BoundsRect, P) then
  begin
    if Assigned(FOnMouseLeave) then
      FOnMouseLeave(Self);
    if TJvDesktopAlert(Owner).StyleHandler.DisplayDuration > 0 then
      TJvDesktopAlert(Owner).StyleHandler.DoEndAnimation;
    MouseInControl := False;
  end;
end;

procedure TJvFormDesktopAlert.DoShow;
begin
  inherited DoShow;
  TJvDesktopAlert(Owner).StyleHandler.AbortAnimation;
  lblText.HotTrackFont.Style := [fsUnderLine];
  lblText.HotTrackFont.Color := clNavy;
  if ClickableMessage then
  begin
    lblText.HotTrack := True;
    lblText.Cursor := crHandPoint;
  end
  else
  begin
    lblText.HotTrack := False;
    lblText.Cursor := crDefault;
  end;

  if tbDropDown.DropDownMenu = nil then
    tbDropDown.Visible := False;

  if not Closeable and (TJvDesktopAlert(Owner).StyleHandler.DisplayDuration > 0) then // must have either WaitTime or close button
  begin
    tbClose.Visible := False;
    tbDropDown.Left := tbClose.Left;
  end;

  imIcon.Top := 13;
  lblHeader.Top := imIcon.Top;
  lblHeader.Left := imIcon.Left + imIcon.Width + 5;
  lblText.Left := lblHeader.Left + 8;
  lblText.Width := tbDropDown.Left - lblText.Left;
  lblText.Top := lblHeader.Top + lblHeader.Height;
  TJvDesktopAlert(Owner).StyleHandler.DoStartAnimation;
  MouseTimer.Enabled := True;
end;

{$IFDEF VCL}
procedure TJvFormDesktopAlert.WMMove(var Msg: TWMMove);
begin
  inherited;
  if Showing and Assigned(FOnUserMove) then
    FOnUserMove(Self);
end;
{$ENDIF VCL}

procedure TJvFormDesktopAlert.SetNewTop(const Value: Integer);
begin
  SetNewOrigin(Left, Value);
end;

procedure TJvFormDesktopAlert.SetNewLeft(const Value: Integer);
begin
  SetNewOrigin(Value, Top);
end;

procedure TJvFormDesktopAlert.SetNewOrigin(ALeft, ATop: Integer);
var
  MoveEvent: TNotifyEvent;
begin
  if ((Top <> ATop) or (Left <> ALeft)) and not MouseInControl then
  begin
    MoveEvent := FOnUserMove;
    FOnUserMove := nil;
    Left := ALeft;
    Top := ATop;
    FOnUserMove := MoveEvent;
  end;
end;

procedure TJvFormDesktopAlert.DoMouseTimer(Sender: TObject);
var
  P: TPoint;
begin
  // this is here to ensure that MouseInControl is corectly set even
  // if we never got a CM_MouseLeave (that happens a lot)
  MouseTimer.Enabled := False;
  GetCursorPos(P);
  MouseInControl := PtInRect(BoundsRect, P) and (FindVCLWindow(P) = Self);
  MouseTimer.Enabled := True;
  if not TJvDesktopAlert(Owner).StyleHandler.Active and not MouseInControl and (TJvDesktopAlert(Owner).StyleHandler.DisplayDuration > 0) then
    TJvDesktopAlert(Owner).StyleHandler.DoEndAnimation;
end;

procedure TJvFormDesktopAlert.DoClose(var Action: TCloseAction);
begin
  inherited DoClose(Action);
  MouseTimer.Enabled := False;
end;

{$IFDEF VCL}
procedure TJvFormDesktopAlert.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);
end;
{$ENDIF VCL}

//=== { TJvDesktopAlertButton } ==============================================

constructor TJvDesktopAlertButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := DoImagesChange;
  Width := 21;
  Height := 21;
end;

destructor TJvDesktopAlertButton.Destroy;
begin
  FChangeLink.Free;
  inherited Destroy;
end;

procedure TJvDesktopAlertButton.DoImagesChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvDesktopAlertButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = Images then
      Images := nil;
end;

procedure TJvDesktopAlertButton.Paint;
var
  Rect: TRect;
begin
  with Canvas do
  begin
    Rect := ClientRect;
    Brush.Style := bsClear;
    if bsMouseInside in MouseStates then
    begin
      Pen.Color := JvDefaultTrackBorderColor;
      Rectangle(Rect);
      InflateRect(Rect, -1, -1);
      if bsMouseDown in MouseStates then
        Brush.Color := JvDefaultHotTrackColor
      else
        Brush.Color := JvDefaultTrackColor;
      FillRect(Rect);
    end;
    case ToolType of
      abtArrowLeft:
        begin
          Canvas.Font.Name := 'Marlett';
          Canvas.Font.Style := [];
          Canvas.Font.Size := 10;
          DrawText(Canvas.Handle, '3', 1, Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
        end;
      abtArrowRight:
        begin
          Canvas.Font.Name := 'Marlett';
          Canvas.Font.Style := [];
          Canvas.Font.Size := 10;
          DrawText(Canvas.Handle, '4', 1, Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
        end;
      abtClose:
        begin
          Canvas.Font.Name := 'Marlett';
          Canvas.Font.Size := 7;
          Canvas.Font.Style := [];
          DrawText(Canvas.Handle, 'r', 1, Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
        end;
      abtMaximize:
        begin
          Canvas.Font.Name := 'Marlett';
          Canvas.Font.Style := [];
          DrawText(Canvas.Handle, '2', 1, Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
        end;
      abtMinimize:
        begin
          Canvas.Font.Name := 'Marlett';
          Canvas.Font.Style := [];
          DrawText(Canvas.Handle, '1', 1, Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
        end;
      abtDropDown:
        begin
          Canvas.Font.Name := 'Marlett';
          Canvas.Font.Size := 10;
          Canvas.Font.Style := [];
          DrawText(Canvas.Handle, 'u', 1, Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
        end;
      abtDropDownChevron:
        begin // area should be 7x12
          InflateRect(Rect, -((Rect.Right - Rect.Left) - 7) div 2, -((Rect.Bottom - Rect.Top) - 12) div 2);
          Canvas.Pen.Color := clWindowText;

          Canvas.MoveTo(Rect.Left, Rect.Top);
          Canvas.LineTo(Rect.Left + 2, Rect.Top);

          Canvas.MoveTo(Rect.Left + 3, Rect.Top);
          Canvas.LineTo(Rect.Left + 5, Rect.Top);
          OffsetRect(Rect, 1, 1);

          Canvas.MoveTo(Rect.Left, Rect.Top);
          Canvas.LineTo(Rect.Left + 2, Rect.Top);

          Canvas.MoveTo(Rect.Left + 3, Rect.Top);
          Canvas.LineTo(Rect.Left + 5, Rect.Top);
          OffsetRect(Rect, 1, 1);

          Canvas.MoveTo(Rect.Left, Rect.Top);
          Canvas.LineTo(Rect.Left + 2, Rect.Top);

          Canvas.MoveTo(Rect.Left + 3, Rect.Top);
          Canvas.LineTo(Rect.Left + 5, Rect.Top);
          OffsetRect(Rect, -1, 1);

          Canvas.MoveTo(Rect.Left, Rect.Top);
          Canvas.LineTo(Rect.Left + 2, Rect.Top);

          Canvas.MoveTo(Rect.Left + 3, Rect.Top);
          Canvas.LineTo(Rect.Left + 5, Rect.Top);
          OffsetRect(Rect, -1, 1);

          Canvas.MoveTo(Rect.Left, Rect.Top);
          Canvas.LineTo(Rect.Left + 2, Rect.Top);

          Canvas.MoveTo(Rect.Left + 3, Rect.Top);
          Canvas.LineTo(Rect.Left + 5, Rect.Top);

          OffsetRect(Rect, 1, 4);
          Canvas.MoveTo(Rect.Left, Rect.Top);
          Canvas.LineTo(Rect.Left + 5, Rect.Top);
          OffsetRect(Rect, 1, 1);
          Canvas.MoveTo(Rect.Left, Rect.Top);
          Canvas.LineTo(Rect.Left + 3, Rect.Top);
          OffsetRect(Rect, 1, 1);
          Canvas.MoveTo(Rect.Left, Rect.Top);
          Canvas.LineTo(Rect.Left + 1, Rect.Top);
        end;
      abtRestore:
        begin
          Canvas.Font.Name := 'Marlett';
          Canvas.Font.Style := [];
          DrawText(Canvas.Handle, '3', 1, Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
        end;
      abtImage:
        begin
          if (Images = nil) or (ImageIndex < 0) or (ImageIndex >= Images.Count) then
            Exit;
          Images.Draw(Canvas,
            (Width - Images.Width) div 2 + Ord(bsMouseDown in MouseStates),
            (Height - Images.Height) div 2 + Ord(bsMouseDown in MouseStates),
            ImageIndex,
            {$IFDEF COMPILER6_UP}
            {$IFDEF VCL}
            dsTransparent,
            {$ENDIF VCL}
            itImage,
            {$ENDIF COMPILER6_UP}
            Enabled);
        end;
    end;
  end;
end;

procedure TJvDesktopAlertButton.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

procedure TJvDesktopAlertButton.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    if FImages <> nil then
      FImages.UnRegisterChanges(FChangeLink);
    FImages := Value;
    if FImages <> nil then
    begin
      FImages.FreeNotification(Self);
      FImages.RegisterChanges(FChangeLink);
    end;
    Invalidate;
  end;
end;

procedure TJvDesktopAlertButton.SetToolType(const Value: TJvDesktopAlertButtonType);
begin
  if FToolType <> Value then
  begin
    FToolType := Value;
    Invalidate;
  end;
end;

initialization
  RegisterClasses([TLabel, TImage, TAction, TJvDesktopAlertButton, TJvLabel]);

end.

