{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDesktopAlertForm.PAS, released on 2004-03-23.

The Initial Developer of the Original Code is Peter Thornqvist <peter3 at peter3 dot com>
Portions created by Sébastien Buysse are Copyright (C) 2004 Peter Thornqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2004-03-23

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
unit JvDesktopAlertForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ExtCtrls, ActnList, Menus, StdCtrls,
  JvButton, JvComponent;

type
  TJvDesktopAlertButtonType = (abtArrowLeft, abtArrowRight, abtClose, abtMaximize, abtMinimize, abtDropDown, abtRestore, abtImage);

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
    procedure Paint;override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ToolType:TJvDesktopAlertButtonType read FToolType write SetToolType;
    property DropDownMenu;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex;
    property Width default 21;
    property Height default 21;
    property OnClick;
  end;

  TJvFormDesktopAlert = class(TJvForm)
    procedure lblTextMouseEnter(Sender: TObject);
    procedure lblTextMouseLeave(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
  private
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnUserMove: TNotifyEvent;
    acClose: TAction;
    FadeTimer: TTimer;
    MouseTimer:TTimer;
    FMouseDown:boolean;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;

    procedure FadeInTimer(Sender:TObject);
    procedure FadeOutTimer(Sender:TObject);
    procedure WaitTimer(Sender:TObject);
    procedure CMMouseenter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseleave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure DoMouseTimer(Sender:TObject);
    procedure FadeIn;
    procedure FadeOut;
    procedure Wait;
  protected
    procedure DoShow; override;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
  public
    imIcon: TImage;
    lblText: TLabel;
    lblHeader: TLabel;
    tbDropDown, tbClose:TJvDesktopAlertButton;

    Moveable, MoveAnywhere, Closeable, ClickableMessage, MouseInControl: boolean;
    MaxAlphaBlendValue: byte;
    FadeInTime, FadeOutTime, WaitTime: integer;
    WindowColorFrom, WindowColorTo, CaptionColorFrom, CaptionColorTo, FrameColor:TColor;

    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    procedure SetNewTop(const Value: integer);
    procedure SetNewLeft(const Value:integer);
    procedure SetNewOrigin(ALeft, ATop:integer);
    property OnMouseEnter:TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave:TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnUserMove:TNotifyEvent read FOnUserMove write FOnUserMove;
  end;

implementation
uses
  JvJVCLUtils;

const
  cAlphaIncrement = 5;
  cCaptionHeight = 8;
  
{.$R *.dfm} // not needed

procedure DrawDesktopAlertCaption(Canvas:TCanvas; ARect:TRect; ColorFrom, ColorTo:TColor);
var i:integer;R:TRect;
begin
  GradientFillRect(Canvas,ARect, ColorFrom, ColorTo, fdTopToBottom, cCaptionHeight);
  R := ARect;
  Inc(R.Left,(R.Right - R.Left) div 2 - 20);
  Inc(R.Top, 3);
  R.Right := R.Left + 2;
  R.Bottom := R.Top + 2;
  for i := 0 to 9 do // draw the dots
  begin
    Canvas.Brush.Color := $808080;
    Canvas.FillRect(R);
    OffsetRect(R,1,1);
    Canvas.Brush.Color := $F8FCF8;
    Canvas.FillRect(R);
    Canvas.Brush.Color := $B8BCB8;
    Canvas.FillRect(Rect(R.Left, R.Top, R.Left + 1, R.Top + 1));
    OffsetRect(R, 3, -1);
  end;
end;

procedure DrawDesktopAlertWindow(Canvas:TCanvas;WindowRect:TRect;
  FrameColor:TColor; WindowColorFrom, WindowColorTo, CaptionColorFrom, CaptionColorTo:TColor);
var CaptionRect:TRect; ATop:integer; AColors:byte;
begin
  CaptionRect := WindowRect;
  CaptionRect.Bottom := CaptionRect.Top + cCaptionHeight;
  DrawDesktopAlertCaption(Canvas, CaptionRect,CaptionColorFrom, CaptionColorTo);
  ATop := WindowRect.Top;
  WindowRect.Top := CaptionRect.Bottom + 1;
  Dec(WindowRect.Bottom);
  if WindowRect.Bottom - WindowRect.Top < 255 then
    AColors := WindowRect.Bottom - WindowRect.Top
  else
    AColors := 32;
  GradientFillRect(Canvas,WindowRect,WindowColorFrom, WindowColorTo, fdTopToBottom, AColors);
  WindowRect.Top := ATop;
  Inc(WindowRect.Bottom);
  Canvas.Brush.Color := clGray;
  Canvas.FrameRect(WindowRect);
end;

procedure TJvFormDesktopAlert.lblTextMouseEnter(Sender: TObject);
begin
  if ClickableMessage then
  begin
    lblText.Font.Color := clNavy;
    lblText.Font.Style := lblText.Font.Style + [fsUnderLine];
  end;
end;

procedure TJvFormDesktopAlert.lblTextMouseLeave(Sender: TObject);
begin
  if ClickableMessage then
  begin
    lblText.Font.Color := clWindowText;
    lblText.Font.Style := lblText.Font.Style - [fsUnderLine];
  end;
end;

procedure TJvFormDesktopAlert.FormPaint(Sender: TObject);
begin
  DrawDesktopAlertWindow(Canvas, ClientRect, FrameColor, WindowColorFrom, WindowColorTo, CaptionColorFrom, CaptionColorTo);
end;

procedure TJvFormDesktopAlert.WMNCHitTest(var Message: TWMNCHitTest);
var P:TPoint;
begin
  with Message do
    P := ScreenToClient(Point(XPos, YPos));
  if ((P.Y <= cCaptionHeight) and Moveable) or (MoveAnywhere and (ControlAtPos(P,false) = nil)) then
  begin
    FadeTimer.Enabled := false;
    AlphaBlendValue := MaxAlphaBlendValue;
    Message.Result := HTCAPTION;
  end
  else
    inherited;
end;

procedure TJvFormDesktopAlert.acCloseExecute(Sender: TObject);
begin
  if Closeable then Close;
end;

procedure TJvFormDesktopAlert.CMMouseenter(var Message: TMessage);
begin
  inherited;
  MouseInControl := true;
//  SetFocus;
  BringToFront;
  FadeTimer.Enabled := false;
  AlphaBlendValue := MaxAlphaBlendValue;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvFormDesktopAlert.CMMouseleave(var Message: TMessage);
var P:TPoint;
begin
  inherited;
  // make sure the mouse actually left the outer boundaries
  GetCursorPos(P);
  if not PtInRect(BoundsRect, P) then
  begin
    if Assigned(FOnMouseLeave) then
      FOnMouseLeave(Self);
    FadeOut;
    MouseInControl := false;
  end;
end;

procedure TJvFormDesktopAlert.FadeInTimer(Sender: TObject);
begin
  FadeTimer.Enabled := false;
  if AlphaBlendValue <= MaxAlphaBlendValue - cAlphaIncrement then
    AlphaBlendValue := AlphaBlendValue + cAlphaIncrement;
  if AlphaBlendValue >= MaxAlphaBlendValue - cAlphaIncrement then
  begin
    AlphaBlendValue := MaxAlphaBlendValue;
    Wait;
  end
  else
    FadeTimer.Enabled := True;;
end;

procedure TJvFormDesktopAlert.FadeOutTimer(Sender: TObject);
begin
  FadeTimer.Enabled := false;
  if AlphaBlendValue >= cAlphaIncrement then
    AlphaBlendValue := AlphaBlendValue - cAlphaIncrement;
  if AlphaBlendValue <= cAlphaIncrement then
    Close
  else
    FadeTimer.Enabled := True;
end;

procedure TJvFormDesktopAlert.WaitTimer(Sender: TObject);
begin
  Update;
  FadeOut;
end;

{ TJvDesktopAlertButton }

constructor TJvDesktopAlertButton.Create(AOwner: TComponent);
begin
  inherited;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := DoImagesChange;
  Width := 21;
  Height := 21;
end;

destructor TJvDesktopAlertButton.Destroy;
begin
  FChangeLink.Free;
  inherited;
end;

procedure TJvDesktopAlertButton.DoImagesChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvDesktopAlertButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = Images then
      Images := nil;
  end;
end;

procedure TJvDesktopAlertButton.Paint;
var Rect:TRect;
begin
  with Canvas do
  begin
    Rect := ClientRect;
    Brush.Style := bsClear;
    if bsMouseInside in MouseStates then
    begin
      Pen.Color := $00663300;
      Rectangle(Rect);
      InflateRect(Rect, -1, -1);
      if bsMouseDown in MouseStates then
        Brush.Color := $00CC9999
      else
        Brush.Color := $00D6BEB5;
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
      abtRestore:
      begin
        Canvas.Font.Name := 'Marlett';
        Canvas.Font.Style := [];
        DrawText(Canvas.Handle, '3', 1, Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
      end;
      abtImage:
      begin
        if (Images = nil) or (ImageIndex < 0) or (ImageIndex >= Images.Count) then Exit;
        Images.Draw(Canvas,
            (Width - Images.Width) div 2 + Ord(bsMouseDown in MouseStates),
            (Height - Images.Height) div 2 + Ord(bsMouseDown in MouseStates), ImageIndex, dsTransparent, itImage, Enabled);
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

procedure TJvFormDesktopAlert.DoShow;
begin
  FadeTimer.Enabled := false;
  AlphaBlendValue := 0;

  if ClickableMessage then
    lblText.Cursor := crHandPoint
  else
    lblText.Cursor := crDefault;

  if tbDropDown.DropDownMenu = nil then
    tbDropDown.Visible := false;

  if not Closeable and (WaitTime > 0) then // must have either WaitTime or close button
  begin
    tbClose.Visible := false;
    tbDropDown.Left := tbClose.Left;
  end;


  imIcon.Top := 13;
  lblHeader.Top := imIcon.Top;
  lblHeader.Left := imIcon.Left + imIcon.Width + 5;
  lblText.Left := lblHeader.Left + 8;
  lblText.Width := tbDropDown.Left - lblText.Left;
  lblText.Top := lblHeader.Top + lblHeader.Height;
  FadeIn;
  inherited;
end;

procedure TJvFormDesktopAlert.WMMove(var Message: TWMMove);
begin
  inherited;
  if Showing and Assigned(FOnUserMove) and FMouseDown then
    FOnUserMove(Self);
end;

procedure TJvFormDesktopAlert.SetNewTop(const Value: integer);
begin
  SetNewOrigin(Left, Value);
end;

procedure TJvFormDesktopAlert.SetNewLeft(const Value: integer);
begin
  SetNewOrigin(Value, Top);
end;

procedure TJvFormDesktopAlert.SetNewOrigin(ALeft, ATop: integer);
var MoveEvent:TNotifyEvent;
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

constructor TJvFormDesktopAlert.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited CreateNew(AOwner, Dummy);
  if MouseTimer = nil then
    MouseTimer := TTimer.Create(Self);
  MouseTimer.Enabled := false;
  MouseTimer.Interval := 200;
  MouseTimer.OnTimer := DoMouseTimer;
  MouseTimer.Enabled := True;

  AlphaBlend := True;
  AlphaBlendValue := 0;
  BorderIcons := [];
  BorderStyle := bsNone;
  FormStyle := fsStayOnTop;
  Scaled := False;
  Height := 76;
  Width := 329;
  OnPaint := FormPaint;

  imIcon := TImage.Create(Self);
  imIcon.Parent := Self;
  imIcon.SetBounds(8,11,32,32);
  imIcon.AutoSize := True;
  imIcon.Transparent := True;

  lblHeader := TLabel.Create(Self);
  lblHeader.Parent := Self;
  lblHeader.SetBounds(48,11,71,13);
  lblHeader.Font.Style := [fsBold];
  lblHeader.Transparent := True;

  lblText := TLabel.Create(Self);
  lblText.Parent := Self;
  lblText.SetBounds(56, 24,67,13);
  lblText.Transparent := True;
  lblText.WordWrap := True;
  lblText.OnMouseEnter := lblTextMouseEnter;
  lblText.OnMouseLeave := lblTextMouseLeave;

  acClose := TAction.Create(Self);
  acClose.Caption := 'Close';

  acClose.ShortCut := ShortCut(VK_F4, [ssAlt]); // 32883
  acClose.OnExecute := acCloseExecute;

  FadeTimer := TTimer.Create(Self);
  FadeTimer.Enabled := False;

  tbClose := TJvDesktopAlertButton.Create(Self);
  tbClose.ToolType := abtClose;
  tbClose.Parent := Self;
  tbClose.SetBounds(Width - 17, cCaptionHeight + 2, 15,15);
  tbClose.Anchors := [akRight, akTop];
  tbClose.OnClick := acCloseExecute;

  tbDropDown := TJvDesktopAlertButton.Create(Self);
  tbDropDown.ToolType := abtDropDown;
  tbDropDown.Parent := Self;
  tbDropDown.BoundsRect := tbClose.BoundsRect;
  tbDropDown.Left := tbDropDown.Left - 16;
  tbDropDown.Anchors := [akRight, akTop];
end;

procedure TJvFormDesktopAlert.DoMouseTimer(Sender: TObject);
var P:TPoint;
begin
  // this is here to ensure that MouseInControl is corectly set even
  // if we never got a CM_MouseLeave (that happens a lot)
  MouseTimer.Enabled := false;
  GetCursorPos(P);
  MouseInControl := PtInRect(BoundsRect, P) and (FindVCLWindow(P) = Self);
  MouseTimer.Enabled := True;
  if not FadeTimer.Enabled and not MouseInControl and (WaitTime > 0) then
    FadeOut;
end;


procedure TJvFormDesktopAlert.FadeIn;
begin
  AlphaBlendValue := 0;
  Update;
  FadeTimer.Enabled := false;
  FadeTimer.Interval := FadeInTime;
  FadeTimer.OnTimer := FadeInTimer;
  FadeTimer.Enabled := FadeInTime > 0;
  if not FadeTimer.Enabled then
    Wait;
end;

procedure TJvFormDesktopAlert.FadeOut;
begin
  AlphaBlendValue := MaxAlphaBlendValue;
  Update;
  FadeTimer.Enabled := false;
  FadeTimer.Interval := FadeOutTime;
  FadeTimer.OnTimer := FadeOutTimer;
  FadeTimer.Enabled := FadeOutTime > 0;
  if not FadeTimer.Enabled then Close;
end;

procedure TJvFormDesktopAlert.Wait;
begin
  AlphaBlendValue := MaxAlphaBlendValue;
  Update;
  FadeTimer.Enabled := false;
  FadeTimer.Interval := WaitTime;
  FadeTimer.OnTimer := WaitTimer;
  FadeTimer.Enabled := WaitTime > 0;
  // NB! If waittime = 0 then we never close - user has to do that manually
//  if not FadeTimer.Enabled then
//    FadeOut;
end;

initialization
  RegisterClasses([TLabel, TImage, TAction, TJvDesktopAlertButton]);

end.

