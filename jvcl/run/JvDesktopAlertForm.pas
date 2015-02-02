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
Miha Vrhovnik (custom form display logic)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
* This form is used by the TJvDesktopAlert component

-----------------------------------------------------------------------------}
// $Id$

unit JvDesktopAlertForm;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls,
  ImgList, ActnList,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  JvButton, JvLabel, JvExForms;

const
  cDefaultAlertFormWidth = 329;
  cDefaultAlertFormHeight = 76;
  JVDESKTOPALERT_AUTOFREE = WM_USER + 1001;

type
  TJvDesktopAlertButtonType = (abtArrowLeft, abtArrowRight, abtClose, abtMaximize,
    abtMinimize, abtDropDown, abtDropDownChevron, abtRestore, abtImage);

  TJvDesktopAlertButton = class(TJvCustomGraphicButton)
  private
    FChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FImageIndex: TImageIndex;
    FToolType: TJvDesktopAlertButtonType;
    FInternalClick: TNotifyEvent;
    procedure SetImages(const Value: TCustomImageList);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure DoImagesChange(Sender: TObject);
    procedure SetToolType(const Value: TJvDesktopAlertButtonType);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
  public
    property InternalClick: TNotifyEvent read FInternalClick write FInternalClick;
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

  // We have to inherit from TJvExForm instead of TJvExCustmForm
  // because otherwise our custom forms might not load correctly
  // ('Property does not exist' exceptions are raised)
  TJvCustomFormDesktopAlert = class(TJvExForm)
  private
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnUserMove: TNotifyEvent;
    MouseTimer: TTimer;
    FCloseable: Boolean;
    FMoveable: Boolean;
    FMoveAnywhere: Boolean;
    FAllowFocus: Boolean;
    FCaptionColorTo: TColor;
    FWindowColorTo: TColor;
    FWindowColorFrom: TColor;
    FCaptionColorFrom: TColor;
    FFrameColor: TColor;
    FOnShown: TNotifyEvent;
    FOnShowing: TNotifyEvent;
    FEndInterval:Cardinal;
    FMouseInControl: Boolean;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMMove(var Msg: TWMMove); message WM_MOVE;
    procedure JvDeskTopAlertAutoFree(var Msg: TMessage); message JVDESKTOPALERT_AUTOFREE;
    procedure DoMouseTimer(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    function GetVisible: Boolean;
  protected
    acClose: TAction;
    procedure DoShow; override;
    procedure DoClose(var Action: TCloseAction); override;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    //override this one if you'd like to exes sth before form is shown
    procedure InternalDoShow; virtual;
    procedure CreateParams(var Params: TCreateParams); override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure acCloseExecute(Sender: TObject);
    procedure SetNewTop(const Value: Integer);
    procedure SetNewLeft(const Value: Integer);
    procedure SetNewOrigin(ALeft, ATop: Integer);
    procedure ShowNoActivate;

    property Moveable: Boolean read FMoveable write FMoveable;
    property MoveAnywhere: Boolean read FMoveAnywhere write FMoveAnywhere;
    property Closeable: Boolean read FCloseable write FCloseable;
    property MouseInControl: Boolean read FMouseInControl;
    property WindowColorFrom: TColor read FWindowColorFrom write FWindowColorFrom;
    property WindowColorTo: TColor read FWindowColorTo write FWindowColorTo;
    property CaptionColorFrom: TColor read FCaptionColorFrom write FCaptionColorFrom;
    property CaptionColorTo: TColor read FCaptionColorTo write FCaptionColorTo;
    property FrameColor: TColor read FFrameColor write FFrameColor;
    property AllowFocus: Boolean read FAllowFocus write FAllowFocus;

    property Showing read GetVisible;
    property Visible read GetVisible;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnUserMove: TNotifyEvent read FOnUserMove write FOnUserMove;
    property OnClose;
    property OnShowing: TNotifyEvent read FOnShowing write FOnShowing;
    property OnShow;
    property OnShown: TNotifyEvent read FOnShown write FOnShown;
  end;

  TJvFormDesktopAlert = class(TJvCustomFormDesktopAlert)
  private
    FClickableMessage: Boolean;
  protected
    procedure InternalDoShow; override;
    procedure DoDropDownClose(Sender: TObject);
    procedure DoDropDownMenu(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
  public
    imIcon: TImage;
    lblText: TJvLabel;
    lblHeader: TLabel;
    tbDropDown: TJvDesktopAlertButton;
    tbClose: TJvDesktopAlertButton;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoButtonClick(Sender: TObject);

    property ClickableMessage: Boolean read FClickableMessage write FClickableMessage;
    property ParentFont;
    property PopupMenu;
    property OnClose;
    property OnShowing;
    property OnShow;
    property OnShown;
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
  Types, Menus, SysUtils,
  JvJVCLUtils, JvDesktopAlert, JvResources;

{.$R *.dfm} // not needed

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
  Canvas.FrameRect(WindowRect);
end;

//=== { TJvFormDesktopAlert } ================================================

constructor TJvFormDesktopAlert.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner, 1);

  Font.Assign(Screen.IconFont);
  MouseTimer := TTimer.Create(Self);
  MouseTimer.Enabled := False;
  MouseTimer.Interval := 200;
  MouseTimer.OnTimer := DoMouseTimer;
  MouseTimer.Enabled := True;

  BorderStyle := bsNone;
  BorderIcons := [];
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
  lblText.AutoSize := False;
  lblText.SetBounds(56, 24, 67, 13);
  lblText.Transparent := True;
  lblText.WordWrap := True;
  lblText.Anchors := [akLeft..akBottom];
  {$IFDEF RTL170_UP} // 2005+
  lblText.TextEllipsis := teEndEllipsis;
  {$ENDIF RTL170_UP}

  acClose := TAction.Create(Self);
  acClose.Caption := RsClose;

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
  tbDropDown.OnDropDownMenu := DoDropDownMenu;
  tbDropDown.OnDropDownClose := DoDropDownClose;
end;

procedure TJvFormDesktopAlert.InternalDoShow;
begin
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

  // if the form is not closeable, then do not show the button
  if not Closeable then
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
end;

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

procedure TJvDesktopAlertButton.MouseEnter(Control: TControl);
begin
  inherited;
  Invalidate;
end;

procedure TJvDesktopAlertButton.MouseLeave(Control: TControl);
begin
  inherited;
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
const
  cMarlett = 'Marlett';
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
          Canvas.Font.Name := cMarlett;
          Canvas.Font.Style := [];
          Canvas.Font.Size := 10;
          DrawText(Canvas.Handle, '3', 1, Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
        end;
      abtArrowRight:
        begin
          Canvas.Font.Name := cMarlett;
          Canvas.Font.Style := [];
          Canvas.Font.Size := 10;
          DrawText(Canvas.Handle, '4', 1, Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
        end;
      abtClose:
        begin
          Canvas.Font.Name := cMarlett;
          Canvas.Font.Size := 7;
          Canvas.Font.Style := [];
          DrawText(Canvas.Handle, 'r', 1, Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
        end;
      abtMaximize:
        begin
          Canvas.Font.Name := cMarlett;
          Canvas.Font.Style := [];
          DrawText(Canvas.Handle, '2', 1, Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
        end;
      abtMinimize:
        begin
          Canvas.Font.Name := cMarlett;
          Canvas.Font.Style := [];
          DrawText(Canvas.Handle, '1', 1, Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
        end;
      abtDropDown:
        begin
          Canvas.Font.Name := cMarlett;
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
          Canvas.Font.Name := cMarlett;
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
            dsTransparent,
            itImage,
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
  if ReplaceImageListReference(Self, Value, FImages, FChangeLink) then
    Invalidate;
end;

procedure TJvDesktopAlertButton.SetToolType(const Value: TJvDesktopAlertButtonType);
begin
  if FToolType <> Value then
  begin
    FToolType := Value;
    Invalidate;
  end;
end;

procedure TJvFormDesktopAlert.DoButtonClick(Sender: TObject);
var
  FEndInterval: Cardinal;
begin
  if Sender is TJvDesktopAlertButton then
  begin
    FEndInterval := TJvDesktopAlert(Owner).StyleHandler.EndInterval;
    try
      // stop the animation while the OnClick handler executes:
      // we don't want the form to disappear before we return
      TJvDesktopAlert(Owner).StyleHandler.EndInterval := 0;
      if Assigned(TJvDesktopAlertButton(Sender).InternalClick) then
        TJvDesktopAlertButton(Sender).InternalClick(Sender);
    finally
      TJvDesktopAlert(Owner).StyleHandler.EndInterval := FEndInterval;
      if not MouseInControl then
        TJvDesktopAlert(Owner).StyleHandler.DoEndAnimation;
    end;
  end;
end;

procedure TJvFormDesktopAlert.DoDropDownClose(Sender: TObject);
begin
  // restore previous EndInterval value
  if FEndInterval <> 0 then
    TJvDesktopAlert(Owner).StyleHandler.EndInterval := FEndInterval;
  FEndInterval := 0;
  if not MouseInControl then
    TJvDesktopAlert(Owner).StyleHandler.DoEndAnimation;
end;

procedure TJvFormDesktopAlert.DoDropDownMenu(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  // suspend the form while the menu is visible
  FEndInterval := TJvDesktopAlert(Owner).StyleHandler.EndInterval;
  TJvDesktopAlert(Owner).StyleHandler.EndInterval := 0;
end;

{ TJvCustomFormDesktopAlert }

procedure TJvCustomFormDesktopAlert.acCloseExecute(Sender: TObject);
begin
  if Closeable then
    Close;
end;

constructor TJvCustomFormDesktopAlert.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  MouseTimer := TTimer.Create(Self);
  MouseTimer.Enabled := False;
  MouseTimer.Interval := 200;
  MouseTimer.OnTimer := DoMouseTimer;
  MouseTimer.Enabled := True;

  BorderStyle := bsNone;
  BorderIcons := [];
  Scaled := False;
  OnPaint := FormPaint;

  acClose := TAction.Create(Self);
  acClose.Caption := RsClose;

  acClose.ShortCut := ShortCut(VK_F4, [ssAlt]); // 32883
  acClose.OnExecute := acCloseExecute;
end;

procedure TJvCustomFormDesktopAlert.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_TOOLWINDOW;
  Params.WndParent := HWND_DESKTOP;
end;

procedure TJvCustomFormDesktopAlert.DoClose(var Action: TCloseAction);
begin
  MouseTimer.Enabled := False;
  inherited DoClose(Action);
  if Action = caHide then
    ShowWindow(Handle, SW_HIDE);
end;

procedure TJvCustomFormDesktopAlert.DoMouseTimer(Sender: TObject);
var
  P: TPoint;

  function IsInForm(P: TPoint): Boolean;
  var
    W: TControl;
  begin
    W := ControlAtPos(P, True, True);
    Result := (W = Self) or (FindVCLWindow(P) = Self) or ((W <> nil) and (GetParentForm(W) = Self));
  end;

begin
  // this is here to ensure that MouseInControl is correctly set even
  // if we never got a CM_MouseLeave (that happens a lot)
  MouseTimer.Enabled := False;
  GetCursorPos(P);
  FMouseInControl := PtInRect(BoundsRect, P); // and IsInForm(P);
  MouseTimer.Enabled := True;
  if not TJvCustomDesktopAlert(Owner).StyleHandler.Active and not MouseInControl and
    (TJvCustomDesktopAlert(Owner).StyleHandler.DisplayDuration > 0) then
    TJvCustomDesktopAlert(Owner).StyleHandler.DoEndAnimation;
end;

procedure TJvCustomFormDesktopAlert.DoShow;
begin
  if Assigned(OnShowing) then
    OnShowing(Self);

  inherited DoShow;
  TJvCustomDesktopAlert(Owner).StyleHandler.AbortAnimation;
  InternalDoShow;
  TJvCustomDesktopAlert(Owner).StyleHandler.DoStartAnimation;
  MouseTimer.Enabled := True;

  if Assigned(OnShown) then
    OnShown(Self);
end;

procedure TJvCustomFormDesktopAlert.FormPaint(Sender: TObject);
begin
  DrawDesktopAlertWindow(Canvas, ClientRect, FrameColor, WindowColorFrom, WindowColorTo, CaptionColorFrom, CaptionColorTo, Moveable or MoveAnywhere);
end;

function TJvCustomFormDesktopAlert.GetVisible: Boolean;
begin
  Result := IsWindowVisible(Handle);
end;

procedure TJvCustomFormDesktopAlert.InternalDoShow;
begin
//
end;

procedure TJvCustomFormDesktopAlert.JvDeskTopAlertAutoFree(var Msg: TMessage);
begin
  // WParam is us, LParam is the TJvDesktopAlert
  if Msg.WParam = WPARAM(Self) then
  begin
    Release;
    TObject(Msg.LParam).Free;
  end;
end;

procedure TJvCustomFormDesktopAlert.MouseEnter(AControl: TControl);
begin
  inherited MouseEnter(AControl);
  FMouseInControl := True;
  //  SetFocus;
  TJvCustomDesktopAlert(Owner).StyleHandler.AbortAnimation;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvCustomFormDesktopAlert.MouseLeave(AControl: TControl);
var
  P: TPoint;
begin
  inherited MouseLeave(AControl);
  // make sure the mouse actually left the outer boundaries
  GetCursorPos(P);
  if MouseInControl and not PtInRect(BoundsRect, P) then
  begin
    if Assigned(FOnMouseLeave) then
      FOnMouseLeave(Self);
    if not TJvCustomDesktopAlert(Owner).StyleHandler.Active and
      (TJvCustomDesktopAlert(Owner).StyleHandler.DisplayDuration > 0) then
      TJvCustomDesktopAlert(Owner).StyleHandler.DoEndAnimation;
    FMouseInControl := False;
  end;
end;

procedure TJvCustomFormDesktopAlert.SetNewLeft(const Value: Integer);
begin
  SetNewOrigin(Value, Top);
end;

procedure TJvCustomFormDesktopAlert.SetNewOrigin(ALeft, ATop: Integer);
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

procedure TJvCustomFormDesktopAlert.SetNewTop(const Value: Integer);
begin
  SetNewOrigin(Left, Value);
end;

procedure TJvCustomFormDesktopAlert.ShowNoActivate;
begin
  Visible := True;
  Include(FFormState, fsShowing);
//  Windows.SetParent(Handle, 0);
//-- The above was introduced to partially solve the issue of the visible
//--   TJvFormDesktopAlert(s) dropping behind another App when this App is
//--   defocused.
//-- Unfortunately, this re-introduces the bug of momentarily taking the focus
//--   away from the active form within this App, when it has the focus.
//--   A further side-effect is to set Application.Active := True
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or
    SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_NOREDRAW or SWP_NOSENDCHANGING);
  DoShow;
  Exclude(FFormState, fsShowing);
  Include(FFormState, fsVisible);
end;


procedure TJvCustomFormDesktopAlert.WMActivate(var Message: TWMActivate);
begin
  if (Message.Active = WA_INACTIVE) or AllowFocus then
    inherited
  else
    Message.Result := 1;
end;

procedure TJvCustomFormDesktopAlert.WMMouseActivate(var Message: TWMMouseActivate);
begin
  if AllowFocus then
    inherited
  else
    Message.Result := MA_NOACTIVATE;
end;

procedure TJvCustomFormDesktopAlert.WMMove(var Msg: TWMMove);
begin
  inherited;
  if Showing and Assigned(FOnUserMove) then
    FOnUserMove(Self);
end;

procedure TJvCustomFormDesktopAlert.WMNCHitTest(var Msg: TWMNCHitTest);
var
  P: TPoint;
begin
  P := ScreenToClient(Point(Msg.XPos, Msg.YPos));
  if ((P.Y <= cCaptionHeight) and Moveable) or (MoveAnywhere and (ControlAtPos(P, False) = nil)) then
  begin
    TJvCustomDesktopAlert(Owner).StyleHandler.AbortAnimation;
    Msg.Result := HTCAPTION;
  end
  else
    inherited;
end;


initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  RegisterClasses([TLabel, TImage, TAction, TJvDesktopAlertButton, TJvLabel]);

{$IFDEF UNITVERSIONING}
finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
