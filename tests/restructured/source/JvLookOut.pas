{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvLookOut.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

{ Outlook style control }

unit JvLookOut;

interface


uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, CommCtrl, Menus, ImgList, JvComponent;

const
  CM_IMAGESIZECHANGED = CM_BASE + 100;
  CM_LEAVEBUTTON = CM_BASE + 101;
  CM_LOOKOUTBUTTONPRESSED = CM_BASE + 102;

type
  TJvImageSize = (isSmall, isLarge);
  TJvButtonBorder = (bbDark, bbLight, bbMono);

  TJvSpacer = class(TGraphicControl)
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
  end;

  TJvUpArrowBtn = class(TSpeedButton)
  private
    FTimer: TTimer;
    FAutoRepeat, FDown, FFlat, FInsideButton: boolean;
    procedure SetFlat(Value: boolean);
    procedure CmDesignHitTest(var Message: TCmDesignHitTest); message Cm_DesignHitTest;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure OnTime(Sender: TObject); virtual;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
  published
    property Flat: boolean read FFlat write SetFlat default False;
    property AutoRepeat: boolean read FAutoRepeat write FAutoRepeat default True;
  end;

  TJvDwnArrowBtn = class(TJvUpArrowBtn)
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure OnTime(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TJvLookOutEditedEvent = procedure(Sender: TObject; var Caption: string) of object;

  TJvCustomLookOutButton = class(TJvGraphicControl)
  private
    FEdit: TEdit;
    FData: Pointer;
    FPImSize: boolean;
    FInsideButton: boolean;
    FDown: boolean;
    FStayDown: boolean;
    FCentered: boolean;
    FImageIndex: TImageIndex;
    FSpacing: integer;
    FOffset: integer;
    FImageSize: TJvImageSize;
    FImageRect: TRect;
    FTextRect: TRect;
    FFillColor: TColor;
    FHiFont: TFont;
    FButtonBorder: TJvButtonBorder;
    FPopUpMenu: TPopUpMenu;
    FCaption: TCaption;
    FGroupIndex: integer;
    FSmallImages: TImageList;
    FLargeImages: TImageList;
    FOnEdited: TJvLookOutEditedEvent;
    FLargeImageChangeLink: TChangeLink;
    FSmallImageChangeLink: TChangeLink;
    FMouseEnter: TNotifyEvent;
    FMouseExit: TNotifyEvent;
    procedure SetGroupIndex(Value: integer);
    procedure UpdateExclusive;
    procedure SetCentered(Value: boolean);
    procedure SetDown(Value: boolean);
    procedure SetOffset(Value: integer);
    procedure SetFillColor(Value: TColor);
    procedure SetHiFont(Value: TFont);
    procedure SetSpacing(Value: integer);
    procedure SetPImSize(Value: boolean);
    procedure SeTJvButtonBorder(Value: TJvButtonBorder);
    procedure SetCaption(Value: TCaption);
    procedure SetSmallImages(Value: TImageList);
    procedure SetLargeImages(Value: TImageList);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SeTJvImageSize(Value: TJvImageSize);
    procedure DrawSmallImages;
    procedure DrawLargeImages;
    procedure ImageListChange(Sender: TObject);
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMButtonPressed(var Message: TMessage); message CM_LOOKOUTBUTTONPRESSED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMParenTJvImageSizeChanged(var Message: TMessage); message CM_IMAGESIZECHANGED;
    procedure CMLeaveButton(var Msg: TMessage); message CM_LEAVEBUTTON;
    procedure WMEraseBkgnd(var M: TWMEraseBkgnd); message WM_ERASEBKGND;
    function ParentVisible: boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoOnEdited(var Caption: string); virtual;
    procedure EditKeyDown(Sender: TObject; var Key: Char);
    procedure EditMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure PaintFrame; virtual;
    procedure SetParent(AParent: TWinControl); override;
    procedure Paint; override;
    procedure MouseEnter; virtual;
    procedure MouseExit; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    property FillColor: TColor read FFillColor write SetFillColor default clNone;
    property Offset: integer read FOffset write SetOffset default 0;
    property ButtonBorder: TJvButtonBorder read FButtonBorder write SeTJvButtonBorder default bbDark;
    property Caption: TCaption read FCaption write SetCaption;
    property Centered: boolean read FCentered write SetCentered;
    property Down: boolean read FStayDown write SetDown default False;
    property HiLiteFont: TFont read FHiFont write SetHiFont;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex;
    property ImageSize: TJvImageSize read FImageSize write SeTJvImageSize default isLarge;
    property ParenTJvImageSize: boolean read FPImSize write SetPImSize default True;
    property PopUpMenu: TPopUpMenu read FPopUpMenu write FPopUpMenu;
    property LargeImages: TImageList read FLargeImages write SetLargeImages;
    property Spacing: integer read FSpacing write SetSpacing default 4; { border offset from bitmap }
    property SmallImages: TImagelist read FSmallImages write SetSmallImages;
    property Data: Pointer read FData write FData;
    property GroupIndex: integer read FGroupIndex write SetGroupIndex default 0;
    property OnEdited: TJvLookOutEditedEvent read FOnEdited write FOnEdited;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure Assign(Source: TPersistent); override;
    procedure EditCaption;
  published
    property Align;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Height default 60;
    property Left;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Top;
    property Visible;
    property Width default 60;
    property OnMouseEnter: TNotifyEvent read FMouseEnter write FMouseEnter;
    property OnMouseExit: TNotifyEvent read FMouseExit write FMouseEXit;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
  end;

  TJvLookOutButton = class(TJvCustomLookOutButton)
  public
    property Data;
  published
    property ButtonBorder;
    property Caption;
    property Down;
    property GroupIndex;
    property HiLiteFont;
    property ImageIndex;
    property ImageSize;
    property ParenTJvImageSize;
    property PopUpMenu;
    property LargeImages;
    property Spacing;
    property SmallImages;
    property OnEdited;
  end;

  TJvExpressButton = class(TJvCustomLookOutButton)
  public
    constructor Create(AOwner: TComponent); override;
    property Data;
  published
    property Down;
    property FillColor default clBtnFace;
    property GroupIndex;
    property Offset default 1;
    property ButtonBorder default bbLight;
    property Caption;
    property HiLiteFont;
    property ImageIndex;
    property ImageSize;
    property ParenTJvImageSize;
    property PopUpMenu;
    property LargeImages;
    property Spacing;
    property SmallImages;
    property OnEdited;
  end;

  TJvLookOut = class;
  TJvLookOutPage = class(TJvCustomControl)
  private
    { Private declarations }
    FEdit: TEdit;
    FInScroll: boolean;
    FAutoRepeat, FAutoCenter, FPImSize, FInsideButton, FDown, FShowPressed: boolean;
    FMargin, FTopControl: integer;
    FPopUpMenu: TPopUpMenu;
    FOnClick: TNotifyEvent;
    FDwnArrow: TJvDwnArrowBtn;
    FScrolling: integer;
    FUpArrow: TJvUpArrowBtn;
    FCaption: TCaption;
    FBitmap: TBitmap;
    FImageSize: TJvImageSize;
    FManager: TJvLookOut;
    FOnCollapse: TNotifyEvent;
    FHiFont: TFont;
    FButtons: TList;
    FActiveButton: TJvCustomLookOutButton;
    FOnEdited: TJvLookOutEditedEvent;
    procedure SetActiveButton(Value: TJvCustomLookOutButton);
    procedure EditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditKeyDown(Sender: TObject; var Key: Char);
    procedure SetAutoRepeat(Value: boolean);
    procedure SetHiFont(Value: TFont);
    procedure SeTJvImageSize(Value: TJvImageSize);
    procedure SetPImSize(Value: boolean);
    procedure SetBitmap(Value: TBitmap);
    procedure SetCaption(Value: TCaption);
    procedure SetMargin(Value: integer);
    procedure SetButton(Index: integer; Value: TJvLookOutButton);
    function GetButton(Index: integer): TJvLookOutButton;
    function GetButtonCount: integer;
    procedure SetAutoCenter(Value: boolean);
    function IsVisible(Control: TControl): boolean;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMParenTJvImageSizeChanged(var Message: TMessage); message CM_IMAGESIZECHANGED;
    procedure TileBitmap;
    procedure WMEraseBkgnd(var M: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    { Protected declarations }
    procedure DoOnEdited(var Caption: string); virtual;
    procedure UpArrowClick(Sender: TObject); virtual;
    procedure DownArrowClick(Sender: TObject); virtual;
    procedure DrawTopButton; virtual;
    procedure CalcArrows; virtual;
    procedure ScrollChildren(Start: word); virtual;
    procedure AlignControls(Control: TControl; var Rect: TRect); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure CreateWnd; override;
    procedure SmoothScroll(aControl: TControl; NewTop, Intervall: integer; Smooth: boolean); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    property AutoCenter: boolean read FAutoCenter write SetAutoCenter;
  public
    { Public declarations }
    procedure Click; override;
    procedure DownArrow;
    procedure UpArrow;
    function AddButton: TJvLookOutButton;
    procedure ExchangeButtons(Button1, Button2: TJvCustomLookOutButton); virtual;
    procedure EditCaption; virtual;
    procedure DisableAdjust;
    procedure EnableAdjust;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Buttons[Index: integer]: TJvLookOutButton read GetButton write SetButton;
    property ButtonCount: integer read GetButtonCount;
    property ActiveButton: TJvCustomLookOutButton read FActiveButton write SetActiveButton;
  published
    { Published declarations }
    property Align;
    property AutoRepeat: boolean read FAutoRepeat write SetAutoRepeat default False;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property ImageSize: TJvImageSize read FImageSize write SeTJvImageSize default isLarge;
    property HiLiteFont: TFont read FHiFont write SetHiFont;
    property ParenTJvImageSize: boolean read FPImSize write SetPImSize default True;
    property ShowPressed: boolean read FShowPressed write FShowPressed default False;
    property Caption: TCaption read FCaption write SetCaption;
    property Color;
    property DragCursor;
    property DragMode;
    property ShowHint;
    property Visible;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopUpMenu: TPopUpMenu read FPopUpMenu write FPopUpMenu;
    property Left;
    property Top;
    property Width;
    property Height;
    property Cursor;
    property Hint;
    property Margin: integer read FMargin write SetMargin default 0;
    property OnEdited: TJvLookOutEditedEvent read FOnEdited write FOnEdited;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
  end;

  TJvLookOut = class(TJvCustomControl)
  private
    FAutoSize, FScroll: boolean;
    FBorderStyle: TBorderStyle;
    FOnCollapse, FOnClick: TNotifyEvent;
    FActivePage, FCurrentPage: TJvLookOutPage;
    FPages: TList;
    FImageSize: TJvImageSize;
    FFlatButtons: boolean;
    procedure SeTJvImageSize(Value: TJvImageSize);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure UpdateControls;
    procedure DoCollapse(Sender: TObject);
    procedure SetActiveOutLook(Value: TJvLookOutPage);
    function GetActiveOutlook: TJvLookOutPage;
    function GetPageCount: integer;
    function GetPage(Index: integer): TJvLookOutPage;
    procedure SetPage(Index: integer; Value: TJvLookOutPage);
    procedure SetFlatButtons(Value: boolean);
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
  protected
    //PRY 2002.06.04
    {$IFDEF COMPILER6_UP}
    procedure SetAutoSize(Value: boolean); override;
    {$ELSE}
    procedure SetAutoSize(Value: boolean);
    {$ENDIF COMPILER6_UP}
    // PRY END
    procedure SmoothScroll(aControl: TControl; NewTop, Intervall: integer; Smooth: boolean); virtual;
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddPage: TJvLookOutPage;
    property Pages[Index: integer]: TJvLookOutPage read GetPage write SetPage;
    property PageCount: integer read GetPageCount;
  published
    property ActivePage: TJvLookOutPage read GetActiveOutlook write SetActiveOutlook;
    property Align;
    property AutoSize: boolean read FAutoSize write SetAutoSize default false;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Color default clBtnShadow;
    property FlatButtons: boolean read FFlatButtons write SetFlatButtons default false;
    property DragCursor;
    property DragMode;
    property ImageSize: TJvImageSize read FImageSize write SeTJvImageSize default isLarge;
    property ShowHint;
    property Smooth: boolean read FScroll write FScroll default False;
    property Visible;
    property Enabled;
    property Left;
    property Top;
    property Width default 92;
    property Height default 300;
    property Cursor;
    property Hint;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
  end;

  TJvExpress = class(TJvLookOutPage)
  private
    FBorderStyle: TBorderStyle;
    FButtonHeight: integer;
    procedure SetButtonHeight(Value: integer);
  protected
    procedure CalcArrows; override;
    procedure ScrollChildren(Start: word); override;
    procedure DrawTopButton; override;
    procedure Paint; override;
    procedure CreateWnd; override;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
  public
    constructor Create(AOwner: TComponent); override;
    function AddButton: TJvExpressButton;
  published
    property AutoCenter;
    property ButtonHeight: integer read FButtonHeight write SetButtonHeight default 60;
    property ImageSize default isLarge;
  end;

implementation

const
  cSpeed = 20;
  cHeight = 19;
  cInitTime = 400;
  cTimeDelay = 120;

  { utility }

  { this creates a correctly masked bitmap - for use with D2 TImageList }
  {
  procedure CreateMaskedImageList(ImageList:TImageList);
  var Bmp:TBitmap;i:integer;
  begin
    Bmp := TBitmap.Create;
    Bmp.Width := ImageList.Width;
    Bmp.Height := ImageList.Height;
    try
      for i := 0 to ImageList.Count - 1 do
      begin
        ImageList.GetBitmap(i,Bmp);
        ImageList.ReplaceMasked(i,Bmp,Bmp.TransparentColor);
      end;
    finally
      Bmp.Free;
    end;
  end;
  }

  { returns number of visible children }
  {
  function NoOfVisibles(Control:TWinControl):integer;
  var R:TRect;i:integer;
  begin
    R := Control.ClientRect;
    Result := 0;
    if (Control = nil) then
      Exit;
    for i := 0 to Control.ControlCount - 1 do
       if (PtInRect(R,Point(R.Left + 1,Control.Controls[i].Top)) and
         PtInRect(R,Point(R.Left + 1,Control.Controls[i].Top + Control.Controls[i].Height)))  then
           Inc(Result);
  end;
  }

  {
  function IMax(Val1,Val2:integer):integer;
  begin
    Result := Val1;
    if Val2 > Val1 then
      Result := Val2;
  end;

  function IMin(Val1,Val2:integer):integer;
  begin
    Result := Val1;
    if Val2 < Val1 then
      Result := Val2;
  end;
  }

  { returns Atleast if Value < AtLeast, Val1 otherwise }
  {
  function IAtLeast(Value,AtLeast:integer):integer;
  begin
    Result := Value;
    if Value < AtLeast then
      Result := AtLeast;
  end;
  }

  { TJvLookOutEdit }

type
  TJvLookOutEdit = class(TEdit)
  private
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  end;

procedure TJvLookOutEdit.CMExit(var Message: TCMExit);
begin
  Visible := False;
end;

{ TJvSpacer }

constructor TJvSpacer.Create(Aowner: TComponent);
begin
  inherited Create(AOwner);
  SetBounds(0, 0, 80, 10);
end;

procedure TJvSpacer.Paint;
begin
  if csDesigning in ComponentState then
  begin
    with Canvas do
    begin
      Brush.Color := clBlack;
      FrameRect(GetClientRect);
    end;
  end;
end;

{ TJvUpArrowBtn }

constructor TJvUpArrowBtn.Create(AOwner: TComponent);
var FSize: word;
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csSetCaption];
  ParentColor := True;
  FDown := False;
  FInsideButton := False;
  FAutoRepeat := False;
  FFlat := False;
  FSize := GetSystemMetrics(SM_CXVSCROLL);
  SetBounds(0, 0, FSize, FSize);
end;

procedure TJvUpArrowBtn.SetFlat(Value: boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TJvUpArrowBtn.CMMouseEnter(var Message: TMessage);
begin
  FInsideButton := True;
  if FFlat then
    Invalidate;
end;

procedure TJvUpArrowBtn.CMMouseLeave(var Message: TMessage);
begin
  FInsideButton := False;
  //  FDown := False;
  if FFlat then
    Invalidate;
end;

procedure TJvUpArrowBtn.CmDesignHitTest(var Message: TCmDesignHitTest);
begin
  Message.Result := 1;
end;

procedure TJvUpArrowBtn.Paint;
var Flags: integer; R: TRect;
begin
  //  if not Visible then Exit;
  R := GetClientRect;

  if FDown then
    Flags := DFCS_PUSHED
  else
    Flags := 0;
  if not Enabled then
    Flags := Flags or DFCS_INACTIVE;

  if FFlat and not FInsideButton then
  begin
    Flags := Flags or DFCS_FLAT;
    OffsetRect(R, 0, -2);
  end;

  if FFlat then
    InflateRect(R, 1, 1);
  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := Color;
  DrawFrameControl(Canvas.Handle, R, DFC_SCROLL, DFCS_SCROLLUP or Flags);

  if FFlat and FInsideButton then
  begin
    R := GetClientRect;

    if FDown then
      Frame3d(Canvas, R, clBlack, clWhite, 1)
    else
      Frame3d(Canvas, R, clWhite, clBlack, 1);
  end;
end;

procedure TJvUpArrowBtn.Click;
begin
  if Enabled then
  begin
    inherited Click;
    ReleaseCapture;
  end;
end;

procedure TJvUpArrowBtn.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDown := True;
  inherited MouseDown(Button, Shift, X, Y);
  if Parent is TJvLookOutPage then
    FAutoRepeat := TJvLookOutPage(Parent).AutoRepeat;
  if FAutoRepeat then
  begin

    if not Assigned(FTimer) then
      FTimer := TTimer.Create(self);
    with FTimer do
    begin
      OnTimer := OnTime;
      Interval := cInitTime;
      Enabled := True;
    end;
  end;
  Repaint;
end;

procedure TJvUpArrowBtn.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Assigned(FTimer) then
  begin
    FTimer.Free;
    FTimer := nil;
  end;
  FDown := False;
  (Parent as TJvLookOutPage).UpArrowClick(self);
end;

procedure TJvUpArrowBtn.OnTime(Sender: TObject);
var R: TRect;
begin
  FTimer.Interval := cTimeDelay;
  if FDown and MouseCapture and Visible then
  begin
    (Parent as TJvLookOutPage).UpArrowClick(self);
    R := Parent.ClientRect;
    R := Rect(R.Left, R.Top + cHeight, R.Right, R.Bottom);
    InvalidateRect(Parent.Handle, @R, False);
    Parent.Update;
  end;
end;

{ TJvDwnArrowBtn }

constructor TJvDwnArrowBtn.Create(AOwner: TComponent);
var FSize: word;
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csSetCaption];
  ParentColor := True;
  FDown := False;
  FInsideButton := False;
  FFlat := False;
  FSize := GetSystemMetrics(SM_CXVSCROLL);
  SetBounds(0, 0, FSize, FSize);
end;

procedure TJvDwnArrowBtn.Paint;
var Flags: integer; R: TRect;
begin
  //  if not Visible then Exit;
  R := GetClientRect;
  if FDown then
    Flags := DFCS_PUSHED
  else
    Flags := 0;
  if not Enabled then
    Flags := Flags or DFCS_INACTIVE;
  if FFlat and not FInsideButton then
  begin
    Flags := Flags or DFCS_FLAT;
    OffsetRect(R, 0, 2);
  end;

  if FFlat then
    InflateRect(R, 1, 1);
  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := Color;

  DrawFrameControl(Canvas.Handle, R, DFC_SCROLL, DFCS_SCROLLDOWN or Flags);

  if FFlat and FInsideButton then
  begin
    R := GetClientRect;
    if FDown then
      Frame3d(Canvas, R, clBlack, clBtnShadow, 1)
    else
      Frame3d(Canvas, R, clWhite, clBlack, 1);
  end;
end;

procedure TJvDwnArrowBtn.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDown := True;

  //  inherited MouseDown(Button, Shift, X, Y);
  if Assigned(OnMouseDown) then
    OnMousedown(self, Button, Shift, X, y);
  if Parent is TJvLookOutPage then
    FAutoRepeat := TJvLookOutPage(Parent).AutoRepeat;
  if FAutoRepeat then
  begin
    if not Assigned(FTimer) then
      FTimer := TTimer.Create(self);
    with FTimer do
    begin
      OnTimer := OnTime;
      Interval := cInitTime;
      Enabled := True;
    end;
  end;
  Repaint;
end;

procedure TJvDwnArrowBtn.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  //  inherited MouseUp(Button, Shift, X, Y);
  if Assigned(OnMouseUp) then
    OnMouseUp(self, Button, Shift, X, y);
  FDown := False;
  (Parent as TJvLookOutPage).DownArrowClick(self);
  //  Parent.ScrollBy(0,-50);
  if Assigned(FTimer) then
  begin
    FTimer.Free;
    FTimer := nil;
  end;
  Repaint;
end;

procedure TJvDwnArrowBtn.OnTime(Sender: TObject);
var R: TRect;
begin
  FTimer.Interval := cTimeDelay;
  if FDown and MouseCapture then
  begin
    (Parent as TJvLookOutPage).DownArrowClick(self);
    //    Parent.ScrollBy(0,-50);
    R := Parent.ClientRect;
    R := Rect(R.Left, R.Top + cHeight, R.Right, R.Bottom);
    InvalidateRect(Parent.Handle, @R, False);
    Parent.Update;
    if not Visible then
      FDown := False;
  end;
end;

{ TJvCustomLookOutButton }

constructor TJvCustomLookOutButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents];
  FButtonBorder := bbDark;
  FPImSize := True;
  FImageSize := isLarge;
  FFillColor := clNone;
  FSpacing := 4;
  FOffset := 0;
  FStayDown := False;
  FHiFont := TFont.Create;
  FHiFont.Assign(Font);
  Width := 60;
  Height := 60;
  FLargeImageChangeLink := TChangeLink.Create;
  FSmallImageChangeLink := TChangeLink.Create;
  FLargeImageChangeLink.OnChange := ImageListChange;
  FSmallImageChangeLink.OnChange := ImageListChange;
end;

destructor TJvCustomLookOutButton.Destroy;
begin
  if Assigned(FEdit) then
    FEdit.Free;
  FLargeImageChangeLink.Free;
  FSmallImageChangeLink.Free;
  FHiFont.Free;
  inherited Destroy;
end;

procedure TJvCustomLookOutButton.Click;
begin
  inherited Click;
end;

procedure TJvCustomLookOutButton.EditCaption;
begin
  if not Assigned(FEdit) then
  begin
    FEdit := TJvLookOutEdit.Create(nil);
    FEdit.Parent := self.Parent;
    FEdit.Visible := false;
  end;

  FEdit.SetBounds(Left + FTextRect.Left, Top + FTextRect.Top,
    Width, FTextRect.Bottom - FTextRect.Top);
  with FEdit do
  begin
    Text := FCaption;
    BorderStyle := bsNone;
    AutoSelect := True;
    OnKeyPress := EditKeydown;
    OnMouseDown := EditMouseDown;
    if not Visible then
      Show;
    SetFocus;
    SetCapture(FEdit.Handle);
    SelStart := 0;
    SelLength := Length(FCaption);
  end;
end;

procedure TJvCustomLookOutButton.DoOnEdited(var Caption: string);
begin
  if Assigned(FOnEdited) then
    FOnEdited(self, Caption);
end;

procedure TJvCustomLookOutButton.EditKeyDown(Sender: TObject; var Key: Char);
var aCaption: string; Modify: boolean;
begin
  Modify := False;
  if (Sender = FEdit) then
    case Key of
      #13:
        begin
          aCaption := FEdit.Text;
          DoOnEdited(aCaption);
          FEdit.Text := aCaption;
          Key := #0;
          Modify := True;
          if FEdit.Handle = GetCapture then
            ReleaseCapture;
          FEdit.Hide;
          FEdit.Free;
          FEdit := nil;
          Screen.Cursor := crDefault;
        end;
      #27:
        begin
          Key := #0;
          if FEdit.Handle = GetCapture then
            ReleaseCapture;
          FEdit.Hide;
          FEdit.Free;
          FEdit := nil;
          Screen.Cursor := crDefault;
        end;
    end; { case }
  if Modify then
    FCaption := aCaption;
end;

procedure TJvCustomLookOutButton.EditMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Assigned(FEdit) then
  begin
    if not PtInRect(FEdit.ClientRect, Point(X, Y)) or ((Button = mbRight) and FEdit.Visible) then
    begin
      if FEdit.Handle = GetCapture then
        ReleaseCapture;
      Screen.Cursor := crDefault;
      FEdit.Hide;
      FEdit.Free;
      FEdit := nil;
    end
    else
    begin
      ReleaseCapture;
      Screen.Cursor := crIBeam;
      SetCapture(FEdit.Handle);
    end;
  end;
end;

procedure TJvCustomLookOutButton.Assign(Source: TPersistent);
begin
  if Source is TJvCustomLookOutButton then
  begin
    Offset := TJvCustomLookOutButton(Source).Offset;
    Height := TJvCustomLookOutButton(Source).Height;
    Width := TJvCustomLookOutButton(Source).Width;
    ButtonBorder := TJvCustomLookOutButton(Source).ButtonBorder;
    Caption := TJvCustomLookOutButton(Source).Caption;
    Centered := TJvCustomLookOutButton(Source).Centered;
    Down := TJvCustomLookOutButton(Source).Down;
    Font := TJvCustomLookOutButton(Source).Font;
    HiLiteFont := TJvCustomLookOutButton(Source).HiLiteFont;
    ParenTJvImageSize := TJvCustomLookOutButton(Source).ParenTJvImageSize;
    ImageSize := TJvCustomLookOutButton(Source).ImageSize;
    ImageIndex := TJvCustomLookOutButton(Source).ImageIndex;
    LargeImages := TJvCustomLookOutButton(Source).LargeImages;
    SmallImages := TJvCustomLookOutButton(Source).SmallImages;
    Spacing := TJvCustomLookOutButton(Source).Spacing;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TJvCustomLookOutButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, FCaption) and Enabled and Visible and ParentVisible then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;

function TJvCustomLookOutButton.ParentVisible: boolean;
begin
  Result := false;
  if Parent = nil then
    Exit;
  if (Parent is TJvLookOutPage) and (Parent.Parent is TJvLookOut) then
    Result := TJvLookOutPage(Parent) = TJvLookOut(Parent.Parent).ActivePage
  else
    Result := Parent.Visible;
end;

procedure TJvCustomLookOutButton.SetGroupIndex(Value: integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TJvCustomLookOutButton.UpdateExclusive;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_LOOKOUTBUTTONPRESSED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := Longint(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TJvCustomLookOutButton.SetCentered(Value: boolean);
begin
  if FCentered <> Value then
  begin
    FCentered := Value;
    Repaint;
  end;
end;

procedure TJvCustomLookOutButton.SetDown(Value: boolean);
begin
  if FStayDown <> Value then
  begin
    FStayDown := Value;
    if FStayDown then
    begin
      FInsideButton := True;
      FDown := True;
    end
    else
      FDown := False;
    if FStayDown then
      UpdateExclusive;
    Invalidate;
  end;
end;

procedure TJvCustomLookOutButton.SetOffset(Value: integer);
begin
  if FOffset <> Value then
    FOffset := Value;
end;

procedure TJvCustomLookOutButton.SetCaption(Value: TCaption);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLookOutButton.SeTJvButtonBorder(Value: TJvButtonBorder);
begin
  if FButtonBorder <> Value then
  begin
    FButtonBorder := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLookOutButton.SetSmallImages(Value: TImageList);
begin
  if FSmallImages <> nil then
    FSmallImages.UnRegisterChanges(FSmallImageChangeLink);
  FSmallImages := Value;

  if FSmallImages <> nil then
    FSmallImages.RegisterChanges(FSmallImageChangeLink);
  Repaint;
end;

procedure TJvCustomLookOutButton.SetLargeImages(Value: TImageList);
begin
  if Assigned(FLargeImages) then
    FLargeImages.UnRegisterChanges(FLargeImageChangeLink);
  FLargeImages := Value;

  if Assigned(FLargeImages) then
    FLargeImages.RegisterChanges(FLargeImageChangeLink);
  Repaint;
end;

procedure TJvCustomLookOutButton.SetImageIndex(Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLookOutButton.SeTJvImageSize(Value: TJvImageSize);
begin
  if FImageSize <> Value then
  begin
    FImageSize := Value;
    if csDesigning in ComponentState then
      SetPImSize(False);
    Invalidate;
  end;
end;

procedure TJvCustomLookOutButton.SetFillColor(Value: TColor);
begin
  if FFillColor <> Value then
  begin
    FFillColor := Value;
    Repaint;
  end;
end;

procedure TJvCustomLookOutButton.SetHiFont(Value: TFont);
begin
  FHiFont.Assign(Value);
  if FHiFont <> Font then
    Invalidate;
end;

procedure TJvCustomLookOutButton.SetSpacing(Value: integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLookOutButton.SetPImSize(Value: boolean);
begin
  FPImSize := Value;
  if FPImSize and (Parent is TJvLookOutPage) then
    SeTJvImageSize((Parent as TJvLookOutPage).ImageSize);
end;

procedure TJvCustomLookOutButton.Paint;
var R: TRect; Flags, h: integer;
begin
  R := GetClientRect;

  with Canvas do
  begin
    if csDesigning in ComponentState then
    begin
      Brush.Color := clBlack;
      FrameRect(R);
    end;

    if (FImageSize = isSmall) and Assigned(FSmallImages) then
    begin
      FImageRect.Left := FSpacing;
      FImageRect.Right := FImageRect.Left + FSmallImages.Width;
      FImageRect.Top := (Height - FSmallImages.Height) div 2;
      FImageRect.Bottom := FImageRect.Top + FSmallImages.Height;
    end
    else if Assigned(FLargeImages) then
    begin
      FImageRect.Left := (Width - FLargeImages.Width) div 2;
      FImageRect.Right := FImageRect.Left + FLargeImages.Width;
      FImageRect.Top := FSpacing;
      FImageRect.Bottom := FImageRect.Top + FLargeImages.Height;
    end;

    PaintFrame;

    Flags := DT_END_ELLIPSIS or DT_EDITCONTROL;

    if (FImageSize = isSmall) and Assigned(FSmallImages) then
    begin
      DrawSmallImages;
      Flags := Flags or DT_VCENTER or DT_SINGLELINE;
      //      W := FSmallImages.Width;
    end
    else if (FImageSize = isLarge) and Assigned(FLargeImages) then
    begin
      DrawLargeImages;
      //      W := FLargeImages.Width;
      Flags := Flags or DT_WORDBREAK or DT_CENTER;
    end;
  end;

  { draw text }
  if Length(Caption) > 0 then
  begin
    if FInsideButton then
      Canvas.Font := FHiFont
    else
      Canvas.Font := Font;

    //    W := FSpacing  + W;
    SetBkMode(Canvas.Handle, Windows.Transparent);
    R := GetClientRect;
    if (ImageSize = isLarge) and Assigned(FLargeImages) then
      R.Top := R.Top + FLargeImages.Height + (FSpacing * 2)
    else if (ImageSize = isSmall) and Assigned(FSmallImages) then
      R.Left := R.Left + FSmallImages.Width + (FSpacing * 3)
    else
      Flags := DT_END_ELLIPSIS or DT_EDITCONTROL or DT_WORDBREAK or DT_CENTER or DT_VCENTER;
    if FDown then
      OffsetRect(R, FOffset, FOffset);
    FTextRect := R;
    h := DrawText(Canvas.Handle, PChar(Caption), -1, FTextRect, Flags or DT_CALCRECT);
    if (ImageSize = isLarge) then
    begin
      FTextRect.Top := R.Top;
      FTextRect.Bottom := FTextRect.Top + h;
      FTextRect.Right := R.Left + Canvas.TextWidth(Caption);
    end
    else
    begin
      FTextRect.Top := (Height - Canvas.TextHeight(Caption)) div 2;
      FTextRect.Bottom := FTextRect.Top + Canvas.TextHeight(Caption);
      FTextRect.Right := R.Left + Canvas.TextWidth(Caption);
    end;
    DrawText(Canvas.Handle, PChar(Caption), -1, R, Flags);
  end;
end;

procedure TJvCustomLookOutButton.DrawSmallImages;
begin
  if FDown then
    OffsetRect(FImageRect, FOffset, FOffset);
  FSmallImages.Draw(Canvas, FImageRect.Left, FImageRect.Top, FImageIndex);
  {   ImageList_DrawEx(FSmallImages.Handle,FImageIndex,Canvas.Handle,
         FImageRect.Left,FImageRect.Top,0,0,clNone,clNone,ILD_TRANSPARENT);}
end;

procedure TJvCustomLookOutButton.DrawLargeImages;
begin
  if FDown then
    OffsetRect(FImageRect, FOffset, FOffset);
  FLargeImages.Draw(Canvas, FImageRect.Left, FImageRect.Top, FImageIndex);

  {  ImageList_DrawEx(FLargeImages.Handle,FImageIndex,Canvas.Handle,
       FImageRect.Left,FImageRect.Top,0,0,clNone,clNone,ILD_TRANSPARENT);}
end;

procedure TJvCustomLookOutButton.PaintFrame;
var R: TRect;
begin
  R := GetClientRect;

  if csDesigning in ComponentState then
  begin
    Canvas.Brush.Color := clBlack;
    Canvas.FrameRect(R);
  end;

  if not Enabled then
    Exit;
  if (FInsideButton or (csDesigning in ComponentState)) then
  begin
    if (FFillColor = clNone) then
    begin
      R := FImageRect;
      InflateRect(R, Spacing, Spacing);
    end
    else
    begin { fill it up! }
      Canvas.Brush.Color := FFillColor;
      Windows.FillRect(Canvas.Handle, R, Canvas.Brush.Handle);
    end;

    if FDown then
    begin
      if FButtonBorder = bbDark then
        Frame3D(Canvas, R, cl3DDkShadow, clBtnFace, 1)
      else if FButtonBorder = bbLight then
        Frame3D(Canvas, R, clBtnShadow, clBtnHighLight, 1)
      else
        Frame3D(Canvas, R, cl3DDkShadow, clBtnHighLight, 1)
    end
    else
      case FButtonBorder of
        bbDark: Frame3D(Canvas, R, clBtnFace, cl3DDkShadow, 1);
        bbLight: Frame3D(Canvas, R, clBtnHighLight, clBtnShadow, 1);
      else
        Frame3D(Canvas, R, clBtnHighLight, cl3DDkShadow, 1);
      end;
  end;
end;

procedure TJvCustomLookOutButton.ImageListChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvCustomLookOutButton.WMEraseBkgnd(var M: TWMEraseBkgnd);
begin
  inherited;
end;

procedure TJvCustomLookOutButton.CMParenTJvImageSizeChanged(var Message: TMessage);
var FTmp: boolean;
begin
  if (Message.LParam <> Longint(self)) and FPImSize then
  begin
    FTmp := FPImSize;
    SeTJvImageSize(TJvImageSize(Message.WParam));
    FPImSize := FTmp;
  end;
end;

procedure TJvCustomLookOutButton.CMButtonPressed(var Message: TMessage);
var
  Sender: TJvCustomLookOutButton;
begin
  if Message.WParam = FGroupIndex then
  begin
    Sender := TJvCustomLookOutButton(Message.LParam);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FStayDown := False;
        FDown := false;
        FInsideButton := false;
        Invalidate;
      end;
    end;
  end;
end;

procedure TJvCustomLookOutButton.MouseEnter;
begin
  if Assigned(FMouseEnter) then
    FMouseEnter(Self);
end;

procedure TJvCustomLookOutButton.MouseExit;
begin
  if Assigned(FMouseExit) then
    FMouseExit(Self);
end;

procedure TJvCustomLookOutButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  MouseEnter;
  FInsideButton := True;
  if FFillColor = clNone then
    PaintFrame
  else
    Invalidate;
end;

procedure TJvCustomLookOutButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  MouseExit;
  if FInsideButton and not FStayDown then
  begin
    FInsideButton := False;
    Invalidate;
  end;
end;

procedure TJvCustomLookOutButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var tmp: TPoint; Msg: TMsg;
begin
  if Parent is TJvLookOutPage then
    TJvLookOutPage(Parent).ActiveButton := self;

  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbRight) then
  begin
    if Assigned(FPopUpMenu) then
    begin
      { calc where to put menu }
      tmp := ClientToScreen(Point(X, Y));
      FPopUpMenu.PopupComponent := self;
      FPopUpMenu.Popup(tmp.X, tmp.Y);
      { wait 'til menu is done }
      while PeekMessage(Msg, 0, WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE) do
        ;
    end;
    { release button }
    if not FStayDown then
      FDown := False;
  end
  else if FInsideButton and (Button = mbLeft) then
    FDown := True
  else if not FStayDown then
    FDown := False;

  if FGroupIndex <> 0 then
    SetDown(not FStayDown);
  if (FOffset = 0) then
    PaintFrame
  else
    Invalidate;
  //  Parent.Update;
end;

procedure TJvCustomLookOutButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var Msg: TMessage;
begin
  inherited MouseMove(Shift, X, Y);

  if PtInRect(GetClientRect, Point(X, Y)) then { entire button }
  begin
    if not FInsideButton then
    begin
      FInsideButton := True;
      { notify others }
      Msg.Msg := CM_LEAVEBUTTON;
      Msg.WParam := 0;
      Msg.LParam := Longint(self);
      Msg.Result := 0;
      Invalidate;
      Parent.Broadcast(Msg);
    end;
  end
  else if FInsideButton then
  begin
    FInsideButton := False;
    Invalidate;
  end;
end;

procedure TJvCustomLookOutButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDown and not FStayDown then
  begin
    FDown := False;
    if (FOffset = 0) then
      PaintFrame
    else
      Invalidate;
    //    Parent.Update;
  end;
end;

procedure TJvCustomLookOutButton.CMLeaveButton(var Msg: TMessage);
begin
  if (Msg.LParam <> longint(self)) and FInsideButton and not FStayDown then
  begin
    FInsideButton := False;
    //    FDown := False;
    Invalidate;
  end;
end;

procedure TJvCustomLookOutButton.SetParent(AParent: TWinControl);
begin
  if (AParent <> Parent) then
  begin
    if (Parent <> nil) and (Parent is TJvLookOutPage) then
      TJvLookOutPage(Parent).FButtons.Delete(TJvLookOutPage(Parent).FButtons.IndexOf(self));
    if (AParent <> nil) and (AParent is TJvLookOutPage) then
      TJvLookOutPage(AParent).FButtons.Add(self);
  end;
  inherited SetParent(AParent);
end;

procedure TJvCustomLookOutButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FSmallImages then
      FSmallImages := nil;
    if AComponent = FLargeImages then
      FLargeImages := nil;
    if AComponent = FPopUpMenu then
      FPopUpMenu := nil;
  end;
  Invalidate;
end;

{ TJvExpressButton }

constructor TJvExpressButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FillColor := clBtnFace;
  Offset := 1;
  FButtonBorder := bbLight;
  FHiFont.Color := clBlack;
  Font.Color := clWhite;
end;

{ TJvLookOutPage }

constructor TJvLookOutPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csSetCaption];
  Color := clBtnShadow;
  FScrolling := 0;
  FCaption := 'Outlook';
  FButtons := TList.Create;
  FDown := False;
  FShowPressed := False;
  SetBounds(0, 0, 92, 100);
  FInsideButton := False;
  FHiFont := TFont.Create;
  FHiFont.Assign(Font);
  FMargin := 0;
  FTopControl := 0;
  FPImSize := True;
  FAutoRepeat := False;
  FBitmap := TBitmap.Create;
end;

destructor TJvLookOutPage.Destroy;
begin
  if Assigned(FEdit) then
    FEdit.Free;
  FUpArrow.Free;
  FDwnArrow.Free;
  FBitmap.Free;
  FHiFont.Free;
  FButtons.Free;
  inherited Destroy;
end;

procedure TJvLookOutPage.DisableAdjust;
begin
  Inc(FScrolling);
end;

procedure TJvLookOutPage.EnableAdjust;
begin
  Dec(FScrolling);
end;

procedure TJvLookOutPage.DownArrow;
begin
  if Enabled then
    DownArrowClick(self);
  Invalidate;
end;

procedure TJvLookOutPage.UpArrow;
begin
  if Enabled then
    UpArrowClick(self);
  Invalidate;
end;

procedure TJvLookOutPage.ExchangeButtons(Button1, Button2: TJvCustomLookOutButton);
var tmp1: integer;
begin
  tmp1 := Button1.Top;
  Button1.Top := Button2.Top;
  Button2.Top := tmp1;
  FButtons.Exchange(FButtons.IndexOf(Button1), FButtons.IndexOf(Button2));
end;

function TJvLookOutPage.AddButton: TJvLookOutButton;
begin
  Result := TJvLookOutButton.Create(Self.Owner);
  Result.ImageIndex := ButtonCount;
  Result.Parent := self;
  Result.Top := MaxInt;
  if Assigned(FUpArrow) and Assigned(FDwnArrow) then
  begin
    FUpArrow.SetZOrder(True);
    FDwnArrow.SetZOrder(True);
  end;
end;

procedure TJvLookOutPage.DoOnEdited(var Caption: string);
begin
  if self is TJvExpress then
    Exit;
  if Assigned(FOnEdited) then
    FOnEdited(self, Caption);
end;

procedure TJvLookOutPage.EditCaption;
begin
  if Self is TJvExpress then
    Exit;

  if not Assigned(FEdit) then
  begin
    FEdit := TJvLookOutEdit.Create(nil);
    FEdit.Parent := self;
  end
  else if not FEdit.Visible then
    FEdit.Show;

  with FEdit do
  begin
    Text := FCaption;
    //    BorderStyle := bsNone;
    SetBounds(0, 0, Width, cHeight);
    AutoSelect := True;
    OnKeyPress := EditKeydown;
    OnMouseDown := EditMouseDown;
    SetFocus;
    SetCapture(FEdit.Handle);
    SelStart := 0;
    SelLength := Length(FCaption);
  end;
end;

procedure TJvLookOutPage.EditMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Assigned(FEdit) then
  begin
    if not PtInRect(FEdit.ClientRect, Point(X, Y)) or ((Button = mbRight) and FEdit.Visible) then
    begin
      if FEdit.Handle = GetCapture then
        ReleaseCapture;
      Screen.Cursor := crDefault;
      FEdit.Hide;
      FEdit.Free;
      FEdit := nil;
    end
    else
    begin
      ReleaseCapture;
      Screen.Cursor := crIBeam;
      SetCapture(FEdit.Handle);
    end;
  end;
end;

procedure TJvLookOutPage.EditKeyDown(Sender: TObject; var Key: Char);
var aCaption: string; Modify: boolean;
begin
  Modify := False;
  if (Sender = FEdit) then
    case Key of
      #13:
        begin
          Key := #0;
          aCaption := FEdit.Text;
          DoOnEdited(aCaption);
          FEdit.Text := aCaption;
          Modify := True;
          if FEdit.Handle = GetCapture then
            ReleaseCapture;
          FEdit.Hide;
          FEdit.Free;
          FEdit := nil;
          Screen.Cursor := crDefault;
        end;
      #27:
        begin
          Key := #0;
          if FEdit.Handle = GetCapture then
            ReleaseCapture;
          FEdit.Hide;
          FEdit.Free;
          FEdit := nil;
          Screen.Cursor := crDefault;
        end;
    end; { case }
  if Modify then
    FCaption := aCaption;
end;

procedure TJvLookOutPage.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, FCaption) and Enabled then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;

procedure TJvLookOutPage.SetActiveButton(Value: TJvCustomLookOutButton);
begin
  if (Value <> nil) and (FActiveButton <> Value) and (Value.Parent = self) then
    FActiveButton := Value;
end;

procedure TJvLookOutPage.SetParent(AParent: TWinControl);
begin
  if (AParent <> Parent) then
  begin
    if (Parent <> nil) and (Parent is TJvLookOut) then
      TJvLookOut(Parent).FPages.Delete(TJvLookOut(Parent).FPages.IndexOf(self));
    if (AParent <> nil) and (AParent is TJvLookOut) then
      TJvLookOut(AParent).FPages.Add(self);
  end;
  inherited SetParent(AParent);
end;

procedure TJvLookOutPage.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (AComponent = FPopUpMenu) then
      FPopUpMenu := nil;
  end;
  if (Operation = opInsert) then
  begin
    if not (csDesigning in ComponentState) then
      if Assigned(FUpArrow) and Assigned(FDwnArrow) then
      begin
        FUpArrow.SetZOrder(True);
        FDwnArrow.SetZOrder(True);
      end;
  end;
end;

procedure TJvLookOutPage.AlignControls(Control: TControl; var Rect: TRect);
begin
  inherited AlignControls(Control, Rect);
end;

procedure TJvLookOutPage.SmoothScroll(aControl: TControl; NewTop, Intervall: integer; Smooth: boolean);
begin
  if Smooth and not (csDesigning in ComponentState) and not (csLoading in ComponentState) and not FInScroll then
  begin
    FInScroll := true;
    if (aControl.Top < NewTop) then
      if (aControl.Top > 0) then
      begin
        while aControl.Top < NewTop do
        begin
          aControl.Top := aControl.Top + Intervall;
          Application.ProcessMessages;
        end;
      end
      else
      begin
        while aControl.Top < NewTop do
        begin
          aControl.Top := aControl.Top - Intervall;
          Application.ProcessMessages;
        end;
      end
    else if (aControl.Top > 0) then
    begin
      while aControl.Top > NewTop do
      begin
        aControl.Top := aControl.Top - Intervall;
        Application.ProcessMessages;
      end;
    end
    else
    begin
      while aControl.Top > NewTop do
      begin
        aControl.Top := aControl.Top + Intervall;
        Application.ProcessMessages;
      end;
    end;
  end;
  { adjust }
  aControl.Top := NewTop;
  Application.ProcessMessages;
  FInScroll := false;
end;

function Compare(Item1, Item2: Pointer): integer;
begin
  Result := TControl(Item1).Top - TControl(Item2).Top;
end;

procedure TJvLookOutPage.ScrollChildren(Start: word);
var R: TRect; i, x, aCount: integer; {AList:TList;} aControl: TControl;
begin
  if FScrolling <> 0 then
    Exit;
  if (csReading in ComponentState) or (csLoading in ComponentState) or (csWriting in ComponentState) or
    (csDestroying in ComponentState) then
    Exit;
  { draw all owned controls }
  if (ControlCount < 3) then
  begin
    if Assigned(FUpArrow) and Assigned(FDwnArrow) then
    begin
      FUpArrow.Visible := False;
      FDwnArrow.Visible := False;
    end;
    Exit;
  end;
  if FInScroll then
    Exit;
  R := GetClientRect;
  x := Width;
  aCount := GetButtonCount;
  if aCount = 0 then
    Exit;
  FButtons.Sort(Compare);
  FInScroll := true;
  for i := 0 to aCount - 1 do
  begin
    aControl := FButtons[i];
    if not aControl.Visible then
      Continue;
    if aControl.Align <> alNone then
      aControl.Align := alNone;

    if (i < FTopControl) then
      aControl.Top := -(aControl.Height + 1) * (aCount - i)
    else if (Start > Height) then
      aControl.Top := (Height + 1) * (i + 1)
    else
    begin
      aControl.Top := Start + FMargin;
      Inc(Start, (aControl.Height + FMargin));
    end;

    if FAutoCenter then
      aControl.Left := (x - aControl.Width) div 2;
  end;
  FInScroll := false;
end;

procedure TJvLookOutPage.CreateWnd;
var R: TRect;
begin
  inherited CreateWnd;
  R := GetClientRect;
  if not Assigned(FUpArrow) then
  begin
    FUpArrow := TJvUpArrowBtn.Create(nil);
    FUpArrow.Parent := self;
  end;

  if not Assigned(FDwnArrow) then
  begin
    FDwnArrow := TJvDwnArrowBtn.Create(nil);
    FDwnArrow.Parent := self;
  end;

  with FUpArrow do
  begin
    Visible := False;
    SetBounds(R.Right - 23, R.Top + 25, 16, 16);
    SetZorder(True);
  end;

  with FDwnArrow do
  begin
    Visible := False;
    SetBounds(R.Right - 23, R.Bottom - 23, 16, 16);
    SetZorder(True);
  end;

  if Assigned(Parent) and (Parent is TJvLookOut) then
  begin
    FManager := TJvLookOut(Parent);
    FOnCollapse := FManager.FOnCollapse;
  end;

end;

procedure TJvLookOutPage.Click;
begin
  if not Enabled then
    Exit;
  if Assigned(FOnCollapse) then
    FOnCollapse(Self);
  inherited Click;
end;

procedure TJvLookOutPage.CMEnabledChanged(var Message: TMessage);
begin
  if not (Assigned(FUpArrow) or Assigned(FDwnArrow)) then
    Exit;
  if not (Enabled) then
  begin
    FUpArrow.Enabled := False;
    FDwnArrow.Enabled := False;
  end
  else
  begin
    FUpArrow.Enabled := True;
    FDwnArrow.Enabled := True;
  end;
  inherited;
  Refresh;
end;

function TJvLookOutPage.IsVisible(Control: TControl): boolean;
var R: TRect;
begin
  Result := False;
  if (Control = nil) then
    Exit;
  R := GetClientRect;
  Result := (PtInRect(R, Point(R.Left + 1, Control.Top))
    and PtInRect(R, Point(R.Left + 1, Control.Top + Control.Height)));
end;

procedure TJvLookOutPage.SetAutoRepeat(Value: boolean);
begin
  if FAutoRepeat <> Value then
  begin
    FAutoRepeat := Value;
    if Assigned(FUpArrow) and Assigned(FDwnArrow) then
    begin
      FUpArrow.AutoRepeat := FAutoRepeat;
      FDwnArrow.AutoRepeat := FAutoRepeat;
    end;
  end;
end;

procedure TJvLookOutPage.SetHiFont(Value: TFont);
begin
  FHiFont.Assign(Value);
  if FHiFont <> Font then
    DrawTopButton;
end;

procedure TJvLookOutPage.SetButton(Index: integer; Value: TJvLookOutButton);
begin
  FButtons[Index] := Value;
end;

function TJvLookOutPage.GetButton(Index: integer): TJvLookOutButton;
begin
  Result := TJvLookOutButton(FButtons[Index]);
end;

function TJvLookOutPage.GetButtonCount: integer;
begin
  Result := FButtons.Count;
end;

procedure TJvLookOutPage.SetAutoCenter(Value: boolean);
begin
  if FAutoCenter <> Value then
  begin
    FAutoCenter := Value;
    if FAutoCenter then
      ScrollChildren(cHeight + 7 - FMargin);
  end;
end;

procedure TJvLookOutPage.SetMargin(Value: integer);
begin
  if FMargin <> Value then
  begin
    FMargin := Value;
    Repaint;
  end;
end;

procedure TJvLookOutPage.SeTJvImageSize(Value: TJvImageSize);
var Message: TMessage;
begin
  if FImageSize <> Value then
  begin
    FImageSize := Value;
    if csDesigning in ComponentState then
      SetPImSize(False);
    { notify children }
    Message.Msg := CM_IMAGESIZECHANGED;
    Message.WParam := Longint(Ord(FImageSize));
    Message.LParam := Longint(self);
    Message.Result := 0;
    if Parent <> nil then
      Parent.Broadcast(Message);
    Broadcast(Message);
  end;
end;

procedure TJvLookOutPage.SetPImSize(Value: boolean);
begin
  FPImSize := Value;
  if FPImSize and (FManager <> nil) then
    SeTJvImageSize(FManager.ImageSize);
end;

procedure TJvLookOutPage.CMParenTJvImageSizeChanged(var Message: TMessage);
var FTmp: boolean;
begin
  if (Message.LParam <> Longint(self)) and FPImSize then
  begin
    FTmp := FPImSize;
    SeTJvImageSize(TJvImageSize(Message.WParam));
    FPImSize := FTmp;
  end;
end;

procedure TJvLookOutPage.SetBitmap(Value: TBitmap);
begin
  FBitmap.Assign(Value);
  if FBitmap.Empty then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
  //  RecreateWnd;
  Invalidate;
end;

procedure TJvLookOutPage.SetCaption(Value: TCaption);
begin
  FCaption := Value;
  Invalidate;
end;

{ determine if arrows should be visible }

procedure TJvLookOutPage.CalcArrows;
var i: integer; R: TRect; AList: TList;
begin
  if Assigned(FUpArrow) and Assigned(FDwnArrow) then
  begin
    if Height < 65 then
    begin
      //      FUpArrow.Visible := False;
      //      FDwnArrow.Visible := False;
      FDwnArrow.Top := FUpArrow.Top + 16;
      Exit;
    end;

    R := GetClientRect;
    FUpArrow.SetBounds(R.Right - 23, R.Top + 25, 16, 16);
    FDwnArrow.SetBounds(R.Right - 23, R.Bottom - 23, 16, 16);
    AList := TList.Create;
    try
      for i := 0 to ControlCount - 1 do
      begin
        if (Controls[i] = FUpArrow) or (Controls[i] = FDwnArrow) or (Controls[i] = FEdit) then
          Continue;

        if not Controls[i].Visible and not (csDesigning in ComponentState) then
          Continue;
        AList.Insert(AList.Count, Controls[i]);
      end;

      if AList.Count = 0 then
        Exit;
      AList.Sort(Compare);
      FDwnArrow.Visible := not IsVisible(AList.Items[AList.Count - 1]);
      FUpArrow.Visible := not IsVisible(AList.Items[0]);
    finally
      AList.Free;
    end;
  end;
end;

procedure TJvLookOutPage.UpArrowClick(Sender: TObject);
begin
  if (FScrolling = 0) and (FTopControl > 0) then
    Dec(FTopControl);
end;

procedure TJvLookOutPage.DownArrowClick(Sender: TObject);
begin
  if (FScrolling = 0) and (FTopControl < ControlCount - 3) then
    Inc(FTopControl);
end;

procedure TJvLookOutPage.Paint;
begin
  inherited Paint;
  if not FBitmap.Empty then
  begin
    ControlStyle := ControlStyle + [csOpaque];
    TileBitmap;
  end
  else
    ControlStyle := ControlStyle - [csOpaque];

  DrawTopButton;
  CalcArrows;
  ScrollChildren(cHeight + 7 - FMargin);
end;

procedure TJvLookOutPage.DrawTopButton;
var R, R2: TRect; DC: hDC; FFlat, FPush: boolean;
begin
  if FInsideButton then
    Canvas.Font := FHiFont
  else
    Canvas.Font := self.Font;

  Canvas.Brush.Color := clBtnFace;
  DC := Canvas.Handle;
  R := GetClientRect;

  { draw top button }
  R.Bottom := cHeight;
  Canvas.FillRect(R);
  FPush := FShowPressed and FDown;
  FFlat := Assigned(FManager) and (FManager.FFlatButtons);
  if FFlat then
  begin
    if FManager.ActivePage = self then
    begin
      R2 := GetClientRect;
      R2.Top := R.Bottom;
      Frame3d(Canvas, R2, cl3DDkShadow, clBtnFace, 1);
    end;

    if FPush then
      Frame3D(Canvas, R, clBtnShadow, clBtnHighlight, 1)
    else if FInsideButton then
    begin
      Frame3D(Canvas, R, clBtnHighLight, cl3DDkShadow, 1);
      Frame3D(Canvas, R, clBtnFace, clBtnShadow, 1);
    end
    else
      Frame3D(Canvas, R, clBtnHighlight, clBtnShadow, 1)
  end
  else
  begin
    if FPush then
    begin
      Frame3D(Canvas, R, cl3DDkShadow, clBtnHighlight, 1);
      Frame3D(Canvas, R, clBtnShadow, clBtnFace, 1);
    end
    else
    begin
      Frame3D(Canvas, R, clBtnHighlight, cl3DDkShadow, 1);
      Frame3D(Canvas, R, clBtnFace, clBtnShadow, 1);
    end;
  end;

  { draw top caption }
  R := GetClientRect;
  R.Bottom := cHeight;
  SetBkMode(DC, Windows.Transparent);
  if FCaption <> '' then
  begin
    if not Enabled then
    begin
      { draw disabled text }
      SetTextColor(DC, ColorToRGB(clBtnHighLight));
      OffsetRect(R, 1, 1);
      DrawText(DC, PChar(FCaption), Length(FCaption), R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
      OffsetRect(R, -1, -1);
      SetTextColor(DC, ColorToRGB(clBtnShadow));
    end
    else
      SetTextColor(DC, ColorToRGB(Canvas.Font.Color));
    if FShowPressed and FDown then
      OffsetRect(R, 1, 1);
    DrawText(DC, PChar(FCaption), Length(FCaption), R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;
end;

procedure TJvLookOutPage.TileBitmap;
var
  X, Y, W, H: LongInt;
  Dest, Source: TRect;
  Tmp: TBitmap;
begin
  if not FBitmap.Empty then
  begin
    with FBitmap do
    begin
      W := Width;
      H := Height;
    end;

    Tmp := TBitmap.Create;
    Tmp.Width := Width;
    Tmp.Height := Height;

    Y := 0;
    Source := Rect(0, 0, W, H);
    while y < Height do
    begin
      X := 0;
      while X < Width do
      begin
        Dest := Rect(X, Y, X + W, Y + H);
        Tmp.Canvas.CopyRect(Dest, FBitmap.Canvas, Source);
        Inc(X, W);
      end;
      Inc(Y, H);
    end;
    Canvas.Draw(0, 0, Tmp);
    Tmp.Free;
  end;
end;

procedure TJvLookOutPage.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var R: TRect; tmp: TPoint; Msg: TMsg;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Assigned(FPopUpMenu) and (Button = mbRight) then
  begin
    { calc where to put menu }
    tmp := ClientToScreen(Point(X, Y));
    FPopUpMenu.PopupComponent := self;
    FPopUpMenu.Popup(tmp.X, tmp.Y);
    { wait 'til menu is done }
    while PeekMessage(Msg, 0, WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE) do
      ;
    FDown := False;
  end
  else
  begin
    R := GetClientRect;
    R.Bottom := cHeight;
    if PtInRect(R, Point(X, Y)) and (Button = mbLeft) then
    begin
      FDown := True;
      DrawTopButton;
    end;
  end;
end;

procedure TJvLookOutPage.MouseMove(Shift: TShiftState; X, Y: Integer);
var R: TRect;
begin
  R := GetClientRect;
  R.Bottom := cHeight;
  if PtInRect(R, Point(X, Y)) then
  begin
    if not FInsideButton then
    begin
      FInsideButton := True;
      DrawTopButton;
    end
  end
  else if FInsideButton or FDown then
  begin
    FInsideButton := False;
    //    FDown := False;
    DrawTopButton;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TJvLookOutPage.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var R: TRect;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if not Enabled then
    Exit;
  FDown := False;
  R := GetClientRect;
  R.Bottom := cHeight;
  if PtInRect(R, Point(X, Y)) and (Button = mbLeft) then
  begin
    if Assigned(FOnCollapse) then
      FOnCollapse(self);
    if Assigned(FOnClick) then
      FOnClick(self);
  end;
  DrawTopButton;
end;

procedure TJvLookOutPage.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FInsideButton then
  begin
    FInsideButton := False;
    //    FDown := False;
    DrawTopButton;
  end;
end;

procedure TJvLookOutPage.WMEraseBkgnd(var M: TWMEraseBkgnd);
begin
  inherited;
end;

{ TJvLookOut}

procedure TJvLookOut.SetFlatButtons(Value: boolean);
begin
  if FFlatButtons <> Value then
  begin
    FFlatButtons := Value;
    //    for i := 0 to PageCount - 1 do
    //      Pages[i].DrawTopButton;
    RecreateWnd;
  end;
end;

constructor TJvLookOut.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csOpaque];
  FPages := TList.Create;
  Width := 92;
  Height := 300;
  FBorderStyle := bsSingle;
  FAutoSize := False;
  FScroll := False;
  FFlatButtons := false;
  Color := clBtnFace;
  FOnCollapse := DoCollapse;
  FImageSize := isLarge;
end;

destructor TJvLookOut.Destroy;
begin
  FPages.Free;
  inherited Destroy;
end;

function TJvLookOut.AddPage: TJvLookOutPage;
begin
  Result := TJvLookOutPage.Create(self.Owner);
  Result.Parent := self;
end;

procedure TJvLookOut.Notification(AComponent: TComponent; Operation: TOperation);
var i: integer;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (AComponent = FActivePage) then
      FActivePage := nil;
    if (AComponent) = FCurrentPage then
      FCurrentPage := nil;
    if (AComponent is TJvLookOutPage) and (TJvLookOutPage(AComponent).Parent = self) then
    begin
      i := FPages.IndexOf(AComponent);
      if i > -1 then
        FPages.Delete(i);
    end;
  end
  else {// insertion} if (AComponent is TJvLookOutPage) and (TJvLookOutPage(AComponent).Parent = self) then
    begin
      if FPages.IndexOf(AComponent) = -1 then
        FPages.Add(AComponent);
    end;

  if Canvas <> nil then
    Invalidate;
end;

procedure TJvLookOut.UpdateControls;
begin
  if (FCurrentPage <> nil) then
    DoCollapse(FCurrentPage)
  else if (FActivePage <> nil) then
    DoCollapse(FActivePage)
  else if (ControlCount > 0) and (Controls[0] is TJvLookOutPage) then
    DoCollapse(Controls[0]);
end;

procedure TJvLookOut.SetAutoSize(Value: boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if FAutoSize then
      UpdateControls;
  end;
end;

procedure TJvLookOut.SeTJvImageSize(Value: TJvImageSize);
var Message: TMessage;
begin
  if FImageSize <> Value then
  begin
    FImageSize := Value;
    { notify children }
    Message.Msg := CM_IMAGESIZECHANGED;
    Message.WParam := Longint(Ord(FImageSize));
    Message.LParam := Longint(self);
    Message.Result := 0;
    Broadcast(Message);
  end;
end;

procedure TJvLookOut.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

{ calculate which TJvLookOutPage should be visible and which should not }

procedure TJvLookOut.DoCollapse(Sender: TObject);
var C: TControl;
  done: boolean;
  vis, i, ht, ofs, bh, cc, flt: integer;
begin
  if Sender is TJvLookOutPage then
  begin
    FCurrentPage := TJvLookOutPage(Sender);
    FActivePage := FCurrentPage;
    FCurrentPage.DrawTopButton;
  end;

  if Assigned(FOnClick) then
    FOnClick(Sender);

  cc := ControlCount - 1;
  done := false;
  ht := Height;
  vis := 0;
  ofs := 0;

  { make sure non-visible pages don't mess up the display }
  for i := 0 to cc do
    if Controls[i].Visible then
      Inc(vis);
  if Height <= (cHeight * vis) + 65 then
    Exit;
  if FFlatButtons then
    flt := 2
  else
    flt := 4;

  for i := 0 to cc do
  begin
    C := Controls[i];

    if not C.Visible then
    begin
      Inc(ofs);
      Continue;
    end;

    C.Align := alNone;
    bh := cHeight + 1;

    if FAutoSize then
      C.SetBounds(0, C.Top, Width - flt, C.Height);

    C.Height := ht - (vis - 1) * bh;

    if (C = Sender) then
      done := true;

    if (C = Sender) or (i = 0) then { first or caller }
      SmoothScroll(C, (i - ofs) * bh, cSpeed, FScroll)
    else if done and (C <> Sender) then { place at bottom }
      SmoothScroll(C, ht - (vis - i + ofs) * bh - flt + 1, cSpeed, FScroll)
    else { place at top }
      SmoothScroll(C, (i - ofs) * bh, cSpeed, FScroll);
  end;
end;

procedure TJvLookOut.SmoothScroll(aControl: TControl; NewTop, Intervall: integer; Smooth: boolean);
begin
  if Smooth and not (csDesigning in ComponentState) then
  begin
    if aControl.Top < NewTop then
      while aControl.Top < NewTop do
      begin
        aControl.Top := aControl.Top + Intervall;
        Application.ProcessMessages;
      end
    else
      while aControl.Top > NewTop do
      begin
        aControl.Top := aControl.Top - Intervall;
        Application.ProcessMessages;
      end;
  end;
  { adjust }
  aControl.Top := NewTop;
  Application.ProcessMessages;
end;

procedure TJvLookOut.SetActiveOutLook(Value: TJvLookOutPage);
var i: integer;
begin
  if (Value <> nil) and (Value.Parent = self) and (Value.Visible) then
    DoCollapse(Value)
  else if (PageCount > 0) then
    for i := 0 to PageCount - 1 do
      if Pages[i].Visible then
        DoCollapse(Pages[i])
      else
        FActivePage := nil;
end;

function TJvLookOut.GetActiveOutlook: TJvLookOutPage;
begin
  if csDesigning in ComponentState then
    Result := FActivePage
  else
    Result := FCurrentPage;
end;

function TJvLookOut.GetPageCount: integer;
begin
  Result := FPages.Count;
end;

function TJvLookOut.GetPage(Index: integer): TJvLookOutPage;
begin
  Result := TJvLookOutPage(FPages[Index]);
end;

procedure TJvLookOut.SetPage(Index: integer; Value: TJvLookOutPage);
begin
  FPages[Index] := Value;
end;

procedure TJvLookOut.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  with Message.CalcSize_Params^ do
    if FFlatButtons then
      InflateRect(rgrc[0], -1, -1)
    else
      InflateRect(rgrc[0], -2, -2);
  inherited;
end;

procedure TJvLookOut.WMNCPaint(var Message: TMessage);
var
  DC: HDC;
  RC, RW: TRect;
begin
  DC := GetWindowDC(Handle);
  try
    Windows.GetClientRect(Handle, RC);
    GetWindowRect(Handle, RW);
    MapWindowPoints(0, Handle, RW, 2);
    OffsetRect(RC, -RW.Left, -RW.Top);
    ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
    OffsetRect(RW, -RW.Left, -RW.Top);
    if FBorderStyle = bsSingle then
      DrawEdge(DC, RW, EDGE_SUNKEN, BF_RECT)
    else
    begin
      Canvas.Brush.Color := Color;
      Windows.FrameRect(DC, RW, Canvas.Brush.Handle);
      InflateRect(RW, -1, -1);
      Windows.FrameRect(DC, RW, Canvas.Brush.Handle);
      InflateRect(RW, 1, 1);
    end;
    { Erase parts not drawn }
    IntersectClipRect(DC, RW.Left, RW.Top, RW.Right, RW.Bottom);
  finally
    ReleaseDC(Handle, DC);
  end;
end;

procedure TJvLookOut.Paint;
begin
  if not (Visible or (csDesigning in ComponentState)) then
    Exit;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(GetClientRect);
  { make TJvLookOuts adjust to Managers size }
  if (ControlCount > 0) and FAutoSize then
    UpdateControls;
end;

{ TJvExpress }

constructor TJvExpress.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ImageSize := isLarge;
  FBorderStyle := bsSingle;
  FTopControl := 0;
  FButtonHeight := 60;
end;

procedure TJvExpress.Paint;
begin
  if not FBitmap.Empty then
  begin
    ControlStyle := ControlStyle + [csOpaque];
    TileBitmap;
  end
  else
  begin
    ControlStyle := ControlStyle - [csOpaque];
    Canvas.Brush.Color := Color;
    Canvas.FillRect(GetClientRect);
  end;

  CalcArrows;
  ScrollChildren(0);
end;

function TJvExpress.AddButton: TJvExpressButton;
begin
  Result := TJvExpressButton.Create(Self.Owner);
  Result.Parent := self;
  Result.ImageIndex := ButtonCount;
  Result.Top := MaxInt;
  if Assigned(FUpArrow) and Assigned(FDwnArrow) then
  begin
    FUpArrow.SetZOrder(True);
    FDwnArrow.SetZOrder(True);
  end;
end;

{
procedure TJvExpress.SetButton(Index:integer;Value:TJvExpressButton);
begin
  inherited SetButton(Index,Value);
end;

function TJvExpress.GetButton(Index:integer):TJvExpressButton;
begin
  Result := TJvExpressButton(inherited GetButton(Index));
end;

function TJvExpress.GetButtonCount:integer;
begin
  inherited GetButtonCount;
end;
}

procedure TJvExpress.CalcArrows;
var i: integer; R: TRect; AList: TList;
begin
  if Assigned(FUpArrow) and Assigned(FDwnArrow) then
  begin
    if Height < 65 then
    begin
      //      FDwnArrow.Top := FUpArrow.Top + 16;
      Exit;
    end;

    R := GetClientRect;
    AList := TList.Create;
    try
      for i := 0 to ControlCount - 1 do
      begin
        if (Controls[i] = FUpArrow) or (Controls[i] = FDwnArrow) or (Controls[i] = FEdit) then
          Continue;

        if not (Controls[i].Visible or (csDesigning in ComponentState)) then
          Continue;
        AList.Insert(AList.Count, Controls[i]);
      end;

      if AList.Count = 0 then
        Exit;
      AList.Sort(Compare);
      FDwnArrow.Visible := not IsVisible(AList.Items[AList.Count - 1]);
      FUpArrow.Visible := not IsVisible(AList.Items[0]);
    finally
      AList.Free;
    end;
  end;
end;

procedure TJvExpress.ScrollChildren(Start: word);
var i: integer;
begin
  { size all children to width of TJvExpress }
  for i := 0 to ControlCount - 1 do
    if (Controls[i] = FDwnArrow) or (Controls[i] = FUpArrow) or (Controls[i] is TJvLookOutEdit) then
      Continue
    else
      Controls[i].SetBounds(0, Controls[i].Top, Width - 4, FButtonHeight);

  if Assigned(FUpArrow) then
    Start := 12 * Ord(FUpArrow.Visible)
  else
    Start := 0;
  inherited ScrollChildren(Start);
end;

procedure TJvExpress.DrawTopButton;
begin
  { do nothing }
end;

procedure TJvExpress.SetButtonHeight(Value: integer);
var i: integer;
begin
  if FButtonHeight <> Value then
  begin
    FButtonHeight := Value;
    for i := 0 to ButtonCount - 1 do // Iterate
      Buttons[i].Height := FButtonHeight;
  end;
end;

procedure TJvExpress.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  with Message.CalcSize_Params^ do
    InflateRect(rgrc[0], -2, -2);
  inherited;
end;

procedure TJvExpress.CreateWnd;
begin
  inherited CreateWnd;
  if not Assigned(FUpArrow) then
    FUpArrow := TJvUpArrowBtn.Create(nil);

  if not Assigned(FDwnArrow) then
    FDwnArrow := TJvDwnArrowBtn.Create(nil);
  with FUpArrow do
  begin
    Parent := self;
    Flat := True;
    Height := 13;
    Align := alTop;
    SetZOrder(True);
  end;

  with FDwnArrow do
  begin
    Parent := self;
    Flat := True;
    Height := 13;
    Align := alBottom;
    SetZOrder(True);
  end;
end;

procedure TJvExpress.WMNCPaint(var Message: TMessage);
var
  DC: HDC;
  RC, RW: TRect;
begin
  DC := GetWindowDC(Handle);
  try
    Windows.GetClientRect(Handle, RC);
    GetWindowRect(Handle, RW);
    MapWindowPoints(0, Handle, RW, 2);
    OffsetRect(RC, -RW.Left, -RW.Top);
    ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
    OffsetRect(RW, -RW.Left, -RW.Top);
    if FBorderStyle = bsSingle then
      DrawEdge(DC, RW, EDGE_SUNKEN, BF_RECT)
    else
    begin
      if csDesigning in ComponentState then
        Canvas.Brush.Color := clBlack
      else
        Canvas.Brush.Color := Color;
      Windows.FrameRect(DC, RW, Canvas.Brush.Handle);
      InflateRect(RW, -1, -1);
      if csDesigning in ComponentState then
        Canvas.Brush.Color := Color;
      Windows.FrameRect(DC, RW, Canvas.Brush.Handle);
      InflateRect(RW, 1, 1);
    end;
    { Erase parts not drawn }
    IntersectClipRect(DC, RW.Left, RW.Top, RW.Right, RW.Bottom);
  finally
    ReleaseDC(Handle, DC);
  end;
end;

end.

