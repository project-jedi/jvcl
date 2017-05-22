{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvLookOut.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Th�rnqvist [peter3 at sourceforge dot net]
Portions created by Peter Th�rnqvist are Copyright (C) 2002 Peter Th�rnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvLookOut;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes,
  Windows, Messages,
  Graphics, Controls, Forms, StdCtrls, ExtCtrls, Buttons, Menus, ImgList,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  JvJCLUtils, JvTypes, JvConsts, JvComponent, JvThemes, JvExControls, JvExButtons;

type
  TJvButtonBorder = (bbDark, bbLight, bbMono);

  TJvUpArrowBtn = class(TJvExSpeedButton, IJvDenySubClassing)
  private
    FTimer: TTimer;
    FAutoRepeat: Boolean;
    FDown: Boolean;
    FFlat: Boolean;
    procedure SetFlat(Value: Boolean);
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
  protected
    procedure OnTime(Sender: TObject); virtual;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
  public
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
  published
    property Flat: Boolean read FFlat write SetFlat default False;
    property AutoRepeat: Boolean read FAutoRepeat write FAutoRepeat default True;
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
    FParentImageSize: Boolean;
    FDown: Boolean;
    FStayDown: Boolean;
    FCentered: Boolean;
    FImageIndex: TImageIndex;
    FSpacing: Integer;
    FOffset: Integer;
    FImageSize: TJvImageSize;
    FImageRect: TRect;
    FTextRect: TRect;
    FFillColor: TColor;
    FHighlightFont: TFont;
    FButtonBorder: TJvButtonBorder;
    FPopupMenu: TPopupMenu;
    FGroupIndex: Integer;
    FSmallImages: TCustomImageList;
    FLargeImages: TCustomImageList;
    FOnEdited: TJvLookOutEditedEvent;
    FLargeImageChangeLink: TChangeLink;
    FSmallImageChangeLink: TChangeLink;
    procedure SetGroupIndex(Value: Integer);
    procedure UpdateExclusive;
    procedure SetCentered(Value: Boolean);
    procedure SetDown(Value: Boolean);
    procedure SetOffset(Value: Integer);
    procedure SetFillColor(Value: TColor);
    procedure SetHighlightFont(Value: TFont);
    procedure SetSpacing(Value: Integer);
    procedure SetParentImageSize(Value: Boolean);
    procedure SetButtonBorder(Value: TJvButtonBorder);
    procedure SetSmallImages(const Value: TCustomImageList);
    procedure SetLargeImages(const Value: TCustomImageList);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetImageSize(Value: TJvImageSize);
    procedure DrawSmallImages;
    procedure DrawLargeImages;
    procedure ImageListChange(Sender: TObject);
    procedure CMButtonPressed(var Msg: TCMButtonPressed); message CM_JVBUTTONPRESSED;
    procedure CMParentImageSizeChanged(var Msg: TMessage); message CM_IMAGESIZECHANGED;
    procedure CMLeaveButton(var Msg: TMessage); message CM_LEAVEBUTTON;
    procedure CMTextChanged(var Msg:TMessage); message CM_TEXTCHANGED;
    function ParentVisible: Boolean;
    procedure SetPopupMenu(const Value: TPopupMenu);
  protected

    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoOnEdited(var Caption: string); virtual;
    procedure EditKeyDown(Sender: TObject; var Key: Char);
    procedure EditMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure PaintFrame; virtual;
    procedure SetParent( AParent: TWinControl); override;
    procedure Paint; override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function WantKey(Key: Integer; Shift: TShiftState): Boolean; override;
    procedure VisibleChanged; override;

    property FillColor: TColor read FFillColor write SetFillColor default clNone;
    property Offset: Integer read FOffset write SetOffset default 0;
    property ButtonBorder: TJvButtonBorder read FButtonBorder write SetButtonBorder default bbDark;
    property Centered: Boolean read FCentered write SetCentered;
    property Down: Boolean read FStayDown write SetDown default False;
    // (rom) renamed
    property HighlightFont: TFont read FHighlightFont write SetHighlightFont;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property ImageSize: TJvImageSize read FImageSize write SetImageSize default isLarge;
    property ParentImageSize: Boolean read FParentImageSize write SetParentImageSize default True;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property LargeImages: TCustomImageList read FLargeImages write SetLargeImages;
    property Spacing: Integer read FSpacing write SetSpacing default 4; { border offset from bitmap }
    property SmallImages: TCustomImageList read FSmallImages write SetSmallImages;
    property Data: Pointer read FData write FData;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property OnEdited: TJvLookOutEditedEvent read FOnEdited write FOnEdited;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure Assign(Source: TPersistent); override;
    procedure EditCaption;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvLookOutButton = class(TJvCustomLookOutButton)
  public
    property Data;
  published
    property Action;
    property Align;
    property Anchors;
    property ButtonBorder;
    property Caption;
    property Constraints;
    property Down;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property GroupIndex;
    property Height default 60;
    property HighlightFont;
    property ImageIndex;
    property ImageSize;
    property LargeImages;
    property Left;
    property ParentFont;
    property ParentImageSize;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property SmallImages;
    property Spacing;
    property Top;
    property Visible;
    property Width default 60;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvExpressButton = class(TJvCustomLookOutButton)
  public
    constructor Create(AOwner: TComponent); override;
    property Data;
  published
    property Action;
    property Align;
    property Anchors;
    property ButtonBorder default bbLight;
    property Caption;
    property Constraints;
    property Down;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FillColor default clBtnFace;
    property Font;
    property GroupIndex;
    property Height default 60;
    property HighlightFont;
    property ImageIndex;
    property ImageSize;
    property LargeImages;
    property Left;
    property Offset default 1;
    property ParentFont default False;
    property ParentImageSize;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property SmallImages;
    property Spacing;
    property Top;
    property Visible;
    property Width default 60;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  TJvLookOut = class;
  TJvLookOutPage = class(TJvCustomControl)
  private
    FEdit: TEdit;
    FInScroll: Boolean;
    FAutoRepeat: Boolean;
    FAutoCenter: Boolean;
    FParentImageSize: Boolean;
    FDown: Boolean;
    FShowPressed: Boolean;
    FMargin: Integer;
    FTopControl: Integer;
    FPopUpMenu: TPopupMenu;
    FOnClick: TNotifyEvent;
    FDownArrow: TJvDwnArrowBtn;
    FScrolling: Integer;
    FUpArrow: TJvUpArrowBtn;
    FBitmap: TBitmap;
    FImageSize: TJvImageSize;
    FManager: TJvLookOut;
    FOnCollapse: TNotifyEvent;
    FHighlightFont: TFont;
    FButtons: TList;
    FActiveButton: TJvCustomLookOutButton;
    FOnEdited: TJvLookOutEditedEvent;
    procedure SetActiveButton(Value: TJvCustomLookOutButton);
    procedure EditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditKeyDown(Sender: TObject; var Key: Char);
    procedure SetAutoRepeat(Value: Boolean);
    procedure SetHighlightFont(Value: TFont);
    procedure SetImageSize(Value: TJvImageSize);
    procedure SetParentImageSize(Value: Boolean);
    procedure SetBitmap(Value: TBitmap);
    procedure SetMargin(Value: Integer);
    procedure SetButton(Index: Integer; Value: TJvCustomLookOutButton);
    function GetButton(Index: Integer): TJvCustomLookOutButton;
    function GetButtonCount: Integer;
    procedure SetAutoCenter(Value: Boolean);
    function IsVisible(Control: TControl): Boolean;
    procedure CMParentImageSizeChanged(var Msg: TMessage); message CM_IMAGESIZECHANGED;
    procedure CMTextChanged(var Msg:TMessage); message CM_TEXTCHANGED;
    procedure TileBitmap;
  protected
    function WantKey(Key: Integer; Shift: TShiftState): Boolean; override;
    procedure MouseLeave(Control: TControl); override;
    procedure EnabledChanged; override;
    procedure DoOnEdited(var Caption: string); virtual;
    procedure UpArrowClick(Sender: TObject); virtual;
    procedure DownArrowClick(Sender: TObject); virtual;
    procedure DrawTopButton; virtual;
    procedure CalcArrows; virtual;
    procedure ScrollChildren(Start: Word); virtual;
    procedure AlignControls(Control: TControl; var Rect: TRect); override;
    procedure SetParent( AParent: TWinControl); override;
    procedure CreateWnd; override;
    procedure SmoothScroll(AControl: TControl; NewTop, AInterval: Integer; Smooth: Boolean); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    property AutoCenter: Boolean read FAutoCenter write SetAutoCenter;
  public
    procedure Click; override;
    procedure DownArrow;
    procedure UpArrow;
    function AddButton: TJvCustomLookOutButton;
    function InsertButton(Index: integer): TJvCustomLookOutButton;
    procedure ExchangeButtons(Button1, Button2: TJvCustomLookOutButton); virtual;
    procedure MoveButton(Button: TJvCustomLookOutButton; NewIndex:integer);virtual;
    procedure EditCaption; virtual;
    procedure DisableAdjust;
    procedure EnableAdjust;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Buttons[Index: Integer]: TJvCustomLookOutButton read GetButton write SetButton;
    property ButtonCount: Integer read GetButtonCount;
    property ActiveButton: TJvCustomLookOutButton read FActiveButton write SetActiveButton;
  published
    property Align;
    property AutoRepeat: Boolean read FAutoRepeat write SetAutoRepeat default False;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property ImageSize: TJvImageSize read FImageSize write SetImageSize default isLarge;
    property HighlightFont: TFont read FHighlightFont write SetHighlightFont;
    property ParentImageSize: Boolean read FParentImageSize write SetParentImageSize default True;
    property ShowPressed: Boolean read FShowPressed write FShowPressed default False;
    property Caption;
    property Color;
    property DragCursor;
    property DragMode;
    property ShowHint;
    property Visible;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu: TPopupMenu read FPopUpMenu write FPopUpMenu;
    property Left;
    property Top;
    property Width;
    property Height;
    property Cursor;
    property Hint;
    property Margin: Integer read FMargin write SetMargin default 0;
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

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvLookOut = class(TJvCustomControl)
  private
    FAutoSize: Boolean;
    FSmooth: Boolean;
    FBorderStyle: TBorderStyle;
    FOnCollapse: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FActivePage: TJvLookOutPage;
    FCurrentPage: TJvLookOutPage;
    FPages: TList;
    FImageSize: TJvImageSize;
    FFlatButtons: Boolean;
    procedure SetImageSize(Value: TJvImageSize);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure UpdateControls;
    procedure DoCollapse(Sender: TObject);
    procedure SetActiveOutlook(Value: TJvLookOutPage);
    function GetActiveOutlook: TJvLookOutPage;
    function GetPageCount: Integer;
    function GetPage(Index: Integer): TJvLookOutPage;
    procedure SetPage(Index: Integer; Value: TJvLookOutPage);
    procedure SetFlatButtons(Value: Boolean);
    procedure WMNCCalcSize(var Msg: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Msg: TMessage); message WM_NCPAINT;
  protected
    procedure SetAutoSize(Value: Boolean);  override;
    procedure SmoothScroll(AControl: TControl; NewTop, AInterval: Integer; Smooth: Boolean); virtual;
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddPage: TJvLookOutPage;
    property Pages[Index: Integer]: TJvLookOutPage read GetPage write SetPage;
    property PageCount: Integer read GetPageCount;
  published
    property ActivePage: TJvLookOutPage read GetActiveOutlook write SetActiveOutlook;
    property Align;
    property Anchors;
    property Constraints;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Color default clBtnShadow;
    property FlatButtons: Boolean read FFlatButtons write SetFlatButtons default False;
    property DragCursor;
    property DragMode;
    property ImageSize: TJvImageSize read FImageSize write SetImageSize default isLarge;
    property ShowHint;
    property Smooth: Boolean read FSmooth write FSmooth default False;
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

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvExpress = class(TJvLookOutPage, IJvDenySubClassing)
  private
    FBorderStyle: TBorderStyle;
    FButtonHeight: Integer;
    procedure SetButtonHeight(Value: Integer);
  protected
    procedure CalcArrows; override;
    procedure ScrollChildren(Start: Word); override;
    procedure DrawTopButton; override;
    procedure Paint; override;
    procedure CreateWnd; override;
    procedure AlignControls(Control: TControl; var Rect: TRect); override;
    procedure WMNCCalcSize(var Msg: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Msg: TMessage); message WM_NCPAINT;
  public
    constructor Create(AOwner: TComponent); override;
    function AddButton: TJvExpressButton;
  published
    property Anchors;
    property Constraints;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight default 60;
    property ImageSize default isLarge;
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
  {$IFDEF COMPILER10_UP}
  Types,
  {$ENDIF COMPILER10_UP}
  ActnList, JvJVCLUtils;

const
  cSpeed = 20;
  cHeight = 19;
  cInitTime = 400;
  cTimeDelay = 120;

  { utility }

  { this creates a correctly masked bitmap - for use with D2 TImageList }
  {
  procedure CreateMaskedImageList(ImageList: TImageList);
  var
    Bmp: TBitmap;
    I: Integer;
  begin
    Bmp := TBitmap.Create;
    Bmp.Width := ImageList.Width;
    Bmp.Height := ImageList.Height;
    try
      for I := 0 to ImageList.Count - 1 do
      begin
        ImageList.GetBitmap(I,Bmp);
        ImageList.ReplaceMasked(I,Bmp,Bmp.TransparentColor);
      end;
    finally
      Bmp.Free;
    end;
  end;
  }

  { returns number of visible children }
  {
  function NoOfVisibles(Control: TWinControl): Integer;
  var
    R: TRect;
    I: Integer;
  begin
    R := Control.ClientRect;
    Result := 0;
    if Control = nil then
      Exit;
    for I := 0 to Control.ControlCount - 1 do
       if (PtInRect(R,Point(R.Left + 1,Control.Controls[I].Top)) and
         PtInRect(R,Point(R.Left + 1,Control.Controls[I].Top + Control.Controls[I].Height)))  then
           Inc(Result);
  end;
  }

  {
  function IMax(Val1, Val2: Integer): Integer;
  begin
    Result := Val1;
    if Val2 > Val1 then
      Result := Val2;
  end;

  function IMin(Val1, Val2: Integer): Integer;
  begin
    Result := Val1;
    if Val2 < Val1 then
      Result := Val2;
  end;
  }

  { returns Atleast if Value < AtLeast, Val1 otherwise }
  {
  function IAtLeast(Value, AtLeast: Integer): Integer;
  begin
    Result := Value;
    if Value < AtLeast then
      Result := AtLeast;
  end;
  }

//=== { TJvLookOutEdit } =====================================================

type
  TJvLookOutEdit = class(TEdit)
  private
    procedure DoExit; override;
  end;

procedure TJvLookOutEdit.DoExit;
begin
  Visible := False;
  // (ahuser) What is with OnExit() ?
end;

//=== { TJvLookOutButtonActionLink } =========================================

type
  TJvLookOutButtonActionLink = class(TControlActionLink)
  protected
    FClient: TJvCustomLookOutButton;
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
    function IsImageIndexLinked: Boolean; override;
    procedure SetImageIndex(Value: Integer); override;
    function IsGroupIndexLinked: Boolean; override;
    procedure SetGroupIndex(Value: Integer); override;
  end;

  TJvLookOutButtonActionLinkClass = class of TJvLookOutButtonActionLink;

procedure TJvLookOutButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TJvCustomLookOutButton;
end;

function TJvLookOutButtonActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and
    (FClient.Down = (Action as TCustomAction).Checked);
end;

procedure TJvLookOutButtonActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then
    FClient.Down := Value;
end;

function TJvLookOutButtonActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (FClient.ImageIndex = (Action as TCustomAction).ImageIndex);
end;

procedure TJvLookOutButtonActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then
    FClient.ImageIndex := Value;

end;

function TJvLookOutButtonActionLink.IsGroupIndexLinked: Boolean;
begin
  Result := inherited IsGroupIndexLinked and
    (FClient.GroupIndex = (Action as TCustomAction).GroupIndex);
end;

procedure TJvLookOutButtonActionLink.SetGroupIndex(Value: Integer);
begin
  if IsGroupIndexLinked then
    FClient.GroupIndex := Value;
end;

//=== { TJvUpArrowBtn } ======================================================

constructor TJvUpArrowBtn.Create(AOwner: TComponent);
var
  FSize: Word;
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csSetCaption];
  ParentColor := True;
  FDown := False;
  FAutoRepeat := False;
  FFlat := False;
  FSize := GetSystemMetrics(SM_CXVSCROLL);
  SetBounds(0, 0, FSize, FSize);
end;

procedure TJvUpArrowBtn.SetFlat(Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TJvUpArrowBtn.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not MouseOver then
  begin
    inherited MouseEnter(Control);
    if FFlat {$IFDEF JVCLThemesEnabled} or ThemeServices.{$IFDEF RTL230_UP}Enabled{$ELSE}ThemesEnabled{$ENDIF RTL230_UP} {$ENDIF} then
      Invalidate;
  end;
end;

procedure TJvUpArrowBtn.MouseLeave(Control: TControl);
begin
  if MouseOver then
  begin
    inherited MouseLeave(Control);
    //  FDown := False;
    if FFlat {$IFDEF JVCLThemesEnabled} or ThemeServices.{$IFDEF RTL230_UP}Enabled{$ELSE}ThemesEnabled{$ENDIF RTL230_UP} {$ENDIF} then
      Invalidate;
  end;
end;


procedure TJvUpArrowBtn.CMDesignHitTest(var Msg: TCMDesignHitTest);
begin
  Msg.Result := 1;
end;


procedure TJvUpArrowBtn.Paint;
var
  Flags: Integer;
  R: TRect;
begin
  //  if not Visible then Exit;
  R := GetClientRect;

  if FDown then
    Flags := DFCS_PUSHED
  else
    Flags := 0;
  if not Enabled then
    Flags := Flags or DFCS_INACTIVE;

  if FFlat and not MouseOver then
  begin
    Flags := Flags or DFCS_FLAT;
    OffsetRect(R, 0, -2);
  end;

  if FFlat then
    InflateRect(R, 1, 1);
  if MouseOver then
    Flags := Flags or DFCS_HOT;
  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := Color;
  DrawThemedFrameControl(Canvas.Handle, R, DFC_SCROLL, DFCS_SCROLLUP or Flags);

  if FFlat and MouseOver then
  begin
    R := GetClientRect;

    if FDown then
      Frame3D(Canvas, R, clBlack, clWhite, 1)
    else
      Frame3D(Canvas, R, clWhite, clBlack, 1);
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
      FTimer := TTimer.Create(Self);
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
  (Parent as TJvLookOutPage).UpArrowClick(Self);
end;

procedure TJvUpArrowBtn.OnTime(Sender: TObject);
var
  R: TRect;
begin
  FTimer.Interval := cTimeDelay;
  if FDown and MouseCapture and Visible then
  begin
    (Parent as TJvLookOutPage).UpArrowClick(Self);
    R := Parent.ClientRect;
    R := Rect(R.Left, R.Top + cHeight, R.Right, R.Bottom);
    Windows.InvalidateRect(Parent.Handle, @R, False);
    Parent.Update;
  end;
end;

//=== { TJvDwnArrowBtn } =====================================================

constructor TJvDwnArrowBtn.Create(AOwner: TComponent);
var
  FSize: Word;
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csSetCaption];
  ParentColor := True;
  FDown := False;
  FFlat := False;
  FSize := GetSystemMetrics(SM_CXVSCROLL);
  SetBounds(0, 0, FSize, FSize);
end;

procedure TJvDwnArrowBtn.Paint;
var
  Flags: Integer;
  R: TRect;
begin
  //  if not Visible then Exit;
  R := GetClientRect;
  if FDown then
    Flags := DFCS_PUSHED
  else
    Flags := 0;
  if not Enabled then
    Flags := Flags or DFCS_INACTIVE;
  if FFlat and not MouseOver then
  begin
    Flags := Flags or DFCS_FLAT;
    OffsetRect(R, 0, 2);
  end;

  if FFlat then
    InflateRect(R, 1, 1);
  if MouseOver then
    Flags := Flags or DFCS_HOT;
  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := Color;
  DrawThemedFrameControl(Canvas.Handle, R, DFC_SCROLL, DFCS_SCROLLDOWN or Flags);

  if FFlat and MouseOver then
  begin
    R := GetClientRect;
    if FDown then
      Frame3D(Canvas, R, clBlack, clBtnShadow, 1)
    else
      Frame3D(Canvas, R, clWhite, clBlack, 1);
  end;
end;

procedure TJvDwnArrowBtn.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDown := True;
  //  inherited MouseDown(Button, Shift, X, Y);
  if Assigned(OnMouseDown) then
    OnMouseDown(Self, Button, Shift, X, Y);
  if Parent is TJvLookOutPage then
    FAutoRepeat := TJvLookOutPage(Parent).AutoRepeat;
  if FAutoRepeat then
  begin
    if not Assigned(FTimer) then
      FTimer := TTimer.Create(Self);
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
    OnMouseUp(Self, Button, Shift, X, Y);
  FDown := False;
  (Parent as TJvLookOutPage).DownArrowClick(Self);
  //  Parent.ScrollBy(0,-50);
  if Assigned(FTimer) then
  begin
    FTimer.Free;
    FTimer := nil;
  end;
  Repaint;
end;

procedure TJvDwnArrowBtn.OnTime(Sender: TObject);
var
  R: TRect;
begin
  FTimer.Interval := cTimeDelay;
  if FDown and MouseCapture then
  begin
    (Parent as TJvLookOutPage).DownArrowClick(Self);
    //    Parent.ScrollBy(0,-50);
    R := Parent.ClientRect;
    R := Rect(R.Left, R.Top + cHeight, R.Right, R.Bottom);
    Windows.InvalidateRect(Parent.Handle, @R, False);
    Parent.Update;
    if not Visible then
      FDown := False;
  end;
end;

//=== { TJvCustomLookOutButton } =============================================

constructor TJvCustomLookOutButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents];
  FButtonBorder := bbDark;
  FParentImageSize := True;
  FImageSize := isLarge;
  FFillColor := clNone;
  FSpacing := 4;
  FOffset := 0;
  FStayDown := False;
  FHighlightFont := TFont.Create;
  FHighlightFont.Assign(Font);
  Width := 60;
  Height := 60;
  FImageIndex := -1;
  FLargeImageChangeLink := TChangeLink.Create;
  FSmallImageChangeLink := TChangeLink.Create;
  FLargeImageChangeLink.OnChange := ImageListChange;
  FSmallImageChangeLink.OnChange := ImageListChange;
end;

destructor TJvCustomLookOutButton.Destroy;
begin
  FEdit.Free;
  FLargeImageChangeLink.Free;
  FSmallImageChangeLink.Free;
  FHighlightFont.Free;
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
    FEdit.Parent := Self.Parent;
    FEdit.Visible := False;
  end;

  FEdit.SetBounds(Left + FTextRect.Left, Top + FTextRect.Top,
    Width, FTextRect.Bottom - FTextRect.Top);
  with FEdit do
  begin
    Text := Caption;
    BorderStyle := bsNone;
    AutoSelect := True;
    OnKeyPress := EditKeyDown;
    OnMouseDown := EditMouseDown;
    if not Visible then
      Show;
    SetFocus;
    SetCapture(FEdit.Handle);
    SelStart := 0;
    SelLength := Length(Caption);
  end;
end;

procedure TJvCustomLookOutButton.DoOnEdited(var Caption: string);
begin
  if Assigned(FOnEdited) then
    FOnEdited(Self, Caption);
end;

procedure TJvCustomLookOutButton.EditKeyDown(Sender: TObject; var Key: Char);
var
  ACaption: string;
  Modify: Boolean;
begin
  Modify := False;
  if Sender = FEdit then
    case Key of
      Cr:
        begin
          ACaption := FEdit.Text;
          DoOnEdited(ACaption);
          FEdit.Text := ACaption;
          Key := #0;
          Modify := True;
          if FEdit.Handle = GetCapture then
            ReleaseCapture;
          FEdit.Hide;
          FEdit.Free;
          FEdit := nil;
          Screen.Cursor := crDefault;
        end;
      Esc:
        begin
          Key := #0;
          if FEdit.Handle = GetCapture then
            ReleaseCapture;
          FEdit.Hide;
          FEdit.Free;
          FEdit := nil;
          Screen.Cursor := crDefault;
        end;
    end;
  if Modify then
    Caption := ACaption;
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
    HighlightFont := TJvCustomLookOutButton(Source).HighlightFont;
    ParentImageSize := TJvCustomLookOutButton(Source).ParentImageSize;
    ImageSize := TJvCustomLookOutButton(Source).ImageSize;
    ImageIndex := TJvCustomLookOutButton(Source).ImageIndex;
    LargeImages := TJvCustomLookOutButton(Source).LargeImages;
    SmallImages := TJvCustomLookOutButton(Source).SmallImages;
    Spacing := TJvCustomLookOutButton(Source).Spacing;
  end
  else
    inherited Assign(Source);
end;

function TJvCustomLookOutButton.WantKey(Key: Integer; Shift: TShiftState): Boolean;
begin
  Result := IsAccel(Key, Caption) and Enabled and
    Visible and ParentVisible and (ssAlt in Shift);
  if Result then
    Click
  else
    Result := inherited WantKey(Key, Shift);
end;

function TJvCustomLookOutButton.ParentVisible: Boolean;
begin
  Result := False;
  if Parent = nil then
    Exit;
  if (Parent is TJvLookOutPage) and (Parent.Parent is TJvLookOut) then
    Result := TJvLookOutPage(Parent) = TJvLookOut(Parent.Parent).ActivePage
  else
    Result := Parent.Visible;
end;

procedure TJvCustomLookOutButton.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TJvCustomLookOutButton.UpdateExclusive;
var
  Msg: TCMButtonPressed;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_JVBUTTONPRESSED;
    Msg.Index := FGroupIndex;
    Msg.Control := Self;
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TJvCustomLookOutButton.SetCentered(Value: Boolean);
begin
  if FCentered <> Value then
  begin
    FCentered := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLookOutButton.SetDown(Value: Boolean);
begin
  if FStayDown <> Value then
  begin
    FStayDown := Value;
    if FStayDown then
    begin
      MouseOver := True;
      FDown := True;
    end
    else
      FDown := False;
    if FStayDown then
      UpdateExclusive;
    Invalidate;
  end;
end;

procedure TJvCustomLookOutButton.SetOffset(Value: Integer);
begin
  if FOffset <> Value then
    FOffset := Value;
end;

procedure TJvCustomLookOutButton.SetButtonBorder(Value: TJvButtonBorder);
begin
  if FButtonBorder <> Value then
  begin
    FButtonBorder := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLookOutButton.SetSmallImages(const Value: TCustomImageList);
begin
  ReplaceImageListReference(Self, Value, FSmallImages, FSmallImageChangeLink);
  Invalidate;
end;

procedure TJvCustomLookOutButton.SetLargeImages(const Value: TCustomImageList);
begin
  ReplaceImageListReference(Self, Value, FLargeImages, FLargeImageChangeLink);
  Invalidate;
end;

procedure TJvCustomLookOutButton.SetImageIndex(Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLookOutButton.SetImageSize(Value: TJvImageSize);
begin
  if FImageSize <> Value then
  begin
    FImageSize := Value;
    if csDesigning in ComponentState then
      SetParentImageSize(False);
    Invalidate;
  end;
end;

procedure TJvCustomLookOutButton.SetFillColor(Value: TColor);
begin
  if FFillColor <> Value then
  begin
    FFillColor := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLookOutButton.SetHighlightFont(Value: TFont);
begin
  FHighlightFont.Assign(Value);
  if FHighlightFont <> Font then
    Invalidate;
end;

procedure TJvCustomLookOutButton.SetSpacing(Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLookOutButton.SetParentImageSize(Value: Boolean);
begin
  FParentImageSize := Value;
  if FParentImageSize and (Parent is TJvLookOutPage) then
    SetImageSize(TJvLookOutPage(Parent).ImageSize);
end;

procedure TJvCustomLookOutButton.Paint;
var
  R: TRect;
  Flags, H: Integer;
begin
  R := GetClientRect;

  with Canvas do
  begin
    if csDesigning in ComponentState then
    begin
      Brush.Color := clBlack;
      FrameRect( R);
    end;

    if (FImageSize = isSmall) and Assigned(FSmallImages) then
    begin
      FImageRect.Left := FSpacing;
      FImageRect.Right := FImageRect.Left + FSmallImages.Width;
      FImageRect.Top := (Height - FSmallImages.Height) div 2;
      FImageRect.Bottom := FImageRect.Top + FSmallImages.Height;
    end
    else
    if Assigned(FLargeImages) then
    begin
      FImageRect.Left := (Width - FLargeImages.Width) div 2;
      FImageRect.Right := FImageRect.Left + FLargeImages.Width;
      FImageRect.Top := FSpacing;
      FImageRect.Bottom := FImageRect.Top + FLargeImages.Height;
    end;

    PaintFrame;

    Flags := DT_END_ELLIPSIS  or DT_EDITCONTROL ;

    if (FImageSize = isSmall) and Assigned(FSmallImages) then
    begin
      DrawSmallImages;
      Flags := Flags or DT_VCENTER or DT_SINGLELINE;
      //      W := FSmallImages.Width;
    end
    else
    if (FImageSize = isLarge) and Assigned(FLargeImages) then
    begin
      DrawLargeImages;
      //      W := FLargeImages.Width;
      Flags := Flags or DT_WORDBREAK or DT_CENTER;
    end;
  end;

  { draw text }
  if Length(Caption) > 0 then
  begin
    if MouseOver then
      Canvas.Font := FHighlightFont
    else
      Canvas.Font := Font;

    //    W := FSpacing  + W;
    SetBkMode(Canvas.Handle, Windows.Transparent);
    R := GetClientRect;
    if (ImageSize = isLarge) and Assigned(FLargeImages) then
      R.Top := R.Top + FLargeImages.Height + (FSpacing * 2)
    else
    if (ImageSize = isSmall) and Assigned(FSmallImages) then
      R.Left := R.Left + FSmallImages.Width + (FSpacing * 3)
    else
      Flags := DT_END_ELLIPSIS or DT_WORDBREAK or DT_CENTER or DT_VCENTER  or DT_EDITCONTROL ;
    if FDown then
      OffsetRect(R, FOffset, FOffset);
    FTextRect := R;
    H := DrawText(Canvas, Caption, -1, FTextRect, Flags or DT_CALCRECT);
    if ImageSize = isLarge then
    begin
      FTextRect.Top := R.Top;
      FTextRect.Bottom := FTextRect.Top + H;
      FTextRect.Right := R.Left + Canvas.TextWidth(Caption);
    end
    else
    begin
      FTextRect.Top := (Height - Canvas.TextHeight(Caption)) div 2;
      FTextRect.Bottom := FTextRect.Top + Canvas.TextHeight(Caption);
      FTextRect.Right := R.Left + Canvas.TextWidth(Caption);
    end;
    DrawText(Canvas, Caption, -1, R, Flags);
  end;
end;

procedure TJvCustomLookOutButton.DrawSmallImages;
var
  Icon: TIcon;
begin
  if FDown then
    OffsetRect(FImageRect, FOffset, FOffset);
  Icon := TIcon.Create;
  try
    FSmallImages.GetIcon(FImageIndex, Icon);
    DrawIconEx(Canvas.Handle, FImageRect.Left, FImageRect.Top, Icon.Handle, 0, 0, 0, 0, DI_NORMAL);
  finally
    Icon.Free;
  end;
end;

procedure TJvCustomLookOutButton.DrawLargeImages;
var
  Icon: TIcon;
begin
  if FDown then
    OffsetRect(FImageRect, FOffset, FOffset);
  Icon := TIcon.Create;
  try
    FLargeImages.GetIcon(FImageIndex, Icon);
    DrawIconEx(Canvas.Handle, FImageRect.Left, FImageRect.Top, Icon.Handle, 0, 0, 0, 0, DI_NORMAL);
  finally
    Icon.Free;
  end;
end;

procedure TJvCustomLookOutButton.PaintFrame;
var
  R: TRect;
begin
  R := GetClientRect;

  if csDesigning in ComponentState then
  begin
    Canvas.Brush.Color := clBlack;
    with Canvas do
      FrameRect( R);
    Canvas.Brush.Color := Color;
  end;

  if not Enabled then
    Exit;
  if MouseOver or (csDesigning in ComponentState) then
  begin
    if (csDesigning in ComponentState) and not Visible then
    begin
      Canvas.Brush.Style := bsBDiagonal;
      Windows.FillRect(Canvas.Handle, R, Canvas.Brush.Handle);
      Canvas.Brush.Style := bsSolid;
    end
    else
    if FFillColor = clNone then
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
      else
      if FButtonBorder = bbLight then
        Frame3D(Canvas, R, clBtnShadow, clBtnHighlight, 1)
      else
        Frame3D(Canvas, R, cl3DDkShadow, clBtnHighlight, 1)
    end
    else
      case FButtonBorder of
        bbDark:
          Frame3D(Canvas, R, clBtnFace, cl3DDkShadow, 1);
        bbLight:
          Frame3D(Canvas, R, clBtnHighlight, clBtnShadow, 1);
      else
        Frame3D(Canvas, R, clBtnHighlight, cl3DDkShadow, 1);
      end;
  end;
end;

procedure TJvCustomLookOutButton.ImageListChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvCustomLookOutButton.CMParentImageSizeChanged(var Msg: TMessage);
var
  FTmp: Boolean;
begin
  if (Msg.LParam <> LPARAM(Self)) and FParentImageSize then
  begin
    FTmp := FParentImageSize;
    SetImageSize(TJvImageSize(Msg.WParam));
    FParentImageSize := FTmp;
  end;
end;

procedure TJvCustomLookOutButton.CMButtonPressed(var Msg: TCMButtonPressed);
var
  Sender: TJvCustomLookOutButton;
begin
  if Msg.Index = FGroupIndex then
  begin
    Sender := TJvCustomLookOutButton(Msg.Control);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FStayDown := False;
        FDown := False;
        MouseOver := False;
        Invalidate;
      end;
    end;
  end;
end;

procedure TJvCustomLookOutButton.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not MouseOver then
  begin
    if FFillColor = clNone then
      PaintFrame
    else
      Invalidate;
  end;
  inherited MouseEnter(Control);
end;

procedure TJvCustomLookOutButton.MouseLeave(Control: TControl);
begin
  if MouseOver then
  begin
    if not FStayDown then
      Invalidate;
  end;
  inherited MouseLeave(Control);
end;

procedure TJvCustomLookOutButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Tmp: TPoint;
  Msg: TMsg;
begin
  if Parent is TJvLookOutPage then
    TJvLookOutPage(Parent).ActiveButton := Self;

  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbRight then
  begin
    if Assigned(FPopUpMenu) then
    begin
      { calc where to put menu }
      Tmp := ClientToScreen(Point(X, Y));
      FPopUpMenu.PopupComponent := Self;
      FPopUpMenu.Popup(Tmp.X, Tmp.Y);
      { wait 'til menu is Done }
      // TODO
      while PeekMessage(Msg, 0, WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE) do
        {nothing};
    end;
    { release button }
    if not FStayDown then
      FDown := False;
  end
  else
  if MouseOver and (Button = mbLeft) then
    FDown := True
  else
  if not FStayDown then
    FDown := False;

  if FGroupIndex <> 0 then
    SetDown(not FStayDown);
  if FOffset = 0 then
    PaintFrame
  else
    Invalidate;
  //  Parent.Update;
end;

procedure TJvCustomLookOutButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Msg: TMessage;
begin
  inherited MouseMove(Shift, X, Y);

  if PtInRect(GetClientRect, Point(X, Y)) then { entire button }
  begin
    if not MouseOver then
    begin
      MouseOver := True;
      { notify others }
      Msg.Msg := CM_LEAVEBUTTON;
      Msg.WParam := 0;
      Msg.LParam := LPARAM(Self);
      Msg.Result := 0;
      Invalidate;
      Parent.Broadcast(Msg);
    end;
  end
  else
  if MouseOver then
  begin
    MouseOver := False;
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
    if FOffset = 0 then
      PaintFrame
    else
      Invalidate;
    //    Parent.Update;
  end;
end;

procedure TJvCustomLookOutButton.CMLeaveButton(var Msg: TMessage);
begin
  if (Msg.LParam <> LPARAM(Self)) and MouseOver and not FStayDown then
  begin
    MouseOver := False;
    //    FDown := False;
    Invalidate;
  end;
end;

procedure TJvCustomLookOutButton.CMTextChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TJvCustomLookOutButton.SetParent( AParent: TWinControl);
begin
  if AParent <> Parent then
  begin
    if (Parent <> nil) and (Parent is TJvLookOutPage) then
      TJvLookOutPage(Parent).FButtons.Delete(TJvLookOutPage(Parent).FButtons.IndexOf(Self));
    if (AParent <> nil) and (AParent is TJvLookOutPage) then
      TJvLookOutPage(AParent).FButtons.Add(Self);
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
    Invalidate;
  end;
end;

procedure TJvCustomLookOutButton.ActionChange(Sender: TObject;
  CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or (Self.ImageIndex = -1) then
        Self.ImageIndex := ImageIndex;
      if not CheckDefaults or (Self.GroupIndex = 0) then
        Self.GroupIndex := GroupIndex;
      if not CheckDefaults or not Self.Down then
        Self.Down := Checked;
    end;
end;

function TJvCustomLookOutButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TJvLookOutButtonActionLink;
end;

procedure TJvCustomLookOutButton.SetPopupMenu(const Value: TPopupMenu);
begin
  ReplaceComponentReference(Self, Value, TComponent(FPopupMenu));
end;

procedure TJvCustomLookOutButton.VisibleChanged;
begin
  inherited VisibleChanged;
  if not (csCreating in ControlState) then
  begin
    Invalidate;
    if Parent is TJvLookOutPage then
      TJvLookOutPage(Parent).ScrollChildren(0);
  end;
end;

//=== { TJvExpressButton } ===================================================

constructor TJvExpressButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FillColor := clBtnFace;
  Offset := 1;
  FButtonBorder := bbLight;
  FHighlightFont.Color := clWindowText;
  ParentFont := False;
  Font.Color := clHighlightText;
end;

//=== { TJvLookOutPage } =====================================================

constructor TJvLookOutPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csSetCaption];
  Color := clBtnShadow;
  FScrolling := 0;
  Caption := 'Outlook';
  FButtons := TList.Create;
  FDown := False;
  FShowPressed := False;
  Width := 92;
  Height := 100;
  //  SetBounds(0, 0, 92, 100);
  FHighlightFont := TFont.Create;
  FHighlightFont.Assign(Font);
  FMargin := 0;
  FTopControl := 0;
  FParentImageSize := True;
  FAutoRepeat := False;
  FAutoCenter := True;
  FBitmap := TBitmap.Create;
end;

destructor TJvLookOutPage.Destroy;
begin
  FEdit.Free;
  FUpArrow.Free;
  FDownArrow.Free;
  FBitmap.Free;
  FHighlightFont.Free;
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
    DownArrowClick(Self);
  Invalidate;
end;

procedure TJvLookOutPage.UpArrow;
begin
  if Enabled then
    UpArrowClick(Self);
  Invalidate;
end;

procedure TJvLookOutPage.ExchangeButtons(Button1, Button2: TJvCustomLookOutButton);
var
  Tmp: Integer;
begin
  Tmp := Button1.Top;
  Button1.Top := Button2.Top;
  Button2.Top := Tmp;
  FButtons.Exchange(FButtons.IndexOf(Button1), FButtons.IndexOf(Button2));
end;

procedure TJvLookOutPage.MoveButton(Button: TJvCustomLookOutButton; NewIndex: integer);
var OldIndex:integer;
begin
  OldIndex := FButtons.IndexOf(Button);
  FButtons.Move(OldIndex, NewIndex);
end;

function TJvLookOutPage.InsertButton(Index:integer): TJvCustomLookOutButton;
begin
  Result := AddButton;
  MoveButton(Result, Index);
end;

function TJvLookOutPage.AddButton: TJvCustomLookOutButton;
begin
  Result := TJvLookOutButton.Create(Self.Owner);
  Result.ImageIndex := ButtonCount;
  Result.Parent := Self;
  Result.Top := MaxInt;
  if Assigned(FUpArrow) and Assigned(FDownArrow) then
  begin
    FUpArrow.SetZOrder(True);
    FDownArrow.SetZOrder(True);
  end;
end;

procedure TJvLookOutPage.DoOnEdited(var Caption: string);
begin
  if Self is TJvExpress then
    Exit;
  if Assigned(FOnEdited) then
    FOnEdited(Self, Caption);
end;

procedure TJvLookOutPage.EditCaption;
begin
  if Self is TJvExpress then
    Exit;

  if not Assigned(FEdit) then
  begin
    FEdit := TJvLookOutEdit.Create(nil);
    FEdit.Parent := Self;
  end
  else
  if not FEdit.Visible then
    FEdit.Show;

  with FEdit do
  begin
    Text := Caption;
    //    BorderStyle := bsNone;
    SetBounds(0, 0, Width, cHeight);
    AutoSelect := True;
    OnKeyPress := EditKeyDown;
    OnMouseDown := EditMouseDown;
    SetFocus;
    SetCapture(FEdit.Handle);
    SelStart := 0;
    SelLength := Length(Caption);
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
var
  ACaption: string;
  Modify: Boolean;
begin
  Modify := False;
  if Sender = FEdit then
    case Key of
      Cr:
        begin
          Key := #0;
          ACaption := FEdit.Text;
          DoOnEdited(ACaption);
          FEdit.Text := ACaption;
          Modify := True;
          if FEdit.Handle = GetCapture then
            ReleaseCapture;
          FEdit.Hide;
          FEdit.Free;
          FEdit := nil;
          Screen.Cursor := crDefault;
        end;
      Esc:
        begin
          Key := #0;
          if FEdit.Handle = GetCapture then
            ReleaseCapture;
          FEdit.Hide;
          FEdit.Free;
          FEdit := nil;
          Screen.Cursor := crDefault;
        end;
    end;
  if Modify then
    Caption := ACaption;
end;

function TJvLookOutPage.WantKey(Key: Integer; Shift: TShiftState): Boolean;
begin
  Result := IsAccel(Key, Caption) and Enabled and (ssAlt in Shift);
  if Result then
    Click
  else
    Result := inherited WantKey(Key, Shift);
end;

procedure TJvLookOutPage.SetActiveButton(Value: TJvCustomLookOutButton);
begin
  if (Value <> nil) and (FActiveButton <> Value) and (Value.Parent = Self) then
    FActiveButton := Value;
end;

procedure TJvLookOutPage.SetParent( AParent: TWinControl);
begin
  if AParent <> Parent then
  begin
    if (Parent <> nil) and (Parent is TJvLookOut) then
      TJvLookOut(Parent).FPages.Delete(TJvLookOut(Parent).FPages.IndexOf(Self));
    if (AParent <> nil) and (AParent is TJvLookOut) then
      TJvLookOut(AParent).FPages.Add(Self);
  end;
  inherited SetParent(AParent);
end;

procedure TJvLookOutPage.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FPopUpMenu then
      FPopUpMenu := nil;
  end;
  if Operation = opInsert then
  begin
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
      if Assigned(FUpArrow) and Assigned(FDownArrow) then
      begin
        FUpArrow.SetZOrder(True);
        FDownArrow.SetZOrder(True);
      end;
  end;
end;

procedure TJvLookOutPage.AlignControls(Control: TControl; var Rect: TRect);
begin
  Inc(Rect.Top, cHeight);
  inherited AlignControls(Control, Rect);
end;

procedure TJvLookOutPage.SmoothScroll(AControl: TControl; NewTop, AInterval: Integer; Smooth: Boolean);
begin
  if Smooth and not (csDesigning in ComponentState) and not (csLoading in ComponentState) and not FInScroll then
  begin
    FInScroll := True;
    if AControl.Top < NewTop then
      if AControl.Top > 0 then
      begin
        while AControl.Top < NewTop do
        begin
          AControl.Top := AControl.Top + AInterval;
          // (rom) not a good implementation
          Application.ProcessMessages;
        end;
      end
      else
      begin
        while AControl.Top < NewTop do
        begin
          AControl.Top := AControl.Top - AInterval;
          Application.ProcessMessages;
        end;
      end
    else
    if AControl.Top > 0 then
    begin
      while AControl.Top > NewTop do
      begin
        AControl.Top := AControl.Top - AInterval;
        Application.ProcessMessages;
      end;
    end
    else
    begin
      while AControl.Top > NewTop do
      begin
        AControl.Top := AControl.Top + AInterval;
        Application.ProcessMessages;
      end;
    end;
  end;
  { adjust }
  AControl.Top := NewTop;
  Application.ProcessMessages;
  FInScroll := False;
end;

function Compare(Item1, Item2: Pointer): Integer;
begin
  Result := TControl(Item1).Top - TControl(Item2).Top;
end;

procedure TJvLookOutPage.ScrollChildren(Start: Word);
var
  R: TRect;
  I, X, ACount: Integer; {AList: TList;}
  AControl: TControl;
begin
  if FScrolling <> 0 then
    Exit;
  if (csReading in ComponentState) or (csLoading in ComponentState) or (csWriting in ComponentState) or
    (csDestroying in ComponentState) then
    Exit;
  { draw all owned controls }
  if ControlCount < 3 then
  begin
    if Assigned(FUpArrow) and Assigned(FDownArrow) then
    begin
      FUpArrow.Visible := False;
      FDownArrow.Visible := False;
    end;
    Exit;
  end;
  if FInScroll then
    Exit;
  R := GetClientRect;
  X := Width;
  ACount := GetButtonCount;
  if ACount = 0 then
    Exit;

// Mantis 2842: Sorting triggers the issue, and I can't see any reason why
// it should be sorted according to "Top" in the first place. (obones)
//  FButtons.Sort(Compare);

  FInScroll := True;
  for I := 0 to ACount - 1 do
  begin
    AControl := FButtons[I];
    if not AControl.Visible and not (csDesigning in ComponentState) then
      Continue;
    if AControl.Align <> alNone then
      AControl.Align := alNone;

    if I < FTopControl then
      AControl.Top := -(AControl.Height + 1) * (ACount - I)
    else
    if Start > Height then
      AControl.Top := (Height + 1) * (I + 1)
    else
    begin
      AControl.Top := Start + FMargin;
      Inc(Start, (AControl.Height + FMargin));
    end;

    if FAutoCenter and (AControl is TJvCustomLookOutButton) and
      (TJvCustomLookOutButton(AControl).ImageSize = isLarge) then
      AControl.Left := (X - AControl.Width) div 2;
  end;
  FInScroll := False;
end;

procedure TJvLookOutPage.CreateWnd;
var
  R: TRect;
begin
  inherited CreateWnd;
  R := GetClientRect;
  if not Assigned(FUpArrow) then
  begin
    FUpArrow := TJvUpArrowBtn.Create(nil);
    FUpArrow.Parent := Self;
  end;

  if not Assigned(FDownArrow) then
  begin
    FDownArrow := TJvDwnArrowBtn.Create(nil);
    FDownArrow.Parent := Self;
  end;

  with FUpArrow do
  begin
    Visible := False;
    SetBounds(R.Right - 23, R.Top + 25, 16, 16);
  end;

  with FDownArrow do
  begin
    Visible := False;
    SetBounds(R.Right - 23, R.Bottom - 23, 16, 16);
  end;

  if Assigned(Parent) and (Parent is TJvLookOut) then
  begin
    FManager := TJvLookOut(Parent);
    FOnCollapse := FManager.FOnCollapse;
  end;
  // (p3) fix to work with frames
  if GetParentForm(Self) <> nil then
  begin
    FUpArrow.SetZOrder(True);
    FDownArrow.SetZOrder(True);
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

procedure TJvLookOutPage.EnabledChanged;
begin
  if not (Assigned(FUpArrow) or Assigned(FDownArrow)) then
    Exit;
  if not Enabled then
  begin
    FUpArrow.Enabled := False;
    FDownArrow.Enabled := False;
  end
  else
  begin
    FUpArrow.Enabled := True;
    FDownArrow.Enabled := True;
  end;
  inherited EnabledChanged;
  Refresh;
end;

function TJvLookOutPage.IsVisible(Control: TControl): Boolean;
var
  R: TRect;
begin
  Result := False;
  if Control = nil then
    Exit;
  R := GetClientRect;
  Result := (PtInRect(R, Point(R.Left + 1, Control.Top)) and
    PtInRect(R, Point(R.Left + 1, Control.Top + Control.Height)));
end;

procedure TJvLookOutPage.SetAutoRepeat(Value: Boolean);
begin
  if FAutoRepeat <> Value then
  begin
    FAutoRepeat := Value;
    if Assigned(FUpArrow) and Assigned(FDownArrow) then
    begin
      FUpArrow.AutoRepeat := FAutoRepeat;
      FDownArrow.AutoRepeat := FAutoRepeat;
    end;
  end;
end;

procedure TJvLookOutPage.SetHighlightFont(Value: TFont);
begin
  FHighlightFont.Assign(Value);
  if FHighlightFont <> Font then
    DrawTopButton;
end;

procedure TJvLookOutPage.SetButton(Index: Integer; Value: TJvCustomLookOutButton);
begin
  FButtons[Index] := Value;
end;

function TJvLookOutPage.GetButton(Index: Integer): TJvCustomLookOutButton;
begin
  Result := TJvLookOutButton(FButtons[Index]);
end;

function TJvLookOutPage.GetButtonCount: Integer;
begin
  Result := FButtons.Count;
end;

procedure TJvLookOutPage.SetAutoCenter(Value: Boolean);
begin
  if FAutoCenter <> Value then
  begin
    FAutoCenter := Value;
    if FAutoCenter then
      ScrollChildren(cHeight + 7 - FMargin);
  end;
end;

procedure TJvLookOutPage.SetMargin(Value: Integer);
begin
  if FMargin <> Value then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TJvLookOutPage.SetImageSize(Value: TJvImageSize);
var
  Msg: TMessage;
begin
  if FImageSize <> Value then
  begin
    FImageSize := Value;
    if csDesigning in ComponentState then
      SetParentImageSize(False);
    { notify children }
    Msg.Msg := CM_IMAGESIZECHANGED;
    Msg.WParam := WPARAM(Ord(FImageSize));
    Msg.LParam := LPARAM(Self);
    Msg.Result := 0;
    if Parent <> nil then
      Parent.Broadcast(Msg);
    Broadcast(Msg);
  end;
end;

procedure TJvLookOutPage.SetParentImageSize(Value: Boolean);
begin
  FParentImageSize := Value;
  if FParentImageSize and (FManager <> nil) then
    SetImageSize(FManager.ImageSize);
end;

procedure TJvLookOutPage.CMParentImageSizeChanged(var Msg: TMessage);
var
  Tmp: Boolean;
begin
  if (Msg.LParam <> LPARAM(Self)) and FParentImageSize then
  begin
    Tmp := FParentImageSize;
    SetImageSize(TJvImageSize(Msg.WParam));
    FParentImageSize := Tmp;
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

{ determine if arrows should be visible }

procedure TJvLookOutPage.CalcArrows;
var
  I: Integer;
  R: TRect;
  AList: TList;
begin
  if Assigned(FUpArrow) and Assigned(FDownArrow) then
  begin
    // (rom) needs constants instead of numbers
    if Height < 65 then
    begin
      //      FUpArrow.Visible := False;
      //      FDownArrow.Visible := False;
      FDownArrow.Top := FUpArrow.Top + 16;
      Exit;
    end;

    R := GetClientRect;
    FUpArrow.SetBounds(R.Right - 23, R.Top + 25, 16, 16);
    FDownArrow.SetBounds(R.Right - 23, R.Bottom - 23, 16, 16);
    AList := TList.Create;
    try
      for I := 0 to ControlCount - 1 do
      begin
        if (Controls[I] = FUpArrow) or (Controls[I] = FDownArrow) or (Controls[I] = FEdit) then
          Continue;

        if not Controls[I].Visible and not (csDesigning in ComponentState) then
          Continue;
          
        if Controls[I].Align = alClient then
          Continue;

        AList.Insert(AList.Count, Controls[I]);
      end;

      if AList.Count = 0 then
        Exit;
      AList.Sort(Compare);
      FDownArrow.Visible := not IsVisible(AList.Items[AList.Count - 1]);
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
var
  R, R2: TRect;
  DC: HDC;
  FFlat, FPush: Boolean;
begin
  if MouseOver then
    Canvas.Font := FHighlightFont
  else
    Canvas.Font := Self.Font;

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
    if FManager.ActivePage = Self then
    begin
      R2 := GetClientRect;
      R2.Top := R.Bottom;
      Frame3D(Canvas, R2, cl3DDkShadow, clBtnFace, 1);
    end;

    if FPush then
      Frame3D(Canvas, R, clBtnShadow, clBtnHighlight, 1)
    else
    if MouseOver then
    begin
      Frame3D(Canvas, R, clBtnHighlight, cl3DDkShadow, 1);
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
  if Caption <> '' then
  begin
    if not Enabled then
    begin
      { draw disabled text }
      SetTextColor(DC, ColorToRGB(clBtnHighlight));
      OffsetRect(R, 1, 1);
      DrawText(DC, Caption, Length(Caption), R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
      OffsetRect(R, -1, -1);
      SetTextColor(DC, ColorToRGB(clBtnShadow));
    end
    else
      SetTextColor(DC, ColorToRGB(Canvas.Font.Color));
    if FShowPressed and FDown then
      OffsetRect(R, 1, 1);
    DrawText(DC, Caption, Length(Caption), R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;
end;

procedure TJvLookOutPage.TileBitmap;
var
  X, Y, W, H: Longint;
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
    while Y < Height do
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
var
  R: TRect;
  Tmp: TPoint;
  Msg: TMsg;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Assigned(FPopUpMenu) and (Button = mbRight) then
  begin
    { calc where to put menu }
    Tmp := ClientToScreen(Point(X, Y));
    FPopUpMenu.PopupComponent := Self;
    FPopUpMenu.Popup(Tmp.X, Tmp.Y);
    { wait 'til menu is Done }
    while PeekMessage(Msg, 0, WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE) do
      {nothing};
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
var
  R: TRect;
begin
  R := GetClientRect;
  R.Bottom := cHeight;
  if PtInRect(R, Point(X, Y)) then
  begin
    if not MouseOver then
    begin
      MouseOver := True;
      DrawTopButton;
    end
  end
  else
  if MouseOver or FDown then
  begin
    MouseOver := False;
    //    FDown := False;
    DrawTopButton;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TJvLookOutPage.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  R: TRect;
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
      FOnCollapse(Self);
    if Assigned(FOnClick) then
      FOnClick(Self);
  end;
  DrawTopButton;
end;

procedure TJvLookOutPage.MouseLeave(Control: TControl);
begin
  if MouseOver then
  begin
    inherited MouseLeave(Control);
    // FDown := False;
    DrawTopButton;
  end;
end;

procedure TJvLookOut.SetFlatButtons(Value: Boolean);
begin
  if FFlatButtons <> Value then
  begin
    FFlatButtons := Value;
    //    for I := 0 to PageCount - 1 do
    //      Pages[I].DrawTopButton;
    RecreateWnd;
  end;
end;

//=== { TJvLookOut } =========================================================

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
  FSmooth := False;
  FFlatButtons := False;
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
  Result := TJvLookOutPage.Create(Self.Owner);
  Result.Parent := Self;
  ActivePage := Result;
end;

procedure TJvLookOut.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FActivePage then
      FActivePage := nil;
    if AComponent = FCurrentPage then
      FCurrentPage := nil;
    if (AComponent is TJvLookOutPage) and (TJvLookOutPage(AComponent).Parent = Self) then
    begin
      I := FPages.IndexOf(AComponent);
      if I > -1 then
        FPages.Delete(I);
    end;
  end
  else {// insertion}
  if (AComponent is TJvLookOutPage) and (TJvLookOutPage(AComponent).Parent = Self) then
  begin
    if FPages.IndexOf(AComponent) = -1 then
      FPages.Add(AComponent);
  end;

  if Canvas <> nil then
    Invalidate;
end;

procedure TJvLookOut.UpdateControls;
begin
  if FCurrentPage <> nil then
    DoCollapse(FCurrentPage)
  else
  if FActivePage <> nil then
    DoCollapse(FActivePage)
  else
  if (ControlCount > 0) and (Controls[0] is TJvLookOutPage) then
    DoCollapse(Controls[0]);
end;

procedure TJvLookOut.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if FAutoSize then
      UpdateControls;
  end;
end;

procedure TJvLookOut.SetImageSize(Value: TJvImageSize);
var
  Msg: TMessage;
begin
  if FImageSize <> Value then
  begin
    FImageSize := Value;
    { notify children }
    Msg.Msg := CM_IMAGESIZECHANGED;
    Msg.WParam := WPARAM(Ord(FImageSize));
    Msg.LParam := LPARAM(Self);
    Msg.Result := 0;
    Broadcast(Msg);
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
var
  C: TControl;
  Done: Boolean;
  vis, I, ht, ofs, bh, cc, flt: Integer;
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
  Done := False;
  ht := Height;
  vis := 0;
  ofs := 0;

  { make sure non-visible pages don't mess up the display }
  for I := 0 to cc do
    if Controls[I].Visible then
      Inc(vis);
  if Height <= (cHeight * vis) + 65 then
    Exit;
  if FFlatButtons then
    flt := 2
  else
    flt := 4;

  for I := 0 to cc do
  begin
    C := Controls[I];

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

    if C = Sender then
      Done := True;

    if (C = Sender) or (I = 0) then { first or caller }
      SmoothScroll(C, (I - ofs) * bh, cSpeed, FSmooth)
    else
    if Done and (C <> Sender) then { place at bottom }
      SmoothScroll(C, ht - (vis - I + ofs) * bh - flt + 1, cSpeed, FSmooth)
    else { place at top }
      SmoothScroll(C, (I - ofs) * bh, cSpeed, FSmooth);
  end;
end;

procedure TJvLookOut.SmoothScroll(AControl: TControl; NewTop, AInterval: Integer; Smooth: Boolean);
begin
  if Smooth and not (csDesigning in ComponentState) then
  begin
    if AControl.Top < NewTop then
      while AControl.Top < NewTop do
      begin
        AControl.Top := AControl.Top + AInterval;
      end
    else
      while AControl.Top > NewTop do
      begin
        AControl.Top := AControl.Top - AInterval;
      end;
  end;
  { adjust }
  AControl.Top := NewTop;
end;

procedure TJvLookOut.SetActiveOutlook(Value: TJvLookOutPage);
var
  I: Integer;
begin
  if (Value <> nil) and (Value.Parent = Self) and (Value.Visible) then
    DoCollapse(Value)
  else
  if PageCount > 0 then
    for I := 0 to PageCount - 1 do
      if Pages[I].Visible then
        DoCollapse(Pages[I])
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

function TJvLookOut.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

function TJvLookOut.GetPage(Index: Integer): TJvLookOutPage;
begin
  Result := TJvLookOutPage(FPages[Index]);
end;

procedure TJvLookOut.SetPage(Index: Integer; Value: TJvLookOutPage);
begin
  FPages[Index] := Value;
end;


procedure TJvLookOut.WMNCCalcSize(var Msg: TWMNCCalcSize);
begin
  with Msg.CalcSize_Params^ do
    if FFlatButtons then
      InflateRect(rgrc[0], -1, -1)
    else
      InflateRect(rgrc[0], -2, -2);
  inherited;
end;



procedure TJvLookOut.WMNCPaint(var Msg: TMessage);
var
  DC: HDC;
  RC, RW: TRect;
begin
  DC := GetWindowDC(Handle);
  try
    GetWindowRect(Handle, RW);
    Windows.GetClientRect(Handle, RC);
    MapWindowPoints(NullHandle, Handle, RW, 2);
    OffsetRect(RC, -RW.Left, -RW.Top);
    ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
    OffsetRect(RW, -RW.Left, -RW.Top);
    if FBorderStyle = bsSingle then
    begin
      {$IFDEF JVCLThemesEnabled}
      if ThemeServices.{$IFDEF RTL230_UP}Enabled{$ELSE}ThemesEnabled{$ENDIF RTL230_UP} then
        DrawThemedBorder(Self)
      else
      {$ENDIF JVCLThemesEnabled}
      DrawEdge(DC, RW, EDGE_SUNKEN, BF_RECT)
    end
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

//=== { TJvExpress } =========================================================

constructor TJvExpress.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCenter := False;
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
  Result.Parent := Self;
  Result.ImageIndex := ButtonCount;
  Result.Top := MaxInt;
  if Assigned(FUpArrow) and Assigned(FDownArrow) then
  begin
    FUpArrow.SetZOrder(True);
    FDownArrow.SetZOrder(True);
  end;
end;

procedure TJvExpress.AlignControls(Control: TControl; var Rect: TRect);
begin
  // TJvLookoutPage adjusts the rects top, so move it back
  Dec(Rect.Top, cHeight);
  inherited AlignControls(Control, Rect);
end;

procedure TJvExpress.CalcArrows;
var
  I: Integer;
  R: TRect;
  AList: TList;
begin
  if Assigned(FUpArrow) and Assigned(FDownArrow) then
  begin
    if Height < 65 then
    begin
      // FDownArrow.Top := FUpArrow.Top + 16;
      Exit;
    end;

    R := GetClientRect;
    AList := TList.Create;
    try
      for I := 0 to ControlCount - 1 do
      begin
        if (Controls[I] = FUpArrow) or (Controls[I] = FDownArrow) or (Controls[I] = FEdit) then
          Continue;

        if not (Controls[I].Visible or (csDesigning in ComponentState)) then
          Continue;
        AList.Insert(AList.Count, Controls[I]);
      end;

      if AList.Count = 0 then
        Exit;
      AList.Sort(Compare);
      FDownArrow.Visible := not IsVisible(AList.Items[AList.Count - 1]);
      FUpArrow.Visible := not IsVisible(AList.Items[0]);
    finally
      AList.Free;
    end;
  end;
end;

procedure TJvExpress.ScrollChildren(Start: Word);
var
  I: Integer;
begin
  { size all children to width of TJvExpress }
  if not AutoCenter then
    for I := 0 to ControlCount - 1 do
      if Controls[I] is TJvExpressButton then
        Controls[I].SetBounds(0, Controls[I].Top, Width - 4, FButtonHeight);

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

procedure TJvExpress.SetButtonHeight(Value: Integer);
var
  I: Integer;
begin
  if FButtonHeight <> Value then
  begin
    FButtonHeight := Value;
    for I := 0 to ButtonCount - 1 do
      Buttons[I].Height := FButtonHeight;
  end;
end;


procedure TJvExpress.WMNCCalcSize(var Msg: TWMNCCalcSize);
begin
  with Msg.CalcSize_Params^ do
    InflateRect(rgrc[0], -2, -2);
  inherited;
end;


procedure TJvExpress.CreateWnd;
begin
  inherited CreateWnd;
  if not Assigned(FUpArrow) then
    FUpArrow := TJvUpArrowBtn.Create(nil);

  if not Assigned(FDownArrow) then
    FDownArrow := TJvDwnArrowBtn.Create(nil);
  with FUpArrow do
  begin
    Parent := Self;
    Flat := True;
    Height := 13;
    Align := alTop;
    SetZOrder(True);
  end;

  with FDownArrow do
  begin
    Parent := Self;
    Flat := True;
    Height := 13;
    Align := alBottom;
    SetZOrder(True);
  end;
end;

procedure TJvExpress.WMNCPaint(var Msg: TMessage);
var
  DC: HDC;
  RC, RW: TRect;
begin
  DC := GetWindowDC(Handle);
  try
    GetWindowRect(Handle, RW);
    Windows.GetClientRect(Handle, RC);
    MapWindowPoints(NullHandle, Handle, RW, 2);
    OffsetRect(RC, -RW.Left, -RW.Top);
    ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
    OffsetRect(RW, -RW.Left, -RW.Top);
    if FBorderStyle = bsSingle then
    begin
      {$IFDEF JVCLThemesEnabled}
      if ThemeServices.{$IFDEF RTL230_UP}Enabled{$ELSE}ThemesEnabled{$ENDIF RTL230_UP} then
        DrawThemedBorder(Self)
      else
      {$ENDIF JVCLThemesEnabled}
      DrawEdge(DC, RW, EDGE_SUNKEN, BF_RECT);
    end
    else
    begin
      if csDesigning in ComponentState then
        Canvas.Brush.Color := clBlack
      else
        Canvas.Brush.Color := Color;
      FrameRect(DC, RW, Canvas.Brush.Handle);
      InflateRect(RW, -1, -1);
      if csDesigning in ComponentState then
        Canvas.Brush.Color := Color;
      FrameRect(DC, RW, Canvas.Brush.Handle);
      InflateRect(RW, 1, 1);
    end;
    { Erase parts not drawn }
    IntersectClipRect(DC, RW.Left, RW.Top, RW.Right, RW.Bottom);
  finally
    ReleaseDC(Handle, DC);
  end;
end;

procedure TJvLookOutPage.CMTextChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

{$IFDEF UNITVERSIONING}


initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
