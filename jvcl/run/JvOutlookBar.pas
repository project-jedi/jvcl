{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvOLBar.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvOutlookBar;

{ Outlook style control. Simpler than TJvLookout)
   Hierarchy:
    TJvCustomOutlookBar
      Pages:TJvOutlookBarPages
        Page:TJvOutlookBarPage
          Buttons:TJvOutlookBarButtons
            Button:TJvOutlookBarButton
}

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Controls, Buttons, Graphics,
  ImgList, Forms, StdCtrls, ExtCtrls,
  {$IFDEF JVCLThemesEnabled}
  UxTheme,
  {$IFNDEF COMPILER7_UP}
  TmSchema,
  {$ENDIF COMPILER7_UP}
  {$ENDIF JVCLThemesEnabled}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Types, QControls, QButtons, QGraphics, QTypes,
  QImgList, QForms, QStdCtrls, QExtCtrls, QWindows,
  {$ENDIF VisualCLX}
  JvThemes, JvComponent, JvExButtons;

const
  CM_CAPTION_EDITING = CM_BASE + 756;
  CM_CAPTION_EDIT_ACCEPT = CM_CAPTION_EDITING + 1;
  CM_CAPTION_EDIT_CANCEL = CM_CAPTION_EDITING + 2;

type
  TJvBarButtonSize = (olbsLarge, olbsSmall);

  TJvOutlookBarButton = class(TCollectionItem)
  private
    FImageIndex: TImageIndex;
    FCaption: TCaption;
    FTag: Integer;
    FDown: Boolean;
    FEnabled: Boolean;
    FAutoToggle: Boolean;
    procedure SetCaption(const Value: TCaption);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetDown(const Value: Boolean);
    procedure Change;
    procedure SetEnabled(const Value: Boolean);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure EditCaption;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex;
    property Tag: Integer read FTag write FTag;
    property Down: Boolean read FDown write SetDown default False;
    property AutoToggle: Boolean read FAutoToggle write FAutoToggle;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

  TJvOutlookBarButtons = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TJvOutlookBarButton;
    procedure SetItem(Index: Integer; const Value: TJvOutlookBarButton);
  protected
    {$IFDEF COMPILER5}
    function Owner: TPersistent;
    {$ENDIF COMPILER5}
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TJvOutlookBarButton;
    procedure Assign(Source: TPersistent); override;
    function Insert(Index: Integer): TJvOutlookBarButton;
    property Items[Index: Integer]: TJvOutlookBarButton read GetItem write SetItem; default;
  end;

  TJvOutlookBarPage = class(TCollectionItem)
  private
    FPicture: TPicture;
    FCaption: TCaption;
    FColor: TColor;
    FButtonSize: TJvBarButtonSize;
    FParentButtonSize: Boolean;
    FParentFont: Boolean;
    FParentColor: Boolean;
    FTopButtonIndex: Integer;
    FButtons: TJvOutlookBarButtons;
    FFont: TFont;
    FDownFont: TFont;
    FImageIndex: TImageIndex;
    FAlignment: TAlignment;
    FEnabled: Boolean;
    procedure SetButtonSize(const Value: TJvBarButtonSize);
    procedure SetCaption(const Value: TCaption);
    procedure SetColor(const Value: TColor);
    procedure SetPicture(const Value: TPicture);
    procedure Change;
    procedure SetParentButtonSize(const Value: Boolean);
    procedure SetParentColor(const Value: Boolean);
    procedure SetTopButtonIndex(const Value: Integer);
    procedure SetButtons(const Value: TJvOutlookBarButtons);
    procedure SetParentFont(const Value: Boolean);
    procedure SetFont(const Value: TFont);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetAlignment(const Value: TAlignment);
    procedure DoFontChange(Sender: TObject);
    procedure SetDownFont(const Value: TFont);
    function GetDownButton: TJvOutlookBarButton;
    function GetDownIndex: Integer;
    procedure SetDownButton(Value: TJvOutlookBarButton);
    procedure SetDownIndex(Value: Integer);
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure DoPictureChange(Sender: TObject);
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure EditCaption;
    property DownButton: TJvOutlookBarButton read GetDownButton write SetDownButton;
    property DownIndex: Integer read GetDownIndex write SetDownIndex;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Buttons: TJvOutlookBarButtons read FButtons write SetButtons;
    property ButtonSize: TJvBarButtonSize read FButtonSize write SetButtonSize;
    property Caption: TCaption read FCaption write SetCaption;
    property Color: TColor read FColor write SetColor;
    property DownFont: TFont read FDownFont write SetDownFont;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property Font: TFont read FFont write SetFont;
    property Picture: TPicture read FPicture write SetPicture;
    property ParentButtonSize: Boolean read FParentButtonSize write SetParentButtonSize default True;
    property ParentFont: Boolean read FParentFont write SetParentFont default False;
    property ParentColor: Boolean read FParentColor write SetParentColor;
    property TopButtonIndex: Integer read FTopButtonIndex write SetTopButtonIndex;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

  TJvOutlookBarPages = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TJvOutlookBarPage;
    procedure SetItem(Index: Integer; const Value: TJvOutlookBarPage);
  protected
    procedure Update(Item: TCollectionItem); override;
    {$IFDEF COMPILER5}
    function Owner: TPersistent;
    {$ENDIF COMPILER5}
  public
    constructor Create(AOwner: TPersistent);
    function Add: TJvOutlookBarPage;
    function Insert(Index: Integer): TJvOutlookBarPage;
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: TJvOutlookBarPage read GetItem write SetItem; default;
  end;

  TOutlookBarPageChanging = procedure(Sender: TObject; Index: Integer; var AllowChange: Boolean) of object;
  TOutlookBarPageChange = procedure(Sender: TObject; Index: Integer) of object;
  TOutlookBarButtonClick = procedure(Sender: TObject; Index: Integer) of object;
  TOutlookBarEditCaption = procedure(Sender: TObject; var NewText: string;
    Index: Integer; var Allow: Boolean) of object;

  TJvOutlookBarCustomDrawStage = (odsBackground, odsPageButton, odsPage, odsButton, odsButtonFrame);
  TJvOutlookBarCustomDrawEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
    AStage: TJvOutlookBarCustomDrawStage; AIndex: Integer; ADown, AInside: Boolean; var DefaultDraw: Boolean) of object;

  TJvCustomOutlookBar = class(TJvCustomControl)
  private
    FTopButton: TSpeedButton;
    FBtmButton: TSpeedButton;
    FPages: TJvOutlookBarPages;
    FLargeChangeLink: TChangeLink;
    FSmallChangeLink: TChangeLink;
    FPageChangeLink: TChangeLink;
    FActivePageIndex: Integer;
    FButtonSize: TJvBarButtonSize;
    FSmallImages: TCustomImageList;
    FLargeImages: TCustomImageList;
    FPageButtonHeight: Integer;
    FBorderStyle: TBorderStyle;
    FNextActivePage: Integer;
    FPressedPageBtn: Integer;
    {$IFDEF JVCLThemesEnabled}
    FHotPageBtn: Integer;
    FThemedBackGround: Boolean;
    {$ENDIF JVCLThemesEnabled}
    FOnPageChange: TOutlookBarPageChange;
    FOnPageChanging: TOutlookBarPageChanging;
    FButtonRect: TRect;
    FLastButtonIndex: Integer;
    FPressedButtonIndex: Integer;
    FOnButtonClick: TOutlookBarButtonClick;
    FPopUpObject: TObject;
    FEdit: TCustomEdit;
    FOnEditButton: TOutlookBarEditCaption;
    FOnEditPage: TOutlookBarEditCaption;
    FOnCustomDraw: TJvOutlookBarCustomDrawEvent;
    FPageImages: TCustomImageList;
    procedure SetPages(const Value: TJvOutlookBarPages);
    procedure DoChangeLinkChange(Sender: TObject);
    procedure SetActivePageIndex(const Value: Integer);
    procedure SetButtonSize(const Value: TJvBarButtonSize);
    procedure SetLargeImages(const Value: TCustomImageList);
    procedure SetSmallImages(const Value: TCustomImageList);
    procedure SetPageImages(const Value: TCustomImageList);
    procedure SetPageButtonHeight(const Value: Integer);
    procedure SetBorderStyle(const Value: TBorderStyle);
    {$IFDEF JVCLThemesEnabled}
    procedure SetThemedBackground(const Value: Boolean);
    {$ENDIF JVCLThemesEnabled}
    function DrawTopPages: Integer;
    procedure DrawCurrentPage(PageIndex: Integer);
    procedure DrawPageButton(R: TRect; Index: Integer; Pressed: Boolean);
    procedure DrawBottomPages(StartIndex: Integer);
    procedure DrawButtons(Index: Integer);
    procedure DrawArrowButtons(Index: Integer);
    procedure DrawButtonFrame(PageIndex, ButtonIndex, PressedIndex: Integer);
    function DrawPicture(R: TRect; Picture: TPicture): Boolean;
    procedure DoDwnClick(Sender: TObject);
    procedure DoUpClick(Sender: TObject);
    procedure RedrawRect(R: TRect; Erase: Boolean = False);
    procedure CMCaptionEditing(var Msg: TMessage); message CM_CAPTION_EDITING;
    procedure CMCaptionEditAccept(var Msg: TMessage); message CM_CAPTION_EDIT_ACCEPT;
    procedure CMCaptionEditCancel(var Msg: TMessage); message CM_CAPTION_EDIT_CANCEL;
    {$IFDEF VCL}
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    {$ENDIF VCL}
    procedure DoButtonEdit(NewText: string; B: TJvOutlookBarButton);
    procedure DoPageEdit(NewText: string; P: TJvOutlookBarPage);
    function GetActivePage: TJvOutlookBarPage;
    function GetActivePageIndex: Integer;
  protected
    {$IFDEF VisualCLX}
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    {$ENDIF VisualCLX}
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; override;
    procedure FontChanged; override;
    {$IFDEF VCL}
    procedure CreateParams(var Params: TCreateParams); override;
    {$ENDIF VCL}
    function GetButtonHeight(PageIndex: Integer): Integer;
    function GetButtonFrameRect(PageIndex, ButtonIndex: Integer): TRect;
    function GetButtonTextRect(PageIndex, ButtonIndex: Integer): TRect;
    function GetButtonRect(PageIndex, ButtonIndex: Integer): TRect;
    function GetPageButtonRect(Index: Integer): TRect;
    function GetPageTextRect(Index: Integer): TRect;
    function GetPageRect(Index: Integer): TRect;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure ColorChanged; override;
    function DoPageChanging(Index: Integer): Boolean; virtual;
    procedure DoPageChange(Index: Integer); virtual;
    procedure DoButtonClick(Index: Integer); virtual;
    procedure DoContextPopup({$IFDEF VisualCLX} const {$ENDIF} MousePos: TPoint; var Handled: Boolean); override;
    function DoDrawBackGround: Boolean;
    function DoDrawPage(ARect: TRect; Index: Integer): Boolean;
    function DoDrawPageButton(ARect: TRect; Index: Integer; Down: Boolean): Boolean;
    function DoDrawButton(ARect: TRect; Index: Integer; Down, Inside: Boolean): Boolean;
    function DoDrawButtonFrame(ARect: TRect; Index: Integer; Down, Inside: Boolean): Boolean;
    function DoCustomDraw(ARect: TRect; Stage: TJvOutlookBarCustomDrawStage; Index: Integer; Down, Inside: Boolean): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetButtonAtPos(P: TPoint): TJvOutlookBarButton;
    function GetPageButtonAtPos(P: TPoint): TJvOutlookBarPage;
  protected
    property PopUpObject: TObject read FPopUpObject write FPopUpObject;
    property Width default 100;
    property Height default 220;
    property TopButton: TSpeedButton read FTopButton;
    property BtmButton: TSpeedButton read FBtmButton;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Font;
    property Color default clBtnShadow;
    property Pages: TJvOutlookBarPages read FPages write SetPages;
    property LargeImages: TCustomImageList read FLargeImages write SetLargeImages;
    property SmallImages: TCustomImageList read FSmallImages write SetSmallImages;
    property PageImages: TCustomImageList read FPageImages write SetPageImages;
    property ButtonSize: TJvBarButtonSize read FButtonSize write SetButtonSize default olbsLarge;
    property PageButtonHeight: Integer read FPageButtonHeight write SetPageButtonHeight default 19;
    property ActivePageIndex: Integer read GetActivePageIndex write SetActivePageIndex default 0;
    {$IFDEF JVCLThemesEnabled}
    property ThemedBackground: Boolean read FThemedBackGround write SetThemedBackground default True;
    {$ENDIF JVCLThemesEnabled}
    property OnPageChanging: TOutlookBarPageChanging read FOnPageChanging write FOnPageChanging;
    property OnPageChange: TOutlookBarPageChange read FOnPageChange write FOnPageChange;
    property OnButtonClick: TOutlookBarButtonClick read FOnButtonClick write FOnButtonClick;
    property OnEditButton: TOutlookBarEditCaption read FOnEditButton write FOnEditButton;
    property OnEditPage: TOutlookBarEditCaption read FOnEditPage write FOnEditPage;
    property OnCustomDraw: TJvOutlookBarCustomDrawEvent read FOnCustomDraw write FOnCustomDraw;
  public
    property ActivePage: TJvOutlookBarPage read GetActivePage;
  end;

  TJvOutlookBar = class(TJvCustomOutlookBar)
  public
    property PopUpObject;
  published
    property Align;
    property Pages;
    property LargeImages;
    property SmallImages;
    property PageImages;
    property ButtonSize;
    property PageButtonHeight;
    property ActivePageIndex;
    {$IFDEF JVCLThemesEnabled}
    property ThemedBackground;
    {$ENDIF JVCLThemesEnabled}
    property OnButtonClick;
    property OnCustomDraw;
    property OnEditButton;
    property OnPageChange;
    property OnPageChanging;
    property OnEditPage;
    property Action;
    property Anchors;
    {$IFDEF VCL}
    property BiDiMode;
    property ParentBiDiMode;
    property DragCursor;
    property DragKind;
    {$ENDIF VCL}
    property BorderStyle;
    property Color;
    property Constraints;
    property Cursor;
    property DragMode;
    property Font;
    property Height;
    property HelpContext;
    //PRY 2002.06.04
    {$IFDEF COMPILER6_UP}
    property HelpKeyword;
    property HelpType;
    {$ENDIF COMPILER6_UP}
    // PRY END
    property Hint;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property Width;
    property OnClick;
    property OnDblClick;
    property OnContextPopup;
  end;

implementation

uses
  Math;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvOutlookBar.res}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvOutlookBar.res}
{$ENDIF LINUX}

const
  cButtonLeftOffset = 4;
  cButtonTopOffset = 2;
  cInitRepeatPause = 400;
  cRepeatPause = 100;

//=== TJvOutlookBarEdit ======================================================

type
  TJvOutlookBarEdit = class(TCustomEdit)
  private
    FCanvas: TControlCanvas;
    {$IFDEF VCL}
    procedure WMNCPaint(var Msg: TMessage); message WM_NCPAINT;
    {$ENDIF VCL}
    procedure EditAccept;
    procedure EditCancel;
    function GetCanvas: TCanvas;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor CreateInternal(AOwner: TComponent; AParent: TWinControl; AObject: TObject);
    destructor Destroy; override;
    procedure ShowEdit(const AText: string; R: TRect);
    property Canvas: TCanvas read GetCanvas;
  end;

constructor TJvOutlookBarEdit.CreateInternal(AOwner: TComponent;
  AParent: TWinControl; AObject: TObject);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  AutoSize := True;
  Visible := False;
  Parent := AParent;
  BorderStyle := bsNone;
  ParentFont := False;
  Tag := Integer(AObject);
end;

destructor TJvOutlookBarEdit.Destroy;
begin
  inherited Destroy;
  // (rom) destroy Canvas AFTER inherited Destroy
  FCanvas.Free;
end;

procedure TJvOutlookBarEdit.EditAccept;
begin
  {$IFDEF VCL}
  Parent.Perform(CM_CAPTION_EDIT_ACCEPT, Integer(Self), Tag);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Perform(Parent, CM_CAPTION_EDIT_ACCEPT, Integer(Self), Tag);
  {$ENDIF VisualCLX}
  Hide;
end;

procedure TJvOutlookBarEdit.EditCancel;
begin
  {$IFDEF VCL}
  Parent.Perform(CM_CAPTION_EDIT_CANCEL, Integer(Self), Tag);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Perform(Parent, CM_CAPTION_EDIT_CANCEL, Integer(Self), Tag);
  {$ENDIF VisualCLX}
  Hide;
end;

function TJvOutlookBarEdit.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

procedure TJvOutlookBarEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      begin
        Key := 0;
        EditAccept;
        if Handle = GetCapture then
          ReleaseCapture;
//      Hide;
//      Free;
//      Screen.Cursor := crDefault;
      end;
    VK_ESCAPE:
      begin
        Key := 0;
        if Handle = GetCapture then
          ReleaseCapture;
        EditCancel;
//      Hide;
//      Free;
//      Screen.Cursor := crDefault;
      end;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TJvOutlookBarEdit.KeyPress(var Key: Char);
begin
  if Key = #13 then
    Key := #0; // remove beep
  inherited KeyPress(Key);
end;

procedure TJvOutlookBarEdit.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if not PtInRect(ClientRect, Point(X, Y)) or ((Button = mbRight) and Visible) then
  begin
    if Handle = GetCapture then
      ReleaseCapture;
    EditCancel;
//    Screen.Cursor := crDefault;
//    FEdit.Hide;
//    FEdit.Free;
//    FEdit := nil;
  end
  else
  begin
    ReleaseCapture;
//    Screen.Cursor := crIBeam;
    SetCapture(Handle);
  end;
end;

procedure TJvOutlookBarEdit.ShowEdit(const AText: string; R: TRect);
begin
  Hide;
  Text := AText;
  SetBounds(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
  Show;
  SetCapture(Handle);
  SelStart := 0;
  SelLength := Length(Text);
  SetFocus;
end;

{$IFDEF VCL}
procedure TJvOutlookBarEdit.WMNCPaint(var Msg: TMessage);
//var
//  DC: HDC;
//  RC, RW: TRect;
begin
  if csDestroying in ComponentState then
    Exit;
  GetCanvas; // make Delphi 5 compiler happy // andreas
  inherited;
(*
  DC := GetWindowDC(Handle);
  try
    FCanvas.Handle := DC;
    Windows.GetClientRect(Handle, RC);
    GetWindowRect(Handle, RW);
    MapWindowPoints(0, Handle, RW, 2);

    OffsetRect(RC, -RW.Left, -RW.Top);
    ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
    OffsetRect(RW, -RW.Left, -RW.Top);

    FCanvas.Brush.Color := clBlack;
    Windows.FrameRect(DC,RW,FCanvas.Brush.Handle);
    InflateRect(RW,-1,-1);

{    FCanvas.Brush.Color := clBlack;
    Windows.FrameRect(DC,RW,FCanvas.Brush.Handle);
    InflateRect(RW,-1,-1);

    FCanvas.Brush.Color := clBlack;
    Windows.FrameRect(DC,RW,FCanvas.Brush.Handle);
    InflateRect(RW,-1,-1); }

    { Erase parts not drawn }
    IntersectClipRect(DC, RW.Left, RW.Top, RW.Right, RW.Bottom);
  finally
    ReleaseDC(Handle, DC);
  end;
  *)
end;
{$ENDIF VCL}

//=== TJvRepeatButton ========================================================

type
  // auto-repeating button using a timer (stolen from Borland's Spin.pas sample component)
  TJvRepeatButton = class(TJvExSpeedButton)
  private
    FRepeatTimer: TTimer;
    procedure TimerExpired(Sender: TObject);
  protected
    procedure VisibleChanged; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    destructor Destroy; override;
  end;

procedure TJvRepeatButton.VisibleChanged;
begin
  inherited VisibleChanged;
  if not Visible then
    FreeAndNil(FRepeatTimer);
end;

destructor TJvRepeatButton.Destroy;
begin
  inherited Destroy;
end;

procedure TJvRepeatButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if FRepeatTimer = nil then
    FRepeatTimer := TTimer.Create(Self);
  FRepeatTimer.OnTimer := TimerExpired;
  FRepeatTimer.Interval := cInitRepeatPause;
  FRepeatTimer.Enabled := True;
end;

procedure TJvRepeatButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FreeAndNil(FRepeatTimer);
end;

procedure TJvRepeatButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := cRepeatPause;
  if (FState = bsDown) and MouseCapture then
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
end;

//=== TJvOutlookBarButton ====================================================

constructor TJvOutlookBarButton.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FEnabled := True;
end;

destructor TJvOutlookBarButton.Destroy;
var
  OBPage: TJvOutlookBarPage;
  OB: TJvOutlookBar;
begin
  OBPage := TJvOutlookBarPage(TJvOutlookBarButtons(Self.Collection).Owner);
  OB := TJvOutlookBar(TJvOutlookBarPages(OBPage.Collection).Owner);
  if Assigned(OB) then
  begin
    if OB.FPressedButtonIndex = Index then
      OB.FPressedButtonIndex := -1;
    if OB.FLastButtonIndex = Index then
      OB.FLastButtonIndex := -1;
    OB.Invalidate;
  end;
  inherited Destroy;
end;

procedure TJvOutlookBarButton.Assign(Source: TPersistent);
begin
  if Source is TJvOutlookBarButton then
  begin
    Caption := TJvOutlookBarButton(Source).Caption;
    ImageIndex := TJvOutlookBarButton(Source).ImageIndex;
    Down := TJvOutlookBarButton(Source).Down;
    AutoToggle := TJvOutlookBarButton(Source).AutoToggle;
    Tag := TJvOutlookBarButton(Source).Tag;
    Enabled := TJvOutlookBarButton(Source).Enabled;
    Change;
  end
  else
    inherited Assign(Source);
end;

procedure TJvOutlookBarButton.Change;
begin
  if (Collection <> nil) and (TJvOutlookBarButtons(Collection).Owner <> nil) and
    (TCollectionItem(TJvOutlookBarButtons(Collection).Owner).Collection <> nil) and
    (TCustomControl(TJvOutlookBarPages(TCollectionItem(TJvOutlookBarButtons(Collection).Owner).Collection).Owner) <> nil) then
    TCustomControl(TJvOutlookBarPages(TCollectionItem(TJvOutlookBarButtons(Collection).Owner).Collection).Owner).Invalidate;
end;

procedure TJvOutlookBarButton.EditCaption;
begin
  SendMessage(TCustomControl(TJvOutlookBarPages(TCollectionItem(TJvOutlookBarButtons(Collection).Owner).Collection).Owner).Handle,
    CM_CAPTION_EDITING, Integer(Self), 0);
end;

function TJvOutlookBarButton.GetDisplayName: string;
begin
  if Caption <> '' then
    Result := Caption
  else
    Result := inherited GetDisplayName;
end;

procedure TJvOutlookBarButton.SetCaption(const Value: TCaption);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Change;
  end;
end;

procedure TJvOutlookBarButton.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Change;
  end;
end;

procedure TJvOutlookBarButton.SetDown(const Value: Boolean);
var
  I: Integer;
begin
  if Value <> FDown then
  begin
    FDown := Value;
    if FDown then
      for I := 0 to TJvOutlookBarButtons(Collection).Count - 1 do
        if TJvOutlookBarButtons(Collection).Items[I] <> Self then
          TJvOutlookBarButtons(Collection).Items[I].Down := False;
    Change;
  end;
end;

procedure TJvOutlookBarButton.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Change;
  end;
end;

//=== TJvOutlookBarButtons ===================================================

constructor TJvOutlookBarButtons.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJvOutlookBarButton);
end;

function TJvOutlookBarButtons.Add: TJvOutlookBarButton;
begin
  Result := TJvOutlookBarButton(inherited Add);
end;

procedure TJvOutlookBarButtons.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TJvOutlookBarButtons then
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to TJvOutlookBarButtons(Source).Count - 1 do
        Add.Assign(TJvOutlookBarButtons(Source)[I]);
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TJvOutlookBarButtons.GetItem(Index: Integer): TJvOutlookBarButton;
begin
  Result := TJvOutlookBarButton(inherited Items[Index]);
end;

{$IFDEF COMPILER5}
function TJvOutlookBarButtons.Owner: TPersistent;
begin
  Result := GetOwner;
end;
{$ENDIF COMPILER5}

function TJvOutlookBarButtons.Insert(Index: Integer): TJvOutlookBarButton;
begin
  Result := TJvOutlookBarButton(inherited Insert(Index));
end;

procedure TJvOutlookBarButtons.SetItem(Index: Integer;
  const Value: TJvOutlookBarButton);
begin
  inherited Items[Index] := Value;
end;

procedure TJvOutlookBarButtons.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Owner <> nil then
    TJvOutlookBarPage(Owner).Changed(False);
end;

//=== TJvOutlookBarPage ======================================================

constructor TJvOutlookBarPage.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FButtons := TJvOutlookBarButtons.Create(Self);
  FFont := TFont.Create;
  FFont.OnChange := DoFontChange;
  FDownFont := TFont.Create;
  FDownFont.OnChange := DoFontChange;
  FParentColor := True;
  FPicture := TPicture.Create;
  FPicture.OnChange := DoPictureChange;
  FAlignment := taCenter;
  FImageIndex := -1;
  FEnabled := True;
  if (Collection <> nil) and (TJvOutlookBarPages(Collection).Owner <> nil) then
  begin
    FButtonSize := TJvCustomOutlookBar(TJvOutlookBarPages(Collection).Owner).ButtonSize;
    FColor := TJvCustomOutlookBar(TJvOutlookBarPages(Collection).Owner).Color;
    Font := TJvCustomOutlookBar(TJvOutlookBarPages(Collection).Owner).Font;
    DownFont := Font;
  end
  else
  begin
    FButtonSize := olbsLarge;
    FColor := clGray;
  end;
  Font.Color := clWhite;
  FParentButtonSize := True;
end;

destructor TJvOutlookBarPage.Destroy;
begin
  FButtons.Free;
  FPicture.Free;
  FFont.Free;
  FDownFont.Free;
  inherited Destroy;
end;

procedure TJvOutlookBarPage.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TJvOutlookBarPage then
  begin
    Caption := TJvOutlookBarPage(Source).Caption;
    Picture := TJvOutlookBarPage(Source).Picture;
    Color := TJvOutlookBarPage(Source).Color;
    DownFont.Assign(TJvOutlookBarPage(Source).DownFont);
    ButtonSize := TJvOutlookBarPage(Source).ButtonSize;
    ParentButtonSize := TJvOutlookBarPage(Source).ParentButtonSize;
    ParentColor := TJvOutlookBarPage(Source).ParentColor;
    Enabled := TJvOutlookBarPage(Source).Enabled;
    Buttons.Clear;
    for I := 0 to TJvOutlookBarPage(Source).Buttons.Count - 1 do
      Buttons.Add.Assign(TJvOutlookBarPage(Source).Buttons[I]);
    Change;
  end
  else
    inherited Assign(Source);
end;

procedure TJvOutlookBarPage.Change;
begin
  if (Collection <> nil) and (TJvOutlookBarPages(Collection).UpdateCount = 0) then
    TJvOutlookBarPages(Collection).Update(Self);
end;

procedure TJvOutlookBarPage.SetTopButtonIndex(const Value: Integer);
begin
  if (FTopButtonIndex <> Value) and (Value >= 0) and (Value < Buttons.Count) then
  begin
    FTopButtonIndex := Value;
    Change;
  end;
end;

procedure TJvOutlookBarPage.SetButtons(const Value: TJvOutlookBarButtons);
begin
  FButtons.Assign(Value);
  Change;
end;

procedure TJvOutlookBarPage.SetCaption(const Value: TCaption);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Change;
  end;
end;

procedure TJvOutlookBarPage.SetButtonSize(const Value: TJvBarButtonSize);
begin
  if FButtonSize <> Value then
  begin
    FButtonSize := Value;
    if not (csReading in TComponent(TJvOutlookBarPages(Collection).Owner).ComponentState) then
      FParentButtonSize := False;
    Change;
  end;
end;

procedure TJvOutlookBarPage.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    FParentColor := False;
    Change;
  end;
end;

procedure TJvOutlookBarPage.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  FParentFont := False;
end;

procedure TJvOutlookBarPage.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Change;
  end;
end;

procedure TJvOutlookBarPage.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TJvOutlookBarPage.SetParentButtonSize(const Value: Boolean);
begin
  if FParentButtonSize <> Value then
  begin
    FParentButtonSize := Value;
    if Value then
    begin
      FButtonSize := (TJvOutlookBarPages(Collection).Owner as TJvCustomOutlookBar).ButtonSize;
      Change;
    end;
  end;
end;

procedure TJvOutlookBarPage.SetParentColor(const Value: Boolean);
begin
  if FParentColor <> Value then
  begin
    FParentColor := Value;
    if Value then
    begin
      FColor := (TJvOutlookBarPages(Collection).Owner as TJvCustomOutlookBar).Color;
      Change;
    end;
  end;
end;

procedure TJvOutlookBarPage.SetParentFont(const Value: Boolean);
begin
  if FParentFont <> Value then
  begin
    if Value then
      Font := (TJvOutlookBarPages(Collection).Owner as TJvCustomOutlookBar).Font;
    FParentFont := Value;
  end;
end;

procedure TJvOutlookBarPage.EditCaption;
begin
  SendMessage(TCustomControl(TJvOutlookBarPages(Collection).Owner).Handle, CM_CAPTION_EDITING, Integer(Self), 1);
end;

function TJvOutlookBarPage.GetDisplayName: string;
begin
  if Caption <> '' then
    Result := Caption
  else
    Result := inherited GetDisplayName;
end;

procedure TJvOutlookBarPage.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Change;
  end;
end;

procedure TJvOutlookBarPage.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Change;
  end;
end;

procedure TJvOutlookBarPage.SetDownFont(const Value: TFont);
begin
  if Value <> FDownFont then
    FDownFont.Assign(Value);
end;

procedure TJvOutlookBarPage.DoFontChange(Sender: TObject);
begin
  Change;
  if Sender <> FDownFont then
    FParentFont := False;
end;

function TJvOutlookBarPage.GetDownButton: TJvOutlookBarButton;
var
  Index: Integer;
begin
  Index := DownIndex;
  if Index <> -1 then
    Result := Buttons[Index]
  else
    Result := nil;
end;

procedure TJvOutlookBarPage.SetDownButton(Value: TJvOutlookBarButton);
begin
  if Value = nil then
    DownIndex := -1
  else
    DownIndex := Value.Index;
end;

function TJvOutlookBarPage.GetDownIndex: Integer;
begin
  for Result := 0 to Buttons.Count - 1 do
    if Buttons[Result].Down then
      Exit;
  Result := -1;
end;

procedure TJvOutlookBarPage.SetDownIndex(Value: Integer);
begin
  if (Value >= 0) and (Value < Buttons.Count) then
    Buttons[Value].Down := True;
end;

//=== TJvOutlookBarPages =====================================================

constructor TJvOutlookBarPages.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJvOutlookBarPage);
end;

{$IFDEF COMPILER5}
function TJvOutlookBarPages.Owner: TPersistent;
begin
  Result := GetOwner;
end;
{$ENDIF COMPILER5}

function TJvOutlookBarPages.Add: TJvOutlookBarPage;
begin
  Result := TJvOutlookBarPage(inherited Add);
end;

procedure TJvOutlookBarPages.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TJvOutlookBarPages then
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to TJvOutlookBarPages(Source).Count - 1 do
        Add.Assign(TJvOutlookBarPages(Source)[I]);
    finally
      EndUpdate
    end;
  end
  else
    inherited Assign(Source);
end;

function TJvOutlookBarPages.GetItem(Index: Integer): TJvOutlookBarPage;
begin
  Result := TJvOutlookBarPage(inherited Items[Index]);
end;

function TJvOutlookBarPages.Insert(Index: Integer): TJvOutlookBarPage;
begin
  Result := TJvOutlookBarPage(inherited Insert(Index));
end;

procedure TJvOutlookBarPages.SetItem(Index: Integer;
  const Value: TJvOutlookBarPage);
begin
  inherited Items[Index] := Value;
end;

procedure TJvOutlookBarPages.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Owner <> nil then
    TJvCustomOutlookBar(Owner).Repaint;
end;

//=== TJvThemedTopBottomButton =================================================

{$IFDEF JVCLThemesEnabled}

type
  TJvThemedTopBottomButton = class(TJvRepeatButton)
  protected
    FIsUpBtn: Boolean;
    procedure WMEraseBkgnd(var Msg: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure Paint; override;
  end;

procedure TJvThemedTopBottomButton.Paint;
var
  Button: TThemedScrollBar;
  Details: TThemedElementDetails;
begin
  if csDestroying in ComponentState then
    Exit;
  if ThemeServices.ThemesEnabled and not Flat then
  begin
    if not Enabled then
      Button := tsArrowBtnUpDisabled
    else
    if FState in [bsDown, bsExclusive] then
      Button := tsArrowBtnUpPressed
    else
    if MouseInControl then
      Button := tsArrowBtnUpHot
    else
      Button := tsArrowBtnUpNormal;

    if not FIsUpBtn then
      Button := TThemedScrollBar(Ord(tsArrowBtnDownNormal) + Ord(Button) - Ord(tsArrowBtnUpNormal));

    Details := ThemeServices.GetElementDetails(Button);

    ThemeServices.DrawElement(Canvas.Handle, Details, ClientRect, nil); //@ClipRect);
  end
  else
    inherited Paint;
end;

procedure TJvThemedTopBottomButton.WMEraseBkgnd(var Msg: TWmEraseBkgnd);
begin
  Msg.Result := 1;
end;

{$ENDIF JVCLThemesEnabled}

//=== TJvCustomOutlookBar ====================================================

constructor TJvCustomOutlookBar.Create(AOwner: TComponent);
var
  Bmp: TBitmap;
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  ControlStyle := ControlStyle - [csAcceptsControls] + [csOpaque];
  IncludeThemeStyle(Self, [csNeedsBorderPaint]);
  Bmp := TBitmap.Create;
  try
    {$IFDEF JVCLThemesEnabled}
    FTopButton := TJvThemedTopBottomButton.Create(Self);
    TJvThemedTopBottomButton(FTopButton).FIsUpBtn := True;
    {$ELSE}
    FTopButton := TJvRepeatButton.Create(Self);
    {$ENDIF JVCLThemesEnabled}
    with FTopButton do
    begin
      Parent := Self;
      Visible := False;
      Transparent := False;
      Bmp.LoadFromResourceName(hInstance, 'UPARROW');
      Glyph := Bmp;
      OnClick := DoUpClick;
      if csDesigning in ComponentState then
        Top := -1000;
    end;

    {$IFDEF JVCLThemesEnabled}
    FBtmButton := TJvThemedTopBottomButton.Create(Self);
    TJvThemedTopBottomButton(FBtmButton).FIsUpBtn := False;
    {$ELSE}
    FBtmButton := TJvRepeatButton.Create(Self);
    {$ENDIF JVCLThemesEnabled}
    with FBtmButton do
    begin
      Parent := Self;
      Visible := False;
      Transparent := False;
      Bmp.LoadFromResourceName(hInstance, 'DWNARROW');
      Glyph := Bmp;
      OnClick := DoDwnClick;
      if csDesigning in ComponentState then
        Top := -1000;
    end;
  finally
    Bmp.Free;
  end;

  FPages := TJvOutlookBarPages.Create(Self);
  FLargeChangeLink := TChangeLink.Create;
  FLargeChangeLink.OnChange := DoChangeLinkChange;
  FSmallChangeLink := TChangeLink.Create;
  FSmallChangeLink.OnChange := DoChangeLinkChange;
  FPageChangeLink := TChangeLink.Create;
  FPageChangeLink.OnChange := DoChangeLinkChange;
  FEdit := TJvOutlookBarEdit.CreateInternal(Self, Self, nil);
  FEdit.Top := -1000;
  // set up defaults
  Width := 100;
  Height := 220;
  Color := clBtnShadow;
  BorderStyle := bsSingle;
  ButtonSize := olbsLarge;
  PageButtonHeight := 19;

  FPressedPageBtn := -1;
  FNextActivePage := -1;
  FLastButtonIndex := -1;
  FPressedButtonIndex := -1;
  {$IFDEF JVCLThemesEnabled}
  FHotPageBtn := -1;
  FThemedBackGround := True;
  {$ENDIF JVCLThemesEnabled}
  ActivePageIndex := 0;
end;

destructor TJvCustomOutlookBar.Destroy;
begin
  FEdit.Free;
  FLargeChangeLink.Free;
  FSmallChangeLink.Free;
  FPageChangeLink.Free;
  FPages.Free;
  inherited Destroy;
end;

procedure TJvCustomOutlookBar.DoDwnClick(Sender: TObject);
begin
  if FBtmButton.Visible then
    with Pages[ActivePageIndex] do
      if TopButtonIndex < Buttons.Count then
        TopButtonIndex := TopButtonIndex + 1;
end;

procedure TJvCustomOutlookBar.DoUpClick(Sender: TObject);
begin
  if FTopButton.Visible then
    with Pages[ActivePageIndex] do
      if TopButtonIndex > 0 then
        TopButtonIndex := TopButtonIndex - 1;
end;

{$IFDEF VCL}
procedure TJvCustomOutlookBar.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array [TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;
{$ENDIF VCL}

procedure TJvCustomOutlookBar.DoChangeLinkChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvCustomOutlookBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FLargeImages then
      LargeImages := nil
    else
    if AComponent = FSmallImages then
      SmallImages := nil
    else
    if AComponent = FPageImages then
      PageImages := nil;
  end;
end;

procedure TJvCustomOutlookBar.DrawPageButton(R: TRect; Index: Integer; Pressed: Boolean);
var
  SavedDC, ATop: Integer;
  SavedColor: TColor;
  Flags: Cardinal;
  HasImage: Boolean;
begin
  ATop := R.Top + 1;
  if Pressed then
  begin
    if BorderStyle = bsNone then
      Frame3D(Canvas, R, clBtnShadow, clBtnHighlight, 1)
    else
    begin
      Frame3D(Canvas, R, cl3DDkShadow, clBtnHighlight, 1);
      Frame3D(Canvas, R, clBtnShadow, clBtnFace, 1);
    end;
  end
  else
  begin
    if BorderStyle = bsNone then
      Frame3D(Canvas, R, clBtnHighlight, clBtnShadow, 1)
    else
    begin
      Frame3D(Canvas, R, clBtnHighlight, cl3DDkShadow, 1);
      Frame3D(Canvas, R, clBtnFace, clBtnShadow, 1);
    end;
  end;
  Flags := DT_CENTER or DT_VCENTER or DT_SINGLELINE;
  HasImage := Assigned(PageImages) and (Pages[Index].ImageIndex >= 0) and (Pages[Index].ImageIndex < PageImages.Count);
  SavedDC := SaveDC(Canvas.Handle);
  try
    case Pages[Index].Alignment of
      taLeftJustify:
        begin
          if HasImage then
          begin
            PageImages.Draw(Canvas, 4, ATop, Pages[Index].ImageIndex, Pages[Index].Enabled);
            Inc(R.Left, PageImages.Width + 8);
          end
          else
            Inc(R.Left, 4);
          Flags := DT_LEFT or DT_VCENTER or DT_SINGLELINE;
        end;
      taCenter:
        if HasImage then
        begin
          PageImages.Draw(Canvas, 4, ATop, Pages[Index].ImageIndex, Pages[Index].Enabled);
          Inc(R.Left, PageImages.Width + 4);
        end;
      taRightJustify:
        begin
          if HasImage then
          begin
            PageImages.Draw(Canvas, 4, ATop, Pages[Index].ImageIndex, Pages[Index].Enabled);
            Inc(R.Left, PageImages.Width + 8);
          end;
          Dec(R.Right, 4);
          Flags := DT_RIGHT or DT_VCENTER or DT_SINGLELINE;
        end;
    end;
  finally
    RestoreDC(Canvas.Handle, SavedDC);
  end;
  SetBkMode(Canvas.Handle, TRANSPARENT);
  OffsetRect(R, 0, -1);
  SavedColor := Canvas.Font.Color;
  try
    if not Pages[Index].Enabled then
    begin
      OffsetRect(R, 1, 1);
      Canvas.Font.Color := clWhite;
      DrawText(Canvas.Handle, PChar(Pages[Index].Caption), -1, R, Flags or DT_END_ELLIPSIS);
      OffsetRect(R, -1, -1);
      Canvas.Font.Color := clGrayText;
    end;
    DrawText(Canvas.Handle, PChar(Pages[Index].Caption), -1, R, Flags or DT_END_ELLIPSIS);
  finally
    Canvas.Font.Color := SavedColor;
  end;
end;

function TJvCustomOutlookBar.DrawTopPages: Integer;
var
  R: TRect;
  I: Integer;
  {$IFDEF JVCLThemesEnabled}
  ToolBar: TThemedToolBar;
  Details: TThemedElementDetails;
  ClipRect: TRect;
  LColor: Cardinal;
  {$ENDIF JVCLThemesEnabled}
begin
  Result := -1;
  if csDestroying in ComponentState then
    Exit;
  R := GetPageButtonRect(0);

  for I := 0 to Pages.Count - 1 do
  begin
    if DoDrawPageButton(R, I, FPressedPageBtn = I) then
    begin
      {$IFDEF JVCLThemesEnabled}
      if ThemeServices.ThemesEnabled then
      begin
        if (FPressedPageBtn = I) or (FHotPageBtn = I) then
          ToolBar := ttbButtonPressed
        else
          ToolBar := ttbButtonHot;
        Details := ThemeServices.GetElementDetails(ToolBar);

        if BorderStyle = bsNone then
        begin
          ClipRect := R;
          InflateRect(R, 1, 1);
          ThemeServices.DrawElement(Canvas.Handle, Details, R, @ClipRect);
          InflateRect(R, -1, -1);
        end
        else
          ThemeServices.DrawElement(Canvas.Handle, Details, R);

        { Determine text color }
        if FPressedPageBtn = I then
          ToolBar := ttbButtonPressed
        else
        if FHotPageBtn = I then
          ToolBar := ttbButtonHot
        else
          ToolBar := ttbButtonNormal;
        Details := ThemeServices.GetElementDetails(ToolBar);

        with Details do
          GetThemeColor(ThemeServices.Theme[Element], Part, State, TMT_TEXTCOLOR, LColor);
        Canvas.Font.Color := LColor;
      end
      else
      {$ENDIF JVCLThemesEnabled}
      begin
        Canvas.Brush.Color := clBtnFace;
        Canvas.FillRect(R);
      end;
      DrawPageButton(R, I, FPressedPageBtn = I);
    end;
    OffsetRect(R, 0, PageButtonHeight);
    if I >= ActivePageIndex then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := Pages.Count - 1;
end;

procedure TJvCustomOutlookBar.DrawButtons(Index: Integer);
var
  I, H: Integer;
  R, R2, R3: TRect;
  C: TColor;
  SavedDC: Integer;
  SavedColor: TColor;
  {$IFDEF JVCLThemesEnabled}
  ThemedColor: Cardinal;
  Details: TThemedElementDetails;
  {$ENDIF JVCLThemesEnabled}
begin
  if csDestroying in ComponentState then
    Exit;
  if (Index < 0) or (Index >= Pages.Count) or (Pages[Index].Buttons = nil) or
    (Pages[Index].Buttons.Count <= 0) then
    Exit;
  R2 := GetPageRect(Index);
  R := GetButtonRect(Index, Pages[Index].TopButtonIndex);
  H := GetButtonHeight(Index);
  C := Canvas.Pen.Color;
  Canvas.Font := Pages[Index].Font;

  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled then
  begin
    Details := ThemeServices.GetElementDetails(ttbButtonNormal);
    with Details do
      GetThemeColor(ThemeServices.Theme[Element], Part, State, TMT_TEXTCOLOR, ThemedColor);
  end;
  {$ENDIF JVCLThemesEnabled}
  try
    Canvas.Brush.Style := bsClear;
    for I := Pages[Index].TopButtonIndex to Pages[Index].Buttons.Count - 1 do
    begin
      Canvas.Font := Pages[Index].Font;
//      Canvas.Rectangle(R);  // DEBUG
      {$IFDEF JVCLThemesEnabled}
      if ThemeServices.ThemesEnabled then
        Canvas.Font.Color := ThemedColor;
      {$ENDIF JVCLThemesEnabled}
      if Pages[Index].Buttons[I].Down then
      begin
        Canvas.Font := Pages[Index].DownFont;
        DrawButtonFrame(Index, I, I);
      end;
      if DoDrawButton(R, I, Pages[Index].Buttons[I].Down, I = FLastButtonIndex) then
        case Pages[Index].ButtonSize of
          olbsLarge:
            begin
              SavedColor := Canvas.Font.Color;
              try
                SavedDC := SaveDC(Canvas.Handle);
                try
                  if LargeImages <> nil then
                    LargeImages.Draw(Canvas, R.Left + ((R.Right - R.Left) - LargeImages.Width) div 2, R.Top + 4,
                      Pages[Index].Buttons[I].ImageIndex, Pages[Index].Enabled and Pages[Index].Buttons[I].Enabled);
                finally
                  RestoreDC(Canvas.Handle, SavedDC);
                end;
                R3 := GetButtonTextRect(ActivePageIndex, I);
                SetBkMode(Canvas.Handle, TRANSPARENT);
                if not Pages[Index].Enabled or not Pages[Index].Buttons[I].Enabled then
                begin
                  if ColorToRGB(Pages[Index].Color) = ColorToRGB(clGrayText) then
                    Canvas.Font.Color := clBtnFace
                  else
                    Canvas.Font.Color := clGrayText;
                end;
                DrawText(Canvas.Handle, PChar(Pages[Index].Buttons[I].Caption), -1, R3,
                  DT_EXPANDTABS or DT_SINGLELINE or DT_CENTER or DT_VCENTER);
              finally
                Canvas.Font.Color := SavedColor;
              end;
            end;
          olbsSmall:
            begin
              SavedColor := Canvas.Font.Color;
              try
                SavedDC := SaveDC(Canvas.Handle);
                try
                  if SmallImages <> nil then
                    SmallImages.Draw(Canvas, R.Left + 2, R.Top + 2, Pages[Index].Buttons[I].ImageIndex, Pages[Index].Enabled and Pages[Index].Buttons[I].Enabled);
                finally
                  RestoreDC(Canvas.Handle, SavedDC);
                end;
                R3 := GetButtonTextRect(ActivePageIndex, I);
                SetBkMode(Canvas.Handle, TRANSPARENT);
                if not Pages[Index].Enabled or not Pages[Index].Buttons[I].Enabled then
                begin
                  if ColorToRGB(Pages[Index].Color) = ColorToRGB(clGrayText) then
                    Canvas.Font.Color := clBtnFace
                  else
                    Canvas.Font.Color := clGrayText;
                end;
                InflateRect(R3, -4, 0);
                DrawText(Canvas.Handle, PChar(Pages[Index].Buttons[I].Caption), -1, R3,
                  DT_EXPANDTABS or DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_NOCLIP or DT_EDITCONTROL);
              finally
                Canvas.Font.Color := SavedColor;
              end;
            end;
        end;
      OffsetRect(R, 0, H);
      if R.Top >= R2.Bottom then
        Break;
    end;
  finally
    Canvas.Font := Self.Font;
    Canvas.Pen.Color := C;
  end;
end;

procedure TJvCustomOutlookBar.DrawArrowButtons(Index: Integer);
var
  R: TRect;
  H: Integer;
begin
  if csDestroying in ComponentState then
    Exit;
  if (Index < 0) or (Index >= Pages.Count) or (Pages[Index].Buttons = nil) or
    (Pages[Index].Buttons.Count <= 0) then
  begin
    TopButton.Visible := False;
    BtmButton.Visible := False;
    Exit;
  end;
  R := GetPageRect(Index);
  H := GetButtonHeight(Index);
  TopButton.Visible := (Pages.Count > 0) and (R.Top < R.Bottom - 20) and (Pages[Index].TopButtonIndex > 0);
  BtmButton.Visible := (Pages.Count > 0) and (R.Top < R.Bottom - 20) and
    (R.Bottom - R.Top < (Pages[Index].Buttons.Count - Pages[Index].TopButtonIndex) * H);
  // remove the last - H to show arrow
  // button when the bottom of the last button is beneath the edge
  if TopButton.Visible then
    TopButton.SetBounds(ClientWidth - 20, R.Top + 4, 16, 16)
  else
  if csDesigning in ComponentState then
    TopButton.Top := -1000;
  if BtmButton.Visible then
    BtmButton.SetBounds(ClientWidth - 20, R.Bottom - 20, 16, 16)
  else
  if csDesigning in ComponentState then
    BtmButton.Top := -1000;
  TopButton.Enabled := TopButton.Visible and Pages[Index].Enabled;
  BtmButton.Enabled := BtmButton.Visible and Pages[Index].Enabled;
end;

function TJvCustomOutlookBar.DrawPicture(R: TRect; Picture: TPicture): Boolean;
var
  Bmp: TBitmap;
begin
  Result := Assigned(Picture) and Assigned(Picture.Graphic) and not Picture.Graphic.Empty;
  if csDestroying in ComponentState then
    Exit;
  if Result then
  begin
    Bmp := TBitmap.Create;
    try
      Bmp.Assign(Picture.Graphic);
      Canvas.Brush.Bitmap := Bmp;
      Canvas.FillRect(R);
      Canvas.Brush.Bitmap := nil;
    finally
      Bmp.Free;
    end;
  end;
end;

procedure TJvCustomOutlookBar.DrawCurrentPage(PageIndex: Integer);
var
  R: TRect;
  AColor: TColor;
begin
  if csDestroying in ComponentState then
    Exit;
  if (PageIndex < 0) or (PageIndex >= Pages.Count) or (Pages[PageIndex].Buttons = nil) then
    Exit;
  R := GetPageRect(PageIndex);
  AColor := Canvas.Brush.Color;
  try
    Canvas.Brush.Color := Pages[PageIndex].Color;
    Canvas.Font := Self.Font;
    if DoDrawPage(R, PageIndex) then
    begin
      if not DrawPicture(R, Pages[PageIndex].Picture) then
      begin
        {$IFDEF JVCLThemesEnabled}
        if not ThemedBackground or not ThemeServices.ThemesEnabled then
        {$ENDIF JVCLThemesEnabled}
          Canvas.FillRect(R);
      end;
    end;
    DrawButtonFrame(ActivePageIndex, FLastButtonIndex, FPressedButtonIndex);
    DrawButtons(PageIndex);
  finally
    Canvas.Brush.Color := AColor;
    Canvas.Brush.Style := bsClear;
    SetBkMode(Canvas.Handle, TRANSPARENT);
  end;
  DrawArrowButtons(PageIndex);
end;

procedure TJvCustomOutlookBar.DrawBottomPages(StartIndex: Integer);
var
  R: TRect;
  I: Integer;
  {$IFDEF JVCLThemesEnabled}
  Details: TThemedElementDetails;
  ClipRect: TRect;
  ToolBar: TThemedToolBar;
  LColor: Cardinal;
  {$ENDIF JVCLThemesEnabled}
begin
  if csDestroying in ComponentState then
    Exit;
  R := GetPageButtonRect(Pages.Count - 1);
  for I := Pages.Count - 1 downto StartIndex do
  begin
    if DoDrawPageButton(R, I, FPressedPageBtn = I) then
    begin
      {$IFDEF JVCLThemesEnabled}
      if ThemeServices.ThemesEnabled then
      begin
        if (FPressedPageBtn = I) or (FHotPageBtn = I) then
          ToolBar := ttbButtonPressed
        else
          ToolBar := ttbButtonHot;
        Details := ThemeServices.GetElementDetails(ToolBar);

        if BorderStyle = bsNone then
        begin
          ClipRect := R;
          InflateRect(R, 1, 1);
          ThemeServices.DrawElement(Canvas.Handle, Details, R, @ClipRect);
          InflateRect(R, -1, -1);
        end
        else
          ThemeServices.DrawElement(Canvas.Handle, Details, R);

        { Determine text color }
        if FPressedPageBtn = I then
          ToolBar := ttbButtonPressed
        else
        if FHotPageBtn = I then
          ToolBar := ttbButtonHot
        else
          ToolBar := ttbButtonNormal;
        Details := ThemeServices.GetElementDetails(ToolBar);

        with Details do
          GetThemeColor(ThemeServices.Theme[Element], Part, State, TMT_TEXTCOLOR, LColor);
        Canvas.Font.Color := LColor;
      end
      else
      {$ENDIF JVCLThemesEnabled}
      begin
        Canvas.Brush.Color := clBtnFace;
        Canvas.FillRect(R);
      end;
      DrawPageButton(R, I, FPressedPageBtn = I);
    end;
    OffsetRect(R, 0, -PageButtonHeight);
  end;
end;

function TJvCustomOutlookBar.GetPageButtonAtPos(P: TPoint): TJvOutlookBarPage;
var
  I: Integer;
begin
  // TODO: rewrite more optimal (no loop)
  for I := 0 to Pages.Count - 1 do
  begin
    if PtInRect(GetPageButtonRect(I), P) then
    begin
      Result := Pages[I];
      Exit;
    end;
  end;
  Result := nil;
end;

function TJvCustomOutlookBar.GetPageButtonRect(Index: Integer): TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if (Index < 0) or (Index >= Pages.Count) then
    Exit;
  Result := Rect(0, 0, ClientWidth, PageButtonHeight);
  if Index <= ActivePageIndex then
    OffsetRect(Result, 0, PageButtonHeight * Index)
  else
    OffsetRect(Result, 0, (ClientHeight - PageButtonHeight * (Pages.Count - Index)));
end;

function TJvCustomOutlookBar.GetPageTextRect(Index: Integer): TRect;
begin
  Result := GetPageButtonRect(Index);
  InflateRect(Result, -2, -2);
end;

function TJvCustomOutlookBar.GetPageRect(Index: Integer): TRect;
begin
  if (Index < 0) or (Index >= Pages.Count) then
    Result := Rect(0, 0, 0, 0)
  else
    Result := Rect(0, PageButtonHeight * Index + PageButtonHeight, ClientWidth, ClientHeight - (Pages.Count - Index) *
      PageButtonHeight + PageButtonHeight);
end;

function TJvCustomOutlookBar.GetButtonAtPos(P: TPoint): TJvOutlookBarButton;
var
  I, H: Integer;
  R, B: TRect;
begin
  // this always returns the button in the visible part of the active page (if any)
  Result := nil;
  if (ActivePageIndex < 0) or (ActivePageIndex >= Pages.Count) then
    Exit;
  B := GetButtonRect(ActivePageIndex, 0);
  H := GetButtonHeight(ActivePageIndex);
  R := GetPageRect(ActivePageIndex);
  for I := 0 to Pages[ActivePageIndex].Buttons.Count - 1 do
  begin
    if PtInRect(B, P) then
    begin
      Result := Pages[ActivePageIndex].Buttons[I];
      Exit;
    end;
    OffsetRect(B, 0, H);
    if B.Top >= R.Bottom then
      Break;
  end;
end;

function TJvCustomOutlookBar.GetButtonRect(PageIndex, ButtonIndex: Integer): TRect;
var
  H: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if (PageIndex < 0) or (PageIndex >= Pages.Count) or
    (ButtonIndex < 0) or (ButtonIndex >= Pages[PageIndex].Buttons.Count) then
    Exit;
  H := GetButtonHeight(PageIndex);
  case Pages[PageIndex].ButtonSize of
    olbsLarge:
      if LargeImages <> nil then
      begin
        Result := Rect(0, 0, Max(LargeImages.Width, Canvas.TextWidth(Pages[PageIndex].Buttons[ButtonIndex].Caption)) +
          4, H);
        OffsetRect(Result, (ClientWidth - (Result.Right - Result.Left)) div 2, cButtonTopOffset);
      end
      else
        Result := Rect(0, 0, ClientWidth, cButtonTopOffset + H);
    olbsSmall:
      if SmallImages <> nil then
      begin
        Result := Rect(0, 0, SmallImages.Width + Canvas.TextWidth(Pages[PageIndex].Buttons[ButtonIndex].Caption) + 8,
          H);
        OffsetRect(Result, cButtonLeftOffset, cButtonTopOffset);
      end
      else
        Result := Rect(0, 0, ClientWidth, cButtonTopOffset + H);
  end;
  OffsetRect(Result, 0, (ButtonIndex - Pages[PageIndex].TopButtonIndex) * H + GetPageRect(PageIndex).Top);
end;

function TJvCustomOutlookBar.GetButtonFrameRect(PageIndex, ButtonIndex: Integer): TRect;
var
  H: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if (PageIndex < 0) or (PageIndex >= Pages.Count) or
    (ButtonIndex < 0) or (ButtonIndex >= Pages[PageIndex].Buttons.Count) then
    Exit;
  H := GetButtonHeight(PageIndex);
  case Pages[PageIndex].ButtonSize of
    olbsLarge:
      if LargeImages <> nil then
      begin
        Result := Rect(0, 0, LargeImages.Width + 6, LargeImages.Height + 6);
        OffsetRect(Result, (ClientWidth - (Result.Right - Result.Left)) div 2,
          cButtonTopOffset + (ButtonIndex - Pages[PageIndex].TopButtonIndex) * H + GetPageRect(PageIndex).Top + 1);
      end
      else
      begin
        Result := Rect(0, 0, ClientWidth, H);
        OffsetRect(Result, 0,
          cButtonTopOffset + (ButtonIndex - Pages[PageIndex].TopButtonIndex) * H + GetPageRect(PageIndex).Top + 1);
      end;
    olbsSmall:
      if SmallImages <> nil then
      begin
        Result := Rect(0, 0, SmallImages.Width + 4, SmallImages.Height + 4);
        OffsetRect(Result, cButtonLeftOffset, cButtonTopOffset + (ButtonIndex - Pages[PageIndex].TopButtonIndex) * H +
          GetPageRect(PageIndex).Top);
      end
      else
      begin
        Result := Rect(0, 0, ClientWidth, H);
        OffsetRect(Result, 0, cButtonTopOffset + (ButtonIndex - Pages[PageIndex].TopButtonIndex) * H +
          GetPageRect(PageIndex).Top);
      end;
  end;
end;

function TJvCustomOutlookBar.GetButtonTextRect(PageIndex,
  ButtonIndex: Integer): TRect;
var
  H: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if Pages[PageIndex].Buttons.Count <= ButtonIndex then
    Exit;
  Result := GetButtonRect(PageIndex, ButtonIndex);
  H := GetButtonHeight(PageIndex);
  case Pages[PageIndex].ButtonSize of
    olbsLarge:
      if LargeImages <> nil then
      begin
        Result.Top := Result.Bottom + Pages[PageIndex].Font.Height - 2;
        OffsetRect(Result, 0, -4);
      end;
    olbsSmall:
      if SmallImages <> nil then
      begin
        Result.Left := SmallImages.Width + 10;
        Result.Top := Result.Top + (GetButtonHeight(PageIndex) + Pages[PageIndex].Font.Height) div 2;
        Result.Bottom := Result.Top - Pages[PageIndex].Font.Height + 2;
        Result.Right := Result.Left + Canvas.TextWidth(Pages[PageIndex].Buttons[ButtonIndex].Caption) + 4;
        OffsetRect(Result, 0, -(H - (Result.Bottom - Result.Top)) div 4);
      end;
  end;
end;

procedure TJvCustomOutlookBar.Paint;
var
  I: Integer;
  {$IFDEF JVCLThemesEnabled}
  Details: TThemedElementDetails;
  R, ClipRect: TRect;
  {$ENDIF JVCLThemesEnabled}
begin
  if csDestroying in ComponentState then
    Exit;
  Canvas.Font := Self.Font;
  Canvas.Brush.Color := Self.Color;
  if Pages.Count = 0 then // we only need to draw the background when there are no pages
  begin
    {$IFDEF JVCLThemesEnabled}
    if ThemedBackground and ThemeServices.ThemesEnabled then
    begin
      R := ClientRect;
      ClipRect := R;
      InflateRect(R, 1, 0);
      Details := ThemeServices.GetElementDetails(ttbButtonHot);
      ThemeServices.DrawElement(Canvas.Handle, Details, R, @ClipRect);
    end
    else
    {$ENDIF JVCLThemesEnabled}
    begin
      if DoDrawBackGround then
        Canvas.FillRect(ClientRect);
    end;
  end;
  SetBkMode(Canvas.Handle, TRANSPARENT);
  I := DrawTopPages;
  if I >= 0 then
    DrawCurrentPage(I);
  DrawBottomPages(I + 1);
end;

function TJvCustomOutlookBar.DoPageChanging(Index: Integer): Boolean;
begin
  Result := True;
  if (Index > -1) and Assigned(FOnPageChanging) then
    FOnPageChanging(Self, Index, Result);
end;

procedure TJvCustomOutlookBar.DoPageChange(Index: Integer);
begin
  if (Index > -1) and Assigned(FOnPageChange) then
    FOnPageChange(Self, Index);
end;

procedure TJvCustomOutlookBar.DoButtonClick(Index: Integer);
begin
  if (Index > -1) then
  begin
    with ActivePage.Buttons[Index] do
      if AutoToggle then
        Down := not Down;
    if Assigned(FOnButtonClick) then
      FOnButtonClick(Self, Index);
  end;
end;

procedure TJvCustomOutlookBar.SetActivePageIndex(const Value: Integer);
begin
  if (Value >= 0) and (Value < FPages.Count) then
  begin
    FPressedPageBtn := -1; // reset cache
    // remove old button info
    FLastButtonIndex := -1;
    FPressedButtonIndex := -1;
    FButtonRect := Rect(0, 0, 0, 0);
    if FActivePageIndex <> Value then
    begin
      if not DoPageChanging(Value) then
        Exit;
      FActivePageIndex := Value;
      DoPageChange(Value);
    end;
    Invalidate;
  end;
end;

procedure TJvCustomOutlookBar.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    {$IFDEF VCL}
    RecreateWnd;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    RecreateWidget;
    {$ENDIF VisualCLX}
  end;
end;

procedure TJvCustomOutlookBar.SetButtonSize(const Value: TJvBarButtonSize);
var
  I: Integer;
begin
  FButtonSize := Value;
  Pages.BeginUpdate;
  try
    for I := 0 to Pages.Count - 1 do
      if Pages[I].ParentButtonSize then
      begin
        Pages[I].ParentButtonSize := False;
        Pages[I].ParentButtonSize := True; // reset flag
      end;
  finally
    Pages.EndUpdate; // calls invalidate
  end;
end;

procedure TJvCustomOutlookBar.SetLargeImages(const Value: TCustomImageList);
begin
  if FLargeImages <> Value then
  begin
    if Assigned(FLargeImages) then
      FLargeImages.UnRegisterChanges(FLargeChangeLink);
    FLargeImages := Value;
    if Assigned(FLargeImages) then
      FLargeImages.RegisterChanges(FLargeChangeLink);
    Invalidate;
  end;
end;

procedure TJvCustomOutlookBar.SetPageButtonHeight(const Value: Integer);
begin
  if FPageButtonHeight <> Value then
  begin
    FPageButtonHeight := Value;
    Invalidate;
  end;
end;

procedure TJvCustomOutlookBar.SetPages(const Value: TJvOutlookBarPages);
begin
  FPages.Assign(Value); // Assign calls Invalidate
end;

procedure TJvCustomOutlookBar.SetSmallImages(const Value: TCustomImageList);
begin
  if FSmallImages <> Value then
  begin
    if Assigned(FSmallImages) then
      FSmallImages.UnRegisterChanges(FSmallChangeLink);
    FSmallImages := Value;
    if Assigned(FSmallImages) then
      FSmallImages.RegisterChanges(FSmallChangeLink);
    Invalidate;
  end;
end;

procedure TJvCustomOutlookBar.DrawButtonFrame(PageIndex, ButtonIndex, PressedIndex: Integer);
var
  R: TRect;
  {$IFDEF JVCLThemesEnabled}
  Details: TThemedElementDetails;
  {$ENDIF JVCLThemesEnabled}
begin
  if csDestroying in ComponentState then
    Exit;
  if (ButtonIndex < 0) or (PageIndex < 0) or (PageIndex >= Pages.Count) or
    (ButtonIndex < Pages[PageIndex].TopButtonIndex) then
    Exit;
  R := GetButtonFrameRect(PageIndex, ButtonIndex);
  if DoDrawButtonFrame(R, ButtonIndex, (PressedIndex = ButtonIndex) or Pages[PageIndex].Buttons[ButtonIndex].Down, True) then
  begin
    {$IFDEF JVCLThemesEnabled}
    if ThemeServices.ThemesEnabled then
    begin
      if (PressedIndex = ButtonIndex) or (Pages[PageIndex].Buttons[ButtonIndex].Down) then
        Details := ThemeServices.GetElementDetails(ttbButtonPressed)
      else
        Details := ThemeServices.GetElementDetails(ttbButtonHot);
      ThemeServices.DrawElement(Canvas.Handle, Details, R);
    end
    else
    {$ENDIF JVCLThemesEnabled}
    begin
      if (PressedIndex = ButtonIndex) or (Pages[PageIndex].Buttons[ButtonIndex].Down) then
        Frame3D(Canvas, R, clBlack, clWhite, 1)
      else
        Frame3D(Canvas, R, clWhite, clBlack, 1);
    end;
  end;
end;

procedure TJvCustomOutlookBar.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TJvOutlookBarPage;
  B: TJvOutlookBarButton;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbRight then
    Exit;
  P := GetPageButtonAtPos(Point(X, Y));
  if (P <> nil) and (P.Enabled) and (P.Index <> FNextActivePage) then
  begin
    FNextActivePage := P.Index;
    if FNextActivePage <> ActivePageIndex then
    begin // draw button pressed
      FPressedPageBtn := FNextActivePage;
      RedrawRect(GetPageButtonRect(FNextActivePage));
    end;
    Exit;
  end
  else
  begin
    if (FNextActivePage > -1) and Pages[FNextActivePage].Enabled then
      RedrawRect(GetPageButtonRect(FNextActivePage));
    FNextActivePage := -1;
    FPressedPageBtn := -1;
  end;
  B := GetButtonAtPos(Point(X, Y));
  if (B <> nil) and B.Enabled and (Pages[ActivePageIndex].Enabled) then
  begin
    FLastButtonIndex := B.Index;
    FPressedButtonIndex := B.Index;
    FButtonRect := GetButtonFrameRect(ActivePageIndex, B.Index);
    RedrawRect(FButtonRect);
  end;
end;

procedure TJvCustomOutlookBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TJvOutlookBarPage;
  B: TJvOutlookBarButton;
  R: TRect;
begin
  inherited MouseMove(Shift, X, Y);
  { TODO -oJv :
    1. check whether the mouse is down on a page button and whether the mouse has moved from
    the currently pressed page button }
  P := GetPageButtonAtPos(Point(X, Y));
  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled then
  begin
    if ((P = nil) and (FHotPageBtn >= 0)) or (Assigned(P) and (P.Index <> FHotPageBtn)) then
    begin
      if FHotPageBtn >= 0 then
      begin
        R := GetPageButtonRect(FHotPageBtn);
        RedrawRect(R);
      end;
      if Assigned(P) then
        FHotPageBtn := P.Index
      else
        FHotPageBtn := -1;
      if FHotPageBtn >= 0 then
      begin
        R := GetPageButtonRect(FHotPageBtn);
        RedrawRect(R);
      end;
    end;
  end;
  {$ENDIF JVCLThemesEnabled}

  if FPressedPageBtn > -1 then
  begin
    if (P = nil) or (P.Index <> FPressedPageBtn) then
    begin
      R := GetPageButtonRect(FPressedPageBtn);
      RedrawRect(R);
      FPressedPageBtn := -1;
    end;
  end
  else
  if (P <> nil) and (P.Index <> ActivePageIndex) and P.Enabled then
  begin
    if P.Index = FNextActivePage then
    begin
      FPressedPageBtn := FNextActivePage;
      RedrawRect(GetPageButtonRect(FPressedPageBtn));
      Exit;
    end;
  end;
  // TODO: check for button highlight
  B := GetButtonAtPos(Point(X, Y));
  if (B <> nil) and B.Enabled and (Pages[ActivePageIndex].Enabled) then
  begin
    if B.Index <> FLastButtonIndex then
    begin
      RedrawRect(FButtonRect, True);
      FButtonRect := GetButtonFrameRect(ActivePageIndex, B.Index);
      RedrawRect(FButtonRect);
      FLastButtonIndex := B.Index;
    end;
  end
  else
  begin
    if FLastButtonIndex > -1 then
      RedrawRect(FButtonRect);
    FLastButtonIndex := -1;
    FButtonRect := Rect(0, 0, 0, 0);
  end;
end;

procedure TJvCustomOutlookBar.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TJvOutlookBarPage;
  B: TJvOutlookBarButton;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbRight then
    Exit;
  if (FNextActivePage > -1) and (FNextActivePage <> ActivePageIndex) then
  begin
    P := GetPageButtonAtPos(Point(X, Y));
    if (P <> nil) and (P.Index = FNextActivePage) then
      ActivePageIndex := FNextActivePage;
  end;
  FNextActivePage := -1;

  B := GetButtonAtPos(Point(X, Y));
  if B <> nil then
  begin
    if B.Index = FPressedButtonIndex then
      DoButtonClick(FPressedButtonIndex);
    FLastButtonIndex := B.Index;
    FPressedButtonIndex := -1;
    FButtonRect := GetButtonFrameRect(ActivePageIndex, FLastButtonIndex);
    RedrawRect(FButtonRect);
  end
  else
  begin
    FButtonRect := GetButtonFrameRect(ActivePageIndex, FLastButtonIndex);
    FLastButtonIndex := -1;
    FPressedButtonIndex := -1;
    RedrawRect(FButtonRect);
  end;
end;

procedure TJvCustomOutlookBar.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  RedrawRect(FButtonRect);
  inherited MouseEnter(Control);
end;

procedure TJvCustomOutlookBar.MouseLeave(Control: TControl);
{$IFDEF JVCLThemesEnabled}
var
  R: TRect;
{$ENDIF JVCLThemesEnabled}
begin
  if csDesigning in ComponentState then
    Exit;
  inherited MouseLeave(Control);
  RedrawRect(FButtonRect);
  FPressedPageBtn := -1;
  FLastButtonIndex := -1;
  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled and (FHotPageBtn >= 0) then
  begin
    R := GetPageButtonRect(FHotPageBtn);
    RedrawRect(R);
    FHotPageBtn := -1;
  end;
  {$ENDIF JVCLThemesEnabled}
end;

function TJvCustomOutlookBar.GetButtonHeight(PageIndex: Integer): Integer;
const
  cLargeOffset = 8;
  cSmallOffset = 4;
var
  TM: TTextMetric;
begin
  GetTextMetrics(Canvas.Handle, TM);
  Result := TM.tmHeight + TM.tmExternalLeading;
  if (PageIndex >= 0) and (PageIndex < Pages.Count) then
  begin
    case Pages[PageIndex].ButtonSize of
      olbsLarge:
        if LargeImages <> nil then
          Result := Max(Result, LargeImages.Height - Pages[PageIndex].Font.Height + cLargeOffset)
        else
          Result := Abs(Pages[PageIndex].Font.Height) + cLargeOffset;
      olbsSmall:
        if SmallImages <> nil then
          Result := Max(SmallImages.Height, -Pages[PageIndex].Font.Height) + cSmallOffset
        else
          Result := Abs(Pages[PageIndex].Font.Height) + cSmallOffset;
    end;
  end;
  Inc(Result, 4);
end;

function TJvCustomOutlookBar.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  // don't redraw background: we always fill it anyway
  Result := True;
end;

procedure TJvCustomOutlookBar.RedrawRect(R: TRect; Erase: Boolean = False);
begin
  {$IFDEF VisualCLX} QWindows.{$ENDIF}InvalidateRect(Handle, @R, Erase);
end;

procedure TJvCustomOutlookBar.CMCaptionEditing(var Msg: TMessage);
var
  R: TRect;
  B: TJvOutlookBarButton;
  P: TJvOutlookBarPage;
begin
  TJvOutlookBarEdit(FEdit).Tag := Msg.wParam;
//  TJvOutlookBarEdit(FEdit).Font.Name := Pages[ActivePageIndex].Font.Name;
//  TJvOutlookBarEdit(FEdit).Font.Size := Pages[ActivePageIndex].Font.Size;
  case Msg.lParam of
    0: // button
      begin
        B := TJvOutlookBarButton(Msg.wParam);
        R := GetButtonTextRect(ActivePageIndex, B.Index);
        R.Left := Max(R.Left, 0);
        R.Right := Min(R.Right, ClientWidth);
        TJvOutlookBarEdit(FEdit).ShowEdit(B.Caption, R);
      end;
    1: // page
      begin
        P := TJvOutlookBarPage(Msg.wParam);
        R := GetPageTextRect(P.Index);
        TJvOutlookBarEdit(FEdit).ShowEdit(P.Caption, R);
      end;
  end;
end;

procedure TJvCustomOutlookBar.DoContextPopup({$IFDEF VisualCLX} const {$ENDIF} MousePos: TPoint;
  var Handled: Boolean);
var
  P: TPersistent;
begin
  P := GetPageButtonAtPos(MousePos);
  if Assigned(P) then
    PopUpObject := P
  else
  begin
    P := GetButtonAtPos(MousePos);
    if Assigned(P) then
      PopUpObject := P;
  end;
  if P = nil then
    PopUpObject := Self;
  inherited DoContextPopup(MousePos, Handled);
end;

procedure TJvCustomOutlookBar.DoButtonEdit(NewText: string; B: TJvOutlookBarButton);
var
  Allow: Boolean;
begin
  Allow := True;
  if Assigned(FOnEditButton) then
    FOnEditButton(Self, NewText, B.Index, Allow);
  if Allow then
    B.Caption := NewText;
end;

procedure TJvCustomOutlookBar.DoPageEdit(NewText: string; P: TJvOutlookBarPage);
var
  Allow: Boolean;
begin
  Allow := True;
  if Assigned(FOnEditPage) then
    FOnEditPage(Self, NewText, P.Index, Allow);
  if Allow then
    P.Caption := NewText;
end;

procedure TJvCustomOutlookBar.CMCaptionEditAccept(var Msg: TMessage);
begin
  with Msg do
  begin
    if TObject(LParam) is TJvOutlookBarButton then
      DoButtonEdit(TJvOutlookBarEdit(WParam).Text, TJvOutlookBarButton(LParam))
    else
    if TObject(LParam) is TJvOutlookBarPage then
      DoPageEdit(TJvOutlookBarEdit(WParam).Text, TJvOutlookBarPage(LParam));
  end;
end;

procedure TJvCustomOutlookBar.CMCaptionEditCancel(var Msg: TMessage);
begin
{  with Msg do
  begin
    if TObject(LParam) is TJvOutlookBarButton then
      DoButtonEditCancel(TJvOutlookBarButton(LParam))
    else TObject(LParam) is TJvOutlookBarPage then
      DoPageEditCancel(TJvOutlookBarPage(LParam));
  end;
  }
end;

function TJvCustomOutlookBar.GetActivePage: TJvOutlookBarPage;
begin
  if (ActivePageIndex > -1) and (ActivePageIndex < Pages.Count) then
    Result := Pages[ActivePageIndex]
  else
    Result := nil;
end;

function TJvCustomOutlookBar.GetActivePageIndex: Integer;
begin
  if (FActivePageIndex < 0) or (FActivePageIndex >= FPages.Count) then
    FActivePageIndex := 0;
  Result := FActivePageIndex;
end;

{$IFDEF JVCLThemesEnabled}
procedure TJvCustomOutlookBar.SetThemedBackground(const Value: Boolean);
begin
  if Value <> FThemedBackGround then
  begin
    FThemedBackGround := Value;
    if ([csDesigning, csLoading] * ComponentState = []) and ThemeServices.ThemesEnabled then
      Repaint;
  end;
end;
{$ENDIF JVCLThemesEnabled}

procedure TJvCustomOutlookBar.ColorChanged;
var
  I: Integer;
begin
  inherited ColorChanged;
  for I := 0 to Pages.Count - 1 do
    if Pages[I].ParentColor then
    begin
      Pages[I].ParentColor := False;
      Pages[I].ParentColor := True; // reset flag
    end;
end;

procedure TJvCustomOutlookBar.FontChanged;
var
  I: Integer;
begin
  inherited FontChanged;
  for I := 0 to Pages.Count - 1 do
    if Pages[I].ParentFont then
    begin //set the font of the buttons as well
      Pages[I].ParentFont := False;
      Pages[I].Font := Self.Font;
      Pages[I].ParentFont := True; // reset flag
    end;
end;


{$IFDEF VCL}
procedure TJvCustomOutlookBar.CMDialogChar(var Message: TCMDialogChar);
var
  I: Integer;
begin
  if CanFocus then
  begin
  // first check the buttons on the active page, then check the pages
    if (ActivePage <> nil) and (ActivePage.Enabled) then
    begin
      for I := 0 to ActivePage.Buttons.Count - 1 do
        if ActivePage.Buttons[I].Enabled and IsAccel(Message.CharCode, ActivePage.Buttons[I].Caption) then
        begin
          Message.Result := 1;
          DoButtonClick(I);
          Exit;
        end;
    end;

    for I := 0 to Pages.Count - 1 do
      if Pages[I].Enabled and IsAccel(Message.CharCode, Pages[I].Caption) then
      begin
        Message.Result := 1;
        ActivePageIndex := I;
        Exit;
      end;
  end;
  inherited;
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
function TJvCustomOutlookBar.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
var
  I:Integer;
begin
  if CanFocus and (ActivePage <> nil) then
  begin
    for I := 0 to ActivePage.Buttons.Count - 1 do
      if IsAccel(Key, ActivePage.Buttons[I].Caption) then
      begin
        Result := True;
        DoButtonClick(I);
        Exit;
      end;
  end;
  Result := inherited WantKey(Key, Shift, KeyText);
end;
{$ENDIF VisualCLX}

function TJvCustomOutlookBar.DoCustomDraw(ARect: TRect; Stage: TJvOutlookBarCustomDrawStage;
  Index: Integer; Down, Inside: Boolean): Boolean;
begin
  Result := True;
  if Assigned(FOnCustomDraw) then
    FOnCustomDraw(Self, Canvas, ARect, Stage, Index, Down, Inside, Result);
end;

function TJvCustomOutlookBar.DoDrawBackGround: Boolean;
begin
  Result := DoCustomDraw(ClientRect, odsBackground, -1, False, False);
end;

function TJvCustomOutlookBar.DoDrawButton(ARect: TRect; Index: Integer; Down, Inside: Boolean): Boolean;
begin
  Result := DoCustomDraw(ARect, odsButton, Index, Down, Inside);
end;

function TJvCustomOutlookBar.DoDrawButtonFrame(ARect: TRect; Index: Integer;
  Down, Inside: Boolean): Boolean;
begin
  Result := DoCustomDraw(ARect, odsButtonFrame, Index, Down, Inside);
end;

function TJvCustomOutlookBar.DoDrawPage(ARect: TRect; Index: Integer): Boolean;
begin
  Result := DoCustomDraw(ARect, odsPage, Index, False, Index = ActivePageIndex);
end;

function TJvCustomOutlookBar.DoDrawPageButton(ARect: TRect; Index: Integer; Down: Boolean): Boolean;
begin
  Result := DoCustomDraw(ARect, odsPageButton, Index, Down, Index = ActivePageIndex);
end;

procedure TJvOutlookBarPage.DoPictureChange(Sender: TObject);
begin
  Change;
end;

procedure TJvCustomOutlookBar.SetPageImages(const Value: TCustomImageList);
begin
  if FPageImages <> Value then
  begin
    if Assigned(FPageImages) then
      FPageImages.UnRegisterChanges(FPageChangeLink);
    FPageImages := Value;
    if Assigned(FPageImages) then
      FPageImages.RegisterChanges(FPageChangeLink);
    Invalidate;
  end;
end;

end.

