{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSpeedBar.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQSpeedbar;

interface

uses
  SysUtils, Classes, IniFiles,
  
  
  QMenus, QButtons, QControls, QWindows, QGraphics, Types,
  QForms, QImgList, QActnList, QExtCtrls, QGrids, QTypes,
  
  
  RTLConsts,
  
  JvQSpeedButton, JvQAppStorage, JvQConsts, JvQTypes, JvQFormPlacement,
  JvQComponent, JvQThemes, JvQExControls;

const
  DefButtonWidth = 24;
  DefButtonHeight = 23;

type
  TJvSpeedItem = class;
  TJvSpeedBarSection = class;
  EJvSpeedbarError = class(EJVCLException);

  TBarOrientation = (boHorizontal, boVertical);
  TBarPosition = (bpAuto, bpCustom);
  TJvSpeedBarOption = (sbAllowDrag, sbAllowResize, sbFlatBtns, sbGrayedBtns,
    sbTransparentBtns, sbStretchBitmap);
  TJvSpeedBarOptions = set of TJvSpeedBarOption;
  TBoundLine = (blTop, blBottom, blLeft, blRight);
  TBoundLines = set of TBoundLine;
  TSbScaleFlags = set of (sfOffsetX, sfOffsetY, sfBtnSizeX, sfBtnSizeY);
  TForEachItem = procedure(Item: TJvSpeedItem; Data: Longint) of object;
  TApplyAlignEvent = procedure(Sender: TObject; Align: TAlign;
    var Apply: Boolean) of object;

  TJvSpeedBar = class(TJvCustomPanel, IJvDenySubClassing)
  private
    FSections: TList;
    FPosition: TBarPosition;
    FOrientation: TBarOrientation;
    FAlign: TAlign;
    FButtonSize: TPoint;
    
    
    FButtonStyle: JvQThemes.TButtonStyle;
    
    FGridSize: TPoint;
    FOffset: TPoint;
    FEditWin: HWnd;
    FRowCount: Integer;
    FPrevRect: TRect;
    FPrevAlign: TAlign;
    FOptions: TJvSpeedBarOptions;
    FLocked: Boolean;
    FVersion: Integer;
    FDrag: Boolean;
    FResizing: Boolean;
    FStartDrag: TPoint;
    FWallpaper: TPicture;
    FBoundLines: TBoundLines;
    FIniLink: TJvIniLink;
    FReserved: Integer;
    FFix: Boolean;
    FDesignStyle: Boolean;
    FScaleFlags: TSbScaleFlags;
    FOnAddItem: TNotifyEvent;
    FOnApplyAlign: TApplyAlignEvent;
    FOnPosChanged: TNotifyEvent;
    FOnVisibleChanged: TNotifyEvent;
    FOnCustomize: TNotifyEvent;
    FImages: TImageList;
    FImageChangeLink: TChangeLink;
    procedure ImageListChange(Sender: TObject);
    procedure SetImages(Value: TImageList);
    procedure InvalidateItem(Item: TJvSpeedItem; Data: Longint);
    function GetOrientation: TBarOrientation;
    procedure SetOrientation(Value: TBarOrientation);
    procedure ApplyOrientation(Value: TBarOrientation);
    procedure ApplyButtonSize;
    procedure UpdateGridSize;
    procedure ClearSections;
    function GetAlign: TAlign;
    procedure SetAlign(Value: TAlign);
    function GetButtonSize(Index: Integer): Integer;
    procedure SetButtonSize(Index, Value: Integer);
    function GetButtonsOffset(Index: Integer): Integer;
    procedure SetButtonsOffset(Index: Integer; Value: Integer);
    procedure SetOptions(Value: TJvSpeedBarOptions);
    procedure SetBoundLines(Value: TBoundLines);
    function MinButtonsOffset: Integer;
    procedure WallpaperChanged(Sender: TObject);
    procedure SetWallpaper(Value: TPicture);
    procedure SetItemParams(Item: TJvSpeedItem; InitBounds: Boolean);
    procedure SetItemVisible(Item: TJvSpeedItem; Data: Longint);
    procedure SetItemEnabled(Item: TJvSpeedItem; Data: Longint);
    procedure SetItemButtonSize(Item: TJvSpeedItem; Data: Longint);
    procedure OffsetItem(Item: TJvSpeedItem; Data: Longint);
    procedure ApplyItemSize(Item: TJvSpeedItem; Data: Longint);
    procedure AlignItemToGrid(Item: TJvSpeedItem; Data: Longint);
    procedure SwapItemBounds(Item: TJvSpeedItem; Data: Longint);
    procedure SetItemEditing(Item: TJvSpeedItem; Data: Longint);
    procedure HideItem(Item: TJvSpeedItem; Data: Longint);
    procedure WriteItemLayout(Item: TJvSpeedItem; Data: Longint);
    procedure FlatItem(Item: TJvSpeedItem; Data: Longint);
    procedure TransparentItem(Item: TJvSpeedItem; Data: Longint);
    function GetSection(Index: Integer): TJvSpeedBarSection;
    function GetSectionCount: Integer;
    procedure GrayedItem(Item: TJvSpeedItem; Data: Longint);
    function GetFramePos(X, Y: Integer; var Apply: Boolean): Integer;
    function GetFrameRect(X, Y: Integer): TRect;
    procedure StartDragFrame;
    procedure DragFrame(X, Y: Integer);
    procedure StopDragFrame(X, Y: Integer);
    function CheckResize(Shift: TShiftState; X, Y: Integer): Boolean;
    procedure ReadSections(Reader: TReader);
    procedure WriteSections(Writer: TWriter);
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
    procedure ReadDesignStyle(Reader: TReader);
    procedure ReadAllowDrag(Reader: TReader);
    procedure WriteDesignStyle(Writer: TWriter);
    function GetStorage: TJvFormPlacement;
    procedure SetStorage(Value: TJvFormPlacement);
    procedure IniSave(Sender: TObject);
    procedure IniLoad(Sender: TObject);
  protected
    procedure VisibleChanged; override;
    procedure EnabledChanged; override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    function AppendSection(Value: TJvSpeedBarSection): Integer; virtual;
    procedure AlignItemsToGrid;
    
    
    procedure ChangeScale(M, D, MH, DH: Integer); override;
    
    procedure Loaded; override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure SetChildOrder(Component: TComponent; Order: Integer); override;
    procedure ForEachItem(Proc: TForEachItem; Data: Longint); virtual;
    procedure PosChanged; dynamic;
    procedure AfterCustomize; dynamic;
    property ScaleFlags: TSbScaleFlags read FScaleFlags write FScaleFlags;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFontDefault; virtual;
    procedure RemoveItem(Item: TJvSpeedItem);
    procedure RemoveSection(Section: Integer); { delete and free section and items }
    procedure DeleteSection(Section: Integer); { delete section }
    function AddSection(const ACaption: string): Integer;
    procedure AddItem(Section: Integer; Item: TJvSpeedItem);
    function NewItem(AOwner: TComponent; Section: Integer;
      const AName: string): TJvSpeedItem;
    function AcceptDropItem(Item: TJvSpeedItem; X, Y: Integer): Boolean;
    procedure SetEditing(Win: HWnd);
    function GetEditing: Boolean;
    function SearchItem(const ItemName: string): TJvSpeedItem;
    function FindItem(Item: TJvSpeedItem; var Section, Index: Integer): Boolean;
    function SearchSection(const ACaption: string): Integer;
    procedure Customize(HelpCtx: THelpContext);
    procedure SaveLayout;
    procedure RestoreLayout;
    procedure LoadFromAppStorage(const AppStorage: TJvCustomAppStorage; const Path: string);
    procedure SaveToAppStorage(const AppStorage: TJvCustomAppStorage; const Path: string);
    procedure Load;
    procedure Save;
    function ItemsCount(Section: Integer): Integer;
    function Items(Section, Index: Integer): TJvSpeedItem;
    property EditMode: Boolean read GetEditing;
    property SectionCount: Integer read GetSectionCount;
    property Sections[Index: Integer]: TJvSpeedBarSection read GetSection;
    property Orientation: TBarOrientation read GetOrientation write SetOrientation
      default boHorizontal;
    property OnAddItem: TNotifyEvent read FOnAddItem write FOnAddItem; { for internal use only }
  published
    property Font;
    property ParentFont default False;
    property BoundLines: TBoundLines read FBoundLines write SetBoundLines default [];
    property Position: TBarPosition read FPosition write FPosition default bpAuto;
    { ensure Position is declared before Align }
    property Align: TAlign read GetAlign write SetAlign default alTop;
    { ensure Options is declared before BtnOffset... }
    property Options: TJvSpeedBarOptions read FOptions write SetOptions
      default [sbAllowDrag, sbGrayedBtns];
    property BtnOffsetHorz: Integer index 0 read GetButtonsOffset write SetButtonsOffset
      stored True;
    property BtnOffsetVert: Integer index 1 read GetButtonsOffset write SetButtonsOffset
      stored True;
    property BtnWidth: Integer index 0 read GetButtonSize write SetButtonSize;
    property BtnHeight: Integer index 1 read GetButtonSize write SetButtonSize;
    property IniStorage: TJvFormPlacement read GetStorage write SetStorage;
    property Version: Integer read FVersion write FVersion default 0;
    property Wallpaper: TPicture read FWallpaper write SetWallpaper;
    property Images: TImageList read FImages write SetImages;
    
    property Constraints;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property Color;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentShowHint default False;
    property PopupMenu;
    property ShowHint default True;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnApplyAlign: TApplyAlignEvent read FOnApplyAlign write FOnApplyAlign;
    property OnCustomize: TNotifyEvent read FOnCustomize write FOnCustomize;
    property OnPosChanged: TNotifyEvent read FOnPosChanged write FOnPosChanged;
    property OnVisibleChanged: TNotifyEvent read FOnVisibleChanged write FOnVisibleChanged;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnContextPopup;
    property OnResize;
  
  end;

  TJvSpeedItem = class(TComponent)
  private
    FCaption: string;
    FEditing: Boolean;
    FEnabled: Boolean;
    FButton: TJvSpeedButton;
    FVisible: Boolean;
    FStored: Boolean;
    FParent: TJvSpeedBar;
    FSection: Integer;
    FSectionName: string;
    FImageIndex: Integer;
    procedure SetImageIndex(Value: Integer);
    function GetAction: TBasicAction;
    procedure SetAction(Value: TBasicAction);
    function GetAllowAllUp: Boolean;
    procedure SetAllowAllUp(Value: Boolean);
    function GetAllowTimer: Boolean;
    procedure SetAllowTimer(Value: Boolean);
    function GetBtnCaption: TCaption;
    procedure SetBtnCaption(const Value: TCaption);
    function GetGroupIndex: Integer;
    procedure SetGroupIndex(Value: Integer);
    function GetDown: Boolean;
    procedure SetDown(Value: Boolean);
    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
    function GetLayout: TButtonLayout;
    procedure SetLayout(Value: TButtonLayout);
    function GetMargin: Integer;
    procedure SetMargin(Value: Integer);
    function GetNumGlyphs: TJvNumGlyphs;
    procedure SetNumGlyphs(Value: TJvNumGlyphs);
    function GetParentShowHint: Boolean;
    procedure SetParentShowHint(Value: Boolean);
    function GetFont: TFont;
    procedure SetFont(Value: TFont);
    function GetParentFont: Boolean;
    procedure SetParentFont(Value: Boolean);
    function IsFontStored: Boolean;
    function GetShowHint: Boolean;
    procedure SetShowHint(Value: Boolean);
    function IsShowHintStored: Boolean;
    function GetSpacing: Integer;
    procedure SetSpacing(Value: Integer);
    function GetCursor: TCursor;
    procedure SetCursor(Value: TCursor);
    function GetHint: string;
    procedure SetHint(const Value: string);
    function GetTag: Longint;
    procedure SetTag(Value: Longint);
    function GetDropDownMenu: TPopupMenu;
    procedure SetDropDownMenu(Value: TPopupMenu);
    function GetMarkDropDown: Boolean;
    procedure SetMarkDropDown(Value: Boolean);
    function GetWordWrap: Boolean;
    procedure SetWordWrap(Value: Boolean);
    function GetOnClick: TNotifyEvent;
    procedure SetOnClick(Value: TNotifyEvent);
    function GetOnDblClick: TNotifyEvent;
    procedure SetOnDblClick(Value: TNotifyEvent);
    function GetOnMouseDown: TMouseEvent;
    procedure SetOnMouseDown(Value: TMouseEvent);
    function GetOnMouseMove: TMouseMoveEvent;
    procedure SetOnMouseMove(Value: TMouseMoveEvent);
    function GetOnMouseUp: TMouseEvent;
    procedure SetOnMouseUp(Value: TMouseEvent);
    function GetOnMouseEnter: TNotifyEvent;
    procedure SetOnMouseEnter(Value: TNotifyEvent);
    function GetOnMouseLeave: TNotifyEvent;
    procedure SetOnMouseLeave(Value: TNotifyEvent);
    function GetCaption: TCaption;
    procedure SetCaption(const Value: TCaption);
    procedure SetEditing(Value: Boolean);
    function GetLeft: Integer;
    function GetTop: Integer;
    procedure SetLeft(Value: Integer);
    procedure SetTop(Value: Integer);
    function GetSection: Integer;
    procedure SetSection(Value: Integer);
    function GetSectionName: string;
    {procedure SetSectionName(const Value: string);}
    procedure ReadSection(Reader: TReader);
    procedure WriteSection(Writer: TWriter);
    procedure ReadSectionName(Reader: TReader);
    procedure WriteSectionName(Writer: TWriter);
  protected
    procedure ReadState(Reader: TReader); override;
    procedure SetName(const Value: TComponentName); override;
    procedure SetEnabled(Value: Boolean);
    procedure SetVisible(Value: Boolean);
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HasParent: Boolean; override;
    function GetParentComponent: TComponent; override;
    procedure SetParentComponent(Value: TComponent); override;
    procedure ButtonClick;
    function CheckBtnMenuDropDown: Boolean;
    procedure Click; virtual;
    procedure UpdateSection;
    procedure InvalidateItem;
    property ASection: Integer read GetSection write SetSection;
    property SpeedBar: TJvSpeedBar read FParent;
    property Button: TJvSpeedButton read FButton;
  published
    property Action: TBasicAction read GetAction write SetAction;
    property AllowAllUp: Boolean read GetAllowAllUp write SetAllowAllUp default False;
    property AllowTimer: Boolean read GetAllowTimer write SetAllowTimer default False;
    property BtnCaption: TCaption read GetBtnCaption write SetBtnCaption;
    property Caption: TCaption read GetCaption write SetCaption;
    property GroupIndex: Integer read GetGroupIndex write SetGroupIndex default 0;
    property Down: Boolean read GetDown write SetDown default False;
    property DropDownMenu: TPopupMenu read GetDropDownMenu write SetDropDownMenu;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Font: TFont read GetFont write SetFont stored IsFontStored;
    property Cursor: TCursor read GetCursor write SetCursor default crDefault;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property Hint: string read GetHint write SetHint;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Layout: TButtonLayout read GetLayout write SetLayout default blGlyphTop;
    property Margin: Integer read GetMargin write SetMargin default -1;
    property MarkDropDown: Boolean read GetMarkDropDown write SetMarkDropDown default True;
    property NumGlyphs: TJvNumGlyphs read GetNumGlyphs write SetNumGlyphs default 1;
    property ParentShowHint: Boolean read GetParentShowHint write SetParentShowHint default True;
    property ParentFont: Boolean read GetParentFont write SetParentFont default True;
    property ShowHint: Boolean read GetShowHint write SetShowHint stored IsShowHintStored;
    property Spacing: Integer read GetSpacing write SetSpacing default 4;
    property Stored: Boolean read FStored write FStored default True;
    property Tag: Longint read GetTag write SetTag default 0;
    property Left: Integer read GetLeft write SetLeft default 0;
    property Top: Integer read GetTop write SetTop default 0;
    property Visible: Boolean read FVisible write SetVisible default False;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap default False;
    property OnClick: TNotifyEvent read GetOnClick write SetOnClick;
    property OnDblClick: TNotifyEvent read GetOnDblClick write SetOnDblClick;
    property OnMouseDown: TMouseEvent read GetOnMouseDown write SetOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read GetOnMouseMove write SetOnMouseMove;
    property OnMouseUp: TMouseEvent read GetOnMouseUp write SetOnMouseUp;
    property OnMouseEnter: TNotifyEvent read GetOnMouseEnter write SetOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read GetOnMouseLeave write SetOnMouseLeave;
  end;

  TJvSpeedBarSection = class(TComponent)
  private
    FList: TList;
    FTitle: string;
    FParent: TJvSpeedBar;
    function Get(Index: Integer): TJvSpeedItem;
    procedure Put(Index: Integer; Item: TJvSpeedItem);
    function GetCount: Integer;
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function GetIndex: Integer;
    procedure SetIndex(Value: Integer);
    procedure SetSpeedBar(Value: TJvSpeedBar);
    procedure ValidateCaption(const NewCaption: string);
  protected
    procedure SetParentComponent(Value: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HasParent: Boolean; override;
    function GetParentComponent: TComponent; override;
    procedure Clear;
    procedure RemoveItem(Item: TJvSpeedItem);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TJvSpeedItem read Get write Put; default;
    property List: TList read FList; { for internal use only }
    property SpeedBar: TJvSpeedBar read FParent write SetSpeedBar stored False;
  published
    property Caption: string read GetTitle write SetTitle;
    property Index: Integer read GetIndex write SetIndex stored False;
  end;

  TJvBtnControl = class(TJvCustomControl)
  private
    FImage: TJvButtonImage;
    FSpacing: Integer;
    FMargin: Integer;
    FLayout: TButtonLayout;
    FImageIndex: Integer;
    FImages: TImageList;
    function GetCaption: TCaption;
    function GetGlyph: TBitmap;
    function GetNumGlyphs: TJvNumGlyphs;
    function GetWordWrap: Boolean;
    function GetAlignment: TAlignment;
    procedure SetAlignment(Value: TAlignment);
    procedure SetCaption(const Value: TCaption);
    procedure SetNumGlyphs(Value: TJvNumGlyphs);
    procedure SetGlyph(Value: TBitmap);
    procedure SetWordWrap(Value: Boolean);
  protected
    
    procedure Paint; override;
    procedure DoBoundsChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AssignSpeedItem(Item: TJvSpeedItem);
    procedure Activate(Rect: TRect);
    procedure ReleaseHandle;
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property Caption: TCaption read GetCaption write SetCaption;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property NumGlyphs: TJvNumGlyphs read GetNumGlyphs write SetNumGlyphs;
    property Spacing: Integer read FSpacing write FSpacing;
    property ImageIndex: Integer read FImageIndex write FImageIndex;
    property Images: TImageList read FImages write FImages;
    property Margin: Integer read FMargin write FMargin;
    property Layout: TButtonLayout read FLayout write FLayout;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap;
    property Font;
  end;

{ Utility routines for SpeedBar Editors }

function FindSpeedBar(const Pos: TPoint): TJvSpeedBar;
procedure DrawCellButton(Grid: TDrawGrid; R: TRect; Item: TJvSpeedItem;
  Image: TJvButtonImage; ARightToLeft: Boolean = False);
function NewSpeedSection(ASpeedbar: TJvSpeedBar; const ACaption: string): Integer;
function NewSpeedItem(AOwner: TComponent; ASpeedbar: TJvSpeedBar; Section: Integer;
  const AName: string): TJvSpeedItem;

implementation

uses
  Math,
  
  
  QConsts,
  
  JvQJVCLUtils, JvQJCLUtils, JvQSpeedbarSetupForm, JvQResources;

const
  DefaultButtonSize: TPoint = (X: DefButtonWidth; Y: DefButtonHeight);
  DragFrameWidth = 3;
  StartDragOffset = 4;

// (rom) changed to var
var
  Registered: Boolean = False;

const
  Alignments: array [TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);

// (rom) moved here to make JvMaxMin obsolete
procedure SwapInt(var Int1, Int2: Integer);
var
  I: Integer;
begin
  I := Int1;
  Int1 := Int2;
  Int2 := I;
end;

//=== TJvSpeedBarSection =====================================================

constructor TJvSpeedBarSection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList := TList.Create;
  FTitle := EmptyStr;
end;

destructor TJvSpeedBarSection.Destroy;
begin
  Clear;
  if FParent <> nil then
    FParent.DeleteSection(Index);
  //if (FTitle <> nil) and (FTitle^ <> '') then Dispose(FTitle);
  FTitle := SysUtils.EmptyStr;
  FList.Free;
  inherited Destroy;
end;

procedure TJvSpeedBarSection.Clear;
begin
  while FList.Count > 0 do
  begin
    TJvSpeedItem(FList[0]).Free;
    FList.Delete(0);
  end;
end;

function TJvSpeedBarSection.Get(Index: Integer): TJvSpeedItem;
begin
  Result := TJvSpeedItem(FList[Index]);
end;

procedure TJvSpeedBarSection.Put(Index: Integer; Item: TJvSpeedItem);
begin
  FList[Index] := Item;
end;

function TJvSpeedBarSection.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TJvSpeedBarSection.GetIndex: Integer;
begin
  if FParent <> nil then
    Result := FParent.FSections.IndexOf(Self)
  else
    Result := -1;
end;

procedure TJvSpeedBarSection.SetIndex(Value: Integer);
var
  CurIndex, Count: Integer;
begin
  CurIndex := GetIndex;
  if CurIndex >= 0 then
  begin
    Count := FParent.FSections.Count;
    if Value < 0 then
      Value := 0;
    if Value >= Count then
      Value := Count - 1;
    if Value <> CurIndex then
    begin
      FParent.FSections.Delete(CurIndex);
      FParent.FSections.Insert(Value, Self);
    end;
  end;
end;

function TJvSpeedBarSection.HasParent: Boolean;
begin
  Result := True;
end;

procedure TJvSpeedBarSection.SetSpeedBar(Value: TJvSpeedBar);
var
  CurIndex: Integer;
begin
  CurIndex := GetIndex;
  if FParent <> nil then
    FParent.DeleteSection(Index);
  if Value <> nil then
    Value.AppendSection(Self);
  if CurIndex >= 0 then
    Index := CurIndex;
end;

function TJvSpeedBarSection.GetParentComponent: TComponent;
begin
  Result := FParent;
end;

procedure TJvSpeedBarSection.SetParentComponent(Value: TComponent);
begin
  SpeedBar := Value as TJvSpeedBar;
end;

procedure TJvSpeedBarSection.RemoveItem(Item: TJvSpeedItem);
var
  I: Integer;
begin
  I := FList.IndexOf(Item);
  if I >= 0 then
  begin
    Item.FButton.Parent := nil;
    Item.FParent := nil;
    Item.FSection := -1;
    FList.Delete(I);
  end;
end;

procedure TJvSpeedBarSection.ValidateCaption(const NewCaption: string);
var
  I: Integer;
begin
  if FParent <> nil then
  begin
    I := FParent.SearchSection(NewCaption);
    if (I <> Index) and (I >= 0) then
      raise EJvSpeedbarError.Create(SDuplicateString);
  end;
end;

procedure TJvSpeedBarSection.SetTitle(const Value: string);
begin
  if not (csLoading in ComponentState) then
    ValidateCaption(Value);
  FTitle := Value;
end;

function TJvSpeedBarSection.GetTitle: string;
begin
  Result := FTitle;
end;

//=== TJvSpeedBarButton ======================================================

type
  TJvSpeedBarButton = class(TJvSpeedButton)
  private
    FItem: TJvSpeedItem;
    FBtn: TJvBtnControl;
    procedure InvalidateGlyph;
  protected
    procedure VisibleChanged; override;
    
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure PaintImage(Canvas: TCanvas; ARect: TRect; const Offset: TPoint;
      AState: TJvButtonState; DrawMark: Boolean); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

constructor TJvSpeedBarButton.Create(AOwner: TComponent);
begin
  FItem := TJvSpeedItem(AOwner);
  { Ensure FItem is assigned before inherited Create }
  inherited Create(AOwner);
  Visible := False;
  Style := bsNew;
  ParentShowHint := True;
  ParentFont := True;
end;

destructor TJvSpeedBarButton.Destroy;
begin
  FBtn.Free;
  inherited Destroy;
end;

procedure TJvSpeedBarButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if FItem.SpeedBar <> nil then
  begin
    case FItem.SpeedBar.Orientation of
      boHorizontal:
        ATop := Max(FItem.SpeedBar.FOffset.Y, ATop);
      boVertical:
        ALeft := Max(FItem.SpeedBar.FOffset.X, ALeft);
    end;
  end;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TJvSpeedBarButton.VisibleChanged;
begin
  if Visible then
    ControlStyle := ControlStyle + [csOpaque]
  else
    ControlStyle := ControlStyle - [csOpaque];
  inherited;
end;



procedure TJvSpeedBarButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  P: TPoint;
begin
  if FItem.FEditing and Visible and (Button = mbLeft) and
    (FItem.SpeedBar <> nil) then
  begin
    P := ClientToScreen(Point(FItem.SpeedBar.BtnWidth {div 2},
      FItem.SpeedBar.BtnHeight {div 2}));
    
    if FBtn = nil then
    begin
      
      
      Mouse.CursorPos := P;
      
      FBtn := TJvBtnControl.Create(Self);
      FBtn.AssignSpeedItem(FItem);
    end;
    BringToFront;
  end
  else
    inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvSpeedBarButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  R: TRect;
begin
  if FItem.FEditing and (FBtn <> nil) then
  begin
    P := ClientToScreen(Point(X - (FBtn.Width {div 2}),
      Y - (FBtn.Height {div 2})));
    X := P.X;
    Y := P.Y;
    if FItem.SpeedBar <> nil then
    begin
      Visible := False;
      if csDesigning in ComponentState then
      begin
        R := BoundsRect;
        InvalidateRect(FItem.SpeedBar.Handle, @R, True);
      end;
      P := FItem.SpeedBar.ScreenToClient(P);
      if PtInRect(FItem.SpeedBar.ClientRect, P) then
      begin
        FBtn.Activate(Bounds(X, Y, FBtn.Width, FBtn.Height));
      end
      else
        FBtn.ReleaseHandle;
    end;
  end
  else
    inherited MouseMove(Shift, X, Y);
end;

procedure TJvSpeedBarButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  P: TPoint;
begin
  if FItem.FEditing and (FBtn <> nil) then
  begin
    X := X - (FBtn.Width {div 2});
    Y := Y - (FBtn.Height {div 2});
    FBtn.Free;
    FBtn := nil;
    P := ClientToScreen(Point(X, Y));
    if FItem.SpeedBar <> nil then
    begin
      P := FItem.SpeedBar.ScreenToClient(P);
      if PtInRect(FItem.SpeedBar.ClientRect, P) then
      begin
        if not FItem.SpeedBar.AcceptDropItem(FItem, P.X, P.Y) then
        begin
          SendMessage(FItem.SpeedBar.FEditWin, CM_SPEEDBARCHANGED, SBR_CHANGED,
            Longint(FItem.SpeedBar));
        end
        else
        begin
          SendMessage(FItem.SpeedBar.FEditWin, CM_SPEEDBARCHANGED, SBR_BTNSELECT,
            Longint(FItem));
          Invalidate;
        end;
      end
      else
      begin
        SendToBack;
        FItem.Visible := False;
        SendMessage(FItem.SpeedBar.FEditWin, CM_SPEEDBARCHANGED, SBR_CHANGED,
          Longint(FItem.SpeedBar));
      end;
    end;
  end
  else
    inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvSpeedBarButton.InvalidateGlyph;
begin
  TJvxButtonGlyph(ButtonGlyph).Invalidate;
end;

procedure TJvSpeedBarButton.PaintImage(Canvas: TCanvas; ARect: TRect; const Offset: TPoint;
  AState: TJvButtonState; DrawMark: Boolean);
begin
  if FItem.SpeedBar <> nil then
  begin
    TJvxButtonGlyph(ButtonGlyph).DrawEx(Canvas, ARect, Offset, Caption, Layout,
      Margin, Spacing, DrawMark, FItem.SpeedBar.Images, FItem.FImageIndex,
      AState,
      
      
      Alignments[Alignment]
      
      );
  end
  else
    inherited PaintImage(Canvas, ARect, Offset, AState, DrawMark);
end;

procedure TJvSpeedBarButton.Paint;
begin
  if Visible then
    inherited Paint;
end;

//=== TJvSpeedItem ===========================================================

constructor TJvSpeedItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TJvSpeedBarButton.Create(Self);
  FButton.Visible := False;
  FButton.SetBounds(0, 0, DefaultButtonSize.X, DefaultButtonSize.Y);
  FCaption := EmptyStr;
  ShowHint := True;
  ParentShowHint := True;
  FVisible := False;
  FStored := True;
  FEnabled := True;
  FEditing := False;
  FParent := nil;
  FImageIndex := -1;
end;

destructor TJvSpeedItem.Destroy;
begin
  FVisible := False;
  if FParent <> nil then
    FParent.RemoveItem(Self);
  FButton.Free;
  //if (FCaption <> nil) and (FCaption^ <> '') then Dispose(FCaption);
  FCaption := EmptyStr;
  inherited Destroy;
end;

function TJvSpeedItem.GetCaption: TCaption;
begin
  Result := TCaption(FCaption);
end;

procedure TJvSpeedItem.SetCaption(const Value: TCaption);
var
  ChangeHint: Boolean;
begin
  ChangeHint := (Owner = nil) or not (Owner is TControl) or
    not (csLoading in TControl(Owner).ComponentState) and
    (Caption = GetShortHint(Hint));
  FCaption := Value;
  if ChangeHint then
  begin
    if Pos('|', Value) = 0 then
    begin
      if Pos('|', Hint) = 0 then
        Hint := Value + '|'
      else
        Hint := Value + '|' + GetLongHint(Hint);
    end
    else
    begin
      if GetLongHint(Value) = '' then
        Hint := GetShortHint(Value) + '|' + GetLongHint(Hint)
      else
        Hint := Value;
    end;
  end;
end;

procedure TJvSpeedItem.SetName(const Value: TComponentName);
var
  ChangeText: Boolean;
begin
  ChangeText := (Name = Caption) and
    ((Owner = nil) or not (Owner is TControl) or
    not (csLoading in TControl(Owner).ComponentState));
  inherited SetName(Value);
  if ChangeText then
    Caption := Value;
end;

procedure TJvSpeedItem.SetEditing(Value: Boolean);
begin
  FEditing := Value;
  if FEditing then
  begin
    FButton.Enabled := True;
    FButton.Flat := False;
  end
  else
  begin
    SetEnabled(FEnabled);
    if SpeedBar <> nil then
      FButton.Flat := (sbFlatBtns in SpeedBar.Options);
  end;
end;

function TJvSpeedItem.HasParent: Boolean;
begin
  Result := True;
end;

procedure TJvSpeedItem.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Assigned(Filer.Ancestor) then
      Result := GetSectionName <> TJvSpeedItem(Filer.Ancestor).GetSectionName
    else
      Result := True;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Section', ReadSection, WriteSection, False);
  Filer.DefineProperty('SectionName', ReadSectionName, WriteSectionName, DoWrite);
end;

procedure TJvSpeedItem.ReadSectionName(Reader: TReader);
begin
  FSectionName := Reader.ReadString;
end;

procedure TJvSpeedItem.WriteSectionName(Writer: TWriter);
begin
  Writer.WriteString(GetSectionName);
end;

procedure TJvSpeedItem.ReadSection(Reader: TReader);
begin
  FSection := Reader.ReadInteger;
end;

procedure TJvSpeedItem.WriteSection(Writer: TWriter);
begin
  UpdateSection;
  Writer.WriteInteger(FSection);
end;

function TJvSpeedItem.GetParentComponent: TComponent;
begin
  Result := FParent;
end;

procedure TJvSpeedItem.SetParentComponent(Value: TComponent);
var
  I: Integer;
begin
  if not (csLoading in ComponentState) then
  begin
    if FParent <> nil then
      FParent.RemoveItem(Self);
    if (Value <> nil) and (Value is TJvSpeedBar) then
    begin
      I := TJvSpeedBar(Value).SearchSection(FSectionName);
      if I >= 0 then
        FSection := I;
      TJvSpeedBar(Value).AddItem(FSection, Self);
    end;
  end;
end;

procedure TJvSpeedItem.SetImageIndex(Value: Integer);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    TJvSpeedBarButton(FButton).InvalidateGlyph;
    FButton.Invalidate;
  end;
end;

procedure TJvSpeedItem.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TJvSpeedBar then
  begin
    if FSectionName <> '' then
      FSection := TJvSpeedBar(Reader.Parent).SearchSection(FSectionName);
    TJvSpeedBar(Reader.Parent).AddItem(Max(FSection, 0), Self);
  end;
end;

function TJvSpeedItem.GetSection: Integer;
begin
  UpdateSection;
  Result := FSection;
end;

procedure TJvSpeedItem.SetSection(Value: Integer);
begin
  if SpeedBar = nil then
    FSection := Value;
end;

function TJvSpeedItem.GetSectionName: string;
begin
  UpdateSection;
  if FSection >= 0 then
    Result := FParent.Sections[FSection].Caption
  else
    Result := FSectionName;
end;

{
procedure TJvSpeedItem.SetSectionName(const Value: string);
begin
  if FParent <> nil then FSection := FParent.SearchSection(Value)
  else FSection := -1;
  FSectionName := Value;
end;
}

procedure TJvSpeedItem.InvalidateItem;
begin
  FSection := -1;
end;

procedure TJvSpeedItem.UpdateSection;
var
  I: Integer;
begin
  if FParent <> nil then
    FParent.FindItem(Self, FSection, I)
  else
    FSection := -1;
end;

procedure TJvSpeedItem.SetEnabled(Value: Boolean);
begin
  if (FButton.Enabled <> Value) or (FEnabled <> Value) then
  begin
    FEnabled := Value;
    if not FEditing then
    begin
      if (SpeedBar <> nil) and Value then
        FButton.Enabled := (Value and SpeedBar.Enabled)
      else
        FButton.Enabled := Value;
    end;
  end;
end;

procedure TJvSpeedItem.SetVisible(Value: Boolean);
begin
  if (FButton.Visible <> Value) or (FVisible <> Value) or
    (Value and (FButton.Parent = nil)) then
  begin
    FVisible := Value;
    if (SpeedBar <> nil) and Value then
      FButton.Visible := Value and SpeedBar.Visible
    else
      FButton.Visible := Value;
    if Value then
      FButton.Parent := SpeedBar;
  end;
end;

function TJvSpeedItem.GetAllowAllUp: Boolean;
begin
  Result := FButton.AllowAllUp;
end;

procedure TJvSpeedItem.SetAllowAllUp(Value: Boolean);
begin
  FButton.AllowAllUp := Value;
end;

function TJvSpeedItem.GetAllowTimer: Boolean;
begin
  Result := FButton.AllowTimer;
end;

procedure TJvSpeedItem.SetAllowTimer(Value: Boolean);
begin
  FButton.AllowTimer := Value;
end;

function TJvSpeedItem.GetBtnCaption: TCaption;
begin
  Result := FButton.Caption;
end;

procedure TJvSpeedItem.SetBtnCaption(const Value: TCaption);
begin
  FButton.Caption := Value;
end;

function TJvSpeedItem.GetGroupIndex: Integer;
begin
  Result := FButton.GroupIndex;
end;

procedure TJvSpeedItem.SetGroupIndex(Value: Integer);
begin
  FButton.GroupIndex := Value;
end;

function TJvSpeedItem.GetOnClick: TNotifyEvent;
begin
  Result := FButton.OnClick;
end;

procedure TJvSpeedItem.SetOnClick(Value: TNotifyEvent);
begin
  FButton.OnClick := Value;
end;

function TJvSpeedItem.GetOnDblClick: TNotifyEvent;
begin
  Result := FButton.OnDblClick;
end;

procedure TJvSpeedItem.SetOnDblClick(Value: TNotifyEvent);
begin
  FButton.OnDblClick := Value;
end;

function TJvSpeedItem.GetOnMouseDown: TMouseEvent;
begin
  Result := FButton.OnMouseDown;
end;

procedure TJvSpeedItem.SetOnMouseDown(Value: TMouseEvent);
begin
  FButton.OnMouseDown := Value;
end;

function TJvSpeedItem.GetOnMouseMove: TMouseMoveEvent;
begin
  Result := FButton.OnMouseMove;
end;

procedure TJvSpeedItem.SetOnMouseMove(Value: TMouseMoveEvent);
begin
  FButton.OnMouseMove := Value;
end;

function TJvSpeedItem.GetOnMouseUp: TMouseEvent;
begin
  Result := FButton.OnMouseUp;
end;

procedure TJvSpeedItem.SetOnMouseUp(Value: TMouseEvent);
begin
  FButton.OnMouseUp := Value;
end;

function TJvSpeedItem.GetOnMouseEnter: TNotifyEvent;
begin
  Result := FButton.OnMouseEnter;
end;

procedure TJvSpeedItem.SetOnMouseEnter(Value: TNotifyEvent);
begin
  FButton.OnMouseEnter := Value;
end;

function TJvSpeedItem.GetOnMouseLeave: TNotifyEvent;
begin
  Result := FButton.OnMouseLeave;
end;

procedure TJvSpeedItem.SetOnMouseLeave(Value: TNotifyEvent);
begin
  FButton.OnMouseLeave := Value;
end;

function TJvSpeedItem.GetDown: Boolean;
begin
  Result := FButton.Down;
end;

procedure TJvSpeedItem.SetDown(Value: Boolean);
begin
  FButton.Down := Value;
end;

function TJvSpeedItem.GetGlyph: TBitmap;
begin
  Result := FButton.Glyph;
end;

procedure TJvSpeedItem.SetGlyph(Value: TBitmap);
begin
  FButton.Glyph := Value;
end;

function TJvSpeedItem.GetLayout: TButtonLayout;
begin
  Result := FButton.Layout;
end;

procedure TJvSpeedItem.SetLayout(Value: TButtonLayout);
begin
  FButton.Layout := Value;
end;

function TJvSpeedItem.GetMargin: Integer;
begin
  Result := FButton.Margin;
end;

procedure TJvSpeedItem.SetMargin(Value: Integer);
begin
  FButton.Margin := Value;
end;

function TJvSpeedItem.GetNumGlyphs: TJvNumGlyphs;
begin
  Result := FButton.NumGlyphs;
end;

procedure TJvSpeedItem.SetNumGlyphs(Value: TJvNumGlyphs);
begin
  FButton.NumGlyphs := Value;
end;

function TJvSpeedItem.GetParentShowHint: Boolean;
begin
  Result := FButton.ParentShowHint;
end;

procedure TJvSpeedItem.SetParentShowHint(Value: Boolean);
begin
  FButton.ParentShowHint := Value;
end;

function TJvSpeedItem.GetShowHint: Boolean;
begin
  Result := FButton.ShowHint;
end;

procedure TJvSpeedItem.SetShowHint(Value: Boolean);
begin
  FButton.ShowHint := Value;
end;

function TJvSpeedItem.GetFont: TFont;
begin
  Result := FButton.Font;
end;

procedure TJvSpeedItem.SetFont(Value: TFont);
begin
  FButton.Font := Value;
end;

function TJvSpeedItem.GetParentFont: Boolean;
begin
  Result := FButton.ParentFont;
end;

procedure TJvSpeedItem.SetParentFont(Value: Boolean);
begin
  FButton.ParentFont := Value;
end;

function TJvSpeedItem.IsFontStored: Boolean;
begin
  Result := not ParentFont;
end;

function TJvSpeedItem.IsShowHintStored: Boolean;
begin
  Result := not ParentShowHint;
end;

function TJvSpeedItem.GetSpacing: Integer;
begin
  Result := FButton.Spacing;
end;

procedure TJvSpeedItem.SetSpacing(Value: Integer);
begin
  FButton.Spacing := Value;
end;

function TJvSpeedItem.GetCursor: TCursor;
begin
  Result := FButton.Cursor;
end;

procedure TJvSpeedItem.SetCursor(Value: TCursor);
begin
  FButton.Cursor := Value;
end;

function TJvSpeedItem.GetHint: string;
begin
  Result := FButton.Hint;
end;

procedure TJvSpeedItem.SetHint(const Value: string);
begin
  FButton.Hint := Value;
end;

function TJvSpeedItem.GetAction: TBasicAction;
begin
  Result := FButton.Action;
end;

procedure TJvSpeedItem.SetAction(Value: TBasicAction);
begin
  FButton.Action := Value;
end;

procedure TJvSpeedItem.ButtonClick;
begin
  FButton.ButtonClick;
end;

function TJvSpeedItem.CheckBtnMenuDropDown: Boolean;
begin
  Result := FButton.CheckBtnMenuDropDown;
end;

procedure TJvSpeedItem.Click;
begin
  FButton.Click;
end;

function TJvSpeedItem.GetTag: Longint;
begin
  Result := inherited Tag;
end;

procedure TJvSpeedItem.SetTag(Value: Longint);
begin
  inherited Tag := Value;
  FButton.Tag := Value;
end;

function TJvSpeedItem.GetDropDownMenu: TPopupMenu;
begin
  Result := FButton.DropDownMenu;
end;

procedure TJvSpeedItem.SetDropDownMenu(Value: TPopupMenu);
begin
  FButton.DropDownMenu := Value;
end;

function TJvSpeedItem.GetMarkDropDown: Boolean;
begin
  Result := FButton.MarkDropDown;
end;

procedure TJvSpeedItem.SetMarkDropDown(Value: Boolean);
begin
  FButton.MarkDropDown := Value;
end;

function TJvSpeedItem.GetWordWrap: Boolean;
begin
  Result := FButton.WordWrap;
end;

procedure TJvSpeedItem.SetWordWrap(Value: Boolean);
begin
  FButton.WordWrap := Value;
end;

function TJvSpeedItem.GetLeft: Integer;
begin
  Result := FButton.Left;
end;

function TJvSpeedItem.GetTop: Integer;
begin
  Result := FButton.Top;
end;

procedure TJvSpeedItem.SetLeft(Value: Integer);
begin
  FButton.Left := Value;
end;

procedure TJvSpeedItem.SetTop(Value: Integer);
begin
  FButton.Top := Value;
end;

const
  InternalVer = 1;

//=== TJvSpeedBar ============================================================

constructor TJvSpeedBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSections := TList.Create;
  FButtonSize := DefaultButtonSize;
  FButtonStyle := bsNew;
  FWallpaper := TPicture.Create;
  FWallpaper.OnChange := WallpaperChanged;
  FIniLink := TJvIniLink.Create;
  FIniLink.OnSave := IniSave;
  FIniLink.OnLoad := IniLoad;
  FOffset.X := MinButtonsOffset;
  FOffset.Y := FOffset.X;
  Height := 2 * FOffset.Y + DefaultButtonSize.Y;
  FRowCount := 1;
  FEditWin := NullHandle;
  FOptions := [sbAllowDrag, sbGrayedBtns];
  ControlStyle := ControlStyle - [csSetCaption, csReplicatable];
  IncludeThemeStyle(Self, [csNeedsBorderPaint, csParentBackground]);
  ParentShowHint := False;
  ShowHint := True;
  SetFontDefault;
  inherited Align := alTop;
  FAlign := alTop;
  UpdateGridSize;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  if not Registered then
  begin
    
    GroupDescendentsWith(TJvSpeedItem, TControl);
    GroupDescendentsWith(TJvSpeedBarSection, TControl);
    
    RegisterClasses([TJvSpeedItem, TJvSpeedBarSection, TJvSpeedBarButton]);
    Registered := True;
  end;
end;

destructor TJvSpeedBar.Destroy;
begin
  FOnVisibleChanged := nil;
  FOnApplyAlign := nil;
  FOnPosChanged := nil;
  FIniLink.Free;
  FWallpaper.OnChange := nil;
  FWallpaper.Free;
  FWallpaper := nil;
  if FEditWin <> NullHandle then
  begin
    SendMessage(FEditWin, CM_SPEEDBARCHANGED, SBR_DESTROYED, Longint(Self));
    FEditWin := NullHandle;
  end;
  ClearSections;
  FSections.Free;
  FImageChangeLink.Free;
  inherited Destroy;
end;

procedure TJvSpeedBar.Loaded;
begin
  inherited Loaded;
  if (FReserved = 0) and FFix then
  begin { fix previous version error }
    inherited Align := alTop;
    FAlign := alTop;
  end;
  UpdateGridSize;
  ForEachItem(SetItemButtonSize, 0);
end;

procedure TJvSpeedBar.ReadData(Reader: TReader);
begin
  FReserved := Reader.ReadInteger;
end;

procedure TJvSpeedBar.WriteData(Writer: TWriter);
begin
  Writer.WriteInteger(InternalVer);
end;

procedure TJvSpeedBar.ReadAllowDrag(Reader: TReader);
begin
  if Reader.ReadBoolean then
    Options := Options + [sbAllowDrag]
  else
    Options := Options - [sbAllowDrag];
end;

procedure TJvSpeedBar.ReadDesignStyle(Reader: TReader);
begin
  FDesignStyle := Reader.ReadBoolean;
end;

procedure TJvSpeedBar.WriteDesignStyle(Writer: TWriter);
begin
  Writer.WriteBoolean(NewStyleControls);
end;

procedure TJvSpeedBar.ReadSections(Reader: TReader);
var
  TmpList: TStringList;
  I: Integer;
begin
  TmpList := TStringList.Create;
  try
    Reader.ReadListBegin;
    while not Reader.EndOfList do
      TmpList.AddObject(Reader.ReadString, nil);
    Reader.ReadListEnd;
    if (Reader.Ancestor = nil) or (TmpList.Count > 0) then
      for I := 0 to TmpList.Count - 1 do
        if SearchSection(TmpList[I]) < 0 then
          AddSection(TmpList[I]);
  finally
    TmpList.Free;
  end;
end;

procedure TJvSpeedBar.WriteSections(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to FSections.Count - 1 do
    Writer.WriteString(Sections[I].Caption);
  Writer.WriteListEnd;
end;

procedure TJvSpeedBar.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Sections', ReadSections, WriteSections, False);
  Filer.DefineProperty('NewStyle', ReadDesignStyle, WriteDesignStyle, False);
  Filer.DefineProperty('InternalVer', ReadData, WriteData,Filer.Ancestor = nil);
  { AllowDrag reading for backward compatibility only }
  Filer.DefineProperty('AllowDrag', ReadAllowDrag, nil, False);
end;

function TJvSpeedBar.GetSection(Index: Integer): TJvSpeedBarSection;
begin
  Result := TJvSpeedBarSection(FSections[Index]);
end;

function TJvSpeedBar.GetSectionCount: Integer;
begin
  Result := FSections.Count;
end;

procedure TJvSpeedBar.ForEachItem(Proc: TForEachItem; Data: Longint);
var
  I, Idx: Integer;
  Sect: TJvSpeedBarSection;
begin
  for I := 0 to FSections.Count - 1 do
    if FSections[I] <> nil then
    begin
      Sect := TJvSpeedBarSection(FSections[I]);
      for Idx := 0 to Sect.Count - 1 do
      begin
        if (Sect[Idx] <> nil) and Assigned(Proc) then
          Proc(TJvSpeedItem(Sect[Idx]), Data);
      end;
    end;
end;

function TJvSpeedBar.MinButtonsOffset: Integer;
begin
  Result := BorderWidth + 2 * Ord(not (sbFlatBtns in Options));
  if BevelOuter <> bvNone then
    Inc(Result, BevelWidth);
  if BevelInner <> bvNone then
    Inc(Result, BevelWidth);
end;

procedure TJvSpeedBar.SetItemVisible(Item: TJvSpeedItem; Data: Longint);
var
  ItemVisible: Boolean;
begin
  ItemVisible := Item.Visible and Self.Visible;
  Item.FButton.Visible := ItemVisible;
  if (Item.FButton.Parent <> Self) and ItemVisible then
    Item.FButton.Parent := Self;
end;

procedure TJvSpeedBar.SetItemEnabled(Item: TJvSpeedItem; Data: Longint);
begin
  Item.FButton.Enabled := Item.Enabled and Self.Enabled;
end;

procedure TJvSpeedBar.SetItemButtonSize(Item: TJvSpeedItem; Data: Longint);
begin
  ApplyItemSize(Item, Data);
  Item.Visible := Item.Visible; { update visible and parent after loading }
end;

procedure TJvSpeedBar.SwapItemBounds(Item: TJvSpeedItem; Data: Longint);
begin
  Item.FButton.SetBounds(Item.Top, Item.Left, FButtonSize.X, FButtonSize.Y);
end;

procedure TJvSpeedBar.SetFontDefault;
{$IFDEF MSWINDOWS}
var
  NCMetrics: TNonClientMetrics;
begin
  ParentFont := False;
  with Font do
  begin
    NCMetrics.cbSize := SizeOf(TNonClientMetrics);
    if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NCMetrics, 0) then
    begin
      Handle := CreateFontIndirect(NCMetrics.lfMenuFont);
      Charset := DEFAULT_CHARSET;
    end
    else
    begin
      Name := 'MS Sans Serif';
      Size := 8;
      Style := [];
      Color := clBtnText;
    end;
  end;
end;
{$ENDIF MSWINDOWS }
{$IFDEF LINUX}
begin
  ParentFont := False;
  with Font do
  begin
    Name := 'Helvetica';
    Height := 11;
    Style := [];
    Color := clBtnText;
  end;
end;
{$ENDIF LINUX}

procedure TJvSpeedBar.VisibleChanged;
begin
  inherited VisibleChanged;
  if not (csLoading in ComponentState) then
    ForEachItem(SetItemVisible, 0);
  if Assigned(FOnVisibleChanged) then
    FOnVisibleChanged(Self);
end;

procedure TJvSpeedBar.EnabledChanged;
begin
  inherited EnabledChanged;
  if not ((csLoading in ComponentState) or (csDesigning in ComponentState)) then
    ForEachItem(SetItemEnabled, 0);
end;

procedure TJvSpeedBar.WallpaperChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvSpeedBar.SetWallpaper(Value: TPicture);
begin
  FWallpaper.Assign(Value);
end;

procedure TJvSpeedBar.ClearSections;
begin
  while FSections.Count > 0 do
    RemoveSection(FSections.Count - 1);
  FSections.Clear;
end;

function TJvSpeedBar.Items(Section, Index: Integer): TJvSpeedItem;
var
  List: TJvSpeedBarSection;
begin
  Result := nil;
  if (Section >= 0) and (Section < FSections.Count) then
  begin
    List := Sections[Section];
    if List <> nil then
      if (Index >= 0) and (Index < List.Count) then
        Result := List[Index];
  end;
end;

function TJvSpeedBar.ItemsCount(Section: Integer): Integer;
begin
  Result := 0;
  if (Section >= 0) and (Section < FSections.Count) then
  begin
    if FSections[Section] <> nil then
      Result := Sections[Section].Count;
  end;
end;

procedure TJvSpeedBar.RemoveSection(Section: Integer);
var
  Sect: TJvSpeedBarSection;
  Item: TJvSpeedItem;
begin
  Sect := Sections[Section];
  if Sect <> nil then
  begin
    while Sect.Count > 0 do
    begin
      Item := Sect[0];
      Item.Free;
    end;
    Sect.FParent := nil;
    Sect.Free;
    FSections[Section] := nil;
  end;
  FSections.Delete(Section);
end;

procedure TJvSpeedBar.DeleteSection(Section: Integer);
var
  Sect: TJvSpeedBarSection;
  I: Integer;
begin
  Sect := Sections[Section];
  if Sect <> nil then
  begin
    for I := Sect.Count - 1 downto 0 do
      RemoveItem(TJvSpeedItem(Sect[I]));
    Sect.FParent := nil;
    FSections[Section] := nil;
  end;
  FSections.Delete(Section);
end;

procedure TJvSpeedBar.RemoveItem(Item: TJvSpeedItem);
var
  I, Index: Integer;
begin
  if FindItem(Item, I, Index) then
  begin
    Item.FButton.Parent := nil;
    Item.FParent := nil;
    Item.FSection := -1;
    Sections[I].FList.Delete(Index);
  end;
end;

function TJvSpeedBar.SearchSection(const ACaption: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FSections.Count - 1 do
    if Sections[I].Caption = ACaption then
    begin
      Result := I;
      Exit;
    end;
end;

function TJvSpeedBar.AppendSection(Value: TJvSpeedBarSection): Integer;
var
  UniqueName: string;
  I: Integer;
begin
  I := 0;
  UniqueName := Value.Caption;
  while SearchSection(UniqueName) >= 0 do
  begin
    Inc(I);
    UniqueName := Value.Caption + Format(' (%d)', [I]);
  end;
  Value.Caption := UniqueName;
  Result := FSections.Add(Value);
  if Result >= 0 then
  begin
    Value.FParent := Self;
    for I := 0 to Value.Count - 1 do
    begin
      Value[I].FSection := Result;
      SetItemParams(Value[I], not (csLoading in ComponentState));
    end;
  end;
end;

function TJvSpeedBar.AddSection(const ACaption: string): Integer;
var
  Section: TJvSpeedBarSection;
begin
  if Owner <> nil then
    Section := TJvSpeedBarSection.Create(Owner)
  else
    Section := TJvSpeedBarSection.Create(Self);
  Section.Caption := ACaption;
  Result := AppendSection(Section);
end;

procedure TJvSpeedBar.SetItemParams(Item: TJvSpeedItem; InitBounds: Boolean);
begin
  with Item do
  begin
    FParent := Self;
    with FButton do
    begin
      if InitBounds then
        SetBounds(0, 0, BtnWidth, BtnHeight);
      Style := FButtonStyle;
      Flat := (sbFlatBtns in Options);
      Transparent := (sbTransparentBtns in Options);
      GrayedInactive := (sbGrayedBtns in Options);
    end;
    SetEditing(FEditWin <> NullHandle);
  end;
end;

function TJvSpeedBar.NewItem(AOwner: TComponent; Section: Integer;
  const AName: string): TJvSpeedItem;
begin
  Result := nil;
  if (Section >= 0) and (Section < FSections.Count) then
  begin
    Result := TJvSpeedItem.Create(AOwner);
    try
      Sections[Section].FList.Add(Result);
      Result.FSection := Section;
      SetItemParams(Result, True);
      if AName <> '' then
        with Result do
        begin
          Name := AName;
          Caption := AName;
          FButton.Visible := False;
          FButton.Parent := Self;
        end;
    except
      Result.Free;
      raise;
    end;
  end;
end;

procedure TJvSpeedBar.AddItem(Section: Integer; Item: TJvSpeedItem);
var
  I, Index: Integer;
begin
  if FindItem(Item, I, Index) then
  begin
    Sections[I].FList.Delete(Index);
    if Section >= FSections.Count then
      Section := FSections.Count - 1;
    Sections[Section].FList.Add(Item);
    Item.FSection := Section;
    Exit;
  end;
  if (Section >= 0) and (Item <> nil) then
  begin
    if Assigned(FOnAddItem) then
    begin
      FOnAddItem(Item);
      Section := Item.FSection;
    end;
    if FSections.Count = 0 then
      Section := AddSection('')
    else
    if Section >= FSections.Count then
      Section := FSections.Count - 1;
    Sections[Section].FList.Add(Item);
    Item.FSection := Section;
    SetItemParams(Item, not (csLoading in ComponentState));
    Item.FButton.Visible := False;
    Item.FButton.Parent := Self;
  end;
end;

function TJvSpeedBar.FindItem(Item: TJvSpeedItem; var Section,
  Index: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  Section := -1;
  for I := 0 to FSections.Count - 1 do
    if FSections[I] <> nil then
    begin
      Index := Sections[I].FList.IndexOf(Item);
      if Index >= 0 then
      begin
        Section := I;
        Result := True;
        Exit;
      end;
    end;
end;

procedure TJvSpeedBar.AlignItemsToGrid;
begin
  ForEachItem(AlignItemToGrid, 0);
end;

procedure TJvSpeedBar.AlignItemToGrid(Item: TJvSpeedItem; Data: Longint);
begin
  if Item.Visible then
  begin
    if GetOrientation = boVertical then
    begin
      Item.Left := Trunc((Item.Left - FOffset.X) / FGridSize.X) * FGridSize.X + FOffset.X;
      Item.Top := Round((Item.Top - FOffset.Y) / FGridSize.Y) * FGridSize.Y + FOffset.Y;
    end
    else
    begin
      Item.Left := Round((Item.Left - FOffset.X) / FGridSize.X) * FGridSize.X + FOffset.X;
      Item.Top := Trunc((Item.Top - FOffset.Y) / FGridSize.Y) * FGridSize.Y + FOffset.Y;
    end;
  end;
end;

function TJvSpeedBar.AcceptDropItem(Item: TJvSpeedItem; X, Y: Integer): Boolean;
var
  I, Sect: Integer;
begin
  Result := False;
  if FindItem(Item, Sect, I) then
  begin
    if GetOrientation = boVertical then
    begin
      X := Trunc((X - FOffset.X) / FGridSize.X) * FGridSize.X + FOffset.X;
      Y := Round((Y - FOffset.Y) / FGridSize.Y) * FGridSize.Y + FOffset.Y;
    end
    else
    begin
      X := Round((X - FOffset.X) / FGridSize.X) * FGridSize.X + FOffset.X;
      Y := Trunc((Y - FOffset.Y) / FGridSize.Y) * FGridSize.Y + FOffset.Y;
    end;
    Item.Left := X;
    Item.Top := Y;
    Result := PtInRect(ClientRect, Point(X, Y));
    if Result then
      Item.FButton.BringToFront
    else
      Item.FButton.SendToBack;
    Item.Visible := Result;
  end;
end;

procedure TJvSpeedBar.SetItemEditing(Item: TJvSpeedItem; Data: Longint);
begin
  Item.SetEditing(FEditWin <> NullHandle);
end;

function TJvSpeedBar.GetEditing: Boolean;
begin
  Result := (FEditWin <> NullHandle);
end;

procedure TJvSpeedBar.SetEditing(Win: HWnd);
begin
  FEditWin := Win;
  ForEachItem(SetItemEditing, 0);
  if (FEditWin = NullHandle) and not (csDesigning in ComponentState) then
    AfterCustomize;
end;

procedure TJvSpeedBar.Paint;
var
  XCnt, YCnt, X, Y: Integer;
  BevelSize, SaveIndex: Integer;
  Rect: TRect;
  C1, C2: TColor;

  procedure BevelLine(C: TColor; X1, Y1, X2, Y2: Integer);
  begin
    with Canvas do
    begin
      Pen.Color := C;
      MoveTo(X1, Y1);
      LineTo(X2, Y2);
    end;
  end;

begin
  if not FLocked then
  begin
    Rect := ClientRect;
    BevelSize := BorderWidth;
    if BevelOuter <> bvNone then
      Inc(BevelSize, BevelWidth);
    if BevelInner <> bvNone then
      Inc(BevelSize, BevelWidth);
    InflateRect(Rect, -BevelSize, -BevelSize);
    inherited Paint;
        
    if (FWallpaper.Graphic <> nil) and (FWallpaper.Width > 0) and
      (FWallpaper.Height > 0) then
    begin
      SaveIndex := SaveDC(Canvas.Handle);
      try
        with Rect do
          IntersectClipRect(Canvas.Handle, Left, Top, Right - Left +
            BevelSize, Bottom - Top + BevelSize);
        if sbStretchBitmap in Options then
          Canvas.StretchDraw(Rect, FWallpaper.Graphic)
        else
        begin
          XCnt := (ClientWidth - 2 * BevelSize) div FWallpaper.Width;
          YCnt := (ClientHeight - 2 * BevelSize) div FWallpaper.Height;
          for X := 0 to XCnt do
            for Y := 0 to YCnt do
              Canvas.Draw(Rect.Left + X * FWallpaper.Width,
                Rect.Top + Y * FWallpaper.Height, FWallpaper.Graphic);
        end;
      finally
        RestoreDC(Canvas.Handle, SaveIndex);
      end;
    end;
    if FBoundLines <> [] then
    begin
      C1 := clBtnShadow;
      C2 := clBtnHighlight;
      if blTop in FBoundLines then
      begin
        BevelLine(C1, Rect.Left, Rect.Top, Rect.Right, Rect.Top);
        BevelLine(C2, Rect.Left, Rect.Top + 1, Rect.Right, Rect.Top + 1);
      end;
      if blLeft in FBoundLines then
      begin
        BevelLine(C1, Rect.Left, Rect.Top, Rect.Left, Rect.Bottom);
        BevelLine(C2, Rect.Left + 1, Rect.Top + Ord(blTop in FBoundLines), Rect.Left + 1, Rect.Bottom);
      end;
      if blBottom in FBoundLines then
      begin
        BevelLine(C1, Rect.Left, Rect.Bottom - 2, Rect.Right, Rect.Bottom - 2);
        BevelLine(C2, Rect.Left, Rect.Bottom - 1, Rect.Right, Rect.Bottom - 1);
      end;
      if blRight in FBoundLines then
      begin
        BevelLine(C1, Rect.Right - 2, Rect.Top, Rect.Right - 2, Rect.Bottom - Ord(blBottom in FBoundLines));
        BevelLine(C2, Rect.Right - 1, Rect.Top, Rect.Right - 1, Rect.Bottom);
      end;
    end;
  end;
end;

procedure TJvSpeedBar.ApplyOrientation(Value: TBarOrientation);
begin
  if (GetOrientation <> Value) and not (csReading in ComponentState) then
  begin
    FLocked := True;
    try
      FOrientation := Value;
      SwapInt(Integer(FButtonSize.X), Integer(FButtonSize.Y));
      SwapInt(Integer(FGridSize.X), Integer(FGridSize.Y));
      SwapInt(Integer(FOffset.X), Integer(FOffset.Y));
      ForEachItem(SwapItemBounds, 0);
    finally
      FLocked := False;
      Invalidate;
    end;
    if FEditWin <> NullHandle then
      SendMessage(FEditWin, CM_SPEEDBARCHANGED, SBR_BTNSIZECHANGED, Longint(Self));
  end;
end;

procedure TJvSpeedBar.SetOrientation(Value: TBarOrientation);
begin
  if GetOrientation <> Value then
  begin
    if FPosition = bpAuto then
      raise EJvSpeedbarError.Create(RsEAutoSpeedbarMode);
    ApplyOrientation(Value);
  end;
end;

function TJvSpeedBar.GetOrientation: TBarOrientation;
begin
  if FPosition = bpCustom then
    Result := FOrientation
  else
    case Align of
      alLeft, alRight:
        Result := boVertical;
      alTop, alBottom:
        Result := boHorizontal;
    else
      Result := FOrientation;
    end;
end;

function TJvSpeedBar.GetAlign: TAlign;
begin
  Result := FAlign;
end;

procedure TJvSpeedBar.SetAlign(Value: TAlign);
var
  X, Y: Integer;
begin
  { fix previous version error }
  if (csLoading in ComponentState) and (Value = alNone) and
    (Position = bpAuto) then
    FFix := True;
  if Align <> Value then
  begin
    X := Width;
    Y := Height;
    if (FPosition = bpAuto) and (Value in [alClient, alNone]) then
      raise EJvSpeedbarError.Create(RsEAutoSpeedbarMode);
    inherited Align := Value;
    if csLoading in ComponentState then
    begin
      Width := X;
      Height := Y;
    end;
    if FPosition = bpAuto then
      case Value of
        alLeft, alRight:
          ApplyOrientation(boVertical);
        alTop, alBottom:
          ApplyOrientation(boHorizontal);
      else
        if not (csLoading in ComponentState) then
          raise EJvSpeedbarError.Create(RsEAutoSpeedbarMode);
      end;
    FAlign := inherited Align;
  end;
end;

procedure TJvSpeedBar.ChangeScale(M, D, MH, DH: Integer);


var
  Flags: TSbScaleFlags;
begin
  DisableAlign;
  try
    if csLoading in ComponentState then
      Flags := ScaleFlags
    else
      Flags := [sfOffsetX, sfOffsetY, sfBtnSizeX, sfBtnSizeY];
    
    
    if (sfBtnSizeX in Flags) and not (csFixedWidth in ControlStyle) then
      FButtonSize.X := MulDiv(FButtonSize.X, MH, DH);
    if sfOffsetX in Flags then
      FOffset.X := MulDiv(FOffset.X, MH, DH);
    
    if (sfBtnSizeY in Flags) and not (csFixedHeight in ControlStyle) then
      FButtonSize.Y := MulDiv(FButtonSize.Y, M, D);
    if sfOffsetY in Flags then
      FOffset.Y := MulDiv(FOffset.Y, M, D);
    UpdateGridSize;
    inherited ChangeScale(M, D , MH, DH );
    ApplyButtonSize;
    AlignItemsToGrid;
    FScaleFlags := [];
  finally
    EnableAlign;
  end;
end;

procedure TJvSpeedBar.AlignControls(AControl: TControl; var Rect: TRect);
var
  P: TPoint;
  Min: Integer;
begin
  if FBoundLines <> [] then
  begin
    if blTop in FBoundLines then
      Inc(Rect.Top, 2);
    if blBottom in FBoundLines then
      Dec(Rect.Bottom, 2);
    if blLeft in FBoundLines then
      Inc(Rect.Left, 2);
    if blRight in FBoundLines then
      Dec(Rect.Right, 2);
  end;
  inherited AlignControls(AControl, Rect);
  Min := MinButtonsOffset;
  if FOffset.X < Min then
  begin
    P.X := Min - FOffset.X;
    FOffset.X := Min;
  end
  else
    P.X := 0;
  if FOffset.Y < Min then
  begin
    P.Y := Min - FOffset.Y;
    FOffset.Y := Min;
  end
  else
    P.Y := 0;
  if not (csLoading in ComponentState) and ((P.X <> 0) or (P.Y <> 0)) then
    ForEachItem(OffsetItem, Longint(@P));
end;

procedure TJvSpeedBar.FlatItem(Item: TJvSpeedItem; Data: Longint);
begin
  Item.FButton.Flat := Boolean(Data);
end;

procedure TJvSpeedBar.GrayedItem(Item: TJvSpeedItem; Data: Longint);
begin
  Item.FButton.GrayedInactive := Boolean(Data);
end;

procedure TJvSpeedBar.TransparentItem(Item: TJvSpeedItem; Data: Longint);
begin
  Item.FButton.Transparent := Boolean(Data);
end;

procedure TJvSpeedBar.SetBoundLines(Value: TBoundLines);
begin
  if FBoundLines <> Value then
  begin
    FBoundLines := Value;
    Realign;
    Invalidate;
  end;
end;

procedure TJvSpeedBar.SetOptions(Value: TJvSpeedBarOptions);
var
  FlatChanged: Boolean;
begin
  if FOptions <> Value then
  begin
    FlatChanged := (sbFlatBtns in FOptions) <> (sbFlatBtns in Value);
    FOptions := Value;
    ForEachItem(FlatItem, Longint(sbFlatBtns in Options));
    ForEachItem(TransparentItem, Longint(sbTransparentBtns in Options));
    ForEachItem(GrayedItem, Longint(sbGrayedBtns in Options));
    UpdateGridSize;
    if FlatChanged then
      Realign;
    Invalidate;
  end;
end;

procedure TJvSpeedBar.OffsetItem(Item: TJvSpeedItem; Data: Longint);
var
  P: TPoint;
begin
  P := PPoint(Data)^;
  Item.FButton.SetBounds(Item.Left + P.X, Item.Top + P.Y, FButtonSize.X,
    FButtonSize.Y);
end;

function TJvSpeedBar.GetButtonsOffset(Index: Integer): Integer;
begin
  if Index = 0 then
    Result := FOffset.X
  else
  if Index = 1 then
    Result := FOffset.Y
  else
    Result := 0;
end;

procedure TJvSpeedBar.SetButtonsOffset(Index: Integer; Value: Integer);
var
  P: TPoint;
begin
  if Value < MinButtonsOffset then
    Value := MinButtonsOffset;
  P.X := 0;
  P.Y := 0;
  if Index = 0 then
  begin
    P.X := Value - FOffset.X;
    FOffset.X := Value;
    Include(FScaleFlags, sfOffsetX);
  end
  else
  if Index = 1 then
  begin
    P.Y := Value - FOffset.Y;
    FOffset.Y := Value;
    Include(FScaleFlags, sfOffsetY);
  end;
  if (P.X <> 0) or (P.Y <> 0) then
    ForEachItem(OffsetItem, Longint(@P));
end;

procedure TJvSpeedBar.UpdateGridSize;
var
  Base: Integer;
begin
  case Orientation of
    boHorizontal:
      Base := FButtonSize.X;
  else {boVertical:}
    Base := FButtonSize.Y;
  end;
  case Orientation of
    boHorizontal:
      begin
        FGridSize.X := Max(1, Min(8, Base div 3));
        while Base mod FGridSize.X <> 0 do
          Inc(FGridSize.X);
        if (FGridSize.X = Base) and (Base > 1) then
        begin
          Dec(FGridSize.X);
          while (FGridSize.X > 1) and (Base mod FGridSize.X <> 0) do
            Dec(FGridSize.X);
        end;
        FGridSize.Y := FButtonSize.Y;
      end;
    boVertical:
      begin
        FGridSize.Y := Max(1, Min(8, Base div 3));
        while (Base mod FGridSize.Y <> 0) do
          Inc(FGridSize.Y);
        if (FGridSize.Y = Base) and (Base > 1) then
        begin
          Dec(FGridSize.Y);
          while (FGridSize.Y > 1) and (Base mod FGridSize.Y <> 0) do
            Dec(FGridSize.Y);
        end;
        FGridSize.X := FButtonSize.X;
      end;
  end;
end;

procedure TJvSpeedBar.ApplyItemSize(Item: TJvSpeedItem; Data: Longint);
begin
  with Item do
    FButton.SetBounds(FButton.Left, FButton.Top, FButtonSize.X, FButtonSize.Y);
end;

procedure TJvSpeedBar.ApplyButtonSize;
begin
  ForEachItem(ApplyItemSize, 0);
  if FEditWin <> NullHandle then { update SpeedBar editor }
    SendMessage(FEditWin, CM_SPEEDBARCHANGED, SBR_BTNSIZECHANGED, Longint(Self));
end;

function TJvSpeedBar.GetButtonSize(Index: Integer): Integer;
begin
  if Index = 0 then
    Result := FButtonSize.X
  else
  if Index = 1 then
    Result := FButtonSize.Y
  else
    Result := 0;
end;

procedure TJvSpeedBar.SetButtonSize(Index, Value: Integer);
var
  NewSize: TPoint;
begin
  NewSize.X := FButtonSize.X;
  NewSize.Y := FButtonSize.Y;
  if Index = 0 then
  begin
    NewSize.X := Value;
    Include(FScaleFlags, sfBtnSizeX);
  end
  else
  if Index = 1 then
  begin
    NewSize.Y := Value;
    Include(FScaleFlags, sfBtnSizeY);
  end
  else
    Exit;
  FButtonSize := NewSize;
  UpdateGridSize;
  if not (csReading in ComponentState) then
    case Orientation of
      boHorizontal:
        ClientHeight := Max(ClientHeight, 2 * FOffset.Y + FButtonSize.Y);
      boVertical:
        ClientWidth := Max(ClientWidth, 2 * FOffset.X + FButtonSize.X);
    end;
  ApplyButtonSize;
end;

procedure TJvSpeedBar.GetChildren(Proc: TGetChildProc ; Root: TComponent);
var
  I, Idx: Integer;
  Sect: TJvSpeedBarSection;
  Item: TJvSpeedItem;
begin
  inherited GetChildren(Proc, Root);
  for I := 0 to FSections.Count - 1 do
  begin
    Sect := Sections[I];
    if Sect <> nil then
      Proc(Sect);
  end;
  for I := 0 to FSections.Count - 1 do
  begin
    Sect := Sections[I];
    if Sect <> nil then
      for Idx := 0 to Sect.Count - 1 do
      begin
        Item := Sect[Idx];
        if (Item <> nil) and (Item.Owner <> Self) then
          Proc(Item);
      end;
  end;
end;

procedure TJvSpeedBar.SetChildOrder(Component: TComponent; Order: Integer);
begin
  if FSections.IndexOf(Component) >= 0 then
    (Component as TJvSpeedBarSection).Index := Order;
end;

procedure TJvSpeedBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FImages then
      SetImages(nil);
end;

procedure TJvSpeedBar.InvalidateItem(Item: TJvSpeedItem; Data: Longint);
begin
  with Item do
    if Button <> nil then
    begin
      TJvSpeedBarButton(Button).InvalidateGlyph;
      if FImageIndex >= 0 then
        Button.Invalidate;
    end;
end;

procedure TJvSpeedBar.ImageListChange(Sender: TObject);
begin
  ForEachItem(InvalidateItem, 0);
end;

procedure TJvSpeedBar.SetImages(Value: TImageList);
begin
  if Images <> nil then
    Images.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if FImages <> nil then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end;
  ImageListChange(FImages);
end;

function TJvSpeedBar.SearchItem(const ItemName: string): TJvSpeedItem;
var
  I, Idx: Integer;
  Sect: TJvSpeedBarSection;
  Item: TJvSpeedItem;
begin
  Result := nil;
  for I := 0 to FSections.Count - 1 do
    if FSections[I] <> nil then
    begin
      Sect := TJvSpeedBarSection(FSections[I]);
      for Idx := 0 to Sect.Count - 1 do
        if Sect[Idx] <> nil then
        begin
          Item := TJvSpeedItem(Sect[Idx]);
          if AnsiCompareText(Item.Name, ItemName) = 0 then
          begin
            Result := Item;
            Exit;
          end;
        end;
    end;
end;

type
  TJvSpeedBarPos = (bpTop, bpBottom, bpLeft, bpRight);

const
  PosToAlign: array [TJvSpeedBarPos] of TAlign = (alTop, alBottom, alLeft, alRight);

function TJvSpeedBar.GetFramePos(X, Y: Integer; var Apply: Boolean): Integer;
var
  P: TPoint;
  W, H: Double;
begin
  P := Parent.ScreenToClient(ClientToScreen(Point(X, Y)));
  W := Parent.ClientWidth;
  H := Parent.ClientHeight;
  if P.Y <= P.X * (H / W) then
  begin { top or right }
    if P.Y >= H * (1 - P.X / W) then
      Result := Ord(bpRight)
    else
      Result := Ord(bpTop);
  end
  else
  begin { left or bottom }
    if P.Y >= H * (1 - P.X / W) then
      Result := Ord(bpBottom)
    else
      Result := Ord(bpLeft);
  end;
  if Assigned(FOnApplyAlign) then
    FOnApplyAlign(Self, PosToAlign[TJvSpeedBarPos(Result)], Apply);
end;

function TJvSpeedBar.GetFrameRect(X, Y: Integer): TRect;
var
  Pos: TJvSpeedBarPos;
  W: Integer;
  Apply: Boolean;

  function InsertBefore(C1, C2: TControl; AAlign: TAlign): Boolean;
  begin
    Result := False;
    case AAlign of
      alTop:
        Result := C1.Top < C2.Top;
      alBottom:
        Result := (C1.Top + C1.Height) > (C2.Top + C2.Height);
      alLeft:
        Result := C1.Left < C2.Left;
      alRight:
        Result := (C1.Left + C1.Width) > (C2.Left + C2.Width);
    end;
  end;

  function MaxRect: TRect;
  var
    I: Integer;
    Control: TControl;
  begin
    Result := Parent.ClientRect;
    for I := 0 to Parent.ControlCount - 1 do
    begin
      Control := Parent.Controls[I];
      if (Control.Visible) and (Control <> Self) and not
        (Control.Align in [alNone, alClient]) then
      begin
        if (Control.Align > PosToAlign[Pos]) or ((Control.Align = PosToAlign[Pos])
          and not InsertBefore(Control, Self, Control.Align)) then
          Continue;
        case Control.Align of
          alTop:
            Inc(Result.Top, Control.Height);
          alBottom:
            Dec(Result.Bottom, Control.Height);
          alLeft:
            Inc(Result.Left, Control.Width);
          alRight:
            Dec(Result.Right, Control.Width);
        end;
      end;
    end;
  end;

begin
  Apply := True;
  Pos := TJvSpeedBarPos(GetFramePos(X, Y, Apply));
  if Apply then
  begin
    Result := MaxRect;
    FPrevAlign := PosToAlign[Pos];
  end
  else
  begin
    Result := FPrevRect;
    Exit;
  end;
  with Result do
  begin
    TopLeft := Parent.ClientToScreen(TopLeft);
    BottomRight := Parent.ClientToScreen(BottomRight);
  end;
  case GetOrientation of
    boHorizontal:
      W := Height;
    boVertical:
      W := Width;
  else
    W := 0;
  end;
  case Pos of
    bpTop:
      Result.Bottom := Result.Top + W;
    bpBottom:
      Result.Top := Result.Bottom - W;
    bpLeft:
      Result.Right := Result.Left + W;
    bpRight:
      Result.Left := Result.Right - W;
  end;
end;

procedure TJvSpeedBar.StartDragFrame;
var
  Rect: TRect;
begin
  with Rect do
  begin
    TopLeft := ClientToScreen(Point(0, 0));
    BottomRight := ClientToScreen(Point(Width, Height));
  end;
  FPrevRect := Rect;
  FPrevAlign := Align;
  DrawInvertFrame(FPrevRect, DragFrameWidth);
  SetCursor(Screen.Cursors[crDragHand]);
  FDrag := True;
end;

procedure TJvSpeedBar.DragFrame(X, Y: Integer);
var
  Rect: TRect;
begin
  Rect := GetFrameRect(X, Y);
  if not EqualRect(Rect, FPrevRect) then
  begin
    DrawInvertFrame(FPrevRect, DragFrameWidth);
    SetCursor(Screen.Cursors[crDragHand]);
    FPrevRect := Rect;
    DrawInvertFrame(FPrevRect, DragFrameWidth);
  end;
end;

procedure TJvSpeedBar.StopDragFrame(X, Y: Integer);
var
  Pos: TJvSpeedBarPos;
  Apply: Boolean;
begin
  DrawInvertFrame(FPrevRect, DragFrameWidth);
  SetCursor(Screen.Cursors[Cursor]);
  FDrag := False;
  if Align in [alLeft, alTop, alRight, alBottom] then
  begin
    Apply := True;
    Pos := TJvSpeedBarPos(GetFramePos(X, Y, Apply));
    Parent.DisableAlign;
    try
      if Apply then
        Align := PosToAlign[Pos]
      else
        Align := FPrevAlign;
    finally
      Parent.EnableAlign;
    end;
    PosChanged;
  end;
end;

function TJvSpeedBar.CheckResize(Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
  if (FEditWin <> NullHandle) and (sbAllowResize in Options) and not FDrag then
  begin
    if (Align in [alTop, alBottom]) and (X > 0) and (X <= ClientWidth) then
    begin
      case Align of
        alTop:
          Result := (Y > ClientHeight - StartDragOffset) and
            (Y <= ClientHeight + StartDragOffset);
        alBottom:
          Result := (Y > -StartDragOffset) and (Y <= StartDragOffset);
      end;
      if Result then
        SetCursor(Screen.Cursors[crSizeNS]);
    end;
    if (Align in [alLeft, alRight]) and (Y > 0) and (Y <= ClientHeight) then
    begin
      case Align of
        alLeft:
          Result := (X > ClientWidth - StartDragOffset) and
            (X <= ClientWidth + StartDragOffset);
        alRight:
          Result := (X > -StartDragOffset) and (X <= StartDragOffset);
      end;
      if Result then
        SetCursor(Screen.Cursors[crSizeWE]);
    end;
  end;
end;

procedure TJvSpeedBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and (Parent <> nil) and CheckResize(Shift, X, Y) then
  begin
    FResizing := True;
    MouseCapture := True;
    Exit;
  end;
  if (Button = mbLeft) and (Parent <> nil) and (sbAllowDrag in Options) and
    (Align in [alLeft, alTop, alRight, alBottom]) then
  begin
    MouseCapture := True;
    FStartDrag := Point(X, Y);
  end;
end;

procedure TJvSpeedBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Cnt: Integer;
  P: TPoint;
begin
  inherited MouseMove(Shift, X, Y);
  CheckResize(Shift, X, Y);
  Cnt := 0;
  if (GetCapture = Handle) and (csLButtonDown in ControlState) then
    if FResizing then
    begin
      P := Parent.ScreenToClient(ClientToScreen(Point(X, Y)));
      if not PtInRectInclusive(Parent.ClientRect,P) then
        Exit;
      case Align of
        alTop:
          Cnt := Abs(Y - (2 * FOffset.Y)) div BtnHeight;
        alLeft:
          Cnt := Abs(X - (2 * FOffset.X)) div BtnWidth;
        alBottom:
          Cnt := Abs(ClientHeight - (2 * FOffset.Y) - Y) div BtnHeight;
        alRight:
          Cnt := Abs(ClientWidth - (2 * FOffset.X) - X) div BtnWidth;
      end;
      Cnt := Max(1, Cnt);
      case Align of
        alTop, alBottom:
          begin
            SetCursor(Screen.Cursors[crSizeNS]);
            Height := Min(BtnHeight * Cnt + (2 * FOffset.Y), Parent.ClientHeight);
          end;
        alLeft, alRight:
          begin
            SetCursor(Screen.Cursors[crSizeWE]);
            Width := Min(BtnWidth * Cnt + (2 * FOffset.X), Parent.ClientWidth);
          end;
      end;
    end
    else
    if sbAllowDrag in Options then
    begin
      if FDrag then
        DragFrame(X, Y)
      else
      begin
        if (Abs(X - FStartDrag.X) > StartDragOffset) or
          (Abs(Y - FStartDrag.Y) > StartDragOffset) then
          StartDragFrame;
      end;
    end;
end;

procedure TJvSpeedBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if FResizing then
    begin
      FResizing := False;
      SetCursor(Screen.Cursors[Cursor]);
    end;
    if FDrag then
      StopDragFrame(X, Y);
    MouseCapture := False;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvSpeedBar.PosChanged;
begin
  if Assigned(FOnPosChanged) then
    FOnPosChanged(Self);
end;

procedure TJvSpeedBar.AfterCustomize;
begin
  if Assigned(FOnCustomize) then
    FOnCustomize(Self);
end;

function TJvSpeedBar.GetStorage: TJvFormPlacement;
begin
  Result := FIniLink.Storage;
end;

procedure TJvSpeedBar.SetStorage(Value: TJvFormPlacement);
begin
  FIniLink.Storage := Value;
end;

procedure TJvSpeedBar.Customize(HelpCtx: THelpContext);
begin
  ShowSpeedbarSetupWindow(Self, HelpCtx);
end;

procedure TJvSpeedBar.IniSave(Sender: TObject);
begin
  if (Name <> '') and IniStorage.IsActive then
    SaveToAppStorage(IniStorage.AppStorage, IniStorage.AppStorage.ConcatPaths([
      IniStorage.AppStoragePath, GetDefaultSection(Self)]));
end;

procedure TJvSpeedBar.IniLoad(Sender: TObject);
begin
  if (Name <> '') and IniStorage.IsActive then
    LoadFromAppStorage(IniStorage.AppStorage, IniStorage.AppStorage.ConcatPaths([
      IniStorage.AppStoragePath, GetDefaultSection(Self)]));
end;

const
  { The following strings should not be localized }
  sPosition = 'Position';
  sCount = 'Count';
  sBtn = 'Button';
  sVer = 'Version';
  sPixelsPerInch = 'PixelsPerInch';
  sBtnWidth = 'BtnWidth';
  sBtnHeight = 'BtnHeight';
  sBarWidth = 'Width';

type
  PIniData = ^TIniData;
  TIniData = record
    AppStorage: TJvCustomAppStorage;
    I: Integer;
    Path: string;
  end;

procedure TJvSpeedBar.HideItem(Item: TJvSpeedItem; Data: Longint);
begin
  Item.Visible := False;
end;

procedure TJvSpeedBar.WriteItemLayout(Item: TJvSpeedItem; Data: Longint);
begin
  if Item.Visible and Item.Stored then
  begin
    Inc(PIniData(Data)^.I);
    with PIniData(Data)^ do
      AppStorage.WriteString(AppStorage.ConcatPaths([Path, sBtn + IntToStr(I)]),
      Format('%s,%d,%d', [Item.Name, Item.Left, Item.Top]));
  end;
end;

procedure TJvSpeedBar.SaveLayout;
begin
  Save;
end;

procedure TJvSpeedBar.RestoreLayout;
begin
  Load;
end;

procedure TJvSpeedBar.LoadFromAppStorage(const AppStorage: TJvCustomAppStorage; const Path: string);
const
  Delims = [' ', ','];
var
  Item: TJvSpeedItem;
  Count: Integer;
  I: Integer;
  S: string;
begin
  FPrevAlign := Align;
  if AppStorage.ReadInteger(AppStorage.ConcatPaths([Path, sVer]), FVersion) < FVersion then
    // (marcelb) shouldn't we raise an exception "Invalid version" here?
    Exit;
  if sbAllowDrag in Options then
  try
    Align := TAlign(AppStorage.ReadInteger(AppStorage.ConcatPaths([Path, sPosition]), Integer(Align)));
  except
    Align := alTop;
  end;
  if Owner is TCustomForm then
    I := TForm(Owner).PixelsPerInch
  else
    I := 0;
  if Screen.PixelsPerInch <> AppStorage.ReadInteger(AppStorage.ConcatPaths([Path, sPixelsPerInch]), I) then
  begin
    if FPrevAlign <> Align then
      PosChanged;
    Exit;
  end;
  if sbAllowResize in Options then
  begin
    if Align in [alTop, alBottom] then
      Height := AppStorage.ReadInteger(AppStorage.ConcatPaths([Path, sBarWidth]), Height)
    else
    if Align in [alLeft, alRight] then
      Width := AppStorage.ReadInteger(AppStorage.ConcatPaths([Path, sBarWidth]), Width);
  end;
  if FPrevAlign <> Align then
    PosChanged;
  {if (AppStorage.ReadInteger(AppStorage.ConcatPaths([Path, sBtnWidth]), FButtonSize.X) >
    FButtonSize.X) or (AppStorage.ReadInteger(AppStorage.ConcatPaths([Path, sBtnHeight]),
    FButtonSize.Y) > FButtonSize.Y) then Exit;}
  Count := AppStorage.ReadInteger(AppStorage.ConcatPaths([Path, sCount]), 0);
  if Count > 0 then
  begin
    ForEachItem(HideItem, 0);
    for I := 1 to Count do
    begin
      S := AppStorage.ReadString(AppStorage.ConcatPaths([Path, sBtn + IntToStr(I)]), '');
      if S <> '' then
      begin
        Item := SearchItem(ExtractWord(1, S, Delims));
        if Item <> nil then
        begin
          Item.Left := Max(StrToIntDef(ExtractWord(2, S, Delims), Item.Left),
            FOffset.X);
          Item.Top := Max(StrToIntDef(ExtractWord(3, S, Delims), Item.Top),
            FOffset.Y);
          Item.Visible := True;
        end;
      end;
    end;
  end;
  Repaint;
end;

procedure TJvSpeedBar.SaveToAppStorage(const AppStorage: TJvCustomAppStorage; const Path: string);
var
  Data: TIniData;
begin
  Data.AppStorage := AppStorage;
  Data.Path := Path;
  Data.I := 0;
  AppStorage.DeleteSubTree(Path);
  AppStorage.WriteInteger(AppStorage.ConcatPaths([Path, sPosition]), Integer(Align));
  if Align in [alTop, alBottom] then
    AppStorage.WriteInteger(AppStorage.ConcatPaths([Path, sBarWidth]), Height)
  else
  if Align in [alLeft, alRight] then
    AppStorage.WriteInteger(AppStorage.ConcatPaths([Path, sBarWidth]), Width);
  AppStorage.WriteInteger(AppStorage.ConcatPaths([Path, sVer]), FVersion);
  AppStorage.WriteInteger(AppStorage.ConcatPaths([Path, sPixelsPerInch]), Screen.PixelsPerInch);
  AppStorage.WriteInteger(AppStorage.ConcatPaths([Path, sBtnWidth]), FButtonSize.X);
  AppStorage.WriteInteger(AppStorage.ConcatPaths([Path, sBtnHeight]), FButtonSize.Y);
  ForEachItem(WriteItemLayout, Longint(@Data));
  AppStorage.WriteInteger(AppStorage.ConcatPaths([Path, sCount]), Data.I);
end;

procedure TJvSpeedBar.Load;
begin
  IniLoad(nil);
end;

procedure TJvSpeedBar.Save;
begin
  IniSave(nil);
end;

//=== TJvBtnControl ==========================================================

constructor TJvBtnControl.Create(AOwner: TComponent);
begin
  FImage := TJvButtonImage.Create;
  inherited Create(AOwner);
  Cursor := crDragHand;
  FSpacing := 1;
  FMargin := -1;
  FLayout := blGlyphTop;
  FImageIndex := -1;
end;

destructor TJvBtnControl.Destroy;
begin
  FImage.Free;
  inherited Destroy;
end;



procedure TJvBtnControl.AssignSpeedItem(Item: TJvSpeedItem);
begin
  Alignment := Item.FButton.Alignment;
  Glyph := Item.Glyph;
  NumGlyphs := Item.NumGlyphs;
  Spacing := Item.Spacing;
  Margin := Item.Margin;
  Layout := Item.Layout;
  Caption := Item.BtnCaption;
  WordWrap := Item.WordWrap;
  ImageIndex := Item.ImageIndex;
  if Item.SpeedBar <> nil then
    Images := Item.SpeedBar.Images
  else
    Images := nil;
  Font := Item.Font;
  
  SetBounds(0, 0, Item.SpeedBar.BtnWidth, Item.SpeedBar.BtnHeight);
end;

function TJvBtnControl.GetGlyph: TBitmap;
begin
  Result := FImage.Glyph;
end;

function TJvBtnControl.GetNumGlyphs: TJvNumGlyphs;
begin
  Result := FImage.NumGlyphs;
end;

function TJvBtnControl.GetCaption: TCaption;
begin
  Result := FImage.Caption;
end;

procedure TJvBtnControl.SetCaption(const Value: TCaption);
begin
  FImage.Caption := Value;
end;

procedure TJvBtnControl.SetNumGlyphs(Value: TJvNumGlyphs);
begin
  FImage.NumGlyphs := Value;
end;

procedure TJvBtnControl.SetGlyph(Value: TBitmap);
begin
  FImage.Glyph := Value;
end;

function TJvBtnControl.GetWordWrap: Boolean;
begin
  Result := FImage.WordWrap;
end;

procedure TJvBtnControl.SetWordWrap(Value: Boolean);
begin
  FImage.WordWrap := Value;
end;

function TJvBtnControl.GetAlignment: TAlignment;
begin
  Result := FImage.Alignment;
end;

procedure TJvBtnControl.SetAlignment(Value: TAlignment);
begin
  FImage.Alignment := Value;
end;

procedure TJvBtnControl.DoBoundsChanged;
begin
  FImage.ButtonSize := Point(ClientWidth, ClientHeight);
  inherited DoBoundsChanged;
end;

procedure TJvBtnControl.Paint;
begin
  FImage.DrawEx(Canvas, 0, 0, Margin, Spacing, Layout, Font, Images,
    ImageIndex,
    
    
    Alignments[Alignment]
    
    );
end;

procedure TJvBtnControl.Activate(Rect: TRect);
begin
  if IsRectEmpty(BoundsRect) then
    BoundsRect := Rect;
  SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, 0,
    0, SWP_SHOWWINDOW or SWP_NOACTIVATE or SWP_NOSIZE);
  SetCursor(Screen.Cursors[Cursor]);
end;

procedure TJvBtnControl.ReleaseHandle;
begin
  DestroyHandle;
end;

{ Utility routines }

function NewSpeedSection(ASpeedbar: TJvSpeedBar; const ACaption: string): Integer;
begin
  Result := ASpeedbar.AddSection(ACaption);
end;

function NewSpeedItem(AOwner: TComponent; ASpeedbar: TJvSpeedBar; Section: Integer;
  const AName: string): TJvSpeedItem;
begin
  Result := ASpeedBar.NewItem(AOwner, Section, AName);
end;

function FindSpeedBar(const Pos: TPoint): TJvSpeedBar;
var
  Window: TWinControl;
  Handle: HWnd;
begin
  Result := nil;
  Handle := WindowFromPoint(Pos);
  Window := nil;
  while (Handle <> NullHandle) and (Window = nil) do
  begin
    Window := FindControl(Handle);
    if Window = nil then
      Handle := GetParent(Handle);
  end;
  if Window <> nil then
  begin
    if Window is TJvSpeedBar then
      Result := Window as TJvSpeedBar;
  end;
end;

procedure DrawCellButton(Grid: TDrawGrid; R: TRect; Item: TJvSpeedItem;
  Image: TJvButtonImage; ARightToLeft: Boolean = False);
var
  FBar: TJvSpeedBar;
  AFont: TFont;
  ImageList: TImageList;
begin
  if Item <> nil then
  begin
    FBar := Item.SpeedBar;
    AFont := nil;
    ImageList := nil;
    if FBar <> nil then
    begin
      AFont := FBar.Font;
      if Item.ImageIndex >= 0 then
        ImageList := FBar.Images;
    end;
    if ImageList = nil then
      Image.Glyph := Item.Glyph
    else
      Image.Glyph := nil;
    with Image do
    begin
      Alignment := Item.FButton.Alignment;
      NumGlyphs := Item.NumGlyphs;
      Caption := Item.BtnCaption;
      WordWrap := Item.WordWrap;
      if FBar <> nil then
        ButtonSize := Point(FBar.BtnWidth, FBar.BtnHeight);
    end;
    Image.DrawEx(Grid.Canvas, R.Left + 1, R.Top + 1, Item.Margin,
      Item.Spacing, Item.Layout, AFont, ImageList, Item.ImageIndex,
      
      
      Alignments[Image.Alignment]
      
      );
    Inc(R.Left, Image.ButtonSize.X + 3);
    DrawCellText(Grid, 0, 0, Item.Caption, R, taLeftJustify, vaCenterJustify, ARightToLeft);
  end;
end;

end.

