{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDockVIDVCStyle.pas, released on 2003-12-31.

The Initial Developer of the Original Code is luxiaoban.
Portions created by luxiaoban are Copyright (C) 2002,2003 luxiaoban.
All Rights Reserved.

Contributor(s):
devedit

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
//  $Id$

{$I jvcl.inc}

unit JvDockVIDVCStyle;

interface

uses
  Windows, Messages, Classes, Controls, Graphics, ComCtrls, ImgList,
  JvDockControlForm, JvDockSupportControl, JvDockTree;

const
  VIDDefaultDockGrabbersSize = 18;
  VIDDefaultDockSplitterWidth = 4;
  HTEXPAND = 31;

type
  TJvDockVIDVCConjoinServerOption = class(TJvDockBasicConjoinServerOption)
  private
    FTextEllipsis: Boolean;
    FTextAlignment: TAlignment;
    FInactiveTitleEndColor: TColor;
    FInactiveTitleStartColor: TColor;
    FActiveTitleEndColor: TColor;
    FActiveTitleStartColor: TColor;
    FSystemInfo: Boolean;
    FActiveFont: TFont;
    FInactiveFont: TFont;
    procedure SetActiveTitleEndColor(const Value: TColor);
    procedure SetActiveTitleStartColor(const Value: TColor);
    procedure SetInactiveTitleEndColor(const Value: TColor);
    procedure SetInactiveTitleStartColor(const Value: TColor);
    procedure SetTextAlignment(const Value: TAlignment);
    procedure SetTextEllipsis(const Value: Boolean);
    procedure SetSystemInfo(const Value: Boolean);
    procedure SetActiveFont(const Value: TFont);
    procedure SetInactiveFont(const Value: TFont);
  protected
    procedure ResetDockControlOption; override;
    procedure SetDefaultSystemCaptionInfo; virtual;
  public
    constructor Create(ADockStyle: TJvDockBasicStyle); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetActiveTitleEndColor_WithoutChangeSystemInfo(const Value: TColor);
    procedure SetActiveTitleStartColor_WithoutChangeSystemInfo(const Value: TColor);
    procedure SetInactiveTitleEndColor_WithoutChangeSystemInfo(const Value: TColor);
    procedure SetInactiveTitleStartColor_WithoutChangeSystemInfo(const Value: TColor);
    procedure SetTextAlignment_WithoutChangeSystemInfo(const Value: TAlignment);
    procedure SetTextEllipsis_WithoutChangeSystemInfo(const Value: Boolean);
    procedure SetActiveFont_WithoutChangeSystemInfo(const Value: TFont);
    procedure SetInactiveFont_WithoutChangeSystemInfo(const Value: TFont);
  published
    property ActiveFont: TFont read FActiveFont write SetActiveFont;
    property InactiveFont: TFont read FInactiveFont write SetInactiveFont;
    property TextAlignment: TAlignment read FTextAlignment write SetTextAlignment;
    property ActiveTitleStartColor: TColor read FActiveTitleStartColor write SetActiveTitleStartColor;
    property ActiveTitleEndColor: TColor read FActiveTitleEndColor write SetActiveTitleEndColor;
    property InactiveTitleStartColor: TColor read FInactiveTitleStartColor write SetInactiveTitleStartColor;
    property InactiveTitleEndColor: TColor read FInactiveTitleEndColor write SetInactiveTitleEndColor;
    property TextEllipsis: Boolean read FTextEllipsis write SetTextEllipsis;
    property SystemInfo: Boolean read FSystemInfo write SetSystemInfo;
  end;

  TJvDockZoneSizeStyle = (zssMinimum, zssNormal, zssMaximum);

  TJvDockVIDVCTabServerOption = class(TJvDockBasicTabServerOption)
  private
    FActiveFont: TFont;
    FActiveSheetColor: TColor;
    FHotTrackColor: TColor;
    FInactiveFont: TFont;
    FInactiveSheetColor: TColor;
    FShowTabImages: Boolean;
    function GetActiveFont: TFont;
    function GetActiveSheetColor: TColor;
    function GetHotTrackColor: TColor;
    function GetInactiveFont: TFont;
    function GetInactiveSheetColor: TColor;
    function GetShowTabImages: Boolean;
    procedure SetActiveFont(const Value: TFont);
    procedure SetActiveSheetColor(const Value: TColor);
    procedure SetHotTrackColor(const Value: TColor);
    procedure SetInactiveFont(const Value: TFont);
    procedure SetInactiveSheetColor(const Value: TColor);
    procedure SetShowTabImages(const Value: Boolean);
  protected
    procedure ResetDockControlOption; override;
    procedure ResetTabPageControl(APage: TJvDockTabPageControl); override;
  public
    constructor Create(ADockStyle: TJvDockBasicStyle); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetTabPosition(const Value: TTabPosition); override;
  published
    property ActiveSheetColor: TColor read GetActiveSheetColor write SetActiveSheetColor;
    property InactiveSheetColor: TColor read GetInactiveSheetColor write SetInactiveSheetColor;
    property ActiveFont: TFont read GetActiveFont write SetActiveFont;
    property InactiveFont: TFont read GetInactiveFont write SetInactiveFont;
    property HotTrackColor: TColor read GetHotTrackColor write SetHotTrackColor;
    property ShowTabImages: Boolean read GetShowTabImages write SetShowTabImages;
  end;

  TJvDockSystemInfoChange = procedure(Value: Boolean) of object;

  TJvDockVIDVCStyle = class(TJvDockAdvStyle)
  private
    FSystemInfoChange: TJvDockSystemInfoChange;
  protected
    function DockClientWindowProc(DockClient: TJvDockClient; var Msg: TMessage): Boolean; override;
    procedure ParentFormWindowProc(var Msg: TMessage); override;
    procedure FormDockDrop(DockClient: TJvDockClient;
      Source: TJvDockDragDockObject; X, Y: Integer); override;
    procedure FormGetSiteInfo(Source: TJvDockDragDockObject; DockClient: TJvDockClient;
      Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean); override;
    procedure FormDockOver(DockClient: TJvDockClient; Source: TJvDockDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure FormStartDock(DockClient: TJvDockClient;
      var Source: TJvDockDragDockObject); override;
    procedure FormGetDockEdge(DockClient: TJvDockClient; Source: TJvDockDragDockObject;
      MousePos: TPoint; var DropAlign: TAlign); override;
    procedure CreateConjoinServerOption(var Option: TJvDockBasicConjoinServerOption); override;
    procedure CreateTabServerOption(var Option: TJvDockBasicTabServerOption); override;
    procedure AssignConjoinServerOption(APanel: TJvDockCustomPanel); override;
    procedure AssignTabServerOption(APage: TJvDockTabPageControl); override;
    procedure DoSystemInfoChange(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    {$IFNDEF USEJVCL}
    function GetControlName: string; override;
    {$ENDIF USEJVCL}
    procedure SetDockBaseControl(IsCreate: Boolean; DockBaseControl: TJvDockBaseControl); override;
  published
    property SystemInfoChange: TJvDockSystemInfoChange read FSystemInfoChange
      write FSystemInfoChange;
    property ConjoinServerOption;
    property TabServerOption;
  end;

  TJvDockVIDVCSplitter = class(TJvDockSplitter)
  protected
    procedure Paint; override;
  end;

  TJvDockVIDVCPanel = class(TJvDockAdvPanel)
  protected
    procedure CustomGetSiteInfo(Source: TJvDockDragDockObject;
      Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean); override;
    procedure CustomStartDock(var Source: TJvDockDragDockObject); override;
    procedure CustomDockDrop(Source: TJvDockDragDockObject; X, Y: Integer); override;
    procedure CustomDockOver(Source: TJvDockDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure CustomGetDockEdge(Source: TJvDockDragDockObject; MousePos: TPoint;
      var DropAlign: TAlign); override;
    function CreateDockManager: IDockManager; override;
  public
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
    procedure UpdateCaption(Exclude: TControl); override;
  end;

  TJvDockVIDVCConjoinPanel = class(TJvDockAdvConjoinPanel)
  protected
    procedure CustomGetSiteInfo(Source: TJvDockDragDockObject;
      Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean); override;
    procedure CustomDockOver(Source: TJvDockDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure CustomGetDockEdge(Source: TJvDockDragDockObject; MousePos: TPoint; var DropAlign: TAlign); override;
    function CustomUnDock(Source: TJvDockDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    procedure CustomDockDrop(Source: TJvDockDragDockObject; X, Y: Integer); override;
    function CreateDockManager: IDockManager; override;
  public
    procedure UpdateCaption(Exclude: TControl); override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
  end;

  TJvDockVIDVCZone = class(TJvDockAdvZone)
  private
    FExpandButtonDown: Boolean;
    FZoneSizeStyle: TJvDockZoneSizeStyle;
    procedure DoSetChildSizeStyle(ZoneSizeStyle: TJvDockZoneSizeStyle);
  protected
    function GetSplitterLimit(IsMin: Boolean): Integer; override;
  public
    property ExpandButtonDown: Boolean read FExpandButtonDown write FExpandButtonDown;
    property ZoneSizeStyle: TJvDockZoneSizeStyle read FZoneSizeStyle write FZoneSizeStyle;
    procedure Insert(DockSize: Integer; Hide: Boolean); override;
    procedure Remove(DockSize: Integer; Hide: Boolean); override;
  end;

  TJvDockVIDVCTree = class(TJvDockAdvTree)
  private
    FDropOnZone: TJvDockZone;
    FExpandBtnZone: TJvDockVIDVCZone;
    FLockDropDockSizeCount: Integer;
    FCaptionLeftOffset: Integer;
    FCaptionRightOffset: Integer;
    procedure LockDropDockSize;
    procedure UnlockDropDockSize;
    procedure SetCaptionLeftOffset(const Value: Integer);
    procedure SetCaptionRightOffset(const Value: Integer);
  protected
    procedure DoLButtonUp(var Msg: TWMMouse;
      var Zone: TJvDockZone; out HTFlag: Integer); override;
    procedure ResetDockZoneSizeStyle(Parent: TJvDockZone;
      ZoneSizeStyle: TJvDockZoneSizeStyle; Exclude: TJvDockZone);
    function GetLeftGrabbersHTFlag(const MousePos: TPoint;
      out HTFlag: Integer; Zone: TJvDockZone): TJvDockZone; override;
    function GetTopGrabbersHTFlag(const MousePos: TPoint;
      out HTFlag: Integer; Zone: TJvDockZone): TJvDockZone; override;

    procedure DoMouseMove(var Msg: TWMMouse;
      var Zone: TJvDockZone; out HTFlag: Integer); override;
    function DoLButtonDown(var Msg: TWMMouse;
      var Zone: TJvDockZone; out HTFlag: Integer): Boolean; override;

    procedure InsertControlFromConjoinHost(Control: TControl;
      InsertAt: TAlign; DropCtl: TControl); virtual;
    procedure IgnoreZoneInfor(Stream: TMemoryStream); virtual;
//    procedure AdjustDockRect(Control: TControl; var ARect: TRect); override;
    procedure WindowProc(var Msg: TMessage); override;
    procedure SplitterMouseUp; override;
    procedure GetSiteInfo(Client: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean); override;
    procedure InsertControl(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl); override;
    procedure InsertSibling(NewZone, SiblingZone: TJvDockZone;
      InsertLast, Update: Boolean); override;
    procedure InsertNewParent(NewZone, SiblingZone: TJvDockZone;
      ParentOrientation: TDockOrientation; InsertLast, Update: Boolean); override;
    procedure DrawDockGrabber(Control: TControl; const ARect: TRect); override;
    procedure DrawSplitterRect(const ARect: TRect); override;
    procedure PaintDockGrabberRect(Canvas: TCanvas; Control: TControl;
      const ARect: TRect); virtual;
    procedure DrawCloseButton(Canvas: TCanvas; Zone: TJvDockZone;
      Left, Top: Integer); virtual;
    procedure ResetBounds(Force: Boolean); override;
    procedure SetActiveControl(const Value: TControl); override;
    procedure DrawDockSiteRect; override;
{    procedure PositionDockRect(Client, DropCtl: TControl; DropAlign: TAlign;
      var DockRect: TRect); override;}
    function GetDockEdge(DockRect: TRect; MousePos: TPoint;
      var DropAlign: TAlign; Control: TControl): TControl; override;
    procedure RemoveZone(Zone: TJvDockZone; Hide: Boolean = True); override;
    procedure GetCaptionRect(var Rect: TRect); override;
    property CaptionLeftOffset: Integer read FCaptionLeftOffset write SetCaptionLeftOffset;
    property CaptionRightOffset: Integer read FCaptionRightOffset write SetCaptionRightOffset;
  public
    constructor Create(DockSite: TWinControl; DockZoneClass: TJvDockZoneClass); override;
  end;

  TJvDockVIDVCTabPageControl = class;

  TJvDockVIDVCTabSheet = class(TJvDockTabSheet)
  private
    FTabWidth: Integer;
    FShowTabWidth: Integer;
    FIsSourceDockClient: Boolean;
    // FZoneSizeStyle: TJvDockZoneSizeStyle;
    procedure SetTabWidth(const Value: Integer);
    procedure WMSetText(var Msg: TMessage); message WM_SETTEXT;
    procedure SetSheetSort(CaptionStr: string);
  protected
    procedure SetPageControl(APageControl: TJvDockPageControl); override;
    property TabWidth: Integer read FTabWidth write SetTabWidth;
    property ShowTabWidth: Integer read FShowTabWidth;
    procedure Loaded; override;
    procedure UpdateTabShowing; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BorderWidth;
    property Caption;
    property DragMode;
    property Enabled;
    property Font;
    property Height stored False;
    property Highlighted;
    property ImageIndex;
    property Left stored False;
    property Constraints;
    property PageIndex;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabVisible;
    property Top stored False;
    property Visible stored False;
    property Width stored False;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnHide;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnShow;
    property OnStartDrag;
  end;

  TJvDockTabPanel = class(TCustomControl)
  private
    FPage: TJvDockVIDVCTabPageControl;
    FActiveSheetColor: TColor;
    FHotTrackColor: TColor;
    FActiveFont: TFont;
    FInactiveFont: TFont;
    FTabLeftOffset: Integer;
    FTabRightOffset: Integer;
    FTabTopOffset: Integer;
    FTabBottomOffset: Integer;
    FCaptionLeftOffset: Integer;
    FCaptionRightOffset: Integer;
    FCaptionTopOffset: Integer;
    FTabSplitterWidth: Integer;
    FTabHeight: Integer;
    FSortList: TList;
    FSelectSheet: TJvDockVIDVCTabSheet;
    FTempPages: TList;
    FSelectHotIndex: Integer;
    FShowTabImages: Boolean;
    procedure SetPage(const Value: TJvDockVIDVCTabPageControl);
    function GetTotalTabWidth: Integer;
    procedure SetTotalTabWidth(const Value: Integer);
    function GetMinTabWidth: TJvDockTabSheet;
    function GetMaxTabWidth: TJvDockTabSheet;
    procedure SetTabBottomOffset(const Value: Integer);
    procedure SetTabLeftOffset(const Value: Integer);
    procedure SetTabRightOffset(const Value: Integer);
    procedure SetTabTopOffset(const Value: Integer);
    procedure SetCaptionLeftOffset(const Value: Integer);
    procedure SetCaptionRightOffset(const Value: Integer);
    procedure SetCaptionTopOffset(const Value: Integer);
    procedure SetTabSplitterWidth(const Value: Integer);
    function GetSorts(Index: Integer): TJvDockVIDVCTabSheet;
    function GetPanelHeight: Integer;
    function GetPanelWidth: Integer;
    procedure SetPanelHeight(const Value: Integer);
    function FindSheetWithPos(cX, cY, cTopOffset, cBottomOffset: Integer): Integer;
    function GetDockClientFromPageIndex(Index: Integer): TControl;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure SetShowTabImages(const Value: Boolean);
    procedure SetTabHeight(const Value: Integer);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function GetPageIndexFromMousePos(X, Y: Integer): Integer; virtual;
    procedure SetShowTabWidth;
    property TotalTabWidth: Integer read GetTotalTabWidth write SetTotalTabWidth;
    property MinTabWidth: TJvDockTabSheet read GetMinTabWidth;
    property MaxTabWidth: TJvDockTabSheet read GetMaxTabWidth;
    property TabLeftOffset: Integer read FTabLeftOffset write SetTabLeftOffset default 5;
    property TabRightOffset: Integer read FTabRightOffset write SetTabRightOffset default 5;
    property TabTopOffset: Integer read FTabTopOffset write SetTabTopOffset default 2;
    property TabBottomOffset: Integer read FTabBottomOffset write SetTabBottomOffset default 3;
    property TabSplitterWidth: Integer read FTabSplitterWidth write SetTabSplitterWidth default 2;
    property CaptionTopOffset: Integer read FCaptionTopOffset write SetCaptionTopOffset default 0;
    property CaptionLeftOffset: Integer read FCaptionLeftOffset write SetCaptionLeftOffset default 5;
    property CaptionRightOffset: Integer read FCaptionRightOffset write SetCaptionRightOffset default 5;
    property Sorts[Index: Integer]: TJvDockVIDVCTabSheet read GetSorts;
    property PanelHeight: Integer read GetPanelHeight write SetPanelHeight;
    property PanelWidth: Integer read GetPanelWidth;
    property TabHeight: Integer read FTabHeight write SetTabHeight;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resize; override;
    procedure DeleteSorts(Sheet: TJvDockVIDVCTabSheet);
    property Page: TJvDockVIDVCTabPageControl read FPage write SetPage;
    property SelectSheet: TJvDockVIDVCTabSheet read FSelectSheet write FSelectSheet;
    property ShowTabImages: Boolean read FShowTabImages write SetShowTabImages;
  end;

  TJvDockTabPanelClass = class of TJvDockTabPanel;

  TJvDockVIDVCTabPageControl = class(TJvDockAdvTabPageControl)
  private
    FTabPanelClass: TJvDockTabPanelClass;
    FPanel: TJvDockTabPanel;
    FTempSheet: TJvDockVIDVCTabSheet;
    FTabImageList: TCustomImageList;
    procedure SetActiveSheetColor(const Value: TColor);
    procedure SetInactiveSheetColor(const Value: TColor);
    procedure SetTabBottomOffset(const Value: Integer);
    procedure SetTabLeftOffset(const Value: Integer);
    procedure SetTabRightOffset(const Value: Integer);
    procedure SetTabTopOffset(const Value: Integer);
    procedure SetActiveFont(const Value: TFont);
    procedure SetInactiveFont(const Value: TFont);
    procedure SetHotTrackColor(const Value: TColor);
    function GetTabBottomOffset: Integer;
    function GetTabLeftOffset: Integer;
    function GetTabRightOffset: Integer;
    function GetTabTopOffset: Integer;
    function GetInactiveSheetColor: TColor;
    function GetActiveSheetColor: TColor;
    function GetActiveFont: TFont;
    function GetInactiveFont: TFont;
    function GetVisibleSheetCount: Integer;
    function GetHotTrackColor: TColor;
    function GetShowTabImages: Boolean;
    procedure SetShowTabImages(const Value: Boolean);
    function GetPage(Index: Integer): TJvDockVIDVCTabSheet;
    function GetActiveVIDPage: TJvDockVIDVCTabSheet;
    procedure SetActiveVIDPage(const Value: TJvDockVIDVCTabSheet);
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure CreatePanel; virtual;
    procedure Change; override;
    procedure CustomDockOver(Source: TJvDockDragDockObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure CustomGetSiteInfo(Source: TJvDockDragDockObject; Client: TControl; var InfluenceRect: TRect;
      MousePos: TPoint; var CanDock: Boolean); override;
    procedure CustomDockDrop(Source: TJvDockDragDockObject; X, Y: Integer); override;
    procedure CustomGetDockEdge(Source: TJvDockDragDockObject; MousePos: TPoint; var DropAlign: TAlign); override;
    function CustomUnDock(Source: TJvDockDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    procedure DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean); override;
    procedure CreateParams(var Params: TCreateParams); override;
    function GetDockClientFromMousePos(MousePos: TPoint): TControl; override;
    procedure Paint; override;
    procedure SetActivePage(Page: TJvDockTabSheet); override;
    procedure SetTabHeight(Value: Smallint); override;
    procedure SetTabPosition(Value: TTabPosition); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure SetHotTrack(Value: Boolean); override;
    procedure SetImages(Value: TCustomImageList); override;
    property TabPanelClass: TJvDockTabPanelClass read FTabPanelClass write FTabPanelClass;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    property ActiveVIDPage: TJvDockVIDVCTabSheet read GetActiveVIDPage write SetActiveVIDPage;
    destructor Destroy; override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
    procedure UpdateCaption(Exclude: TControl); override;
    procedure Resize; override;
    property Pages[Index: Integer]: TJvDockVIDVCTabSheet read GetPage;
    property Panel: TJvDockTabPanel read FPanel;
    property TempSheet: TJvDockVIDVCTabSheet read FTempSheet write FTempSheet;
    property VisibleSheetCount: Integer read GetVisibleSheetCount;
  published
    property ActiveSheetColor: TColor read GetActiveSheetColor write SetActiveSheetColor;
    property InactiveSheetColor: TColor read GetInactiveSheetColor write SetInactiveSheetColor;
    property TabLeftOffset: Integer read GetTabLeftOffset write SetTabLeftOffset default 5;
    property TabRightOffset: Integer read GetTabRightOffset write SetTabRightOffset default 5;
    property TabTopOffset: Integer read GetTabTopOffset write SetTabTopOffset default 2;
    property TabBottomOffset: Integer read GetTabBottomOffset write SetTabBottomOffset default 3;
    property ActiveFont: TFont read GetActiveFont write SetActiveFont;
    property InactiveFont: TFont read GetInactiveFont write SetInactiveFont;
    property HotTrackColor: TColor read GetHotTrackColor write SetHotTrackColor;
    property ShowTabImages: Boolean read GetShowTabImages write SetShowTabImages;
    property ActivePage;
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HotTrack;
    property Images;
    property MultiLine;
    property OwnerDraw;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RaggedRight;
    property ScrollOpposite;
    property ShowHint;
    property Style;
    property TabHeight;
    property TabIndex;
    property TabOrder;
    property TabPosition;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawTab;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TJvDockVIDVCDragDockObject = class(TJvDockDragDockObject)
  private
    FOldDropAlign: TAlign;
    FCurrState: TDragState;
    FOldState: TDragState;
    FOldTarget: Pointer;
    FSourceDockClientList: TList;
    FDropTabControl: TJvDockVIDVCTabPageControl;
    FIsTabDockOver: Boolean;
    FErase: Boolean;
    function GetSourceDockClient(Index: Integer): TControl;
    function GetSourceDockClientCount: Integer;
    procedure SetOldState(const Value: TDragState);
    procedure SetCurrState(const Value: TDragState);
  protected
    procedure GetBrush_PenSize_DrawRect(var ABrush: TBrush;
      var PenSize: Integer; var DrawRect: TRect; Erase: Boolean); override;
    procedure MouseMsg(var Msg: TMessage); override;
    procedure DefaultDockImage(Erase: Boolean); override;
    function CanLeave(NewTarget: TWinControl): Boolean; override;
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;
    function DragFindWindow(const Pos: TPoint): HWND; override;
    function GetDropCtl: TControl; override;
    property SourceDockClients[Index: Integer]: TControl read GetSourceDockClient;
    property SourceDockClientCount: Integer read GetSourceDockClientCount;
    property CurrState: TDragState read FCurrState write SetCurrState;
    property OldState: TDragState read FOldState write SetOldState;
  end;

procedure PaintGradientBackground(Canvas: TCanvas; ARect: TRect; StartColor, EndColor: TColor);

implementation

uses
  Consts, SysUtils, Math, Forms, ExtCtrls, 
  JvDockSupportProc, JvDockGlobals;

type
  TWinControlAccessProtected = class(TWinControl);

// (rom) such global variables are problematic
var
  gi_DockRect: TRect;

procedure PaintGradientBackground(Canvas: TCanvas; ARect: TRect;
  StartColor, EndColor: TColor);
const
  D = 256;
var
  X, C1, C2, R1, G1, B1, W: Integer;
  DR, DG, DB, DH: Real;

  procedure InitRGBValues(C1, C2: Integer);
  begin
    R1 := GetRValue(C1);
    G1 := GetGValue(C1);
    B1 := GetBValue(C1);
    DR := (GetRValue(C2) - R1) / D;
    DG := (GetGValue(C2) - G1) / D;
    DB := (GetBValue(C2) - B1) / D;
  end;

begin
  with Canvas do
  begin
    Lock;
    try
      Brush.Style := bsSolid;

      if StartColor <> EndColor then
      begin
        C1 := StartColor;
        C2 := EndColor;

        InitRGBValues(C1, C2);

        DH := (ARect.Right - ARect.Left) / D;
        for X := 0 to 255 do
        begin
          Brush.Color := RGB(R1 + Round(DR * X), G1 + Round(DG * X),
            B1 + Round(DB * X));
          with ARect do
          begin
            if Right <= Left + Round((X + 1) * DH) then
              W := Right
            else
              W := Left + Round((X + 1) * DH);
            FillRect(Rect(Left + Round(X * DH), Top, W, Bottom))
          end;
        end;
      end
      else
      begin
        Brush.Color := StartColor;
        FillRect(ARect);
      end;
    finally
      Unlock;
    end;
  end;
end;

procedure AssignList(FromList, ToList: TList);
var
  I: Integer;
begin
  ToList.Clear;
  for I := 0 to FromList.Count - 1 do
    ToList.Add(FromList[I]);
end;

function ComputeVIDDockingRect(Target, Control: TControl; var DockRect: TRect; MousePos: TPoint): TAlign;
var
  DockTopRect: TRect;
  DockLeftRect: TRect;
  DockBottomRect: TRect;
  DockRightRect: TRect;
  DockCenterRect: TRect;
  DockTabRect: TRect;
begin
  Result := alNone;
  if Target = nil then
    Exit;

  with Target do
  begin
    DockLeftRect.TopLeft := Point(0, 0);
    DockLeftRect.BottomRight := Point(ClientWidth div 5, ClientHeight);

    DockTopRect.TopLeft := Point(ClientWidth div 5, 0);
    DockTopRect.BottomRight := Point(ClientWidth div 5 * 4, ClientHeight div 5);

    DockRightRect.TopLeft := Point(ClientWidth div 5 * 4, 0);
    DockRightRect.BottomRight := Point(ClientWidth, ClientHeight);

    if Target is TJvDockCustomTabControl then
    begin
      DockBottomRect.TopLeft := Point(ClientWidth div 5, ClientWidth div 5 * 4);
      DockBottomRect.BottomRight := Point(ClientWidth div 5 * 4, ClientHeight - JvDockGetSysCaptionHeight);
    end
    else
    begin
      DockBottomRect.TopLeft := Point(0, ClientHeight div 5 * 4);
      DockBottomRect.BottomRight := Point(ClientWidth, ClientHeight);
    end;

    DockCenterRect.TopLeft := Point(0, -JvDockGetSysCaptionHeight);
    DockCenterRect.BottomRight := Point(ClientWidth, 0);

    if Target is TJvDockCustomTabControl then
    begin
      DockTabRect.TopLeft := Point(0, ClientHeight - JvDockGetSysCaptionHeight);
      DockTabRect.BottomRight := Point(ClientWidth, ClientHeight);
    end
    else
      DockTabRect := Rect(0, 0, 0, 0);

    if PtInRect(DockCenterRect, MousePos) or
      PtInRect(DockTabRect, MousePos) then
    begin
      Result := alClient;
      DockRect := DockCenterRect;
      DockRect.BottomRight := Point(ClientWidth, ClientHeight);
    end
    else
    if PtInRect(DockLeftRect, MousePos) then
    begin
      Result := alLeft;
      DockRect := DockLeftRect;
      DockRect.Right := Min(ClientWidth div 2, Control.ClientWidth);
    end
    else
    if PtInRect(DockTopRect, MousePos) then
    begin
      Result := alTop;
      DockRect := DockTopRect;
      DockRect.Left := 0;
      DockRect.Right := ClientWidth;
      DockRect.Bottom := Min(ClientHeight div 2, Control.ClientHeight);
    end
    else
    if PtInRect(DockRightRect, MousePos) then
    begin
      Result := alRight;
      DockRect := DockRightRect;
      DockRect.Left := Max(ClientWidth div 2, ClientWidth - Control.ClientWidth);
    end
    else
    if PtInRect(DockBottomRect, MousePos) then
    begin
      Result := alBottom;
      DockRect := DockBottomRect;
      DockRect.Top := Max(ClientHeight div 2, ClientHeight - Control.ClientHeight);
    end;
    if Result = alNone then
      Exit;

    DockRect.TopLeft := ClientToScreen(DockRect.TopLeft);
    DockRect.BottomRight := ClientToScreen(DockRect.BottomRight);
  end;
end;

(*  (ahuser) not used - make Delphi 5 happy
procedure SetTabControlPreview(VIDSource: TJvDockVIDVCDragDockObject;
  TabControl: TJvDockVIDVCTabPageControl;
  State: TDragState; DropAlign: TAlign);

var
  I: Integer;
  Index: Integer;
begin
  if TabControl <> nil then
  begin
    if DropAlign = alClient then
    begin

      if TabControl.FTempSheet = nil then
      begin

        for I := VIDSource.SourceDockClientCount - 1 downto 0 do
        begin

          TabControl.FTempSheet := TJvDockVIDVCTabSheet.Create(TabControl);
          TabControl.FTempSheet.PageControl := TabControl;

          TabControl.FTempSheet.Caption := TWinControlAccessProtected(VIDSource.SourceDockClients[I]).Caption;
          Index := TabControl.FTabImageList.AddIcon(TForm(VIDSource.SourceDockClients[I]).Icon);
          if Index <> -1 then
            TabControl.FTempSheet.ImageIndex := Index;

          TabControl.FTempSheet.FIsSourceDockClient := True;
        end;

        TabControl.ActivePage := TabControl.FTempSheet;
        TabControl.Panel.SelectSheet := TabControl.FTempSheet;

        {$IFDEF COMPILER6_UP}
        TabControl.Panel.FTempPages.Assign(TabControl.PageSheets);
        {$ELSE}
        AssignList(TabControl.PageSheets, TabControl.Panel.FTempPages);
        {$ENDIF COMPILER6_UP}

        TabControl.ActivePage.Invalidate;

      end;
    end;

    if ((State = dsDragLeave) or (VIDSource.DropAlign <> alClient)) and (TabControl.FTempSheet <> nil) then
    begin

      for I := TabControl.PageCount - 1 downto 0 do
      begin
        if TJvDockVIDVCTabSheet(TabControl.Pages[I]).FIsSourceDockClient then
        begin

          Index := TabControl.Panel.FTempPages.IndexOf(TabControl.Pages[I]);

          if Index >= 0 then
          begin
            TabControl.Panel.FTempPages.Delete(Index);
            if TabControl.FTabImageList.Count > Index then
              TabControl.FTabImageList.Delete(Index);
          end;

          TabControl.Pages[I].Free;
        end;
      end;

      TabControl.FTempSheet := nil;

    end;

    TabControl.ParentForm.Caption := TabControl.ActivePage.Caption;

    if TabControl.ParentForm.HostDockSite is TJvDockCustomPanel then
      TabControl.ParentForm.HostDockSite.Invalidate;
  end;
end;
*)

//=== { TJvDockVIDVCStyle } ==================================================

constructor TJvDockVIDVCStyle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DockPanelClass := TJvDockVIDVCPanel;
  DockSplitterClass := TJvDockVIDVCSplitter;
  ConjoinPanelClass := TJvDockVIDVCConjoinPanel;
  TabDockClass := TJvDockVIDVCTabPageControl;
  DockPanelTreeClass := TJvDockVIDVCTree;
  DockPanelZoneClass := TJvDockVIDVCZone;
  ConjoinPanelTreeClass := TJvDockVIDVCTree;
  ConjoinPanelZoneClass := TJvDockVIDVCZone;
  ConjoinServerOptionClass := TJvDockVIDVCConjoinServerOption;
  TabServerOptionClass := TJvDockVIDVCTabServerOption;
end;

procedure TJvDockVIDVCStyle.FormDockOver(DockClient: TJvDockClient;
  Source: TJvDockDragDockObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
var
  ARect: TRect;
begin
  with DockClient do
  begin
    Accept := EnableDock and EachOtherDock and
      IsDockable(ParentForm, Source.Control, Source.DropOnControl, Source.DropAlign);
    if State = dsDragMove then
    begin
      Source.DropAlign := ComputeVIDDockingRect(ParentForm, Source.Control, ARect, Point(X, Y));
      if Accept and (Source.DropAlign <> alNone) then
      begin
        if Source.DropAlign = alClient then
          Inc(ARect.Top, JvDockGetSysCaptionHeightAndBorderWidth + 1);
        Source.DockRect := ARect;
      end;
      gi_DockRect := ARect;
    end
    else
    if State = dsDragLeave then
      Source.DropAlign := alNone;
    if Source is TJvDockVIDVCDragDockObject then
    begin
      TJvDockVIDVCDragDockObject(Source).OldState := TJvDockVIDVCDragDockObject(Source).CurrState;
      TJvDockVIDVCDragDockObject(Source).CurrState := State;
    end;
  end;
end;

procedure TJvDockVIDVCStyle.FormGetSiteInfo(Source: TJvDockDragDockObject;
   DockClient: TJvDockClient; Client: TControl; var InfluenceRect: TRect;
   MousePos: TPoint; var CanDock: Boolean);
const
  DefExpandoRect = 20;
var
  CH_BW: Integer;
  ARect: TRect;
begin
  with DockClient do
  begin
    CanDock := IsDockable(ParentForm, Client, Source.DropOnControl, Source.DropAlign);
    if CanDock then
    begin
      GetWindowRect(ParentForm.Handle, InfluenceRect);
      if ParentForm.HostDockSite is TJvDockCustomPanel then
      begin
        Dec(InfluenceRect.Top, TJvDockCustomPanel(ParentForm.HostDockSite).JvDockManager.GrabberSize);
      end;
      if PtInRect(InfluenceRect, MousePos) then
      begin
        ARect := InfluenceRect;
        InflateRect(ARect, -DefExpandoRect, -DefExpandoRect);
        CH_BW := JvDockGetSysCaptionHeightAndBorderWidth;
        Inc(ARect.Top, CH_BW + 1);
        if PtInRect(ARect, MousePos) then
        begin
          InfluenceRect := Rect(0, 0, 0, 0);
          CanDock := False;
        end;
      end;
    end;
  end;
end;

procedure TJvDockVIDVCStyle.FormDockDrop(DockClient: TJvDockClient;
  Source: TJvDockDragDockObject; X, Y: Integer);
var
  ARect, DRect: TRect;
  DockType: TAlign;
  Host: TJvDockableForm;
  APanelDock: TWinControl;
  VIDSource: TJvDockVIDVCDragDockObject;
  I: Integer;
begin
  if Source is TJvDockVIDVCDragDockObject then
  begin
    TJvDockVIDVCDragDockObject(Source).CurrState := dsDragEnter;
    TJvDockVIDVCDragDockObject(Source).OldState := dsDragEnter;
  end;

  if IsDockable(DockClient.ParentForm, Source.Control, Source.DropOnControl, Source.DropAlign) then
  begin
    Host := nil;
    if not JvGlobalDockIsLoading then
      JvDockLockWindow(nil);
    try
      with DockClient do
      begin
        DockType := ComputeVIDDockingRect(DockClient.ParentForm, Source.Control, ARect, Point(X, Y));
        if ParentForm.HostDockSite is TJvDockPanel then
        begin
          if DockType = alClient then
          begin
            if Source.Control is TJvDockTabHostForm then
            begin
              APanelDock := ParentForm.HostDockSite;
              ARect := ParentForm.BoundsRect;
              ParentForm.ManualDock(TJvDockTabHostForm(Source.Control).PageControl, nil, alClient);
              TJvDockTabHostForm(Source.Control).PageControl.ActivePage.PageIndex := 0;
              Source.Control.BoundsRect := ARect;
              Source.Control.ManualDock(APanelDock, nil, alClient);
              if ParentForm.FormStyle = fsStayOnTop then
                TForm(Source.Control).FormStyle := fsStayOnTop;
            end
            else
            begin
              APanelDock := ParentForm.HostDockSite;
              DRect.TopLeft := ParentForm.HostDockSite.ClientToScreen(Point(0, 0));
              Host := CreateTabHostAndDockControl(ParentForm, Source.Control);
              SetDockSite(ParentForm, False);
              SetDockSite(TWinControl(Source.Control), False);
              Host.Top := DRect.Top;
              Host.Left := DRect.Left;
              Host.Visible := True;
              Host.ManualDock(APanelDock, nil, alClient);
            end;
          end
          else
          begin
            DRect := ParentForm.HostDockSite.BoundsRect;
            Source.Control.ManualDock(ParentForm.HostDockSite, nil, DockType);
            ParentForm.HostDockSite.BoundsRect := DRect;
            SetDockSite(TWinControl(Source.Control), False);
          end;
          Exit;
        end;

        if DockType = alClient then
        begin
          if Source.Control is TJvDockTabHostForm then
          begin
            APanelDock := ParentForm.HostDockSite;
            ARect := ParentForm.BoundsRect;
            ParentForm.ManualDock(TJvDockTabHostForm(Source.Control).PageControl, nil, alClient);
            TJvDockTabHostForm(Source.Control).PageControl.ActivePage.PageIndex := 0;
            Source.Control.BoundsRect := ARect;
            Source.Control.ManualDock(APanelDock, nil, alClient);
            if ParentForm.FormStyle = fsStayOnTop then
              TForm(Source.Control).FormStyle := fsStayOnTop;
            Exit;
          end
          else
          begin
            if Source is TJvDockVIDVCDragDockObject then
            begin
              VIDSource := TJvDockVIDVCDragDockObject(Source);
              DoFloatForm(Source.Control);
              FreeAllDockableForm;
              for I := 0 to VIDSource.SourceDockClientCount - 1 do
              begin
                VIDSource.Control := VIDSource.SourceDockClients[I];
                if Host = nil then
                  Host := DockClient.CreateTabHostAndDockControl(DockClient.ParentForm, Source.Control)
                else
                  Source.Control.ManualDock(TJvDockTabHostForm(Host).PageControl, nil, alClient);
              end;
              Host.Visible := True;
            end;
          end;
        end
        else
        if DockType <> alNone then
        begin
          Host := CreateConjoinHostAndDockControl(ParentForm, Source.Control, DockType);
          SetDockSite(ParentForm, False);
          SetDockSite(TWinControl(Source.Control), False);
          Host.Visible := True;
        end;

        if Host <> nil then
        begin
          Host.LRDockWidth := Source.Control.LRDockWidth;
          Host.TBDockHeight := Source.Control.TBDockHeight;
        end;
      end;
    finally
      if not JvGlobalDockIsLoading then
        JvDockUnLockWindow;
    end;
  end;
end;

procedure TJvDockVIDVCStyle.SetDockBaseControl(IsCreate: Boolean;
  DockBaseControl: TJvDockBaseControl);
var
  ADockClient: TJvDockClient;
begin
  if DockBaseControl is TJvDockClient then
  begin
    ADockClient := TJvDockClient(DockBaseControl);
    if IsCreate then
      ADockClient.DirectDrag := False;
  end;
end;

procedure TJvDockVIDVCStyle.FormStartDock(DockClient: TJvDockClient;
  var Source: TJvDockDragDockObject);
begin
  inherited FormStartDock(DockClient, Source);
  Source := TJvDockVIDVCDragDockObject.Create(DockClient.ParentForm);
end;

procedure TJvDockVIDVCStyle.FormGetDockEdge(DockClient: TJvDockClient;
  Source: TJvDockDragDockObject; MousePos: TPoint; var DropAlign: TAlign);
var
  ARect: TRect;
begin
  DropAlign := ComputeVIDDockingRect(DockClient.ParentForm, Source.Control, ARect, MousePos);
end;

function TJvDockVIDVCStyle.DockClientWindowProc(DockClient: TJvDockClient;
  var Msg: TMessage): Boolean;
begin
  Result := inherited DockClientWindowProc(DockClient, Msg);
end;

procedure TJvDockVIDVCStyle.CreateConjoinServerOption(
  var Option: TJvDockBasicConjoinServerOption);
begin
  Option := TJvDockVIDVCConjoinServerOption.Create(Self);
end;

procedure TJvDockVIDVCStyle.CreateTabServerOption(var Option: TJvDockBasicTabServerOption);
begin
  Option := TJvDockVIDVCTabServerOption.Create(Self);
end;

procedure TJvDockVIDVCStyle.AssignConjoinServerOption(APanel: TJvDockCustomPanel);
begin
  inherited AssignConjoinServerOption(APanel);
end;

procedure TJvDockVIDVCStyle.AssignTabServerOption(APage: TJvDockTabPageControl);
var
  TmpPage: TJvDockVIDVCTabPageControl;
  TmpOption: TJvDockVIDVCTabServerOption;
begin
  inherited AssignTabServerOption(APage);
  if (APage is TJvDockVIDVCTabPageControl) and (TabServerOption is TJvDockVIDVCTabServerOption) then
  begin
    TmpPage := APage as TJvDockVIDVCTabPageControl;
    TmpOption := TabServerOption as TJvDockVIDVCTabServerOption;
    TmpPage.ActiveFont.Assign(TmpOption.ActiveFont);
    TmpPage.ActiveSheetColor := TmpOption.ActiveSheetColor;
    TmpPage.InactiveFont.Assign(TmpOption.InactiveFont);
    TmpPage.InactiveSheetColor := TmpOption.InactiveSheetColor;
    TmpPage.HotTrackColor := TmpOption.HotTrackColor;
    TmpPage.ShowTabImages := TmpOption.ShowTabImages;
  end;
end;

procedure TJvDockVIDVCStyle.ParentFormWindowProc(var Msg: TMessage);
begin
  inherited ParentFormWindowProc(Msg);
  if (Msg.Msg = WM_SETTINGCHANGE) or (Msg.Msg = WM_SYSCOLORCHANGE) then
  begin
    ParentForm.Caption := '';
    if ConjoinServerOption is TJvDockVIDVCConjoinServerOption then
      if TJvDockVIDVCConjoinServerOption(ConjoinServerOption).SystemInfo then
        TJvDockVIDVCConjoinServerOption(ConjoinServerOption).SetDefaultSystemCaptionInfo;
  end;
end;

procedure TJvDockVIDVCStyle.DoSystemInfoChange(Value: Boolean);
begin
  if Assigned(FSystemInfoChange) then
    FSystemInfoChange(Value);
end;

//=== { TJvDockVIDVCPanel } ==================================================

function TJvDockVIDVCPanel.CreateDockManager: IDockManager;
var
  Option: TJvDockVIDVCConjoinServerOption;
begin
  Result := inherited CreateDockManager;
  if (DockServer <> nil) and (Result <> nil) then
  begin
    Option := TJvDockVIDVCConjoinServerOption(DockServer.DockStyle.ConjoinServerOption);
    (Result as IJvDockManager).GrabberSize := Option.GrabbersSize;
  end;
end;

procedure TJvDockVIDVCPanel.CustomDockDrop(Source: TJvDockDragDockObject; X, Y: Integer);
begin
  if Source.Control is TJvDockableForm then
    ShowDockPanel(True, Source.Control);
  if not ((Source.Control.HostDockSite <> nil) and
    (Source.DropOnControl = Source.Control.HostDockSite.Parent) and
    (Source.DropAlign = alClient)) then
  begin
    inherited CustomDockDrop(Source, X, Y);
    JvDockManager.ActiveControl := Source.Control;
    if (Source.Control is TWinControl) and TWinControl(Source.Control).CanFocus then
      TWinControl(Source.Control).SetFocus;
  end;
end;

procedure TJvDockVIDVCPanel.CustomDockOver(Source: TJvDockDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  DropAlign: TAlign;
begin
  inherited CustomDockOver(Source, X, Y, State, Accept);
  if Accept and (Source is TJvDockVIDVCDragDockObject) then
    if State = dsDragMove then
    begin
      DropAlign := Source.DropAlign;
      JvDockManager.GetDockEdge(Source.DockRect, Source.DragPos, DropAlign, Source.Control);
    end;
end;

procedure TJvDockVIDVCPanel.CustomGetDockEdge(Source: TJvDockDragDockObject;
  MousePos: TPoint; var DropAlign: TAlign);
begin
end;

procedure TJvDockVIDVCPanel.CustomGetSiteInfo(Source: TJvDockDragDockObject;
  Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
begin
  if VisibleDockClientCount = 0 then
    inherited CustomGetSiteInfo(Source, Client, InfluenceRect, MousePos, CanDock)
  else
  begin
    CanDock := IsDockable(Self, Client, Source.DropOnControl, Source.DropAlign);
    if CanDock then
      JvDockManager.GetSiteInfo(Client, InfluenceRect, MousePos, CanDock);
  end;
end;

procedure TJvDockVIDVCPanel.CustomStartDock(var Source: TJvDockDragDockObject);
begin
  Source := TJvDockVIDVCDragDockObject.Create(Self);
end;

procedure TJvDockVIDVCPanel.DockDrop(Source: TDragDockObject; X, Y: Integer);
begin
  inherited DockDrop(Source, X, Y);
end;

procedure TJvDockVIDVCPanel.UpdateCaption(Exclude: TControl);
begin
  inherited UpdateCaption(Exclude);
  Invalidate;
end;

//=== { TJvDockVIDVCTree } ===================================================

constructor TJvDockVIDVCTree.Create(DockSite: TWinControl;
  DockZoneClass: TJvDockZoneClass);
begin
  inherited Create(DockSite, DockZoneClass);
  FDropOnZone := nil;
  GrabberSize := 18;
  ButtonHeight := 11;
  ButtonWidth := 13;
  LeftOffset := 2;
  RightOffset := 2;
  TopOffset := 4;
  BottomOffset := 3;
  ButtonSplitter := 2;
  BorderWidth := 4;
  MinSize := 20;
  CaptionLeftOffset := 0;
  CaptionRightOffset := 0;
end;

procedure TJvDockVIDVCTree.InsertControl(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);
var
  I: Integer;
  Host: TJvDockTabHostForm;
  ChildCount: Integer;
  VIDSource: TJvDockVIDVCDragDockObject;
  TempControl: TControl;
  ARect: TRect;
  AZone: TJvDockZone;

  function CreateDockPageControl(Client: TControl): TJvDockTabHostForm;
  var
    Zone: TJvDockZone;
    TempCtl: TControl;
    TempPanel: TJvDockConjoinPanel;
    DockClient: TJvDockClient;
    APoint: TPoint;
  begin
    Result := nil;
    Zone := FindControlZone(DropCtl);
    DockClient := FindDockClient(DropCtl);
    if (DockClient <> nil) and (Zone <> nil) then
    begin
      TempCtl := DropCtl;

      if Zone.ParentZone.Orientation = doHorizontal then
      begin
        if Zone.PrevSibling = nil then
        begin
          if Zone.NextSibling <> nil then
            DropCtl := Zone.NextSibling.ChildControl;
          InsertAt := alTop;
        end
        else
        begin
          DropCtl := Zone.PrevSibling.ChildControl;
          InsertAt := alBottom;
        end;
      end
      else
      if Zone.ParentZone.Orientation = doVertical then
      begin
        if Zone.PrevSibling = nil then
        begin
          if Zone.NextSibling <> nil then
            DropCtl := Zone.NextSibling.ChildControl;
          InsertAt := alLeft;
        end
        else
        begin
          DropCtl := Zone.PrevSibling.ChildControl;
          InsertAt := alRight;
        end;
      end;

      if TempCtl.HostDockSite is TJvDockConjoinPanel then
        TempPanel := TJvDockConjoinPanel(TempCtl.HostDockSite)
      else
        TempPanel := nil;

      Result := DockClient.CreateTabHostAndDockControl(TempCtl, Client);
      if TempPanel <> nil then

        TempPanel.ParentForm.UnDockControl := Result;

      SetDockSite(TWinControl(TempCtl), False);
      SetDockSite(TWinControl(Client), False);

      if DockSite.Align = alBottom then
        APoint := Point(0, -TempCtl.TBDockHeight)
      else
      if DockSite.Align = alRight then
        APoint := Point(-TempCtl.LRDockWidth, 0)
      else
        APoint := Point(0, 0);
      APoint := DockSite.ClientToScreen(APoint);
      Result.Left := APoint.x;
      Result.Top := APoint.y;
      Result.UndockWidth := TempCtl.UndockWidth;
      Result.UndockHeight := TempCtl.UndockHeight;
      Result.LRDockWidth := TempCtl.LRDockWidth;
      Result.TBDockHeight := TempCtl.TBDockHeight + GrabberSize;

      Result.Visible := True;
    end;
  end;

begin
  if not JvGlobalDockIsLoading then
    JvDockLockWindow(nil);
  try
    VIDSource := nil;
    if Control is TJvDockableForm then
    begin
      if InsertAt in [alClient] then
      begin
        if DropCtl is TJvDockTabHostForm then
        begin
          try
            VIDSource := TJvDockVIDVCDragDockObject.Create(Control);
            DoFloatForm(Control);
            FreeAllDockableForm;
            for I := VIDSource.SourceDockClientCount - 1 downto 0 do
            begin
              TempControl := VIDSource.SourceDockClients[I];
              TempControl.ManualDock(TJvDockTabHostForm(DropCtl).PageControl);
              if TempControl is TForm then
              begin
                TForm(TempControl).ActiveControl := nil;
                SetDockSite(TForm(TempControl), False);
              end;
            end;
          finally
            VIDSource.Free;
            JvGlobalDockManager.DragObject.Control := nil;
          end;
        end
        else
        begin
          if (DockSite is TJvDockCustomPanel) and (DockSite.VisibleDockClientCount > 1) and (DropCtl <> nil) then
          begin
            try
              VIDSource := TJvDockVIDVCDragDockObject.Create(Control);
              DoFloatForm(Control);
              FreeAllDockableForm;

              Host := CreateDockPageControl(VIDSource.SourceDockClients[0]);
              if Host <> nil then
              begin
                for I := VIDSource.SourceDockClientCount - 1 downto 1 do
                begin
                  TempControl := VIDSource.SourceDockClients[I];
                  TempControl.ManualDock(Host.PageControl);
                  if TempControl is TForm then
                  begin
                    TForm(TempControl).ActiveControl := nil;
                    SetDockSite(TForm(TempControl), False);
                  end;
                end;

                Host.ManualDock(DockSite, nil, InsertAt);
              end;
            finally
              VIDSource.Free;
              JvGlobalDockManager.DragObject.Control := nil;
            end;
          end
          else
            inherited InsertControl(Control, InsertAt, DropCtl);
        end;
      end
      else
      if Control is TJvDockConjoinHostForm then
      begin
        TWinControlAccessProtected(TJvDockableForm(Control).DockableControl).DockManager.ResetBounds(True);
        InsertControlFromConjoinHost(Control, InsertAt, DropCtl);
      end
      else
        inherited InsertControl(Control, InsertAt, DropCtl);
    end
    else
    begin
      if InsertAt in [alLeft, alTop] then
        DropDockSize := DropDockSize + SplitterWidth div 2;
      if InsertAt in [alClient] then
      begin
        if DropCtl is TJvDockTabHostForm then
          Control.ManualDock(TJvDockTabHostForm(DropCtl).PageControl, nil, alClient)
        else
        if TopZone.ChildZones <> nil then
        begin
          ChildCount := TopZone.ChildCount;
          if DropCtl <> nil then
          begin
            ARect := DropCtl.BoundsRect;
            AZone := FindControlZone(DropCtl);

            if DropCtl.DockOrientation = doHorizontal then
            begin
              if ((AZone <> nil) and (AZone.ZoneLimit <> DockSite.Height)) then
                ARect.Bottom := ARect.Bottom + SplitterWidth;
            end
            else
            begin
              if ((AZone <> nil) and (AZone.ZoneLimit <> DockSite.Width)) then
                ARect.Right := ARect.Right + SplitterWidth;
            end;
            DockRect := ARect;
          end
          else
            DockRect := Rect(0, 0, TopZone.Width, TopZone.Height);

          Host := CreateDockPageControl(Control);
          if Host <> nil then
            if (ChildCount >= 2) or (DockSite is TJvDockPanel) then
            begin
              if InsertAt in [alLeft, alRight] then
                DropDockSize := DockRect.Right - DockRect.Left
              else
                DropDockSize := DockRect.Bottom - DockRect.Top + GrabberSize;

              LockDropDockSize;
              Host.ManualDock(DockSite, DropCtl, InsertAt);

              UnlockDropDockSize;
            end
            else
              Host.BoundsRect := DockSite.Parent.BoundsRect;
        end
        else
          inherited InsertControl(Control, InsertAt, DropCtl);
      end
      else
        inherited InsertControl(Control, InsertAt, DropCtl);

      DockRect := gi_DockRect;
    end;
    ForEachAt(nil, UpdateZone);
  finally
    if not JvGlobalDockIsLoading then
      JvDockUnLockWindow;
  end;
end;

procedure TJvDockVIDVCTree.InsertControlFromConjoinHost(Control: TControl;
  InsertAt: TAlign; DropCtl: TControl);
const
  {$IFDEF COMPILER6_UP}
  OrientArray: array [TAlign] of TDockOrientation =
    (doNoOrient, doHorizontal, doHorizontal, doVertical, doVertical, doNoOrient, doNoOrient);
  MakeLast: array [TAlign] of Boolean =
    (False, False, True, False, True, False, False);
  ReverseAt: array [TAlign] of TAlign =
    (alClient, alBottom, alTop, alRight, alLeft, alNone, alCustom);
  {$ELSE}
  OrientArray: array [TAlign] of TDockOrientation =
    (doNoOrient, doHorizontal, doHorizontal, doVertical, doVertical, doNoOrient);
  MakeLast: array [TAlign] of Boolean =
    (False, False, True, False, True, False);
  ReverseAt: array [TAlign] of TAlign =
    (alClient, alBottom, alTop, alRight, alLeft, alNone);
  {$ENDIF COMPILER6_UP}
var
  Stream: TMemoryStream;
  TopOrientation: TDockOrientation;
  InsertOrientation: TDockOrientation;
  CurrentOrientation: TDockOrientation;
  ZoneLimit: Integer;
  Level, LastLevel, I: Integer;
  Zone, NextZone: TJvDockZone;
  DropCtlZone, LastZone: TJvDockZone;
  OffsetXYLimitArr: array [TDockOrientation] of Integer;
  ControlXYLimitArr: array [TDockOrientation] of Integer;

  procedure ReadZone(SetZone: Boolean);
  var
    I: Integer;
  begin
    with Stream do
    begin
      Read(Level, SizeOf(Level));
      if Level = TreeStreamEndFlag then
        Exit;
      Zone := DockZoneClass.Create(Self);
      CustomLoadZone(Stream, Zone);
      ZoneLimit := Zone.ZoneLimit;
    end;
    if SetZone then
    begin
      if Level = LastLevel then
      begin
        Zone.NextSibling := LastZone.NextSibling;
        if LastZone.NextSibling <> nil then
          LastZone.NextSibling.PrevSibling := Zone;
        LastZone.NextSibling := Zone;
        Zone.PrevSibling := LastZone;
        Zone.ParentZone := LastZone.ParentZone;
      end
      else
      if Level > LastLevel then
      begin
        LastZone.ChildZones := Zone;
        Zone.ParentZone := LastZone;
        InsertOrientation := LastZone.Orientation;
      end
      else
      if Level < LastLevel then
      begin
        NextZone := LastZone;
        for I := 1 to LastLevel - Level do
          NextZone := NextZone.ParentZone;
        Zone.NextSibling := NextZone.NextSibling;
        if NextZone.NextSibling <> nil then
          NextZone.NextSibling.PrevSibling := Zone;
        NextZone.NextSibling := Zone;
        Zone.PrevSibling := NextZone;
        Zone.ParentZone := NextZone.ParentZone;
        InsertOrientation := Zone.ParentZone.Orientation;
      end;
      Zone.ZoneLimit := OffsetXYLimitArr[InsertOrientation] + ZoneLimit;
    end;
    LastLevel := Level;
    LastZone := Zone;
  end;

begin
  ControlXYLimitArr[doNoOrient] := 0;
  ControlXYLimitArr[doHorizontal] := DockRect.Bottom - DockRect.Top;
  ControlXYLimitArr[doVertical] := DockRect.Right - DockRect.Left;

  Stream := TMemoryStream.Create;
  if Control is TJvDockConjoinHostForm then
    TJvDockConjoinHostForm(Control).Panel.JvDockManager.SaveToStream(Stream);
  Stream.Position := 0;

  BeginUpdate;
  try
    Stream.Read(I, SizeOf(I));
    Stream.Position := Stream.Position + 8;
    Stream.Read(TopOrientation, SizeOf(TopOrientation));
    Stream.Read(ZoneLimit, SizeOf(ZoneLimit));
    IgnoreZoneInfor(Stream);
    if (DropCtl = nil) and (TopZone.ChildCount = 1) then
      DropCtl := TopZone.ChildZones.ChildControl;
    DropCtlZone := FindControlZone(DropCtl);
    if InsertAt in [alClient, alNone] then
      InsertAt := alRight;
    InsertOrientation := OrientArray[InsertAt];
    if TopZone.ChildCount = 0 then
    begin
      TopZone.Orientation := TopOrientation;
      InsertOrientation := TopOrientation;
    end
    else
    if TopZone.ChildCount = 1 then
    begin
      TopZone.Orientation := InsertOrientation;
      case InsertOrientation of
        doHorizontal:
          begin
            TopZone.ZoneLimit := TopZone.ChildZones.Width;
            TopXYLimit := TopZone.ChildZones.Height;
          end;
        doVertical:
          begin
            TopZone.ZoneLimit := TopZone.ChildZones.Height;
            TopXYLimit := TopZone.ChildZones.Width;
          end;
      end;
    end;

    if DropCtlZone <> nil then
      CurrentOrientation := DropCtlZone.ParentZone.Orientation
    else
      CurrentOrientation := TopZone.Orientation;

    if InsertOrientation = doHorizontal then
      DropDockSize := DockRect.Bottom - DockRect.Top
    else
    if InsertOrientation = doVertical then
      DropDockSize := DockRect.Right - DockRect.Left
    else
      DropDockSize := 0;

    OffsetXYLimitArr[doNoOrient] := 0;
    if DropCtlZone <> nil then
    begin
      OffsetXYLimitArr[doHorizontal] := DropCtlZone.TopLeft[doHorizontal] +
        Integer(MakeLast[InsertAt]) * (DropCtlZone.HeightWidth[doHorizontal] - ControlXYLimitArr[doHorizontal]);
      if (FDropOnZone <> nil) and (InsertOrientation = doHorizontal) then
        OffsetXYLimitArr[doHorizontal] := FDropOnZone.ZoneLimit - Round((FDropOnZone.ZoneLimit -
          FDropOnZone.ParentZone.ChildZones.LimitBegin) * (DropDockSize + BorderWidth) /
          (FDropOnZone.ParentZone.Height));
      OffsetXYLimitArr[doVertical] := DropCtlZone.TopLeft[doVertical] +
        Integer(MakeLast[InsertAt]) * (DropCtlZone.HeightWidth[doVertical] - ControlXYLimitArr[doVertical]);
      if (FDropOnZone <> nil) and (InsertOrientation = doVertical) then
        OffsetXYLimitArr[doVertical] := FDropOnZone.ZoneLimit - Round((FDropOnZone.ZoneLimit -
          FDropOnZone.ParentZone.ChildZones.LimitBegin) * (DropDockSize + BorderWidth) /
          (FDropOnZone.ParentZone.Width));
    end
    else
    begin
      if TopZone.VisibleChildCount = 0 then
      begin
        OffsetXYLimitArr[doHorizontal] := 0;
        OffsetXYLimitArr[doVertical] := 0;
      end
      else
      begin
        OffsetXYLimitArr[doHorizontal] := Integer(MakeLast[InsertAt]) * ControlXYLimitArr[doHorizontal];
        OffsetXYLimitArr[doVertical] := Integer(MakeLast[InsertAt]) * ControlXYLimitArr[doVertical];
      end;
    end;

    if TopOrientation <> InsertOrientation then
    begin
      LastZone := DockZoneClass.Create(Self);
      if InsertOrientation <> CurrentOrientation then
        InsertNewParent(LastZone, DropCtlZone, InsertOrientation, MakeLast[InsertAt], True)
      else
        InsertSibling(LastZone, DropCtlZone, MakeLast[InsertAt], True);
      LastZone.Orientation := TopOrientation;
      LastLevel := 0;
    end
    else
    begin
      LastLevel := 1;
      if TopZone.ChildCount > 0 then
      begin
        ReadZone(False);
        if InsertOrientation <> CurrentOrientation then
          InsertNewParent(LastZone, DropCtlZone, InsertOrientation, MakeLast[InsertAt], True)
        else
          InsertSibling(LastZone, DropCtlZone, MakeLast[InsertAt], True);
        LastZone.ZoneLimit := ZoneLimit + OffsetXYLimitArr[InsertOrientation];
      end
      else
      begin
        LastLevel := 0;
        LastZone := TopZone;
      end;
    end;

    OffsetXYLimitArr[doHorizontal] := LastZone.TopLeft[doHorizontal];
    OffsetXYLimitArr[doVertical] := LastZone.TopLeft[doVertical];

    // (rom) is this rock solid?
    while True do
    begin
      ReadZone(True);
      if Level = TreeStreamEndFlag then
        Break;
    end;
  finally
    Stream.Free;
    EndUpdate;
  end;
  SetNewBounds(nil);
end;

procedure TJvDockVIDVCTree.DrawDockGrabber(Control: TControl; const ARect: TRect);
var
  lbVCDockZone: TJvDockVIDVCZone;
  DrawRect: TRect;

  procedure DrawCloseButton(Left, Top: Integer);
  var
    ADockClient: TJvDockClient;
  begin
    if lbVCDockZone <> nil then
    begin
      ADockClient := FindDockClient(Control);
      if (ADockClient <> nil) and (not ADockClient.EnableCloseButton) then
        Exit;
      DrawFrameControl(Canvas.Handle, Rect(Left, Top, Left + ButtonWidth,
        Top + ButtonHeight), DFC_CAPTION, DFCS_CAPTIONCLOSE or Integer(lbVCDockZone.CloseBtnDown) * DFCS_PUSHED)
    end;
  end;

  procedure DrawExpendBotton(Left, Top: Integer);
  const
    {$IFDEF COMPILER6_UP}
    ArrowOrient: array [TAlign] of DWORD =
      (0, DFCS_SCROLLUP, DFCS_SCROLLDOWN, DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT, 0, 0);
    {$ELSE}
    ArrowOrient: array [TAlign] of DWORD =
      (0, DFCS_SCROLLUP, DFCS_SCROLLDOWN, DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT, 0);
    {$ENDIF COMPILER6_UP}
    CurrArrow: array [Boolean, TDockOrientation] of TAlign =
      ((alNone, alLeft, alTop), (alNone, alRight, alBottom));
  var
    InActive: Boolean;
    IsMaximum: Boolean;
  begin
    if lbVCDockZone <> nil then
    begin
      InActive := not ((lbVCDockZone.ParentZone.Orientation <> DockSiteOrientation) and
        (lbVCDockZone.ParentZone.VisibleChildCount >= 2));
      IsMaximum := lbVCDockZone.ZoneSizeStyle in [zssMaximum];
      DrawFrameControl(Canvas.Handle, Rect(Left, Top, Left + ButtonWidth,
        Top + ButtonHeight), DFC_SCROLL,
        ArrowOrient[CurrArrow[IsMaximum, DockSiteOrientation]] +
        Cardinal(InActive) * (DFCS_INACTIVE) + Cardinal(lbVCDockZone.ExpandButtonDown) * DFCS_PUSHED);
    end;
  end;

  procedure DrawGrabberLine(Left, Top, Right, Bottom: Integer);
  begin
    if (Left >= Right) or (Top >= Bottom) then
      Exit;
    with Canvas do
    begin
      Pen.Color := clBtnHighlight;
      MoveTo(Right, Top);
      LineTo(Left, Top);
      LineTo(Left, Bottom);
      Pen.Color := clBtnShadow;
      LineTo(Right, Bottom);
      LineTo(Right, Top - 1);
    end;
  end;

begin
  lbVCDockZone := TJvDockVIDVCZone(FindControlZone(Control));
  DrawRect := ARect;
  Canvas.Brush.Color := TWinControlAccessProtected(DockSite).Color;
  Canvas.FillRect(DrawRect);
  with ARect do
    case GrabbersPosition of
      gpLeft:
        begin
          DrawExpendBotton(Left + BorderWidth + LeftOffset, Top + TopOffset + ButtonHeight + ButtonSplitter +
            BorderWidth);
          DrawCloseButton(Left + BorderWidth + LeftOffset, Top + TopOffset + BorderWidth);
          DrawGrabberLine(Left + BorderWidth + LeftOffset + 3, Top + 2 * ButtonHeight + TopOffset + ButtonSplitter +
            BottomOffset + BorderWidth + 3, Left + BorderWidth + LeftOffset + 5, Bottom - BorderWidth - 2);
          DrawGrabberLine(Left + BorderWidth + LeftOffset + 7, Top + 2 * ButtonHeight + TopOffset + ButtonSplitter +
            BottomOffset + BorderWidth + 3, Left + BorderWidth + LeftOffset + 9, Bottom - BorderWidth - 2);
        end;
      gpTop:
        begin
          DrawExpendBotton(Right - LeftOffset - 2 * ButtonWidth - ButtonSplitter - BorderWidth, Top + TopOffset +
            BorderWidth);
          DrawCloseButton(Right - LeftOffset - ButtonWidth - BorderWidth, Top + TopOffset + BorderWidth);
          DrawGrabberLine(Left + BorderWidth, Top + BorderWidth + TopOffset + 3, Right - 2 * ButtonWidth - RightOffset -
            ButtonSplitter - LeftOffset - BorderWidth - 3, Top + BorderWidth + TopOffset + 5);
          DrawGrabberLine(Left + BorderWidth, Top + BorderWidth + TopOffset + 7, Right - 2 * ButtonWidth - RightOffset -
            ButtonSplitter - LeftOffset - BorderWidth - 3, Top + BorderWidth + TopOffset + 9);
        end;
      gpBottom:
        begin
        end;
      gpRight:
        begin
        end;
    end;

end;

procedure TJvDockVIDVCTree.ResetBounds(Force: Boolean);
var
  R: TRect;
begin
  if not (csLoading in DockSite.ComponentState) and
    (TopZone <> nil) and (DockSite.DockClientCount > 0) then
  begin
    R := DockSite.ClientRect;
    if DockSite is TJvDockConjoinPanel then
    begin
      if R.Right = R.Left then
        Inc(R.Right, DockSite.Parent.UndockWidth);
      if R.Bottom = R.Top then
        Inc(R.Bottom, DockSite.Parent.UndockHeight);
    end;
    if Force or (not CompareMem(@R, @PreviousRect, SizeOf(TRect))) then
    begin
      case TopZone.Orientation of
        doHorizontal:
          begin
            if R.Right - R.Left > 0 then
              TopZone.ZoneLimit := R.Right - R.Left;
            if R.Bottom - R.Top > 0 then
              TopXYLimit := R.Bottom - R.Top;
          end;
        doVertical:
          begin
            if R.Bottom - R.Top > 0 then
              TopZone.ZoneLimit := R.Bottom - R.Top;
            if R.Right - R.Left > 0 then
              TopXYLimit := R.Right - R.Left;
          end;
      end;
      if DockSite.DockClientCount > 0 then
      begin
        if not JvGlobalDockIsLoading then
        begin
          if (R.Bottom - R.Top > 0) and (PreviousRect.Bottom - PreviousRect.Top > 0) then
            ScaleBy := (R.Bottom - R.Top) / (PreviousRect.Bottom - PreviousRect.Top)
          else
            ScaleBy := 1;

          ShiftScaleOrientation := doHorizontal;

          if (UpdateCount = 0) and (ScaleBy <> 1) then
            ForEachAt(nil, ScaleZone, tskForward);

          if (R.Right - R.Left > 0) and (PreviousRect.Right - PreviousRect.Left > 0) then
            ScaleBy := (R.Right - R.Left) / (PreviousRect.Right - PreviousRect.Left)
          else
            ScaleBy := 1;

          ShiftScaleOrientation := doVertical;

          if (UpdateCount = 0) and (ScaleBy <> 1) then
            ForEachAt(nil, ScaleZone, tskForward);
        end;

        SetNewBounds(nil);
        if UpdateCount = 0 then
          ForEachAt(nil, UpdateZone, tskForward);

        PreviousRect := R;
      end;
    end;
  end;
end;

procedure TJvDockVIDVCTree.DrawSplitterRect(const ARect: TRect);
var
  Rect: TRect;
begin
  inherited DrawSplitterRect(ARect);
  Rect := ARect;
  InflateRect(Rect, 1, 1);
  DrawFrameControl(Canvas.Handle, Rect, DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_ADJUSTRECT);
end;

procedure TJvDockVIDVCTree.SetActiveControl(const Value: TControl);
begin
  if GetActiveControl <> Value then
  begin
    inherited SetActiveControl(Value);
    DockSite.Invalidate;
  end;
end;

procedure TJvDockVIDVCTree.WindowProc(var Msg: TMessage);
var
  Align: TAlign;
begin
  if Msg.Msg = CM_DOCKCLIENT then
  begin
    Align := TCMDockClient(Msg).DockSource.DropAlign;
    TCMDockClient(Msg).DockSource.DockRect := gi_DockRect;
    GetDockEdge(gi_DockRect, TCMDockClient(Msg).DockSource.DragPos, Align, TCMDockClient(Msg).DockSource.Control);
  end;
  inherited WindowProc(Msg);
end;

procedure TJvDockVIDVCTree.SplitterMouseUp;
var
  OldLimit: Integer;
  Zone: TJvDockZone;
begin
  Mouse.Capture := 0;
  DrawSizeSplitter;
  ReleaseDC(SizingWnd, SizingDC);

  OldLimit := SizingZone.ZoneLimit;

  ShiftScaleOrientation := SizingZone.ParentZone.Orientation;
  if SizingZone.ParentZone.Orientation = doHorizontal then
    SizingZone.ZoneLimit := SizePos.y + (SplitterWidth div 2)
  else
    SizingZone.ZoneLimit := SizePos.x + (SplitterWidth div 2);

  ParentLimit := SizingZone.LimitBegin;
  if OldLimit - ParentLimit > 0 then
    ScaleBy := (SizingZone.ZoneLimit - ParentLimit) / (OldLimit - ParentLimit)
  else
    ScaleBy := 1;

  if SizingZone.ChildZones <> nil then
    ForEachAt(SizingZone.ChildZones, ScaleChildZone, tskForward);

  Zone := SizingZone;
  while (Zone.NextSibling <> nil) and (not Zone.NextSibling.Visibled) do
  begin
    Zone.NextSibling.ZoneLimit := SizingZone.ZoneLimit;
    Zone := Zone.NextSibling;
  end;

  if SizingZone.NextSibling <> nil then
  begin
    if SizingZone.NextSibling.ZoneLimit - OldLimit > 0 then
      ScaleBy := (SizingZone.NextSibling.ZoneLimit - SizingZone.ZoneLimit) / (SizingZone.NextSibling.ZoneLimit -
        OldLimit)
    else
      ScaleBy := 1;
    ParentLimit := SizingZone.NextSibling.ZoneLimit;

    if SizingZone.NextSibling.ChildZones <> nil then
      ForEachAt(SizingZone.NextSibling.ChildZones, ScaleSiblingZone, tskForward);
  end;

  SetNewBounds(SizingZone.ParentZone);
  ForEachAt(SizingZone.ParentZone, UpdateZone, tskForward);
  SizingZone := nil;
end;

procedure TJvDockVIDVCTree.InsertSibling(NewZone, SiblingZone: TJvDockZone;
  InsertLast, Update: Boolean);
begin
  if FDropOnZone <> nil then
    SiblingZone := FDropOnZone;
  inherited InsertSibling(NewZone, SiblingZone, InsertLast, Update);
end;

{procedure TJvDockVIDVCTree.PositionDockRect(Client, DropCtl: TControl;
  DropAlign: TAlign; var DockRect: TRect);
label
  LBDropCtlExist;
var
  VisibleClients, NewX, NewY, NewWidth, NewHeight: Integer;
  Zone: TJvDockZone;
  HTFlag: Integer;
  MousePos: TPoint;
  Scale: Double;
  CtrlRect: TRect;

  procedure DockOverSplitter;
  begin
    NewX := Zone.ParentZone.Left;
    NewY := Zone.ParentZone.Top;
    NewWidth := Zone.ParentZone.Width;
    NewHeight := Zone.ParentZone.Height;
    case Zone.ParentZone.Orientation of
      doHorizontal:
        begin
          Scale := (Zone.ZoneLimit - Zone.ParentZone.ChildZones.LimitBegin) / NewHeight;
          NewHeight := Min(NewHeight div 2, Client.ClientHeight);
          //NewY := Zone.ZoneLimit - Round(NewHeight * Scale);
        end;
      doVertical:
        begin
          Scale := (Zone.ZoneLimit - Zone.ParentZone.ChildZones.LimitBegin) / NewWidth;
          NewWidth := Min(NewWidth div 2, Client.ClientWidth);
          NewX := Zone.ZoneLimit - Round(NewWidth * Scale);
        end;
    end;
    DockRect := Bounds(NewX, NewY, NewWidth, NewHeight);
    if Zone.Visibled then
    begin
      if Zone.ParentZone.Orientation = doHorizontal then
        JvGlobalDockManager.DragObject.DropAlign := alBottom
      else
      if Zone.ParentZone.Orientation = doVertical then
        JvGlobalDockManager.DragObject.DropAlign := alRight;
      JvGlobalDockManager.DragObject.DropOnControl := Zone.ChildControl;
      FDropOnZone := Zone;
    end;
  end;

begin
  if DropAlign = alNone then
    DropAlign := alClient;
  VisibleClients := DockSite.VisibleDockClientCount;
  FDropOnZone := nil;

  MousePos := JvGlobalDockManager.DragObject.DragPos;
  MapWindowPoints(0, DockSite.Handle, MousePos, 2);
  Zone := InternalHitTest(MousePos, HTFlag);
  if Zone <> nil then
    if Zone.ChildControl <> nil then
      if (HTFlag = HTCaption) or (HTFlag = HTClose) then
      begin
        DockRect := Zone.ChildControl.BoundsRect;
        JvGlobalDockManager.DragObject.DropAlign := alClient;
        if Zone.ChildControl is TJvDockTabHostForm then
        begin
          if JvGlobalDockManager.DragObject is TJvDockVIDVCDragDockObject then
            TJvDockVIDVCDragDockObject(JvGlobalDockManager.DragObject).FDropTabControl :=
              TJvDockVIDVCTabPageControl(TJvDockTabHostForm(Zone.ChildControl).PageControl);
        end
        else
        begin
          if JvGlobalDockManager.DragObject is TJvDockVIDVCDragDockObject then
            TJvDockVIDVCDragDockObject(JvGlobalDockManager.DragObject).FDropTabControl := nil;
        end;
      end;

  if DropCtl = nil then
  begin
    if Zone <> nil then
    begin
      if Zone.ChildControl <> nil then
      begin
        if (HTFlag = HTCaption) or (HTFlag = HTClose) then
          JvGlobalDockManager.DragObject.DropOnControl := Zone.ChildControl
        else
        if HTFlag = HTClient then
        begin
          DropCtl := Zone.ChildControl;
          goto LBDropCtlExist;
        end
        else
        if HTFlag = HTSplitter then
          DockOverSplitter;
      end
      else
      if HTFlag = HTSplitter then
      begin
        DockOverSplitter;
      end
      else
        Exit;
    end
    else
    begin
      DockRect := Rect(0, 0, DockSite.ClientWidth, DockSite.ClientHeight);

      if VisibleClients > 0 then
        with DockRect do
          case DropAlign of
            alLeft:
              Right := Right div 2;
            alRight:
              Left := Right div 2;
            alTop:
              Bottom := Bottom div 2;
            alBottom:
              Top := Bottom div 2;
          end;
    end;
  end
  else
  begin

  LBDropCtlExist:
    Zone := FindControlZone(DropCtl);
    CtrlRect := DockRect;
    MapWindowPoints(0, DockSite.Handle, CtrlRect, 2);
    if Zone <> nil then
    begin
      if Zone.ParentZone.Orientation = doVertical then
      begin
        if (DropAlign = alRight) and (Zone.NextSibling <> nil) then
        begin
          DockOverSplitter;
          MapWindowPoints(DockSite.Handle, 0, DockRect, 2);
          Exit;
        end
        else
        if (DropAlign = alLeft) and (Zone.PrevSibling <> nil) then
        begin
          Zone := Zone.PrevSibling;
          DockOverSplitter;
          MapWindowPoints(DockSite.Handle, 0, DockRect, 2);
          Exit;
        end
        else
{        begin
          if DropAlign in [alLeft, alRight] then
            CtrlRect := Bounds(Zone.ParentZone.Left, Zone.ParentZone.Top, Zone.ParentZone.Width, Zone.ParentZone.Height)
          else
          if DropAlign in [alTop, alBottom, alClient] then
          begin
            CtrlRect := DropCtl.BoundsRect;
            if DropAlign in [alLeft, alRight] then
              Dec(CtrlRect.Top, GrabberSize);
          end;
            OffsetRect(CtrlRect, 0, GrabberSize);
        end;
      end
      else
      if Zone.ParentZone.Orientation = doHorizontal then
      begin
        if (DropAlign = alBottom) and (Zone.NextSibling <> nil) then
        begin
          DockOverSplitter;
          MapWindowPoints(DockSite.Handle, 0, DockRect, 2);
          Exit;
        end
        else
        if (DropAlign = alTop) and (Zone.PrevSibling <> nil) then
        begin
          Zone := Zone.PrevSibling;
          DockOverSplitter;
          MapWindowPoints(DockSite.Handle, 0, DockRect, 2);
          Exit;
        end
        else
        begin
{          if DropAlign in [alTop, alBottom] then
            CtrlRect := Bounds(Zone.ParentZone.Left, Zone.ParentZone.Top, Zone.ParentZone.Width, Zone.ParentZone.Height)
          else
          if DropAlign in [alLeft, alRight, alClient] then
          begin
            CtrlRect := DropCtl.BoundsRect;
            if DropAlign in [alLeft, alRight] then
              Dec(CtrlRect.Top, GrabberSize);
          end;
          //if DropAlign in [alLeft, alRight] then
            OffsetRect(CtrlRect, 0, GrabberSize);
        end;
      end
      else
      begin
        CtrlRect := DropCtl.BoundsRect;

          //Dec(CtrlRect.Top, GrabberSize);
          //OffsetRect(CtrlRect, 0, GrabberSize);
      end;

      NewX := CtrlRect.Left;
      if DropAlign in [alTop, alBottom] then
        NewY := CtrlRect.Top
      else
        NewY := CtrlRect.Top - GrabberSize;
      NewWidth := CtrlRect.Right - CtrlRect.Left;
      NewHeight := CtrlRect.Bottom - CtrlRect.Top;
      if DropAlign in [alLeft, alRight] then
        NewWidth := Min(Client.UndockWidth, NewWidth div 2)
      else
      if DropAlign in [alTop, alBottom] then
        NewHeight := Min(Client.UndockHeight, NewHeight div 2);
      case DropAlign of
        alRight:
          Inc(NewX, CtrlRect.Right - CtrlRect.Left - NewWidth);
        alBottom:
          Inc(NewY, CtrlRect.Bottom - CtrlRect.Top - NewHeight);
      end;
      DockRect := Bounds(NewX, NewY, NewWidth, NewHeight);
      if DropAlign = alClient then
        DockRect := Bounds(NewX, NewY, NewWidth, NewHeight);
      if DropAlign = alNone then
      begin
      end;
    end;
  end;
  MapWindowPoints(DockSite.Handle, 0, DockRect, 2);
end;                                             }

function TJvDockVIDVCTree.GetDockEdge(DockRect: TRect; MousePos: TPoint;
  var DropAlign: TAlign; Control: TControl): TControl;
begin
  Result := inherited GetDockEdge(DockRect, MousePos, DropAlign, Control);
  if FLockDropDockSizeCount = 0 then
  begin
    if DropAlign in [alLeft, alRight] then
      DropDockSize := DockRect.Right - DockRect.Left
    else
    if DropAlign in [alTop, alBottom] then
      DropDockSize := DockRect.Bottom - DockRect.Top
    else
      DropDockSize := 0;
    Self.DockRect := DockRect;
  end;
end;

function TJvDockVIDVCTree.GetLeftGrabbersHTFlag(const MousePos: TPoint;
  out HTFlag: Integer; Zone: TJvDockZone): TJvDockZone;
begin
  if (MousePos.X >= Zone.Left + BorderWidth) and (MousePos.X <= Zone.Left + BorderWidth + GrabberSize) and
    (MousePos.Y >= Zone.Top) and (MousePos.Y <= Zone.Top + Zone.Height) then
  begin
    Result := Zone;
    with Zone.ChildControl do
    begin
      if PtInRect(Rect(
        Left - GrabberSize + LeftOffset,
        Top + TopOffset,
        Left - GrabberSize + LeftOffset + ButtonWidth,
        Top + TopOffset + ButtonHeight), MousePos) then
        HTFlag := HTCLOSE
      else
      if PtInRect(Rect(
        Left - GrabberSize + LeftOffset,
        Top + ButtonHeight + TopOffset + ButtonSplitter,
        Left - GrabberSize + LeftOffset + ButtonWidth,
        Top + 2 * ButtonHeight + TopOffset + ButtonSplitter), MousePos) then
        HTFlag := HTEXPAND
      else
        HTFlag := HTCAPTION;
    end;
  end
  else
    Result := nil;
end;

function TJvDockVIDVCTree.GetTopGrabbersHTFlag(const MousePos: TPoint;
  out HTFlag: Integer; Zone: TJvDockZone): TJvDockZone;
begin
  if (MousePos.Y >= Zone.Top + BorderWidth) and (MousePos.Y <= Zone.Top + BorderWidth + GrabberSize) and
    (MousePos.X >= Zone.Left) and (MousePos.X <= Zone.Left + Zone.Width) then
  begin
    Result := Zone;
    with Zone.ChildControl do
    begin
      if PtInRect(Rect(
        Left + Width - ButtonWidth - RightOffset,
        Top - GrabberSize + TopOffset,
        Left + Width - RightOffset,
        Top - GrabberSize + TopOffset + ButtonHeight), MousePos) then
        HTFlag := HTCLOSE
      else
      if PtInRect(Rect(
        Left + Width - 2 * ButtonWidth - RightOffset - ButtonSplitter,
        Top - GrabberSize + TopOffset,
        Left + Width - ButtonWidth - RightOffset - ButtonSplitter,
        Top - GrabberSize + TopOffset + ButtonHeight), MousePos) then
        HTFlag := HTEXPAND
      else
        HTFlag := HTCAPTION;
    end;
  end
  else
    Result := nil;
end;

procedure TJvDockVIDVCTree.InsertNewParent(NewZone, SiblingZone: TJvDockZone;
  ParentOrientation: TDockOrientation; InsertLast, Update: Boolean);
begin
  if FDropOnZone <> nil then
  begin
    SiblingZone := FDropOnZone;
    InsertSibling(NewZone, SiblingZone, InsertLast, Update);
  end
  else
    inherited InsertNewParent(NewZone, SiblingZone, ParentOrientation,
      InsertLast, Update);
end;

procedure TJvDockVIDVCTree.RemoveZone(Zone: TJvDockZone; Hide: Boolean);
begin
  if (FDropOnZone <> nil) and
    ((FDropOnZone.NextSibling = Zone) or (FDropOnZone = Zone)) then
    FDropOnZone := nil;
  inherited RemoveZone(Zone, Hide);
end;

procedure TJvDockVIDVCSplitter.Paint;
var
  Rect: TRect;
begin
  Rect := ClientRect;
  Inc(Rect.Right, 2);
  case Align of
    alLeft:
      InflateRect(Rect, 0, 2);
    alRight:
      begin
        OffsetRect(Rect, -1, 0);
        InflateRect(Rect, 0, 2);
      end;
    alTop:
      begin
        Inc(Rect.Bottom, 2);
        InflateRect(Rect, 2, 0);
      end;
    alBottom:
      begin
        Dec(Rect.Top, 2);
        InflateRect(Rect, 2, 1);
      end;
  end;
  Canvas.Brush.Color := Color;
  DrawFrameControl(Canvas.Handle, Rect, DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_ADJUSTRECT);
end;

procedure TJvDockVIDVCTree.GetSiteInfo(Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
var
  Zone: TJvDockZone;
  HTFlag: Integer;
  Pos: TPoint;
  Align: TAlign;
begin
  Pos := DockSite.ScreenToClient(MousePos);
  Zone := InternalHitTest(Pos, HTFlag);
  if Zone <> nil then
  begin
    if HTFlag = HTSPLITTER then
    begin
      InfluenceRect := GetSplitterRect(Zone);
      MapWindowPoints(DockSite.Handle, 0, InfluenceRect, 2);
    end
    else
    begin
      Pos := MousePos;
      if Zone.ChildControl <> nil then
        Pos := Zone.ChildControl.ScreenToClient(MousePos);
      Align := ComputeVIDDockingRect(Zone.ChildControl, Client, InfluenceRect, Pos);
      if (Align = alNone) or (Client = Zone.ChildControl) then
      begin
        InfluenceRect := Rect(0, 0, 0, 0);
        CanDock := False;
      end
      else
      begin
        if Zone.ParentZone.Orientation = doVertical then
        begin
          if (Align = alRight) and (Zone.NextSibling <> nil) and (Zone.NextSibling.Visibled) then
          begin
            InfluenceRect := GetSplitterRect(Zone);
            InflateRect(InfluenceRect, DefExpandoRect, 0);
          end
          else
          if (Align = alLeft) and (Zone.PrevSibling <> nil) and (Zone.PrevSibling.Visibled) then
          begin
            InfluenceRect := GetSplitterRect(Zone.PrevSibling);
            InflateRect(InfluenceRect, DefExpandoRect, 0);
          end
          else
            Exit;
        end
        else
        if Zone.ParentZone.Orientation = doHorizontal then
        begin
          if (Align = alBottom) and (Zone.NextSibling <> nil) and (Zone.NextSibling.Visibled) then
          begin
            InfluenceRect := GetSplitterRect(Zone);
            InflateRect(InfluenceRect, 0, DefExpandoRect);
          end
          else
          if (Align = alTop) and (Zone.PrevSibling <> nil) and (Zone.PrevSibling.Visibled) then
          begin
            InfluenceRect := GetSplitterRect(Zone.PrevSibling);
            InflateRect(InfluenceRect, 0, DefExpandoRect);
          end
          else
            Exit;
        end
        else
          Exit;
      end;
      MapWindowPoints(DockSite.Handle, 0, InfluenceRect, 2);
    end;
  end
  else
  begin
    InfluenceRect := Rect(0, 0, 0, 0);
    CanDock := False;
  end;
end;

procedure TJvDockVIDVCTree.LockDropDockSize;
begin
  Inc(FLockDropDockSizeCount);
end;

procedure TJvDockVIDVCTree.UnlockDropDockSize;
begin
  Dec(FLockDropDockSizeCount);
  if FLockDropDockSizeCount < 0 then
    FLockDropDockSizeCount := 0;
end;

procedure TJvDockVIDVCTree.PaintDockGrabberRect(Canvas: TCanvas;
  Control: TControl; const ARect: TRect);
begin
end;

procedure TJvDockVIDVCTree.SetCaptionLeftOffset(const Value: Integer);
begin
  FCaptionLeftOffset := Value;
end;

procedure TJvDockVIDVCTree.SetCaptionRightOffset(const Value: Integer);
begin
  FCaptionRightOffset := Value;
end;

procedure TJvDockVIDVCTree.DrawCloseButton(Canvas: TCanvas; Zone: TJvDockZone; Left, Top: Integer);
var
  AZone: TJvDockAdvZone;
  ADockClient: TJvDockClient;
begin
  AZone := TJvDockAdvZone(Zone);
  if AZone <> nil then
  begin
    ADockClient := FindDockClient(Zone.ChildControl);
    if (ADockClient <> nil) and not ADockClient.EnableCloseButton then
      Exit;
    DrawFrameControl(Canvas.Handle, Rect(Left, Top, Left + ButtonWidth,
      Top + ButtonHeight), DFC_CAPTION, DFCS_CAPTIONCLOSE or Integer(AZone.CloseBtnDown) * DFCS_PUSHED)
  end;
end;

procedure TJvDockVIDVCTree.GetCaptionRect(var Rect: TRect);
begin
  case GrabbersPosition of
    gpTop:
      Rect.Bottom := Rect.Top + GrabberSize + 2;
    gpLeft:
      Rect.Right := Rect.Left + GrabberSize + 2;
  end;

end;

{procedure TJvDockVIDVCTree.AdjustDockRect(Control: TControl;
  var ARect: TRect);
begin
  if (DockSite.Align <> alClient) or (TopZone.VisibleChildTotal > 1) then
    inherited AdjustDockRect(Control, ARect);
end;                                         }

procedure TJvDockVIDVCTree.IgnoreZoneInfor(Stream: TMemoryStream);
var
  CompName: string;
begin
  Stream.Position := Stream.Position + 6;
  ReadControlName(Stream, CompName);
end;

//=== { TJvDockVIDVCConjoinPanel } ===========================================

function TJvDockVIDVCConjoinPanel.CreateDockManager: IDockManager;
var
  Option: TJvDockVIDVCConjoinServerOption;
begin
  Result := inherited CreateDockManager;
  if (ParentForm <> nil) and (ParentForm.DockClient.DockStyle <> nil) and (Result <> nil) then
  begin
    Option := TJvDockVIDVCConjoinServerOption(ParentForm.DockClient.DockStyle.ConjoinServerOption);
    (Result as IJvDockManager).GrabberSize := Option.GrabbersSize;
  end;
end;

procedure TJvDockVIDVCConjoinPanel.CustomDockDrop(Source: TJvDockDragDockObject;
  X, Y: Integer);
begin
  if not ((Source.Control.HostDockSite <> nil) and
    (Source.DropOnControl = Source.Control.HostDockSite.Parent) and
    (Source.DropAlign = alClient)) then
  begin
    inherited CustomDockDrop(Source, X, Y);
    ParentForm.Caption := '';
    if JvDockManager <> nil then
      JvDockManager.ActiveControl := Source.Control;
    if (Source.Control is TWinControl) and Source.Control.Visible and
      TWinControl(Source.Control).CanFocus then
      TWinControl(Source.Control).SetFocus;
  end;
end;

procedure TJvDockVIDVCConjoinPanel.CustomDockOver(Source: TJvDockDragDockObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  DropAlign: TAlign;
begin
  inherited CustomDockOver(Source, X, Y, State, Accept);
  if Accept and (Source is TJvDockVIDVCDragDockObject) then
    if State = dsDragMove then
    begin
      DropAlign := Source.DropAlign;
      JvDockManager.GetDockEdge(Source.EraseDockRect, Source.DragPos, DropAlign, Source.Control);
    end;
end;

procedure TJvDockVIDVCConjoinPanel.CustomGetDockEdge(Source: TJvDockDragDockObject;
  MousePos: TPoint; var DropAlign: TAlign);
begin
end;

procedure TJvDockVIDVCConjoinPanel.CustomGetSiteInfo(Source: TJvDockDragDockObject; Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  JvDockManager.GetSiteInfo(Client, InfluenceRect, MousePos, CanDock);
  CanDock := IsDockable(Self, Client, Source.DropOnControl, Source.DropAlign);
end;

function TJvDockVIDVCConjoinPanel.CustomUnDock(Source: TJvDockDragDockObject; NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  Result := inherited CustomUnDock(Source, NewTarget, Client);
end;

procedure TJvDockVIDVCConjoinPanel.DockDrop(Source: TDragDockObject;
  X, Y: Integer);
begin
  inherited DockDrop(Source, X, Y);
end;

procedure TJvDockVIDVCConjoinPanel.UpdateCaption(Exclude: TControl);
begin
  if VisibleDockClientCount > 1 then
    ParentForm.Caption := ''
  else
    inherited UpdateCaption(Exclude);
  Invalidate;
end;

//=== { TJvDockNewTabPageControl } ===========================================

constructor TJvDockVIDVCTabPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPanel := nil;
  TabWidth := 1;
  MultiLine := True;
  TabSheetClass := TJvDockVIDVCTabSheet;
  TabPanelClass := TJvDockTabPanel;
  FTempSheet := nil;
  TabPosition := tpBottom;
  FTabImageList := nil;
  Images := nil;
  if AOwner is TJvDockTabHostForm then
  begin
    FTabImageList := TCustomImageList.Create(AOwner);
    Images := FTabImageList;
  end;
end;

destructor TJvDockVIDVCTabPageControl.Destroy;
begin
  if FTabImageList <> nil then
  begin
    FTabImageList.Free;
    FTabImageList := nil;
  end;
  if FPanel <> nil then
  begin
    FPanel.Free;
    FPanel := nil;
  end;
  inherited Destroy;
end;

procedure TJvDockVIDVCTabPageControl.AfterConstruction;
begin
  inherited AfterConstruction;
  CreatePanel;
end;

procedure TJvDockVIDVCTabPageControl.Loaded;
begin
  inherited Loaded;
  CreatePanel;
end;

procedure TJvDockVIDVCTabPageControl.CreatePanel;
begin
  if FPanel = nil then
  begin
    FPanel := TabPanelClass.Create(Self);
    FPanel.Page := Self;
    FPanel.Parent := Self;
    FPanel.TabLeftOffset := 5;
    FPanel.TabRightOffset := 5;
    FPanel.TabTopOffset := 3;
    FPanel.TabBottomOffset := 3;
    ActiveSheetColor := clBtnFace;
    InactiveSheetColor := clBtnShadow;
  end;
  Resize;
end;

procedure TJvDockVIDVCTabPageControl.CreateWnd;
begin
  inherited CreateWnd;
end;

procedure TJvDockVIDVCTabPageControl.CustomDockDrop(Source: TJvDockDragDockObject;
  X, Y: Integer);
var
  ARect: TRect;
  I: Integer;
  VIDSource: TJvDockVIDVCDragDockObject;
  DockClient: TJvDockClient;
  Host: TJvDockConjoinHostForm;
  Index: Integer;
begin
  if Source.DropAlign in [alClient, alNone] then
  begin
    if Source is TJvDockVIDVCDragDockObject then
    begin
      JvDockLockWindow(nil);
      JvGlobalDockIsLoading := True;
      try
        DoFloatForm(Source.Control);
        FreeAllDockableForm;
        VIDSource := TJvDockVIDVCDragDockObject(Source);

        for I := 0 to VIDSource.SourceDockClientCount - 1 do
        begin
          Source.Control := VIDSource.SourceDockClients[I];
          inherited CustomDockDrop(Source, X, Y);
          if Source.Control is TCustomForm then
            if FTabImageList <> nil then
            begin
              Index := FTabImageList.AddIcon(TForm(Source.Control).Icon);
              if Index <> -1 then
                ActivePage.ImageIndex := Index;
            end;
        end;
      finally
        JvGlobalDockIsLoading := False;
        JvDockUnLockWindow;
        ReshowAllVisibleWindow;
        JvGlobalDockManager.DragObject.Control := nil;
      end;
    end;
  end
  else
  begin
    DockClient := FindDockClient(ParentForm);
    if DockClient <> nil then
    begin
      ARect := ParentForm.BoundsRect;
      Host := DockClient.CreateConjoinHostAndDockControl(ParentForm, Source.Control, Source.DropAlign);
      Host.BoundsRect := ARect;
      SetDockSite(ParentForm, False);
      SetDockSite(TWinControl(Source.Control), False);
      Host.Visible := True;
    end;
  end;
  FPanel.SelectSheet := nil;
  ParentForm.Caption := ActivePage.Caption;
end;

procedure TJvDockVIDVCTabPageControl.CustomDockOver(Source: TJvDockDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  ARect: TRect;
begin
  Accept := IsDockable(Self, Source.Control, Source.DropOnControl, Source.DropAlign);
  if Accept then
  begin
    if ParentForm.HostDockSite = nil then
    begin
      Source.DropAlign := ComputeVIDDockingRect(Self, Source.Control, ARect, Point(X, Y));
      if Source.DropAlign = alClient then
        ARect.Top := ARect.Top + JvDockGetSysCaptionHeight;

      if Accept and (Source.DropAlign <> alNone) then
      begin
        Source.DockRect := ARect;
        gi_DockRect := ARect;
      end;
    end
    else
    begin
      if ParentForm.HostDockSite is TJvDockCustomPanel then
      begin
        ARect := Source.DockRect;
        TJvDockCustomPanel(ParentForm.HostDockSite).JvDockManager.PositionDockRect(Source.Control, Source.DropOnControl,
          Source.DropAlign, ARect);
        Source.DockRect := ARect;
      end;
    end;
  end;
end;

procedure TJvDockVIDVCTabPageControl.CustomGetSiteInfo(Source: TJvDockDragDockObject; Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
const
  DefExpandoRect = 20;
var
  CH_BW: Integer;
  ARect: TRect;
begin
  CanDock := IsDockable(Self, Client, Source.DropOnControl, Source.DropAlign);
  if ParentForm.HostDockSite <> nil then
    CanDock := False;
  if CanDock then
  begin
    GetWindowRect(Parent.Handle, InfluenceRect);
    if PtInRect(InfluenceRect, MousePos) then
    begin
      ARect := InfluenceRect;
      InflateRect(ARect, -DefExpandoRect, -DefExpandoRect);

      CH_BW := JvDockGetSysCaptionHeightAndBorderWidth;
      Inc(ARect.Top, CH_BW + 1);
      Dec(ARect.Bottom, TabHeight);
      if PtInRect(ARect, MousePos) then
        InfluenceRect := Rect(0, 0, 0, 0);
    end;
  end;
end;

procedure TJvDockVIDVCTabPageControl.Change;
begin
  inherited Change;
  ParentForm.Caption := ActivePage.Caption;
  if ParentForm.HostDockSite is TJvDockCustomPanel then
  begin
    if ParentForm.Visible and ParentForm.CanFocus then
      ParentForm.SetFocus;
    ParentForm.HostDockSite.Invalidate;
  end;
  if (ActivePage <> nil) and (ActivePage.Visible) and (ActivePage.CanFocus) then
    if ParentForm.Visible and ParentForm.CanFocus then
      ActivePage.SetFocus;
end;

procedure TJvDockVIDVCTabPageControl.AdjustClientRect(var Rect: TRect);
begin
  Rect := ClientRect;
  if (Parent is TJvDockTabHostForm) and (VisibleDockClientCount = 1) then
    Exit;
  case TabPosition of
    tpTop:
      Inc(Rect.Top, Panel.FTabHeight - 1);
    tpBottom:
      Dec(Rect.Bottom, Panel.FTabHeight - 1);
    tpLeft:
      Inc(Rect.Left, Panel.FTabHeight - 1);
    tpRight:
      Dec(Rect.Right, Panel.FTabHeight - 1);
  end;
end;

procedure TJvDockVIDVCTabPageControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

procedure TJvDockVIDVCTabPageControl.DrawTab(TabIndex: Integer;
  const Rect: TRect; Active: Boolean);
begin
  inherited DrawTab(TabIndex, Rect, Active);
end;

function TJvDockVIDVCTabPageControl.GetActiveFont: TFont;
begin
  Result := FPanel.FActiveFont;
end;

function TJvDockVIDVCTabPageControl.GetActiveSheetColor: TColor;
begin
  Result := FPanel.FActiveSheetColor;
end;

function TJvDockVIDVCTabPageControl.GetInactiveFont: TFont;
begin
  Result := FPanel.FInactiveFont;
end;

function TJvDockVIDVCTabPageControl.GetInactiveSheetColor: TColor;
begin
  Result := FPanel.Color;
end;

function TJvDockVIDVCTabPageControl.GetTabBottomOffset: Integer;
begin
  Result := FPanel.TabBottomOffset;
end;

function TJvDockVIDVCTabPageControl.GetTabLeftOffset: Integer;
begin
  Result := FPanel.TabLeftOffset;
end;

function TJvDockVIDVCTabPageControl.GetTabRightOffset: Integer;
begin
  Result := FPanel.TabRightOffset;
end;

function TJvDockVIDVCTabPageControl.GetTabTopOffset: Integer;
begin
  Result := FPanel.TabTopOffset;
end;

procedure TJvDockVIDVCTabPageControl.Paint;
begin
  inherited Paint;
end;

procedure TJvDockVIDVCTabPageControl.Resize;
begin
  inherited Resize;
  if FPanel = nil then
    Exit;
  case TabPosition of
    tpLeft:
      begin
        FPanel.Left := 0;
        FPanel.Width := Panel.FTabHeight;
        FPanel.Top := 0;
        FPanel.Height := Height;
      end;
    tpRight:
      begin
        FPanel.Left := Width - Panel.FTabHeight;
        FPanel.Top := 0;
        FPanel.Width := Panel.FTabHeight;
        FPanel.Height := Height;
      end;
    tpTop:
      begin
        FPanel.Left := 0;
        FPanel.Top := 0;
        FPanel.Width := Width;
        FPanel.Height := Panel.FTabHeight;
      end;
    tpBottom:
      begin
        FPanel.Left := 0;
        FPanel.Top := Height - Panel.FTabHeight;
        FPanel.Width := Width;
        FPanel.Height := Panel.FTabHeight;
      end;
  end;
end;

procedure TJvDockVIDVCTabPageControl.SetActiveFont(const Value: TFont);
begin
  FPanel.FActiveFont.Assign(Value);
  if ActivePage <> nil then
    TJvDockVIDVCTabSheet(ActivePage).SetSheetSort(ActivePage.Caption);
  FPanel.Invalidate;
end;

procedure TJvDockVIDVCTabPageControl.SetActiveSheetColor(const Value: TColor);
begin
  FPanel.FActiveSheetColor := Value;
  FPanel.Invalidate;
end;

procedure TJvDockVIDVCTabPageControl.SetInactiveFont(const Value: TFont);
var
  I: Integer;
begin
  FPanel.FInactiveFont.Assign(Value);
  for I := 0 to Count - 1 do
    if Pages[I] <> ActivePage then
      TJvDockVIDVCTabSheet(Pages[I]).SetSheetSort(Pages[I].Caption);
  FPanel.Invalidate;
end;

procedure TJvDockVIDVCTabPageControl.SetInactiveSheetColor(const Value: TColor);
begin
  if FPanel.Color <> Value then
  begin
    FPanel.Color := Value;
    FPanel.Invalidate;
  end;
end;

procedure TJvDockVIDVCTabPageControl.SetTabBottomOffset(const Value: Integer);
begin
  if FPanel.TabBottomOffset <> Value then
  begin
    FPanel.TabBottomOffset := Value;
    FPanel.Invalidate;
  end;
end;

procedure TJvDockVIDVCTabPageControl.SetTabHeight(Value: Smallint);
begin
  inherited SetTabHeight(Value);
  if Panel.FTabHeight <> Value then
  begin
    Panel.FTabHeight := Value;
    FPanel.Invalidate;
  end;
end;

procedure TJvDockVIDVCTabPageControl.SetTabLeftOffset(const Value: Integer);
begin
  if FPanel.TabLeftOffset <> Value then
  begin
    FPanel.TabLeftOffset := Value;
    FPanel.Invalidate;
  end;
end;

procedure TJvDockVIDVCTabPageControl.SetTabPosition(Value: TTabPosition);
begin
  Assert(Value in [tpTop, tpBottom], RsEDockCannotSetTabPosition);
  inherited SetTabPosition(Value);
  Resize;
end;

procedure TJvDockVIDVCTabPageControl.SetTabRightOffset(const Value: Integer);
begin
  if FPanel.TabRightOffset <> Value then
  begin
    FPanel.TabRightOffset := Value;
    FPanel.Invalidate;
  end;
end;

procedure TJvDockVIDVCTabPageControl.SetTabTopOffset(const Value: Integer);
begin
  if FPanel.TabTopOffset <> Value then
  begin
    FPanel.TabTopOffset := Value;
    FPanel.Invalidate;
  end;
end;

procedure TJvDockVIDVCTabPageControl.SetActivePage(Page: TJvDockTabSheet);
begin
  inherited SetActivePage(Page);
  FPanel.Invalidate;
end;

procedure TJvDockVIDVCTabPageControl.DockDrop(Source: TDragDockObject;
  X, Y: Integer);
var
  Index: Integer;
begin
  inherited DockDrop(Source, X, Y);
  FPanel.SelectSheet := nil;
  ParentForm.Caption := ActivePage.Caption;
  if Source.Control is TCustomForm then
  begin
    if Source.Control.Visible and (Source.Control.Parent is TJvDockTabSheet) then
      ActivePage := TJvDockTabSheet(Source.Control.Parent);
    if FTabImageList <> nil then
    begin
      Index := FTabImageList.AddIcon(TForm(Source.Control).Icon);
      if (Index <> -1) and (ActivePage <> nil) then
        ActivePage.ImageIndex := Index;
    end;
  end;
end;

function TJvDockVIDVCTabPageControl.GetDockClientFromMousePos(MousePos: TPoint): TControl;
var
  PageIndex: Integer;
begin
  Result := nil;
  case TabPosition of
    tpTop:
      PageIndex := Panel.FindSheetWithPos(MousePos.X, MousePos.Y, 0, Panel.Height - TabBottomOffset);
    tpBottom:
      PageIndex := Panel.FindSheetWithPos(MousePos.X, MousePos.Y, TabBottomOffset, Panel.Height);
    tpLeft:
      PageIndex := Panel.FindSheetWithPos(MousePos.Y, MousePos.X, 0, Panel.Height - TabBottomOffset);
    tpRight:
      PageIndex := Panel.FindSheetWithPos(MousePos.Y, MousePos.X, TabBottomOffset, Panel.Height);
  else
    PageIndex := -1;
  end;
  if PageIndex >= 0 then
  begin
    Result := Pages[PageIndex].Controls[0];
    if Result.HostDockSite <> Self then
      Result := nil;
  end;
end;

procedure TJvDockVIDVCTabPageControl.CustomGetDockEdge(Source: TJvDockDragDockObject;
  MousePos: TPoint; var DropAlign: TAlign);
var
  ARect: TRect;
begin
  DropAlign := ComputeVIDDockingRect(Self, Source.Control, ARect, MousePos);
end;

function TJvDockVIDVCTabPageControl.GetVisibleSheetCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Pages[I].TabVisible then
      Inc(Result);
end;

procedure TJvDockVIDVCTabPageControl.UpdateCaption(Exclude: TControl);
begin
  ParentForm.Caption := ActivePage.Caption;
  if Parent <> nil then
  begin
    Parent.Invalidate;
    if Parent.HostDockSite <> nil then
      Parent.HostDockSite.Invalidate;
  end;
end;

procedure TJvDockVIDVCTabPageControl.SetHotTrack(Value: Boolean);
begin
  inherited SetHotTrack(Value);
end;

procedure TJvDockVIDVCTabPageControl.SetImages(Value: TCustomImageList);
begin
  inherited SetImages(Value);
  if Panel <> nil then
  begin
    Panel.ShowTabImages := Value <> nil;
    Panel.Invalidate;
  end;
end;

function TJvDockVIDVCTabPageControl.GetHotTrackColor: TColor;
begin
  Result := Panel.FHotTrackColor;
end;

procedure TJvDockVIDVCTabPageControl.SetHotTrackColor(const Value: TColor);
begin
  if Panel.FHotTrackColor <> Value then
  begin
    Panel.FHotTrackColor := Value;
    Panel.Invalidate;
  end;
end;

function TJvDockVIDVCTabPageControl.GetShowTabImages: Boolean;
begin
  Result := FPanel.FShowTabImages;
end;

procedure TJvDockVIDVCTabPageControl.SetShowTabImages(const Value: Boolean);
begin
  FPanel.ShowTabImages := Value;
end;

function TJvDockVIDVCTabPageControl.CustomUnDock(Source: TJvDockDragDockObject;
  NewTarget: TWinControl; Client: TControl): Boolean;
var
  CurrPage: TJvDockTabSheet;
  I: Integer;
begin
  if not ((Source.Control.HostDockSite <> nil) and
    (Source.DropOnControl = Source.Control.HostDockSite.Parent) and
    (Source.DropAlign = alClient)) then
  begin
    CurrPage := GetPageFromDockClient(Client);
    if CurrPage <> nil then
    begin
      if (FTabImageList <> nil) and ShowTabImages and
        (FTabImageList.Count > CurrPage.ImageIndex) then
      begin
        FTabImageList.Delete(CurrPage.ImageIndex);
        for I := 0 to Count - 1 do
          if Pages[I].ImageIndex > CurrPage.ImageIndex then
            Pages[I].ImageIndex := Pages[I].ImageIndex - 1;
      end;
    end;
    Result := inherited CustomUnDock(Source, NewTarget, Client);
  end
  else
    Result := True;
end;

function TJvDockVIDVCTabPageControl.GetPage(Index: Integer): TJvDockVIDVCTabSheet;
begin
  Result := TJvDockVIDVCTabSheet(inherited Pages[Index]);
end;

function TJvDockVIDVCTabPageControl.GetActiveVIDPage: TJvDockVIDVCTabSheet;
begin
  Result := TJvDockVIDVCTabSheet(inherited ActivePage);
end;

procedure TJvDockVIDVCTabPageControl.SetActiveVIDPage(const Value: TJvDockVIDVCTabSheet);
begin
  ActivePage := Value;
end;

//=== { TJvDockTabPanel } ====================================================

constructor TJvDockTabPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Page := nil;
  FCaptionTopOffset := 0;
  FCaptionLeftOffset := 5;
  FCaptionRightOffset := 5;
  FTabBottomOffset := 3;
  FTabSplitterWidth := 3;
  FTabHeight := 22;
  FSortList := TList.Create;
  FActiveFont := TFont.Create;
  FActiveFont.Color := clBlack;
  FInactiveFont := TFont.Create;
  FInactiveFont.Color := clWhite;
  FHotTrackColor := clBlue;
  FTempPages := TList.Create;
  FSelectHotIndex := -1;
  FShowTabImages := False;
  FSelectSheet := nil;
end;

destructor TJvDockTabPanel.Destroy;
begin
  FActiveFont.Free;
  FInactiveFont.Free;
  FSortList.Free;
  FTempPages.Free;
  inherited Destroy;
end;

procedure TJvDockTabPanel.DeleteSorts(Sheet: TJvDockVIDVCTabSheet);
var
  SheetIndex: Integer;
begin
  SheetIndex := FSortList.IndexOf(Sheet);
  if SheetIndex >= 0 then
    FSortList.Delete(SheetIndex);
  if Sheet <> nil then
    Sheet.TabVisible := False;
  SetShowTabWidth;
  Page.Invalidate;
end;

function TJvDockTabPanel.FindSheetWithPos(cX, cY, cTopOffset, cBottomOffset: Integer): Integer;
var
  I: Integer;
  CompleteWidth, CurrTabWidth: Integer;
  Pages: TList;
begin
  Result := -1;
  if (cY > cBottomOffset) or (cY < cTopOffset) then
    Exit;
  CompleteWidth := 0;
  if FSelectSheet = nil then
    Pages := Page.PageSheets
  else
    Pages := FTempPages;
  for I := 0 to Pages.Count - 1 do
  begin
    if not TJvDockVIDVCTabSheet(Pages[I]).TabVisible then
      Continue;
    CurrTabWidth := TJvDockVIDVCTabSheet(Pages[I]).ShowTabWidth;
    if (cX >= FTabLeftOffset + CompleteWidth) and (cX <= FTabLeftOffset + CurrTabWidth + CompleteWidth +
      FTabSplitterWidth) then
    begin
      Result := I;
      Exit;
    end;
    Inc(CompleteWidth, CurrTabWidth + FTabSplitterWidth);
  end;
end;

function TJvDockTabPanel.GetPageIndexFromMousePos(X, Y: Integer): Integer;
begin
  Result := -1;
  case Page.TabPosition of
    tpTop:
      Result := FindSheetWithPos(x, y, 0, Height - TabBottomOffset);
    tpBottom:
      Result := FindSheetWithPos(x, y, TabBottomOffset, Height);
    tpLeft:
      Result := FindSheetWithPos(y, x, 0, Height - TabBottomOffset);
    tpRight:
      Result := FindSheetWithPos(y, x, TabBottomOffset, Height);
  end;
end;

function TJvDockTabPanel.GetMaxTabWidth: TJvDockTabSheet;
var
  I: Integer;
  MaxWidth, CurrWidth: Integer;
begin
  Result := nil;
  MaxWidth := 0;
  if Page = nil then
    Exit;
  for I := 0 to Page.Count - 1 do
  begin
    CurrWidth := Canvas.TextWidth(Page.Tabs[I]);
    if MaxWidth < CurrWidth then
    begin
      Result := Page.Pages[I];
      MaxWidth := CurrWidth;
    end;
  end;
end;

function TJvDockTabPanel.GetMinTabWidth: TJvDockTabSheet;
var
  I: Integer;
  MinWidth, CurrWidth: Integer;
begin
  Result := nil;
  MinWidth := 0;
  for I := 0 to Page.Count - 1 do
  begin
    CurrWidth := Canvas.TextWidth(Page.Tabs[I]);
    if MinWidth > CurrWidth then
    begin
      Result := Page.Pages[I];
      MinWidth := CurrWidth;
    end;
  end;
end;

function TJvDockTabPanel.GetPanelHeight: Integer;
begin
  case Page.TabPosition of
    tpLeft, tpRight:
      Result := Width;
    tpTop, tpBottom:
      Result := Height;
  else
    Result := 0;
  end;
end;

function TJvDockTabPanel.GetPanelWidth: Integer;
begin
  case Page.TabPosition of
    tpLeft, tpRight:
      Result := Height;
    tpTop, tpBottom:
      Result := Width;
  else
    Result := 0;
  end;
end;

function TJvDockTabPanel.GetSorts(Index: Integer): TJvDockVIDVCTabSheet;
begin
  Result := FSortList[Index];
end;

function TJvDockTabPanel.GetTotalTabWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  if FSortList = nil then
    Exit;
  for I := 0 to FSortList.Count - 1 do
    Inc(Result, Sorts[I].TabWidth + Integer(I <> FSortList.Count - 1) * FTabSplitterWidth);
end;

procedure TJvDockTabPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Ctrl: TControl;
  Index: Integer;
  Msg: TWMMouse;
  Sheet: TJvDockVIDVCTabSheet;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Page = nil then
    Exit;

  Index := GetPageIndexFromMousePos(X, Y);
  if Index >= 0 then
  begin
    if Index <> Page.ActivePageIndex then
    begin
      Sheet := Page.ActiveVIDPage;
      Page.ActivePageIndex := Index;
      Sheet.SetSheetSort(Sheet.Caption);
      Page.ActiveVIDPage.SetSheetSort(Page.ActiveVIDPage.Caption);
      Page.Change;
      Invalidate;
    end;

    if Button = mbLeft then
    begin
      FSelectSheet := TJvDockVIDVCTabSheet(Page.ActivePage);
      {$IFDEF COMPILER6_UP}
      FTempPages.Assign(Page.PageSheets);
      {$ELSE}
      AssignList(Page.PageSheets, FTempPages);
      {$ENDIF COMPILER6_UP}
    end;

    Ctrl := GetDockClientFromPageIndex(Index);
    if Ctrl <> nil then
    begin
      JvGlobalDockClient := FindDockClient(Ctrl);
      if JvGlobalDockClient <> nil then
      begin
        Msg.Msg := WM_NCLBUTTONDOWN + Integer(Button) * 3 + Integer(ssDouble in Shift) * 2;
        Msg.Pos.x := X;
        Msg.Pos.y := Y;
        if not (ssDouble in Shift) then
          JvGlobalDockClient.DoNCButtonDown(Page.DoMouseEvent(Msg, Page), Button, msTabPage)
        else
        begin
          JvGlobalDockClient.DoNCButtonDblClk(Page.DoMouseEvent(Msg, Page), Button, msTabPage);
          if (Button = mbLeft) and JvGlobalDockClient.CanFloat then
            Ctrl.ManualDock(nil, nil, alNone);
        end;
      end;
    end;
  end;
end;

procedure TJvDockTabPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
  Ctrl: TControl;
  ARect: TRect;
begin
  inherited MouseMove(Shift, X, Y);
  Index := GetPageIndexFromMousePos(X, Y);
  if Page.HotTrack and (Index <> FSelectHotIndex) then
  begin
    FSelectHotIndex := Index;
    Invalidate;
  end;

  if Assigned(FSelectSheet) then
  begin
    Index := GetPageIndexFromMousePos(X, Y);
    if Index >= 0 then
    begin
      if (Index <> Page.ActivePageIndex) and (Page.Count > Index) then
      begin
        FSelectSheet.PageIndex := Index;
        Invalidate;
      end;
    end
    else
    begin
      case Page.TabPosition of
        tpTop:
          ARect := Rect(0, 0, Width, Height - FTabBottomOffset);
        tpBottom:
          ARect := Rect(0, FTabBottomOffset, Width, Height);
        tpLeft:
          ARect := Rect(0, 0, Width - FTabBottomOffset, Height);
        tpRight:
          ARect := Rect(FTabBottomOffset, 0, Width, Height);
      else
        ARect := Rect(0, 0, 0, 0);
      end;
      if PtInRect(ARect, Point(X, Y)) then
        Exit;
      if Page.FTempSheet = nil then
      begin
        Ctrl := GetDockClientFromPageIndex(FSelectSheet.PageIndex);
        if Ctrl <> nil then
          JvGlobalDockManager.BeginDrag(Ctrl, False, 1);
      end;
    end;
  end;
end;

procedure TJvDockTabPanel.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Ctrl: TControl;
  Index: Integer;
  Msg: TWMMouse;
begin
  inherited MouseUp(Button, Shift, X, Y);
  FSelectSheet := nil;
  if Page = nil then
    Exit;

  Index := GetPageIndexFromMousePos(X, Y);
  Ctrl := GetDockClientFromPageIndex(Index);
  if Ctrl <> nil then
  begin
    JvGlobalDockClient := FindDockClient(Ctrl);
    if JvGlobalDockClient <> nil then
    begin
      Msg.Msg := WM_NCLBUTTONUP + Integer(Button) * 3 + Integer(ssDouble in Shift) * 2;
      Msg.Pos := PointToSmallPoint(Page.ScreenToClient(ClientToScreen(Point(X, Y))));
      if not (ssDouble in Shift) then
        JvGlobalDockClient.DoNCButtonUp(Page.DoMouseEvent(Msg, Page), Button, msTabPage);
    end;
  end;
end;

procedure TJvDockTabPanel.Paint;
var
  ARect: TRect;
  CurrTabWidth: Integer;
  I, CompleteWidth: Integer;
  ImageWidth: Integer;
  CaptionString: string;
begin
  inherited Paint;
  if Page = nil then
    Exit;

  if (Page.Images <> nil) and (Page.ShowTabImages) then
    ImageWidth := Page.Images.Width
  else
    ImageWidth := 0;

  Canvas.Brush.Color := Page.ActiveSheetColor;
  case Page.TabPosition of
    tpLeft:
      Canvas.FillRect(Rect(PanelHeight - FTabBottomOffset, 0, PanelHeight, PanelWidth));
    tpRight:
      Canvas.FillRect(Rect(0, 0, FTabBottomOffset, PanelWidth));
    tpTop:
      Canvas.FillRect(Rect(0, PanelHeight - FTabBottomOffset, PanelWidth, PanelHeight));
    tpBottom:
      Canvas.FillRect(Rect(0, 0, PanelWidth, FTabBottomOffset));
  end;

  case Page.TabPosition of
    tpTop, tpLeft:
      Canvas.Pen.Color := clWhite;
    tpBottom, tpRight:
      Canvas.Pen.Color := clBlack;
  end;

  case Page.TabPosition of
    tpLeft:
      begin
        Canvas.MoveTo(PanelHeight - FTabBottomOffset, 0);
        Canvas.LineTo(PanelHeight - FTabBottomOffset, PanelWidth);
      end;
    tpRight:
      begin
        Canvas.MoveTo(FTabBottomOffset, 0);
        Canvas.LineTo(FTabBottomOffset, PanelWidth);
      end;
    tpTop:
      begin
        Canvas.MoveTo(0, PanelHeight - FTabBottomOffset);
        Canvas.LineTo(PanelWidth, PanelHeight - FTabBottomOffset);
      end;
    tpBottom:
      begin
        Canvas.MoveTo(0, FTabBottomOffset);
        Canvas.LineTo(PanelWidth, FTabBottomOffset);
      end;
  end;

  CompleteWidth := 0;

  Canvas.Brush.Style := bsClear;

  for I := 0 to Page.Count - 1 do
  begin
    if not Page.Pages[I].TabVisible then
      Continue;

    CurrTabWidth := TJvDockVIDVCTabSheet(Page.Pages[I]).ShowTabWidth;

    if Page.ActivePageIndex = I then
    begin
      Canvas.Brush.Color := Page.ActiveSheetColor;
      case Page.TabPosition of
        tpLeft:
          Canvas.FillRect(Rect(FTabTopOffset, CompleteWidth + FTabLeftOffset,
            PanelHeight, CompleteWidth + FTabLeftOffset + CurrTabWidth));
        tpRight:
          Canvas.FillRect(Rect(FTabBottomOffset, CompleteWidth + FTabLeftOffset,
            PanelHeight - FTabTopOffset, CompleteWidth + FTabLeftOffset + CurrTabWidth));
        tpTop:
          Canvas.FillRect(Rect(CompleteWidth + FTabLeftOffset, FTabTopOffset,
            CompleteWidth + FTabLeftOffset + CurrTabWidth, PanelHeight));
        tpBottom:
          Canvas.FillRect(Rect(CompleteWidth + FTabLeftOffset, FTabBottomOffset,
            CompleteWidth + FTabLeftOffset + CurrTabWidth, PanelHeight - FTabTopOffset));
      end;

      Canvas.Pen.Color := clWhite;
      case Page.TabPosition of
        tpLeft:
          begin
            Canvas.MoveTo(PanelHeight - FTabBottomOffset, CompleteWidth + FTabLeftOffset);
            Canvas.LineTo(FTabTopOffset, CompleteWidth + FTabLeftOffset);
            Canvas.LineTo(FTabTopOffset, CompleteWidth + FTabLeftOffset + CurrTabWidth);
            Canvas.Pen.Color := clBlack;
            Canvas.LineTo(PanelHeight - FTabBottomOffset, CompleteWidth + FTabLeftOffset + CurrTabWidth);
          end;
        tpRight:
          begin
            Canvas.MoveTo(FTabTopOffset, CompleteWidth + FTabLeftOffset);
            Canvas.LineTo(PanelHeight - FTabBottomOffset, CompleteWidth + FTabLeftOffset);
            Canvas.Pen.Color := clBlack;
            Canvas.LineTo(PanelHeight - FTabBottomOffset, CompleteWidth + FTabLeftOffset + CurrTabWidth);
            Canvas.LineTo(FTabTopOffset, CompleteWidth + FTabLeftOffset + CurrTabWidth);
          end;
        tpTop:
          begin
            Canvas.MoveTo(CompleteWidth + FTabLeftOffset, PanelHeight - FTabBottomOffset);
            Canvas.LineTo(CompleteWidth + FTabLeftOffset, FTabTopOffset);
            Canvas.LineTo(CompleteWidth + FTabLeftOffset + CurrTabWidth, FTabTopOffset);
            Canvas.Pen.Color := clBlack;
            Canvas.LineTo(CompleteWidth + FTabLeftOffset + CurrTabWidth, PanelHeight - FTabTopOffset);
          end;
        tpBottom:
          begin
            Canvas.MoveTo(CompleteWidth + FTabLeftOffset, FTabBottomOffset);
            Canvas.LineTo(CompleteWidth + FTabLeftOffset, PanelHeight - FTabTopOffset);
            Canvas.Pen.Color := clBlack;
            Canvas.LineTo(CompleteWidth + FTabLeftOffset + CurrTabWidth, PanelHeight - FTabTopOffset);
            Canvas.LineTo(CompleteWidth + FTabLeftOffset + CurrTabWidth, FTabBottomOffset);
          end;
      end;

      Canvas.Font.Assign(FActiveFont);
    end
    else
    begin
      if (I < Page.ActivePageIndex - 1) or (I > Page.ActivePageIndex) then
      begin
        Canvas.Pen.Color := Page.InactiveFont.Color;
        case Page.TabPosition of
          tpLeft, tpRight:
            begin
              Canvas.MoveTo(PanelHeight - FTabBottomOffset - 3, CompleteWidth + FTabLeftOffset + CurrTabWidth);
              Canvas.LineTo(FTabTopOffset + 2, CompleteWidth + FTabLeftOffset + CurrTabWidth);
            end;
          tpTop, tpBottom:
            begin
              Canvas.MoveTo(CompleteWidth + FTabLeftOffset + CurrTabWidth, PanelHeight - FTabBottomOffset - 3);
              Canvas.LineTo(CompleteWidth + FTabLeftOffset + CurrTabWidth, FTabTopOffset + 2);
            end;
        end;
      end;
      Canvas.Brush.Color := Page.InactiveSheetColor;
      Canvas.Font.Assign(FInactiveFont);
    end;

    if FSelectHotIndex = I then
      Canvas.Font.Color := FHotTrackColor;

    case Page.TabPosition of
      tpLeft:
        ARect := Rect(FTabTopOffset + FCaptionTopOffset + 1,
          CompleteWidth + FTabLeftOffset + FCaptionLeftOffset,
          PanelHeight,
          CompleteWidth + FTabLeftOffset + CurrTabWidth - FCaptionRightOffset);

      tpRight:
        ARect := Rect(FTabBottomOffset + FCaptionTopOffset + 1,
          CompleteWidth + FTabLeftOffset + FCaptionLeftOffset,
          PanelHeight,
          CompleteWidth + FTabLeftOffset + CurrTabWidth - FCaptionRightOffset);

      tpTop:
        ARect := Rect(CompleteWidth + FTabLeftOffset + FCaptionLeftOffset +
          Integer(FShowTabImages) * (ImageWidth + FCaptionLeftOffset),
          FTabTopOffset + FCaptionTopOffset + 1,
          CompleteWidth + FTabLeftOffset + CurrTabWidth - FCaptionRightOffset,
          PanelHeight);

      tpBottom:
        ARect := Rect(CompleteWidth + FTabLeftOffset + FCaptionLeftOffset +
          Integer(FShowTabImages) * (ImageWidth + FCaptionLeftOffset),
          FTabBottomOffset + FCaptionTopOffset + 1,
          CompleteWidth + FTabLeftOffset + CurrTabWidth - FCaptionRightOffset,
          PanelHeight);
    end;

    CaptionString := Page.Pages[I].Caption;
    DrawText(Canvas.Handle, PChar(CaptionString), Length(CaptionString),
      ARect, DT_LEFT or DT_SINGLELINE or DT_END_ELLIPSIS);

    if FShowTabImages and (Page.Images <> nil) and (CurrTabWidth > ImageWidth + 2 * FCaptionLeftOffset) then
      Page.Images.Draw(Canvas, CompleteWidth + FTabLeftOffset + FCaptionLeftOffset,
        FTabBottomOffset + FCaptionTopOffset + 1, Page.Pages[I].ImageIndex, True);

    Inc(CompleteWidth, CurrTabWidth + FTabSplitterWidth);
  end;

  Canvas.Brush.Color := Page.ActiveSheetColor;
  ARect := ClientRect;
  Canvas.FrameRect(ARect);
end;

procedure TJvDockTabPanel.Resize;
begin
  inherited Resize;
  SetShowTabWidth;
end;

procedure TJvDockTabPanel.SetCaptionLeftOffset(const Value: Integer);
begin
  if FCaptionLeftOffset <> Value then
  begin
    FCaptionLeftOffset := Value;
    Invalidate;
  end;
end;

procedure TJvDockTabPanel.SetCaptionRightOffset(const Value: Integer);
begin
  if FCaptionRightOffset <> Value then
  begin
    FCaptionRightOffset := Value;
    Invalidate;
  end;
end;

procedure TJvDockTabPanel.SetCaptionTopOffset(const Value: Integer);
begin
  if FCaptionTopOffset <> Value then
  begin
    FCaptionTopOffset := Value;
    Invalidate;
  end;
end;

procedure TJvDockTabPanel.SetPage(const Value: TJvDockVIDVCTabPageControl);
begin
  FPage := Value;
end;

procedure TJvDockTabPanel.SetPanelHeight(const Value: Integer);
begin
  if PanelHeight <> Value then
  begin
    case Page.TabPosition of
      tpLeft, tpRight:
        Width := Value;
      tpTop, tpBottom:
        Height := Value;
    end;
    SetShowTabWidth;
  end;
end;

procedure TJvDockTabPanel.SetTabBottomOffset(const Value: Integer);
begin
  if FTabBottomOffset <> Value then
  begin
    FTabBottomOffset := Value;
    Invalidate;
  end;
end;

procedure TJvDockTabPanel.SetTabLeftOffset(const Value: Integer);
begin
  if FTabLeftOffset <> Value then
  begin
    FTabLeftOffset := Value;
    Invalidate;
  end;
end;

procedure TJvDockTabPanel.SetTabRightOffset(const Value: Integer);
begin
  if FTabRightOffset <> Value then
  begin
    FTabRightOffset := Value;
    Invalidate;
  end;
end;

procedure TJvDockTabPanel.SetTabSplitterWidth(const Value: Integer);
begin
  if FTabSplitterWidth <> Value then
  begin
    FTabSplitterWidth := Value;
    Invalidate;
  end;
end;

procedure TJvDockTabPanel.SetTabTopOffset(const Value: Integer);
begin
  if FTabTopOffset <> Value then
  begin
    FTabTopOffset := Value;
    Invalidate;
  end;
end;

procedure TJvDockTabPanel.SetTotalTabWidth(const Value: Integer);
begin
end;

function TJvDockTabPanel.GetDockClientFromPageIndex(Index: Integer): TControl;
begin
  Result := nil;
  if Index >= 0 then
    if Page.Pages[Index].ControlCount = 1 then
    begin
      Result := Page.Pages[Index].Controls[0];
      if Result.HostDockSite <> Page then
        Result := nil;
    end;
end;

procedure TJvDockTabPanel.SetShowTabWidth;
var
  I, J, TempWidth: Integer;
  PanelWidth, VisibleCount: Integer;
  ImageWidth: Integer;
begin
  if Page = nil then
    Exit;
  if FSortList = nil then
    Exit;
  PanelWidth := 0;
  case Page.TabPosition of
    tpTop, tpBottom:
      PanelWidth := Width;
    tpLeft, tpRight:
      PanelWidth := Height;
  end;

  TempWidth := PanelWidth - FCaptionLeftOffset - FCaptionRightOffset;
  if Page.ShowTabImages then
    ImageWidth := Page.Images.Width + FCaptionLeftOffset
  else
    ImageWidth := 0;
  VisibleCount := Page.VisibleSheetCount;
  J := 0;
  for I := 0 to FSortList.Count - 1 do
  begin
    if not Sorts[I].TabVisible then
      Continue;
    if (VisibleCount - J) * (Sorts[I].TabWidth + FTabSplitterWidth + ImageWidth) > TempWidth then
      Sorts[I].FShowTabWidth := TempWidth div (VisibleCount - J) - FTabSplitterWidth
    else
      Sorts[I].FShowTabWidth := Sorts[I].TabWidth + ImageWidth;
    Dec(TempWidth, Sorts[I].FShowTabWidth + FTabSplitterWidth);
    Inc(J);
  end;
end;

procedure TJvDockTabPanel.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  if FSelectHotIndex <> -1 then
  begin
    FSelectHotIndex := -1;
    Invalidate;
  end;
end;

procedure TJvDockTabPanel.SetShowTabImages(const Value: Boolean);
begin
  if FShowTabImages <> Value then
  begin
    FShowTabImages := Value;
    SetShowTabWidth;
    Invalidate;
  end;
end;

procedure TJvDockTabPanel.SetTabHeight(const Value: Integer);
begin
  FTabHeight := Value;
  Height := FTabHeight + FTabTopOffset + FTabBottomOffset;
end;

//=== { TJvDockVIDVCTabSheet } ===============================================

constructor TJvDockVIDVCTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsSourceDockClient := False;
end;

destructor TJvDockVIDVCTabSheet.Destroy;
begin
  if (PageControl is TJvDockVIDVCTabPageControl) and (PageControl <> nil) then
    TJvDockVIDVCTabPageControl(PageControl).Panel.DeleteSorts(Self);
  inherited Destroy;
end;

procedure TJvDockVIDVCTabSheet.Loaded;
begin
  inherited Loaded;
  SetSheetSort(Caption);
end;

procedure TJvDockVIDVCTabSheet.SetPageControl(APageControl: TJvDockPageControl);
begin
  inherited SetPageControl(APageControl);
end;

procedure TJvDockVIDVCTabSheet.SetSheetSort(CaptionStr: string);
var
  TabPanel: TJvDockTabPanel;
  TempWidth: Integer;

  procedure DoSetSheetSort;
  var
    I: Integer;
  begin
    TJvDockVIDVCTabPageControl(PageControl).Panel.FSortList.Remove(Self);
    for I := 0 to TJvDockVIDVCTabPageControl(PageControl).Panel.FSortList.Count - 1 do
      if TJvDockVIDVCTabPageControl(PageControl).Panel.Sorts[I].TabWidth > TempWidth then
      begin
        TJvDockVIDVCTabPageControl(PageControl).Panel.FSortList.Insert(I, Self);
        Exit;
      end;
    TJvDockVIDVCTabPageControl(PageControl).Panel.FSortList.Add(Self);
  end;

begin
  if (PageControl is TJvDockVIDVCTabPageControl) and (PageControl <> nil) then
  begin
    TabPanel := TJvDockVIDVCTabPageControl(PageControl).Panel;
    if PageControl.ActivePage = Self then
      TabPanel.Canvas.Font.Assign(TabPanel.Page.ActiveFont)
    else
      TabPanel.Canvas.Font.Assign(TabPanel.Page.InactiveFont);
    TempWidth := TabPanel.Canvas.TextWidth(
      CaptionStr) + TabPanel.CaptionLeftOffset + TabPanel.CaptionRightOffset;
    if TempWidth <> FTabWidth then
    begin
      DoSetSheetSort;
      FTabWidth := TempWidth;
      TabPanel.SetShowTabWidth;
      TabPanel.Invalidate;
    end;
  end;
end;

procedure TJvDockVIDVCTabSheet.SetTabWidth(const Value: Integer);
begin
  FTabWidth := Value;
end;

procedure TJvDockVIDVCTabSheet.UpdateTabShowing;
begin
  inherited UpdateTabShowing;
  TJvDockVIDVCTabPageControl(PageControl).Panel.SetShowTabWidth;
end;

procedure TJvDockVIDVCTabSheet.WMSetText(var Msg: TMessage);
begin
  inherited;
  SetSheetSort(PChar(Msg.LParam));
end;

{$IFNDEF USEJVCL}
function TJvDockVIDVCStyle.GetControlName: string;
begin
  Result := Format(RsDockLikeVIDStyle, [inherited GetControlName]);
end;
{$ENDIF USEJVCL}

//=== { TJvDockVIDVCDragDockObject } =========================================

constructor TJvDockVIDVCDragDockObject.Create(AControl: TControl);

  procedure DoGetSourceDockClients(Control: TControl);
  var
    I: Integer;
    DockableControl: TWinControl;
  begin
    if Control is TJvDockableForm then
    begin
      DockableControl := TJvDockableForm(Control).DockableControl;
      for I := 0 to DockableControl.DockClientCount - 1 do
        DoGetSourceDockClients(DockableControl.DockClients[I]);
    end
    else
      FSourceDockClientList.Add(Control);
  end;

begin
  inherited Create(AControl);
  FSourceDockClientList := TList.Create;
  DoGetSourceDockClients(AControl);
  FDropTabControl := nil;
  FIsTabDockOver := False;
  CurrState := dsDragEnter;
  OldState := CurrState;
end;

destructor TJvDockVIDVCDragDockObject.Destroy;
begin
  FDropTabControl := nil;
  FSourceDockClientList.Free;
  inherited Destroy;
end;

procedure TJvDockVIDVCDragDockObject.GetBrush_PenSize_DrawRect(var ABrush: TBrush;
  var PenSize: Integer; var DrawRect: TRect; Erase: Boolean);
begin
  if DragTarget = nil then
    DropAlign := alNone;
  inherited GetBrush_PenSize_DrawRect(ABrush, PenSize, DrawRect, Erase);
  FIsTabDockOver := ((FOldDropAlign = alClient) and FErase) or
    ((DropAlign = alClient) and not FErase);
  FOldDropAlign := DropAlign;
  FOldTarget := DragTarget;
end;

// (rom) unused writeable const option removed

procedure TJvDockVIDVCDragDockObject.DefaultDockImage(Erase: Boolean);
const
  LeftOffset = 4;
var
  DesktopWindow: HWND;
  DC: HDC;
  OldBrush: HBrush;
  DrawRect: TRect;
  PenSize: Integer;
  ABrush: TBrush;
  ButtomOffset: Integer;
  MaxTabWidth: Integer;

  procedure DoDrawDefaultImage;
  begin
    with DrawRect do
    begin
      PatBlt(DC, Left + PenSize, Top, Right - Left - PenSize, PenSize, PATINVERT);
      PatBlt(DC, Right - PenSize, Top + PenSize, PenSize, Bottom - Top - PenSize, PATINVERT);
      PatBlt(DC, Left, Bottom - PenSize, Right - Left - PenSize, PenSize, PATINVERT);
      PatBlt(DC, Left, Top, PenSize, Bottom - Top - PenSize, PATINVERT);
    end;
  end;

  procedure DoDrawTabImage;
  begin
    with DrawRect do
    begin
      ButtomOffset := 15;
      MaxTabWidth := 30;

      PatBlt(DC, Left + PenSize, Top, Right - Left - PenSize, PenSize, PATINVERT);
      PatBlt(DC, Right - PenSize, Top + PenSize, PenSize, Bottom - Top - 2 * PenSize - ButtomOffset, PATINVERT);

      if DrawRect.Right - DrawRect.Left - 2 * PenSize < LeftOffset + 2 * PenSize + 2 * MaxTabWidth then
        MaxTabWidth := (DrawRect.Right - DrawRect.Left - 4 * PenSize - LeftOffset) div 2;

      if DrawRect.Bottom - DrawRect.Top - 2 * PenSize < 2 * ButtomOffset then
        ButtomOffset := Max((DrawRect.Bottom - DrawRect.Top - 2 * PenSize) div 2, 0);

      PatBlt(DC, Left, Bottom - PenSize - ButtomOffset, 2 * PenSize + LeftOffset, PenSize, PATINVERT);
      PatBlt(DC, Left + PenSize + LeftOffset, Bottom - ButtomOffset, PenSize, ButtomOffset, PATINVERT);
      PatBlt(DC, Left + 2 * PenSize + LeftOffset, Bottom - PenSize, MaxTabWidth, PenSize, PATINVERT);
      PatBlt(DC, Left + 2 * PenSize + LeftOffset + MaxTabWidth, Bottom - PenSize - ButtomOffset, PenSize, PenSize +
        ButtomOffset, PATINVERT);
      PatBlt(DC, Left + 3 * PenSize + LeftOffset + MaxTabWidth, Bottom - PenSize - ButtomOffset, Right - Left - 3 *
        PenSize - LeftOffset - MaxTabWidth, PenSize, PATINVERT);

      PatBlt(DC, Left, Top, PenSize, Bottom - Top - PenSize - ButtomOffset, PATINVERT);
    end;
  end;

begin
  FErase := Erase;
  GetBrush_PenSize_DrawRect(ABrush, PenSize, DrawRect, Erase);

  DesktopWindow := GetDesktopWindow;
  DC := GetDCEx(DesktopWindow, 0, DCX_CACHE or DCX_LOCKWINDOWUPDATE);
  try
    OldBrush := SelectObject(DC, ABrush.Handle);
    if not FIsTabDockOver then
      DoDrawDefaultImage
    else
      DoDrawTabImage;
    SelectObject(DC, OldBrush);
  finally
    ReleaseDC(DesktopWindow, DC);
  end;
end;

function TJvDockVIDVCDragDockObject.DragFindWindow(const Pos: TPoint): HWND;
begin
  Result := 0;
end;

function TJvDockVIDVCDragDockObject.GetDropCtl: TControl;
var
  ARect: TRect;
  I: Integer;
begin
  Result := inherited GetDropCtl;
  if (Result = nil) and (TargetControl is TJvDockCustomPanel) then
    for I := 0 to TargetControl.DockClientCount - 1 do
      if TargetControl.DockClients[I].Visible then
      begin
        ARect := TJvDockCustomPanel(DragTarget).JvDockManager.GetFrameRectEx(TargetControl.DockClients[I]);
        if PtInRect(ARect, DragPos) then
        begin
          Result := TargetControl.DockClients[I];
          Exit;
        end;
      end;
end;

function TJvDockVIDVCDragDockObject.GetSourceDockClient(Index: Integer): TControl;
begin
  Result := TControl(FSourceDockClientList[Index]);
end;

function TJvDockVIDVCDragDockObject.GetSourceDockClientCount: Integer;
begin
  Result := FSourceDockClientList.Count;
end;

procedure TJvDockVIDVCDragDockObject.MouseMsg(var Msg: TMessage);
var
  APos: TPoint;
  Page: TJvDockVIDVCTabPageControl;
begin
  inherited MouseMsg(Msg);
  case Msg.Msg of
    WM_CAPTURECHANGED:
      if JvGlobalDockClient.ParentForm.HostDockSite is TJvDockVIDVCTabPageControl then
        TJvDockVIDVCTabPageControl(JvGlobalDockClient.ParentForm.HostDockSite).Panel.MouseUp(mbLeft, [], 0, 0)
      else
      if TWinControl(JvGlobalDockManager.DragObject.DragTarget) is TJvDockVIDVCTabPageControl then
        TJvDockVIDVCTabPageControl(JvGlobalDockManager.DragObject.TargetControl).Panel.MouseUp(mbLeft, [], 0, 0);
    WM_MOUSEMOVE:
      if JvGlobalDockManager.DragObject.TargetControl is TJvDockVIDVCTabPageControl then
      begin
        Page := TJvDockVIDVCTabPageControl(JvGlobalDockManager.DragObject.TargetControl);
        if Page.FTempSheet <> nil then
        begin
          APos := Point(TWMMouse(Msg).XPos, TWMMouse(Msg).YPos);
          APos := Page.Panel.ScreenToClient(APos);
          Page.Panel.MouseMove([], APos.X, APos.Y);
        end;
      end;
  end;
end;

procedure TJvDockVIDVCDragDockObject.SetOldState(const Value: TDragState);
begin
  FOldState := Value;
end;

procedure TJvDockVIDVCDragDockObject.SetCurrState(const Value: TDragState);
begin
  FCurrState := Value;
end;

function TJvDockVIDVCDragDockObject.CanLeave(NewTarget: TWinControl): Boolean;
begin
  Result := inherited CanLeave(NewTarget);
end;

//=== { TJvDockVIDVCZone } ===================================================

function TJvDockVIDVCZone.GetSplitterLimit(IsMin: Boolean): Integer;
begin
  if IsMin then
    Result := ZoneLimit
  else
    Result := LimitBegin;
end;

procedure TJvDockVIDVCZone.Insert(DockSize: Integer; Hide: Boolean);
var
  PrevShift: Integer;
  NextShift: Integer;
  TempSize: Integer;
  BorderSize: Integer;
  BeforeVisibleZone: TJvDockZone;
  AfterVisibleZone: TJvDockZone;
  BeginSize: Integer;
begin
  if (ParentZone <> nil) and (ParentZone.VisibleChildCount = 0) then
    ParentZone.Insert(ParentZone.VisibleSize, Hide);

  if (ParentZone = nil) or ((ParentZone = Tree.TopZone) and (ParentZone.ChildCount <= 1)) then
  begin
    Visibled := True;
    Exit;
  end;

  if (ParentZone <> nil) and (ParentZone.ChildZones <> nil) then
    BeginSize := ParentZone.ChildZones.LimitBegin
  else
    BeginSize := 0;

  BeforeVisibleZone := BeforeClosestVisibleZone;
  AfterVisibleZone := AfterClosestVisibleZone;

  BorderSize := TJvDockVIDVCTree(Tree).BorderWidth * Integer(AfterClosestVisibleZone <> nil) div 2;
  TempSize := ParentZone.HeightWidth[ParentZone.Orientation] + BorderSize;
  Visibled := False;

  if DockSize >= TempSize - (ParentZone.VisibleChildCount) * TJvDockVIDVCTree(Tree).MinSize then
    DockSize := (TempSize - (ParentZone.VisibleChildCount) * TJvDockVIDVCTree(Tree).MinSize) div 2;

  if DockSize < TJvDockVIDVCTree(Tree).MinSize then
    DockSize := TempSize div 2;

  if (BeforeVisibleZone = nil) and (AfterVisibleZone = nil) then
  begin
    PrevShift := 0;
    NextShift := 0;
    ZoneLimit := TempSize + BeginSize;
  end
  else
  if BeforeVisibleZone = nil then
  begin
    PrevShift := 0;
    NextShift := DockSize + BorderSize;
    ZoneLimit := DockSize + LimitBegin + BorderSize;
    if ParentZone.VisibleChildCount = 1 then
      AfterVisibleZone.ZoneLimit := TempSize + BeginSize;
  end
  else
  if AfterVisibleZone = nil then
  begin
    PrevShift := DockSize + BorderSize;
    NextShift := 0;
    if (ParentZone.VisibleChildCount = 1) and (ParentZone = Tree.TopZone) then
      BeforeVisibleZone.ZoneLimit := Tree.TopXYLimit - PrevShift
    else
      BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.ZoneLimit - PrevShift;
    ZoneLimit := TempSize + BeginSize;
  end
  else
  begin
    PrevShift := Round((BeforeVisibleZone.ZoneLimit - BeginSize) * (DockSize + BorderSize) / TempSize);
    NextShift := DockSize - PrevShift;
    if (ParentZone.VisibleChildCount = 1) and (ParentZone = Tree.TopZone) then
      BeforeVisibleZone.ZoneLimit := Tree.TopXYLimit - PrevShift
    else
      BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.ZoneLimit - PrevShift;
    ZoneLimit := BeforeVisibleZone.ZoneLimit + DockSize;
  end;

  if PrevShift <> 0 then
  begin
    with TJvDockVIDVCTree(Tree) do
    begin
      ReplacementZone := BeforeVisibleZone;
      try
        if (BeforeVisibleZone.ZoneLimit - BeginSize) * (BeforeVisibleZone.ZoneLimit - BeginSize + PrevShift) <> 0 then
          ScaleBy := (BeforeVisibleZone.ZoneLimit - BeginSize) / (BeforeVisibleZone.ZoneLimit - BeginSize + PrevShift)
        else
          ScaleBy := 1;
        ParentLimit := BeginSize;
        ShiftScaleOrientation := ParentZone.Orientation;
        if ScaleBy <> 1 then
          ForEachAt(ParentZone.ChildZones, ScaleChildZone, tskMiddle, tspChild);
      finally
        ReplacementZone := nil;
      end;
    end;

    if BeforeVisibleZone.LimitSize < TJvDockVIDVCTree(Tree).MinSize then
      BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.LimitBegin + TJvDockVIDVCTree(Tree).MinSize;
  end;

  if NextShift <> 0 then
    with TJvDockVIDVCTree(Tree) do
    begin
      if (TempSize + BeginSize - LimitBegin - NextShift) * (TempSize + BeginSize - LimitBegin) <> 0 then
        ScaleBy := (TempSize + BeginSize - LimitBegin - NextShift) / (TempSize + BeginSize - LimitBegin)
      else
        ScaleBy := 1;
      ParentLimit := TempSize + BeginSize;
      ShiftScaleOrientation := ParentZone.Orientation;
      if ScaleBy <> 1 then
        ForEachAt(AfterVisibleZone, ScaleSiblingZone, tskForward);
    end;
  Visibled := True;
end;

procedure TJvDockVIDVCZone.Remove(DockSize: Integer; Hide: Boolean);
var
  PrevShift: Integer;
  NextShift: Integer;
  TempSize: Integer;
  BorderSize: Integer;
  BeforeVisibleZone: TJvDockZone;
  AfterVisibleZone: TJvDockZone;
  BeginSize: Integer;
begin
  if (ParentZone <> nil) and (ParentZone.VisibleChildCount = 1) and (ParentZone <> Tree.TopZone) then
    ParentZone.Remove(ParentZone.LimitSize, Hide);

  if (ParentZone = nil) or ((ParentZone = Tree.TopZone) and (ParentZone.ChildCount <= 1)) then
  begin
    Visibled := False;
    Exit;
  end;

  if (ParentZone <> nil) and (ParentZone.ChildZones <> nil) then
    BeginSize := ParentZone.ChildZones.LimitBegin
  else
    BeginSize := 0;

  BeforeVisibleZone := BeforeClosestVisibleZone;
  AfterVisibleZone := AfterClosestVisibleZone;

  BorderSize := TJvDockVIDVCTree(Tree).BorderWidth * Integer(AfterClosestVisibleZone <> nil) div 2;
  TempSize := ParentZone.HeightWidth[ParentZone.Orientation] + BorderSize;

  if DockSize > TempSize - (ParentZone.VisibleChildCount - 1) * TJvDockVIDVCTree(Tree).MinSize then
    DockSize := TempSize - (ParentZone.VisibleChildCount - 1) * TJvDockVIDVCTree(Tree).MinSize;
  if DockSize = 0 then
    DockSize := TempSize div 2;

  Visibled := False;
  if (BeforeVisibleZone = nil) and (AfterVisibleZone = nil) then
    Exit;

  if BeforeVisibleZone = nil then
  begin
    PrevShift := 0;
    NextShift := -DockSize + BorderSize;
    ZoneLimit := -DockSize + BorderSize + BeginSize;
  end
  else
  if AfterVisibleZone = nil then
  begin
    PrevShift := -DockSize + BorderSize;
    NextShift := 0;
    BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.ZoneLimit - PrevShift;
    ZoneLimit := TempSize + BeginSize;
  end
  else
  begin
    PrevShift := -Round((BeforeVisibleZone.ZoneLimit - BeginSize) * (DockSize + BorderSize) / (TempSize - (DockSize +
      BorderSize)));
    NextShift := -DockSize - PrevShift;
    BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.ZoneLimit - PrevShift;
    ZoneLimit := BeforeVisibleZone.ZoneLimit;
  end;

  if PrevShift <> 0 then
  begin
    with TJvDockVIDVCTree(Tree) do
    begin
      ReplacementZone := BeforeVisibleZone;
      try
        if (BeforeVisibleZone.ZoneLimit - BeginSize) * (BeforeVisibleZone.ZoneLimit - BeginSize + PrevShift) <> 0 then
          ScaleBy := (BeforeVisibleZone.ZoneLimit - BeginSize) / (BeforeVisibleZone.ZoneLimit - BeginSize + PrevShift)
        else
          ScaleBy := 1;
        ParentLimit := BeginSize;
        ShiftScaleOrientation := ParentZone.Orientation;
        if ScaleBy <> 1 then
          ForEachAt(ParentZone.ChildZones, ScaleChildZone, tskMiddle, tspChild);
      finally
        ReplacementZone := nil;
      end;
    end;

    if BeforeVisibleZone.LimitSize < TJvDockVIDVCTree(Tree).MinSize then
      BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.LimitBegin + TJvDockVIDVCTree(Tree).MinSize;
  end;

  if NextShift <> 0 then
    with TJvDockVIDVCTree(Tree) do
    begin
      if (TempSize + BeginSize - LimitBegin) * (TempSize + BeginSize - LimitBegin + NextShift) <> 0 then
        ScaleBy := (TempSize + BeginSize - LimitBegin) / (TempSize + BeginSize - LimitBegin + NextShift)
      else
        ScaleBy := 1;
      ParentLimit := TempSize + BeginSize;
      ShiftScaleOrientation := ParentZone.Orientation;
      if ScaleBy <> 1 then
        ForEachAt(AfterVisibleZone, ScaleSiblingZone, tskForward);
    end;
end;

//=== { TJvDockVIDVCTabServerOption } ========================================

constructor TJvDockVIDVCTabServerOption.Create(ADockStyle: TJvDockBasicStyle);
begin
  inherited Create(ADockStyle);
  TabPosition := tpBottom;
  FActiveFont := TFont.Create;
  FActiveSheetColor := clBtnFace;
  FHotTrackColor := clBlue;
  FInactiveFont := TFont.Create;
  FInactiveFont.Color := clWhite;
  FInactiveSheetColor := clBtnShadow;
  FShowTabImages := False;
end;

destructor TJvDockVIDVCTabServerOption.Destroy;
begin
  FActiveFont.Free;
  FInactiveFont.Free;
  inherited Destroy;
end;

procedure TJvDockVIDVCTabServerOption.Assign(Source: TPersistent);
begin
  if Source is TJvDockVIDVCTabServerOption then
  begin
    FActiveFont.Assign(TJvDockVIDVCTabServerOption(Source).FActiveFont);
    FActiveSheetColor := TJvDockVIDVCTabServerOption(Source).FActiveSheetColor;
    FHotTrackColor := TJvDockVIDVCTabServerOption(Source).FHotTrackColor;
    FInactiveFont.Assign(TJvDockVIDVCTabServerOption(Source).FInactiveFont);
    FInactiveSheetColor := TJvDockVIDVCTabServerOption(Source).FInactiveSheetColor;
    FShowTabImages := TJvDockVIDVCTabServerOption(Source).FShowTabImages;
  end;
  inherited Assign(Source);
end;

function TJvDockVIDVCTabServerOption.GetActiveFont: TFont;
begin
  Result := FActiveFont;
end;

function TJvDockVIDVCTabServerOption.GetActiveSheetColor: TColor;
begin
  Result := FActiveSheetColor;
end;

function TJvDockVIDVCTabServerOption.GetHotTrackColor: TColor;
begin
  Result := FHotTrackColor;
end;

function TJvDockVIDVCTabServerOption.GetInactiveFont: TFont;
begin
  Result := FInactiveFont;
end;

function TJvDockVIDVCTabServerOption.GetInactiveSheetColor: TColor;
begin
  Result := FInactiveSheetColor;
end;

function TJvDockVIDVCTabServerOption.GetShowTabImages: Boolean;
begin
  Result := FShowTabImages;
end;

procedure TJvDockVIDVCTabServerOption.ResetDockControlOption;
begin
  inherited ResetDockControlOption;
end;

procedure TJvDockVIDVCTabServerOption.ResetTabPageControl(APage: TJvDockTabPageControl);
begin
  inherited ResetTabPageControl(APage);
  if APage is TJvDockVIDVCTabPageControl then
  begin
    TJvDockVIDVCTabPageControl(APage).ActiveFont := ActiveFont;
    TJvDockVIDVCTabPageControl(APage).ActiveSheetColor := ActiveSheetColor;
    TJvDockVIDVCTabPageControl(APage).HotTrackColor := HotTrackColor;
    TJvDockVIDVCTabPageControl(APage).InactiveFont := InactiveFont;
    TJvDockVIDVCTabPageControl(APage).InactiveSheetColor := InactiveSheetColor;
    TJvDockVIDVCTabPageControl(APage).ShowTabImages := ShowTabImages;
    TJvDockVIDVCTabPageControl(APage).TabPosition := TabPosition;
  end;
end;

procedure TJvDockVIDVCTabServerOption.SetActiveFont(const Value: TFont);
begin
  FActiveFont.Assign(Value);
  ResetDockControlOption;
end;

procedure TJvDockVIDVCTabServerOption.SetActiveSheetColor(const Value: TColor);
begin
  if FActiveSheetColor <> Value then
  begin
    FActiveSheetColor := Value;
    ResetDockControlOption;
  end;
end;

procedure TJvDockVIDVCTabServerOption.SetHotTrackColor(const Value: TColor);
begin
  if FHotTrackColor <> Value then
  begin
    FHotTrackColor := Value;
    ResetDockControlOption;
  end;
end;

procedure TJvDockVIDVCTabServerOption.SetInactiveFont(const Value: TFont);
begin
  FInactiveFont.Assign(Value);
  ResetDockControlOption;
end;

procedure TJvDockVIDVCTabServerOption.SetInactiveSheetColor(const Value: TColor);
begin
  if FInactiveSheetColor <> Value then
  begin
    FInactiveSheetColor := Value;
    ResetDockControlOption;
  end;
end;

procedure TJvDockVIDVCTabServerOption.SetShowTabImages(const Value: Boolean);
begin
  if FShowTabImages <> Value then
  begin
    FShowTabImages := Value;
    ResetDockControlOption;
  end;
end;

procedure TJvDockVIDVCTabServerOption.SetTabPosition(const Value: TTabPosition);
begin
  if Value = tpBottom then
    inherited SetTabPosition(Value)
  else
    raise Exception.Create(RsEDockTabPositionMustBetpBottom);
end;

///=== { TJvDockVIDVCConjoinServerOption } ===================================

constructor TJvDockVIDVCConjoinServerOption.Create(ADockStyle: TJvDockBasicStyle);
begin
  inherited Create(ADockStyle);
  GrabbersSize := 18;
  FActiveFont := TFont.Create;
  FInactiveFont := TFont.Create;
  SystemInfo := True;
end;

destructor TJvDockVIDVCConjoinServerOption.Destroy;
begin
  FActiveFont.Free;
  FInactiveFont.Free;
  inherited Destroy;
end;

procedure TJvDockVIDVCConjoinServerOption.Assign(Source: TPersistent);
begin
  if Source is TJvDockVIDVCConjoinServerOption then
  begin
    FTextEllipsis := TJvDockVIDVCConjoinServerOption(Source).FTextEllipsis;
    FTextAlignment := TJvDockVIDVCConjoinServerOption(Source).FTextAlignment;
    FInactiveTitleEndColor := TJvDockVIDVCConjoinServerOption(Source).FInactiveTitleEndColor;
    FInactiveTitleStartColor := TJvDockVIDVCConjoinServerOption(Source).FInactiveTitleStartColor;
    FActiveTitleEndColor := TJvDockVIDVCConjoinServerOption(Source).FActiveTitleEndColor;
    FActiveTitleStartColor := TJvDockVIDVCConjoinServerOption(Source).FActiveTitleStartColor;
    FActiveFont.Assign(TJvDockVIDVCConjoinServerOption(Source).FActiveFont);
    FInactiveFont.Assign(TJvDockVIDVCConjoinServerOption(Source).FInactiveFont);
    FSystemInfo := TJvDockVIDVCConjoinServerOption(Source).FSystemInfo;
  end;
  inherited Assign(Source);
end;

procedure TJvDockVIDVCConjoinServerOption.SetActiveTitleEndColor(const Value: TColor);
begin
  if FActiveTitleEndColor <> Value then
  begin
    FActiveTitleEndColor := Value;
    FSystemInfo := False;
    ResetDockControlOption;
  end;
end;

procedure TJvDockVIDVCConjoinServerOption.SetActiveTitleStartColor(const Value: TColor);
begin
  if FActiveTitleStartColor <> Value then
  begin
    FActiveTitleStartColor := Value;
    FSystemInfo := False;
    ResetDockControlOption;
  end;
end;

procedure TJvDockVIDVCConjoinServerOption.SetInactiveTitleEndColor(const Value: TColor);
begin
  if FInactiveTitleEndColor <> Value then
  begin
    FInactiveTitleEndColor := Value;
    FSystemInfo := False;
    ResetDockControlOption;
  end;
end;

procedure TJvDockVIDVCConjoinServerOption.SetInactiveTitleStartColor(const Value: TColor);
begin
  if FInactiveTitleStartColor <> Value then
  begin
    FInactiveTitleStartColor := Value;
    FSystemInfo := False;
    ResetDockControlOption;
  end;
end;

procedure TJvDockVIDVCConjoinServerOption.SetSystemInfo(const Value: Boolean);
begin
  if FSystemInfo <> Value then
  begin
    FSystemInfo := Value;
    if FSystemInfo then
      SetDefaultSystemCaptionInfo;
    ResetDockControlOption;
  end;
end;

procedure TJvDockVIDVCConjoinServerOption.SetTextAlignment(
  const Value: TAlignment);
begin
  if FTextAlignment <> Value then
  begin
    FTextAlignment := Value;
    FSystemInfo := False;
    ResetDockControlOption;
  end;
end;

procedure TJvDockVIDVCConjoinServerOption.SetTextEllipsis(const Value: Boolean);
begin
  if FTextEllipsis <> Value then
  begin
    FTextEllipsis := Value;
    FSystemInfo := False;
    ResetDockControlOption;
  end;
end;

procedure TJvDockVIDVCConjoinServerOption.SetDefaultSystemCaptionInfo;
begin
  FActiveTitleStartColor := JvDockGetActiveTitleBeginColor;
  FActiveTitleEndColor := JvDockGetActiveTitleEndColor;
  FInactiveTitleStartColor := JvDockGetInactiveTitleBeginColor;
  FInactiveTitleEndColor := JvDockGetInactiveTitleEndColor;
  FTextAlignment := taLeftJustify;
  FTextEllipsis := True;
  FActiveFont.Assign(JvDockGetTitleFont);
  FActiveFont.Style := FActiveFont.Style + [fsBold];
  FInactiveFont.Assign(FActiveFont);
  FActiveFont.Color := JvDockGetActiveTitleFontColor;
  FInactiveFont.Color := JvDockGetInactiveTitleFontColor;
  GrabbersSize := VIDDefaultDockGrabbersSize;
  SplitterWidth := VIDDefaultDockSplitterWidth;
end;

procedure TJvDockVIDVCConjoinServerOption.SetActiveFont(const Value: TFont);
begin
  FActiveFont.Assign(Value);
  FSystemInfo := False;
  ResetDockControlOption;
end;

procedure TJvDockVIDVCConjoinServerOption.SetInactiveFont(const Value: TFont);
begin
  FInactiveFont.Assign(Value);
  FSystemInfo := False;
  ResetDockControlOption;
end;

procedure TJvDockVIDVCConjoinServerOption.ResetDockControlOption;
begin
  inherited ResetDockControlOption;
  FSystemInfo := FSystemInfo and (GrabbersSize = VIDDefaultDockGrabbersSize)
    and (SplitterWidth = VIDDefaultDockSplitterWidth);
  TJvDockVIDVCStyle(DockStyle).DoSystemInfoChange(FSystemInfo);
end;

procedure TJvDockVIDVCConjoinServerOption.SetActiveFont_WithoutChangeSystemInfo(
  const Value: TFont);
begin
  FActiveFont.Assign(Value);
end;

procedure TJvDockVIDVCConjoinServerOption.SetActiveTitleEndColor_WithoutChangeSystemInfo(
  const Value: TColor);
begin
  FActiveTitleEndColor := Value;
end;

procedure TJvDockVIDVCConjoinServerOption.SetActiveTitleStartColor_WithoutChangeSystemInfo(
  const Value: TColor);
begin
  FActiveTitleStartColor := Value;
end;

procedure TJvDockVIDVCConjoinServerOption.SetInactiveFont_WithoutChangeSystemInfo(
  const Value: TFont);
begin
  FInactiveFont.Assign(Value);
end;

procedure TJvDockVIDVCConjoinServerOption.SetInactiveTitleEndColor_WithoutChangeSystemInfo(
  const Value: TColor);
begin
  FInactiveTitleEndColor := Value;
end;

procedure TJvDockVIDVCConjoinServerOption.SetInactiveTitleStartColor_WithoutChangeSystemInfo(
  const Value: TColor);
begin
  FInactiveTitleStartColor := Value;
end;

procedure TJvDockVIDVCConjoinServerOption.SetTextAlignment_WithoutChangeSystemInfo(const Value: TAlignment);
begin
  FTextAlignment := Value;
end;

procedure TJvDockVIDVCTree.DrawDockSiteRect;
var
  Rect: TRect;
begin
  inherited DrawDockSiteRect;
  Rect := DockSite.ClientRect;
  InflateRect(Rect, BorderWidth, 0);
  if DockSite.Align = alTop then
    Inc(Rect.Bottom, BorderWidth)
  else
  if DockSite.Align = alBottom then
    Dec(Rect.Top, BorderWidth);
  Frame3D(Canvas, Rect, clBtnShadow, clBtnHighlight, 1);
  Frame3D(Canvas, Rect, clBtnHighlight, clBtnShadow, 1);

  Canvas.Pen.Color := clBlack;
  if DockSite.Align = alRight then
  begin
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(0, DockSite.Height);
  end
  else
  if DockSite.Align = alBottom then
  begin
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(DockSite.Width, 0);
  end;
end;

function TJvDockVIDVCTree.DoLButtonDown(var Msg: TWMMouse;
  var Zone: TJvDockZone; out HTFlag: Integer): Boolean;
var
  TempZone: TJvDockVIDVCZone;
  Active: Boolean;
begin
  Result := inherited DoLButtonDown(Msg, Zone, HTFlag);
  if (Zone <> nil) and (HTFlag = HTEXPAND) then
  begin
    TempZone := TJvDockVIDVCZone(Zone);
    Active := ((TempZone.ParentZone.Orientation <> DockSiteOrientation) and
      (TempZone.ParentZone.VisibleChildCount >= 2));
    if Active then
    begin
      TempZone.ExpandButtonDown := True;
      TempZone.MouseDown := True;
      FExpandBtnZone := TempZone;
      DockSite.Invalidate;
    end;
  end;
end;

procedure TJvDockVIDVCTree.DoMouseMove(var Msg: TWMMouse;
  var Zone: TJvDockZone; out HTFlag: Integer);
var
  TempZone: TJvDockVIDVCZone;
begin
  inherited DoMouseMove(Msg, Zone, HTFlag);
  if SizingZone = nil then
  begin
    TempZone := TJvDockVIDVCZone(Zone);
    if ((TempZone <> nil) and (TempZone.ExpandButtonDown <> (HTFlag = HTEXPAND)) and
      ((FExpandBtnZone = TempZone) and FExpandBtnZone.MouseDown)) then
    begin
      TempZone.ExpandButtonDown := (HTFlag = HTEXPAND) and FExpandBtnZone.MouseDown;
      DockSite.Invalidate;
    end;
  end;
end;

procedure TJvDockVIDVCTree.DoLButtonUp(var Msg: TWMMouse;
  var Zone: TJvDockZone; out HTFlag: Integer);
var
  TempZone: TJvDockVIDVCZone;
begin
  inherited DoLButtonUp(Msg, Zone, HTFlag);
  if (SizingZone = nil) and (FExpandBtnZone <> nil) then
  begin
    FExpandBtnZone := nil;
    if (Zone <> nil) and (HTFlag = HTEXPAND) then
    begin
      TempZone := TJvDockVIDVCZone(Zone);
      TempZone.ExpandButtonDown := False;
      if TempZone.ZoneSizeStyle in [zssMaximum] then
        TJvDockVIDVCZone(TempZone.ParentZone).DoSetChildSizeStyle(zssNormal)
      else
      begin
        TJvDockVIDVCZone(TempZone.ParentZone).DoSetChildSizeStyle(zssMinimum);
        TempZone.ZoneSizeStyle := zssMaximum;
      end;
      ResetDockZoneSizeStyle(TempZone.ParentZone, TempZone.ZoneSizeStyle, nil);
      DockSite.Invalidate;
    end;
  end;
end;

procedure TJvDockVIDVCConjoinServerOption.SetTextEllipsis_WithoutChangeSystemInfo(const Value: Boolean);
begin
  FTextEllipsis := Value;
end;

procedure TJvDockVIDVCZone.DoSetChildSizeStyle(ZoneSizeStyle: TJvDockZoneSizeStyle);
var
  Zone: TJvDockVIDVCZone;
begin
  Zone := TJvDockVIDVCZone(ChildZones);
  while Zone <> nil do
  begin
    Zone.ZoneSizeStyle := ZoneSizeStyle;
    Zone := TJvDockVIDVCZone(Zone.AfterClosestVisibleZone);
  end;
end;

procedure TJvDockVIDVCTree.ResetDockZoneSizeStyle(Parent: TJvDockZone;
  ZoneSizeStyle: TJvDockZoneSizeStyle; Exclude: TJvDockZone);
var
  Zone: TJvDockVIDVCZone;
  ChildCount: Integer;
  AverageSize: Integer;
begin
  ChildCount := Parent.VisibleChildCount - Integer((Exclude <> nil) and (Exclude.ParentZone = Parent));
  AverageSize := DockSiteSizeAlternate div ChildCount;
  Assert(AverageSize > 0);
  Zone := TJvDockVIDVCZone(Parent.FirstVisibleChildZone);
  while Zone <> nil do
  begin
    if Exclude <> Zone then
    begin
      Dec(ChildCount);
      if ZoneSizeStyle in [zssMaximum] then
      begin
        if Zone.ZoneSizeStyle = zssMinimum then
          Zone.ZoneLimit := Zone.LimitBegin + MinSize
        else
        if Zone.ZoneSizeStyle = zssMaximum then
          Zone.ZoneLimit := DockSiteSizeAlternate - ChildCount * MinSize;
      end
      else
      if ZoneSizeStyle in [zssNormal] then
        Zone.ZoneLimit := Zone.LimitBegin + AverageSize;
    end
    else
    if Exclude <> nil then
      Exclude.ZoneLimit := Exclude.LimitBegin;

    Zone := TJvDockVIDVCZone(Zone.AfterClosestVisibleZone);
  end;
  SetNewBounds(Parent);
  ForEachAt(Parent, UpdateZone, tskForward);
end;

end.

