{******************************************************************************}
{                                                                              }
{ Project JEDI Visible Component Library (J-VCL)                               }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Initial Developer of the Original Code is Marcel Bestebroer              }
{  <marcelb@zeelandnet.nl>.                                                    }
{ Portions created by Marcel Bestebroer are Copyright (C) 2000 - 2002 mbeSoft. }
{ All Rights Reserved.                                                         }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ Object Inspector like control which can inspect not only published           }
{ properties, but also variables, string lists (can be parsed as INI files)    }
{ anything you can think of (e.g. DataSet based or event based).               }
{                                                                              }
{ You may retrieve the latest version of this file at the Project JEDI home    }
{ page, located at http://www.delphi-jedi.org                                  }
{                                                                              }
{******************************************************************************}

unit JvInspector;

interface

uses
  SysUtils, Windows, Classes, Contnrs, TypInfo, Controls, StdCtrls, Graphics,
  Messages, IniFiles, JvComponent, JvTypes;

{$A+,B-,C+,E-,F-,G+,H+,I+,J+,K-,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Z1}
{$I JVCL.INC}

resourcestring
  sJvInspItemHasParent = 'Item already assigned to another parent.';
  sJvInspItemValueException = 'Exception ';
  sJvInspItemUnInitialized = '(uninitialized)';
  sJvInspItemUnassigned = '(unassigned)';
  sJvInspItemNoValue = '(no value)';
  sJvInspItemNotAChild = 'Specified Item is not a child of this item.';
  sJvInspItemColNotFound = 'Specified column does not belong to this compound item.';
  sJvInspItemItemIsNotCol = 'Specified item is not a column of this compound item.';
  sJvInspItemInvalidPropValue = 'Invalid property value %s.';
  sJvInspDataNoAccessAs = 'Data cannot be accessed as %s.';
  sJvInspDataNotInit = 'Data not initialized.';
  sJvInspDataNotAssigned = 'Data not assigned.';
  sJvInspDataNoValue = 'Data has no value.';
  sJvInspDataStrTooLong = 'String too long.';
  sJvInspRegNoCompare = 'Cannot compare %s to %s.';
  sJvInspNoGenReg = 'Unable to create generic item registration list.';
  sJvInspPaintNotActive = 'Painter is not the active painter of the specified inspector.';

const
  { Inspector Row Size constants }
  irsNoReSize =     $00000000;
  irsNameHeight =   $10000000;
  irsValueHeight =  $20000000;
  irsItemHeight =   $40000000;
  irsValueMask =    $0FFFFFFF;

type
  { Forward declarations }
  TJvCustomInspector = class;
  TJvInspectorPainter = class;
  TJvInspectorItemSizing = class;
  TJvCustomInspectorItem = class;
  TJvInspectorCompoundColumn = class;
  TJvInspectorCustomCompoundItem = class;
  TJvInspectorCustomCategoryItem = class;
  TJvCustomInspectorData = class;
  TJvInspectorRegister = class;
  TJvCustomInspectorRegItem = class;
  TJvInspectorEventData = class;

  { Types }
  TInspectorItemFlag = (iifReadonly, iifHidden, iifExpanded, iifVisible,
    iifQualifiedNames, iifAutoUpdate, iifMultiLine, iifValueList,
    iifAllowNonListValues, iifOwnerDrawListFixed, iifOwnerDrawListVariable,
    iifEditButton, iifEditFixed);
  TInspectorItemFlags = set of TInspectorItemFlag;
  TInspectorSetFlag = (isfEditString, isfCreateMemberItems);
  TInspectorSetFlags = set of TInspectorSetFlag;
  TInspectorClassFlag = (icfCreateMemberItems, icfShowClassName);
  TInspectorClassFlags = set of TInspectorClassFlag;
  TInspectorComponentFlag = (icfShowOwnerNames, icfNoShowFirstOwnerName, icfSortComponents,
    icfSortOwners, icfKeepFirstOwnerAsFirst);
  TInspectorComponentFlags = set of TInspectorComponentFlag;
  TInspectorCompoundItemFlag = (icifSingleName, icifSingleNameUseFirstCol);
  TInspectorCompoundItemFlags = set of TInspectorCompoundItemFlag;

  TInspectorPaintRect = (iprItem, iprButtonArea, iprBtnSrcRect, iprBtnDstRect,
    iprNameArea, iprName, iprValueArea, iprValue, iprEditValue, iprEditButton,
    iprUser1, iprUser2, iprUser3, iprUser4, iprUser5, iprUser6);

  TItemRowSizing = type Integer;

  TInspectorItemSortKind = (iskNone, iskName, iskManual, iskCustom);

  TJvInspectorItemClass = class of TJvCustomInspectorItem;
  TJvInspectorDataClass = class of TJvCustomInspectorData;
  TJvInspectorPainterClass = class of TJvInspectorPainter;

  TJvInspectorItemInstances = array of TJvCustomInspectorItem;
  TJvInspectorDataInstances = array of TJvCustomInspectorData;

  { Events }
  TInspectorItemEvent = procedure(Sender: TObject; const Item: TJvCustomInspectorItem) of object;
  TInspectorItemBeforeCreateEvent = procedure(Sender: TObject; const Data: TJvCustomInspectorData; var ItemClass: TJvInspectorItemClass) of object;
  TInspectorItemBeforeSelectEvent = procedure(Sender: TObject; const NewItem: TJvCustomInspectorItem; var Allow: Boolean) of object;
  TInspectorDataEvent = procedure(Sender: TObject; const Data: TJvCustomInspectorData) of object;
  TInspectorItemGetValueListEvent = procedure(const Item: TJvCustomInspectorItem; const Values: TStrings) of object;
  TInspectorItemSortCompare = function(const Item1, Item2: TJvCustomInspectorItem): Integer of object;
  TJvInspAsFloat = procedure(Sender: TJvInspectorEventData; var Value: Extended) of object;
  TJvInspAsInt64 = procedure(Sender: TJvInspectorEventData; var Value: Int64) of object;
  TJvInspAsMethod = procedure(Sender: TJvInspectorEventData; var Value: TMethod) of object;
  TJvInspAsString = procedure(Sender: TJvInspectorEventData; var Value: string) of object;
  TJvInspAsSet = procedure(Sender: TJvInspectorEventData; var Value; var BufSize: Integer) of object;
  TJvInspConfSectionEvent = procedure(var SectionName: string; var Parse: Boolean) of object;
  TJvInspConfKeyEvent = procedure(const SectionName: string; var ItemName: string; var ATypeInfo: PTypeInfo; var Allow: Boolean) of object;

  { Exceptions }
  EJvInspector = class(EJVCLException);
  EJvInspectorItem = class(EJvInspector);
  EJvInspectorData = class(EJvInspector);
  EJvInspectorReg = class(EJvInspector);

  { Classes }

  //----------------------------------------------------------------------------
  // Inspector
  //----------------------------------------------------------------------------
  TJvCustomInspector = class(TJvCustomControl)
  private
    FAfterDataCreate: TInspectorDataEvent;
    FAfterItemCreate: TInspectorItemEvent;
    FBandSizing: Boolean;
    FBandSizingBand: Integer;
    FBandStartsSB: TList;
    FBandStartsNoSB: TList;
    FBandWidth: Integer;
    FBeforeItemCreate: TInspectorItemBeforeCreateEvent;
    FBeforeSelection: TInspectorItemBeforeSelectEvent;
    FCollapseButton: TBitmap;
    FDivider: Integer;
    FDraggingDivider: Boolean;
    FExpandButton: TBitmap;
    FImageHeight: Integer;
    FItemHeight: Integer;
    FLockCount: Integer;
    FNeedRebuild: Boolean;
    FNeedRedraw: Boolean;
    FSortNotificationList: TList;
    FOnItemSelected: TNotifyEvent;
    FPainter: TJvInspectorPainter;
    FPaintGen: Integer;
    FReadonly: Boolean;
    FRelativeDivider: Boolean;
    FRoot: TJvCustomInspectorItem;
    FRowSizing: Boolean;
    FRowSizingItem: TJvCustomInspectorItem;
    FSelectedIndex: Integer;
    FSelecting: Boolean;
    FTopIndex: Integer;
    FUseBands: Boolean;
    FVisible: TStrings;
    FWantTabs: Boolean;
  protected
    function CalcImageHeight: Integer; virtual;
    function CalcItemIndex(X, Y: Integer; var Rect: TRect): Integer; virtual;
    function CalcItemRect(const Item: TJvCustomInspectorItem): TRect; virtual;
    procedure DoAfterDataCreate(const Data: TJvCustomInspectorData); virtual;
    procedure DoAfterItemCreate(const Item: TJvCustomInspectorItem); virtual;
    procedure DoBeforeItemCreate(const Data: TJvCustomInspectorData;
      var ItemClass: TJvInspectorItemClass); virtual;
    function DoBeforeItemSelect(const NewItem: TJvCustomInspectorItem): Boolean; virtual;
    procedure DoItemSelected; virtual;
    function GetAfterDataCreate: TInspectorDataEvent; virtual;
    function GetAfterItemCreate: TInspectorItemEvent; virtual;
    function GetBandFor(const ItemIdx: Integer): Integer; virtual;
    function GetBandStarts: TList; virtual;
    function GetBandWidth: Integer; virtual;
    function GetBeforeItemCreate: TInspectorItemBeforeCreateEvent; virtual;
    function GetBeforeSelection: TInspectorItemBeforeSelectEvent; virtual;
    function GetButtonRect(const ItemIndex: Integer): TRect; virtual;
    function GetCollapseButton: TBitmap; virtual;
    function GetDivider: Integer; virtual;
    function GetDividerAbs: Integer; virtual;
    function GetExpandButton: TBitmap; virtual;
    function GetImageHeight: Integer; virtual;
    function GetItemHeight: Integer; virtual;
    function GetLastFullVisible: Integer; virtual;
    function GetLockCount: Integer; virtual;
    function GetOnItemSelected: TNotifyEvent; virtual;
    function GetPainter: TJvInspectorPainter; virtual;
    function GetReadonly: Boolean; virtual;
    function GetRelativeDivider: Boolean; virtual;
    function GetRoot: TJvCustomInspectorItem; virtual;
    function GetSelected: TJvCustomInspectorItem; virtual;
    function GetSelectedIndex: Integer; virtual;
    function GetTopIndex: Integer; virtual;
    function GetUseBands: Boolean; virtual;
    function GetVisibleCount: Integer; virtual;
    function GetVisibleItems(const I: Integer): TJvCustomInspectorItem; virtual;
    function GetWantTabs: Boolean; virtual;
    procedure HandleBandResize(X: Integer); virtual;
    function IdxToY(const Index: Integer): Integer; virtual;
    procedure IncPaintGeneration; virtual;
    procedure InvalidateHeight; virtual;
    procedure InvalidateItem; virtual;
    procedure InvalidateList; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure NotifySort(const Item: TJvCustomInspectorItem); virtual;
    procedure Paint; override;
    procedure RebuildVisible; virtual;
    procedure RemoveNotifySort(const Item: TJvCustomInspectorItem); virtual;
    procedure Resize; override;
    function ScrollfactorV: Extended; virtual;
    procedure SetAfterDataCreate(const Value: TInspectorDataEvent); virtual;
    procedure SetAfterItemCreate(const Value: TInspectorItemEvent); virtual;
    procedure SetBandWidth(Value: Integer); virtual;
    procedure SetBeforeItemCreate(const Value: TInspectorItemBeforeCreateEvent); virtual;
    procedure SetBeforeSelection(const Value: TInspectorItemBeforeSelectEvent); virtual;
    procedure SetCollapseButton(const Value: TBitmap); virtual;
    procedure SetDivider(Value: Integer); virtual;
    procedure SetDividerAbs(Value: Integer); virtual;
    procedure SetExpandButton(const Value: TBitmap); virtual;
    procedure SetItemHeight(Value: Integer); virtual;
    procedure SetLockCount(const Value: Integer); virtual;
    procedure SetOnItemSelected(const Value: TNotifyEvent); virtual;
    procedure SetPainter(const Value: TJvInspectorPainter); virtual;
    procedure SetReadonly(const Value: Boolean); virtual;
    procedure SetRelativeDivider(Value: Boolean); virtual;
    procedure SetSelected(const Value: TJvCustomInspectorItem); virtual;
    procedure SetSelectedIndex(Value: Integer); virtual;
    procedure SetTopIndex(Value: Integer); virtual;
    procedure SetUseBands(Value: Boolean); virtual;
    procedure SetWantTabs(Value: Boolean); virtual;
    procedure UpdateScrollBars; virtual;
    function ViewHeight: Integer;
    function ViewRect: TRect; virtual;
    function ViewWidth: Integer;
		procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMHScroll(var Message: TWMScroll); message WM_HSCROLL;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMVScroll(var Message: TWMScroll); message WM_VSCROLL;
    function YToIdx(const Y: Integer): Integer; virtual;

    property BandSizing: Boolean read FBandSizing write FBandSizing;
    property BandSizingBand: Integer read FBandSizingBand write FBandSizingBand;
    property BandStarts: TList read GetBandStarts;
    property BandWidth: Integer read GetBandWidth write SetBandWidth;
    property CollapseButton: TBitmap read GetCollapseButton write SetCollapseButton;
    property ExpandButton: TBitmap read GetExpandButton write SetExpandButton;
    property Divider: Integer read GetDivider write SetDivider;
    property DividerAbs: Integer read GetDividerAbs write SetDividerAbs;
    property DraggingDivider: Boolean read FDraggingDivider write FDraggingDivider;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property ImageHeight: Integer read GetImageHeight;
    property LockCount: Integer read GetLockCount;
    property NeedRebuild: Boolean read FNeedRebuild write FNeedRebuild;
    property NeedRedraw: Boolean read FNeedRedraw write FNeedRedraw;
    property SortNotificationList: TList read FSortNotificationList;
    property OnItemSelected: TNotifyEvent read GetOnItemSelected write SetOnItemSelected;
    property AfterDataCreate: TInspectorDataEvent read GetAfterDataCreate
      write SetAfterDataCreate;
    property AfterItemCreate: TInspectorItemEvent read GetAfterItemCreate
      write SetAfterItemCreate;
    property BeforeItemCreate: TInspectorItemBeforeCreateEvent
      read GetBeforeItemCreate write SetBeforeItemCreate;
    property BeforeSelection: TInspectorItemBeforeSelectEvent read GetBeforeSelection write SetBeforeSelection;
    property Painter: TJvInspectorPainter read GetPainter write SetPainter;
    property PaintGeneration: Integer read FPaintGen;
    property Readonly: Boolean read GetReadonly write SetReadonly;
    property RelativeDivider: Boolean read GetRelativeDivider write SetRelativeDivider;
    property Root: TJvCustomInspectorItem read GetRoot;
    property RowSizing: Boolean read FRowSizing write FRowSizing;
    property RowSizingItem: TJvCustomInspectorItem read FRowSizingItem write
      FRowSizingItem;
    property Selected: TJvCustomInspectorItem read GetSelected;
    property SelectedIndex: Integer read GetSelectedIndex write SetSelectedIndex;
    property Selecting: Boolean read FSelecting write FSelecting;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;
    property UseBands: Boolean read GetUseBands write SetUseBands;
    property VisibleCount: Integer read GetVisibleCount;
    property VisibleItems[const I: Integer]: TJvCustomInspectorItem read GetVisibleItems;
    property WantTabs: Boolean read GetWantTabs write SetWantTabs;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BeginUpdate: Integer; virtual;
    function EndUpdate: Integer; virtual;
    function Focused: Boolean; override;
    function FocusedItem: TJvCustomInspectorItem; virtual;
    function VisibleIndex(const AItem: TJvCustomInspectorItem): Integer; virtual;
  end;

  TJvInspector = class(TJvCustomInspector)
  public
    property LockCount;
    property Root;
    property Selected;
    property SelectedIndex;
    property TopIndex;
    property VisibleCount;
    property VisibleItems;
  published
    property Align;
    property Anchors;
    property BandWidth;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property CollapseButton;
    property RelativeDivider; // Must be defined before Divider
    property Divider;
    property ExpandButton;
    property Font;
    property ItemHeight;
    property Painter;
    property Readonly;
    property UseBands;
    property WantTabs;
    property AfterDataCreate;
    property AfterItemCreate;
    property BeforeItemCreate;
    property BeforeSelection;
    property OnItemSelected;
  end;

  //----------------------------------------------------------------------------
  // Inspector painter classes
  //----------------------------------------------------------------------------
  TJvInspectorPainter = class(TJvComponent)
  private
    FBackgroundColor: TColor;
    FButtonImage: TBitmap;
    FCanvas: TCanvas;
    FCategoryColor: TColor;
    FCategoryTextColor: TColor;
    FDividerColor: TColor;
    FInitializing: Boolean;
    FInspector: TJvCustomInspector;
    FInternalCollapseButton: TBitmap;
    FInternalExpandButton: TBitmap;
    FItem: TJvCustomInspectorItem;
    FItemIndex: Integer;
    FNameColor: TColor;
    FPaintRect: TRect;
    FSelectedColor: TColor;
    FSelectedTextColor: TColor;
    FValueColor: TColor;
  protected
    procedure ApplyNameFont; virtual;
    procedure ApplyValueFont; virtual;
    procedure CalcButtonBasedRects; virtual;
    procedure CalcEditBasedRects; virtual;
    procedure CalcNameBasedRects; virtual;
    procedure CalcValueBasedRects; virtual;
    function DividerWidth: Integer; virtual;
    procedure DoPaint; virtual;
    function GetBackgroundColor: TColor; virtual;
    function GetCategoryColor: TColor; virtual;
    function GetCategoryTextColor: TColor; virtual;
    function GetCollapseImage: TBitmap; virtual;
    function GetDividerColor: TColor; virtual;
    function GetExpandImage: TBitmap; virtual;
    function GetNameColor: TColor; virtual;
    function GetNameHeight(const AItem: TJvCustomInspectorItem): Integer; virtual;
    function GetRects(const Index: TInspectorPaintRect): TRect; virtual;
    function GetSelectedColor: TColor; virtual;
    function GetSelectedTextColor: TColor; virtual;
    function GetValueColor: TColor; virtual;
    function GetValueHeight(const AItem: TJvCustomInspectorItem): Integer; virtual;
    procedure HideEditor; virtual;
    procedure InitializeColors; virtual;
    function Loading: Boolean;
    procedure Paint; virtual;
    procedure PaintDivider(const X, YTop, YBottom: Integer); virtual;
    procedure PaintItem(var ARect: TRect; const AItemIndex: Integer); overload; virtual;
    procedure PaintItem(const AItem: TJvCustomInspectorItem); overload; virtual;
    procedure SetBackgroundColor(const Value: TColor); virtual;
    procedure SetCategoryColor(const Value: TColor); virtual;
    procedure SetCategoryTextColor(const Value: TColor); virtual;
    procedure SetDividerColor(const Value: TColor); virtual;
    procedure SetNameColor(const Value: TColor); virtual;
    procedure SetRects(const Index: TInspectorPaintRect; const ARect: TRect); virtual;
    procedure SetSelectedColor(const Value: TColor); virtual;
    procedure SetSelectedTextColor(const Value: TColor); virtual;
    procedure Setup(const ACanvas: TCanvas); virtual;
    procedure SetupItem; virtual;
    procedure SetupRects; virtual;
    procedure SetValueColor(const Value: TColor); virtual;
    procedure TeardownItem; virtual;

    property ButtonImage: TBitmap read FButtonImage write FButtonImage;
    property Canvas: TCanvas read FCanvas write FCanvas;
    property Initializing: Boolean read FInitializing write FInitializing;
    property Inspector: TJvCustomInspector read FInspector;
    property InternalCollapseButton: TBitmap read FInternalCollapseButton;
    property InternalExpandButton: TBitmap read FInternalExpandButton;
    property Item: TJvCustomInspectorItem read FItem write FItem;
    property ItemIndex: Integer read FItemIndex write FItemIndex;
    property PaintRect: TRect read FPaintRect write FPaintRect;
    property Rects[const Index: TInspectorPaintRect]: TRect read GetRects
      write SetRects;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetInspector(const AInspector: TJvCustomInspector); virtual;

    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor;
    property SelectedTextColor: TColor read GetSelectedTextColor write SetSelectedTextColor;
  published
    property BackgroundColor: TColor read GetBackgroundColor write SetBackgroundColor;
    property CategoryColor: TColor read GetCategoryColor write SetCategoryColor;
    property CategoryTextColor: TColor read GetCategoryTextColor write SetCategoryTextColor;
    property DividerColor: TColor read GetDividerColor write SetDividerColor;
    property NameColor: TColor read GetNameColor write SetNameColor;
    property ValueColor: TColor read GetValueColor write SetValueColor;
  end;

  TJvInspectorBorlandNETBasePainter = class(TJvInspectorPainter)
  protected
    procedure ApplyNameFont; override;
    procedure ApplyValueFont; override;
    procedure CalcButtonBasedRects; override;
    procedure CalcEditBasedRects; override;
    procedure CalcNameBasedRects; override;
    procedure CalcValueBasedRects; override;
    procedure SetupRects; override;
  published
    property BackgroundColor default clWindow;
    property CategoryColor default clBtnFace;
    property CategoryTextColor default clBtnText;
    property NameColor default clWindowText;
    property ValueColor default clWindowText;
  end;

  TJvInspectorBorlandPainter = class(TJvInspectorBorlandNETBasePainter)
  private
    FDividerLightColor: TColor;
  protected
    function DividerWidth: Integer; override;
    procedure DoPaint; override;
    function GetDividerLightColor: TColor; virtual;
    function GetSelectedColor: TColor; override;
    function GetSelectedTextColor: TColor; override;
    procedure InitializeColors; override;
    procedure PaintDivider(const X, YTop, YBottom: Integer); override;
    procedure SetDividerLightColor(const Value: TColor); virtual;
    procedure Setup(const ACanvas: TCanvas); override;
  published
    property BackgroundColor default clBtnFace;
    property DividerColor default clBtnShadow;
    property DividerLightColor: TColor read GetDividerLightColor write SetDividerLightColor default clBtnHighlight;
    property ValueColor default clNavy;
  end;

  TJvInspectorDotNETPainter = class(TJvInspectorBorlandNETBasePainter)
  private
  protected
    procedure ApplyNameFont; override;
    procedure DoPaint; override;
    procedure PaintDivider(const X, YTop, YBottom: Integer); override;
  published
    property DividerColor default clBtnFace;
    property SelectedColor default clHighlight;
    property SelectedTextColor default clHighlightText;
  end;

  //----------------------------------------------------------------------------
  // Inspector item sizing object
  //----------------------------------------------------------------------------
  TJvInspectorItemSizing = class(TPersistent)
  private
    FMinHeight: TItemRowSizing;
    FSizable: Boolean;
    FSizingFactor: TItemRowSizing;
  protected
    Item: TJvCustomInspectorItem;
    function GetMinHeight: TItemRowSizing;
    function GetSizable: Boolean;
    function GetSizingFactor: TItemRowSizing;
    procedure SetMinHeight(Value: TItemRowSizing);
    procedure SetSizable(Value: Boolean);
    procedure SetSizingFactor(Value: TItemRowSizing);
  public
    constructor Create(const AItem: TJvCustomInspectorItem);
    procedure Assign(Source: TPersistent); override;
    property MinHeight: TItemRowSizing read GetMinHeight write SetMinHeight;
    property Sizable: Boolean read GetSizable write SetSizable;
    property SizingFactor: TItemRowSizing read GetSizingFactor write SetSizingFactor;
  published
  end;

  //----------------------------------------------------------------------------
  // Inspector item classes
  //----------------------------------------------------------------------------
  TJvCustomInspectorItem = class(TPersistent)
  private
    FData: TJvCustomInspectorData;
    FDisplayIndex: Integer;
    FDisplayName: string;
    FDroppedDown: Boolean;
    FEditCtrl: TCustomEdit;
    FEditWndPrc: TWndMethod;
    FEditing: Boolean;
    FFlags: TInspectorItemFlags;
    FHeight: Integer;
    FInspector: TJvCustomInspector;
    FItems: TObjectList;
    FListBox: TCustomListBox;
    FOnCompare: TInspectorItemSortCompare;
    FOnGetValueList: TInspectorItemGetValueListEvent;
    FParent: TJvCustomInspectorItem;
    FLastPaintGen: Integer;
    FPressed: Boolean;
    FRects: array[TInspectorPaintRect] of TRect;
    FRowSizing: TJvInspectorItemSizing;
    FSortKind: TInspectorItemSortKind;
    FTracking: Boolean;
  protected
    procedure AlphaSort;
    procedure Apply; virtual;
    procedure ApplyDisplayIndices(const ItemList: TList); virtual;
    procedure BuildDisplayableList(const ItemList: TList); virtual;
    procedure ButtonClick(Sender: TObject); virtual;
    function CanEdit: Boolean; virtual;
    procedure CloseUp(Accept: Boolean); virtual;
    procedure DataSort;
    procedure DoAfterItemCreate; virtual;
    function DoCompare(const Item: TJvCustomInspectorItem): Integer; virtual;
    procedure DoDrawListItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState); virtual;
    procedure DoDropDownKeys(var Key: Word; Shift: TShiftState); virtual;
    procedure DoGetValueList(const Strings: TStrings); virtual;
    procedure DoMeasureListItem(Control: TWinControl; Index: Integer;
      var Height: Integer); virtual;
    procedure DoMeasureListItemWidth(Control: TWinControl; Index: Integer;
      var Width: Integer); virtual;
    procedure DropDown; dynamic;
    procedure Edit; virtual;
    procedure EditChange(Sender: TObject); virtual;
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      virtual;
    procedure EditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure EditMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      virtual;
    procedure EditMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure Edit_WndProc(var Message: TMessage); virtual;
    function GetAutoUpdate: Boolean; virtual;
    function GetBaseCategory: TJvInspectorCustomCategoryItem; virtual;
    function GetCategory: TJvInspectorCustomCategoryItem; virtual;
    function GetCount: Integer; virtual;
    function GetData: TJvCustomInspectorData; virtual;
    function GetDisplayIndex: Integer; virtual;
    function GetDisplayName: string; virtual;
    function GetDisplayParent: TJvCustomInspectorItem; virtual;
    function GetDisplayValue: string; virtual;
    function GetDroppedDown: Boolean; virtual;
    function GetEditCtrl: TCustomEdit; virtual;
    function GetEditing: Boolean; virtual;
    function GetExpanded: Boolean; virtual;
    function GetFlags: TInspectorItemFlags; virtual;
    function GetHeight: Integer; virtual;
    function GetHeightFactor: Integer; virtual;
    function GetHidden: Boolean; virtual;
    function GetInspector: TJvCustomInspector; virtual;
    function GetInspectorPaintGeneration: Integer;
    function GetIsCompoundColumn: Boolean; virtual;
    function GetItems(const I: Integer): TJvCustomInspectorItem; virtual;
    function GetLevel: Integer; virtual;
    function GetListBox: TCustomListBox; virtual;
    function GetMultiline: Boolean; virtual;
    function GetNextSibling: TJvCustomInspectorItem; virtual;
    function GetParent: TJvCustomInspectorItem; virtual;
    function GetQualifiedNames: Boolean; virtual;
    function GetReadonly: Boolean; virtual;
    function GetRects(const RectKind: TInspectorPaintRect): TRect; virtual;
    function GetRowSizing: TJvInspectorItemSizing; virtual;
    function GetSortKind: TInspectorItemSortKind; virtual;
    function GetSortName: string; virtual;
    procedure GetValueList(const Strings: TStrings); virtual;
    function GetVisible: Boolean; virtual;
    procedure InvalidateItem; virtual;
    procedure InvalidateList; virtual;
    procedure InvalidateSort; virtual;
    procedure InvalidateMetaData; virtual;
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure NaturalSort; 
    procedure SelectValue(const Delta: Integer); virtual;
    procedure SetAutoUpdate(const Value: Boolean); virtual;
    procedure SetDisplayIndex(const Value: Integer); virtual;
    procedure SetDisplayIndexValue(const Value: Integer); virtual;
    procedure SetDisplayName(Value: string); virtual;
    procedure SetDisplayValue(const Value: string); virtual;
    procedure SetEditCtrl(const Value: TCustomEdit); virtual;
    procedure SetEditing(const Value: Boolean); virtual;
    procedure SetExpanded(Value: Boolean); virtual;
    procedure SetFlags(const Value: TInspectorItemFlags); virtual;
    procedure SetFocus; virtual;
    procedure SetHeight(Value: Integer); virtual;
    procedure SetHeightFactor(Value: Integer); virtual;
    procedure SetHidden(Value: Boolean); virtual;
    procedure SetInspector(const AInspector: TJvCustomInspector); virtual;
    procedure SetMultiline(const Value: Boolean); virtual;
    procedure SetOnCompare(const Value: TInspectorItemSortCompare); virtual;
    procedure SetParent(const Value: TJvCustomInspectorItem); virtual;
    procedure SetQualifiedNames(const Value: Boolean); virtual;
    procedure SetReadonly(const Value: Boolean); virtual;
    procedure SetRects(const RectKind: TInspectorPaintRect; Value: TRect); virtual;
    procedure SetRowSizing(Value: TJvInspectorItemSizing); virtual;
    procedure SetSortKind(Value: TInspectorItemSortKind); virtual;
    procedure SetVisible(Value: Boolean); virtual;
    procedure StopTracking; dynamic;
    procedure TrackButton(X,Y: Integer); dynamic;
    procedure Undo; virtual;
    procedure UpdateDisplayOrder(const Item: TJvCustomInspectorItem; const NewIndex: Integer); virtual;
    procedure UpdateLastPaintGeneration;

    property BaseCategory: TJvInspectorCustomCategoryItem read GetBaseCategory;
    property Category: TJvInspectorCustomCategoryItem read GetCategory;
    property DroppedDown: Boolean read GetDroppedDown;
    property EditCtrl: TCustomEdit read GetEditCtrl;
    property EditWndPrc: TWndMethod read FEditWndPrc;
    property IsCompoundColumn: Boolean read GetIsCompoundColumn;
    property LastPaintGeneration: Integer read FLastPaintGen;
    property ListBox: TCustomListBox read GetListBox;
    property OnGetValueList: TInspectorItemGetValueListEvent read FOnGetValueList write FOnGetValueList;

    property Pressed: Boolean read FPressed write FPressed;
    property Tracking: Boolean read FTracking write FTracking;
  public
    constructor Create(const AParent: TJvCustomInspectorItem; const AData: TJvCustomInspectorData); virtual;
    destructor Destroy; override;
    function Add(const Item: TJvCustomInspectorItem): Integer; 
    procedure AfterConstruction; override;
    procedure Clear;
    procedure Delete(const Index: Integer); overload; virtual;
    procedure Delete(const Item: TJvCustomInspectorItem); overload; virtual;
    procedure Delete(const Data: TJvCustomInspectorData); overload; virtual;
    procedure DrawEditor(const ACanvas: TCanvas); virtual;
    procedure DrawName(const ACanvas: TCanvas); virtual;
    procedure DrawValue(const ACanvas: TCanvas); virtual;
    function EditFocused: Boolean; dynamic;
    function HasViewableItems: Boolean; virtual;
    function IndexOf(const Item: TJvCustomInspectorItem): Integer; overload; virtual;
    function IndexOf(const Data: TJvCustomInspectorData): Integer; overload; virtual;
    procedure InitEdit; dynamic;
    procedure DoneEdit(const CancelEdits: Boolean = False); dynamic;
    procedure Insert(const Index: Integer; const Item: TJvCustomInspectorItem); 
    procedure ScrollInView;
    procedure Sort;

    property AutoUpdate: Boolean read GetAutoUpdate write SetAutoUpdate;
    property Count: Integer read GetCount;
    property Data: TJvCustomInspectorData read GetData;
    property DisplayIndex: Integer read GetDisplayIndex write SetDisplayIndex;
    property DisplayName: string read GetDisplayName write SetDisplayName;
    property DisplayValue: string read GetDisplayValue write SetDisplayValue;
    property Editing: Boolean read GetEditing;
    property Expanded: Boolean read GetExpanded write SetExpanded;
    property Flags: TInspectorItemFlags read GetFlags write SetFlags;
    property Hidden: Boolean read GetHidden write SetHidden;
    property Height: Integer read GetHeight write SetHeight;
    property HeightFactor: Integer read GetHeightFactor write SetHeightFactor;
    property Inspector: TJvCustomInspector read GetInspector;
    property Items[const I: Integer]: TJvCustomInspectorItem read GetItems; default;
    property Level: Integer read GetLevel;
    property Multiline: Boolean read GetMultiline write SetMultiline;
    property Parent: TJvCustomInspectorItem read GetParent;
    property QualifiedNames: Boolean read GetQualifiedNames write SetQualifiedNames;
    property Readonly: Boolean read GetReadonly write SetReadonly;
    property Rects[const RectKind: TInspectorPaintRect]: TRect read GetRects write SetRects;
    property RowSizing: TJvInspectorItemSizing read GetRowSizing write SetRowSizing;
    property SortKind: TInspectorItemSortKind read GetSortKind write SetSortKind;
    property Visible: Boolean read GetVisible write SetVisible;

    property OnCompare: TInspectorItemSortCompare read FOnCompare write SetOnCompare;
  end;

  TJvInspectorCustomCategoryItem = class(TJvCustomInspectorItem)
  private
  protected
    procedure SetFlags(const Value: TInspectorItemFlags); override;
  public
  published
  end;

  TJvInspectorCompoundColumn = class(TPersistent)
  private
    FItem: TJvCustomInspectorItem;
    FParent: TJvInspectorCustomCompoundItem;
    FWidth: Integer;
    FWidthSet: Integer;
  protected
    function GetItem: TJvCustomInspectorItem;
    function GetWidth: Integer;
    function GetWidthSet: Integer;
    procedure SetItem(Value: TJvCustomInspectorItem);
    procedure SetWidth(Value: Integer);
    procedure SetWidthExternal(Value: Integer);
    procedure SetWidthSet(Value: Integer);

    property Parent: TJvInspectorCustomCompoundItem read FParent;
  public
    constructor Create(const AParent: TJvInspectorCustomCompoundItem; const AItem: TJvCustomInspectorItem);
    destructor Destroy; override;

    property Item: TJvCustomInspectorItem read GetItem write SetItem;
    property Width: Integer read GetWidth write SetWidthExternal;
    property WidthSet: Integer read GetWidthSet;
  published
  end;

  TJvInspectorCustomCompoundItem = class(TJvCustomInspectorItem)
  private
    FCompoundItemFlags: TInspectorCompoundItemFlags;
    FColumns: TObjectList;
    FSelectedColumnIdx: Integer;
  protected
    function AddColumnPrim(const Item: TJvCustomInspectorItem): Integer; overload; virtual;
    function AddColumnPrim(const ItemIndex: Integer): Integer; overload; virtual;
    procedure DeleteColumnPrim(const Column: TJvInspectorCompoundColumn); overload; virtual;
    procedure DeleteColumnPrim(const Index: Integer); overload; virtual;
    procedure DeleteColumnPrim(const Item: TJvCustomInspectorItem); overload; virtual;
    procedure DivideRect(const RectKind: TInspectorPaintRect; const Value: TRect); virtual;
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    function GetColumnCount: Integer; virtual;
    function GetColumns(I: Integer): TJvInspectorCompoundColumn; virtual;
    function GetDisplayName: string; override;
    function GetEditing: Boolean; override;
    function GetSelectedColumn: TJvInspectorCompoundColumn; virtual;
    function GetSelectedColumnIndex: Integer; virtual;
    function GetSingleName: Boolean;
    function GetSingleNameUseFirstCol: Boolean;
    function IndexOfColumnPrim(const Col: TJvInspectorCompoundColumn): Integer; overload; virtual;
    function IndexOfColumnPrim(const Item: TJvCustomInspectorItem): Integer; overload; virtual;
    procedure InsertColumnPrim(const Index: Integer; const Item: TJvCustomInspectorItem); overload; virtual;
    procedure InsertColumnPrim(const Index, ItemIndex: Integer); overload; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure RecalcColumnWidths(const SetColumn: TJvInspectorCompoundColumn = nil); virtual;
    procedure SetCompoundItemFlags(Value: TInspectorCompoundItemFlags);
    procedure SetDisplayName(Value: string); override;
    procedure SetEditing(const Value: Boolean); override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
    procedure SetFocus; override;
    procedure SetRects(const RectKind: TInspectorPaintRect; Value: TRect); override;
    procedure SetSelectedColumn(Value: TJvInspectorCompoundColumn); virtual;
    procedure SetSelectedColumnIndex(Value: Integer); virtual;
    procedure SetSingleName(Value: Boolean);
    procedure SetSingleNameUseFirstCol(Value: Boolean);

    property ColumnCount: Integer read GetColumnCount;
    property Columns[I: Integer]: TJvInspectorCompoundColumn read GetColumns;
    property CompoundItemFlags: TInspectorCompoundItemFlags read FCompoundItemFlags write SetCompoundItemFlags;
    property SelectedColumn: TJvInspectorCompoundColumn read GetSelectedColumn write SetSelectedColumn;
    property SelectedColumnIndex: Integer read GetSelectedColumnIndex write SetSelectedColumnIndex;
    property SingleName: Boolean read GetSingleName write SetSingleName;
    property SingleNameUseFirstCol: Boolean read GetSingleNameUseFirstCol write SetSingleNameUseFirstCol;
  public
    constructor Create(const AParent: TJvCustomInspectorItem; const AData: TJvCustomInspectorData); override;
    destructor Destroy; override;

    procedure DoneEdit(const CancelEdits: Boolean = False); override;
    procedure DrawEditor(const ACanvas: TCanvas); override;
    procedure DrawName(const ACanvas: TCanvas); override;
    procedure DrawValue(const ACanvas: TCanvas); override;
    function EditFocused: Boolean; override;
    procedure InitEdit; override;
  published
  end;

  TJvInspectorCompoundItem = class(TJvInspectorCustomCompoundItem)
  private
  protected
  public
    function AddColumn(const Item: TJvCustomInspectorItem): Integer; overload;
    function AddColumn(const ItemIndex: Integer): Integer; overload;
    procedure DeleteColumn(const Column: TJvInspectorCompoundColumn); overload;
    procedure DeleteColumn(const Index: Integer); overload;
    procedure DeleteColumn(const Item: TJvCustomInspectorItem); overload;
    function IndexOfColumn(const Col: TJvInspectorCompoundColumn): Integer; overload;
    function IndexOfColumn(const Item: TJvCustomInspectorItem): Integer; overload;
    procedure InsertColumn(const Index: Integer; const Item: TJvCustomInspectorItem); overload;
    procedure InsertColumn(const Index, ItemIndex: Integer); overload;

    property ColumnCount;
    property Columns;
    property CompoundItemFlags;
    property SelectedColumn;
    property SelectedColumnIndex;
    property SingleName;
    property SingleNameUseFirstCol;
  published
  end;
  
  TJvInspectorIntegerItem = class(TJvCustomInspectorItem)
  protected
    function GetDisplayValue: string; override;
    procedure SetDisplayValue(const Value: string); override;
  public
  end;

  TJvInspectorEnumItem = class(TJvCustomInspectorItem)
  protected
    function GetDisplayValue: string; override;
    procedure GetValueList(const Strings: TStrings); override;
    procedure SetDisplayValue(const Value: string); override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
  public
  end;

  TJvInspectorFloatItem = class(TJvCustomInspectorItem)
  protected
    function GetDisplayValue: string; override;
    procedure SetDisplayValue(const Value: string); override;
  public
  end;

  TJvInspectorSetItem = class(TJvCustomInspectorItem)
  private
    FItemSetFlags: TInspectorSetFlags;
  protected
    function CanEdit: Boolean; override;
    procedure CreateMembers; virtual;
    procedure DeleteMembers; virtual;
    function GetCreateMemberItems: Boolean; virtual;
    function GetDisplayValue: string; override;
    function GetEditString: Boolean; virtual;
    function GetItemSetFlags: TInspectorSetFlags; virtual;
    procedure InvalidateMetaData; override;
    procedure SetCreateMemberItems(const Value: Boolean); virtual;
    procedure SetDisplayValue(const Value: string); override;
    procedure SetEditString(const Value: Boolean); virtual;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
    procedure SetItemSetFlags(const Value: TInspectorSetFlags); virtual;
  public
    constructor Create(const AParent: TJvCustomInspectorItem;
      const AData: TJvCustomInspectorData); override;
  published
    property ItemSetFlags: TInspectorSetFlags read GetItemSetFlags
      write SetItemSetFlags;
    property CreateMemberItems: Boolean read GetCreateMemberItems
      write SetCreateMemberItems;
    property EditString: Boolean read GetEditString write SetEditString;
  end;

  TJvInspectorCharItem = class(TJvCustomInspectorItem)
  protected
    function GetDisplayValue: string; override;
    procedure SetDisplayValue(const Value: string); override;
  public
  end;

  TJvInspectorInt64Item = class(TJvCustomInspectorItem)
  protected
    function GetDisplayValue: string; override;
    procedure SetDisplayValue(const Value: string); override;
  public
  end;

  TJvInspectorStringItem = class(TJvCustomInspectorItem)
  protected
    function GetDisplayValue: string; override;
    procedure SetDisplayValue(const Value: string); override;
  public
  end;

  TJvInspectorClassItem = class(TJvCustomInspectorItem)
  private
    FItemClassFlags: TInspectorClassFlags;
    FLastMemberInstance: TObject;
  protected
    procedure CreateMembers; virtual;
    function CanEdit: Boolean; override;
    procedure DeleteMembers; virtual;
    function GetCreateMemberItems: Boolean; virtual;
    function GetDisplayValue: string; override;
    function GetItemClassFlags: TInspectorClassFlags; virtual;
    function GetShowClassName: Boolean; virtual;
    procedure InvalidateItem; override;
    procedure InvalidateMetaData; override;
    procedure SetCreateMemberItems(const Value: Boolean); virtual;
    procedure SetDisplayValue(const Value: string); override;
    procedure SetItemClassFlags(Value: TInspectorClassFlags); virtual;
    procedure SetShowClassName(const Value: Boolean); virtual;
  public
    constructor Create(const AParent: TJvCustomInspectorItem;
      const AData: TJvCustomInspectorData); override;

    property CreateMemberItems: Boolean read GetCreateMemberItems
      write SetCreateMemberItems;
    property ItemClassFlags: TInspectorClassFlags read GetItemClassFlags write
      SetItemClassFlags;
    property OnGetValueList;
    property ShowClassName: Boolean read GetShowClassName
      write SetShowClassName;
  end;

  TJvInspectorComponentItem = class(TJvInspectorClassItem)
  private
    FItemComponentFlags: TInspectorComponentFlags;
    FOwners: TList;
  protected
    function GetItemComponentFlags: TInspectorComponentFlags;
    function GetKeepFirstOwnerAsFirst: Boolean;
    function GetNoShowFirstOwnerName: Boolean;
    function GetOwnerCount: Integer;
    function GetOwners(I: Integer): TComponent;
    function GetShowOwnerNames: Boolean;
    function GetSortComponents: Boolean;
    function GetSortOwners: Boolean;
    procedure GetValueList(const Strings: TStrings); override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
    procedure SetItemClassFlags(Value: TInspectorClassFlags); override;
    procedure SetItemComponentFlags(Value: TInspectorComponentFlags); virtual;
    procedure SetKeepFirstOwnerAsFirst(Value: Boolean);
    procedure SetNoShowFirstOwnerName(Value: Boolean);
    procedure SetOwners(I: Integer; Value: TComponent);
    procedure SetShowOwnerNames(Value: Boolean);
    procedure SetSortComponents(Value: Boolean);
    procedure SetSortOwners(Value: Boolean);
  public
    constructor Create(const AParent: TJvCustomInspectorItem;
      const AData: TJvCustomInspectorData); override;
    destructor Destroy; override;
    procedure AddOwner(const AOwner: TComponent);
    procedure DeleteOwner(const AOwner: TComponent); overload;
    procedure DeleteOwner(const Index: Integer); overload;

    property ItemComponentFlags: TInspectorComponentFlags read GetItemComponentFlags write SetItemComponentFlags;
    property KeepFirstOwnerAsFirst: Boolean read GetKeepFirstOwnerAsFirst write SetKeepFirstOwnerAsFirst;
    property NoShowFirstOwnerName: Boolean read GetNoShowFirstOwnerName write SetNoShowFirstOwnerName;
    property OwnerCount: Integer read GetOwnerCount;
    property Owners[I: Integer]: TComponent read GetOwners write SetOwners;
    property ShowOwnerNames: Boolean read GetShowOwnerNames write SetShowOwnerNames;
    property SortComponents: Boolean read GetSortComponents write SetSortComponents;
    property SortOwners: Boolean read GetSortOwners write SetSortOwners;
  end;
  
  TJvInspectorFontItem = class(TJvInspectorClassItem)
  protected
    procedure Edit; override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
  end;

  TJvInspectorFontNameItem = class(TJvInspectorStringItem)
  protected
    procedure DoDrawListItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    procedure DoMeasureListItem(Control: TWinControl; Index: Integer;
      var Height: Integer); override;
    procedure DoMeasureListItemWidth(Control: TWinControl; Index: Integer;
      var Width: Integer); override;
    procedure GetValueList(const Strings: TStrings); override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
  end;

  TJvInspectorBooleanItem = class(TJvInspectorEnumItem)
  private
    FCheckRect: TRect;
    FShowAsCheckbox: Boolean;
  protected
    function GetShowAsCheckbox: Boolean; virtual;
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure SetShowAsCheckbox(Value: Boolean); virtual;
  public
    procedure DoneEdit(const CancelEdits: Boolean = False); override;
    procedure DrawValue(const ACanvas: TCanvas); override;
    procedure InitEdit; override;

    property ShowAsCheckbox: Boolean read GetShowAsCheckBox
      write SetShowAsCheckBox;
  end;

  TJvInspectorDateItem = class(TJvInspectorFloatItem)
  private
    FFormat: string;
  protected
    function GetDisplayValue: string; override;
    procedure SetDisplayValue(const Value: string); override;
    procedure SetFormat(Value: string);
  public
    constructor Create(const AParent: TJvCustomInspectorItem;
      const AData: TJvCustomInspectorData); override;
  published
    property Format: string read FFormat write SetFormat;
  end;

  TJvInspectorTimeItem = class(TJvInspectorFloatItem)
  private
    FFormat: string;
    FShowAMPM: Boolean;
    FShowSeconds: Boolean;
  protected
    function GetDisplayValue: string; override;
    procedure SetDisplayValue(const Value: string); override;
    procedure SetFormat;
    procedure SetShowAMPM(Value: Boolean);
    procedure SetShowSeconds(Value: Boolean);

    property Format: string read FFormat;
  public
    constructor Create(const AParent: TJvCustomInspectorItem;
      const AData: TJvCustomInspectorData); override;
  published
    property ShowAMPM: Boolean read FShowAMPM write SetShowAMPM;
    property ShowSeconds: Boolean read FShowSeconds write SetShowSeconds;
  end;

  TJvInspectorDateTimeItem = class(TJvInspectorCustomCompoundItem)
  private
    FDate: TJvInspectorDateItem;
    FTime: TJvInspectorTimeItem;
  protected
    function GetDateFormat: string;
    function GetTimeShowAMPM: Boolean;
    function GetTimeShowSeconds: Boolean;
    procedure SetDateFormat(Value: string);
    procedure SetTimeShowAMPM(Value: Boolean);
    procedure SetTimeShowSeconds(Value: Boolean);
  public
    constructor Create(const AParent: TJvCustomInspectorItem;
      const AData: TJvCustomInspectorData); override;
  published
    property DateFormat: string read GetDateFormat write SetDateFormat;
    property TimeShowAMPM: Boolean read GetTimeShowAMPM write SetTimeShowAMPM;
    property TimeShowSeconds: Boolean read GetTimeShowSeconds write SetTimeShowSeconds;
  end;

  //----------------------------------------------------------------------------
  // Inspector data classes
  //----------------------------------------------------------------------------
  TJvCustomInspectorData = class(TPersistent)
  private
    FTypeInfo: PTypeInfo;
    FItems: TJvInspectorItemInstances;
    FName: string;
    FRegistered: Boolean;
  protected
    constructor CreatePrim(const AName: string; const ATypeInfo: PTypeInfo);
    procedure CheckReadAccess; virtual;
    procedure CheckWriteAccess; virtual;
    procedure DoneEdits(const CancelEdits: Boolean = False);
    function GetAsFloat: Extended; virtual; abstract;
    function GetAsInt64: Int64; virtual; abstract;
    function GetAsMethod: TMethod; virtual; abstract;
    function GetAsOrdinal: Int64; virtual; abstract;
    function GetAsString: string; virtual; abstract;
    function GetItemCount: Integer;
    function GetItems(I: Integer): TJvCustomInspectorItem;
    function GetName: string; virtual;
    function GetTypeInfo: PTypeInfo; virtual;
    procedure InitEdits;
    procedure Invalidate; virtual;
    function IsEqualReference(const Ref: TJvCustomInspectorData): Boolean; virtual;
    procedure NotifyRemoveData(const Instance: TJvCustomInspectorData); virtual;
    procedure RefreshEdits;
    class function RegisterInstance(const Instance: TJvCustomInspectorData): TJvCustomInspectorData;
    procedure RemoveItem(const Item: TJvCustomInspectorItem);
    procedure SetAsFloat(const Value: Extended); virtual; abstract;
    procedure SetAsInt64(const Value: Int64); virtual; abstract;
    procedure SetAsMethod(const Value: TMethod); virtual; abstract;
    procedure SetAsOrdinal(const Value: Int64); virtual; abstract;
    procedure SetAsString(const Value: string); virtual; abstract;
    procedure SetName(const Value: string); virtual;
    procedure SetTypeInfo(const Value: PTypeInfo); virtual;
  public
    constructor Create;
    procedure BeforeDestruction; override;
    procedure GetAsSet(var Buf); virtual; abstract;
    function HasValue: Boolean; virtual; abstract;
    function IsAssigned: Boolean; virtual; abstract;
    function IsInitialized: Boolean; virtual; abstract;
    class function ItemRegister: TJvInspectorRegister; virtual;
    class function New: TJvCustomInspectorData;
    function NewItem(const AParent: TJvCustomInspectorItem): TJvCustomInspectorItem; virtual;
    procedure SetAsSet(const Buf); virtual; abstract;

    property AsFloat: Extended read GetAsFloat write SetAsFloat;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsMethod: TMethod read GetAsMethod;
    property AsOrdinal: Int64 read GetAsOrdinal write SetAsOrdinal;
    property AsString: string read GetAsString write SetAsString;
    property ItemCount: Integer read GetItemCount;
    property Items[I: Integer]: TJvCustomInspectorItem read GetItems;
    property Name: string read GetName write SetName;
    property TypeInfo: PTypeInfo read GetTypeInfo write SetTypeInfo;
  end;

  TJvInspectorSetMemberData = class(TJvCustomInspectorData)
  private
    FBitOffset: Integer;
    FDataParent: TJvCustomInspectorData;
  protected
    function GetAsFloat: Extended; override;
    function GetAsInt64: Int64; override;
    function GetAsMethod: TMethod; override;
    function GetAsOrdinal: Int64; override;
    function GetAsString: string; override;
    function IsEqualReference(const Ref: TJvCustomInspectorData): Boolean; override;
    procedure NotifyRemoveData(const Instance: TJvCustomInspectorData); override;
    procedure SetAsFloat(const Value: Extended); override;
    procedure SetAsInt64(const Value: Int64); override;
    procedure SetAsMethod(const Value: TMethod); override;
    procedure SetAsOrdinal(const Value: Int64); override;
    procedure SetAsString(const Value: string); override;
  public
    procedure GetAsSet(var Buf); override;
    function HasValue: Boolean; override;
    function IsAssigned: Boolean; override;
    function IsInitialized: Boolean; override;
    class function New(const AParent: TJvCustomInspectorItem; const Ordinal: Integer; const ADataParent: TJvCustomInspectorData): TJvCustomInspectorItem;
    procedure SetAsSet(const Buf); override;

    property BitOffset: Integer read FBitOffset;
    property DataParent: TJvCustomInspectorData read FDataParent;
  end;

  TJvInspectorVarData = class(TJvCustomInspectorData)
  private
    FAddress: Pointer;
  protected
    function GetAddress: Pointer; virtual;
    function GetAsFloat: Extended; override;
    function GetAsInt64: Int64; override;
    function GetAsMethod: TMethod; override;
    function GetAsOrdinal: Int64; override;
    function GetAsString: string; override;
    function IsEqualReference(const Ref: TJvCustomInspectorData): Boolean; override;
    procedure SetAddress(const Value: Pointer); virtual;
    procedure SetAsFloat(const Value: Extended); override;
    procedure SetAsInt64(const Value: Int64); override;
    procedure SetAsMethod(const Value: TMethod); override;
    procedure SetAsOrdinal(const Value: Int64); override;
    procedure SetAsString(const Value: string); override;
  public
    procedure GetAsSet(var Buf); override;
    function HasValue: Boolean; override;
    function IsAssigned: Boolean; override;
    function IsInitialized: Boolean; override;
    class function ItemRegister: TJvInspectorRegister; override;
    class function New(const AParent: TJvCustomInspectorItem; const AName: string; const ATypeInfo: PTypeInfo; const AAddress: Pointer): TJvCustomInspectorItem; overload;
    class function New(const AParent: TJvCustomInspectorItem; const AName: string; const ATypeInfo: PTypeInfo; const AVar): TJvCustomInspectorItem; overload;
    procedure SetAsSet(const Buf); override;

    property Address: Pointer read GetAddress write SetAddress;
  end;

  TJvInspectorPropData = class(TJvCustomInspectorData)
  private
    FInstance: TObject;
    FProp: PPropInfo;
  protected
    function GetAsFloat: Extended; override;
    function GetAsInt64: Int64; override;
    function GetAsMethod: TMethod; override;
    function GetAsOrdinal: Int64; override;
    function GetAsString: string; override;
    function GetInstance: TObject; virtual;
    function GetProp: PPropInfo; virtual;
    function IsEqualReference(const Ref: TJvCustomInspectorData): Boolean; override;
    procedure NotifyRemoveData(const Instance: TJvCustomInspectorData); override;
    procedure SetAsFloat(const Value: Extended); override;
    procedure SetAsInt64(const Value: Int64); override;
    procedure SetAsMethod(const Value: TMethod); override;
    procedure SetAsOrdinal(const Value: Int64); override;
    procedure SetAsString(const Value: string); override;
    procedure SetInstance(const Value: TObject); virtual;
    procedure SetProp(const Value: PPropInfo); virtual;
  public
    procedure GetAsSet(var Buf); override;
    function HasValue: Boolean; override;
    function IsAssigned: Boolean; override;
    function IsInitialized: Boolean; override;
    class function ItemRegister: TJvInspectorRegister; override;
    class function New(const AParent: TJvCustomInspectorItem; const AInstance: TObject;
      const PropInfo: PPropInfo): TJvCustomInspectorItem; overload;
    class function New(const AParent: TJvCustomInspectorItem; const AInstance: TObject;
      const TypeKinds: TTypeKinds = tkProperties): TJvInspectorItemInstances; overload;
    class function New(const AParent: TJvCustomInspectorItem; const AInstance: TObject;
      const NameList: array of string; const ExcludeList: Boolean = False;
      const TypeKinds: TTypeKinds = tkProperties): TJvInspectorItemInstances; overload;
    class function New(const AParent: TJvCustomInspectorItem; const AInstance: TObject;
      const PropInfos: PPropList; const PropCount: Integer): TJvInspectorItemInstances; overload;
    procedure SetAsSet(const Buf); override;

    property Instance: TObject read GetInstance write SetInstance;
    property Prop: PPropInfo read GetProp write SetProp;
  end;

  TJvInspectorEventData  = class(TJvCustomInspectorData)
  private
    FOnGetAsFloat: TJvInspAsFloat;
    FOnGetAsInt64: TJvInspAsInt64;
    FOnGetAsMethod: TJvInspAsMethod;
    FOnGetAsOrdinal: TJvInspAsInt64;
    FOnGetAsString: TJvInspAsString;
    FOnGetAsSet: TJvInspAsSet;
    FOnSetAsFloat: TJvInspAsFloat;
    FOnSetAsInt64: TJvInspAsInt64;
    FOnSetAsMethod: TJvInspAsMethod;
    FOnSetAsOrdinal: TJvInspAsInt64;
    FOnSetAsString: TJvInspAsString;
    FOnSetAsSet: TJvInspAsSet;
  protected
    function DoGetAsFloat: Extended;
    function DoGetAsInt64: Int64;
    function DoGetAsMethod: TMethod;
    function DoGetAsOrdinal: Int64;
    function DoGetAsString: string;
    procedure DoGetAsSet(out Buf; var BufSize: Integer);
    procedure DoSetAsFloat(Value: Extended);
    procedure DoSetAsInt64(Value: Int64);
    procedure DoSetAsMethod(Value: TMethod);
    procedure DoSetAsOrdinal(Value: Int64);
    procedure DoSetAsString(Value: string);
    procedure DoSetAsSet(const Buf; var BufSize: Integer);
    function GetAsFloat: Extended; override;
    function GetAsInt64: Int64; override;
    function GetAsMethod: TMethod; override;
    function GetAsOrdinal: Int64; override;
    function GetAsString: string; override;
    function IsEqualReference(const Ref: TJvCustomInspectorData): Boolean; override;
    procedure SetAsFloat(const Value: Extended); override;
    procedure SetAsInt64(const Value: Int64); override;
    procedure SetAsMethod(const Value: TMethod); override;
    procedure SetAsOrdinal(const Value: Int64); override;
    procedure SetAsString(const Value: string); override;
    procedure SetOnGetAsFloat(Value: TJvInspAsFloat);
    procedure SetOnGetAsInt64(Value: TJvInspAsInt64);
    procedure SetOnGetAsMethod(Value: TJvInspAsMethod);
    procedure SetOnGetAsOrdinal(Value: TJvInspAsInt64);
    procedure SetOnGetAsString(Value: TJvInspAsString);
    procedure SetOnGetAsSet(Value: TJvInspAsSet);
    procedure SetOnSetAsFloat(Value: TJvInspAsFloat);
    procedure SetOnSetAsInt64(Value: TJvInspAsInt64);
    procedure SetOnSetAsMethod(Value: TJvInspAsMethod);                  
    procedure SetOnSetAsOrdinal(Value: TJvInspAsInt64);
    procedure SetOnSetAsString(Value: TJvInspAsString);
    procedure SetOnSetAsSet(Value: TJvInspAsSet);
  public
    procedure GetAsSet(var Buf); override;
    function HasValue: Boolean; override;
    function IsAssigned: Boolean; override;
    function IsInitialized: Boolean; override;
    class function New(const AParent: TJvCustomInspectorItem; const AName: string; const ATypeInfo: PTypeInfo): TJvCustomInspectorItem; 
    procedure SetAsSet(const Buf); override;

    property OnGetAsFloat: TJvInspAsFloat read FOnGetAsFloat write SetOnGetAsFloat;
    property OnGetAsInt64: TJvInspAsInt64 read FOnGetAsInt64 write SetOnGetAsInt64;
    property OnGetAsMethod: TJvInspAsMethod read FOnGetAsMethod write SetOnGetAsMethod;
    property OnGetAsOrdinal: TJvInspAsInt64 read FOnGetAsOrdinal write SetOnGetAsOrdinal;
    property OnGetAsString: TJvInspAsString read FOnGetAsString write SetOnGetAsString;
    property OnGetAsSet: TJvInspAsSet read FOnGetAsSet write SetOnGetAsSet;
    property OnSetAsFloat: TJvInspAsFloat read FOnSetAsFloat write SetOnSetAsFloat;
    property OnSetAsInt64: TJvInspAsInt64 read FOnSetAsInt64 write SetOnSetAsInt64;
    property OnSetAsMethod: TJvInspAsMethod read FOnSetAsMethod write SetOnSetAsMethod;
    property OnSetAsOrdinal: TJvInspAsInt64 read FOnSetAsOrdinal write SetOnSetAsOrdinal;
    property OnSetAsString: TJvInspAsString read FOnSetAsString write SetOnSetAsString;
    property OnSetAsSet: TJvInspAsSet read FOnSetAsSet write SetOnSetAsSet;
  end;

  TJvInspectorCustomConfData = class(TJvCustomInspectorData)
  private
    FKey: string;
    FSection: string;
  protected
    constructor CreatePrim(const AName, ASection, AKey: string; const ATypeInfo: PTypeInfo);
    function ExistingValue: Boolean; virtual; abstract;
    function GetAsFloat: Extended; override;
    function GetAsInt64: Int64; override;
    function GetAsMethod: TMethod; override;
    function GetAsOrdinal: Int64; override;
    function GetAsString: string; override;
    function ReadValue: string; virtual; abstract;
    function IsEqualReference(const Ref: TJvCustomInspectorData): Boolean; override;
    procedure SetAsFloat(const Value: Extended); override;
    procedure SetAsInt64(const Value: Int64); override;
    procedure SetAsMethod(const Value: TMethod); override;
    procedure SetAsOrdinal(const Value: Int64); override;
    procedure SetAsString(const Value: string); override;
    procedure SetKey(Value: string);
    procedure SetSection(Value: string);
    procedure WriteValue(Value: string); virtual; abstract;
  public
    procedure GetAsSet(var Buf); override;
    function HasValue: Boolean; override;
    function IsAssigned: Boolean; override;
    function IsInitialized: Boolean; override;
    procedure SetAsSet(const Buf); override;

    property Key: string read FKey write SetKey;
    property Section: string read FSection write SetSection;
  end;

  TJvInspectorINIFileData = class(TJvInspectorCustomConfData)
  private
    FINIFile: TCustomIniFile;
  protected
    function ExistingValue: Boolean; override;
    function IsEqualReference(const Ref: TJvCustomInspectorData): Boolean; override;
    function ReadValue: string; override;
    procedure WriteValue(Value: string); override;
  public
    class function New(const AParent: TJvCustomInspectorItem; const AName, ASection, AKey: string; const ATypeInfo: PTypeInfo; const AINIFile: TCustomIniFile): TJvCustomInspectorItem; overload;
    class function New(const AParent: TJvCustomInspectorItem; const ASection: string; const AINIFile: TCustomIniFile; const OnAddKey: TJvInspConfKeyEvent): TJvInspectorItemInstances; overload;
    class function New(const AParent: TJvCustomInspectorItem; const AINIFile: TCustomIniFile; const OnAddSection: TJvInspConfSectionEvent; const OnAddKey: TJvInspConfKeyEvent): TJvInspectorItemInstances; overload;

    property INIFile: TCustomIniFile read FINIFile;
  end;

//----------------------------------------------------------------------------
// Inspector item registration
//----------------------------------------------------------------------------
  TJvInspectorRegister = class(TPersistent)
  private
    FDataClass: TJvInspectorDataClass;
    FItems: TObjectList;
  protected
    function Compare(const ADataObj: TJvCustomInspectorData; const Item1,
      Item2: TJvCustomInspectorRegItem): Integer;
    function GetCount: Integer;
    function GetItems(const I: Integer): TJvCustomInspectorRegItem; virtual;
  public
    constructor Create(const ADataClass: TJvInspectorDataClass);
    destructor Destroy; override;
    procedure Add(const RegItem: TJvCustomInspectorRegItem);
    procedure Delete(const RegItem: TJvCustomInspectorRegItem);
    function FindMatch(
      const ADataObj: TJvCustomInspectorData): TJvCustomInspectorRegItem;

    property Count: Integer read GetCount;
    property DataClass: TJvInspectorDataClass read FDataClass;
    property Items[const I: Integer]: TJvCustomInspectorRegItem read GetItems;
  end;

  TJvCustomInspectorRegItem = class(TPersistent)
  private
    FItemClass: TJvInspectorItemClass;
  protected
    function CompareTo(const ADataObj: TJvCustomInspectorData;
      const Item: TJvCustomInspectorRegItem): Integer; virtual;
    function GetItemClass: TJvInspectorItemClass; virtual;
    procedure SetItemClass(const Value: TJvInspectorItemClass); virtual;
  public
    constructor Create(const AItemClass: TJvInspectorItemClass);
    procedure ApplyDefaults(const Item: TJvCustomInspectorItem); virtual;
    function Compare(const ADataObj: TJvCustomInspectorData;
      const Item: TJvCustomInspectorRegItem): Integer; virtual;
    function IsMatch(const ADataObj: TJvCustomInspectorData): Boolean; virtual;
    function MatchValue(const ADataObj: TJvCustomInspectorData): Integer;
      virtual; abstract;
    function MatchPercent(const ADataObj: TJvCustomInspectorData): Integer; virtual; abstract;

    property ItemClass: TJvInspectorItemClass read GetItemClass;
  end;

  TJvInspectorTypeInfoRegItem = class(TJvCustomInspectorRegItem)
  private
    FTypeInfo: PTypeInfo;
  protected
    function GetTypeInfo: PTypeInfo; virtual;
    procedure SetTypeInfo(const Value: PTypeInfo); virtual;
  public
    constructor Create(const AItemClass: TJvInspectorItemClass;
      const ATypeInfo: PTypeInfo);
    function MatchValue(const ADataObj: TJvCustomInspectorData): Integer;
      override;
    function MatchPercent(const ADataObj: TJvCustomInspectorData): Integer; override;

    property TypeInfo: PTypeInfo read GetTypeInfo;
  end;

  TJvInspectorTCaptionRegItem = class(TJvInspectorTypeInfoRegItem)
  public
    procedure ApplyDefaults(const Item: TJvCustomInspectorItem); override;
  end;

  TJvInspectorTypeKindRegItem = class(TJvCustomInspectorRegItem)
  private
    FTypeKind: TTypeKind;
  protected
    function CompareTo(const ADataObj: TJvCustomInspectorData;
      const Item: TJvCustomInspectorRegItem): Integer; override;
    function GetTypeKind: TTypeKind; virtual;
    procedure SetTypeKind(const Value: TTypeKind); virtual;
  public
    constructor Create(const AItemClass: TJvInspectorItemClass;
      const ATypeKind: TTypeKind);
    function Compare(const ADataObj: TJvCustomInspectorData;
      const Item: TJvCustomInspectorRegItem): Integer; override;
    function MatchValue(const ADataObj: TJvCustomInspectorData): Integer;
      override;
    function MatchPercent(const ADataObj: TJvCustomInspectorData): Integer; override;

    property TypeKind: TTypeKind read GetTypeKind;
  end;

  TJvInspectorPropRegItem = class(TJvCustomInspectorRegItem)
  private
    FObjectClass: TClass;
    FName: string;
    FTypeInfo: PTypeInfo;
  protected
  public
    constructor Create(const AItemClass: TJvInspectorItemClass; const AObjectClass: TClass;
      const AName: string; const ATypeInfo: PTypeInfo);
    function Compare(const ADataObj: TJvCustomInspectorData;
      const Item: TJvCustomInspectorRegItem): Integer; override;
    function MatchValue(const ADataObj: TJvCustomInspectorData): Integer; override;
    function MatchPercent(const ADataObj: TJvCustomInspectorData): Integer; override;

    property Name: string read FName;
    property ObjectClass: TClass read FObjectClass;
    property TypeInfo: PTypeInfo read FTypeInfo;
  end;

implementation

uses
  Consts, Dialogs, ExtCtrls, Forms, {$IFDEF COMPILER6_UP}RTLConsts,{$ENDIF}
  JclRTTI, JclLogic;

type
  PMethod = ^TMethod;
  PComp = ^Comp;
  PPointer = ^Pointer;
  TOpenEdit = class(TCustomEdit);

var
  FGenItemReg: TJvInspectorRegister;
  FVarItemReg: TJvInspectorRegister;
  FPropItemReg: TJvInspectorRegister;

type
  TJvPopupListBox = class(TCustomListBox)
  private
    FSearchText: string;
    FSearchTickCount: Longint;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyPress(var Key: Char); override;
  public
  end;

{ TJvPopupListBox - copied from RxLib }

procedure TJvPopupListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
    AddBiDiModeExStyle(ExStyle);
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

procedure TJvPopupListBox.CreateWnd;
begin
  inherited CreateWnd;
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0);
end;

procedure TJvPopupListBox.KeyPress(var Key: Char);
var
  TickCount: Integer;
begin
  case Key of
    #8, #27: FSearchText := '';
    #32..#255:
      begin
        TickCount := GetTickCount;
        if TickCount - FSearchTickCount > 4000 then
          FSearchText := '';
        FSearchTickCount := TickCount;
        if Length(FSearchText) < 32 then
          FSearchText := FSearchText + Key;
        SendMessage(Handle, LB_SELECTSTRING, WORD(-1),
          Longint(PChar(FSearchText)));
        Key := #0;
      end;
  end;
  inherited KeyPress(Key);
end;

function HeightOf(const Rect: TRect): Integer;
begin
  Result := Rect.Bottom - Rect.Top;
end;

function WidthOf(const Rect: TRect): Integer;
begin
  Result := Rect.Right - Rect.Left;
end;

type
  TCanvasStack = class(TObjectList)
  private
    FTop: Integer;
  protected
    procedure ChangeCapacity(const Value: Integer);
  public
    constructor Create(const ACapacity: Integer);
    function Push(const Canvas: TCanvas): Integer;
    procedure Pop(const Canvas: TCanvas; Index: Integer = -2);
    procedure Peek(const Canvas: TCanvas; Index: Integer = -2);

    property Capacity write ChangeCapacity;
    property Top: Integer read FTop write FTop;
  end;

  TCanvasState = class(TPersistent)
  private
    FBrush: TBrush;
    FPen: TPen;
    FFont: TFont;
  protected
  public
    constructor Create(const Canvas: TCanvas);
    destructor Destroy; override;
    procedure ApplyTo(const Canvas: TCanvas);
    procedure SetState(const Canvas: TCanvas);
  end;

var
  CanvasStack: TCanvasStack;

procedure TCanvasStack.ChangeCapacity(const Value: Integer);
var
  I: Integer;
begin
  if Capacity <> Value then
  begin
    if Value < Capacity then
    begin
      inherited Capacity := Value;
      if FTop >= Capacity then
        FTop := Pred(Capacity);
    end
    else
    begin
      I := Capacity;
      inherited Capacity := Value;
      for I := I to Pred(Value) do
        Add(TCanvasState.Create(nil));
    end;
  end;
end;

constructor TCanvasStack.Create(const ACapacity: Integer);
begin
  inherited Create(True);
  FTop := -1;
  Capacity := ACapacity;
end;

function TCanvasStack.Push(const Canvas: TCanvas): Integer;
begin
  Inc(FTop);
  if FTop >= Capacity then
    Capacity := Capacity + 128;
  Result := FTop;
  TCanvasState(Items[Result]).SetState(Canvas);
end;

procedure TCanvasStack.Pop(const Canvas: TCanvas; Index: Integer = -2);
begin
  if Index = -1 then
    Index := FTop;
  TCanvasState(Items[Index]).ApplyTo(Canvas);
  FTop := Pred(Index);
end;

procedure TCanvasStack.Peek(const Canvas: TCanvas; Index: Integer = -2);
begin
  if Index = -1 then
    Index := FTop;
  TCanvasState(Items[Index]).ApplyTo(Canvas);
end;

//------------------------------------------------------------------------------

constructor TCanvasState.Create(const Canvas: TCanvas);
begin
  inherited Create;
  FBrush := TBrush.Create;
  FPen := TPen.Create;
  FFont := TFont.Create;
  if Canvas <> nil then
    SetState(Canvas);
end;

destructor TCanvasState.Destroy;
begin
  FFont.Free;
  FPen.Free;
  FBrush.Free;
end;

procedure TCanvasState.ApplyTo(const Canvas: TCanvas);
begin
  Canvas.Brush.Assign(FBrush);
  Canvas.Pen.Assign(FPen);
  Canvas.Font.Assign(FFont);
end;

procedure TCanvasState.SetState(const Canvas: TCanvas);
begin
  FBrush.Assign(Canvas.Brush);
  FPen.Assign(Canvas.Pen);
  FFont.Assign(Canvas.Font);
end;

function SaveCanvasState(const Canvas: TCanvas): Integer;
begin
  Result := CanvasStack.Push(Canvas);
end;

procedure ApplyCanvasState(const Canvas: TCanvas; const SavedIdx: Integer);
begin
  TCanvasState(CanvasStack[SavedIdx]).ApplyTo(Canvas);
end;

procedure RestoreCanvasState(const Canvas: TCanvas; const SavedIdx: Integer);
begin
  CanvasStack.Pop(Canvas, SavedIdx);
end;

procedure SetDefaultProp(const Instance: TObject; const PropName: string); overload;
var
  Prop: PPropInfo;
begin
  Prop := GetPropInfo(Instance, PropName);
  if (Prop <> nil) and (Prop.Default <> Low(Integer)) then
    SetOrdProp(Instance, Prop, Prop.Default);
end;

procedure SetDefaultProp(const Instance: TObject; const PropNames: array of string); overload;
var
  I: Integer;
begin
  for I := Low(PropNames) to High(PropNames) do
    SetDefaultProp(Instance, PropNames[I]);
end;

{ TJvInspDataReg }

type
  TJvInspDataReg = class(TPersistent)
  private
    FInstanceList: TJvInspectorDataInstances;
    FClearing: Boolean;
  protected
    function GetCount: Integer;
    function GetItems(I: Integer): TJvCustomInspectorData;
  public
    constructor Create;
    destructor Destroy; override;

    // Adds a new data instance. If an instance pointing to the same data exists the given instance is destroyed and the registered instance returned
    function Add(Instance: TJvCustomInspectorData): TJvCustomInspectorData;
    // Deletes a data instance and all items referencing it. All other data instances are notified.
    procedure Delete(Instance: TJvCustomInspectorData);
    // Deletes all data instances and items referencing them. No notification is issued to the data instances as they will be removed also.
    procedure Clear;
    // Locates a data instance that references the same data as the given instance. The index is returned or -1 if no instance was found.
    function Locate(Instance: TJvCustomInspectorData): Integer;
    // Removes a data instance from the list. All other data instances are notified.
    procedure Remove(Instance: TJvCustomInspectorData);

    property Count: Integer read GetCount;
    property Items[I: Integer]: TJvCustomInspectorData read GetItems;
  end;

function TJvInspDataReg.GetCount: Integer;
begin
  Result := Length(FInstanceList);
end;

function TJvInspDataReg.GetItems(I: Integer): TJvCustomInspectorData;
begin
  if (I < Low(FInstanceList)) or (I > High(FInstanceList)) then
    TList.Error(@SListIndexError, I);
  Result := FInstanceList[I];
end;

constructor TJvInspDataReg.Create;
begin
  inherited Create;
  SetLength(FInstanceList, 0);
end;

destructor TJvInspDataReg.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJvInspDataReg.Add(Instance: TJvCustomInspectorData): TJvCustomInspectorData;
var
  I: Integer;
begin
  I := Locate(Instance);
  if I = -1 then
  begin
    SetLength(FInstanceList, Count + 1);
    FInstanceList[High(FInstanceList)] := Instance;
    Result := Instance;
    Result.FRegistered := True;
  end
  else
  begin
    if Items[I] <> Instance then
      Instance.Free;
    Result := Items[I];
  end;
end;

procedure TJvInspDataReg.Delete(Instance: TJvCustomInspectorData);
begin
  Instance.Free;
end;

procedure TJvInspDataReg.Clear;
var
  I: Integer;
begin
  FClearing := True;
  try
    for I := High(FInstanceList) downto Low(FInstanceList) do
      Items[I].Free;
  finally
    FClearing := False;
  end;
end;

function TJvInspDataReg.Locate(Instance: TJvCustomInspectorData): Integer;
begin
  Result := High(FInstanceList);
  while Result > -1 do
  begin
    if (Instance = Items[Result]) or Instance.IsEqualReference(Items[Result]) then
      Break;
    Dec(Result);
  end;
end;

procedure TJvInspDataReg.Remove(Instance: TJvCustomInspectorData);
var
  I: Integer;
begin
  I := Locate(Instance);
  if I > -1 then
  begin
    if Items[I] <> Instance then
      raise EJvInspectorData.Create('Internal error: two data instances pointing to the same data are registered.');
    if I < High(FInstanceList) then
      Move(FInstanceList[I + 1], FInstanceList[I], (Length(FInstanceList) - I) * SizeOf(TJvCustomInspectorData));
    SetLength(FInstanceList, High(FInstanceList));
    if not FClearing then
      for I := Low(FInstanceList) to High(FInstanceList) do
        Items[I].NotifyRemoveData(Instance);
  end;
end;

var
  DataRegister: TJvInspDataReg;
  
{ TJvCustomInspector }

function TJvCustomInspector.CalcImageHeight: Integer;
var
  BandHeightNoSB: Integer;
  BandHeightSB: Integer;
  ClHeightNoSB: Integer;
  ClHeightSB: Integer;
  WinStyle: Longint;
  I: Integer;
begin
  BandHeightNoSB := 0;
  BandHeightSB := 0;
  FImageHeight := 0;
  FBandStartsNoSB.Clear;
  FBandStartsNoSB.Add(Pointer(0));
  FBandStartsSB.Clear;
  FBandStartsSB.Add(Pointer(0));
  ClHeightNoSB := ClientHeight;
  WinStyle := GetWindowLong(Handle, GWL_STYLE);
  if (WinStyle and WS_HSCROLL) <> 0 then
  begin
    ClHeightSB := ClHeightNoSB;
    Inc(ClHeightNoSB, GetSystemMetrics(SM_CYHSCROLL));
  end
  else
  begin
    ClHeightSB := ClHeightNoSB;
    Dec(ClHeightSB, GetSystemMetrics(SM_CYHSCROLL));
  end;
  for I := 0 to Pred(VisibleCount) do
  begin
    Inc(FImageHeight, VisibleItems[I].Height);
    if UseBands then
    begin
      if ((BandHeightSB + VisibleItems[I].Height) > ClHeightSB) and (BandHeightSB > 0) then
      begin
        FBandStartsSB.Add(Pointer(I));
        BandHeightSB := 0;
      end;
      if ((BandHeightNoSB + VisibleItems[I].Height) > ClHeightNoSB) and (BandHeightNoSB > 0) then
      begin
        FBandStartsNoSB.Add(Pointer(I));
        BandHeightNoSB := 0;
      end;
    end;
    Inc(BandHeightNoSB, VisibleItems[I].Height);
    Inc(BandHeightSB, VisibleItems[I].Height);
  end;
  Result := FImageHeight;
end;

function TJvCustomInspector.CalcItemIndex(X, Y: Integer; var Rect: TRect): Integer;
var
  BandIdx: Integer;
  MaxIdx: Integer;
begin
  if UseBands then
  begin
    BandIdx := X div BandWidth + BandStarts.IndexOf(Pointer(TopIndex));
    if BandIdx < BandStarts.Count then
      Result := Integer(BandStarts[BandIdx])
    else
      Result := -1;
  end
  else
    Result := TopIndex;
  MaxIdx := VisibleCount;
  while (Result <> -1) and (Result < MaxIdx) and not PtInRect(VisibleItems[Result].Rects[iprItem], Point(X, Y)) do
    Inc(Result);
  if Result >= MaxIdx then
    Result := -1;
  if Result > -1 then
    Rect := VisibleItems[Result].Rects[iprItem];
end;

function TJvCustomInspector.CalcItemRect(
  const Item: TJvCustomInspectorItem): TRect;
begin
  Result := Item.Rects[iprItem];
end;

procedure TJvCustomInspector.DoAfterDataCreate(
  const Data: TJvCustomInspectorData);
begin
  if Assigned(FAfterDataCreate) then
    FAfterDataCreate(Self, Data);
end;

procedure TJvCustomInspector.DoAfterItemCreate(const Item: TJvCustomInspectorItem);
begin
  if Assigned(FAfterItemCreate) then
    FAfterItemCreate(Self, Item);
end;

procedure TJvCustomInspector.DoBeforeItemCreate(
  const Data: TJvCustomInspectorData; var ItemClass: TJvInspectorItemClass);
begin
  if Assigned(FBeforeItemCreate) then
    FBeforeItemCreate(Self, Data, ItemClass);
end;

function TJvCustomInspector.DoBeforeItemSelect(const NewItem: TJvCustomInspectorItem): Boolean;
begin
  Result := True;
  if Assigned(FBeforeSelection) then
    FBeforeSelection(Self, NewItem, Result);
end;

procedure TJvCustomInspector.DoItemSelected;
begin
  if Assigned(FOnItemSelected) then
    FOnItemSelected(Self);
end;

function TJvCustomInspector.GetAfterDataCreate: TInspectorDataEvent;
begin
  Result := FAfterDataCreate;
end;

function TJvCustomInspector.GetAfterItemCreate: TInspectorItemEvent;
begin
  Result := FAfterItemCreate;
end;

function TJvCustomInspector.GetBandFor(const ItemIdx: Integer): Integer;
begin
  Result := Pred(BandStarts.Count);
  while (Result > -1) and (Integer(BandStarts[Result]) > ItemIdx) do
    Dec(Result);
end;

function TJvCustomInspector.GetBandStarts: TList;
begin
  if FBandStartsNoSB.Count > (ClientWidth div BandWidth) then
    Result := FBandStartsSB
  else
    Result := FBandStartsNoSB;
end;

function TJvCustomInspector.GetBandWidth: Integer;
begin
  Result := FBandWidth;
end;

function TJvCustomInspector.GetBeforeItemCreate: TInspectorItemBeforeCreateEvent;
begin
  Result := FBeforeItemCreate;
end;

function TJvCustomInspector.GetBeforeSelection: TInspectorItemBeforeSelectEvent;
begin
  Result := FBeforeSelection;
end;

function TJvCustomInspector.GetButtonRect(const ItemIndex: Integer): TRect;
var
  Item: TJvCustomInspectorItem;
begin
  // retrieve item
  Item := VisibleItems[ItemIndex];

  // retrieve button rectangle
  if Item.Expanded or Item.HasViewableItems then
    Result := Item.Rects[iprBtnDstRect]
  else
    Result := Rect(0, 0, 0, 0);
end;

function TJvCustomInspector.GetCollapseButton: TBitmap;
begin
  Result := FCollapseButton;
end;

function TJvCustomInspector.GetDivider: Integer;
begin
  Result := FDivider;
end;

function TJvCustomInspector.GetDividerAbs: Integer;
begin
  if RelativeDivider then
  begin
    if UseBands then
      Result := (FDivider * BandWidth) div 100
    else if HandleAllocated then
      Result := (FDivider * ClientWidth) div 100
    else
      Result := (FDivider * Width) div 100;
  end
  else
    Result := FDivider;
end;

function TJvCustomInspector.GetExpandButton: TBitmap;
begin
  Result := FExpandButton;
end;

function TJvCustomInspector.GetImageHeight: Integer;
begin
  if FImageHeight = 0 then
    CalcImageHeight;
  Result := FImageHeight;
end;

function TJvCustomInspector.GetItemHeight: Integer;
begin
  Result := FItemHeight;
end;

function TJvCustomInspector.GetLastFullVisible: Integer;
begin
  Result := YToIdx (IdxToY(TopIndex) + Pred(ClientHeight));
  if Result < 0 then
    Result := Pred(VisibleCount)
  else
  begin
    while (IdxToY(Result) + VisibleItems[Result].Height) > ClientHeight do
      Dec(Result);
  end;
end;

function TJvCustomInspector.GetLockCount: Integer;
begin
  Result := FLockCount;
end;

function TJvCustomInspector.GetRelativeDivider: Boolean;
begin
  Result := FRelativeDivider;
end;

function TJvCustomInspector.GetRoot: TJvCustomInspectorItem;
begin
  Result := FRoot;
end;

function TJvCustomInspector.GetOnItemSelected: TNotifyEvent;
begin
  Result := FOnItemSelected;
end;

function TJvCustomInspector.GetPainter: TJvInspectorPainter;
begin
  Result := FPainter;
end;

function TJvCustomInspector.GetReadonly: Boolean;
begin
  Result := FReadonly;
end;

function TJvCustomInspector.GetSelected: TJvCustomInspectorItem;
begin
  if (SelectedIndex > -1) and (SelectedIndex < VisibleCount) then
    Result := VisibleItems[SelectedIndex]
  else
    Result := nil;
end;

function TJvCustomInspector.GetSelectedIndex: Integer;
begin
  Result := FSelectedIndex;
end;

function TJvCustomInspector.GetTopIndex: Integer;
begin
  Result := FTopIndex;
end;

function TJvCustomInspector.GetUseBands: Boolean;
begin
  Result := FUseBands;
end;

function TJvCustomInspector.GetVisibleCount: Integer;
begin
  Result := FVisible.Count;
end;

function TJvCustomInspector.GetVisibleItems(
  const I: Integer): TJvCustomInspectorItem;
begin
  if I < 0 then
    Result := nil
  else
    Result := TJvCustomInspectorItem(FVisible.Objects[I]);
end;

function TJvCustomInspector.GetWantTabs: Boolean;
begin
  Result := FWantTabs;
end;

procedure TJvCustomInspector.HandleBandResize(X: Integer);
var
  BSize: Integer;
begin
  BSize := X div Succ(BandSizingBand);
  if BSize < 100 then
    BSize := 100;
  BandWidth := BSize;
end;

function TJvCustomInspector.IdxToY(const Index: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Pred(Index) do
    Inc(Result, VisibleItems[I].Height);
end;

procedure TJvCustomInspector.IncPaintGeneration;
begin
  Inc(FPaintGen);
end;

procedure TJvCustomInspector.InvalidateHeight;
begin
  FImageHeight := 0;
  if not BandSizing then
    TopIndex := TopIndex; // Adapt position
end;

procedure TJvCustomInspector.InvalidateItem;
begin
  if (LockCount = 0) and HandleAllocated then
    UpdateScrollbars
  else if not NeedRebuild then
    NeedRedraw := True;
end;

procedure TJvCustomInspector.InvalidateList;
begin
  if not (csDestroying in ComponentState) and (LockCount = 0) then
  begin
    if HandleAllocated then
    begin
      RebuildVisible;
      UpdateScrollbars;
    end;
  end
  else
    NeedRebuild := True;
end;

procedure TJvCustomInspector.KeyDown(var Key: Word; Shift: TShiftState);
var
  Item: TJvCustomInspectorItem;
  IgnoreKey: Boolean;
  TmpH: Integer;
  TmpIdx: Integer;
begin
  Item := Selected;
  if Shift = [] then
  begin
    IgnoreKey := True;
    case Key of
      VK_UP:
        if SelectedIndex > 0 then
          SelectedIndex := SelectedIndex - 1;
      VK_DOWN:
        if SelectedIndex < Pred(VisibleCount) then
          SelectedIndex := SelectedIndex + 1;
      VK_PRIOR:
        begin
          if SelectedIndex > TopIndex  then
            SelectedIndex := TopIndex
          else if SelectedIndex > 0 then
          begin
            TmpH := VisibleItems[Pred(SelectedIndex)].Height;
            TmpIdx := YToIdx(IdxToY(SelectedIndex) + TmpH - ClientHeight);
            if TmpIdx < 0 then
              TmpIdx := 0;
            SelectedIndex := TmpIdx;
          end;
        end;
      VK_NEXT:
        begin
          TmpIdx := GetLastFullVisible;
          if SelectedIndex < TmpIdx then
            SelectedIndex := TmpIdx
          else if SelectedIndex < Pred(VisibleCount) then
          begin
            TmpH := VisibleItems[SelectedIndex].Height;
            TmpIdx := YToIdx(IdxToY(SelectedIndex) + TmpH + ClientHeight);
            if TmpIdx < 0 then
              TmpIdx := Pred(VisibleCount);
            SelectedIndex := TmpIdx;
          end;
        end;
      VK_TAB:
        if WantTabs then
        begin
          if Item is TJvInspectorCustomCompoundItem then
          with Item as TJvInspectorCustomCompoundItem do
          begin
            if SelectedColumnIndex < Pred(ColumnCount) then
              SelectedColumnIndex := SelectedColumnIndex + 1
            else if SelectedIndex < Pred(VisibleCount) then
              SelectedIndex := SelectedIndex + 1;
          end
          else if SelectedIndex < Pred(VisibleCount) then
            SelectedIndex := SelectedIndex + 1;
          if Item <> Selected then
          begin
            if Selected is TJvInspectorCustomCompoundItem then
              TJvInspectorCustomCompoundItem(Selected).SelectedColumnIndex := 0;
          end;
        end;
      VK_ADD:
        if Item.HasViewableItems and not Item.Expanded then
          Item.Expanded := True;
      VK_SUBTRACT:
        if Item.Expanded then
          Item.Expanded := False;
      else
        IgnoreKey := False;
    end;
    if IgnoreKey then
      Key := 0;
  end
  else if Shift = [ssShift] then
  begin
    IgnoreKey := True;
    case Key of
      VK_TAB:
        if WantTabs then
        begin
          if Item is TJvInspectorCustomCompoundItem then
          with Item as TJvInspectorCustomCompoundItem do
          begin
            if SelectedColumnIndex > 0 then
              SelectedColumnIndex := SelectedColumnIndex - 1
            else if SelectedIndex > 0 then
              SelectedIndex := SelectedIndex - 1;
          end
          else if SelectedIndex > 0 then
            SelectedIndex := SelectedIndex - 1;
          if Item <> Selected then
          begin
            if Selected is TJvInspectorCustomCompoundItem then
              TJvInspectorCustomCompoundItem(Selected).SelectedColumnIndex := TJvInspectorCustomCompoundItem(Selected).ColumnCount - 1;
          end;
        end;
      else
        IgnoreKey := True;
    end;
    if IgnoreKey then
      Key := 0;
  end;
  inherited KeyDown(Key, Shift);
  if (SelectedIndex >= 0) and
    (SelectedIndex < VisibleCount) then
  begin
    Item := Selected;
    if (Item <> nil) and Item.Editing then
    begin
      Item.ScrollInView;
      Item.EditKeyDown(Self, Key, Shift);
    end;
  end;
end;

procedure TJvCustomInspector.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if ((Shift = []) and ((Key = VK_DOWN) or (Key = VK_UP) or (Key = VK_ADD) or (Key = VK_SUBTRACT) or (Key = VK_PRIOR) or (Key = VK_NEXT))) or ((Key = VK_TAB) and WantTabs) then
    Key := 0;
end;

procedure TJvCustomInspector.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  BWidth: Integer;
  BandIdx: Integer;
  XB: Integer;
  ItemIndex: Integer;
  ItemRect: TRect;
  Item: TJvCustomInspectorItem;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if UseBands then
  begin
    BWidth := BandWidth;
    BandIdx := X div BWidth + BandStarts.IndexOf(Pointer(TopIndex));
  end
  else
  begin
    BWidth := ClientWidth;
    BandIdx := -1;
  end;
  XB := X mod BWidth;
  ItemIndex := CalcItemIndex(X, Y, ItemRect);
  if ItemIndex < VisibleCount then
    Item := VisibleItems[ItemIndex]
  else
    Item := nil;
  if not Focused and ((Item = nil) or (not Item.Editing)) then
    SetFocus
  else if (Item <> nil) and (Item.Editing) then
    Item.SetFocus;
  if Button = mbLeft then
  begin
    // Check divider dragging
    if (XB >= Pred(DividerAbs)) and (XB <= Succ(DividerAbs)) then
      DraggingDivider := True
    // Check row sizing
    else if (ItemIndex < VisibleCount) and (Y >= Pred(ItemRect.Bottom)) and
        (Y <= Succ(ItemRect.Bottom)) and (Item.RowSizing.SizingFactor <> irsNoReSize) and
        Item.RowSizing.Sizable then
    begin
      RowSizing := True;
      RowSizingItem := Item;
    end
    // Check band sizing
    else if (UseBands and (XB >= BWidth - 3)) and (not UseBands or
      (BandIdx < BandStarts.Count)) then
    begin
      BandSizing := True;
      BandSizingBand := BandIdx - BandStarts.IndexOf(Pointer(TopIndex));
    end
    // Check selecting
    else if (ItemIndex < VisibleCount) and
      (ItemIndex <> SelectedIndex) then
    begin
      SelectedIndex := ItemIndex;
      Item := VisibleItems[ItemIndex];
    end;
    if not DraggingDivider and not RowSizing and not BandSizing then
      Selecting := True;
    if (Item <> nil) and
      ((Item.HasViewableItems and not (iifExpanded in Item.Flags)) or
        (iifExpanded in Item.Flags)) then
    begin
      if PtInRect(Item.Rects[iprBtnDstRect], Point(X, Y)) or
        ((ssDouble in Shift) and (XB < Pred(DividerAbs))) then
      begin
        Item.Expanded := not Item.Expanded;
        Selecting := False;
      end;
    end;
    if (Item <> nil) and (PtInRect(Item.Rects[iprNameArea], Point(X, Y)) or
        PtInRect(Item.Rects[iprValueArea], Point(X, Y))) then
      Item.MouseDown(Button, Shift, X, Y);
  end;
end;

procedure TJvCustomInspector.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  BWidth: Integer;
  BandIdx: Integer;
  XB: Integer;
  ItemIndex: Integer;
  ItemRect: TRect;
  Item: TJvCustomInspectorItem;
begin
  inherited MouseMove(Shift, X, Y);
  if UseBands then
  begin
    BWidth := BandWidth;
    BandIdx := X div BWidth + BandStarts.IndexOf(Pointer(TopIndex));
  end
  else
  begin
    BWidth := ClientWidth;
    BandIdx := -1;
  end;
  XB := X mod BWidth;
  if DraggingDivider then
    DividerAbs := XB
  else if BandSizing then
    HandleBandResize(X)
  else if (((XB >= Pred(DividerAbs)) and (XB <= Succ(DividerAbs))) or
      (UseBands and (XB >= BWidth - 3))) and (not UseBands or
      (BandIdx < BandStarts.Count)) then
    Cursor := crHSplit
  else
  begin
    Cursor := crDefault;
    ItemIndex := CalcItemIndex(X, Y, ItemRect);
    if RowSizing then
    begin
      if RowSizingItem <> nil then
      begin
        ItemRect := CalcItemRect(RowSizingItem);
        RowSizingItem.Height := Y - ItemRect.Top
      end;
    end
    else if Selecting then
    begin
      if (ItemIndex < VisibleCount) and
        (ItemIndex <> SelectedIndex) then
      begin
        if ItemIndex < 0 then
          ItemIndex := SelectedIndex;
        SelectedIndex := ItemIndex;
      end;
      if ItemIndex < VisibleCount then
        Item := VisibleItems[ItemIndex]
      else
        Item := nil;
      if Item <> nil then
        Item.MouseMove(Shift, X, Y);
    end
    else
    begin
      if (ItemIndex < VisibleCount) and (ItemIndex > -1) then
        Item := VisibleItems[ItemIndex]
      else
        Item := nil;
      if (Item <> nil) and (Y >= Pred(ItemRect.Bottom)) and
          (Y <= Succ(ItemRect.Bottom)) and (Item.RowSizing.SizingFactor <> irsNoReSize) and
          Item.RowSizing.Sizable then
        Cursor := crVSplit
      else
        Cursor := crDefault;
    end;
  end
end;

procedure TJvCustomInspector.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  ItemIndex: Integer;
  ItemRect: TRect;
  Item: TJvCustomInspectorItem;
begin
  inherited MouseUp(Button, Shift, X, Y);
  ItemIndex := CalcItemIndex(X, Y, ItemRect);
  if ItemIndex < VisibleCount then
    Item := VisibleItems[ItemIndex]
  else
    Item := nil;
  if Button = mbLeft then
  begin
    if DraggingDivider then
      DraggingDivider := False
    else if RowSizing then
      RowSizing := False
    else if BandSizing then
    begin
      BandSizing := False;
      TopIndex := TopIndex; // resync position
    end
    else if Selecting then
      Selecting := False;
  end;
  if (Item <> nil) and (PtInRect(Item.Rects[iprNameArea], Point(X, Y)) or
      PtInRect(Item.Rects[iprValueArea], Point(X, Y))) then
    Item.MouseUp(Button, Shift, X, Y);
end;

procedure TJvCustomInspector.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = Painter) then
    FPainter := nil;
end;

procedure TJvCustomInspector.NotifySort(const Item: TJvCustomInspectorItem);
begin
  if LockCount = 0 then
    Item.Sort
  else if (Item <> nil) and (SortNotificationList.IndexOf(Item) = -1) then
    SortNotificationList.Add(Item);
end;

procedure TJvCustomInspector.Paint;
begin
  if Painter <> nil then
  begin
    IncPaintGeneration;
    Painter.Setup(Canvas);
    Painter.Paint;
  end;
end;

function ListCompare(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := CompareText(List[Index1], List[Index2]);
end;

procedure TJvCustomInspector.RebuildVisible;
var
  OldSel: TJvCustomInspectorItem;
  Item: TJvCustomInspectorItem;
  ItemStack: TStack;
begin
  FImageHeight := 0;
  OldSel := Selected;
  FVisible.Clear;
  Item := Root;
  ItemStack := TStack.Create;
  while (Item <> nil) do
  begin
    if not Item.Hidden then
      FVisible.AddObject(Item.GetSortName, Item);
    if Item.Visible and Item.Expanded and (Item.Count > 0) then
    begin
      ItemStack.Push(Item);
      Item := Item.Items[0];
    end
    else
    begin
      Item := Item.GetNextSibling;
      while (Item = nil) and (ItemStack.Count > 0) do
      begin
        Item := TJvCustomInspectorItem(ItemStack.Pop);
        Item := Item.GetNextSibling;
      end;
    end;
  end;
  TStringList(FVisible).CustomSort(ListCompare);
  if OldSel <> nil then
    SelectedIndex := FVisible.IndexOfObject(OldSel);
  CalcImageHeight;
end;

procedure TJvCustomInspector.RemoveNotifySort(const Item: TJvCustomInspectorItem);
begin
  SortNotificationList.Remove(Item);
end;

procedure TJvCustomInspector.Resize;
begin
  inherited Resize;
  if not BandSizing then
  begin
    FImageHeight := 0; // Force recalculation of bands
    if (ImageHeight <= ClientHeight) and UseBands then
      TopIndex := 0
    else
      TopIndex := TopIndex;
  end;
  if HandleAllocated then
    UpdateScrollbars;
end;

function TJvCustomInspector.ScrollfactorV: Extended;
begin
  if ClientHeight > 32767 then
		Result := ClientHeight / 32767
	else
		Result := 1;
end;

procedure TJvCustomInspector.SetAfterDataCreate(
  const Value: TInspectorDataEvent);
begin
  FAfterDataCreate := Value;
end;

procedure TJvCustomInspector.SetAfterItemCreate(const Value: TInspectorItemEvent);
begin
  FAfterItemCreate := Value;
end;

procedure TJvCustomInspector.SetBandWidth(Value: Integer);
begin
  if Value <> BandWidth then
  begin
    FBandWidth := Value;
    if not RelativeDivider then
      DividerAbs := DividerAbs;
    if HandleAllocated then
    begin
      CalcImageHeight;
      UpdateScrollbars;
    end;
  end;
end;

procedure TJvCustomInspector.SetBeforeItemCreate(
  const Value: TInspectorItemBeforeCreateEvent);
begin
  FBeforeItemCreate := Value;
end;

procedure TJvCustomInspector.SetBeforeSelection(const Value: TInspectorItemBeforeSelectEvent);
begin
  FBeforeSelection := Value;
end;

procedure TJvCustomInspector.SetCollapseButton(const Value: TBitmap);
begin
  if Value = nil then
    FreeAndNil(FCollapseButton)
  else if not Assigned(FCollapseButton) then
  begin
    FCollapseButton := TBitmap.Create;
    FCollapseButton.Assign(Value);
  end;
  if HandleAllocated then
    UpdateScrollbars;
end;

procedure TJvCustomInspector.SetDivider(Value: Integer);
begin
  if FDivider <> Value then
  begin
    if RelativeDivider then
    begin
      if UseBands then
        DividerAbs := (Value * BandWidth) div 100
      else if HandleAllocated then
        DividerAbs := (Value * ClientWidth) div 100
      else
        DividerAbs := (Value * Width) div 100;
    end
    else
      DividerAbs := Value;
  end;
end;

procedure TJvCustomInspector.SetDividerAbs(Value: Integer);
var
  W: Integer;
begin
  if UseBands then
    W := BandWidth
  else if HandleAllocated then
    W := ClientWidth
  else
    W := Width;
  if Value > (W - 2 * ItemHeight) then
    Value := W - 2 * ItemHeight;
  if Value < (2 * ItemHeight) then
    Value := 2 * ItemHeight;
{  if DividerAbs <> Value then
  begin}
    if RelativeDivider then
    begin
      if UseBands then
        FDivider := (Value * 100) div BandWidth
      else if HandleAllocated then
        FDivider := (Value * 100) div ClientWidth
      else
        FDivider := (Value * 100) div Width;
    end
    else
      FDivider := Value;
    if HandleAllocated then
      UpdateScrollbars;
//  end;
end;

procedure TJvCustomInspector.SetExpandButton(const Value: TBitmap);
begin
  if Value = nil then
    FreeAndNil(FExpandButton)
  else if not Assigned(FExpandButton) then
  begin
    FExpandButton := TBitmap.Create;
    FExpandButton.Assign(Value);
  end;
  if HandleAllocated then
    UpdateScrollbars;
end;

procedure TJvCustomInspector.SetItemHeight(Value: Integer);
begin
  if Value <> ItemHeight then
  begin
    FItemHeight := Value;
    if HandleAllocated then
      UpdateScrollbars;
  end;
end;

procedure TJvCustomInspector.SetLockCount(const Value: Integer);
begin
  if Value <> LockCount then
  begin
    FLockCount := Value;
    if LockCount = 0 then
    begin
      if NeedRebuild then
        InvalidateList
      else
        InvalidateItem;
    end;
  end;
end;

procedure TJvCustomInspector.SetOnItemSelected(const Value: TNotifyEvent);
begin
  FOnItemSelected := Value;
end;

procedure TJvCustomInspector.SetPainter(const Value: TJvInspectorPainter);
begin
  if (Value <> Painter) then
  begin
    if Painter <> nil then
      Painter.RemoveFreeNotification(Self);
    FPainter := Value;
    if Painter <> nil then
    begin
      Painter.SetInspector(Self);
      Painter.FreeNotification(Self);
    end;
    if HandleAllocated then
      UpdateScrollbars;
  end;
end;

procedure TJvCustomInspector.SetReadonly(const Value: Boolean);
begin
  FReadonly := Value;
end;

procedure TJvCustomInspector.SetRelativeDivider(Value: Boolean);
var
  OrgPos: Integer;
begin
  if Value <> RelativeDivider then
  begin
    OrgPos := DividerAbs;
    FRelativeDivider := Value;
    DividerAbs := OrgPos;
  end;
end;

procedure TJvCustomInspector.SetSelected(const Value: TJvCustomInspectorItem);
var
  Idx: Integer;
begin
  Idx := FVisible.IndexOfObject(Value);
  if Idx > -1 then
    SelectedIndex := Idx;
end;

procedure TJvCustomInspector.SetSelectedIndex(Value: Integer);
var
  NewItem: TJvCustomInspectorItem;
begin
  if Value >= VisibleCount then
    Value := Pred(VisibleCount);
  if Value < -1 then
    Value := -1;
  if Value <> SelectedIndex then
  begin
    if Value > -1 then
      NewItem := VisibleItems[Value]
    else
      NewItem := nil;
    if DoBeforeItemSelect(NewItem) then
    begin
      if Selected <> nil then
        Selected.DoneEdit(False);
      FSelectedIndex := Value;
      if Selected <> nil then
      begin
        Selected.ScrollInView;
        Selected.InitEdit;
      end;
      DoItemSelected;
      InvalidateItem;
    end;
  end;
end;

procedure TJvCustomInspector.SetTopIndex(Value: Integer);
var
  MaxIdx: Integer;
begin
  if UseBands then
  begin
    MaxIdx := BandStarts.Count - (ClientWidth div BandWidth);
    if MaxIdx < 0 then
      MaxIdx := 0;
    MaxIdx := Integer(BandStarts[MaxIdx]);
  end
  else
    MaxIdx := Succ(YToIdx(ImageHeight - ClientHeight));
  if MaxIdx < 0 then
    MaxIdx := 0;
  if Value > MaxIdx then
    Value := MaxIdx;
  if Value < 0 then
    Value := 0;
  if UseBands and (BandStarts.IndexOf(Pointer(Value)) > -1) then
  begin
    MaxIdx := Pred(BandStarts.Count);
    while (MaxIdx > -1) and (Integer(BandStarts[MaxIdx]) > Value) do
      Dec(MaxIdx);
    Assert(MaxIdx > -1);
    Value := Integer(BandStarts[MaxIdx]);
  end;
  if TopIndex <> Value then
  begin
    FTopIndex := Value;
    if HandleAllocated then
      UpdateScrollbars;
  end;
end;

procedure TJvCustomInspector.SetUseBands(Value: Boolean);
begin
  if UseBands <> Value then
  begin
    FUseBands := Value;
    if not RelativeDivider then
      DividerAbs := DividerAbs;
    FImageHeight := 0;
    if HandleAllocated then
      UpdateScrollbars;
  end;
end;

procedure TJvCustomInspector.SetWantTabs(Value: Boolean);
begin
  if Value <> WantTabs then
  begin
    FWantTabs := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomInspector.UpdateScrollBars;
var
  DrawHeight: Integer;
  ClHeight: Integer;
  ScFactor: Extended;
	ScrollInfo: TScrollInfo;
  BCount: Integer;
  BPerPage: Integer;
begin
  if not UseBands then
  begin
  	ShowScrollBar(Handle, SB_HORZ, False);
    // Cache the image height, client height and scroll factor
    DrawHeight := ImageHeight;
    ClHeight := ClientHeight;
    ScFactor := ScrollfactorV;
    { Needed to redisplay the scrollbar after it's hidden in the CloseUp method
      of an enumerated item's combobox }
  	ShowScrollBar(Handle, SB_VERT, Round((DrawHeight) / ScFactor) >=
      Round(ClHeight / ScFactor));
  	ScrollInfo.cbSize := SizeOf(ScrollInfo);
	  ScrollInfo.fMask := SIF_ALL;
  	ScrollInfo.nMin := 0;
	  ScrollInfo.nMax := Round((DrawHeight) / ScFactor);
  	ScrollInfo.nPage := Round(ClHeight / ScFactor);
  	ScrollInfo.nPos := Round(IdxToY(TopIndex) / ScFactor);
	  ScrollInfo.nTrackPos := 0;
  	SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
  end
  else
  begin
  	ShowScrollBar(Handle, SB_VERT, False);
    { Needed to redisplay the scrollbar after it's hidden in the CloseUp method
      of an enumerated item's combobox }
    BCount := BandStarts.Count;
    BPerPage := ClientWidth div BandWidth;
  	ShowScrollBar(Handle, SB_HORZ, BCount > BPerPage);
  	ScrollInfo.cbSize := SizeOf(ScrollInfo);
	  ScrollInfo.fMask := SIF_ALL;
  	ScrollInfo.nMin := 0;
	  ScrollInfo.nMax := BCount - 1;
  	ScrollInfo.nPage := BPerPage;
  	ScrollInfo.nPos := GetBandFor(TopIndex);
	  ScrollInfo.nTrackPos := 0;
  	SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
  end;
  Invalidate;
end;

function TJvCustomInspector.ViewHeight: Integer;
begin
  Result := HeightOf(ViewRect);
end;

function TJvCustomInspector.ViewRect: TRect;
begin
  Result := ClientRect;
end;

function TJvCustomInspector.ViewWidth: Integer;
begin
  Result := WidthOf(ViewRect);
end;

procedure TJvCustomInspector.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := DLGC_WANTARROWS;
  if WantTabs then
    Msg.Result := Msg.Result + DLGC_WANTTAB;
end;

procedure TJvCustomInspector.WMHScroll(var Message: TWMScroll);
var
  CurBand: Integer;
  Delta: Integer;
begin
  CurBand := BandStarts.IndexOf(Pointer(TopIndex));
  case Message.ScrollCode of
    SB_BOTTOM:        Delta := BandStarts.Count - 1 - CurBand;
    SB_ENDSCROLL:     Delta := 0;
    SB_LINEDOWN:      Delta := 1;
    SB_LINEUP:        Delta := -1;
    SB_PAGEDOWN:      Delta := ClientWidth div bandWidth;
    SB_PAGEUP:        Delta := -ClientWidth div bandWidth;
    SB_THUMBPOSITION: Delta := Message.Pos - CurBand;
    SB_THUMBTRACK:    Delta := Message.Pos - CurBand;
    SB_TOP:           Delta := -CurBand;
    else              Delta := 0;
  end;
  Curband := CurBand + Delta;
  if CurBand < 0 then
    CurBand := 0;
  if CurBand >= BandStarts.Count then
    CurBand := BandStarts.Count - 1;
  TopIndex := Integer(BandStarts[CurBand]);
end;

procedure TJvCustomInspector.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if Selected <> nil then
    Selected.SetFocus;
end;

procedure TJvCustomInspector.WMVScroll(var Message: TWMScroll);
var
  Delta: Integer;
  ScFactor: Extended;
begin
  Delta := 0;
  ScFactor := ScrollfactorV;
  case Message.ScrollCode of
    SB_BOTTOM:        Delta := ImageHeight - ClientHeight - IdxToY(TopIndex);
    SB_ENDSCROLL:     Delta := 0;
    SB_LINEDOWN:      TopIndex := TopIndex + 1;
    SB_LINEUP:        TopIndex := TopIndex - 1;
    SB_PAGEDOWN:      Delta := ClientHeight;
    SB_PAGEUP:        Delta := -ClientHeight;
    SB_THUMBPOSITION: Delta := Round(Message.Pos * ScFactor) - IdxToY(TopIndex);
    SB_THUMBTRACK:    Delta := Round(Message.Pos * ScFactor) - IdxToY(TopIndex);
    SB_TOP:           Delta := -IdxToY(TopIndex);
    else              Delta := 0;
  end;
  if Delta <> 0 then
    TopIndex := YToIdx(IdxToY(TopIndex) + Delta);
end;

function TJvCustomInspector.YToIdx(const Y: Integer): Integer;
var
  CurY: Integer;
begin
  Result := 0;
  CurY := 0;
  while (Result < VisibleCount) and (Y > (CurY + VisibleItems[Result].Height)) do
  begin
    Inc(CurY, VisibleItems[Result].Height);
    Inc(Result);
  end;
  if Result >= VisibleCount then
    Result := -1;
end;

constructor TJvCustomInspector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBandStartsNoSB := TList.Create;
  FBandStartsSB := TList.Create;
  FSortNotificationList := TList.Create;
  FItemHeight := 16;
  DoubleBuffered := True;
  FVisible := TStringList.Create;
  FRoot := TJvCustomInspectorItem.Create(nil, nil);
  Root.SetInspector(Self);
  Root.Flags := [iifHidden, iifExpanded, iifReadonly, iifVisible];
  FSelectedIndex := -1;
  BevelKind := bkTile;
  BevelInner := bvNone;
  BevelOuter := bvLowered;
  TabStop := True;
  Width := 300;
  Height := 100;
  Divider := 75;
  BandWidth := 150;
end;

destructor TJvCustomInspector.Destroy;
begin
  inherited Destroy;
  FRoot.Free;
  FBandStartsSB.Free;
  FBandStartsNoSB.Free;
  FSortNotificationList.Free;
  Painter := nil;
end;

function TJvCustomInspector.BeginUpdate: Integer;
begin
  Inc(FLockCount);
  Result := FLockCount;
end;

function TJvCustomInspector.EndUpdate: Integer;
var
  I: Integer;
begin
  if LockCount > 0 then
    Dec(FLockCount);
  Result := LockCount;
  if Result = 0 then
  begin
    I := 0;
    FLockCount := -1; // Keep InvalidateSort from calling InvalidateList
    try
      while I < SortNotificationList.Count do
      begin
        TJvCustomInspectorItem(SortNotificationList[I]).InvalidateSort;
        Inc(I);
      end;
    finally
      FLockCount := 0;
      if SortNotificationList.Count > 0 then
        NeedRebuild := True;
      if NeedRebuild then
        InvalidateList
      else
        InvalidateItem;
      SortNotificationList.Clear;
    end;
  end;
end;

function TJvCustomInspector.Focused: Boolean;
begin
  Result := inherited Focused or ((Selected <> nil) and Selected.EditFocused);
end;

function TJvCustomInspector.FocusedItem: TJvCustomInspectorItem;
begin
  Result := Selected;
  if (Result <> nil) and (Result is TJvInspectorCustomCompoundItem) then
  begin
    with (Result as TJvInspectorCustomCompoundItem) do
      if SelectedColumn <> nil then
        Result := SelectedColumn.Item;
  end;
end;

function TJvCustomInspector.VisibleIndex(
  const AItem: TJvCustomInspectorItem): Integer;
begin
  Result := FVisible.IndexOfObject(AItem);
end;

{ TJvInspectorPainter }

procedure TJvInspectorPainter.ApplyNameFont;
begin
  Canvas.Font := Inspector.Font;
  if Item is TJvInspectorCustomCategoryItem then
    Canvas.Font.Color := CategoryTextColor
  else
    Canvas.Font.Color := NameColor;
end;

procedure TJvInspectorPainter.ApplyValueFont;
begin
  Canvas.Font := Inspector.Font;
  Canvas.Font.Color := ValueColor;
end;

procedure TJvInspectorPainter.CalcButtonBasedRects;
begin
end;

procedure TJvInspectorPainter.CalcEditBasedRects;
begin
end;

procedure TJvInspectorPainter.CalcNameBasedRects;
begin
end;

procedure TJvInspectorPainter.CalcValueBasedRects;
begin
end;

function TJvInspectorPainter.DividerWidth: Integer;
begin
  Result := 1;
end;

procedure TJvInspectorPainter.DoPaint;
begin
end;

function TJvInspectorPainter.GetBackgroundColor: TColor;
begin
  Result := FBackgroundColor;
end;

function TJvInspectorPainter.GetCategoryColor: TColor;
begin
  Result := FCategoryColor;
end;

function TJvInspectorPainter.GetCategoryTextColor: TColor;
begin
  Result := FCategoryTextColor;
end;

function TJvInspectorPainter.GetCollapseImage: TBitmap;
begin
  if Assigned(Inspector.CollapseButton) then
    Result := Inspector.CollapseButton
  else
    Result := FInternalCollapseButton;
end;

function TJvInspectorPainter.GetDividerColor: TColor;
begin
  Result := FDividerColor;
end;

function TJvInspectorPainter.GetExpandImage: TBitmap;
begin
  if Assigned(Inspector.ExpandButton) then
    Result := Inspector.ExpandButton
  else
    Result := FInternalExpandButton;
end;

function TJvInspectorPainter.GetNameColor: TColor;
begin
  Result := FNameColor;
end;

function TJvInspectorPainter.GetNameHeight(
  const AItem: TJvCustomInspectorItem): Integer;
var
  TmpCanvas: TCanvas;
begin
  TmpCanvas := Canvas;
  try
    Canvas := TControlCanvas.Create;
    TControlCanvas(Canvas).Control := Inspector;
    ApplyNameFont;
    Result := Canvas.TextHeight('Wy');
  finally
    if TmpCanvas <> Canvas then
      Canvas.Free;
    Canvas := TmpCanvas;
  end;
end;

function TJvInspectorPainter.GetRects(const Index: TInspectorPaintRect): TRect;
begin
  if Item <> nil then
    Result := Item.Rects[Index]
  else
    Result := Rect(0, 0, 0, 0);
end;

function TJvInspectorPainter.GetSelectedColor: TColor;
begin
  Result := FSelectedColor;
end;

function TJvInspectorPainter.GetSelectedTextColor: TColor;
begin
  Result := FSelectedTextColor;
end;

function TJvInspectorPainter.GetValueColor: TColor;
begin
  Result := FValueColor;
end;

function TJvInspectorPainter.GetValueHeight(
  const AItem: TJvCustomInspectorItem): Integer;
var
  TmpCanvas: TCanvas;
begin
  TmpCanvas := Canvas;
  try
    Canvas := TControlCanvas.Create;
    TControlCanvas(Canvas).Control := Inspector;
    ApplyValueFont;
    Result := Canvas.TextHeight('Wy');
  finally
    if TmpCanvas <> Canvas then
      Canvas.Free;
    Canvas := TmpCanvas;
  end;
end;

procedure TJvInspectorPainter.HideEditor;
begin
  Inspector.Selected.Rects[iprEditValue] := Rect(0, 0, 0, 0);
end;

procedure TJvInspectorPainter.InitializeColors;
begin
  SetDefaultProp(Self, ['BackgroundColor', 'DividerColor', 'NameColor', 'ValueColor', 'CategoryColor',
    'CategoryTextColor', 'SelectedColor', 'SelectedTextColor']);
end;

function TJvInspectorPainter.Loading: Boolean;
begin
  Result := csLoading in ComponentState;
end;

procedure TJvInspectorPainter.Paint;
var
  SelItemVisible: Boolean;
  Rect: TRect;
  ItemIdx: Integer;
  MaxItemIdx: Integer;
  BandIdx: Integer;
  MaxBandItemIdx: Integer;
begin
  SelItemVisible := False;
  Rect := Inspector.ViewRect;
  Canvas.FillRect(Rect);
  ItemIdx := Inspector.TopIndex;
  MaxItemIdx := Inspector.VisibleCount;
  if not Inspector.UseBands then
  begin
    // Loop through the visible list
    while (Rect.Top < Rect.Bottom) and (ItemIdx < MaxItemIdx) do
    begin
      SelItemVisible := SelItemVisible or (ItemIdx = Inspector.SelectedIndex);
      PaintItem(Rect, ItemIdx);
      Inc(ItemIdx);
    end;
  end
  else // if UseBands
  begin
    BandIdx := Inspector.BandStarts.IndexOf(Pointer(ItemIdx));
    Rect.Right := Rect.Left + Inspector.BandWidth - 4;
    while (ItemIdx < MaxItemIdx) and (Rect.Left < Inspector.ClientWidth) do
    begin
      Inc(BandIdx);
      if BandIdx < Inspector.BandStarts.Count then
        MaxBandItemIdx := Integer(Inspector.BandStarts[BandIdx])
      else
        MaxBandItemIdx := MaxItemIdx;
      while (Rect.Top < Rect.Bottom) and (ItemIdx < MaxBandItemIdx) do
      begin
        SelItemVisible := SelItemVisible or (ItemIdx = Inspector.SelectedIndex);
        PaintItem(Rect, ItemIdx);
        Inc(ItemIdx);
      end;
      MaxBandItemIdx := Rect.Right + 4;
      Rect := Inspector.ClientRect;
      Rect.Left := MaxBandItemIdx;
      Rect.Right := Rect.Left + Inspector.BandWidth - 4;
      Canvas.Pen.Color := clBtnShadow;
      Canvas.MoveTo(Rect.Left - 3, Rect.Top);
      Canvas.LineTo(Rect.Left - 3, Rect.Bottom);
      Canvas.MoveTo(Rect.Left - 1, Rect.Top);
      Canvas.LineTo(Rect.Left - 1, Rect.Bottom);
    end;
  end;
  if not SelItemVisible and (Inspector.Selected <> nil) then
    HideEditor;
end;

procedure TJvInspectorPainter.PaintDivider(const X, YTop, YBottom: Integer);
begin
end;

procedure TJvInspectorPainter.PaintItem(var ARect: TRect;
  const AItemIndex: Integer);
var
  OrgState: Integer;
begin
  OrgState := SaveCanvasState(Canvas);
  try
    // Initialize painter variables
    PaintRect := ARect;
    ItemIndex := AItemIndex;
    SetupItem;

    // Do actual painting
    DoPaint;

    // Finalize painting
    TeardownItem;
    ARect := PaintRect;
  finally
    RestoreCanvasState(Canvas, OrgState);
  end;
end;

procedure TJvInspectorPainter.PaintItem(const AItem: TJvCustomInspectorItem);
var
  OrgState: Integer;
begin
  OrgState := SaveCanvasState(Canvas);
  try
    // Initialize painter variables
    ItemIndex := -1;
    Item := AItem;
    SetupItem;

    // Do actual painting
    DoPaint;

    // Finalize painting
    TeardownItem;
  finally
    RestoreCanvasState(Canvas, OrgState);
  end;
end;

procedure TJvInspectorPainter.SetBackgroundColor(const Value: TColor);
begin
  if Value <> BackgroundColor then
  begin
    FBackgroundColor := Value;
    if not Initializing and not Loading then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorPainter.SetCategoryColor(const Value: TColor);
begin
  if Value <> CategoryColor then
  begin
    FCategoryColor := Value;
    if not Initializing and not Loading then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorPainter.SetCategoryTextColor(const Value: TColor);
begin
  if Value <> CategoryTextColor then
  begin
    FCategoryTextColor := Value;
    if not Initializing and not Loading then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorPainter.SetDividerColor(const Value: TColor);
begin
  if DividerColor <> Value then
  begin
    FDividerColor := Value;
    if not Initializing and not Loading then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorPainter.SetNameColor(const Value: TColor);
begin
  if Value <> NameColor then
  begin
    FNameColor := Value;
    if not Initializing and not Loading then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorPainter.SetRects(const Index: TInspectorPaintRect;
  const ARect: TRect);
begin
  if Item <> nil then
    Item.Rects[Index] := ARect;
end;

procedure TJvInspectorPainter.SetSelectedColor(const Value: TColor);
begin
  if Value <> SelectedColor then
  begin
    FSelectedColor := Value;
    if not Initializing and not Loading then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorPainter.SetSelectedTextColor(const Value: TColor);
begin
  if Value <> SelectedTextColor then
  begin
    FSelectedTextColor := Value;
    if not Initializing and not Loading then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorPainter.Setup(const ACanvas: TCanvas);
begin
  Canvas := ACanvas;
  Canvas.Brush.Color := BackgroundColor;
end;

procedure TJvInspectorPainter.SetupItem;
begin
  // retrieve item
  if ItemIndex > -1 then
    Item := Inspector.VisibleItems[ItemIndex];

  if Item <> nil then
  begin
    // retrieve button image
    if Item.Expanded then
      ButtonImage := GetCollapseImage
    else if Item.HasViewableItems then
      ButtonImage := GetExpandImage
    else
      ButtonImage := nil;
  end
  else
      ButtonImage := nil;

  // calculate rectangles
  if ItemIndex > -1 then
    SetupRects;
end;

procedure TJvInspectorPainter.SetupRects;
begin
  Rects[iprItem] := Rect(PaintRect.Left, PaintRect.Top,
    PaintRect.Right, Pred(PaintRect.Top + Item.Height));
end;

procedure TJvInspectorPainter.SetValueColor(const Value: TColor);
begin
  if Value <> ValueColor then
  begin
    FValueColor := Value;
    if not Initializing and not Loading then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorPainter.TeardownItem;
var
  TmpRect: TRect;
begin
  TmpRect := PaintRect;
  TmpRect.Top := Succ(Rects[iprItem].Bottom);
  PaintRect := TmpRect;
  Item := nil;
  ItemIndex := -1;
end;

constructor TJvInspectorPainter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInspector := nil;
  FInternalCollapseButton := TBitmap.Create;
  FInternalExpandButton := TBitmap.Create;
  Initializing := True;
  try
    InitializeColors;
  finally
    Initializing := False;
  end;
  with FInternalCollapseButton do
  begin
    Width := 9;
    Height := 9;
    Canvas.Brush.Color := clWhite;
    Canvas.Pen.Color := clBlack;
    Canvas.Rectangle(0, 0, 9, 9);
    Canvas.MoveTo(2, 4);
    Canvas.LineTo(7, 4);
  end;
  with FInternalExpandButton do
  begin
    Width := 9;
    Height := 9;
    Canvas.Brush.Color := clWhite;
    Canvas.Pen.Color := clBlack;
    Canvas.Rectangle(0, 0, 9, 9);
    Canvas.MoveTo(2, 4);
    Canvas.LineTo(7, 4);
    Canvas.MoveTo(4, 2);
    Canvas.LineTo(4, 7);
  end;
end;

destructor TJvInspectorPainter.Destroy;
begin
  FInternalCollapseButton.Free;
  FInternalExpandButton.Free;
  inherited Destroy;
end;

procedure TJvInspectorPainter.SetInspector(const AInspector: TJvCustomInspector);
begin
  if AInspector.Painter <> Self then
    raise EJvInspector.Create(sJvInspPaintNotActive);
  if AInspector <> Inspector then
  begin
    FInspector := AInspector;
  end;
end;

{ TJvInspectorBorlandNETBasePainter }

procedure TJvInspectorBorlandNETBasePainter.ApplyNameFont;
begin
  inherited ApplyNameFont;
  if Item is TJvInspectorCustomCategoryItem then
    Canvas.Font.Style := Canvas.Font.Style + [fsBold];
end;

procedure TJvInspectorBorlandNETBasePainter.ApplyValueFont;
begin
  inherited ApplyValueFont;
  if Item is TJvInspectorCustomCategoryItem then
    Canvas.Font.Style := Canvas.Font.Style + [fsBold];
end;

procedure TJvInspectorBorlandNETBasePainter.CalcButtonBasedRects;
var
  BtnSrcRect: TRect;
  BtnDstRect: TRect;
  Y: Integer;
begin
  if ButtonImage <> nil then
  begin
    BtnSrcRect := Rect(0, 0, ButtonImage.Width, ButtonImage.Height);
    BtnDstRect := Rect(0, 0, WidthOf(Rects[iprButtonArea]),
      HeightOf(Rects[iprButtonArea]));
    if BtnSrcRect.Right > BtnDstRect.Right then
    begin
      BtnSrcRect.Left := (BtnDstRect.Right - BtnSrcRect.Right) div 2;
      BtnSrcRect.Right := BtnSrcRect.Left + BtnDstRect.Right;
    end;
    if BtnSrcRect.Bottom > BtnDstRect.Bottom then
    begin
      BtnSrcRect.Top := (BtnDstRect.Bottom - BtnSrcRect.Bottom) div 2;
      BtnSrcRect.Bottom := BtnSrcRect.Top + BtnDstRect.Bottom;
    end;
    if BtnDstRect.Right > WidthOf(BtnSrcRect) then
    begin
      BtnDstRect.Left := (BtnDstRect.Right - WidthOf(BtnSrcRect)) div 2;
      BtnDstRect.Right := BtnDstRect.Left + WidthOf(BtnSrcRect);
    end;
    if BtnDstRect.Bottom > HeightOf(BtnSrcRect) then
    begin
      if (HeightOf(BtnDstRect) div Inspector.ItemHeight) < 2 then
        Y := (HeightOf(BtnDstRect) - HeightOf(BtnSrcRect)) div 2
      else
        Y := (Inspector.ItemHeight - HeightOf(BtnSrcRect)) div 2;
      BtnDstRect.Top := Y;
      BtnDstRect.Bottom := BtnDstRect.Top + HeightOf(BtnSrcRect);
    end;
    OffsetRect(BtnDstRect, Rects[iprButtonArea].Left, Rects[iprButtonArea].Top);
  end
  else
  begin
    BtnSrcRect := Rect(0, 0, 0, 0);
    BtnDstRect := Rect(0, 0, 0, 0);
  end;
  Rects[iprBtnSrcRect] := BtnSrcRect;
  Rects[iprBtnDstRect] := BtnDstRect;
end;

procedure TJvInspectorBorlandNETBasePainter.CalcEditBasedRects;
var
  TmpRect: TRect;
begin
  if [iifValueList, iifEditButton] * Item.Flags = [] then
  begin
    Rects[iprEditValue] := Rects[iprValue];
    Rects[iprEditButton] := Rect(0, 0, 0, 0);
  end
  else
  begin
    TmpRect := Rects[iprValue];
    Dec(TmpRect.Right, Inspector.ItemHeight);
    Rects[iprEditValue] := TmpRect;
    TmpRect := Rects[iprValueArea];
    TmpRect.Left := TmpRect.Right - Inspector.ItemHeight;
    Rects[iprEditButton] := TmpRect;
  end;
end;

procedure TJvInspectorBorlandNETBasePainter.CalcNameBasedRects;
var
  CanvasState: Integer;
  RowHeight: Integer;
  TmpRect: TRect;
begin
  CanvasState := SaveCanvasState(Canvas);
  try
    ApplyNameFont;
    RowHeight := Canvas.TextHeight('Wy');
    TmpRect := Rects[iprNameArea];
    if HeightOf(TmpRect) div RowHeight < 2 then
      OffsetRect(TmpRect, 0, (HeightOf(TmpRect) - RowHeight) div 2)
    else
    begin
      Inc(TmpRect.Top, 1);
      Dec(TmpRect.Bottom, 1);
    end;
    IntersectRect(TmpRect, TmpRect, Rects[iprNameArea]);
    Rects[iprName] := TmpRect;
  finally
    RestoreCanvasState(Inspector.Canvas, CanvasState);
  end;
end;

procedure TJvInspectorBorlandNETBasePainter.CalcValueBasedRects;
var
  CanvasState: Integer;
  RowHeight: Integer;
  TmpRect: TRect;
begin
  CanvasState := SaveCanvasState(Canvas);
  try
    ApplyValueFont;
    RowHeight := Canvas.TextHeight('Wy');
    TmpRect := Rects[iprValueArea];
    if HeightOf(TmpRect) div RowHeight < 2 then
    begin
      OffsetRect(TmpRect, 0, (HeightOf(TmpRect) - RowHeight) div 2);
      IntersectRect(TmpRect, TmpRect, Rects[iprValueArea]);
    end
    else
    begin
      Inc(TmpRect.Top, 1);
      Dec(TmpRect.Bottom, 1);
      IntersectRect(TmpRect, TmpRect, Rects[iprValueArea]);
    end;
    Rects[iprValue] := TmpRect;
  finally
    RestoreCanvasState(Inspector.Canvas, CanvasState);
  end;
  CalcEditBasedRects;
end;

procedure TJvInspectorBorlandNETBasePainter.SetupRects;
var
  ItemRect2: TRect;
  TmpRect: TRect;
begin
  inherited SetupRects;
  ItemRect2 := Rects[iprItem];
  Rects[iprButtonArea] := Rect(ItemRect2.Left + (Item.Level *
    Inspector.ItemHeight), ItemRect2.Top, ItemRect2.Left +
    (Succ(Item.Level) * Inspector.ItemHeight), ItemRect2.Bottom);
  TmpRect := ItemRect2;
  TmpRect.Left := Rects[iprButtonArea].Right;
  Rects[iprNameArea] := TmpRect;
  if Item is TJvInspectorCustomCategoryItem then
    Rects[iprValueArea] := Rect(0, 0, 0, 0)
  else
  begin
    TmpRect.Right := ItemRect2.Left + Pred(Inspector.DividerAbs);
    Rects[iprNameArea] := TmpRect;
    TmpRect := ItemRect2;
    TmpRect.Left := ItemRect2.Left + Inspector.DividerAbs + DividerWidth;
    Rects[iprValueArea] := TmpRect;
  end;
  CalcButtonBasedRects;
  CalcNameBasedRects;
  CalcValueBasedRects;
end;

{ TJvInspectorBorlandPainter }

function TJvInspectorBorlandPainter.DividerWidth: Integer;
begin
  Result := 2;
end;

procedure TJvInspectorBorlandPainter.DoPaint;
var
  TmpRect: TRect;
  X: Integer;
  MaxX: Integer;
begin
  TmpRect := Rects[iprItem];
  if Item = Inspector.Selected then
  begin
    // Selected frame
    InflateRect(TmpRect, 0, 1);
    Dec(TmpRect.Top);
    Inc(TmpRect.Right);
    Frame3D(Canvas, TmpRect, clBlack, clWhite, 1);
    Frame3D(Canvas, TmpRect, clBlack, cl3DLight, 1);
  end
  else
  begin
    // Dotted line
    X := TmpRect.Left;
    MaxX := TmpRect.Right;
    Canvas.Pen.Color := clBlack;
    while X < MaxX do
    begin
      Canvas.Pixels[X, TmpRect.Bottom] := clBlack;
      Inc(X, 2);
    end;
  end;

  if not (Item is TJvInspectorCustomCategoryItem) then
  begin
    // Draw divider line
    TmpRect := Rects[iprItem];
    PaintDivider(TmpRect.Left + Inspector.DividerAbs, Pred(TmpRect.Top), TmpRect.Bottom);
  end;

  ApplyNameFont;
  Item.DrawName(Canvas);
  ApplyValueFont;
  Item.DrawValue(Canvas);

  if ButtonImage <> nil then
    Canvas.CopyRect(Rects[iprBtnDstRect], ButtonImage.Canvas, Rects[iprBtnSrcRect]);
end;

function TJvInspectorBorlandPainter.GetDividerLightColor: TColor;
begin
  Result := FDividerLightColor;
end;

function TJvInspectorBorlandPainter.GetSelectedColor: TColor;
begin
  Result := BackgroundColor;
end;

function TJvInspectorBorlandPainter.GetSelectedTextColor: TColor;
begin
  Result := NameColor;
end;

procedure TJvInspectorBorlandPainter.InitializeColors;
begin
  inherited InitializeColors;
  SetDefaultProp(Self, 'DividerLightColor');
end;

procedure TJvInspectorBorlandPainter.PaintDivider(const X, YTop, YBottom: Integer);
begin
  Canvas.Pen.Color := DividerColor;
  Canvas.MoveTo(X, YTop);
  Canvas.LineTo(X, YBottom);
  Canvas.Pen.Color := DividerLightColor;
  Canvas.MoveTo(Succ(X), YBottom);
  Canvas.LineTo(Succ(X), YTop);
end;

procedure TJvInspectorBorlandPainter.SetDividerLightColor(const Value: TColor);
begin
  if DividerLightColor <> Value then
  begin
    FDividerLightColor := Value;
    if not Initializing and not Loading then
      Inspector.Invalidate;
  end;
end;

procedure TJvInspectorBorlandPainter.Setup(const ACanvas: TCanvas);
begin
  inherited Setup(ACanvas);
  Canvas.Brush.Color := clBtnFace;
end;

{ TJvInspectorDotNETPainter }

procedure TJvInspectorDotNETPainter.ApplyNameFont;
begin
  inherited ApplyNameFont;
  if (Item = Inspector.Selected) and not (Item is TJvInspectorCustomCompoundItem) then
  begin
    Canvas.Brush.Color := SelectedColor;
    Canvas.Font.Color := SelectedTextColor;
  end
  else if (Item is TJvInspectorCustomCategoryItem) and
      (Item.Level = 0) then
    Canvas.Brush.Color := CategoryColor
  else
    Canvas.Brush.Color := BackgroundColor;
end;

procedure TJvInspectorDotNETPainter.DoPaint;
var
  EndOfList: Boolean;
  NextItem: TJvCustomInspectorItem;
  EndOfCat: Boolean;
  PreNameRect: TRect;
  CatRect: TRect;
  SaveIdx: Integer;
  LeftX: Integer;
begin
  SaveIdx := SaveCanvasState(Canvas);

  // Determine item type (end of list, end of a level 0 category)
  EndOfList := Succ(ItemIndex) >= Inspector.VisibleCount;
  if not EndOfList then
  begin
    NextItem := Inspector.VisibleItems[Succ(ItemIndex)];
    EndOfCat := (NextItem.BaseCategory <> Item.BaseCategory) and
      (Item.BaseCategory <> nil);
  end
  else
    EndOfCat := Item.BaseCategory <> nil;

  PreNameRect := Rects[iprButtonArea];
  PreNameRect.Left := Rects[iprItem].Left + WidthOf(Rects[iprButtonArea]);
  Inc(PreNameRect.Right);

  CatRect := Rects[iprItem];
  CatRect.Right := CatRect.Left + WidthOf(Rects[iprButtonArea]);
  Inc(CatRect.Bottom);
  if (Item.BaseCategory <> nil) then
  begin
    Canvas.Brush.Color := CategoryColor;
    Canvas.FillRect(CatRect);
    ApplyCanvasState(Canvas, SaveIdx);
  end;
  
  if not (Item is TJvInspectorCustomCategoryItem) or (Item.Level > 0) then
    PaintDivider(Rects[iprItem].Left + Inspector.DividerAbs, Pred(Rects[iprItem].Top),
      Rects[iprItem].Bottom);

  if (Item is TJvInspectorCustomCategoryItem) and (Item.Level = 0) then
    Canvas.Brush.Color := CategoryColor;
  if (Item = Inspector.Selected) and (not (Item is TJvInspectorCustomCompoundItem) or (TJvInspectorCustomCompoundItem(Item).SelectedColumnIndex = 0)) and ((Item.Level > 0) or
      not (Item is TJvInspectorCustomCategoryItem)) then
    Canvas.Brush.Color := SelectedColor;
  Canvas.FillRect(PreNameRect);
  ApplyNameFont;
  Canvas.FillRect(Rects[iprNameArea]);
  Item.DrawName(Canvas);
  ApplyCanvasState(Canvas, SaveIdx);
  ApplyValueFont;
  Item.DrawValue(Canvas);
  RestoreCanvasState(Canvas, SaveIdx);

  if ButtonImage <> nil then
    Canvas.CopyRect(Rects[iprBtnDstRect], ButtonImage.Canvas, Rects[iprBtnSrcRect]);

  SaveIdx := SaveCanvasState(Canvas);
  if EndOfCat or ((Item is TJvInspectorCustomCategoryItem) and
      (Item.Level = 0)) then
    Canvas.Pen.Color := clBtnShadow
  else
    Canvas.Pen.Color := clBtnFace;
  if not EndOfList and not EndOfCat then
    LeftX := Rects[iprItem].Left + WidthOf(Rects[iprButtonArea])
  else
    LeftX := Rects[iprItem].Left;
  Canvas.MoveTo(Rects[iprItem].Right, Rects[iprItem].Bottom);
  Canvas.LineTo(Pred(LeftX), Rects[iprItem].Bottom);

  if Item <> Item.BaseCategory then
  begin
    if Item.BaseCategory <> nil then
      Canvas.Pen.Color := clBtnShadow
    else
      Canvas.Pen.Color := CategoryColor;
    Canvas.MoveTo(Rects[iprItem].Left + WidthOf(Rects[iprButtonArea]), Rects[iprItem].Top);
    Canvas.LineTo(Rects[iprItem].Left + WidthOf(Rects[iprButtonArea]), Succ(Rects[iprItem].Bottom));
  end;
  RestoreCanvasState(Canvas, SaveIdx);
end;

procedure TJvInspectorDotNETPainter.PaintDivider(const X, YTop, YBottom: Integer);
begin
  Canvas.Pen.Color := DividerColor;
  Canvas.MoveTo(X, YTop);
  Canvas.LineTo(X, YBottom);
end;

{ TJvInspectorItemSizing }

function TJvInspectorItemSizing.GetMinHeight: TItemRowSizing;
begin
  Result := FMinHeight;
end;

function TJvInspectorItemSizing.GetSizable: Boolean;
begin
  Result := FSizable;
end;

function TJvInspectorItemSizing.GetSizingFactor: TItemRowSizing;
begin
  Result := FSizingFactor;
end;

procedure TJvInspectorItemSizing.SetMinHeight(Value: TItemRowSizing);
var
  CurHeight: Integer;
begin
  CurHeight := Item.Height;
  if Value = irsNoResize then
  begin
    if SizingFactor <> Value then
      SizingFactor := Value
    else if MinHeight <> irsItemHeight then
    begin
      FMinHeight := irsItemHeight;
      Item.Height := CurHeight;
    end;
  end
  else if MinHeight <> Value then
  begin
    FMinHeight := Value;
    Item.Height := CurHeight;
  end;
end;

procedure TJvInspectorItemSizing.SetSizable(Value: Boolean);
begin
  if Sizable <> Value then
    FSizable := Value;
end;

procedure TJvInspectorItemSizing.SetSizingFactor(Value: TItemRowSizing);
var
  CurHeight: Integer;
begin
  CurHeight := Item.Height;
  if SizingFactor <> Value then
  begin
    FSizingFactor := Value;
    if SizingFactor = irsNoReSize then
      FMinHeight := irsItemHeight
    else
      Item.Height := CurHeight;
  end;
end;

constructor TJvInspectorItemSizing.Create(const AItem: TJvCustomInspectorItem);
begin
  inherited Create;
  Item := AItem;
end;

procedure TJvInspectorItemSizing.Assign(Source: TPersistent);
begin
  if Source is TJvInspectorItemSizing then
  begin
    MinHeight := TJvInspectorItemSizing(Source).MinHeight;
    SizingFactor := TJvInspectorItemSizing(Source).SizingFactor;
  end
  else
    inherited Assign(Source);
end;

{ Item sorting functions }

function AlphaSortCompare(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TJvCustomInspectorItem(Item1).DisplayName,
    TJvCustomInspectorItem(Item2).DisplayName); 
end;

var // maybe a threadvar would be better? OTOH, VCL is not threadsafe anyway so why bother?
  DataSortCompareEvent: TInspectorItemSortCompare;

function DataSortCompare(Item1, Item2: Pointer): Integer;
begin
  if @DataSortCompareEvent <> nil then
    Result := DataSortCompareEvent(Item1, Item2)
  else
    Result := 0;
end;

function DisplayIndexSortCompare(Item1, Item2: Pointer): Integer;
var
  Idx1: Integer;
  Idx2: Integer;
begin
  Idx1 := TJvCustomInspectorItem(Item1).DisplayIndex;
  Idx2 := TJvCustomInspectorItem(Item2).DisplayIndex;
  if (Idx1 <> -1) and (Idx2 <> -1) then
    Result := Idx1 - Idx2
  else
  begin
    if Idx1 = -1 then
      if Idx2 = -1 then
        Result := 0
      else
        Result := 1
    else
      Result := -1;
  end;
end;

{ TJvCustomInspectorItem }

procedure TJvCustomInspectorItem.AlphaSort;
var
  ItemList: TList;
begin
  ItemList := TList.Create;
  try
    BuildDisplayableList(ItemList);
    ItemList.Sort(AlphaSortCompare);
    ApplyDisplayIndices(ItemList);
  finally
    ItemList.Free;
  end;
end;

procedure TJvCustomInspectorItem.Apply;
var
  TmpOnChange: TNotifyEvent;
begin
  if Editing and (EditCtrl <> nil) and (DisplayValue <> EditCtrl.Text) then
  begin
    DisplayValue := EditCtrl.Text;
    InvalidateItem;
    if EditCtrl <> nil then
    begin
      TmpOnChange := TOpenEdit(EditCtrl).OnChange;
      TOpenEdit(EditCtrl).OnChange := nil;
      try
        EditCtrl.Text := DisplayValue;
      finally
        TOpenEdit(EditCtrl).OnChange := TmpOnChange;
      end;
    end;
  end;
  if Editing and (EditCtrl <> nil) then
  begin
    EditCtrl.SelectAll;
    EditCtrl.Modified := False;
    EditCtrl.ClearUndo;
  end;
end;

procedure TJvCustomInspectorItem.ApplyDisplayIndices(const ItemList: TList);
var
  I: Integer;
begin
  for I := ItemList.Count -1 downto 0 do
    TJvCustomInspectorItem(ItemList[I]).SetDisplayIndexValue(I);
end;

procedure TJvCustomInspectorItem.BuildDisplayableList(const ItemList: TList);
var
  TempList: TList;
  I: Integer;
  Item: TJvCustomInspectorItem;
  {$IFNDEF COMPILER6_UP}
  J: Integer;
  {$ENDIF}
begin
  TempList := TList.Create;
  try
    if ItemList.Capacity < 64 then
      ItemList.Capacity := 64; // Avoid small growth steps
    I := 0;
    while I < Count do
    begin
      Item := Items[I];
      if not Item.Hidden then
        ItemList.Add(Item)
      else
      begin
        Item.BuildDisplayableList(TempList);
        {$IFNDEF COMPILER6_UP}
        for J := 0 to TempList.Count - 1 do
          if ItemList.IndexOf(TempList[J]) = -1 then
            ItemList.Add(TempList[J]);
        {$ELSE}
        ItemList.Assign(TempList, laOr);
        {$ENDIF}
        TempList.Clear;
      end;
      Inc(I);
    end;
  finally
    TempList.Free;
  end;
end;

procedure TJvCustomInspectorItem.ButtonClick(Sender: TObject);
begin
  Edit;
end;

function TJvCustomInspectorItem.CanEdit: Boolean;
begin
  Result := not Readonly and not Inspector.Readonly and Data.IsInitialized and Data.HasValue;
end;

procedure TJvCustomInspectorItem.CloseUp(Accept: Boolean);
var
  ListValue: string;
begin
  if DroppedDown then
  begin
  	ShowScrollBar(Inspector.Handle, SB_BOTH, False);
    if GetCapture <> 0 then
      SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    if ListBox.ItemIndex > -1 then
      ListValue := ListBox.Items[ListBox.ItemIndex];
    SetWindowPos(ListBox.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    FDroppedDown := False;
    InvalidateItem;
    if Accept then
    begin
      EditCtrl.Text := ListValue;
      Apply;
    end;
  end;
end;

procedure TJvCustomInspectorItem.DataSort;
var
  ItemList: TList;
begin
  ItemList := TList.Create;
  try
    BuildDisplayableList(ItemList);
    DataSortCompareEvent := OnCompare;
    ItemList.Sort(DataSortCompare);
    ApplyDisplayIndices(ItemList);
  finally
    ItemList.Free;
  end;
end;

procedure TJvCustomInspectorItem.DoAfterItemCreate;
begin
  if Inspector <> nil then
    Inspector.DoAfterItemCreate(Self);
end;

function TJvCustomInspectorItem.DoCompare(const Item: TJvCustomInspectorItem): Integer;
begin
  if @FOnCompare <> nil then
    Result := OnCompare(Self, Item)
  else
    Result := 0;
end;

procedure TJvCustomInspectorItem.DoDrawListItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  with (Control as TListBox) do
    Canvas.TextOut(Rect.Left, Rect.Top, Items[Index]);
end;

procedure TJvCustomInspectorItem.DoDropDownKeys(var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_UP, VK_DOWN:
      if ssAlt in Shift then begin
        if DroppedDown then
          CloseUp(True)
        else
          DropDown;
        Key := 0;
      end;
    VK_RETURN, VK_ESCAPE:
      if DroppedDown and not (ssAlt in Shift) then
      begin
        CloseUp(Key = VK_RETURN);
        Key := 0;
      end;
  end;
end;

procedure TJvCustomInspectorItem.DoGetValueList(const Strings: TStrings);
begin
  if @FOnGetValueList <> nil then
    FOnGetValueList(Self, Strings);
end;

procedure TJvCustomInspectorItem.DoMeasureListItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
begin
end;

procedure TJvCustomInspectorItem.DoMeasureListItemWidth(Control: TWinControl;
  Index: Integer; var Width: Integer);
begin
end;

procedure TJvCustomInspectorItem.DropDown;
const
  MaxListCount = 8;
var
  ListCount: Integer;
  P: TPoint;
  Y: Integer;
  J: Integer;
  I: Integer;
begin
  if not DroppedDown then
  begin
    ListBox.Width := WidthOf(Rects[iprValue]);
    TListBox(ListBox).Font := TOpenEdit(EditCtrl).Font;
    if TListBox(ListBox).IntegralHeight then
    begin
      ListBox.Canvas.Font := TListBox(ListBox).Font;
      TListBox(ListBox).ItemHeight := ListBox.Canvas.TextHeight('Wy');
    end;
    ListBox.Items.Clear;
    GetValueList(ListBox.Items);
    if ListBox.Items.Count < MaxListCount then
      ListCount := ListBox.Items.Count
    else
      ListCount := MaxListCount;
    if ListCount = 0 then
      ListCount := 1;
    TListBox(ListBox).Height := ListCount * TListBox(ListBox).ItemHeight + 4;
    ListBox.ItemIndex := ListBox.Items.IndexOf(EditCtrl.Text);
    J := ListBox.ClientWidth;
    if ListBox.Items.Count > ListCount then
      Dec(J, GetSystemMetrics(SM_CXVSCROLL));
    for I := 0 to ListBox.Items.Count - 1 do
    begin
      Y := ListBox.Canvas.TextWidth(ListBox.Items[I]);
      if TListBox(ListBox).Style <> lbStandard then
        DoMeasureListItemWidth(ListBox, I, Y);
      if Y > J then J := Y;
    end;
    if ListBox.Items.Count > ListCount then
      Inc(J, GetSystemMetrics(SM_CXVSCROLL));
    ListBox.ClientWidth := J;
    P := Inspector.ClientToScreen(Point(EditCtrl.Left, EditCtrl.Top));
    Y := P.Y + HeightOf(Rects[iprValueArea]);
    if Y + ListBox.Height > Screen.Height then
      Y := P.Y - TListBox(ListBox).Height;
    SetWindowPos(ListBox.Handle, HWND_TOP, P.X, Y, 0, 0,
      SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
    FDroppedDown := True;
    InvalidateItem;
    Windows.SetFocus(EditCtrl.Handle);
  end;
end;

procedure TJvCustomInspectorItem.Edit;
begin
end;

procedure TJvCustomInspectorItem.EditChange(Sender: TObject);
begin
  if AutoUpdate then
  begin
    DisplayValue := EditCtrl.Text;
    InvalidateItem;
  end;
end;

procedure TJvCustomInspectorItem.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Sender <> Inspector then
    { OnKeyDown event from the edit control. Pass control to the inspector.
      When the inspector finishes it will pass control to EditKeyDown method
      with the Sender parameter pointing to itself. }
    Inspector.KeyDown(Key, Shift)
  else
  begin
    { OnKeyDown event called from the inspector. Handle item keys }
    if Shift = [] then
    begin
      case Key of
        VK_RETURN:
          Apply;
        VK_ESCAPE:
          Undo;
      end;
      if (Key = VK_RETURN) or (Key = VK_ESCAPE) then
        Key := 0;
    end
    else if Shift = [ssCtrl] then
      case Key of
        VK_UP:
          if iifValueList in Flags then
          begin
            SelectValue(-1);
            Key := 0;
          end;
        VK_DOWN:
          if iifValueList in Flags then
          begin
            SelectValue(1);
            Key := 0;
          end;
        VK_RETURN:
          if iifValueList in Flags then
          begin
            SelectValue(1);
            Key := 0;
          end
          else if iifEditButton in Flags then
          begin
            Key := 0;
            ButtonClick(Sender);
          end;
      end;
  end;
end;

procedure TJvCustomInspectorItem.EditMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (ssDouble in Shift) and
      (iifValueList in Flags) then
    SelectValue(1);
end;

procedure TJvCustomInspectorItem.EditMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TJvCustomInspectorItem.EditMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TJvCustomInspectorItem.Edit_WndProc(var Message: TMessage);
var
  ExecInherited: Boolean;
begin
  ExecInherited := True;
  case Message.Msg of
    WM_KEYDOWN,
    WM_SYSKEYDOWN,
    WM_CHAR:
      begin
        if iifValueList in Flags then
          with TWMKey(Message) do
          begin
            DoDropDownKeys(CharCode, KeyDataToShiftState(KeyData));
            if (CharCode <> 0) and DroppedDown then
            begin
              with TMessage(Message) do
                SendMessage(ListBox.Handle, Msg, WParam, LParam);
              if not (iifAllowNonListValues in Flags) or
                  ((Msg = WM_KEYDOWN) and
                  (TWMKeyDown(Message).CharCode in [VK_UP, VK_DOWN])) then
                ExecInherited := False;
            end;
          end;
      end;
  end;
  if ExecInherited then
    EditWndPrc(Message);
  case Message.Msg of
    WM_GETDLGCODE:
      begin
        if Inspector.WantTabs then
          Message.Result := Message.Result or DLGC_WANTTAB;
      end;
  end;
end;

function TJvCustomInspectorItem.GetAutoUpdate: Boolean;
begin
  Result := (iifAutoUpdate in Flags);
end;

function TJvCustomInspectorItem.GetBaseCategory: TJvInspectorCustomCategoryItem;
begin
  if (Self is TJvInspectorCustomCategoryItem) and (Level = 0) then
    Result := Self as TJvInspectorCustomCategoryItem
  else
  begin
    Result := Category;
    while (Result <> nil) and (Result.Level > 0) do
      Result := Result.Category;
  end;
end;

function TJvCustomInspectorItem.GetCategory: TJvInspectorCustomCategoryItem;
var
  ParItem: TJvCustomInspectorItem;
begin
  ParItem := Parent;
  while (ParItem <> nil) and not (ParItem is TJvInspectorCustomCategoryItem) do
    ParItem := ParItem.Parent;
  if ParItem is TJvInspectorCustomCategoryItem then
    Result := ParItem as TJvInspectorCustomCategoryItem
  else
    Result := nil;
end;

function TJvCustomInspectorItem.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJvCustomInspectorItem.GetData: TJvCustomInspectorData;
begin
  Result := FData;
end;

function TJvCustomInspectorItem.GetDisplayIndex: Integer;
begin
  Result := FDisplayIndex;
end;

function TJvCustomInspectorItem.GetDisplayName: string;
begin
  Result := FDisplayName;
  if (Parent <> nil) and (iifQualifiedNames in Parent.Flags) then
    Result := Parent.DisplayName + '.' + Result;
end;

function TJvCustomInspectorItem.GetDisplayParent: TJvCustomInspectorItem;
begin
  Result := Parent;
  while (Result <> nil) and Result.Hidden do
    Result := Result.Parent;
  if Result = nil then
    Result := Inspector.Root;
end;

function TJvCustomInspectorItem.GetDisplayValue: string;
begin
  Result := '';
end;

function TJvCustomInspectorItem.GetDroppedDown: Boolean;
begin
  Result := FDroppedDown;
end;

function TJvCustomInspectorItem.GetEditCtrl: TCustomEdit;
begin
  Result := FEditCtrl;
end;

function TJvCustomInspectorItem.GetEditing: Boolean;
begin
  Result := FEditing;
end;

function TJvCustomInspectorItem.GetExpanded: Boolean;
begin
  Result := iifExpanded in Flags;
end;

function TJvCustomInspectorItem.GetFlags: TInspectorItemFlags;
begin
  Result := FFlags;
end;

function TJvCustomInspectorItem.GetHeight: Integer;
begin
  if RowSizing.SizingFactor = irsNoReSize then
    Result := Inspector.ItemHeight
  else
  begin
    case RowSizing.MinHeight of
      irsNameHeight:  Result := Inspector.Painter.GetNameHeight(Self);
      irsValueHeight: Result := Inspector.Painter.GetValueHeight(Self);
      irsItemHeight:  Result := Inspector.ItemHeight;
      else            Result := RowSizing.MinHeight;
    end;
    case RowSizing.SizingFactor of
      irsNameHeight:  Result := Result + HeightFactor * Inspector.Painter.GetNameHeight(Self);
      irsValueHeight: Result := Result + HeightFactor * Inspector.Painter.GetValueHeight(Self);
      irsItemHeight:  Result := Result + HeightFactor * Inspector.ItemHeight;
      else            Result := Result + HeightFactor * RowSizing.SizingFactor;
    end;
  end;
end;

function TJvCustomInspectorItem.GetHeightFactor: Integer;
begin
  Result := FHeight;
end;

function TJvCustomInspectorItem.GetHidden: Boolean;
begin
  Result := iifHidden in Flags;
end;

function TJvCustomInspectorItem.GetInspector: TJvCustomInspector;
begin
  Result := FInspector;
end;

function TJvCustomInspectorItem.GetInspectorPaintGeneration: Integer;
begin
  Result := Inspector.PaintGeneration;
end;

function TJvCustomInspectorItem.GetIsCompoundColumn: Boolean;
begin
  Result := (Parent <> nil) and (Parent is TJvInspectorCustomCompoundItem) and (Parent.IndexOf(Self) < 0);
end;

function TJvCustomInspectorItem.GetItems(
  const I: Integer): TJvCustomInspectorItem;
begin
  Result := TJvCustomInspectorItem(FItems[I]);
end;

function TJvCustomInspectorItem.GetLevel: Integer;
var
  Item: TJvCustomInspectorItem;

begin
  Item := Self;
  Result := -1;
  while (Item <> nil) do
  begin
    if not (iifHidden in Item.Flags) then
      Inc(Result);
    Item := Item.Parent;
  end;
end;

function TJvCustomInspectorItem.GetListBox: TCustomListBox;
begin
  Result := FListBox;
end;

function TJvCustomInspectorItem.GetMultiline: Boolean;
begin
  Result := (iifMultiline in Flags);
end;

function TJvCustomInspectorItem.GetNextSibling: TJvCustomInspectorItem;
var
  I: Integer;

begin
  Result := Parent;
  if Result <> nil then
  begin
    I := Succ(Result.IndexOf(Self));
    if (I = 0) or (I >= Result.Count) then
      Result := nil
    else
      Result := Result.Items[I];
  end;
end;

function TJvCustomInspectorItem.GetParent: TJvCustomInspectorItem;
begin
  Result := FParent;
end;

function TJvCustomInspectorItem.GetQualifiedNames: Boolean;
begin
  Result := (iifQualifiedNames in Flags);
end;

function TJvCustomInspectorItem.GetReadonly: Boolean;
begin
  Result := (iifReadonly in Flags);
end;

function TJvCustomInspectorItem.GetRects(const RectKind: TInspectorPaintRect): TRect;
begin
  if LastPaintGeneration = GetInspectorPaintGeneration then
    Result := FRects[RectKind]
  else
    Result := Rect(0, 0, 0, 0);
end;

function TJvCustomInspectorItem.GetRowSizing: TJvInspectorItemSizing;
begin
  Result := FRowSizing;
end;

function TJvCustomInspectorItem.GetSortKind: TInspectorItemSortKind;
begin
  Result := FSortKind;
end;

function TJvCustomInspectorItem.GetSortName: string;
var
  DisplayParent: TJvCustomInspectorItem;
begin
  Result := Format('%.7d', [DisplayIndex]);
  DisplayParent := GetDisplayParent;
  if (DisplayParent <> nil) and (DisplayParent <> Inspector.Root) then
    Result := DisplayParent.GetSortName + #31 + Result;
end;

procedure TJvCustomInspectorItem.GetValueList(const Strings: TStrings);
begin
  DoGetValueList(Strings);
end;

function TJvCustomInspectorItem.GetVisible: Boolean;
begin
  Result := iifVisible in Flags;
end;

procedure TJvCustomInspectorItem.InvalidateItem;
begin
  if Inspector <> nil then
    Inspector.InvalidateItem;
end;

procedure TJvCustomInspectorItem.InvalidateList;
begin
  if Inspector <> nil then
    Inspector.InvalidateList;
end;

procedure TJvCustomInspectorItem.InvalidateSort;
begin
  if Inspector.LockCount > 0 then
    Inspector.NotifySort(Self)
  else
  begin
    if SortKind in [iskNone, iskName, iskCustom] then
      Sort;
    if Inspector.LockCount = 0 then // LockCount will be -1 if called from EndUpdate
      InvalidateList;
  end;
end;

procedure TJvCustomInspectorItem.InvalidateMetaData;
begin
  InvalidateItem;
end;

procedure TJvCustomInspectorItem.ListMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CloseUp(PtInRect(ListBox.ClientRect, Point(X, Y)));
end;

procedure TJvCustomInspectorItem.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and
    PtInRect(Rects[iprEditButton], Point(X,Y)) then
  begin
    if DroppedDown then
      CloseUp(False)
    else
    begin
      Inspector.MouseCapture := True;
      Tracking := True;
      TrackButton(X, Y);
      if (iifValueList in Flags) then
        DropDown;
    end;
  end
  else if (Button = mbLeft) and (ssDouble in Shift) and
      (iifValueList in Flags) and
      (PtInRect(Rects[iprValueArea], Point(X, Y))) then
    SelectValue(1);
end;

procedure TJvCustomInspectorItem.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ListPos: TPoint;
  MousePos: TSmallPoint;
begin
  if Tracking then
  begin
    TrackButton(X, Y);
    if DroppedDown then
    begin
      ListPos := ListBox.ScreenToClient(Inspector.ClientToScreen(Point(X, Y)));
      if PtInRect(ListBox.ClientRect, ListPos) then
      begin
        StopTracking;
        MousePos := PointToSmallPoint(ListPos);
        SendMessage(ListBox.Handle, WM_LBUTTONDOWN, 0, Integer(MousePos));
        Exit;
      end;
    end;
  end;
end;

procedure TJvCustomInspectorItem.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  WasPressed: Boolean;
begin
  WasPressed := FPressed;
  StopTracking;
  if (Button = mbLeft) and WasPressed and (iifEditButton in Flags) then
    ButtonClick(Self);
end;

procedure TJvCustomInspectorItem.NaturalSort;
var
  ItemList: TList;
begin
  ItemList := TList.Create;
  try
    BuildDisplayableList(ItemList);
    ApplyDisplayIndices(ItemList);
  finally
    ItemList.Free;
  end;
end;

procedure TJvCustomInspectorItem.SelectValue(const Delta: Integer);
var
  SL: TStrings;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    GetValueList(SL);
    if SL.Count>0 then
    begin
      I := SL.IndexOf(DisplayValue);
      Inc(I, Delta);
      while I < 0 do
        I := I + SL.Count;
      while I >= SL.Count do
        I := I - SL.Count;
      EditCtrl.Text := SL[I];
      Apply;
    end;
  finally
    SL.Free;
  end;
end;

procedure TJvCustomInspectorItem.SetAutoUpdate(const Value: Boolean);
begin
  if Value <> AutoUpdate then
  begin
    if Value then
      Flags := Flags + [iifAutoUpdate]
    else
      Flags := Flags - [iifAutoUpdate];
  end;
end;

procedure TJvCustomInspectorItem.SetDisplayIndex(const Value: Integer);
var
  DisplayParent: TJvCustomInspectorItem;
begin
  if Value <> DisplayIndex then
  begin
    DisplayParent := GetDisplayParent;
    if DisplayParent <> nil then
      DisplayParent.UpdateDisplayOrder(Self, Value);
  end;
  SortKind := iskManual;
end;

procedure TJvCustomInspectorItem.SetDisplayIndexValue(const Value: Integer);
begin
  FDisplayIndex := Value;
end;

procedure TJvCustomInspectorItem.SetDisplayName(Value: string);
var
  S: string;
begin
  if (Parent <> nil) and (iifQualifiedNames in Parent.Flags) then
    S := Parent.DisplayName + '.';
  if S <> Copy(Value, 1, Length(S)) then
    System.Delete(Value, 1, Length(S));
  if Value <> FDisplayName then
  begin
    FDisplayName := Value;
    InvalidateItem;
  end;
end;

procedure TJvCustomInspectorItem.SetDisplayValue(const Value: string);
begin
end;

procedure TJvCustomInspectorItem.SetEditCtrl(const Value: TCustomEdit);
begin
  if EditCtrl <> Value then
  begin
    if EditCtrl <> nil then
    begin
      EditCtrl.WindowProc := Edit_WndProc;
      EditCtrl.Free;
    end;
    FEditCtrl := Value;
    if EditCtrl <> nil then
      with TOpenEdit(EditCtrl) do
      begin
        Ctl3D := False;
        BorderStyle := bsNone;
        Parent := TWinControl(Owner);
      end;
  end;
end;

procedure TJvCustomInspectorItem.SetEditing(const Value: Boolean);
begin
  FEditing := Value;
end;

procedure TJvCustomInspectorItem.SetExpanded(Value: Boolean);
begin
  if Value <> Expanded then
  begin
    if Value then
      Flags := Flags + [iifExpanded]
    else
      Flags := Flags - [iifExpanded];
  end;
end;

procedure TJvCustomInspectorItem.SetFlags(const Value: TInspectorItemFlags);
var
  NewFlags: TInspectorItemFlags;
  OldFlags: TInspectorItemFlags;
begin
  NewFlags := Value;
  if (iifOwnerDrawListFixed in NewFlags) and (iifOwnerDrawListVariable in NewFlags) then
    Exclude(NewFlags, iifOwnerDrawListFixed);
  if (iifAllowNonListValues in NewFlags) or (iifOwnerDrawListFixed in NewFlags) or
      (iifOwnerDrawListVariable in NewFlags) then
    Include(NewFlags, iifValueList);
  if Flags <> NewFlags then
  begin
    OldFlags := Flags;
    FFlags := NewFlags;
    OldFlags := OldFlags * [iifExpanded, iifHidden, iifVisible];
    NewFlags := NewFlags * [iifExpanded, iifHidden, iifVisible];
    if NewFlags <> OldFlags then
      InvalidateList
    else
      InvalidateItem;
  end;
end;

procedure TJvCustomInspectorItem.SetFocus;
begin
  if (EditCtrl <> nil) and EditCtrl.CanFocus then
    EditCtrl.SetFocus
  else
    Inspector.SetFocus;
end;

procedure TJvCustomInspectorItem.SetHeight(Value: Integer);
var
  Factor: Integer;
begin
  case RowSizing.MinHeight of
    irsNameHeight:  Dec(Value, Inspector.Painter.GetNameHeight(Self));
    irsValueHeight: Dec(Value, Inspector.Painter.GetValueHeight(Self));
    irsItemHeight:  Dec(Value, Inspector.ItemHeight);
    else            Dec(Value, RowSizing.MinHeight);
  end;
  if Value < 0 then
    Value := 0;
  case RowSizing.SizingFactor of
    irsNoReSize:    Factor := 0;
    irsNameHeight:  Factor := Value div Inspector.Painter.GetNameHeight(Self);
    irsValueHeight: Factor := Value div Inspector.Painter.GetValueHeight(Self);
    irsItemHeight:  Factor := Value div Inspector.ItemHeight;
    else            Factor := Value div RowSizing.SizingFactor;
  end;

  if Factor <> HeightFactor then
  begin
    HeightFactor := Factor;
    InvalidateItem;
    Inspector.CalcImageHeight;
  end;
end;

procedure TJvCustomInspectorItem.SetHeightFactor(Value: Integer);
begin
  FHeight := Value;
  Inspector.InvalidateHeight;
  InvalidateItem;
end;

procedure TJvCustomInspectorItem.SetHidden(Value: Boolean);
begin
  if Value <> Hidden then
  begin
    if Value then
      Flags := Flags + [iifHidden]
    else
      Flags := Flags - [iifHidden];
  end;
end;

procedure TJvCustomInspectorItem.SetInspector(const AInspector: TJvCustomInspector);
begin
  if Parent = nil then
    FInspector := AInspector;
end;

procedure TJvCustomInspectorItem.SetMultiline(const Value: Boolean);
begin
  if Value <> Multiline then
  begin
    if Value then
      Flags := Flags + [iifMultiline]
    else
      Flags := Flags - [iifMultiline];
  end;
end;

procedure TJvCustomInspectorItem.SetOnCompare(const Value: TInspectorItemSortCompare);
begin
  if @Value <> @OnCompare then
  begin
    FOnCompare := Value;
    if @Value = nil then
      SortKind := iskNone;
    InvalidateSort;
  end;
end;

procedure TJvCustomInspectorItem.SetParent(const Value: TJvCustomInspectorItem);
begin
  if Parent <> Value then
  begin
    if Parent = nil then
    begin
      FParent := Value;
    end
    else
      raise EJvInspectorItem.Create(sJvInspItemHasParent);
  end;
end;

procedure TJvCustomInspectorItem.SetQualifiedNames(const Value: Boolean);
begin
  if Value <> QualifiedNames then
  begin
    if Value then
      Flags := Flags + [iifQualifiedNames]
    else
      Flags := Flags - [iifQualifiedNames];
  end;
end;

procedure TJvCustomInspectorItem.SetReadonly(const Value: Boolean);
begin
  if Value <> Readonly then
  begin
    if Value then
      Flags := Flags + [iifReadonly]
    else
      Flags := Flags - [iifReadonly];
  end;
end;

procedure TJvCustomInspectorItem.SetRects(const RectKind: TInspectorPaintRect;
  Value: TRect);
begin
  UpdateLastPaintGeneration;
  if not EqualRect(Rects[RectKind], Value) then
  begin
    FRects[RectKind] := Value;
    if (RectKind = iprEditValue) and (EditCtrl <> nil) then
    begin
      EditCtrl.BoundsRect := Rects[iprEditValue];
      if DroppedDown then
        CloseUp(False);
    end;
  end;
end;

procedure TJvCustomInspectorItem.SetRowSizing(Value: TJvInspectorItemSizing);
begin
  if (Value <> nil) and (Value <> RowSizing) then
    RowSizing.Assign(Value);
end;

procedure TJvCustomInspectorItem.SetSortKind(Value: TInspectorItemSortKind);
begin
  if (Value = iskCustom) and (@OnCompare = nil) then
    Value := iskNone;
  if Value <> SortKind then
  begin
    FSortKind := Value;
    InvalidateSort;
  end;
end;

procedure TJvCustomInspectorItem.SetVisible(Value: Boolean);
begin
  if Value <> Visible then
  begin
    if Value then
      Flags := Flags + [iifVisible]
    else
      Flags := Flags - [iifVisible];
  end;
end;

procedure TJvCustomInspectorItem.StopTracking;
begin
  if Tracking then
  begin
    TrackButton(-1, -1);
    Tracking := False;
    Inspector.MouseCapture := False;
  end;
end;

procedure TJvCustomInspectorItem.TrackButton(X,Y: Integer);
var
  NewState: Boolean;
  R: TRect;
begin
  R := Rects[iprEditButton];
  NewState := PtInRect(R, Point(X, Y));
  if Pressed <> NewState then
  begin
    Pressed := NewState;
    InvalidateRect(Inspector.Handle, @R, False);
  end;
end;

procedure TJvCustomInspectorItem.Undo;
begin
  if Editing then
  begin
    EditCtrl.Undo;
    EditCtrl.SelectAll;
  end;
end;

procedure TJvCustomInspectorItem.UpdateDisplayOrder(const Item: TJvCustomInspectorItem;
  const NewIndex: Integer);
var
  L: TList;
begin
  L := TList.Create;
  try
    BuildDisplayableList(L);
    L.Sort(DisplayIndexSortCompare);
    L.Remove(Item);
    L.Insert(NewIndex, Item);
    ApplyDisplayIndices(L);
  finally
    L.Free;
  end;
end;

procedure TJvCustomInspectorItem.UpdateLastPaintGeneration;
begin
  FLastPaintGen := GetInspectorPaintGeneration;
end;

constructor TJvCustomInspectorItem.Create(const AParent: TJvCustomInspectorItem;
  const AData: TJvCustomInspectorData);
begin
  inherited Create;
  FData := nil;
  FItems := TObjectList.Create(True);
  Flags := [iifVisible];
  FRowSizing := TJvInspectorItemSizing.Create(Self);
  FSortKind := iskName;
  FDisplayIndex := -1;
  if AData <> nil then
    FDisplayName := AData.Name;
  if AParent <> nil then
  begin
    FInspector := AParent.Inspector;
    AParent.Add(Self)
  end;
  FData := AData;
end;

destructor TJvCustomInspectorItem.Destroy;
begin
  if Inspector <> nil then
    Inspector.RemoveNotifySort(Self);
  FItems.Free;
  if Data <> nil then
    FData.RemoveItem(Self);
  inherited Destroy;
end;

function TJvCustomInspectorItem.Add(const Item: TJvCustomInspectorItem): Integer;
begin
  Result := Count;
  Insert(Result, Item);
end;

procedure TJvCustomInspectorItem.AfterConstruction;
begin
  inherited AfterConstruction;
  InvalidateMetaData;
  DoAfterItemCreate;
end;

procedure TJvCustomInspectorItem.Clear;
begin
  Inspector.BeginUpdate;
  try
    while Count > 0 do
      Delete(Count - 1);
  finally
    Inspector.EndUpdate;
  end;
end;

procedure TJvCustomInspectorItem.Delete(const Index: Integer);
var
  Disp: TJvCustomInspectorItem;
begin
  Disp := Items[Index].GetDisplayParent;
  FItems.Delete(Index);
  if Disp <> nil then
    Disp.InvalidateSort
  else
    InvalidateSort;
end;

procedure TJvCustomInspectorItem.Delete(const Item: TJvCustomInspectorItem);
var
  Idx: Integer;
begin
  Idx := IndexOf(Item);
  if Idx > -1 then
    Delete(Idx);
end;

procedure TJvCustomInspectorItem.Delete(const Data: TJvCustomInspectorData);
var
  Idx: Integer;
begin
  Idx := IndexOf(Data);
  if Idx > -1 then
    Delete(Idx);
end;

procedure TJvCustomInspectorItem.DrawEditor(const ACanvas: TCanvas);
const
  LeftOffs = 3;
var
  R: TRect;
  BFlags: Integer;
  W, G, I: Integer;
begin
  // This reduces the flickering when dragging the divider bar
  if EditCtrl <> nil then
  begin
    ACanvas.Lock;
    try
      EditCtrl.PaintTo(ACanvas.Handle, EditCtrl.Left, EditCtrl.Top);
    finally
      ACanvas.Unlock;
    end;
  end;
  R := Rects[iprEditButton];
  if not IsRectEmpty(R) then
  begin
    BFlags := 0;
    if iifValueList in Flags then
    begin
      if not EditCtrl.Enabled then
        BFlags := DFCS_INACTIVE
      else if Pressed then
        BFlags := DFCS_FLAT or DFCS_PUSHED;
      DrawFrameControl(ACanvas.Handle, R, DFC_SCROLL, BFlags or DFCS_SCROLLCOMBOBOX);
    end
    else if iifEditButton in Flags then
    begin
      if Pressed then
        BFlags := BF_FLAT;
      DrawEdge(ACanvas.Handle, R, EDGE_RAISED, BF_RECT or BF_MIDDLE or BFlags);
      W := 2;
      G := (WidthOf(R) - 2 * Ord(Pressed) - (3 * W)) div 4;
      if G < 1 then
      begin
        W := 1;
        G := (WidthOf(R) - 2 * Ord(Pressed) - (3 * W)) div 4;
      end;
      if G < 1 then
        G := 1;
      if G > 3 then
        G := 3;

      BFlags := R.Left + (WidthOf(R) - 3 * W -2 * G) div 2 + Ord(Pressed);
      I := R.Top + (HeightOf(R) - W) div 2;
      PatBlt(ACanvas.handle, BFlags, I, W, W, BLACKNESS);
      PatBlt(ACanvas.handle, BFlags + G + W, I, W, W, BLACKNESS);
      PatBlt(ACanvas.handle, BFlags + 2 * G + 2 * W, I, W, W, BLACKNESS);
    end;
  end;
end;

procedure TJvCustomInspectorItem.DrawName(const ACanvas: TCanvas);
var
  ARect: TRect;
begin
  ARect := Rects[iprName];
  ACanvas.TextRect(ARect, ARect.Left, ARect.Top, DisplayName);
end;

procedure TJvCustomInspectorItem.DrawValue(const ACanvas: TCanvas);
var
  S: string;
  ARect: TRect;
  SafeColor: TColor;
begin
  if Data = nil then
    S := sJvInspItemUnInitialized
  else
  try
    if not Data.IsInitialized then
      S := sJvInspItemUnInitialized
    else if not Data.HasValue then
      S := sJvInspItemNoValue
    else if not Data.IsAssigned then
      S := sJvInspItemUnassigned
    else
      S := DisplayValue;
  except
      S := sJvInspItemValueException + ExceptObject.ClassName + ': ' +
        Exception(ExceptObject).Message;
  end;
  ARect := Rects[iprValue];
  SafeColor := ACanvas.Brush.Color;
  if Editing then
    ACanvas.Brush.Color := clWindow;
  try
    if not Editing then
    begin
      if not (iifMultiLine in Flags) then
        ACanvas.TextRect(ARect, ARect.Left, ARect.Top, S)
      else
        DrawTextEx(ACanvas.Handle, PChar(S), Length(S), ARect, DT_EDITCONTROL or
          DT_WORDBREAK, nil)
    end
    else
    begin
      ARect := Rects[iprValueArea];
      Inc(ARect.Top);
      ACanvas.FillRect(ARect);
      DrawEditor(ACanvas);
    end;
  finally
    if Editing then
      ACanvas.Brush.Color := SafeColor;
  end;
end;

function TJvCustomInspectorItem.EditFocused: Boolean;
begin
  Result := (EditCtrl <> nil) and EditCtrl.Focused;
end;

function TJvCustomInspectorItem.HasViewableItems: Boolean;
var
  I: Integer;

begin
  Result := False;
  I := 0;
  while (I < Count) and not Result do
  begin
    Result := (iifVisible in Items[I].Flags) and (
      not (iifHidden in Items[I].Flags) or (
        (iifExpanded in Items[I].Flags) and Items[I].HasViewableItems));
    Inc(I);
  end;
end;

function TJvCustomInspectorItem.IndexOf(
  const Item: TJvCustomInspectorItem): Integer;
begin
  Result := Pred(Count);
  while (Result > -1) and (Items[Result] <> Item) do
    Dec(Result);
end;

function TJvCustomInspectorItem.IndexOf(
  const Data: TJvCustomInspectorData): Integer;
begin
  Result := Pred(Count);
  while (Result > -1) and (Items[Result].Data <> Data) do
    Dec(Result);
end;

procedure TJvCustomInspectorItem.InitEdit;
begin
  SetEditing(CanEdit);
  if Editing then
  begin
    if Multiline then
    begin
      SetEditCtrl(TMemo.Create(Inspector));
      TMemo(EditCtrl).WordWrap := True;
      TMemo(EditCtrl).WantReturns := False;
      TMemo(EditCtrl).ScrollBars := ssVertical;
    end
    else
      SetEditCtrl(TEdit.Create(Inspector));
    if iifEditFixed in Flags then
    begin
      TOpenEdit(EditCtrl).ReadOnly := True;
      TOpenEdit(EditCtrl).TabStop := False;
      TOpenEdit(EditCtrl).Color := Inspector.Canvas.Brush.Color;
    end
    else
    begin
      TOpenEdit(EditCtrl).Color := clWindow;
    end;
    FEditWndPrc := EditCtrl.WindowProc;
    EditCtrl.WindowProc := Edit_WndProc;
    TOpenEdit(EditCtrl).AutoSize := False;
    if iifValueList in Flags then
    begin
      FListBox := TJvPopupListBox.Create(Inspector);
      ListBox.Visible := False;
      ListBox.Parent := EditCtrl;
      TListBox(ListBox).OnMouseUp := ListMouseUp;
      TListBox(ListBox).IntegralHeight := not (iifOwnerDrawListVariable in
        Flags);
      TListBox(ListBox).ItemHeight := 11;
      if iifOwnerDrawListFixed in Flags then
        TListBox(ListBox).Style := lbOwnerDrawFixed
      else if iifOwnerDrawListVariable in Flags then
        TListBox(ListBox).Style := lbOwnerDrawVariable;
      TListBox(ListBox).OnDrawItem := DoDrawListItem;
      TListBox(ListBox).OnMeasureItem := DoMeasureListItem;
    end;
    TOpenEdit(EditCtrl).Font.Assign(Inspector.Font);
    EditCtrl.BoundsRect := Rects[iprEditValue];
    TOpenEdit(EditCtrl).OnKeyDown := EditKeyDown;
    TOpenEdit(EditCtrl).OnMouseDown := EditMouseDown;
    TOpenEdit(EditCtrl).OnMouseMove := EditMouseMove;
    TOpenEdit(EditCtrl).OnMouseUp := EditMouseUp;
    TOpenEdit(EditCtrl).OnChange := EditChange;
    EditCtrl.Visible := True;
    if Data.IsAssigned then
      EditCtrl.Text := DisplayValue
    else
      EditCtrl.Text := '';
    EditCtrl.Modified := False;
    EditCtrl.SelectAll;
    if EditCtrl.CanFocus and Inspector.Focused then
      EditCtrl.SetFocus;
  end;
end;

procedure TJvCustomInspectorItem.DoneEdit(const CancelEdits: Boolean = False);
var
  HadFocus: Boolean;
begin
  if Editing then
  begin
    HadFocus := EditFocused;
    if DroppedDown then
      CloseUp(False);
    FreeAndNil(FListBox);
    if not CancelEdits and EditCtrl.Modified and (not Data.IsAssigned or (DisplayValue <> EditCtrl.Text)) then
      DisplayValue := EditCtrl.Text;
    SetEditCtrl(nil);
    if HadFocus then
      SetFocus;
  end;
  FEditing := False;
end;

procedure TJvCustomInspectorItem.Insert(const Index: Integer; const Item: TJvCustomInspectorItem);
var
  Disp: TJvCustomInspectorItem;
begin
  Item.SetParent(Self);
  FItems.Insert(Index, Item);
  Disp := Item.GetDisplayParent;
  if Disp <> nil then
    Disp.InvalidateSort
  else
    InvalidateSort;
end;

procedure TJvCustomInspectorItem.ScrollInView;
var
  ViewIdx: Integer;
  Item: TJvCustomInspectorItem;
  YDelta: Integer;
  BandIdx: Integer;
  FirstBand: Integer;
  BandsVisible: Integer;
begin
  ViewIdx := Inspector.VisibleIndex(Self);
  if ViewIdx < 0 then
  begin
    { Find visible parent }
    Item := Parent;
    while (Item <> nil) and (ViewIdx < 0) do
    begin
      ViewIdx := Inspector.VisibleIndex(Item);
      if ViewIdx < 0 then
        Item := Item.Parent;
    end;
  end;
  if ViewIdx > -1 then
  begin
    if not Inspector.UseBands then
    begin
      if Inspector.TopIndex > ViewIdx then
        Inspector.TopIndex := ViewIdx
      else if (Inspector.IdxToY(ViewIdx) - Inspector.IdxToY(Inspector.TopIndex) +
        Height) > Inspector.ClientHeight then
      begin
        YDelta := (Inspector.IdxToY(ViewIdx) + Height - Inspector.ClientHeight -
          Inspector.IdxToY(Inspector.TopIndex));
        ViewIdx := Inspector.TopIndex;
        while (YDelta > 0) and (ViewIdx < Inspector.VisibleCount) do
        begin
          Dec(YDelta, Inspector.VisibleItems[ViewIdx].Height);
          Inc(ViewIdx);
        end;
        if ViewIdx < Inspector.VisibleCount then
          Inspector.TopIndex := ViewIdx;
      end;
    end
    else
    begin
      // Find band and scroll that band into the view
      BandIdx := Inspector.GetBandFor(ViewIdx);
      FirstBand := Inspector.GetBandFor(Inspector.TopIndex);
      BandsVisible := Inspector.ClientWidth div Inspector.BandWidth;
      if (BandIdx < FirstBand) or (BandIdx >= (FirstBand + BandsVisible)) then
      begin
        if BandIdx < FirstBand then
          Inspector.TopIndex := Integer(Inspector.BandStarts[BandIdx])
        else
        begin
          FirstBand := BandIdx - BandsVisible + 1;
          if (FirstBand > -1) and (FirstBand < Inspector.BandStarts.Count) then
            Inspector.TopIndex := Integer(Inspector.BandStarts[FirstBand]);
        end;
      end;
    end;
  end;
end;

procedure TJvCustomInspectorItem.Sort;
begin
  case SortKind of
    iskNone:
      NaturalSort;
    iskName:
      AlphaSort;
    iskCustom:
      DataSort;
  end;
end;

{ TJvInspectorCustomCategoryItem }

procedure TJvInspectorCustomCategoryItem.SetFlags(
  const Value: TInspectorItemFlags);
var
  NewFlags: TInspectorItemFlags;
begin
  NewFlags := Value - [iifAutoUpdate, iifMultiLine, iifValueList,
    iifAllowNonListValues, iifOwnerDrawListFixed, iifOwnerDrawListVariable,
    iifEditButton] + [iifReadonly, iifEditFixed];
  inherited SetFlags(NewFlags);
end;

{ TJvInspectorCompoundColumn }

function TJvInspectorCompoundColumn.GetItem: TJvCustomInspectorItem;
begin
  Result := FItem;
end;

function TJvInspectorCompoundColumn.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TJvInspectorCompoundColumn.GetWidthSet: Integer;
begin
  Result := FWidthSet;
end;

procedure TJvInspectorCompoundColumn.SetItem(Value: TJvCustomInspectorItem);
begin
  if Item <> Value then
  begin
    if (Value <> nil) and (Value.Parent <> Parent) then
      raise EJvInspectorItem.Create(sJvInspItemNotAChild);
    if Item <> nil then
      Parent.Add(Item);
    FItem := Value;
    if Item <> nil then
      Parent.FItems.Extract(Item);
    FWidthSet := 0;
    FWidth := -1;
    Parent.InvalidateList;
  end;
end;

procedure TJvInspectorCompoundColumn.SetWidth(Value: Integer);
begin
  if Value <> Width then
    FWidth := Value;
end;

procedure TJvInspectorCompoundColumn.SetWidthExternal(Value: Integer);
begin
  if Value <> WidthSet then
  begin
    SetWidthSet(Value);
    TJvInspectorCustomCompoundItem(Item.Parent).RecalcColumnWidths(Self);
  end;
end;

procedure TJvInspectorCompoundColumn.SetWidthSet(Value: Integer);
begin
  if Value <> WidthSet then
  begin
    FWidthSet := Value;
    FWidth := -1;
  end;
end;

constructor TJvInspectorCompoundColumn.Create(const AParent: TJvInspectorCustomCompoundItem;
  const AItem: TJvCustomInspectorItem);
begin
  inherited Create;
  FParent := AParent;
  Item := AItem;
end;

destructor TJvInspectorCompoundColumn.Destroy;
begin
  Item := nil;
  inherited Destroy;
end;

{ TJvInspectorCustomCompoundItem }

function TJvInspectorCustomCompoundItem.AddColumnPrim(const Item: TJvCustomInspectorItem): Integer;
begin
  Result := ColumnCount;
  InsertColumnPrim(Result, Item);
end;

function TJvInspectorCustomCompoundItem.AddColumnPrim(const ItemIndex: Integer): Integer;
begin
  Result := ColumnCount;
  InsertColumnPrim(Result, Items[ItemIndex]);
end;

procedure TJvInspectorCustomCompoundItem.DeleteColumnPrim(const Column: TJvInspectorCompoundColumn);
var
  Idx: Integer;
begin
  Idx := IndexOfColumnPrim(Column);
  if Idx > -1 then
    DeleteColumnPrim(Idx)
  else
    raise EJvInspectorItem.Create(sJvInspItemColNotFound);
end;

procedure TJvInspectorCustomCompoundItem.DeleteColumnPrim(const Index: Integer);
begin
  FColumns.Delete(Index);
end;

procedure TJvInspectorCustomCompoundItem.DeleteColumnPrim(const Item: TJvCustomInspectorItem);
var
  Idx: Integer;
begin
  Idx := IndexOfColumnPrim(Item);
  if Idx > -1 then
    DeleteColumnPrim(Idx)
  else
    raise EJvInspectorItem.Create(sJvInspItemItemIsNotCol);
end;

procedure TJvInspectorCustomCompoundItem.DivideRect(const RectKind: TInspectorPaintRect; const Value: TRect);
var
  VisibleColCount: Integer;
  I: Integer;
  WidthAvail: Integer;
  CurRect: TRect;
  WidthUsedInt: Integer;
  WidthUsedDbl: Double;
  ColWidth: Double;
  SaveItem: TJvCustomInspectorItem;
begin
  Assert(Inspector.Painter <> nil);
  VisibleColCount := 0;
  for I := 0 to ColumnCount - 1 do
  begin
    if Columns[I].Width > 0 then
      Inc(VisibleColCount);
  end;
  WidthAvail := WidthOf(Value);
  if VisibleColCount > 1 then
    Dec(WidthAvail, Pred(VisibleColCount) * Inspector.Painter.DividerWidth);
  CurRect := Value;
  WidthUsedInt := 0;
  WidthUsedDbl := 0;
  for I := 0 to ColumnCount - 1 do
  begin
    ColWidth := (Columns[I].Width / 100) * WidthAvail;
    WidthUsedDbl := WidthUsedDbl + ColWidth;
    Inc(WidthUsedInt, Trunc(ColWidth));
    if WidthUsedDbl - WidthUsedInt > 1 then
    begin
      Inc(WidthUsedInt);
      ColWidth := ColWidth + 1;
    end;
    CurRect.Right := CurRect.Left + Trunc(ColWidth);
    Columns[I].Item.SetRects(RectKind, CurRect);
    if RectKind = iprValue then
    begin
      SaveItem := Inspector.Painter.Item;
      try
        Inspector.Painter.Item := Columns[I].Item;
        Inspector.Painter.CalcEditBasedRects;
      finally
        Inspector.Painter.Item := SaveItem;
      end;
    end;
    CurRect.Left := CurRect.Right + Inspector.Painter.DividerWidth;
  end;
end;

procedure TJvInspectorCustomCompoundItem.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (SelectedColumn <> nil) and SelectedColumn.Item.Editing then
    SelectedColumn.Item.EditKeyDown(Sender, Key, Shift)
  else
    inherited EditKeyDown(Sender, Key, Shift);
end;

function TJvInspectorCustomCompoundItem.GetColumnCount: Integer;
begin
  if FColumns <> nil then
    Result := FColumns.Count
  else
    Result := 0;
end;

function TJvInspectorCustomCompoundItem.GetColumns(I: Integer): TJvInspectorCompoundColumn;
begin
  Result := TJvInspectorCompoundColumn(FColumns[I]);
end;

function TJvInspectorCustomCompoundItem.GetDisplayName: string;
begin
  if SingleName then
  begin
    if SingleNameUseFirstCol then
    begin
      if ColumnCount > 0 then
        Result := Columns[0].Item.DisplayName
      else
        Result := '';
      if (Parent <> nil) and (iifQualifiedNames in Parent.Flags) then
        Result := Parent.DisplayName + '.' + Result;
    end
    else
      Result := inherited GetDisplayName;
  end;
end;

function TJvInspectorCustomCompoundItem.GetEditing: Boolean;
begin
  Result := (SelectedColumn <> nil) and SelectedColumn.Item.Editing;
end;

function TJvInspectorCustomCompoundItem.GetSelectedColumn: TJvInspectorCompoundColumn;
begin
  if SelectedColumnIndex > -1 then
    Result := Columns[SelectedColumnIndex]
  else
    Result := nil;
end;

function TJvInspectorCustomCompoundItem.GetSelectedColumnIndex: Integer;
begin
  Result := FSelectedColumnIdx;
end;

function TJvInspectorCustomCompoundItem.GetSingleName: Boolean;
begin
  Result := icifSingleName in CompoundItemFlags;
end;

function TJvInspectorCustomCompoundItem.GetSingleNameUseFirstCol: Boolean;
begin
  Result := icifSingleNameUseFirstCol in CompoundItemFlags;
end;

function TJvInspectorCustomCompoundItem.IndexOfColumnPrim(const Col: TJvInspectorCompoundColumn): Integer;
begin
  Result := ColumnCount -1;
  while (Result >= 0) and (Columns[Result] <> Col) do
    Dec(Result);
end;

function TJvInspectorCustomCompoundItem.IndexOfColumnPrim(const Item: TJvCustomInspectorItem): Integer;
begin
  Result := ColumnCount -1;
  while (Result >= 0) and (Columns[Result].Item <> Item) do
    Dec(Result);
end;

procedure TJvInspectorCustomCompoundItem.InsertColumnPrim(const Index: Integer; const Item: TJvCustomInspectorItem);
var
  Col: TJvInspectorCompoundColumn;
begin
  Col := TJvInspectorCompoundColumn.Create(Self, Item);
  try
    FColumns.Insert(Index, Col);
    RecalcColumnWidths(Col);
  except
    Col.Free;
    raise;
  end;
end;

procedure TJvInspectorCustomCompoundItem.InsertColumnPrim(const Index, ItemIndex: Integer);
begin
  InsertColumnPrim(Index, Items[ItemIndex]);
end;

procedure TJvInspectorCustomCompoundItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  for I := ColumnCount - 1 downto 0 do
  begin
    if PtInRect(Columns[I].Item.Rects[iprName], Point(X, Y)) or
      PtInRect(Columns[I].Item.Rects[iprValue], Point(X, Y)) then
    begin
      SelectedColumnIndex := I;
      Columns[I].Item.MouseDown(Button, Shift, X, Y);
      Break;
    end;
  end;
end;

procedure TJvInspectorCustomCompoundItem.RecalcColumnWidths(const SetColumn: TJvInspectorCompoundColumn = nil);
var
  Idx: Integer;
  PercentLeft: Integer;
  I: Integer;
  DivideOver: array of Integer;

  procedure AddDivide(const DivideIndex: Integer);
  begin
    SetLength(DivideOver, Length(DivideOver) + 1);
    DivideOver[High(DivideOver)] := DivideIndex;
  end;

begin
  if SetColumn <> nil then
  begin
    Idx := IndexOfColumnPrim(SetColumn);
    PercentLeft := 100 - SetColumn.WidthSet;
    if SetColumn.WidthSet > 0 then
      SetColumn.SetWidth(SetColumn.WidthSet)
    else
      AddDivide(Idx);
  end
  else
  begin
    Idx := -1;
    PercentLeft := 100;
  end;
  for I := 0 to ColumnCount - 1 do
  begin
    if I <> Idx then
    begin
      if Columns[I].WidthSet <> 0 then
      begin
        if Columns[I].WidthSet <= PercentLeft then
        begin
          Columns[I].SetWidth(Columns[I].WidthSet);
          Dec(PercentLeft, Columns[I].WidthSet);
        end
        else
        begin
          Columns[I].SetWidth(PercentLeft);
          PercentLeft := 0;
        end;
      end
      else
        AddDivide(I);
    end;
  end;
  if Length(DivideOver) > 0 then
  begin
    Idx := PercentLeft mod Length(DivideOver);
    PercentLeft := PercentLeft div Length(DivideOver);
    for I := 0 to High(DivideOver) do
    begin
      if I <> 0 then
        Columns[DivideOver[I]].SetWidth(PercentLeft)
      else
        Columns[DivideOver[I]].SetWidth(PercentLeft + Idx);
    end;
  end;
end;

procedure TJvInspectorCustomCompoundItem.SetCompoundItemFlags(Value: TInspectorCompoundItemFlags);
begin
  // Check the difference: if icifSingleName is removed, remove icifSingleNameUseFirstCol as well
  if ((CompoundItemFlags - Value) * [icifSingleName]) <> [] then
    Exclude(Value, icifSingleNameUseFirstCol)
  else if Value = [icifSingleNameUseFirstCol] then
    Include(Value, icifSingleName);
  if Value <> CompoundItemFlags then
  begin
    FCompoundItemFlags := Value;
    InvalidateItem;
  end;
end;

procedure TJvInspectorCustomCompoundItem.SetDisplayName(Value: string);
var
  S: string;
begin
  if SingleName then
  begin
    if SingleNameUseFirstCol then
    begin
      if (Parent <> nil) and (iifQualifiedNames in Parent.Flags) then
        S := Parent.DisplayName + '.';
      if S <> Copy(Value, 1, Length(S)) then
        System.Delete(Value, 1, Length(S));
      if (ColumnCount > 0) and (Columns[0].Item.DisplayName <> Value) then
        Columns[0].Item.DisplayName := Value;
    end
    else
      inherited SetDisplayName(Value);
  end;
end;

procedure TJvInspectorCustomCompoundItem.SetEditing(const Value: Boolean);
begin
  if SelectedColumn <> nil then
    SelectedColumn.Item.SetEditing(Value);
end;

procedure TJvInspectorCustomCompoundItem.SetFlags(const Value: TInspectorItemFlags);
var
  NewFlags: TInspectorItemFlags;
begin
  NewFlags := Value - [iifQualifiedNames, iifAutoUpdate, iifMultiLine,
    iifValueList, iifAllowNonListValues, iifOwnerDrawListFixed,
    iifOwnerDrawListVariable, iifEditButton] + [iifReadonly,
    iifEditFixed];
  inherited SetFlags(NewFlags);
end;

procedure TJvInspectorCustomCompoundItem.SetFocus;
begin
  if SelectedColumn <> nil then
    SelectedColumn.Item.SetFocus;
end;

procedure TJvInspectorCustomCompoundItem.SetRects(const RectKind: TInspectorPaintRect; Value: TRect);
begin
  inherited SetRects(RectKind, Value);
  case RectKind of
    iprName,
    iprValue:
      DivideRect(RectKind, Value);
  end;
end;

procedure TJvInspectorCustomCompoundItem.SetSelectedColumn(Value: TJvInspectorCompoundColumn);
begin
  SelectedColumnIndex := IndexOfColumnPrim(Value);
end;

procedure TJvInspectorCustomCompoundItem.SetSelectedColumnIndex(Value: Integer);
begin
  if Value <> SelectedColumnIndex then
  begin
    DoneEdit(False);
    FSelectedColumnIdx := Value;
    InitEdit;
    InvalidateItem;
  end;
end;

procedure TJvInspectorCustomCompoundItem.SetSingleName(Value: Boolean);
begin
  if Value <> SingleName then
  begin
    if Value then
      CompoundItemFlags := CompoundItemFlags + [icifSingleName]
    else
      CompoundItemFlags := CompoundItemFlags - [icifSingleName];
  end;
end;

procedure TJvInspectorCustomCompoundItem.SetSingleNameUseFirstCol(Value: Boolean);
begin
  if Value <> SingleNameUseFirstCol then
  begin
    if Value then
      CompoundItemFlags := CompoundItemFlags + [icifSingleNameUseFirstCol]
    else
      CompoundItemFlags := CompoundItemFlags - [icifSingleNameUseFirstCol];
  end;
end;

constructor TJvInspectorCustomCompoundItem.Create(const AParent: TJvCustomInspectorItem;
  const AData: TJvCustomInspectorData);
begin
  inherited Create(AParent, AData);
  FColumns := TObjectList.Create;
end;

destructor TJvInspectorCustomCompoundItem.Destroy;
begin
  FreeAndNil(FColumns);
  inherited Destroy;
end;

procedure TJvInspectorCustomCompoundItem.DoneEdit(const CancelEdits: Boolean = False);
begin
  if SelectedColumn <> nil then
    SelectedColumn.Item.DoneEdit(CancelEdits);
end;
 
procedure TJvInspectorCustomCompoundItem.DrawEditor(const ACanvas: TCanvas);
begin
end;

procedure TJvInspectorCustomCompoundItem.DrawName(const ACanvas: TCanvas);
var
  RTop: Integer;
  RBottom: Integer;
  LastI: Integer;
  I: Integer;
  Col: TJvInspectorCompoundColumn;
begin
  if SingleName then
    inherited DrawName(ACanvas)
  else
  begin
    with Rects[iprNameArea] do
    begin
      RTop := Top;
      RBottom := Bottom;
    end;
    LastI := ColumnCount - 1;
    while (LastI > 0) and (Columns[LastI].Width < 1) do
      Dec(LastI);
    for I := 0 to LastI do
    begin
      Col := Columns[I];
      if Col.Width >= 0 then
      begin
        if (Inspector.Selected = Self) and (I = SelectedColumnIndex) then
        begin
          ACanvas.Brush.Color := Inspector.Painter.SelectedColor;
          ACanvas.Font.Color := Inspector.Painter.SelectedTextColor;
          with Col.Item.Rects[iprName] do
            ACanvas.FillRect(Rect(Left, RTop, Right, RBottom));
        end
        else
        begin
          ACanvas.Brush.Color := Inspector.Painter.BackgroundColor;
          ACanvas.Font.Color := Inspector.Painter.NameColor;
        end;
        Col.Item.DrawName(ACanvas);
        if I <> LastI then
          with Col.Item.Rects[iprName] do
            Inspector.Painter.PaintDivider(Right - 1, Top + 1, Bottom - 2);
      end;
    end;
  end;
end;

procedure TJvInspectorCustomCompoundItem.DrawValue(const ACanvas: TCanvas);
var
  LastI: Integer;
  I: Integer;
  Col: TJvInspectorCompoundColumn;
begin
  LastI := ColumnCount - 1;
  while (LastI > 0) and (Columns[LastI].Width < 1) do
    Dec(LastI);
  for I := 0 to LastI do
  begin
    Col := Columns[I];
    if Col.Width >= 0 then
    begin
      Col.Item.DrawValue(ACanvas);
      if I <> LastI then
        with Col.Item.Rects[iprValue] do
          Inspector.Painter.PaintDivider(Right - 1, Top + 1, Bottom - 2);
    end;
  end;
end;

function TJvInspectorCustomCompoundItem.EditFocused: Boolean;
begin
  Result := (SelectedColumn <> nil) and (SelectedColumn.Item.EditCtrl <> nil) and
    SelectedColumn.Item.EditCtrl.Focused;
end;

procedure TJvInspectorCustomCompoundItem.InitEdit;
begin
  if SelectedColumn <> nil then
    SelectedColumn.Item.InitEdit;
end;

{ TJvInspectorCompoundItem }

function TJvInspectorCompoundItem.AddColumn(const Item: TJvCustomInspectorItem): Integer;
begin
  Result := AddColumnPrim(Item);
end;

function TJvInspectorCompoundItem.AddColumn(const ItemIndex: Integer): Integer;
begin
  Result := AddColumnPrim(ItemIndex);
end;

procedure TJvInspectorCompoundItem.DeleteColumn(const Column: TJvInspectorCompoundColumn);
begin
  DeleteColumnPrim(Column);
end;

procedure TJvInspectorCompoundItem.DeleteColumn(const Index: Integer);
begin
  DeleteColumnPrim(Index);
end;

procedure TJvInspectorCompoundItem.DeleteColumn(const Item: TJvCustomInspectorItem);
begin
  DeleteColumnPrim(Item);
end;

function TJvInspectorCompoundItem.IndexOfColumn(const Col: TJvInspectorCompoundColumn): Integer;
begin
  Result := IndexOfColumnPrim(Col);
end;

function TJvInspectorCompoundItem.IndexOfColumn(const Item: TJvCustomInspectorItem): Integer;
begin
  Result := IndexOfColumnPrim(Item);
end;

procedure TJvInspectorCompoundItem.InsertColumn(const Index: Integer; const Item: TJvCustomInspectorItem);
begin
  InsertColumnPrim(Index, Item);
end;

procedure TJvInspectorCompoundItem.InsertColumn(const Index, ItemIndex: Integer);
begin
  InsertColumnPrim(Index, ItemIndex);
end;

{ TJvInspectorIntegerItem }

function TJvInspectorIntegerItem.GetDisplayValue: string;
begin
  Result := JclTypedIntToStr(Integer(Data.AsOrdinal), Data.TypeInfo)
end;

procedure TJvInspectorIntegerItem.SetDisplayValue(const Value: string);
var
  TmpOrd: Integer;
begin
  TmpOrd := JclStrToTypedInt(Value, Data.TypeInfo);
  if (JclTypeInfo(Data.TypeInfo) as IJclOrdinalRangeTypeInfo).OrdinalType = otULong then
    Data.AsOrdinal := Cardinal(TmpOrd)
  else
    Data.AsOrdinal := TmpOrd;
end;

{ TJvInspectorEnumItem }

function TJvInspectorEnumItem.GetDisplayValue: string;
begin
  Result := GetEnumName(Data.TypeInfo, Ord(Data.AsOrdinal));
end;

procedure TJvInspectorEnumItem.GetValueList(const Strings: TStrings);
var
  EnumInfo: IJclEnumerationTypeInfo;
  I: Integer;
begin
  EnumInfo := JclTypeInfo(Data.TypeInfo) as IJclEnumerationTypeInfo;
  for I := EnumInfo.MinValue to EnumInfo.MaxValue do
    if Trim(EnumInfo.Names[I]) <> '' then
      Strings.Add(EnumInfo.Names[I]);
end;

procedure TJvInspectorEnumItem.SetDisplayValue(const Value: string);
var
  OrdVal: Integer;
begin
  OrdVal := GetEnumValue(Data.TypeInfo, Value);
  if OrdVal <> -1 then
    Data.AsOrdinal := GetEnumValue(Data.TypeInfo, Value)
  else
    raise EJvInspectorItem.CreateFmt(sJvInspItemInvalidPropValue, [AnsiQuotedStr(Value, '''')]);
end;

procedure TJvInspectorEnumItem.SetFlags(const Value: TInspectorItemFlags);
var
  TmpFlags: TInspectorItemFlags;
begin
  TmpFlags := Value;
  Include(TmpFlags, iifValueList);
  inherited SetFlags(TmpFlags);
end;

{ TJvInspectorFloatItem }

function TJvInspectorFloatItem.GetDisplayValue: string;
begin
  Result := FloatToStr(Data.AsFloat);
end;

procedure TJvInspectorFloatItem.SetDisplayValue(const Value: string);
begin
  Data.AsFloat := StrToFloat(Value);
end;

{ TJvInspectorSetMemberData }

function TJvInspectorSetMemberData.GetAsFloat: Extended;
begin
  CheckReadAccess;
  raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Float']);
end;

function TJvInspectorSetMemberData.GetAsInt64: Int64;
begin
  CheckReadAccess;
  raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Int64']);
end;

function TJvInspectorSetMemberData.GetAsMethod: TMethod;
begin
  CheckReadAccess;
  raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['TMethod']);
end;

function TJvInspectorSetMemberData.GetAsOrdinal: Int64;
var
  Buf: array [0..31] of Byte;
begin
  CheckReadAccess;
  DataParent.GetAsSet(Buf);
  Result := Ord(TestBitBuffer(Buf, BitOffset));
end;

function TJvInspectorSetMemberData.GetAsString: string;
begin
  CheckReadAccess;
  raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['String']);
end;

function TJvInspectorSetMemberData.IsEqualReference(const Ref: TJvCustomInspectorData): Boolean;
begin
  Result := (Ref is TJvInspectorSetMemberData) and (TJvInspectorSetMemberData(Ref).DataParent = DataParent) and (TJvInspectorSetMemberData(Ref).BitOffset = BitOffset)
end;

procedure TJvInspectorSetMemberData.NotifyRemoveData(const Instance: TJvCustomInspectorData);
begin
  // if the instance to be removed is the data parent of this instance, free this instance as well.
  if Instance = DataParent then
    Free;
end;

procedure TJvInspectorSetMemberData.SetAsFloat(const Value: Extended);
begin
  CheckWriteAccess;
  raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Float']);
end;

procedure TJvInspectorSetMemberData.SetAsInt64(const Value: Int64);
begin
  CheckWriteAccess;
  raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Int64']);
end;

procedure TJvInspectorSetMemberData.SetAsMethod(const Value: TMethod);
begin
  CheckWriteAccess;
  raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Method']);
end;

procedure TJvInspectorSetMemberData.SetAsOrdinal(const Value: Int64);
var
  Buf: array [0..31] of Byte;
begin
  CheckWriteAccess;
  DataParent.GetAsSet(Buf);
  if Value <> 0 then
    SetBitBuffer(Buf, BitOffset)
  else
    ClearBitBuffer(Buf, BitOffset);
  DataParent.SetAsSet(Buf);
end;

procedure TJvInspectorSetMemberData.SetAsString(const Value: string);
begin
  CheckWriteAccess;
  raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['String']);
end;

procedure TJvInspectorSetMemberData.GetAsSet(var Buf);
begin
  CheckReadAccess;
  raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Set']);
end;

function TJvInspectorSetMemberData.HasValue: Boolean;
begin
  Result := IsInitialized;
end;

function TJvInspectorSetMemberData.IsAssigned: Boolean;
begin
  Result := IsInitialized;
end;

function TJvInspectorSetMemberData.IsInitialized: Boolean;
begin
  Result := True;
end;

class function TJvInspectorSetMemberData.New(const AParent: TJvCustomInspectorItem;
  const Ordinal: Integer; const ADataParent: TJvCustomInspectorData): TJvCustomInspectorItem;
var
  BaseInfo: IJclOrdinalRangeTypeInfo;
  Data: TJvInspectorSetMemberData;
begin
  Assert(ADataParent <> nil);
  Assert(AParent <> nil);
  BaseInfo := ((JclTypeInfo(ADataParent.TypeInfo) as IJclSetTypeInfo).
    BaseType as IJclOrdinalRangeTypeInfo);
  if BaseInfo.TypeKind = tkEnumeration then
    Data := CreatePrim(GetEnumName(BaseInfo.TypeInfo, Ordinal), System.TypeInfo(Boolean))
  else
    Data := CreatePrim(IntToStr(Ordinal), System.TypeInfo(Boolean));
  Data.FBitOffset := Ordinal mod 8 + 8 * ((Ordinal div 8) - (BaseInfo.MinValue div 8));
  Data.FDataParent := ADataParent;
  Data := TJvInspectorSetMemberData(DataRegister.Add(Data));
  if Data <> nil then
    Result := Data.NewItem(AParent)
  else
    Result := nil;
end;

procedure TJvInspectorSetMemberData.SetAsSet(const Buf);
begin
  CheckWriteAccess;
  raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Set']);
end;

{ TJvInspectorSetItem }

function TJvInspectorSetItem.CanEdit: Boolean;
begin
  Result := inherited CanEdit and (isfEditString in ItemSetFlags);
end;

procedure TJvInspectorSetItem.CreateMembers;
var
  SetInfo: IJclSetTypeInfo;
  BaseInfo: IJclOrdinalRangeTypeInfo;
  OrdVal: Integer;
begin
  Inspector.BeginUpdate;
  try
    DeleteMembers;
    JclTypeInfo(Data.TypeInfo).QueryInterface(IJclSetTypeInfo, SetInfo);
    SetInfo.BaseType.QueryInterface(IJclOrdinalRangeTypeInfo, BaseInfo);
    for OrdVal := Integer(BaseInfo.MinValue) to Integer(BaseInfo.MaxValue) do
      TJvInspectorSetMemberData.New(Self, OrdVal, Data);
  finally
    Inspector.EndUpdate;
  end;
end;

procedure TJvInspectorSetItem.DeleteMembers;
var
  I: Integer;
begin
  Inspector.BeginUpdate;
  try
    I := Pred(Count);
    while (I >= 0) do
    begin
      if Items[I].Data is TJvInspectorSetMemberData then
        Delete(I);
      Dec(I);
    end;
  finally
    Inspector.EndUpdate;
  end;
end;

function TJvInspectorSetItem.GetCreateMemberItems: Boolean;
begin
  Result := (isfCreateMemberItems in ItemSetFlags);
end;

function TJvInspectorSetItem.GetDisplayValue: string;
var
  SetBuf: array[0..31] of Byte;
begin
  Data.GetAsSet(SetBuf);
  Result := JclSetToStr(Data.TypeInfo, SetBuf, True, False);
end;

function TJvInspectorSetItem.GetEditString: Boolean;
begin
  Result := (isfEditString in ItemSetFlags);
end;

function TJvInspectorSetItem.GetItemSetFlags: TInspectorSetFlags;
begin
  Result := FItemSetFlags;
end;

procedure TJvInspectorSetItem.InvalidateMetaData;
begin
  if CreateMemberItems then
    CreateMembers
  else
    DeleteMembers;
end;

procedure TJvInspectorSetItem.SetCreateMemberItems(const Value: Boolean);
begin
  if Value <> CreateMemberItems then
  begin
    if Value then
      ItemSetFlags := ItemSetFlags + [isfCreateMemberItems]
    else
      ItemSetFlags := ItemSetFlags - [isfCreateMemberItems];
  end;
end;

procedure TJvInspectorSetItem.SetDisplayValue(const Value: string);
var
  SetBuf: array[0..31] of Byte;
begin
  JclStrToSet(Data.TypeInfo, SetBuf[0], Value);
  Data.SetAsSet(SetBuf[0]);
end;

procedure TJvInspectorSetItem.SetEditString(const Value: Boolean);
begin
  if Value <> EditString then
  begin
    if Value then
      ItemSetFlags := ItemSetFlags + [isfEditString]
    else
      ItemSetFlags := ItemSetFlags - [isfEditString];
  end;
end;

procedure TJvInspectorSetItem.SetFlags(const Value: TInspectorItemFlags);
var
  OldRO: Boolean;
  I: Integer;
begin
  OldRO := Readonly;
  inherited SetFlags(Value);
  if (OldRO <> Readonly) and CreateMemberItems then
    for I := 0 to Pred(Count) do
      Items[I].Readonly := Readonly;
end;

procedure TJvInspectorSetItem.SetItemSetFlags(const Value: TInspectorSetFlags);
begin
  if ItemSetFlags <> Value then
  begin
    FItemSetFlags := Value;
    InvalidateMetaData;
  end;
end;

constructor TJvInspectorSetItem.Create(const AParent: TJvCustomInspectorItem;
  const AData: TJvCustomInspectorData);
begin
  inherited Create(AParent, AData);
  ItemSetFlags := [isfCreateMemberItems];
end;

{ TJvInspectorCharItem }

function TJvInspectorCharItem.GetDisplayValue: string;
var
  I: Integer;
begin
  I := Data.AsOrdinal;
  if (I <= Ord(' ')) or (I > Ord('~')) then
    Result := '#' + IntToStr(I)
  else
    Result := Chr(Byte(I));
end;

procedure TJvInspectorCharItem.SetDisplayValue(const Value: string);
var
  I: Integer;
begin
  if Length(Value) > 1 then
    I := StrToInt(Copy(Value, 2, Length(Value)))
  else if Length(Value) = 1 then
    I := Ord(Value[1])
  else
    I := 0;
  Data.AsOrdinal := I;
end;

{ TJvInspectorInt64Item }

function TJvInspectorInt64Item.GetDisplayValue: string;
begin
  Result := IntToStr(Data.AsInt64);
end;

procedure TJvInspectorInt64Item.SetDisplayValue(const Value: string);
begin
  Data.AsInt64 := StrToInt64(Value);
end;

{ TJvInspectorStringItem }

function TJvInspectorStringItem.GetDisplayValue: string;
begin
  Result := Data.AsString;
end;

procedure TJvInspectorStringItem.SetDisplayValue(const Value: string);
begin
  Data.AsString := Value;
end;

{ TJvInspectorClassItem }

procedure TJvInspectorClassItem.CreateMembers;
begin
  if Data.IsInitialized and (Data.AsOrdinal <> 0) then
  begin
    Inspector.BeginUpdate;
    try
      DeleteMembers;
      TJvInspectorPropData.New(Self, TObject(Data.AsOrdinal));
      FLastMemberInstance := TObject(Data.AsOrdinal);
    finally
      Inspector.EndUpdate;
    end;
  end;
end;

function TJvInspectorClassItem.CanEdit: Boolean;
begin
  Result := inherited CanEdit and ((iifEditButton in Flags) or
    (iifValueList in Flags));
end;

procedure TJvInspectorClassItem.DeleteMembers;
var
  I: Integer;
begin
  if Data.IsInitialized then
  begin
    for I := Pred(Count) downto 0 do
      if (Items[I].Data is TJvInspectorPropData) and (Items[I].Data.IsInitialized) and
          (TJvInspectorPropData(Items[I].Data).Instance = FLastMemberInstance) then
        Delete(I);
    FLastMemberInstance := nil;
  end;
end;

function TJvInspectorClassItem.GetCreateMemberItems: Boolean;
begin
  Result := (icfCreateMemberItems in ItemClassFlags);
end;

function TJvInspectorClassItem.GetDisplayValue: string;
var
  Obj: TObject;
  SL: TStrings;
  I: Integer;
begin
  Obj := TObject(Data.AsOrdinal);
  if ShowClassName then
  begin
    if Obj <> nil then
      Result := Result + '('+ Obj.ClassName + ')'
    else
      Result := Result + '(' + GetTypeData(Data.TypeInfo).ClassType.ClassName +
        ')';
  end
  else
  begin
    if Obj <> nil then
    begin
      SL := TStringList.Create;
      try
        GetValueList(SL);
        I := SL.IndexOfObject(Obj);
        if I > -1 then
          Result := SL[I]
        else
          Result := '';
      finally
        SL.Free;
      end;
    end
    else
      Result := '';
  end;
end;

function TJvInspectorClassItem.GetItemClassFlags: TInspectorClassFlags;
begin
  Result := FItemClassFlags;
end;

function TJvInspectorClassItem.GetShowClassName: Boolean;
begin
  Result := (icfShowClassName in ItemClassFlags);
end;

procedure TJvInspectorClassItem.InvalidateItem;
begin
  inherited InvalidateItem;
  if icfCreateMemberItems in ItemClassFlags then
    CreateMembers;
end;

procedure TJvInspectorClassItem.InvalidateMetaData;
begin
  if icfCreateMemberItems in ItemClassFlags then
    CreateMembers;
end;

procedure TJvInspectorClassItem.SetCreateMemberItems(const Value: Boolean);
begin
  if Value <> CreateMemberItems then
  begin
    if Value then
      ItemClassFlags := ItemClassFlags + [icfCreateMemberItems]
    else
      ItemClassFlags := ItemClassFlags - [icfCreateMemberItems];
  end;
end;

procedure TJvInspectorClassItem.SetDisplayValue(const Value: string);
var
  SL: TStrings;
  I: Integer;
begin
  if Value = '' then
    Data.AsOrdinal := 0
  else
  begin
    SL := TStringList.Create;
    try
      GetValueList(SL);
      I := SL.IndexOf(Value);
      if I > -1 then
        Data.AsOrdinal := Integer(SL.Objects[I])
      else
        raise EJvInspectorItem.CreateFmt(sJvInspItemInvalidPropValue,
          [AnsiQuotedStr(Value, '''')]);
    finally
      SL.Free;
    end;
  end;
end;

procedure TJvInspectorClassItem.SetItemClassFlags(Value: TInspectorClassFlags);
begin
  if Value <> ItemClassFlags then
  begin
    FItemClassFlags := Value;
    InvalidateMetaData;
  end;
end;

procedure TJvInspectorClassItem.SetShowClassName(const Value: Boolean);
begin
  if Value <> ShowClassName then
  begin
    if Value then
      ItemClassFlags := ItemClassFlags + [icfShowClassName]
    else
      ItemClassFlags := ItemClassFlags - [icfShowClassName];
  end;
end;

constructor TJvInspectorClassItem.Create(const AParent: TJvCustomInspectorItem;
  const AData: TJvCustomInspectorData);
begin
  inherited Create(AParent, AData);
  if GetTypeData(Data.TypeInfo).ClassType.InheritsFrom(Classes.TComponent) then
  begin
    ItemClassFlags := [icfCreateMemberItems];
    Flags := Flags + [iifValueList];
  end
  else if GetTypeData(Data.TypeInfo).ClassType.InheritsFrom(TPersistent) then
  begin
    ItemClassFlags := [icfCreateMemberItems];
    ItemClassFlags := [icfShowClassName];
  end
  else
    ItemClassFlags := [icfShowClassName];
end;

{ TJvInspectorComponentItem }

function TJvInspectorComponentItem.GetItemComponentFlags: TInspectorComponentFlags;
begin
  Result := FItemComponentFlags;
end;

function TJvInspectorComponentItem.GetKeepFirstOwnerAsFirst: Boolean;
begin
  Result := icfKeepFirstOwnerAsFirst in ItemComponentFlags;
end;

function TJvInspectorComponentItem.GetNoShowFirstOwnerName: Boolean;
begin
  Result := icfNoShowFirstOwnerName in ItemComponentFlags;
end;

function TJvInspectorComponentItem.GetOwnerCount: Integer;
begin
  Result := FOwners.Count;
end;

function TJvInspectorComponentItem.GetOwners(I: Integer): TComponent;
begin
  Result := TComponent(FOwners[I]);
end;

function TJvInspectorComponentItem.GetShowOwnerNames: Boolean;
begin
  Result := icfShowOwnerNames in ItemComponentFlags;
end;

function TJvInspectorComponentItem.GetSortComponents: Boolean;
begin
  Result := icfSortComponents in ItemComponentFlags;
end;

function TJvInspectorComponentItem.GetSortOwners: Boolean;
begin
  Result := icfSortOwners in ItemComponentFlags;
end;

procedure TJvInspectorComponentItem.GetValueList(const Strings: TStrings);
var
  MinClass: TClass;
  SL: TStringList;
  OwnerList: TStringList;
  I: Integer;
  CurOwner: TComponent;
  PrefixWithOwner: string;
  J: Integer;
begin
  MinClass := GetTypeData(Data.TypeInfo).ClassType;
  SL := TStringList.Create;
  try
    OwnerList := TStringList.Create;
    try
      for I := 0 to OwnerCount - 1 do
        OwnerList.AddObject(Owners[I].Name, Owners[I]);
      if SortOwners then
        OwnerList.Sort;
      if (OwnerCount > 0) and KeepFirstOwnerAsFirst then
      begin
        I := OwnerList.IndexOfObject(Owners[0]);
        if I > 0 then
        begin
          OwnerList.Delete(I);
          OwnerList.InsertObject(0, Owners[0].Name, Owners[0]);
        end;
      end;
      for I := 0 to OwnerCount - 1 do
      begin
        SL.Clear;
        CurOwner := TComponent(OwnerList.Objects[I]);
        if ShowOwnerNames then
        begin
          if (I > 0) or not NoShowFirstOwnerName then
            PrefixWithOwner := CurOwner.Name + '.';
        end
        else
          PrefixWithOwner := '';
        for J := 0 to CurOwner.ComponentCount - 1 do
          if CurOwner.Components[J] is MinClass then
            SL.AddObject(PrefixWithOwner + CurOwner.Components[J].Name, CurOwner.Components[J]);
        if SL.Count > 0 then
        begin
          if SortComponents then
            SL.Sort;
          Strings.AddStrings(SL);
        end;
      end;
      SL.Clear;
      inherited GetValueList(SL);
      if SortComponents then
        SL.Sort;
      if SL.Count > 0 then
        Strings.AddStrings(SL);
    finally
      OwnerList.Free;
    end;
  finally
    SL.Free;
  end;
end;

procedure TJvInspectorComponentItem.SetFlags(const Value: TInspectorItemFlags);
begin
  inherited SetFlags(Value + [iifValueList]);
end;

procedure TJvInspectorComponentItem.SetItemClassFlags(Value: TInspectorClassFlags);
begin
  inherited SetItemClassFlags(Value - [icfShowClassName]);
end;

procedure TJvInspectorComponentItem.SetItemComponentFlags(Value: TInspectorComponentFlags);
begin
  if ItemComponentFlags <> Value then
  begin
    FItemComponentFlags := Value;
    InvalidateMetaData;
  end;
end;

procedure TJvInspectorComponentItem.SetKeepFirstOwnerAsFirst(Value: Boolean);
begin
  if Value <> KeepFirstOwnerAsFirst then
  begin
    if Value then
      ItemComponentFlags := ItemComponentFlags + [icfKeepFirstOwnerAsFirst]
    else
      ItemComponentFlags := ItemComponentFlags - [icfKeepFirstOwnerAsFirst];
  end;
end;

procedure TJvInspectorComponentItem.SetNoShowFirstOwnerName(Value: Boolean);
begin
  if Value <> NoShowFirstOwnerName then
  begin
    if Value then
      ItemComponentFlags := ItemComponentFlags + [icfNoShowFirstOwnerName]
    else
      ItemComponentFlags := ItemComponentFlags - [icfNoShowFirstOwnerName];
  end;
end;

procedure TJvInspectorComponentItem.SetOwners(I: Integer; Value: TComponent);
begin
  FOwners[I] := Value;
end;

procedure TJvInspectorComponentItem.SetShowOwnerNames(Value: Boolean);
begin
  if Value <> ShowOwnerNames then
  begin
    if Value then
      ItemComponentFlags := ItemComponentFlags + [icfShowOwnerNames]
    else
      ItemComponentFlags := ItemComponentFlags - [icfShowOwnerNames];
  end;
end;

procedure TJvInspectorComponentItem.SetSortComponents(Value: Boolean);
begin
  if Value <> SortComponents then
  begin
    if Value then
      ItemComponentFlags := ItemComponentFlags + [icfSortComponents]
    else
      ItemComponentFlags := ItemComponentFlags - [icfSortComponents];
  end;
end;

procedure TJvInspectorComponentItem.SetSortOwners(Value: Boolean);
begin
  if Value <> SortOwners then
  begin
    if Value then
      ItemComponentFlags := ItemComponentFlags + [icfSortOwners]
    else
      ItemComponentFlags := ItemComponentFlags - [icfSortOwners];
  end;
end;

constructor TJvInspectorComponentItem.Create(const AParent: TJvCustomInspectorItem;
  const AData: TJvCustomInspectorData);
begin
  inherited Create(AParent, AData);
  FOwners := TList.Create;
end;

destructor TJvInspectorComponentItem.Destroy;
begin
  inherited Destroy;
  FOwners.Free;
end;

procedure TJvInspectorComponentItem.AddOwner(const AOwner: TComponent);
begin
  if FOwners.IndexOf(Aowner) < 0 then
    FOwners.Add(AOwner);
end;

procedure TJvInspectorComponentItem.DeleteOwner(const AOwner: TComponent);
begin
  FOwners.Remove(AOwner);
end;

procedure TJvInspectorComponentItem.DeleteOwner(const Index: Integer);
begin
  FOwners.Delete(Index);
end;

{ TJvInspectorFontItem }

procedure TJvInspectorFontItem.Edit;
begin
  with TFontDialog.Create(GetParentForm(Inspector)) do
  try
    Font.Assign(TFont(Data.AsOrdinal));
    Device := fdScreen;
    if Execute then
    begin
      TFont(Data.AsOrdinal).Assign(Font);
      InvalidateItem;
    end;
  finally
    Free;
	  ShowScrollBar(Inspector.Handle, SB_BOTH, False);
  end;
end;

procedure TJvInspectorFontItem.SetFlags(const Value: TInspectorItemFlags);
var
  NewValue: TInspectorItemFlags;
begin
  NewValue := Value + [iifEditButton, iifEditFixed];
  inherited SetFlags(NewValue);
end;

{ TJvInspectorFontNameItem }

procedure TJvInspectorFontNameItem.DoDrawListItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  FontName: string;
begin
  with TListBox(Control) do
  begin
    FontName := Items[Index];
    Canvas.Font.Name := FontName;
    Canvas.TextRect(Rect, Rect.Left, Rect.Top, Items[Index]);
  end;
end;

procedure TJvInspectorFontNameItem.DoMeasureListItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
var
  FontName: string;
begin
  with TListBox(Control) do
  begin
    FontName := Items[Index];
    Canvas.Font.Name := FontName;
  end;
  Height := TListBox(Control).Canvas.TextHeight('Wy');
end;

procedure TJvInspectorFontNameItem.DoMeasureListItemWidth(Control: TWinControl;
  Index: Integer; var Width: Integer);
var
  FontName: string;
begin
  with TListBox(Control) do
  begin
    FontName := Items[Index];
    Canvas.Font.Name := FontName;
  end;
  Width := TListBox(Control).Canvas.TextWidth(FontName);
end;

procedure TJvInspectorFontNameItem.GetValueList(const Strings: TStrings);
begin
  Strings.Assign(Screen.Fonts);
end;

procedure TJvInspectorFontNameItem.SetFlags(const Value: TInspectorItemFlags);
var
  NewValue: TInspectorItemFlags;
begin
  NewValue := Value + [iifValueList, iifOwnerDrawListVariable];
  inherited SetFlags(NewValue);
end;

{ TJvInspectorBooleanItem }

function TJvInspectorBooleanItem.GetShowAsCheckbox: Boolean;
begin
  Result := FShowAsCheckbox;
end;

procedure TJvInspectorBooleanItem.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Bool: Boolean;
begin
  if ShowAsCheckbox then
  begin
    Bool := not (Data.AsOrdinal <> Ord(False));
    if Editing and (Shift = []) and (Key = VK_SPACE) then
    begin
      Data.AsOrdinal := Ord(Bool);
      InvalidateItem;
    end;
  end
  else
    inherited EditKeyDown(Sender, Key, Shift)
end;

procedure TJvInspectorBooleanItem.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Bool: Boolean;
begin
  Bool := not (Data.AsOrdinal <> Ord(False));
  if PtInRect(FCheckRect, Point(X, Y)) and (Shift = [ssLeft]) and
    Editing and ShowAsCheckbox then
  begin
    Data.AsOrdinal := Ord(Bool);
    InvalidateItem;
  end
  else
  begin
    if (ssDouble in Shift) and ShowAsCheckbox then
      Shift := Shift - [ssDouble];
    inherited MouseDown(Button, Shift, X, Y);
  end;
end;

procedure TJvInspectorBooleanItem.SetShowAsCheckbox(Value: Boolean);
var
  WasEditing: Boolean;
begin
  if Value <> ShowAsCheckbox then
  begin
    WasEditing := Editing;
    DoneEdit(False);
    FShowAsCheckbox := Value;
    InvalidateMetaData;
    if WasEditing then
      InitEdit;
  end;
end;

procedure TJvInspectorBooleanItem.DoneEdit(const CancelEdits: Boolean = False);
begin
  if ShowAsCheckbox then
    SetEditing(False)
  else
    inherited DoneEdit(CancelEdits);
end;

procedure TJvInspectorBooleanItem.DrawValue(const ACanvas: TCanvas);
var
  Bool: Boolean;
  ARect: TRect;
  SaveRgn: HRGN;
  HasRgn: Boolean;
  ClipRect: TRect;
  Rgn: HRGN;

begin
  if not ShowAsCheckbox then
    inherited DrawValue(ACanvas)
  else
  begin
    if Data.IsInitialized and Data.IsAssigned and Data.HasValue then
      Bool := Data.AsOrdinal <> Ord(False)
    else
      Bool := False;

    if Editing and Data.IsAssigned then
      ACanvas.Brush.Color := clWindow;
    ACanvas.FillRect(Rects[iprValueArea]);
    ARect := Rects[iprValue];
    OffsetRect(ARect, 2, 0);
    ARect.Right := ARect.Left + 13;
    ARect.Bottom := ARect.Top + 13;
    { Remember current clipping region }
    SaveRgn := CreateRectRgn(0,0,0,0);
    HasRgn := GetClipRgn(ACanvas.Handle, SaveRgn) > 0;
    { Clip all outside of the item rectangle }
    IntersectRect(ClipRect, ARect, Rects[iprValue]);
    FCheckRect := ClipRect;
    with ClipRect do
      Rgn := CreateRectRgn(Left, Top, Right, Bottom);
    SelectClipRgn(ACanvas.Handle, Rgn);
    DeleteObject(Rgn);
    try
    { Paint the 3d checkbox: Frame }
    Frame3D(ACanvas, ARect, clBlack, clWhite, 1);
    Frame3D(ACanvas, ARect, clBlack, cl3DLight, 1);

    if Bool then
    begin
      { Paint the 3d checkbox: Draw the checkmark }
      ACanvas.Pen.Color := clWindowText;
      ACanvas.Pen.Width := 1;
      ACanvas.MoveTo(ARect.Left + 1, ARect.Top + 3);
      ACanvas.LineTo(ARect.Left + 3, ARect.Top + 5);
      ACanvas.LineTo(ARect.Left + 8, ARect.Top);
      ACanvas.MoveTo(ARect.Left + 1, ARect.Top + 4);
      ACanvas.LineTo(ARect.Left + 3, ARect.Top + 6);
      ACanvas.LineTo(ARect.Left + 8, ARect.Top + 1);
      ACanvas.MoveTo(ARect.Left + 1, ARect.Top + 5);
      ACanvas.LineTo(ARect.Left + 3, ARect.Top + 7);
      ACanvas.LineTo(ARect.Left + 8, ARect.Top + 2);
    end;

    finally
    { restore previous clipping region }
    if HasRgn then
      SelectClipRgn(ACanvas.Handle, SaveRgn)
    else
      SelectClipRgn(ACanvas.Handle, 0);
    DeleteObject(SaveRgn);
    end;
  end;
end;

procedure TJvInspectorBooleanItem.InitEdit;
begin
  if ShowAsCheckbox then
    SetEditing(CanEdit)
  else
    inherited InitEdit;
end;

{ TJvInspectorDateItem }

function TJvInspectorDateItem.GetDisplayValue: string;
begin
  Result := FormatDateTime(Format, Data.AsFloat);
end;

procedure TJvInspectorDateItem.SetDisplayValue(const Value: string);
begin
  Data.AsFloat := Trunc(StrToDate(Value)) + Frac(Data.AsFloat);
end;

procedure TJvInspectorDateItem.SetFormat(Value: string);
var
  I: Integer;
  MCount: Integer;
  DCount: Integer;
  YCount: Integer;
  SepCount: Integer;
  WasEditing: Boolean;
begin
  // Only allow d, dd, m, mm, yy, yyyy and the date separator characters to ease parsing
  I := 1;
  MCount := 0;
  DCount := 0;
  YCount := 0;
  SepCount := 0;
  while (I < Length(Value)) do
  begin
    case Value[I] of
      'd':
        begin
          if (DCount = 0) and (I > 1) and (Value[I - 1] <>  DateSeparator) then
            raise EJvInspectorData.Create('A specifier should be placed before and after a separator.');
          if (DCount = 1) and (Value[I - 1] <> 'd') then
            raise EJvInspectorData.Create('''d'' or ''dd'' should appear only once.');
          if (DCount = 2) then
            raise EJvInspectorData.Create('Only ''d'' or ''dd'' are allowed.');
          Inc(DCount);
        end;
      'm':
        begin
          if (MCount = 0) and (I > 1) and (Value[I - 1] <>  DateSeparator) then
            raise EJvInspectorData.Create('A specifier should be placed before and after a separator.');
          if (MCount = 1) and (Value[I - 1] <> 'm') then
            raise EJvInspectorData.Create('''m'' or ''mm'' should appear only once.');
          if (MCount = 2) then
            raise EJvInspectorData.Create('Only ''m'' or ''mm'' are allowed.');
          Inc(MCount);
        end;
      'y':
        begin
          if (MCount = 0) and (I > 1) and (Value[I - 1] <>  DateSeparator) then
            raise EJvInspectorData.Create('A specifier should be placed before and after a separator.');
          if (YCount > 1) and (YCount < 4) and (Value[I - 1] <> 'y') then
            raise EJvInspectorData.Create('''yy'' or ''yyyy'' should appear only once.');
          if (YCount = 4) then
            raise EJvInspectorData.Create('Only ''yy'' or ''yyyy'' are allowed.');
          Inc(YCount);
        end;
      else
        if Value[I] = DateSeparator then
        begin
          if ((SepCount = 0) and (I = 1)) or
              ((SepCount = 1) and ((Value[I - 1]) = DateSeparator) or (I = Length(Value))) then
            raise EJvInspectorData.Create('A specifier should be placed before and after a separator.');
          if SepCount = 2 then
            raise EJvInspectorData.Create('Only two separators are allowed.');
          Inc(SepCount);
        end
        else
          raise EJvInspectorData.CreateFmt('Only ''d'', ''m'', ''y'' and ''%s'' are allowed', [DateSeparator]);
    end;
    Inc(I);
  end;
  if DCount = 0 then
    raise EJvInspectorData.Create('''d'' or ''dd'' are required.');
  if MCount = 0 then
    raise EJvInspectorData.Create('''m'' or ''mm'' are required.');
  if YCount = 0 then
    raise EJvInspectorData.Create('''yy'' or ''yyyy'' are required.');
  if (YCount = 1) or (YCount = 3) then
    raise EJvInspectorData.Create('Only ''yy'' or ''yyyy'' are allowed.');
  if Value <> FFormat then
  begin
    WasEditing := Editing;
    if Editing then
      DoneEdit;
    FFormat := Value;
    if WasEditing then
      InitEdit;
  end;
end;

constructor TJvInspectorDateItem.Create(const AParent: TJvCustomInspectorItem;
  const AData: TJvCustomInspectorData);
begin
  inherited Create(AParent, AData);
  FFormat := ShortDateFormat;
end;

{ TJvInspectorTimeItem }

function TJvInspectorTimeItem.GetDisplayValue: string;
begin
  Result := FormatDateTime(Format, Data.AsFloat);
end;

procedure TJvInspectorTimeItem.SetDisplayValue(const Value: string);
begin
  Data.AsFloat := Frac(StrToTime(Value)) + Trunc(Data.AsFloat);
end;

procedure TJvInspectorTimeItem.SetFormat;
begin
  FFormat := 'hh:nn';
  if ShowSeconds then
    FFormat := FFormat + ':ss';
  if ShowAMPM then
    FFormat := FFormat + ' ampm';
end;

procedure TJvInspectorTimeItem.SetShowAMPM(Value: Boolean);
var
  WasEditing: Boolean;
begin
  if Value <> ShowAMPM then
  begin
    WasEditing := Editing;
    DoneEdit;
    FShowAMPM := Value;
    SetFormat;
    if WasEditing then
      InitEdit;
  end;
end;

procedure TJvInspectorTimeItem.SetShowSeconds(Value: Boolean);
var
  WasEditing: Boolean;
begin
  if Value <> ShowSeconds then
  begin
    WasEditing := Editing;
    DoneEdit;
    FShowSeconds := Value;
    SetFormat;
    if WasEditing then
      InitEdit;
  end;
end;

constructor TJvInspectorTimeItem.Create(const AParent: TJvCustomInspectorItem;
  const AData: TJvCustomInspectorData);
begin
  inherited Create(AParent, AData);
  FShowSeconds := True;
  FShowAMPM := False;
  SetFormat;
end;

{ TJvInspectorDateTimeItem }

function TJvInspectorDateTimeItem.GetDateFormat: string;
begin
  Result := FDate.Format;
end;

function TJvInspectorDateTimeItem.GetTimeShowAMPM: Boolean;
begin
  Result := FTime.ShowAMPM;
end;

function TJvInspectorDateTimeItem.GetTimeShowSeconds: Boolean;
begin
  Result := FTime.ShowSeconds;
end;

procedure TJvInspectorDateTimeItem.SetDateFormat(Value: string);
begin
  FDate.Format := Value;
end;

procedure TJvInspectorDateTimeItem.SetTimeShowAMPM(Value: Boolean);
begin
  FTime.ShowAMPM := Value;
end;

procedure TJvInspectorDateTimeItem.SetTimeShowSeconds(Value: Boolean);
begin
  FTime.ShowSeconds := Value;
end;

constructor TJvInspectorDateTimeItem.Create(const AParent: TJvCustomInspectorItem;
  const AData: TJvCustomInspectorData);
begin
  inherited Create(AParent, AData);
  SingleNameUseFirstCol := True;
  FDate := TJvInspectorDateItem.Create(Self, AData);
  FTime := TJvInspectorTimeItem.Create(Self, AData);
  AddColumnPrim(FDate);
  AddColumnPrim(FTime);
end;

{ TJvCustomInspectorData }

constructor TJvCustomInspectorData.CreatePrim(const AName: string;
  const ATypeInfo: PTypeInfo);
begin
  inherited Create;
  Name := AName;
  TypeInfo := ATypeInfo;
end;

procedure TJvCustomInspectorData.CheckReadAccess;
begin
  if not IsInitialized then
    raise EJvInspectorData.Create(sJvInspDataNotInit);
  if not IsAssigned then
    raise EJvInspectorData.Create(sJvInspDataNotAssigned);
  if not HasValue then
    raise EJvInspectorData.Create(sJvInspDataNoValue);
end;

procedure TJvCustomInspectorData.CheckWriteAccess;
begin
  if not IsInitialized then
    raise EJvInspectorData.Create(sJvInspDataNotInit);
  if not HasValue then
    raise EJvInspectorData.Create(sJvInspDataNoValue);
end;

procedure TJvCustomInspectorData.DoneEdits(const CancelEdits: Boolean = False);
var
  I: Integer;
begin
  for I := Low(FItems) to High(FItems) do
    if Items[I].Editing then
      Items[I].DoneEdit(CancelEdits);
end;

function TJvCustomInspectorData.GetItemCount: Integer;
begin
  Result := Length(FItems);
end;

function TJvCustomInspectorData.GetItems(I: Integer): TJvCustomInspectorItem;
begin
  if (I < Low(FItems)) or (I > High(FItems)) then
    TList.Error(@SListIndexError, I);
  Result := FItems[I];
end;

function TJvCustomInspectorData.GetName: string;
begin
  Result := FName;
end;

function TJvCustomInspectorData.GetTypeInfo: PTypeInfo;
begin
  Result := FTypeInfo;
end;

procedure TJvCustomInspectorData.InitEdits;
var
  I: Integer;
begin
  for I := Low(FItems) to High(FItems) do
    if Items[I].Inspector.FocusedItem = Items[I] then
      Items[I].InitEdit;
end;

procedure TJvCustomInspectorData.Invalidate;
var
  I: Integer;
begin
  for I := High(FItems) downto Low(FItems) do
    FItems[I].InvalidateItem;
end;

function TJvCustomInspectorData.IsEqualReference(const Ref: TJvCustomInspectorData): Boolean;
begin
  Result := False;
end;

procedure TJvCustomInspectorData.NotifyRemoveData(const Instance: TJvCustomInspectorData);
begin
end;

procedure TJvCustomInspectorData.RefreshEdits;
var
  I: Integer;
begin
  for I := High(FItems) downto Low(FItems) do
    if Items[I].Editing then
    begin
      Items[I].DoneEdit(True);
      Items[I].InitEdit;
    end;
end;

class function TJvCustomInspectorData.RegisterInstance(const Instance: TJvCustomInspectorData): TJvCustomInspectorData;
begin
  Result := DataRegister.Add(Instance);
end;

procedure TJvCustomInspectorData.RemoveItem(const Item: TJvCustomInspectorItem);
var
  I: Integer;
begin
  I := High(FItems);
  while (I >= 0) do
  begin
    if Items[I] = Item then
      Break;
    Dec(I);
  end;
  if I >= 0 then
  begin
    if I <> High(FItems) then
      Move(FItems[I + 1], FITems[I], (Length(FItems) - I) * SizeOf(TJvCustomInspectorItem));
    SetLength(FItems, High(FItems));
  end;
end;

procedure TJvCustomInspectorData.SetName(const Value: string);
begin
  if Value <> Name then
  begin
    FName := Value;
    Invalidate;
  end;
end;

procedure TJvCustomInspectorData.SetTypeInfo(const Value: PTypeInfo);
begin
  if Value <> TypeInfo then
  begin
    FTypeInfo := Value;
    Invalidate;
  end;
end;

constructor TJvCustomInspectorData.Create;
begin
  raise EJvInspectorData.Create(ClassName + ' cannot be created separately.');
end;

procedure TJvCustomInspectorData.BeforeDestruction;
var
  I: Integer;
begin
  for I := High(FItems) downto Low(FItems) do
    Items[I].Free;
  if FRegistered then
    DataRegister.Remove(Self);
end;

class function TJvCustomInspectorData.ItemRegister: TJvInspectorRegister;
begin
  if FGenItemReg = nil then
    FGenItemReg := TJvInspectorRegister.Create(TJvCustomInspectorData);
  Result := FGenItemReg;
end;

class function TJvCustomInspectorData.New: TJvCustomInspectorData;
begin
  raise EJvInspectorData.Create(ClassName + ' does not allow a new instance to be created.');
end;

function TJvCustomInspectorData.NewItem(
  const AParent: TJvCustomInspectorItem): TJvCustomInspectorItem;
var
  ItemClass: TJvInspectorItemClass;
  RegItem: TJvCustomInspectorRegItem;
begin
  Result := nil;
  RegItem := ItemRegister.FindMatch(Self);
  if RegItem <> nil then
  begin
    ItemClass := RegItem.ItemClass;
    AParent.Inspector.DoBeforeItemCreate(Self, ItemClass);
    if ItemClass <> nil then
    begin
      Result := ItemClass.Create(AParent, Self);
      if Result <> nil then
      begin
        RegItem.ApplyDefaults(Result);
        SetLength(FItems, Length(FItems) + 1);
        FItems[High(FItems)] := Result;
      end;
    end;
  end;
end;

{ TJvInspectorVarData }

function TJvInspectorVarData.GetAddress: Pointer;
begin
  Result := FAddress;
end;

function TJvInspectorVarData.GetAsFloat: Extended;
begin
  CheckReadAccess;
  if TypeInfo.Kind = tkFloat then
    case GetTypeData(TypeInfo).FloatType of
      ftSingle:   Result := PSingle(Address)^;
      ftDouble:   Result := PDouble(Address)^;
      ftExtended: Result := PExtended(Address)^;
      ftComp:     Result := PComp(Address)^;
      ftCurr:     Result := PCurrency(Address)^;
      else        Result := 0;
    end
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Float']);
end;

function TJvInspectorVarData.GetAsInt64: Int64;
begin
  CheckReadAccess;
  if TypeInfo.Kind = tkInt64 then
    Result := PInt64(Address)^
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Int64']);
end;

function TJvInspectorVarData.GetAsMethod: TMethod;
begin
  CheckReadAccess;
  if TypeInfo.Kind = tkMethod then
    Result := PMethod(Address)^
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['TMethod']);
end;

function TJvInspectorVarData.GetAsOrdinal: Int64;
begin
  CheckReadAccess;
  if TypeInfo.Kind in [tkInteger, tkChar, tkEnumeration, tkSet, tkWChar] then
  begin
    case GetTypeData(TypeInfo).OrdType of
      otSByte:  Result := PShortint(Address)^;
      otUByte:  Result := PByte(Address)^;
      otSWord:  Result := PSmallint(Address)^;
      otUWord:  Result := PWord(Address)^;
      otSLong:  Result := PLongint(Address)^;
      otULong:  Result := PLongword(Address)^;
      else      Result := 0;
    end;
  end
  else if TypeInfo.Kind = tkClass then
    Result := PLongword(Address)^
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Ordinal']);
end;

function TJvInspectorVarData.GetAsString: string;
begin
  CheckReadAccess;
  if TypeInfo.Kind in [tkLString, tkWString, tkString] then
  begin
    case TypeInfo.Kind of
      tkLString:  Result := PString(Address)^;
      tkWString:  Result := PWideString(Address)^;
      tkString:   Result := PShortString(Address)^;
      else        Result := '';
    end;
  end
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['String']);
end;

function TJvInspectorVarData.IsEqualReference(const Ref: TJvCustomInspectorData): Boolean;
begin
  Result := (Ref is TJvInspectorVarData) and (TJvInspectorVarData(Ref).Address = Address);
end;
 
procedure TJvInspectorVarData.SetAddress(const Value: Pointer);
begin
  if Value <> Address then
  begin
    FAddress := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorVarData.SetAsFloat(const Value: Extended);
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkFloat then
    case GetTypeData(TypeInfo).FloatType of
      ftSingle:   PSingle(Address)^ := Value;
      ftDouble:   PDouble(Address)^ := Value;
      ftExtended: PExtended(Address)^ := Value;
      ftComp:     PComp(Address)^ := Value;
      ftCurr:     PCurrency(Address)^ := Value;
    end
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Float']);
  Invalidate;
end;

procedure TJvInspectorVarData.SetAsInt64(const Value: Int64);
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkInt64 then
  begin
    if (Value < GetTypeData(TypeInfo).MinValue) or
        (Value > GetTypeData(TypeInfo).MaxValue) then
      raise ERangeError.CreateFmt(SOutOfRange, [GetTypeData(TypeInfo).MinValue,
        GetTypeData(TypeInfo).MaxValue]);
    PInt64(Address)^ := Value;
    Invalidate;
  end
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Int64']);
end;

procedure TJvInspectorVarData.SetAsMethod(const Value: TMethod);
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkMethod then
    PMethod(Address)^ := Value
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['TMethod']);
  Invalidate;
end;

procedure TJvInspectorVarData.SetAsOrdinal(const Value: Int64);
var
  MinValue: Int64;
  MaxValue: Int64;
begin
  CheckWriteAccess;
  if TypeInfo.Kind in [tkInteger, tkChar, tkEnumeration, tkWChar] then
  begin
    case GetTypeData(TypeInfo).OrdType of
      otSByte:
        begin
          MinValue := GetTypeData(TypeInfo).MinValue;
          MaxValue := GetTypeData(TypeInfo).MaxValue;
          if (Value < MinValue) or (Value > MaxValue) then
            raise ERangeError.CreateFmt(SOutOfRange, [MinValue, MaxValue]);
          PShortint(Address)^ := Value;
        end;
      otUByte:
        begin
          MinValue := GetTypeData(TypeInfo).MinValue;
          MaxValue := GetTypeData(TypeInfo).MaxValue;
          if (Value < MinValue) or (Value > MaxValue) then
            raise ERangeError.CreateFmt(SOutOfRange, [MinValue, MaxValue]);
          PByte(Address)^ := Value;
        end;
      otSWord:
        begin
          MinValue := GetTypeData(TypeInfo).MinValue;
          MaxValue := GetTypeData(TypeInfo).MaxValue;
          if (Value < MinValue) or (Value > MaxValue) then
            raise ERangeError.CreateFmt(SOutOfRange, [MinValue, MaxValue]);
          PSmallint(Address)^ := Value;
        end;
      otUWord:
        begin
          MinValue := GetTypeData(TypeInfo).MinValue;
          MaxValue := GetTypeData(TypeInfo).MaxValue;
          if (Value < MinValue) or (Value > MaxValue) then
            raise ERangeError.CreateFmt(SOutOfRange, [MinValue, MaxValue]);
          PWord(Address)^ := Value;
        end;
      otSLong:
        begin
          MinValue := GetTypeData(TypeInfo).MinValue;
          MaxValue := GetTypeData(TypeInfo).MaxValue;
          if (Value < MinValue) or (Value > MaxValue) then
            raise ERangeError.CreateFmt(SOutOfRange, [MinValue, MaxValue]);
          PLongint(Address)^ := Value;
        end;
      otULong:
        begin
          MinValue := Longword(GetTypeData(TypeInfo).MinValue);
          MaxValue := Longword(GetTypeData(TypeInfo).MaxValue);
          if (Value < MinValue) or (Value > MaxValue) then
            raise ERangeError.CreateFmt(SOutOfRange, [MinValue, MaxValue]);
          PLongword(Address)^ := Value;
        end;
    end;
  end
  else if TypeInfo.Kind = tkClass then
    PLongword(Address)^ := Value
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Ordinal']);
  Invalidate;
end;

procedure TJvInspectorVarData.SetAsString(const Value: string);
begin
  CheckWriteAccess;
  if TypeInfo.Kind in [tkLString, tkWString, tkString] then
  begin
    case TypeInfo.Kind of
      tkLString:  PString(Address)^ := Value;
      tkWString:  PWideString(Address)^ := Value;
      tkString:   if Length(Value) < GetTypeData(TypeInfo).MaxLength then
                    PShortString(Address)^ := Value
                  else
                    raise EJvInspectorData.Create(sJVInspDataStrTooLong);
    end;
    Invalidate;
  end
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['String']);
end;

procedure TJvInspectorVarData.GetAsSet(var Buf);
var
  CompType: PTypeInfo;
  EnumMin: Integer;
  EnumMax: Integer;
  ResBytes: Integer;
begin
  CheckReadAccess;
  if TypeInfo.Kind = tkSet then
  begin
    CompType := GetTypeData(TypeInfo).CompType^;
    EnumMin := GetTypeData(CompType).MinValue;
    EnumMax := GetTypeData(CompType).MaxValue;
    ResBytes := (EnumMax div 8) - (EnumMin div 8) + 1;
    Move(PChar(Address)[0], Buf, ResBytes);
  end
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Set']);
end;

function TJvInspectorVarData.HasValue: Boolean;
begin
  Result := IsInitialized;
end;

function TJvInspectorVarData.IsAssigned: Boolean;
begin
  Result := IsInitialized;
end;

function TJvInspectorVarData.IsInitialized: Boolean;
begin
  Result := (TypeInfo <> nil) and (Address <> nil);
end;

class function TJvInspectorVarData.ItemRegister: TJvInspectorRegister;
begin
  if FVarItemReg = nil then
    FVarItemReg := TJvInspectorRegister.Create(TJvInspectorVarData);
  Result := FVarItemReg;
end;

class function TJvInspectorVarData.New(const AParent: TJvCustomInspectorItem; const AName: string; const ATypeInfo: PTypeInfo; const AAddress: Pointer): TJvCustomInspectorItem;
var
  Data: TJvInspectorVarData;
begin
  Data := CreatePrim(AName, ATypeInfo);
  Data.FAddress := AAddress;
  Data := TJvInspectorVarData(DataRegister.Add(Data));
  if Data <> nil then
    Result := Data.NewItem(AParent)
  else
    Result := nil;
end;

class function TJvInspectorVarData.New(const AParent: TJvCustomInspectorItem; const AName: string; const ATypeInfo: PTypeInfo; const AVar): TJvCustomInspectorItem;
begin
  Result := New(AParent, AName, ATypeInfo, Addr(AVar));
end;

procedure TJvInspectorVarData.SetAsSet(const Buf);
var
  CompType: PTypeInfo;
  EnumMin: Integer;
  EnumMax: Integer;
  ResBytes: Integer;
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkSet then
  begin
    CompType := GetTypeData(TypeInfo).CompType^;
    EnumMin := GetTypeData(CompType).MinValue;
    EnumMax := GetTypeData(CompType).MaxValue;
    ResBytes := (EnumMax div 8) - (EnumMin div 8) + 1;
    Move(Buf, PChar(Address)[0], ResBytes);
    Invalidate;
  end
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Set']);
end;

{ TJvInspectorPropData }

function TJvInspectorPropData.GetAsFloat: Extended;
begin
  CheckReadAccess;
  if Prop.PropType^.Kind = tkFloat then
    Result := GetFloatProp(Instance, Prop)
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Float']);
end;

function TJvInspectorPropData.GetAsInt64: Int64;
begin
  CheckReadAccess;
  if Prop.PropType^.Kind = tkInt64 then
    Result := GetInt64Prop(Instance, Prop)
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Int64']);
end;

function TJvInspectorPropData.GetAsMethod: TMethod;
begin
  CheckReadAccess;
  if Prop.PropType^.Kind = tkMethod then
    Result := GetMethodProp(Instance, Prop)
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Method']);
end;

function TJvInspectorPropData.GetAsOrdinal: Int64;
begin
  CheckReadAccess;
  if Prop.PropType^.Kind in [tkInteger, tkChar, tkEnumeration, tkSet,
    tkWChar, tkClass] then
  begin
    if GetTypeData(Prop.PropType^).OrdType = otULong then
      Result := Cardinal(GetOrdProp(Instance, Prop))
    else
      Result := GetOrdProp(Instance, Prop);
  end
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Ordinal']);
end;

function TJvInspectorPropData.GetAsString: string;
begin
  CheckReadAccess;
  if Prop.PropType^.Kind in [tkString, tkLString, tkWString] then
    Result := GetStrProp(Instance, Prop)
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['String']);
end;

function TJvInspectorPropData.GetInstance: TObject;
begin
  Result := FInstance;
end;

function TJvInspectorPropData.GetProp: PPropInfo;
begin
  Result := FProp;
end;

function TJvInspectorPropData.IsEqualReference(const Ref: TJvCustomInspectorData): Boolean;
begin
  Result := (Ref is TJvInspectorPropData) and (TJvInspectorPropData(Ref).Instance = Instance) and
    (TJvInspectorPropData(Ref).Prop = Prop);
end;

procedure TJvInspectorPropData.NotifyRemoveData(const Instance: TJvCustomInspectorData);
begin
  if (Instance <> Self) and (Instance.TypeInfo.Kind = tkClass) and
      (TObject(Instance.AsOrdinal) = Self.Instance) then
    Free;
end;

procedure TJvInspectorPropData.SetAsFloat(const Value: Extended);
begin
  CheckWriteAccess;
  if Prop.PropType^.Kind = tkFloat then
    SetFloatProp(Instance, Prop, Value)
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Float']);
  Invalidate;
end;

procedure TJvInspectorPropData.SetAsInt64(const Value: Int64);
begin
  CheckWriteAccess;
  if Prop.PropType^.Kind = tkInt64 then
    SetInt64Prop(Instance, Prop, Value)
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Int64']);
  Invalidate;
end;

procedure TJvInspectorPropData.SetAsMethod(const Value: TMethod);
begin
  CheckWriteAccess;
  if Prop.PropType^.Kind = tkMethod then
    SetMethodProp(Instance, Prop, Value)
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Method']);
  Invalidate;
end;

procedure TJvInspectorPropData.SetAsOrdinal(const Value: Int64);
begin
  CheckWriteAccess;
  if Prop.PropType^.Kind in [tkInteger, tkChar, tkEnumeration, tkSet,
    tkWChar, tkClass] then
  begin
    if GetTypeData(Prop.PropType^).OrdType = otULong then
      SetOrdProp(Instance, Prop, Cardinal(Value))
    else
      SetOrdProp(Instance, Prop, Value);
  end
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Ordinal']);
  Invalidate;
end;

procedure TJvInspectorPropData.SetAsString(const Value: string);
begin
  CheckWriteAccess;
  if Prop.PropType^.Kind in [tkString, tkLString, tkWString] then
    SetStrProp(Instance, Prop, Value)
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['String']);
  Invalidate;
end;

procedure TJvInspectorPropData.SetInstance(const Value: TObject);
begin
  if Instance <> Value then
  begin
    FInstance := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorPropData.SetProp(const Value: PPropInfo);
begin
  if Prop <> Value then
  begin
    FProp := Value;
    TypeInfo := Value.PropType^;
    Invalidate;
  end;
end;

procedure TJvInspectorPropData.GetAsSet(var Buf);
begin
  Integer(Buf) := AsOrdinal;
end;

function TJvInspectorPropData.HasValue: Boolean;
begin
  Result := IsInitialized;
end;

function TJvInspectorPropData.IsAssigned: Boolean;
begin
  Result := IsInitialized;
end;

function TJvInspectorPropData.IsInitialized: Boolean;
begin
  Result := (Instance <> nil) and (Prop <> nil);
end;

class function TJvInspectorPropData.ItemRegister: TJvInspectorRegister;
begin
  if FPropItemReg = nil then
    FPropItemReg := TJvInspectorRegister.Create(TJvInspectorPropData);
  Result := FPropItemReg;
end;

class function TJvInspectorPropData.New(const AParent: TJvCustomInspectorItem;
  const AInstance: TObject; const PropInfo: PPropInfo): TJvCustomInspectorItem;
var
  Data: TJvInspectorPropData;
begin
  Assert(PropInfo <> nil, 'PropInfo nil in TJvInspectorPropData.New');
  Data := CreatePrim(PropInfo.Name, PropInfo.PropType^);
  Data.Instance := AInstance;
  Data.Prop := PropInfo;
  Data := TJvInspectorPropData(DataRegister.Add(Data));
  if Data <> nil then
    Result := Data.NewItem(AParent)
  else
    Result := nil;
end;

class function TJvInspectorPropData.New(const AParent: TJvCustomInspectorItem;
  const AInstance: TObject; const TypeKinds: TTypeKinds = tkProperties): TJvInspectorItemInstances;
var
  PropCount: Integer;
  PropList: PPropList;
begin
  SetLength(Result, 0);
  PropCount := GetPropList(AInstance.ClassInfo, tkAny, nil);
  GetMem(PropList, PropCount * SizeOf(PPropInfo));
  try
    GetPropList(AInstance.ClassInfo, tkAny, PropList);
    Result := New(AParent, AInstance, PropList, PropCount);
  finally
    FreeMem(PropList);
  end;
end;

class function TJvInspectorPropData.New(const AParent: TJvCustomInspectorItem;
  const AInstance: TObject; const NameList: array of string;
  const ExcludeList: Boolean = False;
  const TypeKinds: TTypeKinds = tkProperties): TJvInspectorItemInstances;
var
  PropCount: Integer;
  PropList: PPropList;
  I: Integer;
  PropInfo: PPropInfo;
  NameIdx: Integer;
begin
  SetLength(Result, 0);
  PropCount := GetPropList(AInstance.ClassInfo, tkAny, nil);
  GetMem(PropList, PropCount * SizeOf(PPropInfo));
  try
    GetPropList(AInstance.ClassInfo, tkAny, PropList);
    for I := 0 to Pred(PropCount) do
    begin
      PropInfo := PropList[I];
      NameIdx := High(NameList);
      while (NameIdx >= 0) and not SameText(NameList[NameIdx], PropInfo.Name) do
        Dec(NameIdx);
      if (NameIdx < 0) or not ExcludeList then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := New(AParent, AInstance, PropInfo);
      end;
    end;
  finally
    FreeMem(PropList);
  end;
end;

class function TJvInspectorPropData.New(const AParent: TJvCustomInspectorItem;
  const AInstance: TObject; const PropInfos: PPropList;
  const PropCount: Integer): TJvInspectorItemInstances;
var
  I: Integer;
begin
  SetLength(Result, PropCount);
  for I := 0 to Pred(PropCount) do
    Result[I] := New(AParent, AInstance, PropInfos[I]);
end;

procedure TJvInspectorPropData.SetAsSet(const Buf);
begin
  AsOrdinal := Integer(Buf);
end;

{ TJvInspectorEventData }

function TJvInspectorEventData.DoGetAsFloat: Extended;
begin
  if @FOnGetAsFloat <> nil then
    OnGetAsFloat(Self, Result)
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Float']);
end;

function TJvInspectorEventData.DoGetAsInt64: Int64;
begin
  if @FOnGetAsInt64 <> nil then
    OnGetAsInt64(Self, Result)
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Int64']);
end;

function TJvInspectorEventData.DoGetAsMethod: TMethod;
begin
  if @FOnGetAsMethod <> nil then
    OnGetAsMethod(Self, Result)
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['TMethod']);
end;

function TJvInspectorEventData.DoGetAsOrdinal: Int64;
begin
  if @FOnGetAsOrdinal <> nil then
    OnGetAsOrdinal(Self, Result)
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['ordinal']);
end;

function TJvInspectorEventData.DoGetAsString: string;
begin
  if @FOnGetAsString <> nil then
    OnGetAsString(Self, Result)
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['string']);
end;

procedure TJvInspectorEventData.DoGetAsSet(out Buf; var BufSize: Integer);
begin
  if @FOnGetAsSet <> nil then
    OnGetAsSet(Self, Buf, BufSize)
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['set']);
end;

procedure TJvInspectorEventData.DoSetAsFloat(Value: Extended);
begin
  if @FOnSetAsFloat <> nil then
    OnSetAsFloat(Self, Value)
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Float']);
end;

procedure TJvInspectorEventData.DoSetAsInt64(Value: Int64);
begin
  if @FOnSetAsInt64 <> nil then
    OnSetAsInt64(Self, Value)
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Int64']);
end;

procedure TJvInspectorEventData.DoSetAsMethod(Value: TMethod);
begin
  if @FOnSetAsMethod <> nil then
    OnSetAsMethod(Self, Value)
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['TMethod']);
end;

procedure TJvInspectorEventData.DoSetAsOrdinal(Value: Int64);
begin
  if @FOnSetAsOrdinal <> nil then
    OnSetAsOrdinal(Self, Value)
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['ordinal']);
end;

procedure TJvInspectorEventData.DoSetAsString(Value: string);
begin
  if @FOnSetAsString <> nil then
    OnSetAsString(Self, Value)
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['string']);
end;

procedure TJvInspectorEventData.DoSetAsSet(const Buf; var BufSize: Integer);
var
  TmpBuf: PChar;
begin
  TmpBuf := @Buf;
  if @FOnSetAsSet <> nil then
    OnSetAsSet(Self, TmpBuf[0], BufSize)
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['set']);
end;

function TJvInspectorEventData.GetAsFloat: Extended;
begin
  CheckReadAccess;
  if TypeInfo.Kind = tkFloat then
    Result := DoGetAsFloat
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Float']);
end;

function TJvInspectorEventData.GetAsInt64: Int64;
begin
  CheckReadAccess;
  if TypeInfo.Kind = tkInt64 then
    Result := DoGetAsInt64
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Int64']);
end;

function TJvInspectorEventData.GetAsMethod: TMethod;
begin
  CheckReadAccess;
  if TypeInfo.Kind = tkMethod then
    Result := DoGetAsMethod
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['TMethod']);
end;

function TJvInspectorEventData.GetAsOrdinal: Int64;
begin
  CheckReadAccess;
  if TypeInfo.Kind in [tkInteger, tkChar, tkEnumeration, tkSet, tkWChar] then
  begin
    case GetTypeData(TypeInfo).OrdType of
      otSByte:  Result := Shortint(DoGetAsOrdinal);
      otUByte:  Result := Byte(DoGetAsOrdinal);
      otSWord:  Result := Smallint(DoGetAsOrdinal);
      otUWord:  Result := Word(DoGetAsOrdinal);
      otSLong:  Result := Longint(DoGetAsOrdinal);
      otULong:  Result := Longword(DoGetAsOrdinal);
      else      Result := 0;
    end;
  end
  else if TypeInfo.Kind = tkClass then
    Result := Longword(DoGetAsOrdinal)
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['ordinal']);
end;

function TJvInspectorEventData.GetAsString: string;
begin
  CheckReadAccess;
  if TypeInfo.Kind in [tkString, tkLString, tkWString] then
    Result := DoGetAsString
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['string']);
end;

function TJvInspectorEventData.IsEqualReference(const Ref: TJvCustomInspectorData): Boolean;
begin
  Result := (Ref is TJvInspectorEventData) and (TJvInspectorEventData(Ref).Name = Name) and
    (TJvInspectorEventData(Ref).TypeInfo = TypeInfo);
end;

procedure TJvInspectorEventData.SetAsFloat(const Value: Extended);
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkFloat then
    DoSetAsFloat(Value)
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Float']);
  Invalidate;
end;

procedure TJvInspectorEventData.SetAsInt64(const Value: Int64);
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkInt64 then
  begin
    if (Value < GetTypeData(TypeInfo).MinValue) or
        (Value > GetTypeData(TypeInfo).MaxValue) then
      raise ERangeError.CreateFmt(SOutOfRange, [GetTypeData(TypeInfo).MinValue,
        GetTypeData(TypeInfo).MaxValue]);
    DoSetAsInt64(Value);
  end
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Int64']);
  Invalidate;
end;

procedure TJvInspectorEventData.SetAsMethod(const Value: TMethod);
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkMethod then
    DoSetAsMethod(Value)
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['TMethod']);
  Invalidate;
end;

procedure TJvInspectorEventData.SetAsOrdinal(const Value: Int64);
var
  MinValue: Int64;
  MaxValue: Int64;
begin
  CheckWriteAccess;
  if TypeInfo.Kind in [tkInteger, tkChar, tkEnumeration, tkWChar] then
  begin
    if GetTypeData(TypeInfo).OrdType <> otULong then
    begin
      MinValue := GetTypeData(TypeInfo).MinValue;
      MaxValue := GetTypeData(TypeInfo).MaxValue;
    end
    else
    begin
      MinValue := Longword(GetTypeData(TypeInfo).MinValue);
      MaxValue := Longword(GetTypeData(TypeInfo).MaxValue);
    end;
    if (Value < MinValue) or (Value > MaxValue) then
      raise ERangeError.CreateFmt(SOutOfRange, [MinValue, MaxValue]);
    case GetTypeData(TypeInfo).OrdType of
      otSByte:
        DoSetAsOrdinal(Shortint(Value));
      otUByte:
        DoSetAsOrdinal(Byte(Value));
      otSWord:
        DoSetAsOrdinal(Smallint(Value));
      otUWord:
        DoSetAsOrdinal(Word(Value));
      otSLong:
        DoSetAsOrdinal(Longint(Value));
      otULong:
        DoSetAsOrdinal(Longword(Value));
    end;
  end
  else if TypeInfo.Kind = tkClass then
    DoSetAsOrdinal(Longword(Value))
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['ordinal']);
  Invalidate;
end;

procedure TJvInspectorEventData.SetAsString(const Value: string);
begin
  CheckWriteAccess;
  if TypeInfo.Kind in [tkLString, tkWString, tkString] then
  begin
    case TypeInfo.Kind of
      tkLString:  DoSetAsString(Value);
      tkWString:  DoSetAsString(Value);
      tkString:   if Length(Value) < GetTypeData(TypeInfo).MaxLength then
                    DoSetAsString(Value)
                  else
                    raise EJvInspectorData.Create(sJVInspDataStrTooLong);
    end;
  end
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['string']);
  Invalidate;
end;

procedure TJvInspectorEventData.SetOnGetAsFloat(Value: TJvInspAsFloat);
begin
  if @FOnGetAsFloat <> @Value then
  begin
    FOnGetAsFloat := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorEventData.SetOnGetAsInt64(Value: TJvInspAsInt64);
begin
  if @FOnGetAsInt64 <> @Value then
  begin
    FOnGetAsInt64 := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorEventData.SetOnGetAsMethod(Value: TJvInspAsMethod);
begin
  if @FOnGetAsMethod <> @Value then
  begin
    FOnGetAsMethod := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorEventData.SetOnGetAsOrdinal(Value: TJvInspAsInt64);
begin
  if @FOnGetAsOrdinal <> @Value then
  begin
    FOnGetAsOrdinal := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorEventData.SetOnGetAsString(Value: TJvInspAsString);
begin
  if @FOnGetAsString <> @Value then
  begin
    FOnGetAsString := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorEventData.SetOnGetAsSet(Value: TJvInspAsSet);
begin
  if @FOnGetAsSet <> @Value then
  begin
    FOnGetAsSet := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorEventData.SetOnSetAsFloat(Value: TJvInspAsFloat);
begin
  if @FOnSetAsFloat <> @Value then
  begin
    FOnSetAsFloat := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorEventData.SetOnSetAsInt64(Value: TJvInspAsInt64);
begin
  if @FOnSetAsInt64 <> @Value then
  begin
    FOnSetAsInt64 := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorEventData.SetOnSetAsMethod(Value: TJvInspAsMethod);
begin
  if @FOnSetAsMethod <> @Value then
  begin
    FOnSetAsMethod := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorEventData.SetOnSetAsOrdinal(Value: TJvInspAsInt64);
begin
  if @FOnSetAsOrdinal <> @Value then
  begin
    FOnSetAsOrdinal := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorEventData.SetOnSetAsString(Value: TJvInspAsString);
begin
  if @FOnSetAsString <> @Value then
  begin
    FOnSetAsString := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorEventData.SetOnSetAsSet(Value: TJvInspAsSet);
begin
  if @FOnSetAsSet <> @Value then
  begin
    FOnSetAsSet := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorEventData.GetAsSet(var Buf);
var
  CompType: PTypeInfo;
  EnumMin: Integer;
  EnumMax: Integer;
  ResBytes: Integer;
begin
  CheckReadAccess;
  if TypeInfo.Kind = tkSet then
  begin
    CompType := GetTypeData(TypeInfo).CompType^;
    EnumMin := GetTypeData(CompType).MinValue;
    EnumMax := GetTypeData(CompType).MaxValue;
    ResBytes := (EnumMax div 8) - (EnumMin div 8) + 1;
    DoGetAsSet(Buf, ResBytes);
  end
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['set']);
end;

function TJvInspectorEventData.HasValue: Boolean;
begin
  Result := IsInitialized;
end;

function TJvInspectorEventData.IsAssigned: Boolean;
begin
  Result := IsInitialized;
end;

function TJvInspectorEventData.IsInitialized: Boolean;
begin
  Result := (TypeInfo <> nil) and ((@OnGetAsFloat <> nil) or (@OnGetAsInt64 <> nil) or
    (@OnGetAsMethod <> nil) or (@OnGetAsOrdinal <> nil) or (@OnGetAsString <> nil) or
    (@OnGetAsSet <> nil));
end;

class function TJvInspectorEventData.New(const AParent: TJvCustomInspectorItem; const AName: string;
  const ATypeInfo: PTypeInfo): TJvCustomInspectorItem;
var
  Data: TJvInspectorEventData;
begin
  Data := TJvInspectorEventData(DataRegister.Add(CreatePrim(AName, ATypeInfo)));
  if Data <> nil then
    Result := Data.NewItem(AParent)
  else
    Result := nil;
end;

procedure TJvInspectorEventData.SetAsSet(const Buf);
var
  CompType: PTypeInfo;
  EnumMin: Integer;
  EnumMax: Integer;
  ResBytes: Integer;
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkSet then
  begin
    CompType := GetTypeData(TypeInfo).CompType^;
    EnumMin := GetTypeData(CompType).MinValue;
    EnumMax := GetTypeData(CompType).MaxValue;
    ResBytes := (EnumMax div 8) - (EnumMin div 8) + 1;
    DoSetAsSet(Buf, ResBytes);
  end
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['set']);
  Invalidate;
end;

{ TJvInspectorCustomConfData }

constructor TJvInspectorCustomConfData.CreatePrim(const AName, ASection, AKey: string;
  const ATypeInfo: PTypeInfo);
begin
  inherited CreatePrim(AName, ATypeInfo);
  FKey := AKey;
  FSection := ASection;
end;

function TJvInspectorCustomConfData.GetAsFloat: Extended;
begin
  CheckReadAccess;
  if TypeInfo.Kind = tkFloat then
    Result := StrToFloat(Trim(StringReplace(ReadValue, ThousandSeparator, DecimalSeparator, [rfReplaceAll, rfIgnoreCase])))
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Float']);
end;

function TJvInspectorCustomConfData.GetAsInt64: Int64;
begin
  CheckReadAccess;
  if TypeInfo.Kind = tkInt64 then
    Result := StrToInt64(ReadValue)
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Int64']);
end;

function TJvInspectorCustomConfData.GetAsMethod: TMethod;
begin
  CheckReadAccess;
  raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['TMethod']);
end;

function TJvInspectorCustomConfData.GetAsOrdinal: Int64;
var
  S: string;
begin
  CheckReadAccess;
  S := ReadValue;
  case TypeInfo.Kind of
    tkInteger:
      begin
        case GetTypeData(TypeInfo).OrdType of
          otSByte:  Result := Shortint(StrToInt(S));
          otUByte:  Result := Byte(StrToInt(S));
          otSWord:  Result := Smallint(StrToInt(S));
          otUWord:  Result := Word(StrToInt(S));
          otSLong:  Result := Longint(StrToInt(S));
          otULong:  Result := Longword(StrToInt(S));
          else      Result := 0;
        end;
      end;
    tkChar,
    tkWChar:
      begin
        if Length(S) > 1 then
          Result := StrToInt(Copy(S, 2, Length(S)))
        else if Length(S) = 1 then
          Result := Ord(S[1])
        else
          Result := 0;
      end;
    tkEnumeration:
      Result := GetEnumValue(TypeInfo, S);
    tkSet:
      GetAsSet(Result);
    else
      raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['ordinal']);
  end;
end;

function TJvInspectorCustomConfData.GetAsString: string;
begin
  CheckReadAccess;
  if TypeInfo.Kind in [tkString, tkWString, tkLString] then
    Result := ReadValue
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['string']);
end;

function TJvInspectorCustomConfData.IsEqualReference(const Ref: TJvCustomInspectorData): Boolean;
begin
  Result := (Ref is TJvInspectorCustomConfData) and
    AnsiSameText(TJvInspectorCustomConfData(Ref).Section, Section) and
    AnsiSameText(TJvInspectorCustomConfData(Ref).Key, Key);
end;

procedure TJvInspectorCustomConfData.SetAsFloat(const Value: Extended);
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkFloat then
    WriteValue(FloatToStr(Value))
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Float']);
  Invalidate;
end;

procedure TJvInspectorCustomConfData.SetAsInt64(const Value: Int64);
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkInt64 then
    WriteValue(IntToStr(Value))
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['Int64']);
  Invalidate;
end;

procedure TJvInspectorCustomConfData.SetAsMethod(const Value: TMethod);
begin
  CheckWriteAccess;
  raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['TMethod']);
end;

procedure TJvInspectorCustomConfData.SetAsOrdinal(const Value: Int64);
begin
  CheckWriteAccess;
  case TypeInfo.Kind of
    tkInteger:
      WriteValue(IntToStr(Value));
    tkChar,
    tkWChar:
      begin
        if (Value <= Ord(' ')) or (Value > Ord('~')) then
          WriteValue('#' + IntToStr(Value))
        else
          WriteValue(Chr(Byte(Value)));
      end;
    tkEnumeration:
      WriteValue(GetEnumName(TypeInfo, Value));
    tkSet:
      SetAsSet(Value);
    else
      raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['ordinal']);
  end;
  Invalidate;
end;

procedure TJvInspectorCustomConfData.SetAsString(const Value: string);
begin
  CheckWriteAccess;
  case TypeInfo.Kind of
    tkString:
      begin
        if Length(Value) < GetTypeData(TypeInfo).MaxLength then
          WriteValue(Value)
        else
          raise EJvInspectorData.Create(sJVInspDataStrTooLong);
      end;
    tkLString,
    tkWString:
      WriteValue(Value)
    else
      raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['string']);
  end;
  Invalidate;
end;

procedure TJvInspectorCustomConfData.SetKey(Value: string);
begin
  if Value <> Key then
  begin
    FKey := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorCustomConfData.SetSection(Value: string);
begin
  if Value <> Section then
  begin
    FSection := Value;
    Invalidate;
  end;
end;

procedure TJvInspectorCustomConfData.GetAsSet(var Buf);
begin
  CheckReadAccess;
  if TypeInfo.Kind = tkSet then
    JclStrToSet(TypeInfo, Buf, ReadValue)
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['set']);
end;

function TJvInspectorCustomConfData.HasValue: Boolean;
begin
  Result := IsInitialized;
end;

function TJvInspectorCustomConfData.IsAssigned: Boolean;
begin
  Result := IsInitialized and ExistingValue;
end;

function TJvInspectorCustomConfData.IsInitialized: Boolean;
begin
  Result := (Key <> '') and (Section <> '');
end;

procedure TJvInspectorCustomConfData.SetAsSet(const Buf);
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkSet then
    WriteValue(JclSetToStr(TypeInfo, Buf, True, False))
  else
    raise EJvInspectorData.CreateFmt(sJvInspDataNoAccessAs, ['set']);
  Invalidate;
end;

{ TJvInspectorINIFileData }

function TJvInspectorINIFileData.ExistingValue: Boolean;
begin
  Result := IsInitialized and INIFile.SectionExists(Section) and INIFile.ValueExists(Section, Key);
end;

function TJvInspectorINIFileData.IsEqualReference(const Ref: TJvCustomInspectorData): Boolean;
begin
  Result := (Ref is TJvInspectorINIFileData) and
    (TJvInspectorINIFileData(Ref).INIFile = INIFile) and inherited IsEqualReference(Ref);
end;

function TJvInspectorINIFileData.ReadValue: string;
begin
  Result := INIFile.ReadString(Section, Key, '');
end;

procedure TJvInspectorINIFileData.WriteValue(Value: string);
begin
  INIFile.WriteString(Section, Key, Value);
end;

class function TJvInspectorINIFileData.New(const AParent: TJvCustomInspectorItem; const AName,
  ASection, AKey: string; const ATypeInfo: PTypeInfo;
  const AINIFile: TCustomIniFile): TJvCustomInspectorItem;
var
  Data: TJvInspectorINIFileData;
begin
  Assert(AINIFile <> nil);
  Data := CreatePrim(AName, ASection, AKey, ATypeInfo);
  Data.FINIFile := AINIFile;
  Data := TJvInspectorINIFileData(DataRegister.Add(Data));
  if Data <> nil then
    Result := Data.NewItem(AParent)
  else
    Result := nil;
end;

class function TJvInspectorINIFileData.New(const AParent: TJvCustomInspectorItem;
  const ASection: string; const AINIFile: TCustomIniFile;
  const OnAddKey: TJvInspConfKeyEvent): TJvInspectorItemInstances;
var
  SL: TStrings;
  I: Integer;
  KeyName: string;
  KeyTypeInfo: PTypeInfo;
  TmpItem: TJvCustomInspectorItem;

  function AllowAddKey: Boolean;
  begin
    KeyName := SL[I];
    KeyTypeInfo := System.TypeInfo(string);
    Result := True;
    if @OnAddKey <> nil then
      OnAddKey(ASection, KeyName, KeyTypeInfo, Result);
  end;

begin
  Assert(AINIFile <> nil);
  SetLength(Result, 0);
  SL := TStringList.Create;
  try
    AINIFile.ReadSection(ASection, SL);
    for I := 0 to SL.Count - 1 do
    begin
      if AllowAddKey then
      begin
        TmpItem := TJvInspectorINIFileData.New(AParent, KeyName, ASection, SL[I], KeyTypeInfo,
          AINIFile);
        if TmpItem <> nil then
        begin
          SetLength(Result, Length(Result) + 1);
          Result[High(Result)] := TmpItem;
        end;
      end;
    end;
  finally
    SL.Free;
  end;
end;

class function TJvInspectorINIFileData.New(const AParent: TJvCustomInspectorItem;
  const AINIFile: TCustomIniFile; const OnAddSection: TJvInspConfSectionEvent;
  const OnAddKey: TJvInspConfKeyEvent): TJvInspectorItemInstances;
var
  TmpLst: TJvInspectorItemInstances;
  SL: TStrings;
  I: Integer;
  CatName: string;
  CatItem: TJvInspectorCustomCategoryItem;

  function AllowAddSection: Boolean;
  begin
    CatName := SL[I];
    Result := True;
    if @OnAddSection <> nil then
      OnAddSection(CatName, Result);
  end;

begin
  SetLength(TmpLst, 0);
  Assert(AINIFile <> nil);
  SL := TStringList.Create;
  try
    AINIFile.ReadSections(SL);
    for I := 0 to SL.Count - 1 do
    begin
      if AllowAddSection then
      begin
        CatItem := TJvInspectorCustomCategoryItem.Create(AParent, nil);
        CatItem.DisplayName := CatName;
        TmpLst := TJvInspectorINIFileData.New(CatItem, SL[I], AINIFile, OnAddKey);
        SetLength(Result, Length(Result) + Length(TmpLst));
        Move(TmpLst[0], Result[Length(Result) - Length(TmpLst)], Length(TmpLst));
        if CatItem.Count = 0 then
          CatItem.Parent.Delete(CatItem);
      end;
    end;
  finally
    SL.Free;
  end;
end;

{ TJvInspectorRegister }

function TJvInspectorRegister.Compare(
  const ADataObj: TJvCustomInspectorData; const Item1,
  Item2: TJvCustomInspectorRegItem): Integer;
begin
  Result := Item1.Compare(ADataObj, Item2);
end;

function TJvInspectorRegister.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJvInspectorRegister.GetItems(
  const I: Integer): TJvCustomInspectorRegItem;
begin
  Result := TJvCustomInspectorRegItem(FItems[I]);
end;

constructor TJvInspectorRegister.Create(
  const ADataClass: TJvInspectorDataClass);
begin
  inherited Create;
  FDataClass := ADataClass;
  FItems := TObjectList.Create(True);
end;

destructor TJvInspectorRegister.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TJvInspectorRegister.Add(
  const RegItem: TJvCustomInspectorRegItem);
begin
  FItems.Add(RegItem);
end;

procedure TJvInspectorRegister.Delete(
  const RegItem: TJvCustomInspectorRegItem);
begin
  FItems.Remove(RegItem);
end;

function TJvInspectorRegister.FindMatch(
  const ADataObj: TJvCustomInspectorData): TJvCustomInspectorRegItem;
var
  I: Integer;
  ParDataClass: TJvInspectorDataClass;
  ParResult: TJvCustomInspectorRegItem;
begin
  Result := nil;
  for I := Pred(Count) downto 0 do
  begin
    if Items[I].IsMatch(ADataObj) then
    begin
      if Result = nil then
        Result := Items[I]
      else if Compare(ADataObj, Result, Items[I]) < 0 then
        Result := Items[I];
    end;
  end;
  if (Result = nil) or (Result.MatchPercent(ADataObj) <> 100) then
  begin
    ParDataClass := TJvInspectorDataClass(DataClass.ClassParent);
    while (ParDataClass <> nil) and
        ParDataClass.InheritsFrom(TJvCustomInspectorData) and
        (ParDataClass.ItemRegister = Self) do
      ParDataClass := TJvInspectorDataClass(ParDataClass.ClassParent);
    if (ParDataClass <> nil) and
      ParDataClass.InheritsFrom(TJvCustomInspectorData) and
      (ParDataClass.ItemRegister <> Self) then
    begin
      ParResult := ParDataClass.ItemRegister.FindMatch(ADataObj);
      if (ParResult <> nil) and (((Result <> nil) and
          (Result.Compare(ADataObj, ParResult) < 0)) or (Result = nil)) then
        Result := ParResult;
    end;
  end;
end;

{ TJvCustomInspectorRegItem }

function TJvCustomInspectorRegItem.CompareTo(
  const ADataObj: TJvCustomInspectorData;
  const Item: TJvCustomInspectorRegItem): Integer;
begin
  if MatchPercent(ADataObj) > Item.MatchPercent(ADataObj) then
    Result := MatchPercent(ADataObj)
  else
    Result := -Item.MatchPercent(ADataObj)
end;

function TJvCustomInspectorRegItem.GetItemClass: TJvInspectorItemClass;
begin
  Result := FItemClass;
end;

procedure TJvCustomInspectorRegItem.SetItemClass(
  const Value: TJvInspectorItemClass);
begin
  FItemClass := Value;
end;

constructor TJvCustomInspectorRegItem.Create(
  const AItemClass: TJvInspectorItemClass);
begin
  inherited Create;
  FItemClass := AItemClass;
end;

procedure TJvCustomInspectorRegItem.ApplyDefaults(
  const Item: TJvCustomInspectorItem);
begin
  { Override in descendants to apply special defaults }
end;

function TJvCustomInspectorRegItem.Compare(
  const ADataObj: TJvCustomInspectorData;
  const Item: TJvCustomInspectorRegItem): Integer;
begin
  if ClassType = Item.ClassType then
  begin
    if MatchValue(ADataObj) >= Item.MatchValue(ADataObj) then
      Result := MatchValue(ADataObj)
    else
      Result := -Item.MatchValue(ADataObj);
  end
  else
    Result := -Item.CompareTo(ADataObj, Self);
end;

function TJvCustomInspectorRegItem.IsMatch(
  const ADataObj: TJvCustomInspectorData): Boolean;
begin
  Result := MatchValue(ADataObj) <> 0;
end;

{ TJvInspectorTypeInfoRegItem }

function TJvInspectorTypeInfoRegItem.GetTypeInfo: PTypeInfo;
begin
  Result := FTypeInfo;
end;

procedure TJvInspectorTypeInfoRegItem.SetTypeInfo(const Value: PTypeInfo);
begin
  FTypeInfo := Value;
end;

constructor TJvInspectorTypeInfoRegItem.Create(
  const AItemClass: TJvInspectorItemClass; const ATypeInfo: PTypeInfo);
begin
  inherited Create(AItemClass);
  FTypeInfo := ATypeInfo;
end;

function TJvInspectorTypeInfoRegItem.MatchValue(
  const ADataObj: TJvCustomInspectorData): Integer;
begin
  if ADataObj.TypeInfo = TypeInfo then
    Result := 100
  else if (TypeInfo.Kind = tkClass) and
      (ADataObj.TypeInfo.Kind = tkClass) and
      (GetTypeData(ADataObj.TypeInfo).ClassType.InheritsFrom(
        GetTypeData(TypeInfo).ClassType)) then
    Result := 50
  else
    Result := 0;
end;

function TJvInspectorTypeInfoRegItem.MatchPercent(const ADataObj: TJvCustomInspectorData): Integer;
begin
  { Matching TypeInfo is a perfect match. Since MatchValue already returns a
    percentage, just return that value. }
  Result := MatchValue(ADataObj);
end;

{ TJvInspectorTCaptionRegItem }

procedure TJvInspectorTCaptionRegItem.ApplyDefaults(
  const Item: TJvCustomInspectorItem);
begin
  if Item <> nil then
  begin
    Item.AutoUpdate := True;
    Item.Flags := Item.Flags + [iifMultiLine];
    Item.RowSizing.SizingFactor := irsValueHeight;
    Item.RowSizing.MinHeight := irsItemHeight;
    Item.RowSizing.Sizable := True;
  end;
end;

{ TJvInspectorTypeKindRegItem }

function TJvInspectorTypeKindRegItem.CompareTo(
  const ADataObj: TJvCustomInspectorData;
  const Item: TJvCustomInspectorRegItem): Integer;
begin
  if Item is TJvInspectorTypeInfoRegItem then
    Result := -Item.MatchValue(ADataObj)
  else
    Result := inherited CompareTo(ADataObj, Item);
end;

function TJvInspectorTypeKindRegItem.GetTypeKind: TTypeKind;
begin
  Result := FTypeKind;
end;

procedure TJvInspectorTypeKindRegItem.SetTypeKind(const Value: TTypeKind);
begin
  FTypeKind := Value;
end;

constructor TJvInspectorTypeKindRegItem.Create(
  const AItemClass: TJvInspectorItemClass; const ATypeKind: TTypeKind);
begin
  inherited Create(AItemClass);
  FTypeKind := ATypeKind;
end;

function TJvInspectorTypeKindRegItem.Compare(
  const ADataObj: TJvCustomInspectorData;
  const Item: TJvCustomInspectorRegItem): Integer;
begin
  if Item is TJvInspectorTypeInfoRegItem then
  begin
    if MatchValue(ADataObj) >= Item.MatchValue(ADataObj) then
      Result := MatchValue(ADataObj)
    else
      Result := -Item.MatchValue(ADataObj);
  end
  else
    Result := inherited Compare(ADataObj, Item);
end;

function TJvInspectorTypeKindRegItem.MatchValue(
  const ADataObj: TJvCustomInspectorData): Integer;
begin
  if ADataObj.TypeInfo.Kind = TypeKind then
    Result := 100
  else
    Result := 0;
end;

function TJvInspectorTypeKindRegItem.MatchPercent(const ADataObj: TJvCustomInspectorData): Integer;
begin
  { Matching TypeKind is 50% match. Since MatchValue returns either 0 or 100,
    devide it by two to get 0 or 50. }
  Result := MatchValue(ADataObj) div 2;
end;

{ TJvInspectorPropRegItem }

function TJvInspectorPropRegItem.Compare(const ADataObj: TJvCustomInspectorData;
  const Item: TJvCustomInspectorRegItem): Integer;
begin
  if not (Item is TJvInspectorPropRegItem) then
    Result := MatchValue(ADataObj)
  else
    Result := inherited Compare(ADataObj, Item);
end;

constructor TJvInspectorPropRegItem.Create(
  const AItemClass: TJvInspectorItemClass; const AObjectClass: TClass;
  const AName: string; const ATypeInfo: PTypeInfo);
begin
  inherited Create(AItemClass);
  FObjectClass := AObjectClass;
  FName := AName;
  FTypeInfo := ATypeInfo;
end;

function TJvInspectorPropRegItem.MatchValue(
  const ADataObj: TJvCustomInspectorData): Integer;
var
  GoOn: Boolean;
  ObjParentClass: TClass;
begin
  { Match value will be based on the all set items according to the following
    table:

    Base value is 0
    * ClassType known
      * class type equal:           add 32
      * class type inherits:        add 16
      * class does not match:       return 0
    * Name known
      * Name exact match:           add  8
      * Name matches by mask:       add  4
      * Name does not match:        return 0
    * Type info known
      * Typeinfo exact match:       add  2
      * Typeinfo typekind matches:  add  1
      * Typeinfo does not match:    return 0
     }
  Result := 0;
  GoOn := True;
  if TypeInfo <> nil then
  begin
    if TypeInfo = ADataObj.TypeInfo then
      Result := Result or 2
    else if TypeInfo.Kind = ADataObj.TypeInfo.Kind then
    begin
      if (TypeInfo.Kind <> tkClass) or
          (GetTypeData(ADataObj.TypeInfo).ClassType.InheritsFrom(
            GetTypeData(TypeInfo).ClassType)) then
        Result := Result or 1
      else
        GoOn := False;
    end
    else
      GoOn := False;
  end;

  if GoOn and (Name <> '') then
  begin
    if SameText(Name, ADataObj.Name) then
      Result := Result or 8
    { Match by mask }
    else
      GoOn := False;
  end;

  if GoOn and (ObjectClass <> nil) then
  begin
    { Class type based on the parent object }
    ObjParentClass := TJvInspectorPropData(ADataObj).Instance.ClassType;
    if ObjParentClass = ObjectClass then
      Result := Result or 32
    else if (ObjParentClass <> nil) and ObjParentClass.InheritsFrom(ObjectClass) then
      Result := Result or 16
    else
      GoOn := False;
  end;

  if not GoOn then
    Result := 0;
end;

function TJvInspectorPropRegItem.MatchPercent(const ADataObj: TJvCustomInspectorData): Integer;
var
  MV: Integer;
begin
  { A 100% score would mean that Class, Name and TypeInfo all were a perfect
    match. }
  Result := 100;
  MV := MatchValue(ADataObj);
  if MV = 0 then
    Result := 0
  else
  begin
    if ObjectClass <> nil then
    begin
      if (MV and 16) <> 0 then
        Result := Result div 2;
    end
    else
      Dec(Result, 8);

    if Name <> '' then
    begin
      if (MV and 4) <> 0 then
        Result := Result div 2;
    end
    else
      Dec(Result, 4);

    if TypeInfo <> nil then
    begin
      if (MV and 1) <> 0 then
        Result := Result div 2;
    end
    else
      Dec(Result, 8);
  end;
end;

procedure RegisterTypeKinds;
begin
  if TJvCustomInspectorData.ItemRegister = nil then
    raise EJvInspectorReg.Create(sJvInspNoGenReg);
  with TJvCustomInspectorData.ItemRegister do
  begin
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorStringItem, tkLString));
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorStringItem, tkWString));
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorStringItem, tkString));
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorIntegerItem, tkInteger));
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorEnumItem, tkEnumeration));
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorFloatItem, tkFloat));
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorSetItem, tkSet));
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorCharItem, tkChar));
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorCharItem, tkWChar));
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorInt64Item, tkInt64));
    Add(TJvInspectorTypeKindRegItem.Create(TJvInspectorClassItem, tkClass));
    Add(TJvInspectorTCaptionRegItem.Create(TJvInspectorStringItem, TypeInfo(TCaption)));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorFontItem, TypeInfo(TFont)));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorBooleanItem, TypeInfo(Boolean)));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorBooleanItem, TypeInfo(BYTEBOOL)));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorBooleanItem, TypeInfo(WORDBOOL)));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorBooleanItem, TypeInfo(LONGBOOL)));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorStringItem, TypeInfo(TStrings)));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorComponentItem, TypeInfo(TComponent)));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorDateItem, TypeInfo(TDate)));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorTimeItem, TypeInfo(TTime)));
    Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorDateTimeItem, TypeInfo(TDateTime)));
  end;
  with TJvInspectorPropData.ItemRegister do
    Add(TJvInspectorPropRegItem.Create(TJvInspectorFontNameItem, TFont, 'Name', nil));
end;

const
  SizingConsts: array[0..3] of TIdentMapEntry = (
    (Value: irsNoReSize; Name: 'irsNoReSize'),
    (Value: irsNameHeight; Name: 'irsNameHeight'),
    (Value: irsValueHeight; Name: 'irsValueHeight'),
    (Value: irsItemHeight; Name: 'irsItemHeight')
  );

function irsToInt(const Ident: string; var Int: Longint): Boolean;
begin
  Result := IdentToInt(Ident, Int, SizingConsts);
end;

function IntToirs(Int: Longint; var Ident: string): Boolean;
begin
  Result := IntToIdent(Int, Ident, SizingConsts);
end;

procedure RegisterConsts;
begin
  RegisterIntegerConsts(TypeInfo(TItemRowSizing), irsToInt, IntToirs);
end;

initialization
  CanvasStack := TCanvasStack.Create(512);
  RegisterTypeKinds;
  RegisterConsts;
  DataRegister := TJvInspDataReg.Create;

finalization
  DataRegister.Free; // Can't use FreeAndNil as it will set DataRegister to nil before it's destroyed.
  DataRegister := nil;
  TJvCustomInspectorData.ItemRegister.Free;
  TJvInspectorPropData.ItemRegister.Free;
  TJvInspectorVarData.ItemRegister.Free;
  CanvasStack.Free;
end.
