{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvListView.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].
                dejoy
                Olivier Sannier [obones att altern dott org]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
  Mantis 3932: In the OnCustomDrawItem, if you change the canvas font directly,
               then your changes will be ignored and the items be drawn bold if
               the item brush is not used for the given list view style
               (report for instance). As a workaround, always change the item's
               properties, never the canvas' directly.
-----------------------------------------------------------------------------}
// $Id$

unit JvListView;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ComCtrls, CommCtrl, Menus, ImgList, Clipbrd, ListActns,
  JvJCLUtils, JvJVCLUtils, JvTypes, JvExComCtrls, JvAppStorage;

type
  TJvViewStyle = (vsIcon, vsSmallIcon, vsList, vsReport, vsTile);
  TJvHeaderImagePosition = (hipLeft, hipRight);

const
  WM_AUTOSELECT = WM_USER + 1;
  ALL_VIEW_STYLES = [vsIcon, vsSmallIcon, vsList, vsReport, vsTile];

type
  TJvListView = class;
  TJvListItem = class;
  {$IFNDEF RTL200_UP}
  TJvListViewGroup = class;
  {$ENDIF !RTL200_UP}
  EJvListViewError = EJVCLException;

  // Mantis 980: new type for Groups
  TLVGROUP = record
    cbSize: UINT;
    mask: UINT;
    pszHeader: LPWSTR;
    cchHeader: Integer;
    pszFooter: LPWSTR;
    cchFooter: Integer;
    iGroupId: Integer;
    stateMask: UINT;
    state: UINT;
    uAlign: UINT;
  end;

  //  TJvSortMethod = (smAutomatic, smAlphabetic, smNonCaseSensitive, smNumeric, smDate, smTime, smDateTime, smCurrency);
  TJvOnProgress = procedure(Sender: TObject; Progression, Total: Integer) of object;
  TListViewItemClickNotifyEvent = procedure(Sender: TObject; Item: TListItem; SubItemIndex: Integer; X, Y: Integer) of object;
  {$IFNDEF RTL200_UP}
  TJvListViewCompareGroupEvent = procedure(Sender: TObject; Group1, Group2: TJvListViewGroup; var Compare: Integer) of object;
  {$ENDIF !RTL200_UP}
  TJvListViewCancelEditEvent = procedure(Sender: TObject; Item: TListItem) of object;
  TJvListViewBeginColumnResizeEvent = procedure(Sender: TCustomListview; ColumnIndex: Integer; ColumnWidth: Integer; var CanResize: Boolean) of object;
  TJvListViewColumnResizeEvent = procedure(Sender: TCustomListview; ColumnIndex: Integer; ColumnWidth: Integer) of Object;

  TJvListItems = class(TListItems, IJvAppStorageHandler, IJvAppStoragePublishedProps)
  private
    FOwnerInterface: IInterface;
    function GetItem(Index: Integer): TJvListItem;
    procedure SetItem(Index: Integer; const Value: TJvListItem);
  protected
    { IInterface }
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IJvAppStorageHandler }
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);

    { List item reader used in the call to ReadList. }
    procedure ReadListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const Index: Integer; const ItemName: string);
    { List item writer used in the call to WriteList. }
    procedure WriteListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const Index: Integer; const ItemName: string);
    { List item deleter used in the call to WriteList. }
    procedure DeleteListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const First, Last: Integer; const ItemName: string);
  public
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; virtual; stdcall;
    procedure AfterConstruction; override;

    function Add: TJvListItem;
    function AddItem(Item: TJvListItem; Index: Integer = -1): TJvListItem;
    function Insert(Index: Integer): TJvListItem;
    property Item[Index: Integer]: TJvListItem read GetItem write SetItem; default;
  end;

  TJvListItem = class(TListItem)
  private
    FPopupMenu: TPopupMenu;
    FBold: Boolean;
    FFont: TFont;
    FBrush: TBrush;
    FGroupId: Integer;
    FTileColumns: TIntegerList;
    procedure SetBrush(const Value: TBrush);
    procedure SetGroupId(const Value: Integer);
    procedure SetTileColumns(const Value: TIntegerList);

    procedure ReadTileColumns(Reader: TReader);
    procedure WriteTileColumns(Writer: TWriter);
    procedure TileColumnsChange(Sender: TObject; Item: Integer; Action: TListNotification);
  protected
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure SetFont(const Value: TFont);
    procedure UpdateTileColumns;
  public
    constructor CreateEnh(AOwner: TListItems; const Popup: TPopupMenu);
    destructor Destroy; override;

    procedure DefineProperties(Filer: TFiler); override;

    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property TileColumns: TIntegerList read FTileColumns write SetTileColumns;
  published
    property Font: TFont read FFont write SetFont;
    property Brush: TBrush read FBrush write SetBrush;
    property GroupId: Integer read FGroupId write SetGroupId default -1;
    // Published now for the usage of AppStorage.Read/WritePersistent
    property Caption;
    property Checked;
    property Selected;
    property SubItems;
  end;

  TJvListExtendedColumn = class(TCollectionItem)
  private
    FSortMethod: TJvSortMethod;
    FUseParentSortMethod: Boolean;
    FHeaderImagePosition: TJvHeaderImagePosition;
    FUseParentHeaderImagePosition: Boolean;
    function GetSortMethod: TJvSortMethod;
    procedure SetSortMethod(const Value: TJvSortMethod);
    function GetHeaderImagePosition: TJvHeaderImagePosition;
    procedure SetHeaderImagePosition(const Value: TJvHeaderImagePosition);
    procedure SetUseParentHeaderImagePosition(const Value: Boolean);
  public
    constructor Create(Collection: Classes.TCollection); override;

    procedure Assign(AValue: TPersistent); override;
  published
    property SortMethod: TJvSortMethod read GetSortMethod write SetSortMethod default smAutomatic;
    property UseParentSortMethod : Boolean read FUseParentSortMethod write FUseParentSortMethod default True;
    property HeaderImagePosition: TJvHeaderImagePosition read GetHeaderImagePosition write SetHeaderImagePosition default hipLeft;
    property UseParentHeaderImagePosition : Boolean read FUseParentHeaderImagePosition write SetUseParentHeaderImagePosition default True;
  end;

  TJvListExtendedColumns = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TJvListExtendedColumn;
    procedure SetItem(Index: Integer; const Value: TJvListExtendedColumn);

    function Owner : TPersistent;
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer] : TJvListExtendedColumn read GetItem write SetItem; default;
  end;

  {$IFNDEF RTL200_UP}
  TJvListViewGroup = class(TCollectionItem)
  private
    FHeader: WideString;
    FGroupId: Integer;
    FHeaderAlignment: TAlignment;
    procedure SetHeader(const Value: WideString);
    procedure SetHeaderAlignment(const Value: TAlignment);
    procedure SetGroupId(const Value: Integer);

    procedure UpdateGroupProperties(const NewGroupId: Integer = -1);
  public
    constructor Create(Collection: Classes.TCollection); override;
    destructor Destroy; override;
    procedure Assign(AValue: TPersistent); override;
    procedure SetLVGROUP(var GroupInfo: TLVGROUP);
  published
    property GroupId: Integer read FGroupId write SetGroupId default -1;
    property Header: WideString read FHeader write SetHeader;
    property HeaderAlignment: TAlignment read FHeaderAlignment write SetHeaderAlignment default taLeftJustify;
  end;

  TJvListViewGroups = class(TOwnedCollection)
  private
    FSorted: Boolean;
    function GetItem(Index: Integer): TJvListViewGroup;
    procedure SetItem(Index: Integer; const Value: TJvListViewGroup);

    function ParentList: TJvListView;
    procedure InsertGroupIntoList(group: TJvListViewGroup);
    procedure RemoveGroupFromList(group: TJvListViewGroup);

    function Compare(Id1, Id2: Integer): Integer;
    function GetItemById(GroupId: Integer): TJvListViewGroup;
    procedure SetSorted(const Value: Boolean);
  protected
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  public
    constructor Create(AOwner: TPersistent);
    procedure Sort;

    property Items[Index: Integer] : TJvListViewGroup read GetItem write SetItem; default;
    property ItemsById[GroupId: Integer]: TJvListViewGroup read GetItemById;
  published
    property Sorted: Boolean read FSorted write SetSorted default False;
  end;

  TJvGroupsPropertiesBorderRect = class(TJvRect)
  public
    constructor Create;
  published
    property Top default 12;
  end;

  TJvGroupsPropertiesBorderColors = class(TPersistent)
  private
    FRight: TColor;
    FBottom: TColor;
    FTop: TColor;
    FLeft: TColor;
    FOnChange: TNotifyEvent;
    procedure SetBottom(const Value: TColor);
    procedure SetLeft(const Value: TColor);
    procedure SetRight(const Value: TColor);
    procedure SetTop(const Value: TColor);
  protected
    procedure DoChange;
  public
    constructor Create;

    procedure Assign(Source: TPersistent); override;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Top: TColor read FTop write SetTop default $C8D0D4;
    property Left: TColor read FLeft write SetLeft default clWhite;
    property Bottom: TColor read FBottom write SetBottom default clWhite;
    property Right: TColor read FRight write SetRight default clWhite;
  end;

  TJvGroupsProperties = class(TPersistent)
  private
    FBorderSize: TJvGroupsPropertiesBorderRect;
    FBorderColor: TJvGroupsPropertiesBorderColors;
    FHeaderColor: TColor;

    FOnChange: TNotifyEvent;
    FLoading: Boolean;
    procedure SetBorderSize(const Value: TJvGroupsPropertiesBorderRect);
    procedure SetBorderColor(const Value: TJvGroupsPropertiesBorderColors);

    procedure BorderSizeChange(Sender: TObject);
    procedure BorderColorChange(Sender: TObject);
    procedure SetHeaderColor(const Value: TColor);
  protected
    procedure DoChange;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromList(List: TCustomListView);

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property BorderSize: TJvGroupsPropertiesBorderRect read FBorderSize write SetBorderSize;
    property HeaderColor: TColor read FHeaderColor write SetHeaderColor default clBlack;

    // Note that BorderColor is currently ignored by the Win32 API
    property BorderColor: TJvGroupsPropertiesBorderColors read FBorderColor write SetBorderColor;
  end;
  {$ENDIF !RTL200_UP}

  TJvViewStyles = set of TJvViewStyle;

  TJvTileSizeKind = (tskAutoSize, tskFixedWidth, tskFixedHeight, tskFixedSize);

  TJvTileViewProperties = class(TPersistent)
  private
    FLabelMargin: TJvRect;
    FTileSize: TJvSize;
    FSubLinesCount: Integer;
    FTileSizeKind: TJvTileSizeKind;
    FOnChange: TNotifyEvent;
    FLoading: Boolean;
    procedure SetLabelMargin(const Value: TJvRect);
    procedure SetSubLinesCount(const Value: Integer);
    procedure SetTileSize(const Value: TJvSize);
    procedure SetTileSizeKind(const Value: TJvTileSizeKind);

    procedure LabelMarginChange(Sender: TObject);
    procedure TileSizeChange(Sender: TObject);
  protected
    procedure DoChange;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromList(List: TCustomListView);

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property TileSizeKind: TJvTileSizeKind read FTileSizeKind write SetTileSizeKind default tskAutoSize;
    property TileSize: TJvSize read FTileSize write SetTileSize;
    property SubLinesCount: Integer read FSubLinesCount write SetSubLinesCount default 1;
    property LabelMargin: TJvRect read FLabelMargin write SetLabelMargin;
  end;

  TJvInsertMarkPosition = (impBefore, impAfter);

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvListView = class(TJvExListView)
  private
    FAutoClipboardCopy: Boolean;
    FSortOnClick: Boolean;
    FLast: Integer;
    FOnSaveProgress: TJvOnProgress;
    FOnLoadProgress: TJvOnProgress;
    FOnAutoSort: TJvListViewColumnSortEvent;
    FSortMethod: TJvSortMethod;
    FOnHorizontalScroll: TNotifyEvent;
    FOnVerticalScroll: TNotifyEvent;
    FImageChangeLink: TChangeLink;
    FHeaderImagePosition: TJvHeaderImagePosition;
    FHeaderImages: TCustomImageList;
    FAutoSelect: Boolean;
    FPicture: TPicture;
    FExtendedColumns: TJvListExtendedColumns;
    FSavedExtendedColumns: TJvListExtendedColumns;
    FSavedColumnOrder: string;
    FViewStylesItemBrush: TJvViewStyles;  // use for Create/DestroyWnd process
    {$IFNDEF RTL200_UP}
    FGroupView: Boolean;
    FGroups: TJvListViewGroups;
    FGroupsProperties: TJvGroupsProperties;
    FOnCompareGroups: TJvListViewCompareGroupEvent;
    {$ENDIF !RTL200_UP}
    FViewStyle: TJvViewStyle;
    FTileViewProperties: TJvTileViewProperties;
    FInsertMarkColor: TColor;
    FSettingJvViewStyle: Boolean;
    FSettingHeaderImagePosition: Boolean;
    FReturnKeyTriggersItemDblClick: Boolean;
    FOnItemClick: TListViewItemClickNotifyEvent;
    FOnItemDblClick: TListViewItemClickNotifyEvent;
    FOnCancelEdit: TJvListViewCancelEditEvent;
    FOnBeginColumnResize: TJvListViewBeginColumnResizeEvent;
    FOnEndColumnResize: TJvListViewColumnResizeEvent;
    FOnColumnResizing: TJvListViewColumnResizeEvent;
    FLastSortedColumnIndex: Integer;

    procedure DoPictureChange(Sender: TObject);
    procedure SetPicture(const Value: TPicture);
    {$IFNDEF RTL200_UP}
    procedure SetGroupView(const Value: Boolean);
    procedure SetGroups(const Value: TJvListViewGroups);
    procedure SetGroupsProperties(const Value: TJvGroupsProperties);
    {$ENDIF !RTL200_UP}
    procedure SetTileViewProperties(const Value: TJvTileViewProperties);
    procedure SetInsertMarkColor(const Value: TColor);
    procedure SetHeaderImagePosition(const Value: TJvHeaderImagePosition);
    procedure SetHeaderImages(const Value: TCustomImageList);
    procedure UpdateHeaderImages(HeaderHandle: Integer);
    procedure WMAutoSelect(var Msg: TMessage); message WM_AUTOSELECT;
    procedure SetExtendedColumns(const Value: TJvListExtendedColumns);
    procedure SetViewStylesItemBrush(const Value: TJvViewStyles);
    {$IFNDEF RTL200_UP}
    function DoCompareGroups(Group1, Group2: TJvListViewGroup): Integer;
    procedure GroupsPropertiesChange(Sender: TObject);
    procedure LoadGroupsProperties;
    {$ENDIF !RTL200_UP}
    procedure TileViewPropertiesChange(Sender: TObject);
    procedure LoadTileViewProperties;
    function AreItemsStored: Boolean;
    function GetListItems: TJvListItems;
    procedure SetListItems(const Value: TJvListItems);
    function GetColumnIndex(PHeader: PNMHdr): Integer;
    function GetColumnWidth(PHeader: PNMHdr): Integer;
  protected
    function CreateListItem: TListItem; override;
    function CreateListItems: TListItems; override;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetColumnsOrder: string;
    procedure SetColumnsOrder(const Order: string);
    procedure SetItemPopup(Node: TListItem; Value: TPopupMenu);
    function GetItemPopup(Node: TListItem): TPopupMenu;
    procedure DoHeaderImagesChange(Sender: TObject);
    procedure Loaded; override;
    procedure SetViewStyle(Value: TViewStyle); override;
    procedure SetJvViewStyle(Value: TJvViewStyle); virtual;
    procedure ItemClick(AItem: TListItem; SubItemIndex: Integer; X, Y: Integer); virtual;
    procedure ItemDblClick(AItem: TListItem; SubItemIndex: Integer; X, Y: Integer); virtual;
    procedure SetLastSortedColumnIndex(const Value: Integer); virtual;

    procedure CreateWnd; override;
    procedure DestroyWnd; override;

    procedure WMNCCalcSize(var Msg: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure LVMDeleteColumn(var Msg: TMessage); message LVM_DELETECOLUMN;
    procedure LVMInsertColumn(var Msg: TMessage); message LVM_INSERTCOLUMN;
    procedure LVMSetColumn(var Msg: TMessage); message LVM_SETCOLUMN;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure WMNotify(var Msg: TWMNotify); message WM_NOTIFY;

    procedure InsertItem(Item: TListItem); override;
    function IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean; override;
    function CustomDraw(const ARect: TRect; Stage: TCustomDrawStage): Boolean; override;
    function CustomDrawItem(Item: TListItem; State: TCustomDrawState;
      Stage: TCustomDrawStage): Boolean; override;
    function CustomDrawSubItem(Item: TListItem; SubItem: Integer;
      State: TCustomDrawState; Stage: TCustomDrawStage): Boolean; override;

    procedure EditCanceled(Item: TListItem); virtual;
    function DoBeginColumnResize(ColumnIndex, ColumnWidth: Integer): Boolean; virtual;
    procedure DoColumnResizing(ColumnIndex, ColumnWidth: Integer); virtual;
    procedure DoEndColumnResize(ColumnIndex, ColumnWidth: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ColClick(Column: TListColumn); override;
    procedure SaveToStrings(Strings: TStrings; Separator: Char);
    procedure LoadFromStrings(Strings: TStrings; Separator: Char; ClearItems: Boolean = False);
    procedure SaveToFile(FileName: string; ForceOldStyle: Boolean = False);
    procedure LoadFromFile(FileName: string);
    procedure SaveToStream(Stream: TStream; ForceOldStyle: Boolean = False);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToCSV(FileName: string; Separator: Char = ';');
    procedure LoadFromCSV(FileName: string; Separator: Char = ';');
    procedure SetSmallImages(const Value: TCustomImageList);
    procedure UnselectAll;
    procedure InvertSelection;
    function MoveUp(Index: Integer; Focus: Boolean = True): Integer;
    function MoveDown(Index: Integer; Focus: Boolean = True): Integer;
    function SelectNextItem(Focus: Boolean = True): Integer;
    function SelectPrevItem(Focus: Boolean = True): Integer;

    function ShowInsertMark(ItemIndex: Integer; Position: TJvInsertMarkPosition): Boolean;
    function HideInsertMark: Boolean;
    function GetInsertMarkPosition(const X, Y: Integer; var ItemIndex: Integer; var Position: TJvInsertMarkPosition): Boolean;

    property ItemPopup[Item: TListItem]: TPopupMenu read GetItemPopup write SetItemPopup;
    property Items: TJvListItems read GetListItems write SetListItems stored AreItemsStored;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SetFocus; override;

    property LastSortedColumnIndex: Integer read FLastSortedColumnIndex write SetLastSortedColumnIndex;
  published
    property AutoSelect: Boolean read FAutoSelect write FAutoSelect default True;
    property ColumnsOrder: string read GetColumnsOrder write SetColumnsOrder;
    property HintColor;
    property Picture: TPicture read FPicture write SetPicture;
    property HeaderImagePosition: TJvHeaderImagePosition read FHeaderImagePosition write SetHeaderImagePosition default hipLeft;
    property HeaderImages: TCustomImageList read FHeaderImages write SetHeaderImages;
    property SortMethod: TJvSortMethod read FSortMethod write FSortMethod default smAutomatic;
    property SortOnClick: Boolean read FSortOnClick write FSortOnClick default True;
    property SmallImages write SetSmallImages;
    property AutoClipboardCopy: Boolean read FAutoClipboardCopy write FAutoClipboardCopy default True;
    property ReturnKeyTriggersItemDblClick: Boolean read FReturnKeyTriggersItemDblClick write FReturnKeyTriggersItemDblClick default True;
    {$IFNDEF RTL200_UP}
    property GroupView: Boolean read FGroupView write SetGroupView default False;
    property Groups: TJvListViewGroups read FGroups write SetGroups;
    property GroupsProperties: TJvGroupsProperties read FGroupsProperties write SetGroupsProperties;
    {$ENDIF !RTL200_UP}
    property TileViewProperties: TJvTileViewProperties read FTileViewProperties write SetTileViewProperties;
    property InsertMarkColor: TColor read FInsertMarkColor write SetInsertMarkColor default clBlack;

    property ViewStylesItemBrush : TJvViewStyles read FViewStylesItemBrush write SetViewStylesItemBrush default ALL_VIEW_STYLES;
    property ViewStyle: TJvViewStyle read FViewStyle write SetJvViewStyle default vsIcon;

    property OnAutoSort: TJvListViewColumnSortEvent read FOnAutoSort write FOnAutoSort;
    property OnCancelEdit: TJvListViewCancelEditEvent read FOnCancelEdit write FOnCancelEdit;
    property OnHorizontalScroll: TNotifyEvent read FOnHorizontalScroll write FOnHorizontalScroll;
    property OnLoadProgress: TJvOnProgress read FOnLoadProgress write FOnLoadProgress;
    property OnSaveProgress: TJvOnProgress read FOnSaveProgress write FOnSaveProgress;
    property OnVerticalScroll: TNotifyEvent read FOnVerticalScroll write FOnVerticalScroll;
    {$IFNDEF RTL200_UP}
    property OnCompareGroups: TJvListViewCompareGroupEvent read FOnCompareGroups write FOnCompareGroups;
    {$ENDIF !RTL200_UP}
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property OnItemClick: TListViewItemClickNotifyEvent read FOnItemClick write FOnItemClick;
    property OnItemDblClick: TListViewItemClickNotifyEvent read FOnItemDblClick write FOnItemDblClick;
    property OnBeginColumnResize: TJvListViewBeginColumnResizeEvent read FOnBeginColumnResize write FOnBeginColumnResize;
    property OnEndColumnResize: TJvListViewColumnResizeEvent read FOnEndColumnResize write FOnEndColumnResize;
    property OnColumnResizing: TJvListViewColumnResizeEvent read FOnColumnResizing write FOnColumnResizing;

    // This property contains a collection that allows to specify additional
    // properties for each columns (sort method for instance). It can not be
    // included in the Columns collection as the VCL does not offer a way
    // to specify which class to use for the items of the Columns collection.
    // Note that this one (ExtendedColumns) is populated automatically when
    // a column is added or deleted. But because the VCL code for add starts
    // by deleting all columns to reinsert them after, you should not change
    // the properties for any item of ExtendedColumns in a loop that contains
    // a call to the Add method of the Columns property.
    property ExtendedColumns : TJvListExtendedColumns read FExtendedColumns write SetExtendedColumns;
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
  {$IFDEF HAS_UNIT_TYPES}
  Types,
  {$ENDIF HAS_UNIT_TYPES}
  {$IFDEF RTL250_UP}
  AnsiStrings,
  {$ENDIF RTL250_UP}
  VarUtils, Variants,
  JclSysInfo,
  JvConsts;

type
  // Mantis 980: New types for group/tile/insert mark handling
  tagLVITEMA = record
    mask: UINT;
    iItem: Integer;
    iSubItem: Integer;
    state: UINT;
    stateMask: UINT;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iImage: Integer;
    lParam: LPARAM;
    iIndent: Integer;
    iGroupId: Integer;
    cColumns: UINT;
    puColumns: PUINT;
  end;
  TLVITEMA = tagLVITEMA;

  TFNLVGROUPCOMPARE = function (Group1_ID: Integer; Group2_ID: Integer; pvData: Pointer): Integer; stdcall;
  PFNLVGROUPCOMPARE = ^TFNLVGROUPCOMPARE;

  tagLVINSERTGROUPSORTED = record
    pfnGroupCompare: PFNLVGROUPCOMPARE;
    pvData: Pointer;
    lvGroup: TLVGROUP;
  end;
  TLVINSERTGROUPSORTED = tagLVINSERTGROUPSORTED;
  PLVINSERTGROUPSORTED = ^TLVINSERTGROUPSORTED;

  tagLVTILEVIEWINFO = record
    cbSize: UINT;
    dwMask: DWORD;
    dwFlags: DWORD;
    sizeTile: TSize;
    cLines: Integer;
    rcLabelMargin: TRect;
  end;
  TLVTILEVIEWINFO = tagLVTILEVIEWINFO;
  PLVTILEVIEWINFO = ^TLVTILEVIEWINFO;

  tagLVTILEINFO = record
    cbSize: UINT;
    iItem: Integer;
    cColumns: UINT;
    puColumns: PUINT;
  end;
  TLVTILEINFO = tagLVTILEINFO;
  PLVTILEINFO = ^TLVTILEINFO;

  tagLVINSERTMARK = record
    cbSize: UINT;
    dwFlags: DWORD;
    iItem: Integer;
    dwReserved: DWORD;
  end;
  TLVINSERTMARK = tagLVINSERTMARK;
  PLVINSERTMARK = ^TLVINSERTMARK;

  tagLVGROUPMETRICS = record
    cbSize: UINT;
    mask: UINT;
    Left: UINT;
    Top: UINT;
    Right: UINT;
    Bottom: UINT;
    crLeft: COLORREF;
    crTop: COLORREF;
    crRight: COLORREF;
    crBottom: COLORREF;
    crHeader: COLORREF;
    crFooter: COLORREF;
  end;
  TLVGROUPMETRICS = tagLVGROUPMETRICS;
  PLVGROUPMETRICS = ^TLVGROUPMETRICS;

const
  // Mantis 980: New constants for group/tile/insert mark handling
  LVM_SETTILEWIDTH       = LVM_FIRST + 141;
  LVM_SETVIEW            = LVM_FIRST + 142;
  LVM_INSERTGROUP        = LVM_FIRST + 145;
  LVM_SETGROUPINFO       = LVM_FIRST + 147;
  LVM_REMOVEGROUP        = LVM_FIRST + 150;
  LVM_MOVEITEMTOGROUP    = LVM_FIRST + 154;
  LVM_SETGROUPMETRICS    = LVM_FIRST + 155;
  LVM_GETGROUPMETRICS    = LVM_FIRST + 156;
  LVM_ENABLEGROUPVIEW    = LVM_FIRST + 157;
  LVM_SORTGROUPS         = LVM_FIRST + 158;
  LVM_INSERTGROUPSORTED  = LVM_FIRST + 159;
  LVM_REMOVEALLGROUPS    = LVM_FIRST + 160;
  LVM_SETTILEVIEWINFO    = LVM_FIRST + 162;
  LVM_GETTILEVIEWINFO    = LVM_FIRST + 163;
  LVM_SETTILEINFO        = LVM_FIRST + 164;
  LVM_GETTILEINFO        = LVM_FIRST + 165;
  LVM_SETINSERTMARK      = LVM_FIRST + 166;
  LVM_INSERTMARKHITTEST  = LVM_FIRST + 168;
  LVM_GETINSERTMARKRECT  = LVM_FIRST + 169;
  LVM_SETINSERTMARKCOLOR = LVM_FIRST + 170;
  LVM_GETINSERTMARKCOLOR = LVM_FIRST + 171;

  // ListViewItemFlag
  LVIF_GROUPID = $0100;

  // ListViewGroupFlag
  LVGF_HEADER  = $00000001;
  LVGF_ALIGN   = $00000008;
  LVGF_GROUPID = $00000010;

  // group alignment
  LVGA_HEADER_LEFT   = $00000001;
  LVGA_HEADER_CENTER = $00000002;
  LVGA_HEADER_RIGHT  = $00000004;

  // view styles
  LV_VIEW_ICON = $00;
  LV_VIEW_DETAILS = $01;
  LV_VIEW_SMALLICON = $02;
  LV_VIEW_LIST = $03;
  LV_VIEW_TILE = $04;

  // LVTVIF (ListViewTileViewInfoFlag Constants)
  LVTVIF_AUTOSIZE    = 0;
  LVTVIF_FIXEDWIDTH  = 1;
  LVTVIF_FIXEDHEIGHT = 2;
  LVTVIF_FIXEDSIZE   = 3;

  // LVTVIM (ListViewTileViewInfoMask Constants)
  LVTVIM_TILESIZE    = 1;
  LVTVIM_COLUMNS     = 2;
  LVTVIM_LABELMARGIN = 4;

  // LVIM (ListViewInsertMark Constants)
  LVIM_AFTER = 1;

  // LVGMF (ListViewGroupMetricsFlag Constants)
  LVGMF_NONE        = $00000000;
  LVGMF_BORDERSIZE  = $00000001;
  LVGMF_BORDERCOLOR = $00000002;
  LVGMF_TEXTCOLOR   = $00000004;

  AlignmentToLVGA: array[TAlignment] of Integer = (LVGA_HEADER_LEFT, LVGA_HEADER_RIGHT, LVGA_HEADER_CENTER);
  TileSizeKindToLVTVIF: array[TJvTileSizeKind] of Integer = (LVTVIF_AUTOSIZE, LVTVIF_FIXEDWIDTH, LVTVIF_FIXEDHEIGHT, LVTVIF_FIXEDSIZE);
  InsertMarkPositionToLVIM: array[TJvInsertMarkPosition] of Integer = (0, LVIM_AFTER);

//=== { TJvListItem } ========================================================

constructor TJvListItem.CreateEnh(AOwner: TListItems; const Popup: TPopupMenu);
begin
  inherited Create(AOwner);

  FBold := False;
  FPopupMenu := Popup; // (Salvatore) Get it from the JvListView
  FFont := TFont.Create;
  FBrush := TBrush.Create;
  FGroupId := -1;
  FTileColumns := TIntegerList.Create;

  FTileColumns.OnChange := TileColumnsChange;
  if AOwner.Owner is TJvListView then
    FFont.Assign((AOwner.Owner as TJvListView).Canvas.Font);
end;

procedure TJvListItem.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);

  // Because a TList is not saved natively by Delphi, we do it ourselves.
  Filer.DefineProperty('TileColumns', ReadTileColumns, WriteTileColumns, True);
end;

destructor TJvListItem.Destroy;
begin
  FTileColumns.Free;
  FFont.Free;
  FBrush.Free;

  inherited Destroy;
end;

procedure TJvListItem.ReadTileColumns(Reader: TReader);
begin
  FTileColumns.ReadData(Reader);
  UpdateTileColumns;
end;

procedure TJvListItem.SetBrush(const Value: TBrush);
begin
  FBrush.Assign(Value);
end;

procedure TJvListItem.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TJvListItem.SetGroupId(const Value: Integer);
var
  Infos: JvListView.TLVITEMA;
  List: TCustomListView;
begin
  if FGroupId <> Value then
  begin
    FGroupId := Value;

    List := Owner.Owner;
    if Assigned(List) then
    begin
      ZeroMemory(@Infos, sizeof(Infos));
      Infos.mask := LVIF_GROUPID;
      Infos.iItem := Index;
      Infos.iGroupId := FGroupId;

      SendMessage(List.Handle, LVM_SETITEM, 0, LPARAM(@Infos));
    end;
  end;
end;

procedure TJvListItem.SetPopupMenu(const Value: TPopupMenu);
begin
  FPopupMenu := Value;
end;

procedure TJvListItem.SetTileColumns(const Value: TIntegerList);
begin
  FTileColumns.Assign(Value);
end;

procedure TJvListItem.TileColumnsChange(Sender: TObject; Item: Integer;
  Action: TListNotification);
begin
  if not TileColumns.Loading then
    UpdateTileColumns;
end;

procedure TJvListItem.UpdateTileColumns;
type
  TCardinalArray = array [0..0] of Cardinal;
var
  List: TCustomListView;
  TileInfos: TLVTILEINFO;
  Cols: ^TCardinalArray;
  I: Integer;
begin
  List := Owner.Owner;
  if Assigned(List) then
  begin
    GetMem(Cols, FTileColumns.Count);
    try
      for I := 0 to FTileColumns.Count - 1 do
      begin
        Cols[I] := FTileColumns[I];
      end;

      ZeroMemory(@TileInfos, SizeOf(TileInfos));
      TileInfos.cbSize := SizeOf(TileInfos);
      TileInfos.iItem := Index;
      TileInfos.cColumns := FTileColumns.Count;
      TileInfos.puColumns := PUINT(Cols);
      SendMessage(List.Handle, LVM_SETTILEINFO, 0, LPARAM(@TileInfos));
    finally
      FreeMem(Cols);
    end;
  end;
end;

procedure TJvListItem.WriteTileColumns(Writer: TWriter);
begin
  FTileColumns.WriteData(Writer);
end;

//=== { TJvListItems } =======================================================

procedure TJvListItems.AfterConstruction;
begin
  inherited AfterConstruction;
  if GetOwner <> nil then
    GetOwner.GetInterface(IInterface, FOwnerInterface);
end;

function TJvListItems._AddRef: Integer;
begin
  if FOwnerInterface <> nil then
    Result := FOwnerInterface._AddRef
  else
    Result := -1;
end;

function TJvListItems._Release: Integer;
begin
  if FOwnerInterface <> nil then
    Result := FOwnerInterface._Release
  else
    Result := -1;
end;

function TJvListItems.QueryInterface(const IID: TGUID; out Obj): HRESULT;
const
  E_NOINTERFACE = HRESULT($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TJvListItems.ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
begin
  BeginUpdate;
  try
    Clear;
    AppStorage.ReadList(BasePath, Self, ReadListItem, cItem);
  finally
    EndUpdate;
  end;
end;

procedure TJvListItems.WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
begin
  AppStorage.WriteList(BasePath, Self, Count, WriteListItem, DeleteListItem, cItem);
end;

procedure TJvListItems.ReadListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const Index: Integer; const ItemName: string);
var
  NewItem: TPersistent;
  NewPath: string;
begin
  if List is TJvListItems then
    try
      NewPath := Sender.ConcatPaths([Path, ItemName + IntToStr(Index)]);
      NewItem := TJvListItems(List).Add;
      Sender.ReadPersistent(NewPath, NewItem);
    except
    end;
end;

procedure TJvListItems.WriteListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const Index: Integer; const ItemName: string);
begin
  if List is TJvListItems then
    if Assigned(TJvListItems(List)[Index]) then
      Sender.WritePersistent(Sender.ConcatPaths([Path, ItemName + IntToStr(Index)]), TPersistent(TJvListItems(List)[Index]));
end;

procedure TJvListItems.DeleteListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const First, Last: Integer; const ItemName: string);
var
  I: Integer;
begin
  if List is TJvListItems then
    for I := First to Last do
      Sender.DeleteValue(Sender.ConcatPaths([Path, ItemName + IntToStr(I)]));
end;

function TJvListItems.GetItem(Index: Integer): TJvListItem;
begin
  Result := inherited Item[Index] as TJvListItem;
end;

procedure TJvListItems.SetItem(Index: Integer; const Value: TJvListItem);
begin
  inherited Item[Index] := Value;
end;

function TJvListItems.Add: TJvListItem;
begin
  Result := inherited Add as TJvListItem;
end;

function TJvListItems.AddItem(Item: TJvListItem; Index: Integer): TJvListItem;
begin
  Result := inherited AddItem(Item, Index) as TJvListItem;
end;

function TJvListItems.Insert(Index: Integer): TJvListItem;
begin
  Result := inherited Insert(Index) as TJvListItem;
end;

{ TJvListExtendedColumn }

procedure TJvListExtendedColumn.Assign(AValue: TPersistent);
begin
  if AValue is TJvListExtendedColumn then
  begin
    FSortMethod := TJvListExtendedColumn(AValue). SortMethod;
    FUseParentSortMethod := TJvListExtendedColumn(AValue).UseParentSortMethod;

    FHeaderImagePosition := TJvListExtendedColumn(AValue).HeaderImagePosition;
    FUseParentHeaderImagePosition := TJvListExtendedColumn(AValue).UseParentHeaderImagePosition;
  end
  else
    inherited Assign(AValue);
end;

constructor TJvListExtendedColumn.Create(Collection: Classes.TCollection);
begin
  inherited Create(Collection);

  FSortMethod := smAutomatic;
  FUseParentSortMethod := True;

  FHeaderImagePosition := hipLeft;
  FUseParentHeaderImagePosition := True;
end;

function TJvListExtendedColumn.GetHeaderImagePosition: TJvHeaderImagePosition;
begin
  if (TJvListExtendedColumns(Collection).Owner is TJvListView) and UseParentHeaderImagePosition then
    Result := TJvListView(TJvListExtendedColumns(Collection).Owner).HeaderImagePosition
  else
    Result := FHeaderImagePosition;
end;

function TJvListExtendedColumn.GetSortMethod: TJvSortMethod;
begin
  if (TJvListExtendedColumns(Collection).Owner is TJvListView) and UseParentSortMethod then
    Result := TJvListView(TJvListExtendedColumns(Collection).Owner).SortMethod
  else
    Result := FSortMethod;
end;

procedure TJvListExtendedColumn.SetHeaderImagePosition(
  const Value: TJvHeaderImagePosition);
begin
  FHeaderImagePosition := Value;
  UseParentHeaderImagePosition := False;

  if (TJvListExtendedColumns(Collection).Owner is TJvListView) then
  begin
    TJvListView(TJvListExtendedColumns(Collection).Owner).DoHeaderImagesChange(Self);
  end;
end;

procedure TJvListExtendedColumn.SetSortMethod(
  const Value: TJvSortMethod);
begin
  FSortMethod := Value;
  UseParentSortMethod := False;
end;

procedure TJvListExtendedColumn.SetUseParentHeaderImagePosition(
  const Value: Boolean);
begin
  if FUseParentHeaderImagePosition <> Value then
  begin
    FUseParentHeaderImagePosition := Value;
    if (TJvListExtendedColumns(Collection).Owner is TJvListView) then
    begin
      TJvListView(TJvListExtendedColumns(Collection).Owner).DoHeaderImagesChange(Self);
    end;
  end;
end;

{ TJvListExtendedColumns }

constructor TJvListExtendedColumns.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJvListExtendedColumn);
end;

function TJvListExtendedColumns.GetItem(
  Index: Integer): TJvListExtendedColumn;
begin
  Result := TJvListExtendedColumn(inherited Items[Index]);
end;

function TJvListExtendedColumns.Owner: TPersistent;
begin
  Result := GetOwner;
end;

procedure TJvListExtendedColumns.SetItem(Index: Integer;
  const Value: TJvListExtendedColumn);
begin
  inherited Items[Index] := Value;
end;

//=== { TJvListView } ========================================================

const
  cLISTVIEW01: PAnsiChar = 'LISTVIEW01'; // 10 chars

constructor TJvListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSortOnClick := True;
  FSortMethod := smAutomatic;
  FLast := -1;
  FInsertMarkColor := clBlack;
  FAutoClipboardCopy := True;
  FReturnKeyTriggersItemDblClick := True;
  FHeaderImagePosition := hipLeft;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := DoHeaderImagesChange;
  FAutoSelect := True;
  FPicture := TPicture.Create;
  FPicture.OnChange := DoPictureChange;

  FViewStylesItemBrush := ALL_VIEW_STYLES;
  FExtendedColumns := TJvListExtendedColumns.Create(Self);
  FSavedExtendedColumns := TJvListExtendedColumns.Create(Self);
  {$IFNDEF RTL200_UP}
  FGroups := TJvListViewGroups.Create(Self);
  FGroupsProperties := TJvGroupsProperties.Create;
  {$ENDIF !RTL200_UP}
  FTileViewProperties := TJvTileViewProperties.Create;

  FTileViewProperties.OnChange := TileViewPropertiesChange;
  {$IFNDEF RTL200_UP}
  FGroupsProperties.OnChange := GroupsPropertiesChange;
  {$ENDIF !RTL200_UP}
end;

destructor TJvListView.Destroy;
begin
  {$IFNDEF RTL200_UP}
  FGroupsProperties.Free;
  FGroups.Free;
  {$ENDIF !RTL200_UP}
  FTileViewProperties.Free;
  FExtendedColumns.Free;
  FSavedExtendedColumns.Free;

  FImageChangeLink.Free;
  FPicture.Free;
  inherited Destroy;
end;

procedure TJvListView.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  UpdateHeaderImages(ListView_GetHeader(Handle));
  if Assigned(FOnHorizontalScroll) then
    FOnHorizontalScroll(Self);
end;

procedure TJvListView.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  UpdateHeaderImages(ListView_GetHeader(Handle));
  if Assigned(FOnVerticalScroll) then
    FOnVerticalScroll(Self);
end;

procedure TJvListView.ColClick(Column: TListColumn);
type
  TParamSort = record
    ColumnIndex: Integer;
    Sender: TObject;
  end;
var
  Parm: TParamSort;

  function CustomCompare1(Item1, Item2, ParamSort: LPARAM): Integer stdcall;
  var
    Parm: TParamSort;
    i1, i2: TListItem;
    S1, S2: string;
    I: Integer;
    SortKind: TJvSortMethod;

    function IsBigger(First, Second: string; SortType: TJvSortMethod): Boolean;
    var
      I, J: Double;
      d, e: TDateTime;
      a, b: Currency;
      l, m: Int64;
      st, st2: string;
      int1, int2: Integer;

      function FirstNonAlpha(Value: string): Integer;
      var
        Len: Integer;
        I, J: Integer;
        Comma: Boolean;
      begin
        Len := Length(Value);
        I := 1;
        J := 0;
        Comma := False;

        while I <= Len do
        begin
          case Value[I] of
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
              J := I;
            ',', '.':
              if not Comma then
                Comma := True
              else
              begin
                J := I - 1;
                I := Len;
              end;
          else
            begin
              J := I - 1;
              I := Len;
            end;
          end;
          Inc(I);
        end;

        Result := J;
      end;

    begin
      Result := False;
      if Trim(First) = '' then
        Result := False
      else
        if Trim(Second) = '' then
          Result := True
        else
        begin
          case SortType of
            smAlphabetic:
              Result := First > Second;
            smNonCaseSensitive:
              Result := UpperCase(First) > UpperCase(Second);
            smNumeric:
              begin
                try
                  VarR8FromStr({$IFDEF RTL240_UP}PChar{$ENDIF RTL240_UP}(First), LOCALE_USER_DEFAULT, 0, I);
                  VarR8FromStr({$IFDEF RTL240_UP}PChar{$ENDIF RTL240_UP}(Second), LOCALE_USER_DEFAULT, 0, J);
                  Result := I > J;
                except
                  try
                    l := StrToInt64(First);
                  except
                    l := 0;
                  end;
                  try
                    m := StrToInt64(Second);
                  except
                    m := 0;
                  end;
                  Result := l > m;
                end;
              end;
            smDate:
              begin
                d := StrToDate(First);
                e := StrToDate(Second);
                Result := d > e;
              end;
            smTime:
              begin
                d := StrToTime(First);
                e := StrToTime(Second);
                Result := d > e;
              end;
            smDateTime:
              begin
                d := StrToDateTime(First);
                e := StrToDateTime(Second);
                Result := d > e;
              end;
            smCurrency:
              begin
                VarCyFromStr({$IFDEF RTL240_UP}PChar{$ENDIF RTL240_UP}(First), LOCALE_USER_DEFAULT, 0, a);
                VarCyFromStr({$IFDEF RTL240_UP}PChar{$ENDIF RTL240_UP}(Second), LOCALE_USER_DEFAULT, 0, b);
                Result := a > b;
              end;
            smAutomatic:
              begin
                int1 := FirstNonAlpha(First);
                int2 := FirstNonAlpha(Second);
                if (int1 <> 0) and (int2 <> 0) then
                begin
                  st := Copy(First, 1, int1);
                  st2 := Copy(Second, 1, int2);
                  try
                    Result := StrToFloat(st) > StrToFloat(st2);
                  except
                    Result := First > Second;
                  end;
                end
                else
                  Result := First > Second;
              end;
          end;
        end;
    end;

  begin
    Parm := TParamSort(Pointer(ParamSort)^);
    i1 := TListItem(Item1);
    i2 := TListItem(Item2);
    I := Parm.ColumnIndex;

    // (Salvatore)
    if Parm.ColumnIndex < TJvListView(Parm.Sender).ExtendedColumns.Count  then
      SortKind := TJvListView(Parm.Sender).ExtendedColumns[Parm.ColumnIndex].SortMethod
    else
      SortKind := TJvListView(Parm.Sender).SortMethod;

    if Assigned(TJvListView(Parm.Sender).OnAutoSort) then
      TJvListView(Parm.Sender).OnAutoSort(Parm.Sender, Parm.ColumnIndex, SortKind);

    case I of
      {sort by caption}
      0:
        begin
          S1 := i1.Caption;
          S2 := i2.Caption;

          if IsBigger(S1, S2, SortKind) then
            Result := 1
          else
            if IsBigger(S2, S1, SortKind) then
              Result := -1
            else
              Result := 0;
        end;
    else
      {sort by Column}
      begin
        if I > i1.SubItems.Count then
        begin
          if I > i2.SubItems.Count then
            Result := 0
          else
            Result := -1;
        end
        else
          if I > i2.SubItems.Count then
            Result := 1
          else
          begin
            S1 := i1.SubItems[I - 1];
            S2 := i2.SubItems[I - 1];
            if IsBigger(S1, S2, SortKind) then
              Result := 1
            else
              if IsBigger(S2, S1, SortKind) then
                Result := -1
              else
                Result := 0;
          end;
      end;
    end;
  end;

  function CustomCompare2(Item1, Item2, ParamSort: LPARAM): Integer; stdcall;
  begin
    Result := -CustomCompare1(Item1, Item2, ParamSort);
  end;

begin
  inherited ColClick(Column);
  if FSortOnClick then
  begin
    FLastSortedColumnIndex := Column.Index;
    Parm.ColumnIndex := Column.Index;
    Parm.Sender := Self;
    if FLast = Column.Index then
    begin
      FLast := -1;
      CustomSort(TLVCompare(@CustomCompare2), LPARAM(@Parm));
    end
    else
    begin
      FLast := Column.Index;
      CustomSort(TLVCompare(@CustomCompare1), LPARAM(@Parm));
    end;
  end;
end;

function TJvListView.CreateListItem: TListItem;
begin
  Result := TJvListItem.CreateEnh(Items, Self.PopupMenu);
end;

function TJvListView.CreateListItems: TListItems;
begin
  Result := TJvListItems.Create(Self);
end;

function TJvListView.GetItemPopup(Node: TListItem): TPopupMenu;
begin
  Result := TJvListItem(Node).PopupMenu;
end;

procedure TJvListView.SetItemPopup(Node: TListItem; Value: TPopupMenu);
begin
  TJvListItem(Node).PopupMenu := Value;
end;

procedure TJvListView.LoadFromFile(FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJvListView.LoadFromStream(Stream: TStream);
var
  Start: Integer;

  procedure LoadOldStyle(Stream: TStream);
  var
    I, J, K: Integer;
    Buf: array [0..100] of Byte;
    S: AnsiString;
    ch1, Checks: Boolean;
    ListItem: TListItem;
  begin
    I := Stream.Position;
    ListItem := nil;
    S := '';
    if Assigned(FOnLoadProgress) then
      FOnLoadProgress(Self, 0, Stream.Size - Start);
    Checks := False;
    ch1 := CheckBoxes;
    while I < Stream.Size do
    begin
      J := Stream.Read(Buf, SizeOf(Buf));
      if Assigned(FOnLoadProgress) then
        FOnLoadProgress(Self, J, Stream.Size - Start);
      I := I + J;
      K := 0;
      while K < J do
      begin
        while (K < J) and (Buf[K] <> 0) and (Buf[K] <> 1) do
        begin
          S := S + AnsiChar(Buf[K]);
          Inc(K);
        end;

        if K < J then
        begin
          if ListItem <> nil then
            ListItem.SubItems.Add(string(S))
          else
          begin
            ListItem := Items.Add;
            Checks := Checks or (S[1] = 'T');
            ListItem.Checked := S[1] = 'T';
            S := Copy(S, 2, Length(S));
            ListItem.Caption := string(S);
          end;
          if Buf[K] = 1 then
            ListItem := nil;
          S := '';
        end;
        Inc(K);
      end;
    end;
    if not ch1 and not Checks then
      CheckBoxes := False;
  end;

  procedure LoadNewStyle(Stream: TStream);
  const
    LV_HASCHECKBOXES = $80;
  var
    Count, J: SmallInt;
    I: Integer;
    Options: Byte;
    UTF8St: UTF8String;
    S: string;
    ListItem: TListItem;
    Buf: array of AnsiChar;
  begin
    Stream.Read(Options, SizeOf(Options));
    CheckBoxes := (Options and LV_HASCHECKBOXES) = LV_HASCHECKBOXES;

    //Read all lines
    while Stream.Position < Stream.Size do
    begin
      Stream.Read(Count, SizeOf(Count));

      //statistics
      if Assigned(FOnLoadProgress) then
        FOnLoadProgress(Self, Stream.Position, Stream.Size - Start);

      //Read all columns
      ListItem := Self.Items.Add;
      for I := 1 to Count do
      begin
        if I = 1 then
        begin
          Stream.Read(Options, SizeOf(Options));
          if CheckBoxes then
            ListItem.Checked := Boolean(Options and Ord(True));
        end;

        Stream.Read(J, SizeOf(J));

        //Read the string
        if Length(Buf) < J then
          SetLength(Buf, J);
        if J > 0 then
        begin
          Stream.Read(Buf[0], J);
          SetString(UTF8St, PAnsiChar(@Buf[0]), J);
          S := UTF8ToString(UTF8St);
        end
        else
          S := '';

        if I = 1 then
          ListItem.Caption := S
        else
          ListItem.SubItems.Add(S);
      end;
    end;
  end;

var
  Buf: array [0..10] of AnsiChar;
begin
  Start := Stream.Position;
  Stream.Read(Buf, 10);
  Buf[10] := #0;

  Items.BeginUpdate;
  try
    Items.Clear;
    if {$IFDEF RTL250_UP}AnsiStrings.{$ENDIF}StrComp(Buf, cLISTVIEW01) <> 0 then
    begin
      Stream.Position := Start;
      LoadOldStyle(Stream);
    end
    else
      LoadNewStyle(Stream);
  finally
    Items.EndUpdate;
  end;
end;

procedure TJvListView.SaveToFile(FileName: string; ForceOldStyle: Boolean);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
    SaveToStream(Stream, ForceOldStyle);
  finally
    Stream.Free;
  end;
end;

procedure TJvListView.SaveToStream(Stream: TStream; ForceOldStyle: Boolean);

  procedure SaveOldStyle(Stream: TStream);
  var
    I, SubItemIndex, K: Integer;
    b, c, d, e: Byte;
    S: AnsiString;
    Buf: array [0..1000] of Byte;
  begin
    b := 0;
    c := 1;
    d := Ord('T'); //checked
    e := Ord('F'); //not checked
    if Assigned(FOnSaveProgress) then
      FOnSaveProgress(Self, 0, Self.Items.Count);
    for I := 0 to Self.Items.Count - 1 do
    begin
      if Assigned(FOnSaveProgress) then
        FOnSaveProgress(Self, I + 1, Self.Items.Count);
      S := AnsiString(Self.Items[I].Caption);
      for K := 1 to Length(S) do
        Buf[K - 1] := Byte(S[K]);
      K := Length(S);
      //write checked,not
      if Self.Items[I].Checked then
        Stream.Write(d, 1)
      else
        Stream.Write(e, 1);
      Stream.Write(Buf, K);
      if Self.Items[I].SubItems.Count = 0 then
        Stream.Write(c, 1)
      else
      begin
        Stream.Write(b, 1);
        for SubItemIndex := 0 to Self.Items[I].SubItems.Count - 2 do
        begin
          S := AnsiString(Self.Items[I].SubItems[SubItemIndex]);
          for K := 1 to Length(S) do
            Buf[K - 1] := Byte(S[K]);
          K := Length(S);
          Stream.Write(Buf, K);
          Stream.Write(b, 1);
        end;
        SubItemIndex := Self.Items[I].SubItems.Count - 1;
        S := AnsiString(Self.Items[I].SubItems[SubItemIndex]);
        for K := 1 to Length(S) do
          Buf[K - 1] := Byte(S[K]);
        K := Length(S);
        Stream.Write(Buf, K);
        Stream.Write(c, 1);
      end;
    end;
  end;

  procedure SaveNewStyle(Stream: TStream);
  const
    LV_HASCHECKBOXES = $80;

    procedure WriteString(const Txt: string);
    var
      I: Word;
      UTF8Txt: UTF8String;
    begin
      UTF8Txt := UTF8Encode(Txt);
      I := Length(UTF8Txt);
      Stream.Write(I, SizeOf(I));
      if I > 0 then
        Stream.Write(UTF8Txt[1], I);
    end;

  var
    I: Integer;
    J: SmallInt;
    Options, IsChecked: Byte;
  begin
    Stream.Write(cLISTVIEW01[0], 10);
    Options := 0;
    if CheckBoxes then
      Options := LV_HASCHECKBOXES;
    Stream.Write(Options, SizeOf(Options));
    for I := 0 to Items.Count - 1 do
      with Items[I] do
      begin
        J := SubItems.Count + 1;
        Stream.Write(J, SizeOf(J));
        IsChecked := Options or (Byte(Ord(Checked)));
        Stream.Write(IsChecked, SizeOf(IsChecked));
        WriteString(Items[I].Caption);
        for J := 0 to Items[I].SubItems.Count - 1 do
          WriteString(SubItems[J]);
      end;
  end;

begin
  if ForceOldStyle then
    SaveOldStyle(Stream)
  else
    SaveNewStyle(Stream);
end;

procedure TJvListView.SaveToStrings(Strings: TStrings; Separator: Char);
var
  I, J: Integer;
  TmpStr: string;
begin
  if Assigned(FOnSaveProgress) then
    FOnSaveProgress(Self, 0, Items.Count);
  for I := 0 to Items.Count - 1 do
  begin
    if Assigned(FOnSaveProgress) then
      FOnSaveProgress(Self, I + 1, Items.Count);
    TmpStr := AnsiQuotedStr(Items[I].Caption, '"');
    for J := 0 to Items[I].SubItems.Count - 1 do
      TmpStr := TmpStr + Separator + AnsiQuotedStr(Items[I].SubItems[J], '"');
    Strings.Add(TmpStr);
  end;
end;

procedure TJvListView.LoadFromStrings(Strings: TStrings; Separator: Char; ClearItems: Boolean);
var
  I: Integer;
  Start, Stop, TmpStart: PChar;
  TmpStr: string;
  ListItem: TListItem;
begin
  Items.BeginUpdate;
  try
    if ClearItems then
      Items.Clear;

    for I := 0 to Strings.Count - 1 do
    begin
      ListItem := nil;
      Start := PChar(Strings[I]);
      Stop := Start + Length(Strings[I]);
      if (Start <> Stop) and (Start <> nil) and (Start^ <> #0) then
      begin
        if Start^ = '"' then
        begin
          ListItem := Items.Add;
          TmpStr := AnsiExtractQuotedStr(Start, '"'); // this moves the PChar pointer
          ListItem.Caption := TmpStr;
        end
        else
        begin
          TmpStart := Start;
          while Start^ <> Separator do
          begin
            if Start = Stop then
              Break;
            Inc(Start);
          end;
          SetString(TmpStr, TmpStart, Start - TmpStart);
          ListItem := Items.Add;
          ListItem.Caption := TmpStr;
        end;
      end;
      if ListItem <> nil then
      begin
        while (Start <> Stop) and (Start <> nil) and (Start^ <> #0) do
        begin
          while Start^ = Separator do
            Inc(Start);
          if Start^ = '"' then
          begin
            TmpStr := AnsiExtractQuotedStr(Start, '"'); // this moves the PChar pointer
            ListItem.SubItems.Add(TmpStr);
          end
          else
          begin
            TmpStart := Start;
            while Start^ <> Separator do
            begin
              if Start = Stop then
                Break;
              Inc(Start);
            end;
            SetString(TmpStr, TmpStart, Start - TmpStart);
            ListItem.SubItems.Add(TmpStr);
          end;
        end;
      end;
    end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TJvListView.LoadFromCSV(FileName: string; Separator: Char);
var
  S: TStringList;
begin
  S := TStringList.Create;
  try
    S.LoadFromFile(FileName);
    LoadFromStrings(S, Separator, True);
  finally
    S.Free;
  end;
end;

procedure TJvListView.SaveToCSV(FileName: string; Separator: Char);
var
  S: TStringList;
begin
  S := TStringList.Create;
  try
    SaveToStrings(S, Separator);
    S.SaveToFile(FileName);
  finally
    S.Free;
  end;
end;

procedure TJvListView.InvertSelection;
var
  I: Integer;
begin
  Items.BeginUpdate;
  try
    for I := 0 to Items.Count - 1 do
      Items[I].Selected := not Items[I].Selected;
  finally
    Items.EndUpdate;
  end;
end;

procedure TJvListView.UnselectAll;
var
  I: Integer;
begin
  Items.BeginUpdate;
  try
    for I := 0 to Items.Count - 1 do
      Items[I].Selected := False;
  finally
    Items.EndUpdate;
  end;
end;

procedure TJvListView.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if ReturnKeyTriggersItemDblClick and (Key = #13) and (Selected <> nil) and (SelCount = 1) then
    ItemDblClick(Selected, -1, -1, -1);
end;

procedure TJvListView.KeyUp(var Key: Word; Shift: TShiftState);
var
  S: string;
  I, J: Integer;
begin
  inherited KeyUp(Key, Shift);
  if AutoClipboardCopy then
    if (Key in [Ord('c'), Ord('C')]) and (ssCtrl in Shift) then
    begin
      for I := 0 to Columns.Count - 1 do
        S := S + Columns[I].Caption + Tab;
      if S <> '' then
        S := S + sLineBreak;
      for I := 0 to Items.Count - 1 do
        if (SelCount = 0) or Items[I].Selected then
        begin
          S := S + Items[I].Caption;
          for J := 0 to Items[I].SubItems.Count - 1 do
            S := S + Tab + Items[I].SubItems[J];
          S := S + sLineBreak;
        end;
      Clipboard.SetTextBuf(PChar(S));
    end;
end;

function TJvListView.GetColumnIndex(PHeader: PNMHdr): Integer;
var
  HwndHeader: HWND;
  ItemInfo: THdItem;
  ItemIndex: Integer;
  Buffer: array [0..128] of Char;
begin
  Result := -1;
  HwndHeader := pHeader^.hwndFrom;
  ItemIndex := pHDNotify(pHeader)^.Item;
  FillChar(ItemInfo, SizeOf(ItemInfo), 0);
  ItemInfo.Mask := HDI_TEXT;
  ItemInfo.pszText := Buffer;
  ItemInfo.cchTextMax := SizeOf(Buffer) - 1;
  Header_GetItem(HwndHeader, ItemIndex, ItemInfo);
  if CompareStr(Columns[ItemIndex].Caption, ItemInfo.pszText) = 0 then
  begin
    Result := ItemIndex;
  end
  else
  begin
    for ItemIndex := 0 to Columns.Count - 1 do
      if CompareStr(Columns[ItemIndex].Caption, ItemInfo.pszText) = 0 then
      begin
        Result := ItemIndex;
        Break;
      end;
  end;
end;

function TJvListView.GetColumnsOrder: string;
var
  Res: array of Integer;
  I: Integer;
begin
  if Columns.Count > 0 then
  begin
    if not Columns.Owner.HandleAllocated then
      Result := FSavedColumnOrder
    else
    begin
      SetLength(Res, Columns.Count);
      ListView_GetColumnOrderArray(Columns.Owner.Handle, Columns.Count, @Res[0]);
      Result := '';
      for I := 0 to Columns.Count - 1 do
      begin
        if Result <> '' then
          Result := Result + ',';
        Result := Result + IntToStr(Res[I]) + '=' + IntToStr(Columns[I].Width);
      end;
    end;
  end
  else
    Result := '';
end;

function TJvListView.GetColumnWidth(PHeader: PNMHdr): Integer;
begin
  Result := -1;
  if Assigned(PHDNotify(PHeader)^.PItem) and
    ((PHDNotify(PHeader)^.PItem^.Mask and HDI_WIDTH) <> 0) then
    Result := PHDNotify(PHeader)^.PItem^.cxy;
end;

procedure TJvListView.SetColumnsOrder(const Order: string);
var
  Res: array of Integer;
  I, J: Integer;
  S: string;
  SL: TStrings;
begin
  if not Columns.Owner.HandleAllocated then
    FSavedColumnOrder := Order
  else
  begin
    if Columns.Count > 0 then
    begin
      SetLength(Res, Columns.Count);
      FillChar(Res[0], Length(Res) * SizeOf(Integer), 0);
      SL := TStringList.Create;
      try
        SL.CommaText := Order;
        for I := 0 to SL.Count - 1 do
        begin
          S := SL[I];
          J := Pos('=', S);
          if (J <> 0) and (I < Columns.Count) then
          begin
            Columns[I].Width := StrToIntDef(Copy(S, J + 1, Length(S)), Columns[I].Width);
            S := Copy(S, 1, J - 1);
          end;
          Res[I] := StrToIntDef(S, 0);
        end;
      finally
        SL.Free;
      end;
      ListView_SetColumnOrderArray(Columns.Owner.Handle, Columns.Count, @Res[0]);
    end;
  end;
end;

procedure TJvListView.SetHeaderImages(const Value: TCustomImageList);
begin
  if ReplaceImageListReference(Self, Value, FHeaderImages, FImageChangeLink) then
    UpdateHeaderImages(ListView_GetHeader(Handle));
end;

procedure TJvListView.SetExtendedColumns(
  const Value: TJvListExtendedColumns);
begin
  FExtendedColumns.Assign(Value);
end;

procedure TJvListView.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = HeaderImages then
      HeaderImages := nil
    else
      if not (csDestroying in ComponentState) and (AComponent is TPopupMenu) then
        for I := 0 to Items.Count - 1 do
          if TJvListItem(Items[I]).PopupMenu = AComponent then
            TJvListItem(Items[I]).PopupMenu := nil;
end;

procedure TJvListView.CreateWnd;
var
  Wnd: HWND;
begin
  inherited CreateWnd;
  
  UpdateHeaderImages(ListView_GetHeader(Handle));
  if FSavedExtendedColumns.Count > 0 then
    FExtendedColumns.Assign(FSavedExtendedColumns);

  // Get the values from the newly created list view
  LoadTileViewProperties;
  {$IFNDEF RTL200_UP}
  LoadGroupsProperties;
  {$ENDIF !RTL200_UP}
  FInsertMarkColor := SendMessage(Handle, LVM_GETINSERTMARKCOLOR, 0, 0);

  {$IFNDEF RTL200_UP}
  // Force a change from True to False so that InsertMarks work correctly.
  SendMessage(Handle, LVM_ENABLEGROUPVIEW, WPARAM(Integer(not FGroupView)), 0);
  SendMessage(Handle, LVM_ENABLEGROUPVIEW, WPARAM(Integer(FGroupView)), 0);
  {$ENDIF !RTL200_UP}
  if FSavedColumnOrder <> '' then
  begin
    ColumnsOrder := FSavedColumnOrder;
    FSavedColumnOrder := '';
  end;

  // This will ensure the HDN_Track notification message is sent:
  Wnd := GetWindow(Handle, GW_CHILD);
  SetWindowLong(wnd, GWL_STYLE, GetWindowLong(wnd, GWL_STYLE) and not HDS_FULLDRAG);
end;

procedure TJvListView.UpdateHeaderImages(HeaderHandle: Integer);
//var
//  WP: TWindowPlacement;
begin
  if (HeaderHandle <> 0) and (ViewStyle = vsReport) and ShowColumnHeaders then
  begin
//    WP.length := SizeOf(WP);
//    GetWindowPlacement(HeaderHandle, @WP);
    if HeaderImages <> nil then
    begin
      Header_SetImageList(HeaderHandle, HeaderImages.Handle);
//      WP.rcNormalPosition.Bottom := WP.rcNormalPosition.Top + HeaderImages.Height + 3;
    end
    else
      if ComponentState * [csLoading, csDestroying] = [] then
      begin
        Header_SetImageList(HeaderHandle, 0);
//      WP.rcNormalPosition.Bottom := WP.rcNormalPosition.Top + 17;
      end;
    // the problem with resizing the header is that there doesn't seem to be an easy way of telling the listview about it...
//    SetWindowPlacement(HeaderHandle, @WP);
    UpdateColumns;
    Windows.InvalidateRect(HeaderHandle, nil, True)
  end;
end;

procedure TJvListView.DoHeaderImagesChange(Sender: TObject);
begin
  UpdateHeaderImages(ListView_GetHeader(Handle));
end;

procedure TJvListView.SetSmallImages(const Value: TCustomImageList);
begin
  inherited SmallImages := Value;
  UpdateHeaderImages(ListView_GetHeader(Handle));
end;

procedure TJvListView.Loaded;
begin
  inherited Loaded;
  UpdateHeaderImages(ListView_GetHeader(Handle));
  TileViewProperties.DoChange;
end;

procedure TJvListView.WMNCCalcSize(var Msg: TWMNCCalcSize);
//var
//  R: TRect;
begin
  inherited;
//  if Msg.CalcValidRects and Assigned(HeaderImages) and (ViewStyle = vsReport) and ShowColumnHeaders then
//    with Msg.CalcSize_Params^.rgrc[0] do
//      Top := Top + HeaderImages.Height + 3;
end;

procedure TJvListView.WMNotify(var Msg: TWMNotify);
begin
  inherited;

  // Must be tested for in WM_NOTIFY handler because the CN_NOTIFY handler
  // does not receive them.
  // Must also be processed AFTER the inherited handler or the code won't work
  case Msg.NMHdr^.code of
    HDN_ENDTRACK:
      DoEndColumnResize(GetColumnIndex(Msg.NMHdr), GetColumnWidth(Msg.NMHdr));
    HDN_BEGINTRACK:
      if not DoBeginColumnResize(GetColumnIndex(Msg.NMHdr), GetColumnWidth(Msg.NMHdr)) Then
        Msg.Result := 1;
    HDN_TRACK:
      DoColumnResizing(GetColumnIndex(Msg.NMHdr), GetColumnWidth(Msg.NMHdr));
  end;
end;

procedure TJvListView.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if HandleAllocated then
    UpdateHeaderImages(ListView_GetHeader(Handle));
end;

procedure TJvListView.InsertItem(Item: TListItem);
begin
  inherited InsertItem(Item);
  if AutoSelect and (Selected = nil) and (Items.Count < 2) then
    PostMessage(Handle, WM_AUTOSELECT, WPARAM(Item), 1);
end;

procedure TJvListView.WMAutoSelect(var Msg: TMessage);
var
  lv: TListItem;
begin
  if AutoSelect and (Selected = nil) then // Mantis 6037: Prevent AutoSelect from stealing the selected item when processing messages
  begin
    with Msg do
    begin
      lv := TListItem(WParam);
      if Assigned(lv) and (Items.IndexOf(lv) >= 0) and (LParam = 1) then
      begin
        lv.Selected := True;
        lv.Focused := True;
      end;
    end;
  end;
end;

function TJvListView.MoveDown(Index: Integer; Focus: Boolean = True): Integer;
var
  lv, lv2: TListItem;
  FOnInsert, FOnDeletion: TLVDeletedEvent;
  FOnCompare: TLVCompareEvent;
begin
  Result := Index;
  if (Index >= 0) and (Index < Items.Count) then
  begin
    lv2 := Items[Index];
    FOnInsert := OnInsert;
    FOnDeletion := OnDeletion;
    FOnCompare := OnCompare;
    try
      OnInsert := nil;
      OnDeletion := nil;
      OnCompare := nil;
      lv := Items.Insert(Index + 2);
      lv.Assign(lv2);
      lv2.Delete;
    finally
      OnInsert := FOnInsert;
      OnDeletion := FOnDeletion;
      OnCompare := FOnCompare;
    end;
    if Focus then
    begin
      lv.Selected := True;
      lv.Focused := True;
    end;
    Result := lv.Index;
  end;
end;

function TJvListView.MoveUp(Index: Integer; Focus: Boolean = True): Integer;
var
  lv, lv2: TListItem;
  FOnInsert, FOnDeletion: TLVDeletedEvent;
  FOnCompare: TLVCompareEvent;
begin
  Result := Index;
  if (Index > 0) and (Index < Items.Count) then
  begin
    lv2 := Items[Index];
    FOnInsert := OnInsert;
    FOnDeletion := OnDeletion;
    FOnCompare := OnCompare;
    try
      OnInsert := nil;
      OnDeletion := nil;
      OnCompare := nil;
      lv := Items.Insert(Index - 1);
      lv.Assign(lv2);
      lv2.Delete;
    finally
      OnInsert := FOnInsert;
      OnDeletion := FOnDeletion;
      OnCompare := FOnCompare;
    end;
    if Focus then
    begin
      lv.Selected := True;
      lv.Focused := True;
    end;
    Result := lv.Index;
  end;
end;

function TJvListView.SelectNextItem(Focus: Boolean = True): Integer;
begin
  Result := ItemIndex + 1;
  if Result < Items.Count then
    ItemIndex := Result;
  Result := ItemIndex;
  if Focus and (Result >= 0) and (Result < Items.Count) then
  begin
    Items[Result].Selected := True;
    Items[Result].Focused := True;
  end;
end;

function TJvListView.SelectPrevItem(Focus: Boolean = True): Integer;
begin
  Result := ItemIndex - 1;
  if Result >= 0 then
    ItemIndex := Result;
  Result := ItemIndex;
  if Focus and (Result >= 0) and (Result < Items.Count) then
  begin
    Items[Result].Selected := True;
    Items[Result].Focused := True;
  end;
end;

procedure TJvListView.SetFocus;
var
  Index: Integer;
begin
  inherited SetFocus;

  Index := 0;
  if Assigned(ItemFocused) then
    Index := ItemIndex;

  if AutoSelect and (Selected = nil) and (Items.Count > 0) then
    PostMessage(Handle, WM_AUTOSELECT, WPARAM(Items[Index]), 1);
end;

function TJvListView.ShowInsertMark(ItemIndex: Integer; Position: TJvInsertMarkPosition): Boolean;
var
  Infos: TLVINSERTMARK;
begin
  ZeroMemory(@Infos, SizeOf(Infos));

  Infos.cbSize := SizeOf(Infos);
  Infos.dwFlags := InsertMarkPositionToLVIM[Position];
  Infos.iItem := ItemIndex;

  Result := Bool(SendMessage(Handle, LVM_SETINSERTMARK, 0, LPARAM(@Infos)));
end;

function TJvListView.HideInsertMark: Boolean;
begin
  Result := ShowInsertMark(-1, impBefore);
end;

function TJvListView.GetInsertMarkPosition(const X, Y: Integer;
  var ItemIndex: Integer; var Position: TJvInsertMarkPosition): Boolean;
var
  Infos: TLVINSERTMARK;
  Point: TPoint;
begin
  Point.X := X;
  Point.Y := Y;

  ZeroMemory(@Infos, SizeOf(Infos));

  Infos.cbSize := SizeOf(Infos);
  Result := Bool(SendMessage(Handle, LVM_INSERTMARKHITTEST, WPARAM(@Point), LPARAM(@Infos)));
  if Result then
  begin
    ItemIndex := Infos.iItem;
    if (Infos.dwFlags and LVIM_AFTER) = LVIM_AFTER then
      Position := impAfter
    else
      Position := impBefore;
  end;
end;

function TJvListView.IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean;
begin
  { We must custom draw both cdPrePaint and cdPostPaint because without the
    cdPostPaint the TListView creates GDI fonts without releasing them. }
  Result := inherited IsCustomDrawn(Target, Stage) or
    ((Stage in [cdPrePaint, cdPostPaint]) and (Picture.Graphic <> nil) and not Picture.Graphic.Empty) or
    ((Stage in [cdPrePaint, cdPostPaint]) and ((Target = dtItem) or (Target = dtSubItem)));
end;


function TJvListView.CustomDraw(const ARect: TRect; Stage: TCustomDrawStage): Boolean;
var
  BmpXPos, BmpYPos: Integer; // X and Y position for bitmap
  ItemRect: TRect; // List item bounds rectangle
  TopOffset: Integer; // Y pos where bmp drawing starts
  Bmp: TBitmap;

  function GetHeaderHeight: Integer;
  var
    Header: HWND; // header window handle
    Pl: TWindowPlacement; // header window placement
  begin
    // Get header window
    Header := SendMessage(Handle, LVM_GETHEADER, 0, 0);
    // Get header window placement
    FillChar(Pl, SizeOf(Pl), 0);
    Pl.length := SizeOf(Pl);
    GetWindowPlacement(Header, @Pl);
    // Calculate header window height
    Result := Pl.rcNormalPosition.Bottom - Pl.rcNormalPosition.Top;
  end;

begin
  Result := inherited CustomDraw(ARect, Stage);
  if Result and (Stage = cdPrePaint) and (FPicture <> nil) and (FPicture.Graphic <> nil) and not
    FPicture.Graphic.Empty and (FPicture.Graphic.Width > 0) and (FPicture.Graphic.Height > 0) then
  begin
    Bmp := TBitmap.Create;
    try
      Bmp.Width := ClientWidth;
      Bmp.Height := ClientHeight;
      Bmp.Canvas.Brush.Color := Self.Color;
      Bmp.Canvas.FillRect(ClientRect);

    // Get top offset where drawing starts
      if Items.Count > 0 then
      begin
        ListView_GetItemRect(Handle, 0, ItemRect, LVIR_BOUNDS);
        TopOffset := ListView_GetTopIndex(Handle) * (ItemRect.Bottom - ItemRect.Top);
      end
      else
        TopOffset := 0;
      if ViewStyle = vsReport then
        BmpYPos := ARect.Top - TopOffset + GetHeaderHeight
      else
        BmpYPos := 0;
      // Draw the image
      while BmpYPos < ARect.Bottom do
      begin
        // draw image across width of display
        BmpXPos := ARect.Left;
        while BmpXPos < ARect.Right do
        begin
//      DrawIconEx draws alpha-blended icons better (on XP) but gives problems with selecting in the listview
//      if Picture.Graphic is TIcon then
//        DrawIconEx(Canvas.Handle, BmpXPos, BmpYPos, Picture.Icon.Handle, 0, 0, 0, 0, DI_NORMAL)
//      else
          Bmp.Canvas.Draw(BmpXPos, BmpYPos, Picture.Graphic);
          Inc(BmpXPos, Picture.Graphic.Width);
        end;
        // move to next row
        Inc(BmpYPos, Picture.Graphic.Height);
      end;
      BitBlt(Canvas, 0, 0, ClientWidth, ClientHeight, Bmp.Canvas, 0, 0, SRCCOPY);
    // Ensure that the items are drawn transparently
      SetBkMode(Canvas.Handle, TRANSPARENT);
      ListView_SetTextBkColor(Handle, CLR_NONE);
      ListView_SetBKColor(Handle, CLR_NONE);
    finally
      Bmp.Free;
    end;
  end;
end;

function TJvListView.CustomDrawItem(Item: TListItem;
  State: TCustomDrawState; Stage: TCustomDrawStage): Boolean;
var
  TextColor: TColorRef;
begin
  TextColor := 0; // silence the compiler
  if (Stage = cdPrePaint) and Assigned(Item) then
  begin
    TextColor := GetTextColor(Canvas.Handle);
    Canvas.Font := TJvListItem(Item).Font;
    if ViewStyle in ViewStylesItemBrush then
    begin
      if JclCheckWinVersion(6, 0) then
        SetBkMode(Canvas.Handle, TRANSPARENT);
      Canvas.Brush := TJvListItem(Item).Brush;
    end;
    Canvas.Handle;
  end;

  Result := inherited CustomDrawItem(Item, State, Stage);

  // Restore the text color to allow the ListView to paint the focus rectangle correctly.
  if (Stage = cdPrePaint) and Assigned(Item) then
    SetTextColor(Canvas.Handle, TextColor);
end;


procedure TJvListView.CNNotify(var Message: TWMNotify);
var
  HitTestInfo: TLVHitTestInfo;
begin
  with Message do
  begin
    case NMHdr^.code of
      NM_CUSTOMDRAW:
        with PNMCustomDraw(NMHdr)^ do
        begin
          if (dwDrawStage and CDDS_SUBITEM <> 0) and
             (PNMLVCustomDraw(NMHdr)^.iSubItem = 0) then
          begin
            // Mantis 3908: For some reason, the inherited handler will not call
            // the CustomDrawSubItem if iSubItem is equal to zero. But not calling
            // it has the consequence to trigger wrong rendering if the order of
            // columns is modified and the list item has a non standard font.
            // Calling it ourselves here is not enough as the inherited handler
            // does some very specific management with the canvas. So we must
            // trick it by changing the value to a recognizable value used
            // in our CustomDrawSubItem handler.
            PNMLVCustomDraw(NMHdr)^.iSubItem := -1;
            inherited;
            PNMLVCustomDraw(NMHdr)^.iSubItem := 0;
            Exit;
          end;
        end;

      LVN_ENDLABELEDITA, LVN_ENDLABELEDITW:
        with PLVDispInfo(Message.NMHdr)^ do
          if (item.pszText = nil) and (item.iItem <> -1) then
            EditCanceled(Items[item.iItem]);

      NM_CLICK, NM_DBLCLK:
        with PNMListView(NMHdr)^ do
        begin
          HitTestInfo.iItem := iItem;
          if HitTestInfo.iItem = -1 then
          begin
            HitTestInfo.pt := ptAction;
            ListView_SubItemHitTest(Handle, @HitTestInfo);
          end;
          if HitTestInfo.iItem <> -1 then
          begin
            if NMHdr^.code = NM_CLICK then
              ItemClick(Items[HitTestInfo.iItem], iSubItem - 1, ptAction.X, ptAction.Y)
            else
              ItemDblClick(Items[HitTestInfo.iItem], iSubItem - 1, ptAction.X, ptAction.Y);
          end;
        end;
    end;
  end;

  inherited;
end;

procedure TJvListView.ItemClick(AItem: TListItem; SubItemIndex: Integer; X, Y: Integer);
begin
  if Assigned(FOnItemClick) then
    FOnItemClick(Self, AItem, SubItemIndex, X, Y);
end;

procedure TJvListView.ItemDblClick(AItem: TListItem; SubItemIndex: Integer; X, Y: Integer);
begin
  if Assigned(FOnItemDblClick) then
    FOnItemDblClick(Self, AItem, SubItemIndex, X, Y);
end;

procedure TJvListView.EditCanceled(Item: TListItem);
begin
  if Assigned(FOnCancelEdit) then
    FOnCancelEdit(Self, Item);
end;

function TJvListView.CustomDrawSubItem(Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage): Boolean;
begin
  if SubItem = -1 then      // See above
    SubItem := 0;

  if {(Stage = cdPrePaint) and} Assigned(Item) then
  begin
    Canvas.Font := TJvListItem(Item).Font;
    if ViewStyle in ViewStylesItemBrush then
      Canvas.Brush := TJvListItem(Item).Brush;
  end;

  Result := inherited CustomDrawSubItem(Item, SubItem, State, Stage);
end;

procedure TJvListView.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
end;

{$IFNDEF RTL200_UP}
procedure TJvListView.SetGroupView(const Value: Boolean);
begin
  if FGroupView <> Value then
  begin
    FGroupView := Value;
    SendMessage(Handle, LVM_ENABLEGROUPVIEW, WPARAM(FGroupView), 0);
  end;
end;

procedure TJvListView.SetGroups(const Value: TJvListViewGroups);
begin
  FGroups.Assign(Value);
end;

procedure TJvListView.SetGroupsProperties(const Value: TJvGroupsProperties);
begin
  FGroupsProperties.Assign(Value);
end;
{$ENDIF !RTL200_UP}

procedure TJvListView.SetTileViewProperties(const Value: TJvTileViewProperties);
begin
  FTileViewProperties.Assign(Value);
end;

procedure TJvListView.SetInsertMarkColor(const Value: TColor);
begin
  if FInsertMarkColor <> Value then
  begin
    FInsertMarkColor := Value;

    SendMessage(Handle, LVM_SETINSERTMARKCOLOR, 0, ColorToRGB(FInsertMarkColor));
  end;
end;

procedure TJvListView.SetHeaderImagePosition(const Value: TJvHeaderImagePosition);
begin
  if FHeaderImagePosition <> Value then
  begin
    FHeaderImagePosition := Value;
    UpdateHeaderImages(ListView_GetHeader(Handle));
  end;
end;

procedure TJvListView.DoPictureChange(Sender: TObject);
begin
//  if (Picture.Graphic <> nil) and not Picture.Graphic.Empty then
//    Picture.Graphic.Transparent := true;
  Invalidate;
end;

procedure TJvListView.LVMDeleteColumn(var Msg: TMessage);
begin
  inherited;
  // This may happen at design time, especially when migrating
  // a project that uses an old version of TJvListView that did
  // not have the ExtendedColumns
  if Msg.WParam < WPARAM(FExtendedColumns.Count) then
    FExtendedColumns.Delete(Msg.WParam);
end;

procedure TJvListView.LVMInsertColumn(var Msg: TMessage);
begin
  inherited;
  FExtendedColumns.Insert(Msg.WParam);
end;

procedure TJvListView.LVMSetColumn(var Msg: TMessage);
var
  i: Integer;
  Column: tagLVCOLUMN;
begin
  inherited;

  if not FSettingHeaderImagePosition then
  begin
    for i := 0 to ExtendedColumns.Count - 1 do
    begin
      if ExtendedColumns[i].GetHeaderImagePosition = hipRight then
      begin
        Column.mask := LVCF_FMT;
        ListView_GetColumn(Handle, i, Column);
        if Column.fmt and LVCFMT_IMAGE <> 0 then
        begin
          Column.fmt := Column.fmt or LVCFMT_BITMAP_ON_RIGHT;
          FSettingHeaderImagePosition := True;
          try
            ListView_SetColumn(Handle, i, Column);
          finally
            FSettingHeaderImagePosition := False;
          end;
        end;
      end;
    end;
  end;
end;

procedure TJvListView.DestroyWnd;
begin
  if not (csDestroying in ComponentState) then
  begin
    FSavedColumnOrder := ColumnsOrder;
    FSavedExtendedColumns.Assign(FExtendedColumns);
  end;
  inherited DestroyWnd;
end;

procedure TJvListView.SetViewStylesItemBrush(const Value: TJvViewStyles);
begin
  FViewStylesItemBrush := Value;
  Invalidate;
end;

procedure TJvListView.SetViewStyle(Value: TViewStyle);
begin
  // If someone is setting the view style via an ancestor class reference,
  // we force it to be set through our setter. But if it's set via our setter
  // then we inform the ancestor class' code so that display is updated.
  if not FSettingJvViewStyle then
    SetJvViewStyle(TJvViewStyle(Value))
  else
    inherited SetViewStyle(Value);
end;

procedure TJvListView.SetJvViewStyle(Value: TJvViewStyle);
begin
  if Value <> FViewStyle then
  begin
    FSettingJvViewStyle := True;
    try
      FViewStyle := Value;
      if Value = vsTile then
      begin
        if HandleAllocated then
        begin
          SendMessage(Handle, LVM_SETVIEW, LV_VIEW_TILE, 0);
        end;
      end
      else
      begin
        inherited ViewStyle := TViewStyle(Value);
      end;
    finally
      FSettingJvViewStyle := False;
    end;
  end;
end;

function TJvListView.AreItemsStored: Boolean;
begin
  if Action <> nil then
    Result := not (Action is TCustomListAction)
  else
    Result := not OwnerData;
end;

function TJvListView.GetListItems: TJvListItems;
begin
  Result := inherited Items as TJvListItems;
end;

procedure TJvListView.SetListItems(const Value: TJvListItems);
begin
  inherited Items := Value;
end;

procedure TJvListView.SetLastSortedColumnIndex(const Value: Integer);
begin
  if FLastSortedColumnIndex <> Value then
  begin
    FLastSortedColumnIndex := Value;
    ColClick(Columns[Value]);
  end;
end;

function TJvListView.DoBeginColumnResize(ColumnIndex,
  ColumnWidth: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnBeginColumnResize) then
    FOnBeginColumnResize(Self, ColumnIndex, ColumnWidth, Result);
end;

{$IFNDEF RTL200_UP}
function TJvListView.DoCompareGroups(Group1, Group2: TJvListViewGroup): Integer;
begin
  if Assigned(OnCompareGroups) then
    OnCompareGroups(Self, Group1, Group2, Result)
  else
    Result := Group2.GroupId - Group1.GroupId;
end;
{$ENDIF !RTL200_UP}

procedure TJvListView.DoColumnResizing(ColumnIndex, ColumnWidth: Integer);
begin
  if Assigned(FOnColumnResizing) then
    FOnColumnResizing(Self, ColumnIndex, ColumnWidth);
end;

procedure TJvListView.DoEndColumnResize(ColumnIndex, ColumnWidth: Integer);
begin
  if Assigned(FOnEndColumnResize) then
    FOnEndColumnResize(Self, ColumnIndex, ColumnWidth);
end;

procedure TJvListView.TileViewPropertiesChange(Sender: TObject);
var
  Infos: TLVTILEVIEWINFO;
begin
  if not (csLoading in ComponentState) then
  begin
    Infos.cbSize := SizeOf(Infos);
    Infos.dwMask := LVTVIM_TILESIZE or LVTVIM_COLUMNS or LVTVIM_LABELMARGIN;
    Infos.dwFlags := TileSizeKindToLVTVIF[TileViewProperties.TileSizeKind];
    TileViewProperties.TileSize.CopyToSize(Infos.sizeTile);
    infos.cLines := TileViewProperties.SubLinesCount;
    TileViewProperties.LabelMargin.CopyToRect(infos.rcLabelMargin);

    SendMessage(Handle, LVM_SETTILEVIEWINFO, 0, LPARAM(@Infos));
  end;
end;

{$IFNDEF RTL200_UP}
procedure TJvListView.GroupsPropertiesChange(Sender: TObject);
var
  Infos: TLVGROUPMETRICS;
begin
  if not (csLoading in ComponentState) then
  begin
    ZeroMemory(@Infos, SizeOf(Infos));

    Infos.cbSize := SizeOf(Infos);
    Infos.mask := LVGMF_BORDERSIZE or LVGMF_BORDERCOLOR or LVGMF_TEXTCOLOR;
    Infos.Top := GroupsProperties.BorderSize.Top;
    Infos.Left := GroupsProperties.BorderSize.Left;
    Infos.Bottom := GroupsProperties.BorderSize.Bottom;
    Infos.Right := GroupsProperties.BorderSize.Right;
    Infos.crTop := GroupsProperties.BorderColor.Top;
    Infos.crLeft := GroupsProperties.BorderColor.Left;
    Infos.crBottom := GroupsProperties.BorderColor.Bottom;
    Infos.crRight := GroupsProperties.BorderColor.Right;
    Infos.crHeader := GroupsProperties.HeaderColor;

    SendMessage(Handle, LVM_SETGROUPMETRICS, 0, LPARAM(@Infos));
  end;
end;
{$ENDIF !RTL200_UP}

procedure TJvListView.LoadTileViewProperties;
begin
  TileViewProperties.LoadFromList(Self);
end;

{$IFNDEF RTL200_UP}
procedure TJvListView.LoadGroupsProperties;
begin
  GroupsProperties.LoadFromList(Self);
end;
{$ENDIF !RTL200_UP}

{$IFNDEF RTL200_UP}
  { TJvListViewGroup }

procedure TJvListViewGroup.Assign(AValue: TPersistent);
var
  Source: TJvListViewGroup;
begin
  if AValue is TJvListViewGroup then
  begin
    Source := AValue as TJvListViewGroup;

    FHeader := Source.Header;
    FHeaderAlignment := Source.HeaderAlignment;
    FGroupId := Source.GroupId;
    UpdateGroupProperties;
  end;
end;

constructor TJvListViewGroup.Create(Collection: Classes.TCollection);
begin
  // Before inherited for Notify to acces it
  FGroupId := -1;
  FHeaderAlignment := taLeftJustify;
  FHeader := 'Group';

  inherited Create(Collection);
end;

destructor TJvListViewGroup.Destroy;
begin
  inherited Destroy;
end;

procedure TJvListViewGroup.SetHeader(const Value: WideString);
var
  SavedGroupId: Integer;
begin
  if FHeader <> Value then
  begin
    FHeader := Value;

    // Due to a undocumented bug/feature in the list view, one has to change
    // the GroupId as well when changing the caption or the modification is
    // not taken into account.
    SavedGroupId := GroupId;
    UpdateGroupProperties(MaxInt);
    FGroupId := MaxInt;
    UpdateGroupProperties(SavedGroupId);
    FGroupId := SavedGroupId;
  end;
end;

procedure TJvListViewGroup.SetHeaderAlignment(const Value: TAlignment);
begin
  if FHeaderAlignment <> Value then
  begin
    FHeaderAlignment := Value;
    UpdateGroupProperties;
  end;
end;

procedure TJvListViewGroup.SetLVGROUP(var GroupInfo: TLVGROUP);
begin
  ZeroMemory(@GroupInfo, sizeof(GroupInfo));

  GroupInfo.cbSize := sizeof(GroupInfo);
  GroupInfo.mask := LVGF_HEADER or LVGF_ALIGN or LVGF_GROUPID;
  GroupInfo.iGroupId := FGroupId;
  GroupInfo.pszHeader := PWideChar(FHeader);
  GroupInfo.cchHeader := Length(FHeader);
  GroupInfo.uAlign := AlignmentToLVGA[HeaderAlignment];
end;

procedure TJvListViewGroup.SetGroupId(const Value: Integer);
begin
  if FGroupId <> Value then
  begin
    UpdateGroupProperties(Value);
    FGroupId := Value;
  end;
end;

procedure TJvListViewGroup.UpdateGroupProperties(const NewGroupId: Integer = -1);
var
  GroupInfo: TLVGROUP;
  List: TJvListView;
begin
  List := (Collection as TJvListViewGroups).ParentList;
  if Assigned(List) then
  begin
    SetLVGROUP(GroupInfo);
    if NewGroupId <> -1 then
      GroupInfo.iGroupId := NewGroupId;
    SendMessage(List.Handle, LVM_SETGROUPINFO, FGroupId, LPARAM(@GroupInfo));
    List.Invalidate;
  end;
end;

{ TJvListViewGroups }

function TJvListViewGroups.Compare(Id1, Id2: Integer): Integer;
var
  List: TJvListView;
begin
  Result := Id2 - Id1;
  List := ParentList;
  if Assigned(List) then
  begin
    Result := List.DoCompareGroups(ItemsById[Id1], ItemsById[Id2]);
  end;
end;

constructor TJvListViewGroups.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJvListViewGroup);
end;

function TJvListViewGroups.GetItem(Index: Integer): TJvListViewGroup;
begin
  Result := inherited Items[Index] as TJvListViewGroup;
end;

function TJvListViewGroups.GetItemById(GroupId: Integer): TJvListViewGroup;
var
  I: Integer;
begin
  Result := nil;
  I := 0;
  while (I < Count) and not Assigned(Result) do
  begin
    if Items[I].GroupId = GroupId then
      Result := Items[I];
    Inc(I);
  end;
end;

function LVGroupCompare(Group1_ID: Integer; Group2_ID: Integer; pvData: Pointer): Integer; stdcall;
begin
  Result := TJvListViewGroups(pvData).Compare(Group1_ID, Group2_ID);
end;

procedure TJvListViewGroups.InsertGroupIntoList(group: TJvListViewGroup);
var
  List: TJvListView;
  GroupInfo: TLVGROUP;
  GroupSortedInfo: TLVINSERTGROUPSORTED;
begin
  List := ParentList;
  if Assigned(List) then
  begin
    if group.GroupId = -1 then
      group.FGroupId := Count;
    if Sorted then
    begin
      GroupSortedInfo.pfnGroupCompare := @LVGroupCompare;
      GroupSortedInfo.pvData := Self;
      group.SetLVGROUP(GroupSortedInfo.lvGroup);
      SendMessage(List.Handle, LVM_INSERTGROUPSORTED, WPARAM(@GroupSortedInfo), 0);
    end
    else
    begin
      group.SetLVGROUP(GroupInfo);
      SendMessage(List.Handle, LVM_INSERTGROUP, group.Index, LPARAM(@GroupInfo));
    end;
  end;
end;

procedure TJvListViewGroups.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  case Action of
    cnAdded:
      InsertGroupIntoList(Item as TJvListViewGroup);
    cnDeleting:
      RemoveGroupFromList(Item as TJvListViewGroup);
  end;
end;

function TJvListViewGroups.ParentList: TJvListView;
var
  Owner: TPersistent;
begin
  Result := nil;
  Owner := GetOwner;
  if Owner is TJvListView then
    Result := Owner as TJvListView;
end;

procedure TJvListViewGroups.RemoveGroupFromList(group: TJvListViewGroup);
var
  List: TJvListView;
begin
  List := ParentList;
  if Assigned(List) then
  begin
    SendMessage(List.Handle, LVM_REMOVEGROUP, group.GroupId, 0);
  end;
end;

procedure TJvListViewGroups.SetItem(Index: Integer;
  const Value: TJvListViewGroup);
begin
  inherited Items[Index] := Value;
end;

procedure TJvListViewGroups.SetSorted(const Value: Boolean);
begin
  if FSorted <> Value then
  begin
    FSorted := Value;
    if FSorted then
      Sort;
  end;
end;

procedure TJvListViewGroups.Sort;
var
  List: TJvListView;
begin
  List := ParentList;
  if Assigned(List) then
  begin
    SendMessage(List.Handle, LVM_SORTGROUPS, WPARAM(@LVGroupCompare), LPARAM(Self));
  end;
end;
{$ENDIF !RTL200_UP}

  { TJvTileViewProperties }

constructor TJvTileViewProperties.Create;
begin
  inherited Create;

  FLabelMargin := TJvRect.Create;
  FTileSize := TJvSize.Create;

  FLabelMargin.OnChange := LabelMarginChange;
  FTileSize.OnChange := TileSizeChange;

  FSubLinesCount := 1;
  FTileSizeKind := tskAutoSize;
end;

destructor TJvTileViewProperties.Destroy;
begin
  FTileSize.Free;
  FLabelMargin.Free;

  inherited Destroy;
end;

procedure TJvTileViewProperties.DoChange;
begin
  if not FLoading and Assigned(OnChange) then
    OnChange(Self);
end;

procedure TJvTileViewProperties.LabelMarginChange(Sender: TObject);
begin
  DoChange;
end;

procedure TJvTileViewProperties.LoadFromList(List: TCustomListView);
var
  Infos: TLVTILEVIEWINFO;
begin
  if not (csDesigning in List.ComponentState) then
  begin
    Infos.cbSize := SizeOf(Infos);
    Infos.dwMask := LVTVIM_TILESIZE or LVTVIM_COLUMNS or LVTVIM_LABELMARGIN;

    SendMessage(List.Handle, LVM_GETTILEVIEWINFO, 0, LPARAM(@Infos));

    FLoading := True;
    try
      case Infos.dwFlags of
        LVTVIF_FIXEDHEIGHT:
          FTileSizeKind := tskFixedHeight;
        LVTVIF_FIXEDWIDTH:
          FTileSizeKind := tskFixedWidth;
        LVTVIF_FIXEDSIZE:
          FTileSizeKind := tskFixedSize;
        else
          FTileSizeKind := tskAutoSize;
      end;
      TileSize.Assign(Infos.sizeTile);
      FSubLinesCount := infos.cLines;
      LabelMargin.Assign(infos.rcLabelMargin);
    finally
      FLoading := False;
    end;
  end;
end;

procedure TJvTileViewProperties.SetLabelMargin(const Value: TJvRect);
begin
  FLabelMargin.Assign(Value);
end;

procedure TJvTileViewProperties.SetSubLinesCount(const Value: Integer);
begin
  FSubLinesCount := Value;
  DoChange;
end;

procedure TJvTileViewProperties.SetTileSize(const Value: TJvSize);
begin
  FTileSize.Assign(Value);
end;

procedure TJvTileViewProperties.SetTileSizeKind(const Value: TJvTileSizeKind);
begin
  FTileSizeKind := Value;
  DoChange;
end;

procedure TJvTileViewProperties.TileSizeChange(Sender: TObject);
begin
  DoChange;
end;

{$IFNDEF RTL200_UP}
  { TJvGroupProperties }

procedure TJvGroupsProperties.BorderColorChange(Sender: TObject);
begin
  DoChange;
end;

procedure TJvGroupsProperties.BorderSizeChange(Sender: TObject);
begin
  DoChange;
end;

constructor TJvGroupsProperties.Create;
begin
  inherited Create;

  FBorderSize := TJvGroupsPropertiesBorderRect.Create;
  FBorderColor := TJvGroupsPropertiesBorderColors.Create;

  FBorderSize.OnChange := BorderSizeChange;
  FBorderColor.OnChange := BorderColorChange;
end;

destructor TJvGroupsProperties.Destroy;
begin
  FBorderSize.Free;
  FBorderColor.Free;

  inherited Destroy;
end;

procedure TJvGroupsProperties.DoChange;
begin
  if not FLoading and Assigned(OnChange) then
    OnChange(Self);
end;

procedure TJvGroupsProperties.LoadFromList(List: TCustomListView);
var
  Infos: TLVGROUPMETRICS;
begin
  if not (csDesigning in List.ComponentState) then
  begin
    ZeroMemory(@Infos, SizeOf(Infos));

    Infos.cbSize := SizeOf(Infos);
    Infos.mask := LVGMF_BORDERSIZE or LVGMF_BORDERCOLOR or LVGMF_TEXTCOLOR;
    SendMessage(List.Handle, LVM_GETGROUPMETRICS, 0, LPARAM(@Infos));

    FLoading := True;
    try
      BorderSize.Top := Infos.Top;
      BorderSize.Left := Infos.Left;
      BorderSize.Bottom := Infos.Bottom;
      BorderSize.Right := Infos.Right;
      BorderColor.Top := Infos.crTop and $00FFFFFF;
      BorderColor.Left := Infos.crLeft and $00FFFFFF;
      BorderColor.Bottom := Infos.crBottom and $00FFFFFF;
      BorderColor.Right := Infos.crRight and $00FFFFFF;
      HeaderColor := Infos.crHeader and $00FFFFFF;
    finally
      FLoading := False;
    end;
  end;
end;

procedure TJvGroupsProperties.SetBorderColor(const Value: TJvGroupsPropertiesBorderColors);
begin
  FBorderColor.Assign(Value);
end;

procedure TJvGroupsProperties.SetBorderSize(const Value: TJvGroupsPropertiesBorderRect);
begin
  FBorderSize.Assign(Value);
end;

procedure TJvGroupsProperties.SetHeaderColor(const Value: TColor);
begin
  if FHeaderColor <> Value then
  begin
    FHeaderColor := Value;
    DoChange;
  end;
end;

  { TJvGroupsPropertiesBorderRect }

constructor TJvGroupsPropertiesBorderRect.Create;
begin
  inherited Create;

  Top := 12;
end;

{ TJvGroupsPropertiesBorderColors }

procedure TJvGroupsPropertiesBorderColors.Assign(Source: TPersistent);
var
  SourceColors: TJvGroupsPropertiesBorderColors;
begin
  if Source is TJvGroupsPropertiesBorderColors then
  begin
    SourceColors := Source as TJvGroupsPropertiesBorderColors;
    FTop := SourceColors.Top;
    FLeft := SourceColors.Left;
    FBottom := SourceColors.Bottom;
    FRight := SourceColors.Right;
    DoChange;
  end
  else
  begin
    inherited Assign(Source);
  end;
end;

constructor TJvGroupsPropertiesBorderColors.Create;
begin
  inherited Create;

  Top := $C8D0D4;
  Left := clWhite;
  Bottom := clWhite;
  Right := clWhite;
end;

procedure TJvGroupsPropertiesBorderColors.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TJvGroupsPropertiesBorderColors.SetBottom(const Value: TColor);
begin
  if FBottom <> Value then
  begin
    FBottom := Value;
    DoChange;
  end;
end;

procedure TJvGroupsPropertiesBorderColors.SetLeft(const Value: TColor);
begin
  if FLeft <> Value then
  begin
    FLeft := Value;
    DoChange;
  end;
end;

procedure TJvGroupsPropertiesBorderColors.SetRight(const Value: TColor);
begin
  if FRight <> Value then
  begin
    FRight := Value;
    DoChange;
  end;
end;

procedure TJvGroupsPropertiesBorderColors.SetTop(const Value: TColor);
begin
  if FTop <> Value then
  begin
    FTop := Value;
    DoChange;
  end;
end;
{$ENDIF !RTL200_UP}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
