{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBCtrl.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}


unit JvDBCtrl;

interface

uses 
  Windows, Registry,
{$IFDEF COMPILER6_UP}
   Variants,
{$ENDIF}
  Messages, Classes, Controls, Forms, Grids, Graphics, Buttons, Menus,
  StdCtrls, Mask, IniFiles, JvToolEdit, DB, DBGrids, 
  {$IFNDEF COMPILER3_UP} DBTables, {$ENDIF}
  JvPlacemnt, JvDateUtil, DBCtrls, JvxCtrls, JvCurrEdit;

{ TJvDBGrid }

const
  DefRxGridOptions = [dgEditing, dgTitles, dgIndicator, dgColumnResize,
    dgColLines, dgRowLines, dgConfirmDelete, dgCancelOnExit];

{$IFDEF COMPILER35_UP}
 {$IFDEF CBUILDER}
  {$NODEFINE DefRxGridOptions}
 {$ENDIF}
{$ENDIF}

type
  TTitleClickEvent = procedure (Sender: TObject; ACol: Longint;
    Field: TField) of object;
  TCheckTitleBtnEvent = procedure (Sender: TObject; ACol: Longint;
    Field: TField; var Enabled: Boolean) of object;
  TGetCellParamsEvent = procedure (Sender: TObject; Field: TField;
    AFont: TFont; var Background: TColor; Highlight: Boolean) of object;
  TSortMarker = (smNone, smDown, smUp);
  TGetBtnParamsEvent = procedure (Sender: TObject; Field: TField;
    AFont: TFont; var Background: TColor; var SortMarker: TSortMarker;
    IsDown: Boolean) of object;
  TGetCellPropsEvent = procedure (Sender: TObject; Field: TField;
    AFont: TFont; var Background: TColor) of object; { obsolete }
  TDBEditShowEvent = procedure (Sender: TObject; Field: TField;
    var AllowEdit: Boolean) of object;

{$IFNDEF WIN32}
  TBookmarkList = class
  private
    FList: TJvHugeList;
    FGrid: TCustomDBGrid;
    FCache: TBookmark;
    FCacheIndex: Longint;
    FCacheFind: Boolean;
    FLinkActive: Boolean;
    function GetCount: Longint;
    function GetCurrentRowSelected: Boolean;
    function GetItem(Index: Longint): TBookmark;
    procedure SetCurrentRowSelected(Value: Boolean);
    procedure ListChanged;
  protected
    function CurrentRow: TBookmark;
    function Compare(const Item1, Item2: TBookmark): Longint;
    procedure LinkActive(Value: Boolean);
  public
    constructor Create(AGrid: TCustomDBGrid);
    destructor Destroy; override;
    procedure Clear;  { free all bookmarks }
    procedure Delete; { delete all selected rows from dataset }
    function Find(const Item: TBookmark; var Index: Longint): Boolean;
    function IndexOf(const Item: TBookmark): Longint;
    function Refresh: Boolean; { drop orphaned bookmarks; True = orphans found }
    property Count: Longint read GetCount;
    property CurrentRowSelected: Boolean read GetCurrentRowSelected
      write SetCurrentRowSelected;
    property Items[Index: Longint]: TBookmark read GetItem; default;
  end;
{$ENDIF}

  TJvDBGrid = class(TDBGrid)
  private
    FShowGlyphs: Boolean;
    FDefaultDrawing: Boolean;
    FMultiSelect: Boolean;
    FSelecting: Boolean;
    FClearSelection: Boolean;
    FTitleButtons: Boolean;
{$IFDEF WIN32}
    FPressedCol: TColumn;
{$ELSE}
    FPressedCol: Longint;
{$ENDIF}
    FPressed: Boolean;
    FTracking: Boolean;
    FSwapButtons: Boolean;
    FIniLink: TJvIniLink;
    FDisableCount: Integer;
    FFixedCols: Integer;
    FMsIndicators: TImageList;
    FOnCheckButton: TCheckTitleBtnEvent;
    FOnGetCellProps: TGetCellPropsEvent;
    FOnGetCellParams: TGetCellParamsEvent;
    FOnGetBtnParams: TGetBtnParamsEvent;
    FOnEditChange: TNotifyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnTitleBtnClick: TTitleClickEvent;
    FOnShowEditor: TDbEditShowEvent;
    FOnTopLeftChanged: TNotifyEvent;
{$IFDEF WIN32}
    FSelectionAnchor: TBookmarkStr;
{$ELSE}
    FSelectionAnchor: TBookmark;
    FBookmarks: TBookmarkList;
    FOnColumnMoved: TMovedEvent;
{$ENDIF}
    function GetImageIndex(Field: TField): Integer;
    procedure SetShowGlyphs(Value: Boolean);
    procedure SetRowsHeight(Value: Integer);
    function GetRowsHeight: Integer;
    function GetStorage: TJvFormPlacement;
    procedure SetStorage(Value: TJvFormPlacement);
    procedure IniSave(Sender: TObject);
    procedure IniLoad(Sender: TObject);
    procedure SetMultiSelect(Value: Boolean);
    procedure SetTitleButtons(Value: Boolean);
    procedure StopTracking;
    procedure TrackButton(X, Y: Integer);
    function ActiveRowSelected: Boolean;
    function GetSelCount: Longint;
    procedure InternalSaveLayout(IniFile: TObject; const Section: string);
    procedure InternalRestoreLayout(IniFile: TObject; const Section: string);
{$IFDEF WIN32}
    procedure SaveColumnsLayout(IniFile: TObject; const Section: string);
    procedure RestoreColumnsLayout(IniFile: TObject; const Section: string);
    function GetOptions: TDBGridOptions;
    procedure SetOptions(Value: TDBGridOptions);
    function GetMasterColumn(ACol, ARow: Longint): TColumn;
{$ELSE}
    function GetFixedColor: TColor;
    procedure SetFixedColor(Value: TColor);
    function GetIndicatorOffset: Byte;
{$ENDIF}
    function GetTitleOffset: Byte;
    procedure SetFixedCols(Value: Integer);
    function GetFixedCols: Integer;
{$IFDEF COMPILER4_UP}
    function CalcLeftColumn: Integer;
{$ENDIF}
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
{$IFDEF WIN32}
    procedure WMRButtonUp(var Message: TWMMouse); message WM_RBUTTONUP;
{$ENDIF}
  protected
    function AcquireFocus: Boolean;
    function CanEditShow: Boolean; override;
    function CreateEditor: TInplaceEdit; override;
    procedure DoTitleClick(ACol: Longint; AField: TField); dynamic;
    procedure CheckTitleButton(ACol, ARow: Longint; var Enabled: Boolean); dynamic;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure DrawDataCell(const Rect: TRect; Field: TField;
      State: TGridDrawState); override; { obsolete from Delphi 2.0 }
    procedure EditChanged(Sender: TObject); dynamic;
    procedure GetCellProps(Field: TField; AFont: TFont; var Background: TColor;
      Highlight: Boolean); dynamic;
    function HighlightCell(DataCol, DataRow: Integer; const Value: string;
      AState: TGridDrawState): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure SetColumnAttributes; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
{$IFDEF COMPILER4_UP}
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
{$ENDIF}
    procedure Scroll(Distance: Integer); override;
    procedure LayoutChanged; override;
    procedure TopLeftChanged; override;
{$IFDEF WIN32}
    procedure DrawColumnCell(const Rect: TRect; DataCol: Integer;
      Column: TColumn; State: TGridDrawState); override;
    procedure ColWidthsChanged; override;
{$ELSE}
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    procedure LinkActive(Value: Boolean); override;
{$ENDIF}
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultDataCellDraw(const Rect: TRect; Field: TField;
      State: TGridDrawState);
    procedure DisableScroll;
    procedure EnableScroll;
    function ScrollDisabled: Boolean;
    procedure MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
    procedure SaveLayout(IniFile: TIniFile);
    procedure RestoreLayout(IniFile: TIniFile);
    procedure SelectAll;
    procedure UnselectAll;
    procedure ToggleRowSelection;
    procedure GotoSelection(Index: Longint);
{$IFDEF WIN32}
    procedure SaveLayoutReg(IniFile: TRegIniFile);
    procedure RestoreLayoutReg(IniFile: TRegIniFile);
    property SelectedRows;
{$ELSE}
    property SelectedRows: TBookmarkList read FBookmarks;
{$ENDIF WIN32}
    property SelCount: Longint read GetSelCount;
    property Canvas;
    property Col;
    property InplaceEditor;
    property LeftCol;
    property Row;
    property VisibleRowCount;
    property VisibleColCount;
    property IndicatorOffset {$IFNDEF WIN32}: Byte read GetIndicatorOffset {$ENDIF};
    property TitleOffset: Byte read GetTitleOffset;
  published
{$IFDEF WIN32}
    property Options: TDBGridOptions read GetOptions write SetOptions
      default DefRxGridOptions;
{$ELSE}
    property FixedColor: TColor read GetFixedColor write SetFixedColor
      default clBtnFace; { fix Delphi 1.0 bug }
    property Options default DefRxGridOptions;
{$ENDIF}
    property FixedCols: Integer read GetFixedCols write SetFixedCols default 0;
    property ClearSelection: Boolean read FClearSelection write FClearSelection
      default True;
    property DefaultDrawing: Boolean read FDefaultDrawing write FDefaultDrawing
      default True;
    property IniStorage: TJvFormPlacement read GetStorage write SetStorage;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect
      default False;
    property ShowGlyphs: Boolean read FShowGlyphs write SetShowGlyphs
      default True;
    property TitleButtons: Boolean read FTitleButtons write SetTitleButtons
      default False;
    property RowsHeight: Integer read GetRowsHeight write SetRowsHeight
      stored False; { obsolete, for backward compatibility only }
    property OnCheckButton: TCheckTitleBtnEvent read FOnCheckButton write FOnCheckButton;
    property OnGetCellProps: TGetCellPropsEvent read FOnGetCellProps
      write FOnGetCellProps; { obsolete }
    property OnGetCellParams: TGetCellParamsEvent read FOnGetCellParams write FOnGetCellParams;
    property OnGetBtnParams: TGetBtnParamsEvent read FOnGetBtnParams write FOnGetBtnParams;
    property OnEditChange: TNotifyEvent read FOnEditChange write FOnEditChange;
    property OnShowEditor: TDBEditShowEvent read FOnShowEditor write FOnShowEditor;
    property OnTitleBtnClick: TTitleClickEvent read FOnTitleBtnClick write FOnTitleBtnClick;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnTopLeftChanged: TNotifyEvent read FOnTopLeftChanged write FOnTopLeftChanged;
{$IFNDEF WIN32}
    property OnColumnMoved: TMovedEvent read FOnColumnMoved write FOnColumnMoved;
{$ENDIF}
{$IFDEF COMPILER5_UP}
    property OnContextPopup;
{$ENDIF}
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IFDEF COMPILER4_UP}
    property OnMouseWheelDown;
    property OnMouseWheelUp;
{$ENDIF}
  end;

{ TJvDBComboEdit }

  TJvDBComboEdit = class(TJvCustomComboEdit)
  private
    FDataLink: TFieldDataLink;
{$IFDEF WIN32}
    FCanvas: TControlCanvas;
{$ENDIF}
    FFocused: Boolean;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetFocused(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
{$IFDEF WIN32}
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
{$ENDIF}
  protected
    procedure Change; override;
    function EditCanModify: Boolean; override;
    function GetReadOnly: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Reset; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
{$IFDEF COMPILER4_UP}
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
{$ENDIF}
    property Button;
    property Field: TField read GetField;
  published
    property AutoSelect;
    property BorderStyle;
    property ButtonHint;
    property CharCase;
    property ClickKey;
    property Color;
    property Ctl3D;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DirectInput;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property GlyphKind;
    { Ensure GlyphKind is published before Glyph and ButtonWidth }
    property Glyph;
    property ButtonWidth;
    property HideSelection;
{$IFDEF COMPILER4_UP}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
{$ENDIF}
{$IFDEF WIN32}
  {$IFNDEF VER90}
    property ImeMode;
    property ImeName;
  {$ENDIF}
{$ENDIF}
    property MaxLength;
    property NumGlyphs;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IFDEF WIN32}
    property OnStartDrag;
{$ENDIF}
{$IFDEF COMPILER5_UP}
    property OnContextPopup;
{$ENDIF}
{$IFDEF COMPILER4_UP}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
  end;

{ TJvDBDateEdit }

  TJvDBDateEdit = class(TJvCustomDateEdit)
  private
    FDataLink: TFieldDataLink;
{$IFDEF WIN32}
    FCanvas: TControlCanvas;
{$ENDIF}
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure AfterPopup(Sender: TObject; var Date: TDateTime; var Action: Boolean);
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
{$IFDEF WIN32}
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
{$ENDIF}
  protected
{$IFDEF WIN32}
    procedure AcceptValue(const Value: Variant); override;
{$ENDIF}
    procedure ApplyDate(Value: TDateTime); override;
    function GetReadOnly: Boolean; override;
    procedure Change; override;
    function EditCanModify: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Reset; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateMask; override;
{$IFDEF COMPILER4_UP}
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
{$ENDIF}
    property Field: TField read GetField;
  published
    property CalendarHints;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property AutoSelect;
    property BlanksChar;
    property BorderStyle;
    property ButtonHint;
    property CheckOnExit;
    property ClickKey;
    property Color;
    property Ctl3D;
    property DefaultToday;
    property DialogTitle;
    property DirectInput;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property GlyphKind;
    { Ensure GlyphKind is declared before Glyph and ButtonWidth }
    property Glyph;
    property ButtonWidth;
    property HideSelection;
{$IFDEF COMPILER4_UP}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
{$ENDIF}
{$IFDEF WIN32}
  {$IFNDEF VER90}
    property ImeMode;
    property ImeName;
  {$ENDIF}
{$ENDIF}
    property MaxLength;
    property NumGlyphs;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupAlign;
    property PopupColor;
    property PopupMenu;
    property ShowHint;
    property CalendarStyle;
    property TabOrder;
    property TabStop;
    property StartOfWeek;
    property Weekends;
    property WeekendColor;
    property YearDigits;
    property Visible;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IFDEF WIN32}
    property OnStartDrag;
{$ENDIF}
{$IFDEF COMPILER5_UP}
    property OnContextPopup;
{$ENDIF}
{$IFDEF COMPILER4_UP}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
  end;

{ TJvDBCalcEdit }

  TJvDBCalcEdit = class(TJvCustomCalcEdit)
  private
    FDataLink: TFieldDataLink;
    FDefaultParams: Boolean;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetDefaultParams(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateFieldData(Sender: TObject);
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
{$IFDEF WIN32}
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
{$ENDIF}
  protected
{$IFDEF WIN32}
    procedure AcceptValue(const Value: Variant); override;
    function GetDisplayText: string; override;
{$ENDIF}
    function GetReadOnly: Boolean; override;
    procedure Change; override;
    function EditCanModify: Boolean; override;
    function IsValidChar(Key: Char): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Reset; override;
    procedure UpdatePopup; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateFieldParams;
{$IFDEF COMPILER4_UP}
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
{$ENDIF}
    property Field: TField read GetField;
    property Value;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DefaultParams: Boolean read FDefaultParams write SetDefaultParams default False;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property Alignment;
    property AutoSelect;
    property BeepOnError;
    property BorderStyle;
    property ButtonHint;
    property CheckOnExit;
    property ClickKey;
    property Color;
    property Ctl3D;
    property DecimalPlaces;
    property DirectInput;
    property DisplayFormat;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property FormatOnEditing;
    property GlyphKind;
    { Ensure GlyphKind is declared before Glyph and ButtonWidth }
    property Glyph;
    property ButtonWidth;
    property HideSelection;
{$IFDEF COMPILER4_UP}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
{$ENDIF}
{$IFDEF WIN32}
  {$IFNDEF VER90}
    property ImeMode;
    property ImeName;
  {$ENDIF}
{$ENDIF}
    property MaxLength;
    property MaxValue;
    property MinValue;
    property NumGlyphs;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupAlign;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property ZeroEmpty;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IFDEF WIN32}
    property OnStartDrag;
{$ENDIF}
{$IFDEF COMPILER5_UP}
    property OnContextPopup;
{$ENDIF}
{$IFDEF COMPILER4_UP}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
  end;

{ TJvDBStatusLabel }

  TGetStringEvent = function(Sender: TObject): string of object;
  TDataValueEvent = procedure(Sender: TObject; DataSet: TDataSet;
    var Value: Longint) of object;
  TDBLabelStyle = (lsState, lsRecordNo, lsRecordSize);
  TGlyphAlign = glGlyphLeft..glGlyphRight;
  TDBStatusKind = dsInactive..dsCalcFields;
  TDBLabelOptions = (doCaption, doGlyph, doBoth);

  TJvDBStatusLabel = class(TJvCustomLabel)
  private
    FDataLink: TDataLink;
    FDataSetName: String;
    FStyle: TDBLabelStyle;
    FEditColor: TColor;
    FCalcCount: Boolean;
    FCaptions: TStrings;
    FGlyph: TBitmap;
    FCell: TBitmap;
    FGlyphAlign: TGlyphAlign;
    FRecordCount: Longint;
    FRecordNo: Longint;
    FShowOptions: TDBLabelOptions;
    FOnGetDataName: TGetStringEvent;
    FOnGetRecNo: TDataValueEvent;
    FOnGetRecordCount: TDataValueEvent;
    function GetStatusKind(State: TDataSetState): TDBStatusKind;
    procedure CaptionsChanged(Sender: TObject);
    function GetDataSetName: string;
    procedure SetDataSetName(Value: string);
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    function GetDatasetState: TDataSetState;
    procedure SetEditColor(Value: TColor);
    procedure SetStyle(Value: TDBLabelStyle);
    procedure SetShowOptions(Value: TDBLabelOptions);
    procedure SetGlyphAlign(Value: TGlyphAlign);
    procedure SetCaptions(Value: TStrings);
    procedure SetCalcCount(Value: Boolean);
  protected
    procedure Loaded; override;
    function GetDefaultFontColor: TColor; override;
    function GetLabelCaption: string; override;
    function GetCaption(State: TDataSetState): string; virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Paint; override;
    procedure SetName(const Value: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateData; virtual;
    procedure UpdateStatus; virtual;
    property Caption;
    property DatasetState: TDataSetState read GetDatasetState;
  published
    property DatasetName: string read GetDataSetName write SetDataSetName;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property EditColor: TColor read FEditColor write SetEditColor default clRed;
    property Captions: TStrings read FCaptions write SetCaptions;
    property Style: TDBLabelStyle read FStyle write SetStyle default lsState;
    property CalcRecCount: Boolean read FCalcCount write SetCalcCount default False;
    property ShowOptions: TDBLabelOptions read FShowOptions write SetShowOptions
      default doCaption;
    property GlyphAlign: TGlyphAlign read FGlyphAlign write SetGlyphAlign
      default glGlyphLeft;
    property Layout default tlCenter;
    property ShadowSize default 0;
    property Align;
    property Alignment;
    property AutoSize;
    property Color;
    property DragCursor;
    property DragMode;
    property Font;
{$IFDEF COMPILER4_UP}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
{$ENDIF}
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShadowColor;
    property ShadowPos;
    property ShowHint;
    property Transparent;
    property Visible;
    property WordWrap;
    property OnGetDataName: TGetStringEvent read FOnGetDataName write FOnGetDataName;
    property OnGetRecordCount: TDataValueEvent read FOnGetRecordCount
      write FOnGetRecordCount;
    property OnGetRecNo: TDataValueEvent read FOnGetRecNo write FOnGetRecNo;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
{$IFDEF WIN32}
    property OnStartDrag;
{$ENDIF}
{$IFDEF COMPILER5_UP}
    property OnContextPopup;
{$ENDIF}
{$IFDEF COMPILER4_UP}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
  end;

implementation

uses SysUtils, JvStrUtils, Dialogs, ExtCtrls, DbConsts, JvAppUtils, JvVCLUtils,
  JvDBUtils, {$IFNDEF COMPILER3_UP} JvBdeUtils, {$ENDIF} JvPickDate, JvCalc, JvMaxMin,
  JvDConst;

{$IFDEF WIN32}
  {$R *.Res}
{$ELSE}
  {$R *.R16}
{$ENDIF}

type
  TGridPicture = (gpBlob, gpMemo, gpPicture, gpOle, gpObject, gpData,
    gpNotEmpty, gpMarkDown, gpMarkUp);

const
  GridBmpNames: array[TGridPicture] of PChar =
    ('DBG_BLOB', 'DBG_MEMO', 'DBG_PICT', 'DBG_OLE', 'DBG_OBJECT', 'DBG_DATA',
     'DBG_NOTEMPTY', 'DBG_SMDOWN', 'DBG_SMUP');
  GridBitmaps: array[TGridPicture] of TBitmap =
    (nil, nil, nil, nil, nil, nil, nil, nil, nil);
  bmMultiDot = 'DBG_MSDOT';
  bmMultiArrow = 'DBG_MSARROW';

function GetGridBitmap(BmpType: TGridPicture): TBitmap;
begin
  if GridBitmaps[BmpType] = nil then begin
    GridBitmaps[BmpType] := TBitmap.Create;
    GridBitmaps[BmpType].Handle := LoadBitmap(HInstance, GridBmpNames[BmpType]);
  end;
  Result := GridBitmaps[BmpType];
end;

procedure DestroyLocals; far;
var
  I: TGridPicture;
begin
  for I := Low(TGridPicture) to High(TGridPicture) do GridBitmaps[I].Free;
end;

procedure GridInvalidateRow(Grid: TJvDBGrid; Row: Longint);
var
  I: Longint;
begin
  for I := 0 to Grid.ColCount - 1 do Grid.InvalidateCell(I, Row);
end;

{$IFNDEF WIN32}

{ TBookmarkList }

constructor TBookmarkList.Create(AGrid: TCustomDBGrid);
begin
  inherited Create;
  FList := TJvHugeList.Create;
  FGrid := AGrid;
end;

destructor TBookmarkList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TBookmarkList.Clear;
var
  I: Longint;
begin
  if FList.Count = 0 then Exit;
  for I := FList.Count - 1 downto 0 do StrDispose(FList[I]);
  FList.Clear;
  ListChanged;
  FGrid.Invalidate;
end;

function TBookmarkList.Compare(const Item1, Item2: TBookmark): Longint;
begin
  Result := BookmarksCompare(TJvDBGrid(FGrid).Datalink.Dataset,
    Item1, Item2);
end;

function TBookmarkList.CurrentRow: TBookmark;
begin
  if not FLinkActive then _DBError(sDataSetClosed);
  Result := TJvDBGrid(FGrid).Datalink.Dataset.GetBookmark;
end;

function TBookmarkList.GetCurrentRowSelected: Boolean;
var
  Index: Longint;
  Row: TBookmark;
begin
  Row := CurrentRow;
  try
    Result := Find(Row, Index);
  finally
    StrDispose(Row);
  end;
end;

function TBookmarkList.Find(const Item: TBookmark; var Index: Longint): Boolean;
var
  L, H, I, C: Longint;
  P: PChar;
begin
  if (Compare(Item, FCache) = 0) and (FCacheIndex >= 0) then begin
    Index := FCacheIndex;
    Result := FCacheFind;
    Exit;
  end;
  Result := False;
  L := 0;
  H := FList.Count - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := Compare(TBookmark(FList[I]), Item);
    if C < 0 then L := I + 1
    else begin
      H := I - 1;
      if C = 0 then begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
  StrDispose(FCache);
  FCache := nil;
  P := PChar(Item);
  if P <> nil then begin
    Dec(P, 2);
    FCache := StrAlloc(Word(Pointer(P)^));
    Move(Item^, FCache^, Word(Pointer(P)^));
  end;
  FCacheIndex := Index;
  FCacheFind := Result;
end;

function TBookmarkList.GetCount: Longint;
begin
  Result := FList.Count;
end;

function TBookmarkList.GetItem(Index: Longint): TBookmark;
begin
  Result := TBookmark(FList[Index]);
end;

function TBookmarkList.IndexOf(const Item: TBookmark): Longint;
begin
  if not Find(Item, Result) then Result := -1;
end;

procedure TBookmarkList.LinkActive(Value: Boolean);
begin
  Clear;
  FLinkActive := Value;
end;

procedure TBookmarkList.Delete;
var
  I: Longint;
begin
  with TJvDBGrid(FGrid).Datalink.Dataset do begin
    DisableControls;
    try
      for I := FList.Count - 1 downto 0 do begin
        if FList[I] <> nil then begin
          GotoBookmark(TBookmark(FList[I]));
          Delete;
          StrDispose(FList[I]);
        end;
        FList.Delete(I);
      end;
      ListChanged;
    finally
      EnableControls;
    end;
  end;
end;

function TBookmarkList.Refresh: Boolean;
var
  I: Longint;
begin
  Result := False;
  with TJvDBGrid(FGrid).DataLink.Dataset do
  try
    CheckBrowseMode;
    for I := FList.Count - 1 downto 0 do
      if DbiSetToBookmark(Handle, Pointer(FList[I])) <> 0 then begin
        Result := True;
        StrDispose(FList[I]);
        FList.Delete(I);
      end;
    ListChanged;
  finally
    UpdateCursorPos;
    if Result then FGrid.Invalidate;
  end;
end;

procedure TBookmarkList.SetCurrentRowSelected(Value: Boolean);
var
  Index: Longint;
  Current: TBookmark;
begin
  Current := CurrentRow;
  Index := 0;
  if (Current = nil) or (Find(Current, Index) = Value) then begin
    if Current <> nil then StrDispose(Current);
    Exit;
  end;
  if Value then begin
    try
      FList.Insert(Index, Current);
    except
      StrDispose(Current);
      raise;
    end;
  end
  else begin
    if (Index < FList.Count) and (Index >= 0) then begin
      StrDispose(FList[Index]);
      FList.Delete(Index);
    end;
    StrDispose(Current);
  end;
  ListChanged;
  TJvDBGrid(FGrid).InvalidateRow(TJvDBGrid(FGrid).Row);
  GridInvalidateRow(TJvDBGrid(FGrid), TJvDBGrid(FGrid).Row);
end;

procedure TBookmarkList.ListChanged;
begin
  if FCache <> nil then StrDispose(FCache);
  FCache := nil;
  FCacheIndex := -1;
end;

{$ENDIF WIN32}

type
  TBookmarks = class(TBookmarkList);

{ TJvDBGrid }

constructor TJvDBGrid.Create(AOwner: TComponent);
var
  Bmp: TBitmap;
begin
  inherited Create(AOwner);
  inherited DefaultDrawing := False;
  Options := DefRxGridOptions;
  Bmp := TBitmap.Create;
  try
    Bmp.Handle := LoadBitmap(hInstance, bmMultiDot);
{$IFDEF WIN32}
    FMsIndicators := TImageList.CreateSize(Bmp.Width, Bmp.Height);
{$ELSE}
    FMsIndicators := TImageList.Create(Bmp.Width, Bmp.Height);
    Bmp.Monochrome := False;
{$ENDIF}
    FMsIndicators.AddMasked(Bmp, clWhite);
    Bmp.Handle := LoadBitmap(hInstance, bmMultiArrow);
{$IFNDEF WIN32}
    Bmp.Monochrome := False;
{$ENDIF}
    FMsIndicators.AddMasked(Bmp, clWhite);
  finally
    Bmp.Free;
  end;
  FIniLink := TJvIniLink.Create;
  FIniLink.OnSave := IniSave;
  FIniLink.OnLoad := IniLoad;
  FShowGlyphs := True;
  FDefaultDrawing := True;
  FClearSelection := True;
{$IFNDEF WIN32}
  FBookmarks := TBookmarkList.Create(Self);
  FPressedCol := -1;
{$ENDIF}
end;

destructor TJvDBGrid.Destroy;
begin
  FIniLink.Free;
{$IFNDEF WIN32}
  if FSelectionAnchor <> nil then StrDispose(FSelectionAnchor);
  FSelectionAnchor := nil;
  FBookmarks.Free;
  FBookmarks := nil;
{$ENDIF}
  FMsIndicators.Free;
  inherited Destroy;
end;

function TJvDBGrid.GetImageIndex(Field: TField): Integer;
var
  AOnGetText: TFieldGetTextEvent;
  AOnSetText: TFieldSetTextEvent;
begin
  Result := -1;
  if FShowGlyphs and Assigned(Field) then begin
    if (not ReadOnly) and Field.CanModify then begin
      { Allow editing of memo fields if OnSetText and OnGetText
        events are assigned }
      AOnGetText := Field.OnGetText;
      AOnSetText := Field.OnSetText;
      if Assigned(AOnSetText) and Assigned(AOnGetText) then Exit;
    end;
    case Field.DataType of
      ftBytes, ftVarBytes, ftBlob: Result := Ord(gpBlob);
      ftMemo: Result := Ord(gpMemo);
      ftGraphic: Result := Ord(gpPicture);
{$IFDEF WIN32}
      ftTypedBinary: Result := Ord(gpBlob);
      ftFmtMemo: Result := Ord(gpMemo);
      ftParadoxOle, ftDBaseOle: Result := Ord(gpOle);
{$ENDIF}
{$IFDEF COMPILER3_UP}
      ftCursor: Result := Ord(gpData);
{$ENDIF}
{$IFDEF COMPILER4_UP}
      ftReference, ftDataSet: Result := Ord(gpData);
{$ENDIF}
{$IFDEF COMPILER5_UP}
      ftOraClob: Result := Ord(gpMemo);
      ftOraBlob: Result := Ord(gpBlob);
{$ENDIF}
    end;
  end;
end;

function TJvDBGrid.ActiveRowSelected: Boolean;
var
{$IFDEF WIN32}
  Index: Integer;
{$ELSE}
  Index: Longint;
  Bookmark: TBookmark;
{$ENDIF}
begin
  Result := False;
  if MultiSelect and Datalink.Active then begin
{$IFDEF WIN32}
    Result := SelectedRows.Find(Datalink.DataSet.Bookmark, Index);
{$ELSE}
    Bookmark := Datalink.Dataset.GetBookmark;
    try
      Result := SelectedRows.Find(Bookmark, Index);
    finally
      StrDispose(Bookmark);
    end;
{$ENDIF}
  end;
end;

function TJvDBGrid.HighlightCell(DataCol, DataRow: Integer;
  const Value: string; AState: TGridDrawState): Boolean;
begin
  Result := ActiveRowSelected;
  if not Result then
    Result := inherited HighlightCell(DataCol, DataRow, Value, AState);
end;

procedure TJvDBGrid.ToggleRowSelection;
begin
  if MultiSelect and Datalink.Active then
    with SelectedRows do CurrentRowSelected := not CurrentRowSelected;
end;

function TJvDBGrid.GetSelCount: Longint;
begin
  if MultiSelect and (Datalink <> nil) and Datalink.Active then
    Result := SelectedRows.Count
  else Result := 0;
end;

procedure TJvDBGrid.SelectAll;
var
  ABookmark: TBookmark;
begin
  if MultiSelect and DataLink.Active then begin
    with Datalink.Dataset do begin
      if (BOF and EOF) then Exit;
      DisableControls;
      try
        ABookmark := GetBookmark;
        try
          First;
          while not EOF do begin
            SelectedRows.CurrentRowSelected := True;
            Next;
          end;
        finally
          try
            GotoBookmark(ABookmark);
          except
          end;
          FreeBookmark(ABookmark);
        end;
      finally
        EnableControls;
      end;
    end;
  end;
end;

procedure TJvDBGrid.UnselectAll;
begin
  if MultiSelect then begin
    SelectedRows.Clear;
    FSelecting := False;
  end;
end;

procedure TJvDBGrid.GotoSelection(Index: Longint);
begin
  if MultiSelect and DataLink.Active and (Index < SelectedRows.Count) and
    (Index >= 0) then
    Datalink.DataSet.GotoBookmark(Pointer(SelectedRows[Index]));
end;

{$IFNDEF WIN32}
function TJvDBGrid.GetIndicatorOffset: Byte;
begin
  Result := 0;
  if dgIndicator in Options then Inc(Result);
end;
{$ENDIF WIN32}

procedure TJvDBGrid.LayoutChanged;
var
  ACol: Longint;
begin
  ACol := Col;
  inherited LayoutChanged;
  if Datalink.Active and (FixedCols > 0) then
{$IFDEF COMPILER4_UP}
    Col := Min(Max(CalcLeftColumn, ACol), ColCount - 1);
{$ELSE}
    Col := Min(Max(inherited FixedCols, ACol), ColCount - 1);
{$ENDIF}
end;

{$IFDEF WIN32}
procedure TJvDBGrid.ColWidthsChanged;
var
  ACol: Longint;
begin
  ACol := Col;
  inherited ColWidthsChanged;
  if Datalink.Active and (FixedCols > 0) then
{$IFDEF COMPILER4_UP}
    Col := Min(Max(CalcLeftColumn, ACol), ColCount - 1);
{$ELSE}
    Col := Min(Max(inherited FixedCols, ACol), ColCount - 1);
{$ENDIF}
end;
{$ENDIF}

function TJvDBGrid.CreateEditor: TInplaceEdit;
begin
  Result := inherited CreateEditor;
  TEdit(Result).OnChange := EditChanged;
end;

function TJvDBGrid.GetTitleOffset: Byte;
{$IFDEF COMPILER4_UP}
var
  I, J: Integer;
{$ENDIF}
begin
  Result := 0;
  if dgTitles in Options then begin
    Result := 1;
{$IFDEF COMPILER4_UP}
    if (Datalink <> nil) and (Datalink.Dataset <> nil) and
      Datalink.Dataset.ObjectView then
    begin
      for I := 0 to Columns.Count - 1 do begin
        if Columns[I].Showing then begin
          J := Columns[I].Depth;
          if J >= Result then Result := J + 1;
        end;
      end;
    end;
{$ENDIF}
  end;
end;

procedure TJvDBGrid.SetColumnAttributes;
begin
  inherited SetColumnAttributes;
  SetFixedCols(FFixedCols);
end;

procedure TJvDBGrid.SetFixedCols(Value: Integer);
var
  FixCount, I: Integer;
begin
  FixCount := Max(Value, 0) + IndicatorOffset;
  if DataLink.Active and not (csLoading in ComponentState) and
    (ColCount > IndicatorOffset + 1) then
  begin
    FixCount := Min(FixCount, ColCount - 1);
    inherited FixedCols := FixCount;
    for I := 1 to Min(FixedCols, ColCount - 1) do
      TabStops[I] := False;
  end;
  FFixedCols := FixCount - IndicatorOffset;
end;

function TJvDBGrid.GetFixedCols: Integer;
begin
  if DataLink.Active then Result := inherited FixedCols - IndicatorOffset
  else Result := FFixedCols;
end;

{$IFDEF COMPILER4_UP}
function TJvDBGrid.CalcLeftColumn: Integer;
begin
  Result := FixedCols + IndicatorOffset;
  while (Result < ColCount) and (ColWidths[Result] <= 0) do
    Inc(Result);
end;
{$ENDIF}

procedure TJvDBGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  KeyDownEvent: TKeyEvent;

  procedure ClearSelections;
  begin
    if FMultiSelect then begin
      if FClearSelection then SelectedRows.Clear;
      FSelecting := False;
    end;
  end;

  procedure DoSelection(Select: Boolean; Direction: Integer);
  var
    AddAfter: Boolean;
{$IFNDEF WIN32}
    CurRow: TBookmark;
{$ENDIF}
  begin
    AddAfter := False;
{$IFDEF WIN32}
    BeginUpdate;
    try
{$ENDIF}
      if MultiSelect and DataLink.Active then
        if Select and (ssShift in Shift) then begin
          if not FSelecting then begin
{$IFNDEF WIN32}
            if FSelectionAnchor <> nil then StrDispose(FSelectionAnchor);
{$ENDIF}
            FSelectionAnchor := TBookmarks(SelectedRows).CurrentRow;
            SelectedRows.CurrentRowSelected := True;
            FSelecting := True;
            AddAfter := True;
          end
          else with TBookmarks(SelectedRows) do begin
{$IFDEF WIN32}
            AddAfter := Compare(CurrentRow, FSelectionAnchor) <> -Direction;
{$ELSE}
            CurRow := CurrentRow;
            try
              AddAfter := Compare(CurRow, FSelectionAnchor) <> -Direction;
            finally
              StrDispose(CurRow);
            end;
{$ENDIF}
            if not AddAfter then CurrentRowSelected := False;
          end
        end
        else ClearSelections;
      if Direction <> 0 then Datalink.DataSet.MoveBy(Direction);
      if AddAfter then SelectedRows.CurrentRowSelected := True;
{$IFDEF WIN32}
    finally
      EndUpdate;
    end;
{$ENDIF}
  end;

  procedure NextRow(Select: Boolean);
  begin
    with Datalink.Dataset do begin
      DoSelection(Select, 1);
      if EOF and CanModify and (not ReadOnly) and (dgEditing in Options) then
        Append;
    end;
  end;

  procedure PriorRow(Select: Boolean);
  begin
    DoSelection(Select, -1);
  end;

  procedure CheckTab(GoForward: Boolean);
  var
    ACol, Original: Integer;
  begin
    ACol := Col;
    Original := ACol;
    if MultiSelect and DataLink.Active then
      while True do begin
        if GoForward then Inc(ACol) else Dec(ACol);
        if ACol >= ColCount then begin
          ClearSelections;
          ACol := IndicatorOffset;
        end
        else if ACol < IndicatorOffset then begin
          ClearSelections;
          ACol := ColCount;
        end;
        if ACol = Original then Exit;
        if TabStops[ACol] then Exit;
      end;
  end;

  function DeletePrompt: Boolean;
  var
    S: string;
  begin
    if (SelectedRows.Count > 1) then
{$IFDEF WIN32}
      S := ResStr(SDeleteMultipleRecordsQuestion)
{$ELSE}
      S := LoadStr(SDeleteMultipleRecords)
{$ENDIF}
    else S := ResStr(SDeleteRecordQuestion);
    Result := not (dgConfirmDelete in Options) or
      (MessageDlg(S, mtConfirmation, [mbYes, mbNo], 0) = mrYes);
  end;

begin
  KeyDownEvent := OnKeyDown;
  if Assigned(KeyDownEvent) then KeyDownEvent(Self, Key, Shift);
  if not Datalink.Active or not CanGridAcceptKey(Key, Shift) then Exit;
  with Datalink.DataSet do
    if ssCtrl in Shift then begin
      if (Key in [VK_UP, VK_PRIOR, VK_DOWN, VK_NEXT, VK_HOME, VK_END]) then
        ClearSelections;
      case Key of
        VK_LEFT:
          if FixedCols > 0 then begin
{$IFDEF COMPILER4_UP}
            SelectedIndex := CalcLeftColumn - IndicatorOffset;
{$ELSE}
            SelectedIndex := FixedCols;
{$ENDIF}
            Exit;
          end;
        VK_DELETE:
          if not ReadOnly and CanModify and not
            IsDataSetEmpty(Datalink.DataSet) then
          begin
            if DeletePrompt then begin
              if SelectedRows.Count > 0 then SelectedRows.Delete
              else Delete;
            end;
            Exit;
          end;
      end
    end
    else begin
      case Key of
        VK_LEFT:
          if (FixedCols > 0) and not (dgRowSelect in Options) then begin
{$IFDEF COMPILER4_UP}
            if SelectedIndex <= CalcLeftColumn - IndicatorOffset then
              Exit;
{$ELSE}
            if SelectedIndex <= FFixedCols then Exit;
{$ENDIF}
          end;
        VK_HOME:
          if (FixedCols > 0) and (ColCount <> IndicatorOffset + 1) and
            not (dgRowSelect in Options) then
          begin
{$IFDEF COMPILER4_UP}
            SelectedIndex := CalcLeftColumn - IndicatorOffset;
{$ELSE}
            SelectedIndex := FixedCols;
{$ENDIF}
            Exit;
          end;
      end;
      if (Datalink.DataSet.State = dsBrowse) then begin
        case Key of
          VK_UP:
            begin
              PriorRow(True); Exit;
            end;
          VK_DOWN:
            begin
              NextRow(True); Exit;
            end;
        end;
      end;
      if ((Key in [VK_LEFT, VK_RIGHT]) and (dgRowSelect in Options)) or
        ((Key in [VK_HOME, VK_END]) and ((ColCount = IndicatorOffset + 1)
          or (dgRowSelect in Options))) or (Key in [VK_ESCAPE, VK_NEXT,
          VK_PRIOR]) or ((Key = VK_INSERT) and (CanModify and
          (not ReadOnly) and (dgEditing in Options))) then
        ClearSelections
      else if ((Key = VK_TAB) and not (ssAlt in Shift)) then
        CheckTab(not (ssShift in Shift));
    end;
  OnKeyDown := nil;
  try
    inherited KeyDown(Key, Shift);
  finally
    OnKeyDown := KeyDownEvent;
  end;
end;

procedure TJvDBGrid.SetShowGlyphs(Value: Boolean);
begin
  if FShowGlyphs <> Value then begin
    FShowGlyphs := Value;
    Invalidate;
  end;
end;

procedure TJvDBGrid.SetRowsHeight(Value: Integer);
begin
  if not (csDesigning in ComponentState) and (DefaultRowHeight <> Value) then
  begin
    DefaultRowHeight := Value;
    if dgTitles in Options then RowHeights[0] := Value + 2;
    if HandleAllocated then
      Perform(WM_SIZE, SIZE_RESTORED, MakeLong(ClientWidth, ClientHeight));
  end;
end;

function TJvDBGrid.GetRowsHeight: Integer;
begin
  Result := DefaultRowHeight;
end;

{$IFDEF WIN32}

function TJvDBGrid.GetOptions: TDBGridOptions;
begin
  Result := inherited Options;
  if FMultiSelect then Result := Result + [dgMultiSelect]
  else Result := Result - [dgMultiSelect];
end;

procedure TJvDBGrid.SetOptions(Value: TDBGridOptions);
var
  NewOptions: TGridOptions;
begin
  inherited Options := Value - [dgMultiSelect];
  NewOptions := TDrawGrid(Self).Options;
  {
  if FTitleButtons then begin
    TDrawGrid(Self).Options := NewOptions + [goFixedHorzLine, goFixedVertLine];
  end else
  }
  begin
    if not (dgColLines in Value) then
      NewOptions := NewOptions - [goFixedVertLine];
    if not (dgRowLines in Value) then
      NewOptions := NewOptions - [goFixedHorzLine];
    TDrawGrid(Self).Options := NewOptions;
  end;
  SetMultiSelect(dgMultiSelect in Value);
end;

{$ELSE}

procedure TJvDBGrid.LinkActive(Value: Boolean);
begin
  SelectedRows.LinkActive(Value);
  inherited LinkActive(Value);
end;

function TJvDBGrid.GetFixedColor: TColor;
begin
  Result := inherited TitleColor;
end;

procedure TJvDBGrid.SetFixedColor(Value: TColor);
begin
  if FixedColor <> Value then begin
    inherited TitleColor := Value;
    inherited FixedColor := Value;
    Invalidate;
  end;
end;

procedure TJvDBGrid.ColumnMoved(FromIndex, ToIndex: Longint);
begin
  inherited ColumnMoved(FromIndex, ToIndex);
  if Assigned(FOnColumnMoved) then FOnColumnMoved(Self, FromIndex, ToIndex);
end;

{$ENDIF WIN32}

procedure TJvDBGrid.Paint;
begin
  inherited Paint;
  if not (csDesigning in ComponentState) and
    (dgRowSelect in Options) and DefaultDrawing and Focused then
  begin
    Canvas.Font.Color := clWindowText;
    with Selection do
      DrawFocusRect(Canvas.Handle, BoxRect(Left, Top, Right, Bottom));
  end;
end;

procedure TJvDBGrid.SetTitleButtons(Value: Boolean);
begin
  if FTitleButtons <> Value then begin
    FTitleButtons := Value;
    Invalidate;
{$IFDEF WIN32}
    SetOptions(Options);
{$ENDIF}
  end;
end;

procedure TJvDBGrid.SetMultiSelect(Value: Boolean);
begin
  if FMultiSelect <> Value then begin
    FMultiSelect := Value;
    if not Value then SelectedRows.Clear;
  end;
end;

function TJvDBGrid.GetStorage: TJvFormPlacement;
begin
  Result := FIniLink.Storage;
end;

procedure TJvDBGrid.SetStorage(Value: TJvFormPlacement);
begin
  FIniLink.Storage := Value;
end;

function TJvDBGrid.AcquireFocus: Boolean;
begin
  Result := True;
  if FAcquireFocus and CanFocus and not (csDesigning in ComponentState) then
  begin
    SetFocus;
    Result := Focused or (InplaceEditor <> nil) and InplaceEditor.Focused;
  end;
end;

function TJvDBGrid.CanEditShow: Boolean;
var
  F: TField;
begin
  Result := inherited CanEditShow;
  F := nil;
  if Result and (Datalink <> nil) and Datalink.Active and (FieldCount > 0) and
    (SelectedIndex < FieldCount) and (SelectedIndex >= 0) and
    (FieldCount <= DataSource.DataSet.FieldCount) then
  begin
    F := Fields[SelectedIndex];
    if F <> nil then Result := GetImageIndex(F) < 0;
  end;
  if Result and Assigned(FOnShowEditor) then
    FOnShowEditor(Self, F, Result);
end;

procedure TJvDBGrid.GetCellProps(Field: TField; AFont: TFont;
  var Background: TColor; Highlight: Boolean);
var
  AColor, ABack: TColor;
begin
  if Assigned(FOnGetCellParams) then
    FOnGetCellParams(Self, Field, AFont, Background, Highlight)
  else if Assigned(FOnGetCellProps) then begin
    if Highlight then begin
      AColor := AFont.Color;
      FOnGetCellProps(Self, Field, AFont, ABack);
      AFont.Color := AColor;
    end
    else FOnGetCellProps(Self, Field, AFont, Background);
  end;
end;

procedure TJvDBGrid.DoTitleClick(ACol: Longint; AField: TField);
begin
  if Assigned(FOnTitleBtnClick) then FOnTitleBtnClick(Self, ACol, AField);
end;

procedure TJvDBGrid.CheckTitleButton(ACol, ARow: Longint; var Enabled: Boolean);
var
  Field: TField;
begin
  if (ACol >= 0) and (ACol < {$IFDEF WIN32} Columns.Count {$ELSE}
    FieldCount {$ENDIF}) then
  begin
    if Assigned(FOnCheckButton) then begin
{$IFDEF WIN32}
      Field := Columns[ACol].Field;
  {$IFDEF COMPILER4_UP}
      if ColumnAtDepth(Columns[ACol], ARow) <> nil then
        Field := ColumnAtDepth(Columns[ACol], ARow).Field;
  {$ENDIF}
{$ELSE}
      Field := Fields[ACol];
{$ENDIF}
      FOnCheckButton(Self, ACol, Field, Enabled);
    end;
  end
  else Enabled := False;
end;

procedure TJvDBGrid.DisableScroll;
begin
  Inc(FDisableCount);
end;

type
  THackLink = class(TGridDataLink);

procedure TJvDBGrid.EnableScroll;
begin
  if FDisableCount <> 0 then begin
    Dec(FDisableCount);
    if FDisableCount = 0 then
      THackLink(DataLink).DataSetScrolled(0);
  end;
end;

function TJvDBGrid.ScrollDisabled: Boolean;
begin
  Result := FDisableCount <> 0;
end;

procedure TJvDBGrid.Scroll(Distance: Integer);
{$IFNDEF COMPILER3_UP}
var
  IndicatorRect: TRect;
{$ENDIF}
begin
  if FDisableCount = 0 then begin
    inherited Scroll(Distance);
{$IFNDEF COMPILER3_UP}
    if (dgIndicator in Options) and HandleAllocated and MultiSelect then
    begin
      IndicatorRect := BoxRect(0, 0, 0, RowCount - 1);
      InvalidateRect(Handle, @IndicatorRect, False);
    end;
{$ENDIF}
  end;
end;

{$IFDEF COMPILER4_UP}

function TJvDBGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(OnMouseWheelDown) then
    OnMouseWheelDown(Self, Shift, MousePos, Result);
  if not Result then begin
    if not AcquireFocus then Exit;
    if Datalink.Active then begin
      Result := Datalink.DataSet.MoveBy(1) <> 0;
    end;
  end;
end;

function TJvDBGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(OnMouseWheelUp) then
    OnMouseWheelUp(Self, Shift, MousePos, Result);
  if not Result then begin
    if not AcquireFocus then Exit;
    if Datalink.Active then begin
      Result := Datalink.DataSet.MoveBy(-1) <> 0;
    end;
  end;
end;

{$ENDIF COMPILER4_UP}

procedure TJvDBGrid.EditChanged(Sender: TObject);
begin
  if Assigned(FOnEditChange) then FOnEditChange(Self);
end;

procedure TJvDBGrid.TopLeftChanged;
begin
  if (dgRowSelect in Options) and DefaultDrawing then
    GridInvalidateRow(Self, Self.Row);
  inherited TopLeftChanged;
  if FTracking then StopTracking;
  if Assigned(FOnTopLeftChanged) then FOnTopLeftChanged(Self);
end;

procedure TJvDBGrid.StopTracking;
begin
  if FTracking then begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TJvDBGrid.TrackButton(X, Y: Integer);
var
  Cell: TGridCoord;
  NewPressed: Boolean;
  I, Offset: Integer;
begin
  Cell := MouseCoord(X, Y);
  Offset := TitleOffset;
  NewPressed := PtInRect(Rect(0, 0, ClientWidth, ClientHeight), Point(X, Y)) and
    (FPressedCol = {$IFDEF WIN32} GetMasterColumn(Cell.X, Cell.Y) {$ELSE}
    Cell.X {$ENDIF}) and (Cell.Y < Offset);
  if FPressed <> NewPressed then begin
    FPressed := NewPressed;
    for I := 0 to Offset - 1 do
      GridInvalidateRow(Self, I);
  end;
end;

procedure TJvDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Cell: TGridCoord;
  MouseDownEvent: TMouseEvent;
  EnableClick: Boolean;
begin
  if not AcquireFocus then Exit;
  if (ssDouble in Shift) and (Button = mbLeft) then begin
    DblClick;
    Exit;
  end;
  if Sizing(X, Y) then
    inherited MouseDown(Button, Shift, X, Y)
  else begin
    Cell := MouseCoord(X, Y);
{$IFDEF COMPILER4_UP}
    if (DragKind = dkDock) and (Cell.X < IndicatorOffset) and
      (Cell.Y < TitleOffset) and (not (csDesigning in ComponentState)) then
    begin
      BeginDrag(False);
      Exit;
    end;
{$ENDIF}
    if FTitleButtons and (Datalink <> nil) and Datalink.Active and
      (Cell.Y < TitleOffset) and (Cell.X >= IndicatorOffset) and
      not (csDesigning in ComponentState) then
    begin
      if (dgColumnResize in Options) and (Button = mbRight) then begin
        Button := mbLeft;
        FSwapButtons := True;
        MouseCapture := True;
      end
      else if Button = mbLeft then begin
        EnableClick := True;
        CheckTitleButton(Cell.X - IndicatorOffset, Cell.Y, EnableClick);
        if EnableClick then begin
          MouseCapture := True;
          FTracking := True;
{$IFDEF WIN32}
          FPressedCol := GetMasterColumn(Cell.X, Cell.Y);
{$ELSE}
          FPressedCol := Cell.X;
{$ENDIF}
          TrackButton(X, Y);
        end else Beep;
        Exit;
      end;
    end;
    if (Cell.X < FixedCols + IndicatorOffset) and Datalink.Active then begin
      if (dgIndicator in Options) then
        inherited MouseDown(Button, Shift, 1, Y)
      else if Cell.Y >= TitleOffset then
        if Cell.Y - Row <> 0 then Datalink.Dataset.MoveBy(Cell.Y - Row);
    end
    else inherited MouseDown(Button, Shift, X, Y);
    MouseDownEvent := OnMouseDown;
    if Assigned(MouseDownEvent) then MouseDownEvent(Self, Button, Shift, X, Y);
    if not (((csDesigning in ComponentState) or (dgColumnResize in Options)) and
      (Cell.Y < TitleOffset)) and (Button = mbLeft) then
    begin
      if MultiSelect and Datalink.Active then
        with SelectedRows do begin
          FSelecting := False;
          if ssCtrl in Shift then
            CurrentRowSelected := not CurrentRowSelected
          else begin
            Clear;
            if FClearSelection then CurrentRowSelected := True;
          end;
        end;
    end;
  end;
end;

procedure TJvDBGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FTracking then TrackButton(X, Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TJvDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Cell: TGridCoord;
  ACol: Longint;
  DoClick: Boolean;
begin
  if FTracking and {$IFDEF WIN32} (FPressedCol <> nil) {$ELSE}
    (FPressedCol >= 0) {$ENDIF} then
  begin
    Cell := MouseCoord(X, Y);
    DoClick := PtInRect(Rect(0, 0, ClientWidth, ClientHeight), Point(X, Y))
      and (Cell.Y < TitleOffset) and
{$IFDEF WIN32}
      (FPressedCol = GetMasterColumn(Cell.X, Cell.Y));
{$ELSE}
      (Cell.X = FPressedCol);
{$ENDIF}
    StopTracking;
    if DoClick then begin
      ACol := Cell.X;
      if (dgIndicator in Options) then Dec(ACol);
      if (DataLink <> nil) and DataLink.Active and (ACol >= 0) and
        (ACol < {$IFDEF WIN32} Columns.Count {$ELSE} FieldCount {$ENDIF}) then
      begin
{$IFDEF WIN32}
        DoTitleClick(FPressedCol.Index, FPressedCol.Field);
{$ELSE}
        DoTitleClick(ACol, Fields[ACol]);
{$ENDIF}
      end;
    end;
  end
  else if FSwapButtons then begin
    FSwapButtons := False;
    MouseCapture := False;
    if Button = mbRight then Button := mbLeft;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

{$IFDEF WIN32}
procedure TJvDBGrid.WMRButtonUp(var Message: TWMMouse);
begin
  if not (FGridState in [gsColMoving, gsRowMoving]) then
    inherited
  else if not (csNoStdEvents in ControlStyle) then
    with Message do MouseUp(mbRight, KeysToShiftState(Keys), XPos, YPos);
end;
{$ENDIF}

procedure TJvDBGrid.WMCancelMode(var Message: TMessage);
begin
  StopTracking;
  inherited;
end;

type
  TJvHack = class(TWinControl);

procedure TJvDBGrid.WMChar(var Msg: TWMChar);

  function DoKeyPress(var Msg: TWMChar): Boolean;
  var
    Form: TCustomForm;
    Ch: Char;
  begin
    Result := True;
    Form := GetParentForm(Self);
    if (Form <> nil) and TForm(Form).KeyPreview and
      TJvHack(Form).DoKeyPress(Msg) then Exit;
    with Msg do begin
      if Assigned(FOnKeyPress) then begin
        Ch := Char(CharCode);
        FOnKeyPress(Self, Ch);
        CharCode := Word(Ch);
      end;
      if Char(CharCode) = #0 then Exit;
    end;
    Result := False;
  end;

begin
  if EditorMode or not DoKeyPress(Msg) then inherited;
end;

procedure TJvDBGrid.KeyPress(var Key: Char);
begin
  if EditorMode then inherited OnKeyPress := FOnKeyPress;
  try
    inherited KeyPress(Key);
  finally
    inherited OnKeyPress := nil;
  end;
end;

procedure TJvDBGrid.DefaultDataCellDraw(const Rect: TRect; Field: TField;
  State: TGridDrawState);
begin
  DefaultDrawDataCell(Rect, Field, State);
end;

{$IFDEF WIN32}
function TJvDBGrid.GetMasterColumn(ACol, ARow: Longint): TColumn;
begin
  if (dgIndicator in Options) then Dec(ACol, IndicatorOffset);
  if (Datalink <> nil) and Datalink.Active and (ACol >= 0) and
    (ACol < Columns.Count) then
  begin
    Result := Columns[ACol];
{$IFDEF COMPILER4_UP}
    Result := ColumnAtDepth(Result, ARow);
{$ENDIF}
  end
  else Result := nil;
end;
{$ENDIF}

procedure TJvDBGrid.DrawCell(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);

{$IFDEF COMPILER4_UP}
  function CalcTitleRect(Col: TColumn; ARow: Integer; var MasterCol: TColumn): TRect;
    { copied from Inprise's DbGrids.pas }
  var
    I,J: Integer;
    InBiDiMode: Boolean;
    DrawInfo: TGridDrawInfo;
  begin
    MasterCol := ColumnAtDepth(Col, ARow);
    if MasterCol = nil then Exit;
    I := DataToRawColumn(MasterCol.Index);
    if I >= LeftCol then J := MasterCol.Depth
    else begin
      if (FixedCols > 0) and (MasterCol.Index < FixedCols) then begin
        J := MasterCol.Depth;
      end
      else begin
        I := LeftCol;
        if Col.Depth > ARow then J := ARow
        else J := Col.Depth;
      end;
    end;
    Result := CellRect(I, J);
    InBiDiMode := UseRightToLeftAlignment and (Canvas.CanvasOrientation = coLeftToRight);
    for I := Col.Index to Columns.Count - 1 do begin
      if ColumnAtDepth(Columns[I], ARow) <> MasterCol then Break;
      if not InBiDiMode then begin
        J := CellRect(DataToRawColumn(I), ARow).Right;
        if J = 0 then Break;
        Result.Right := Max(Result.Right, J);
      end
      else begin
        J := CellRect(DataToRawColumn(I), ARow).Left;
        if J >= ClientWidth then Break;
        Result.Left := J;
      end;
    end;
    J := Col.Depth;
    if (J <= ARow) and (J < FixedRows - 1) then begin
      CalcFixedInfo(DrawInfo);
      Result.Bottom := DrawInfo.Vert.FixedBoundary -
        DrawInfo.Vert.EffectiveLineWidth;
    end;
  end;

  procedure DrawExpandBtn(var TitleRect, TextRect: TRect; InBiDiMode: Boolean;
    Expanded: Boolean); { copied from Inprise's DbGrids.pas }
  const
    ScrollArrows: array [Boolean, Boolean] of Integer =
      ((DFCS_SCROLLRIGHT, DFCS_SCROLLLEFT), (DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT));
  var
    ButtonRect: TRect;
    I: Integer;
  begin
    I := GetSystemMetrics(SM_CXHSCROLL);
    if ((TextRect.Right - TextRect.Left) > I) then begin
      Dec(TextRect.Right, I);
      ButtonRect := TitleRect;
      ButtonRect.Left := TextRect.Right;
      I := SaveDC(Canvas.Handle);
      try
        Canvas.FillRect(ButtonRect);
        InflateRect(ButtonRect, -1, -1);
        with ButtonRect do
          IntersectClipRect(Canvas.Handle, Left, Top, Right, Bottom);
        InflateRect(ButtonRect, 1, 1);
        { DrawFrameControl doesn't draw properly when orienatation has changed.
          It draws as ExtTextOut does. }
        if InBiDiMode then { stretch the arrows box }
          Inc(ButtonRect.Right, GetSystemMetrics(SM_CXHSCROLL) + 4);
        DrawFrameControl(Canvas.Handle, ButtonRect, DFC_SCROLL,
          ScrollArrows[InBiDiMode, Expanded] or DFCS_FLAT);
      finally
        RestoreDC(Canvas.Handle, I);
      end;
      TitleRect.Right := ButtonRect.Left;
    end;
  end;
{$ENDIF COMPILER4_UP}

var
  FrameOffs: Byte;
  BackColor: TColor;
  SortMarker: TSortMarker;
  Indicator, ALeft: Integer;
  Down: Boolean;
  Bmp: TBitmap;
  SavePen: TColor;
  OldActive: Longint;
  MultiSelected: Boolean;
  FixRect: TRect;
  TitleRect, TextRect: TRect;
  AField: TField;
{$IFDEF COMPILER4_UP}
  MasterCol: TColumn;
  InBiDiMode: Boolean;
{$ENDIF}
{$IFDEF WIN32}
  DrawColumn: TColumn;
const
  EdgeFlag: array[Boolean] of UINT = (BDR_RAISEDINNER, BDR_SUNKENINNER);
{$ENDIF}
begin
  inherited DrawCell(ACol, ARow, ARect, AState);
{$IFDEF COMPILER4_UP}
  InBiDiMode := Canvas.CanvasOrientation = coRightToLeft;
{$ENDIF}
  if (dgIndicator in Options) and (ACol = 0) and (ARow - TitleOffset >= 0)
    and MultiSelect and (DataLink <> nil) and DataLink.Active and
    (Datalink.DataSet.State = dsBrowse) then
  begin { draw multiselect indicators if needed }
    FixRect := ARect;
    if ([dgRowLines, dgColLines] * Options = [dgRowLines, dgColLines]) then
    begin
      InflateRect(FixRect, -1, -1);
      FrameOffs := 1;
    end
    else FrameOffs := 2;
    OldActive := DataLink.ActiveRecord;
    try
      Datalink.ActiveRecord := ARow - TitleOffset;
      MultiSelected := ActiveRowSelected;
    finally
      Datalink.ActiveRecord := OldActive;
    end;
    if MultiSelected then begin
      if (ARow - TitleOffset <> Datalink.ActiveRecord) then Indicator := 0
      else Indicator := 1;  { multiselected and current row }
{$IFDEF WIN32}
      FMsIndicators.BkColor := FixedColor;
{$ELSE}
      Canvas.Brush.Color := TitleColor;
      Canvas.FillRect(FixRect);
{$ENDIF}
      ALeft := FixRect.Right - FMsIndicators.Width - FrameOffs;
{$IFDEF COMPILER4_UP}
      if InBiDiMode then Inc(ALeft);
{$ENDIF}
      FMsIndicators.Draw(Self.Canvas, ALeft, (FixRect.Top +
        FixRect.Bottom - FMsIndicators.Height) shr 1, Indicator);
    end;
  end
  else if not (csLoading in ComponentState) and
    (FTitleButtons {$IFDEF COMPILER4_UP} or (FixedCols > 0) {$ENDIF}) and
    (gdFixed in AState) and (dgTitles in Options) and (ARow < TitleOffset) then
  begin
    SavePen := Canvas.Pen.Color;
    try
      Canvas.Pen.Color := clWindowFrame;
      if (dgIndicator in Options) then Dec(ACol, IndicatorOffset);
      AField := nil;
      SortMarker := smNone;
{$IFDEF WIN32}
      if (Datalink <> nil) and Datalink.Active and (ACol >= 0) and
        (ACol < Columns.Count) then
      begin
        DrawColumn := Columns[ACol];
        AField := DrawColumn.Field;
      end
      else DrawColumn := nil;
{$IFDEF COMPILER4_UP}
      if Assigned(DrawColumn) and not DrawColumn.Showing then Exit;
      TitleRect := CalcTitleRect(DrawColumn, ARow, MasterCol);
      if TitleRect.Right < ARect.Right then
        TitleRect.Right := ARect.Right;
      if MasterCol = nil then
        Exit
      else if MasterCol <> DrawColumn then
        AField := MasterCol.Field;
      DrawColumn := MasterCol;
      if ((dgColLines in Options) or FTitleButtons) and (ACol = FixedCols - 1) then
      begin
        if (ACol < Columns.Count - 1) and not (Columns[ACol + 1].Showing) then
        begin
          Canvas.MoveTo(TitleRect.Right, TitleRect.Top);
          Canvas.LineTo(TitleRect.Right, TitleRect.Bottom);
        end;
      end;
      if ((dgRowLines in Options) or FTitleButtons) and not MasterCol.Showing then
      begin
        Canvas.MoveTo(TitleRect.Left, TitleRect.Bottom);
        Canvas.LineTo(TitleRect.Right, TitleRect.Bottom);
      end;
{$ELSE}
      TitleRect := ARect;
{$ENDIF COMPILER4_UP}
      Down := FPressed and FTitleButtons and (FPressedCol = DrawColumn);
      if FTitleButtons or ([dgRowLines, dgColLines] * Options =
        [dgRowLines, dgColLines]) then
      begin
        DrawEdge(Canvas.Handle, TitleRect, EdgeFlag[Down], BF_BOTTOMRIGHT);
        DrawEdge(Canvas.Handle, TitleRect, EdgeFlag[Down], BF_TOPLEFT);
        InflateRect(TitleRect, -1, -1);
      end;
      Canvas.Font := TitleFont;
      Canvas.Brush.Color := FixedColor;
      if (DrawColumn <> nil) then begin
        Canvas.Font := DrawColumn.Title.Font;
        Canvas.Brush.Color := DrawColumn.Title.Color;
      end;
      if FTitleButtons and (AField <> nil) and Assigned(FOnGetBtnParams) then
      begin
        BackColor := Canvas.Brush.Color;
        FOnGetBtnParams(Self, AField, Canvas.Font, BackColor, SortMarker, Down);
        Canvas.Brush.Color := BackColor;
      end;
      if Down then begin
        Inc(TitleRect.Left); Inc(TitleRect.Top);
      end;
      ARect := TitleRect;
      if (DataLink = nil) or not DataLink.Active then
        Canvas.FillRect(TitleRect)
      else if (DrawColumn <> nil) then begin
        case SortMarker of
          smDown: Bmp := GetGridBitmap(gpMarkDown);
          smUp: Bmp := GetGridBitmap(gpMarkUp);
          else Bmp := nil;
        end;
        if Bmp <> nil then Indicator := Bmp.Width + 6
        else Indicator := 1;
        TextRect := TitleRect;
{$IFDEF COMPILER4_UP}
        if DrawColumn.Expandable then
          DrawExpandBtn(TitleRect, TextRect, InBiDiMode, DrawColumn.Expanded);
{$ENDIF}
        with DrawColumn.Title do
          DrawCellText(Self, ACol, ARow, MinimizeText(Caption, Canvas,
            WidthOf(TextRect) - Indicator), TextRect, Alignment, vaCenter
            {$IFDEF COMPILER4_UP}, IsRightToLeft {$ENDIF});
        if Bmp <> nil then begin
          ALeft := TitleRect.Right - Bmp.Width - 3;
          if Down then Inc(ALeft);
{$IFDEF COMPILER4_UP}
          if IsRightToLeft then ALeft := TitleRect.Left + 3;
{$ENDIF}
          if (ALeft > TitleRect.Left) and (ALeft + Bmp.Width < TitleRect.Right) then
            DrawBitmapTransparent(Canvas, ALeft, (TitleRect.Bottom +
              TitleRect.Top - Bmp.Height) div 2, Bmp, clFuchsia);
        end;
      end
{$ELSE WIN32}
      if not (dgColLines in Options) then begin
        Canvas.MoveTo(ARect.Right - 1, ARect.Top);
        Canvas.LineTo(ARect.Right - 1, ARect.Bottom);
        Dec(ARect.Right);
      end;
      if not (dgRowLines in Options) then begin
        Canvas.MoveTo(ARect.Left, ARect.Bottom - 1);
        Canvas.LineTo(ARect.Right, ARect.Bottom - 1);
        Dec(ARect.Bottom);
      end;
      Down := FPressed and FTitleButtons and (FPressedCol = ACol);
      if (Datalink <> nil) and Datalink.Active and (ACol >= 0) and
        (ACol < FieldCount) then
      begin
        AField := Fields[ACol];
      end;
      if Down then begin
        with ARect do begin
          Canvas.Pen.Color := clBtnShadow;
          Canvas.PolyLine([Point(Left, Bottom - 1), Point(Left, Top),
            Point(Right, Top)]);
          Inc(Left, 2); Inc(Top, 2);
        end;
      end
      else Frame3D(Canvas, ARect, clBtnHighlight, clBtnShadow, 1);
      Canvas.Font := TitleFont;
      Canvas.Brush.Color := TitleColor;
      if FTitleButtons and (AField <> nil) and Assigned(FOnGetBtnParams) then
      begin
        BackColor := Canvas.Brush.Color;
        FOnGetBtnParams(Self, AField, Canvas.Font, BackColor, SortMarker, Down);
        Canvas.Brush.Color := BackColor;
      end;
      if (DataLink = nil) or not DataLink.Active then
        Canvas.FillRect(ARect)
      else if (AField <> nil) then begin
        case SortMarker of
          smDown: Bmp := GetGridBitmap(gpMarkDown);
          smUp: Bmp := GetGridBitmap(gpMarkUp);
          else Bmp := nil;
        end;
        if Bmp <> nil then Indicator := Bmp.Width + 8
        else Indicator := 1;
        DrawCellText(Self, ACol, ARow, MinimizeText(AField.DisplayLabel,
          Canvas, WidthOf(ARect) - Indicator), ARect, taLeftJustify, vaCenter);
        if Bmp <> nil then begin
          ALeft := ARect.Right - Bmp.Width - 4;
          if Down then Inc(ALeft);
          DrawBitmapTransparent(Canvas, ALeft,
            (ARect.Bottom + ARect.Top - Bmp.Height) div 2, Bmp, clFuchsia);
        end;
      end
{$ENDIF WIN32}
      else DrawCellText(Self, ACol, ARow, '', ARect, taLeftJustify, vaCenter);
    finally
      Canvas.Pen.Color := SavePen;
    end;
  end
  else begin
{$IFDEF COMPILER4_UP}
    Canvas.Font := Self.Font;
    if (DataLink <> nil) and DataLink.Active and (ACol >= 0) and
      (ACol < Columns.Count) then
    begin
      DrawColumn := Columns[ACol];
      if DrawColumn <> nil then Canvas.Font := DrawColumn.Font;
    end;
{$ENDIF}
  end;
end;

{$IFDEF WIN32}
procedure TJvDBGrid.DrawColumnCell(const Rect: TRect; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
{$ELSE}
procedure TJvDBGrid.DrawDataCell(const Rect: TRect; Field: TField;
  State: TGridDrawState);
{$ENDIF}
var
  I: Integer;
  NewBackgrnd: TColor;
  Highlight: Boolean;
  Bmp: TBitmap;
{$IFDEF WIN32}
  Field: TField;
{$ENDIF}
begin
{$IFDEF WIN32}
  Field := Column.Field;
{$ENDIF}
  NewBackgrnd := Canvas.Brush.Color;
  Highlight := (gdSelected in State) and ((dgAlwaysShowSelection in Options) or
    Focused);
  GetCellProps(Field, Canvas.Font, NewBackgrnd, Highlight or ActiveRowSelected);
  Canvas.Brush.Color := NewBackgrnd;
  if FDefaultDrawing then begin
    I := GetImageIndex(Field);
    if I >= 0 then begin
      Bmp := GetGridBitmap(TGridPicture(I));
      Canvas.FillRect(Rect);
      DrawBitmapTransparent(Canvas, (Rect.Left + Rect.Right - Bmp.Width) div 2,
        (Rect.Top + Rect.Bottom - Bmp.Height) div 2, Bmp, clOlive);
    end else
{$IFDEF WIN32}
    DefaultDrawColumnCell(Rect, DataCol, Column, State);
{$ELSE}
    DefaultDrawDataCell(Rect, Field, State);
{$ENDIF}
  end;
{$IFDEF WIN32}
  if Columns.State = csDefault then
    inherited DrawDataCell(Rect, Field, State);
  inherited DrawColumnCell(Rect, DataCol, Column, State);
{$ELSE}
  inherited DrawDataCell(Rect, Field, State);
{$ENDIF}
  if FDefaultDrawing and Highlight and not (csDesigning in ComponentState)
    and not (dgRowSelect in Options)
    and (ValidParentForm(Self).ActiveControl = Self) then
    Canvas.DrawFocusRect(Rect);
end;

{$IFDEF WIN32}
procedure TJvDBGrid.DrawDataCell(const Rect: TRect; Field: TField;
  State: TGridDrawState);
begin
end;
{$ENDIF}

procedure TJvDBGrid.MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
var
  Coord: TGridCoord;
begin
  Coord := MouseCoord(X, Y);
  ACol := Coord.X;
  ARow := Coord.Y;
end;

{$IFDEF WIN32}

procedure TJvDBGrid.SaveColumnsLayout(IniFile: TObject;
  const Section: string);
var
  I: Integer;
  S: string;
begin
  if Section <> '' then S := Section
  else S := GetDefaultSection(Self);
  IniEraseSection(IniFile, S);
  with Columns do begin
    for I := 0 to Count - 1 do begin
      IniWriteString(IniFile, S, Format('%s.%s', [Name, Items[I].FieldName]),
        Format('%d,%d', [Items[I].Index, Items[I].Width]));
    end;
  end;
end;

procedure TJvDBGrid.RestoreColumnsLayout(IniFile: TObject;
  const Section: string);
type
  TColumnInfo = record
    Column: TColumn;
    EndIndex: Integer;
  end;
  PColumnArray = ^TColumnArray;
  TColumnArray = array[0..0] of TColumnInfo;
const
  Delims = [' ',','];
var
  I, J: Integer;
  SectionName, S: string;
  ColumnArray: PColumnArray;
begin
  if Section <> '' then SectionName := Section
  else SectionName := GetDefaultSection(Self);
  with Columns do begin
    ColumnArray := AllocMemo(Count * SizeOf(TColumnInfo));
    try
      for I := 0 to Count - 1 do begin
        S := IniReadString(IniFile, SectionName,
          Format('%s.%s', [Name, Items[I].FieldName]), '');
        ColumnArray^[I].Column := Items[I];
        ColumnArray^[I].EndIndex := Items[I].Index;
        if S <> '' then begin
          ColumnArray^[I].EndIndex := StrToIntDef(ExtractWord(1, S, Delims),
            ColumnArray^[I].EndIndex);
          Items[I].Width := StrToIntDef(ExtractWord(2, S, Delims),
            Items[I].Width);
        end;
      end;
      for I := 0 to Count - 1 do begin
        for J := 0 to Count - 1 do begin
          if ColumnArray^[J].EndIndex = I then begin
            ColumnArray^[J].Column.Index := ColumnArray^[J].EndIndex;
            Break;
          end;
        end;
      end;
    finally
      FreeMemo(Pointer(ColumnArray));
    end;
  end;
end;

procedure TJvDBGrid.SaveLayoutReg(IniFile: TRegIniFile);
begin
  InternalSaveLayout(IniFile, '');
end;

procedure TJvDBGrid.RestoreLayoutReg(IniFile: TRegIniFile);
begin
  InternalRestoreLayout(IniFile, '');
end;

{$ENDIF WIN32}

procedure TJvDBGrid.InternalSaveLayout(IniFile: TObject;
  const Section: string);
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
{$IFDEF WIN32}
    if StoreColumns then SaveColumnsLayout(IniFile, Section) else
{$ENDIF}
    InternalSaveFields(DataSource.DataSet, IniFile, Section);
end;

procedure TJvDBGrid.InternalRestoreLayout(IniFile: TObject;
  const Section: string);
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then begin
    HandleNeeded;
{$IFDEF WIN32}
    BeginLayout;
    try
      if StoreColumns then RestoreColumnsLayout(IniFile, Section) else
{$ENDIF}
      InternalRestoreFields(DataSource.DataSet, IniFile, Section, False);
{$IFDEF WIN32}
    finally
      EndLayout;
    end;
{$ENDIF}
  end;
end;

procedure TJvDBGrid.SaveLayout(IniFile: TIniFile);
begin
  InternalSaveLayout(IniFile, '');
end;

procedure TJvDBGrid.RestoreLayout(IniFile: TIniFile);
begin
  InternalRestoreLayout(IniFile, '');
end;

procedure TJvDBGrid.IniSave(Sender: TObject);
var
  Section: string;
begin
  if (Name <> '') and (FIniLink.IniObject <> nil) then begin
{$IFDEF WIN32}
    if StoreColumns then
      Section := FIniLink.RootSection + GetDefaultSection(Self) else
{$ENDIF}
    if (FIniLink.RootSection <> '') and (DataSource <> nil) and
      (DataSource.DataSet <> nil) then
      Section := FIniLink.RootSection + DataSetSectionName(DataSource.DataSet)
    else Section := '';
    InternalSaveLayout(FIniLink.IniObject, Section);
  end;
end;

procedure TJvDBGrid.IniLoad(Sender: TObject);
var
  Section: string;
begin
  if (Name <> '') and (FIniLink.IniObject <> nil) then begin
{$IFDEF WIN32}
    if StoreColumns then
      Section := FIniLink.RootSection + GetDefaultSection(Self) else
{$ENDIF}
    if (FIniLink.RootSection <> '') and (DataSource <> nil) and
      (DataSource.DataSet <> nil) then
      Section := FIniLink.RootSection + DataSetSectionName(DataSource.DataSet)
    else Section := '';
    InternalRestoreLayout(FIniLink.IniObject, Section);
  end;
end;

{ TJvDBComboEdit }

procedure ResetMaxLength(DBEdit: TJvDBComboEdit);
var
  F: TField;
begin
  with DBEdit do
    if (MaxLength > 0) and (DataSource <> nil) and
      (DataSource.DataSet <> nil) then
    begin
      F := DataSource.DataSet.FindField(DataField);
      if Assigned(F) and (F.DataType = ftString) and
        (F.Size = MaxLength) then MaxLength := 0;
    end;
end;

constructor TJvDBComboEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF WIN32}
  ControlStyle := ControlStyle + [csReplicatable];
{$ENDIF}
  inherited ReadOnly := True;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  AlwaysEnable := True;
end;

destructor TJvDBComboEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
{$IFDEF WIN32}
  FCanvas.Free;
{$ENDIF}
  inherited Destroy;
end;

procedure TJvDBComboEdit.Loaded;
begin
  inherited Loaded;
  ResetMaxLength(Self);
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TJvDBComboEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TJvDBComboEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
    FDataLink.Edit;
end;

procedure TJvDBComboEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key in [#32..#255]) and (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
    Beep;
    Key := #0;
  end;
  case Key of
    ^H, ^V, ^X, #32..#255:
      FDataLink.Edit;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
  end;
end;

function TJvDBComboEdit.EditCanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;

procedure TJvDBComboEdit.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;

procedure TJvDBComboEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then begin
    FFocused := Value;
    if (Alignment <> taLeftJustify) and not IsMasked then Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TJvDBComboEdit.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;

function TJvDBComboEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvDBComboEdit.SetDataSource(Value: TDataSource);
begin
{$IFDEF COMPILER4_UP}
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
{$ENDIF}
    FDataLink.DataSource := Value;
{$IFDEF WIN32}
  if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
end;

function TJvDBComboEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TJvDBComboEdit.SetDataField(const Value: string);
begin
  if not (csDesigning in ComponentState) then ResetMaxLength(Self);
  FDataLink.FieldName := Value;
end;

function TJvDBComboEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TJvDBComboEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TJvDBComboEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TJvDBComboEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then begin
    if Alignment <> FDataLink.Field.Alignment then begin
      EditText := '';  {forces update}
      Alignment := FDataLink.Field.Alignment;
    end;
    EditMask := FDataLink.Field.EditMask;
    if not (csDesigning in ComponentState) then begin
      if (FDataLink.Field.DataType = ftString) and (MaxLength = 0) then
        MaxLength := FDataLink.Field.Size;
    end;
    if FFocused and FDataLink.CanModify then
      Text := FDataLink.Field.Text
    else begin
      EditText := FDataLink.Field.DisplayText;
      {if FDataLink.Editing then Modified := True;}
    end;
  end
  else begin
    Alignment := taLeftJustify;
    EditMask := '';
    if csDesigning in ComponentState then EditText := Name
    else EditText := '';
  end;
end;

procedure TJvDBComboEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
end;

procedure TJvDBComboEdit.UpdateData(Sender: TObject);
begin
  ValidateEdit;
  FDataLink.Field.Text := Text;
end;

procedure TJvDBComboEdit.WMPaste(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TJvDBComboEdit.WMCut(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TJvDBComboEdit.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
{$IFDEF COMPILER3_UP}
  if SysLocale.FarEast and FDataLink.CanModify then
    inherited ReadOnly := False;
{$ENDIF}
end;

procedure TJvDBComboEdit.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SelectAll;
    if CanFocus then SetFocus;
    raise;
  end;
  SetFocused(False);
  CheckCursor;
  DoExit;
end;

{$IFDEF WIN32}
procedure TJvDBComboEdit.WMPaint(var Message: TWMPaint);
var
  S: string;
begin
  if (csPaintCopy in ControlState) and (FDataLink.Field <> nil) then
  begin
    S := FDataLink.Field.DisplayText;
    case CharCase of
      ecUpperCase: S := AnsiUpperCase(S);
      ecLowerCase: S := AnsiLowerCase(S);
    end;
  end
  else S := EditText;
  if not PaintComboEdit(Self, S, Alignment, True, FCanvas, Message) then
    inherited;
end;

procedure TJvDBComboEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;
{$ENDIF}

{$IFDEF COMPILER4_UP}
function TJvDBComboEdit.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

function TJvDBComboEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TJvDBComboEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;
{$ENDIF}

{ TJvDBDateEdit }

constructor TJvDBDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF WIN32}
  ControlStyle := ControlStyle + [csReplicatable];
{$ENDIF}
  inherited ReadOnly := True;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  Self.OnAcceptDate := AfterPopup;
  AlwaysEnable := True;
  UpdateMask;
end;

destructor TJvDBDateEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
{$IFDEF WIN32}
  FCanvas.Free;
{$ENDIF}
  inherited Destroy;
end;

procedure TJvDBDateEdit.AfterPopup(Sender: TObject; var Date: TDateTime;
  var Action: Boolean);
begin
  Action := Action and (DataSource <> nil) and (DataSource.DataSet <> nil) and
    DataSource.DataSet.CanModify;
  if Action then Action := EditCanModify;
end;

procedure TJvDBDateEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TJvDBDateEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if not ReadOnly and ((Key = VK_DELETE) or ((Key = VK_INSERT)
    and (ssShift in Shift))) then
    FDataLink.Edit;
end;

procedure TJvDBDateEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key in [#32..#255]) and (FDataLink.Field <> nil) and
    not (Key in ['0'..'9']) and (Key <> DateSeparator) then
  begin
    Beep;
    Key := #0;
  end;
  case Key of
    ^H, ^V, ^X, '0'..'9': FDataLink.Edit;
    #27:
      begin
        Reset;
        Key := #0;
      end;
  end;
end;

function TJvDBDateEdit.EditCanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;

procedure TJvDBDateEdit.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;

procedure TJvDBDateEdit.Change;
begin
  if not Formatting then FDataLink.Modified;
  inherited Change;
end;

function TJvDBDateEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvDBDateEdit.SetDataSource(Value: TDataSource);
begin
{$IFDEF COMPILER4_UP}
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
{$ENDIF}
    FDataLink.DataSource := Value;
{$IFDEF WIN32}
  if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
end;

function TJvDBDateEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TJvDBDateEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TJvDBDateEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TJvDBDateEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TJvDBDateEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TJvDBDateEdit.UpdateMask;
begin
  UpdateFormat;
  UpdatePopup;
  DataChange(nil);
end;

procedure TJvDBDateEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then begin
    EditMask := GetDateMask;
    Self.Date := FDataLink.Field.AsDateTime;
  end
  else begin
    if csDesigning in ComponentState then begin
      EditMask := '';
      EditText := Name;
    end
    else begin
      EditMask := GetDateMask;
      if DefaultToday then Date := SysUtils.Date
      else Date := NullDate;
    end;
  end;
end;

procedure TJvDBDateEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
  if FDataLink.Editing and DefaultToday and (FDataLink.Field <> nil) and
    (FDataLink.Field.AsDateTime = NullDate) then
    FDataLink.Field.AsDateTime := SysUtils.Now;
end;

procedure TJvDBDateEdit.UpdateData(Sender: TObject);
var
  D: TDateTime;
begin
  ValidateEdit;
  D := Self.Date;
  if D <> NullDate then
    FDataLink.Field.AsDateTime := D + Frac(FDataLink.Field.AsDateTime)
  else FDataLink.Field.Clear;
end;

{$IFDEF WIN32}
procedure TJvDBDateEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

procedure TJvDBDateEdit.WMPaint(var Message: TWMPaint);
var
  S: string;
begin
  if (csPaintCopy in ControlState) and (FDataLink.Field <> nil) then begin
    if FDataLink.Field.IsNull then begin
      S := GetDateFormat;
      S := ReplaceStr(ReplaceStr(ReplaceStr(ReplaceStr(S, '/', DateSeparator),
        'Y', ' '), 'M', ' '), 'D', ' ');
    end
    else
      S := FormatDateTime(GetDateFormat, FDataLink.Field.AsDateTime);
  end else S := EditText;
  if not PaintComboEdit(Self, S, Alignment, True, FCanvas, Message) then
    inherited;
end;

procedure TJvDBDateEdit.AcceptValue(const Value: Variant);
begin
  if VarIsNull(Value) or VarIsEmpty(Value) then FDataLink.Field.Clear
  else FDataLink.Field.AsDateTime :=
    VarToDateTime(Value) + Frac(FDataLink.Field.AsDateTime);
  DoChange;
end;
{$ENDIF}

procedure TJvDBDateEdit.ApplyDate(Value: TDateTime);
begin
  FDataLink.Edit;
  inherited ApplyDate(Value);
end;

procedure TJvDBDateEdit.WMPaste(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TJvDBDateEdit.WMCut(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TJvDBDateEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TJvDBDateEdit.CMExit(var Message: TCMExit);
begin
  try
    if not (csDesigning in ComponentState) and CheckOnExit then
      CheckValidDate;
    FDataLink.UpdateRecord;
  except
    SelectAll;
    if CanFocus then SetFocus;
    raise;
  end;
  CheckCursor;
  DoExit;
end;

{$IFDEF COMPILER4_UP}
function TJvDBDateEdit.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

function TJvDBDateEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TJvDBDateEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;
{$ENDIF}

{ TJvDBCalcEdit }

constructor TJvDBCalcEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF WIN32}
  ControlStyle := ControlStyle + [csReplicatable];
{$ENDIF}
  inherited ReadOnly := True;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateFieldData;
  AlwaysEnable := True;
end;

destructor TJvDBCalcEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TJvDBCalcEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TJvDBCalcEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if not ReadOnly and ((Key = VK_DELETE) or ((Key = VK_INSERT)
    and (ssShift in Shift))) then FDataLink.Edit;
end;

procedure TJvDBCalcEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    ^H, ^V, ^X, #32..#255:
      if not PopupVisible then FDataLink.Edit;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
  end;
end;

function TJvDBCalcEdit.IsValidChar(Key: Char): Boolean;
begin
  Result := inherited IsValidChar(Key);
  if Result and (FDatalink.Field <> nil) then
    Result := FDatalink.Field.IsValidChar(Key);
end;

procedure TJvDBCalcEdit.UpdatePopup;
var
  Precision: Byte;
begin
  Precision := DefCalcPrecision;
  if (FDatalink <> nil) and (FDatalink.Field <> nil) and
    (FDatalink.Field is TFloatField) then
    Precision := TFloatField(FDatalink.Field).Precision;
  if FPopup <> nil then
    SetupPopupCalculator(FPopup, Precision, BeepOnError);
end;

function TJvDBCalcEdit.EditCanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;

{$IFDEF WIN32}
function TJvDBCalcEdit.GetDisplayText: string;
var
  E: Extended;
begin
  if (csPaintCopy in ControlState) and (FDatalink.Field <> nil) then begin
    if FDataLink.Field.IsNull then E := 0.0
    else if FDataLink.Field.DataType in [ftSmallint, ftInteger, ftWord] then
      E := FDataLink.Field.AsInteger
    else if FDataLink.Field.DataType = ftBoolean then
      E := Ord(FDataLink.Field.AsBoolean)
{$IFDEF COMPILER4_UP}
    else if FDataLink.Field is TLargeintField then
      E := TLargeintField(FDataLink.Field).AsLargeInt
{$ENDIF}
    else E := FDataLink.Field.AsFloat;
    if FDataLink.Field.IsNull then Result := ''
    else Result := FormatDisplayText(E);
  end
  else begin
    if (FDataLink.Field = nil) then begin
      if (csDesigning in ComponentState) then Result := Format('(%s)', [Name])
      else Result := '';
    end
    else Result := inherited GetDisplayText;
  end;
end;
{$ENDIF}

procedure TJvDBCalcEdit.Reset;
begin
  FDataLink.Reset;
  inherited Reset;
end;

procedure TJvDBCalcEdit.Change;
begin
  if not Formatting then FDataLink.Modified;
  inherited Change;
end;

function TJvDBCalcEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvDBCalcEdit.SetDataSource(Value: TDataSource);
begin
  if FDataLink.DataSource <> Value then begin
{$IFDEF COMPILER4_UP}
    if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
{$ENDIF}
      FDataLink.DataSource := Value;
{$IFDEF WIN32}
    if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
    UpdateFieldParams;
  end;
end;

function TJvDBCalcEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TJvDBCalcEdit.SetDataField(const Value: string);
begin
  if FDataLink.FieldName <> Value then begin
    FDataLink.FieldName := Value;
    UpdateFieldParams;
  end;
end;

procedure TJvDBCalcEdit.SetDefaultParams(Value: Boolean);
begin
  if DefaultParams <> Value then begin
    FDefaultParams := Value;
    if FDefaultParams then UpdateFieldParams;
  end;
end;

procedure TJvDBCalcEdit.UpdateFieldParams;
begin
  if FDatalink.Field <> nil then begin
    if FDatalink.Field is TNumericField then begin
      if TNumericField(FDatalink.Field).DisplayFormat <> '' then
        DisplayFormat := TNumericField(FDatalink.Field).DisplayFormat;
      Alignment := TNumericField(FDatalink.Field).Alignment;
    end;
{$IFDEF COMPILER4_UP}
    if FDatalink.Field is TLargeintField then begin
      MaxValue := TLargeintField(FDatalink.Field).MaxValue;
      MinValue := TLargeintField(FDatalink.Field).MinValue;
      DecimalPlaces := 0;
      if DisplayFormat = '' then DisplayFormat := ',#';
    end else
{$ENDIF}
    if FDatalink.Field is TIntegerField then begin
      MaxValue := TIntegerField(FDatalink.Field).MaxValue;
      MinValue := TIntegerField(FDatalink.Field).MinValue;
      DecimalPlaces := 0;
      if DisplayFormat = '' then DisplayFormat := ',#';
    end
{$IFDEF WIN32}
    else if FDatalink.Field is TBCDField then begin
      MaxValue := TBCDField(FDatalink.Field).MaxValue;
      MinValue := TBCDField(FDatalink.Field).MinValue;
    end
{$ENDIF}
    else if FDatalink.Field is TFloatField then begin
      MaxValue := TFloatField(FDatalink.Field).MaxValue;
      MinValue := TFloatField(FDatalink.Field).MinValue;
      DecimalPlaces := TFloatField(FDatalink.Field).Precision;
    end
    else if FDatalink.Field is TBooleanField then begin
      MinValue := 0;
      MaxValue := 1;
      DecimalPlaces := 0;
      if DisplayFormat = '' then DisplayFormat := ',#';
    end;
  end;
  UpdatePopup;
end;

function TJvDBCalcEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TJvDBCalcEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TJvDBCalcEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TJvDBCalcEdit.DataChange(Sender: TObject);
begin
  if FDefaultParams then UpdateFieldParams;
  if FDataLink.Field <> nil then begin
    if FDataLink.Field.IsNull then begin
      Self.Value := 0.0;
      EditText := '';
    end
    else if FDataLink.Field.DataType in [ftSmallint, ftInteger, ftWord] then
      Self.AsInteger := FDataLink.Field.AsInteger
    else if FDataLink.Field.DataType = ftBoolean then
      Self.AsInteger := Ord(FDataLink.Field.AsBoolean)
{$IFDEF COMPILER4_UP}
    else if FDataLink.Field is TLargeintField then
      Self.Value := TLargeintField(FDataLink.Field).AsLargeInt
{$ENDIF}
    else Self.Value := FDataLink.Field.AsFloat;
    DataChanged;
  end
  else begin
    if csDesigning in ComponentState then begin
      Self.Value := 0;
      EditText := Format('(%s)', [Name]);
    end
    else Self.Value := 0;
  end;
end;

procedure TJvDBCalcEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
end;

procedure TJvDBCalcEdit.UpdateFieldData(Sender: TObject);
begin
  inherited UpdateData;
  if (Value = 0) and ZeroEmpty then FDataLink.Field.Clear
  else if FDataLink.Field.DataType in [ftSmallint, ftInteger, ftWord] then
    FDataLink.Field.AsInteger := Self.AsInteger
  else if FDataLink.Field.DataType = ftBoolean then
    FDataLink.Field.AsBoolean := Boolean(Self.AsInteger)
  else FDataLink.Field.AsFloat := Self.Value;
end;

{$IFDEF WIN32}
procedure TJvDBCalcEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

procedure TJvDBCalcEdit.AcceptValue(const Value: Variant);
begin
  if VarIsNull(Value) or VarIsEmpty(Value) then FDataLink.Field.Clear
  else FDataLink.Field.Value := Value;
  DoChange;
end;
{$ENDIF}

procedure TJvDBCalcEdit.WMPaste(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TJvDBCalcEdit.WMCut(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TJvDBCalcEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TJvDBCalcEdit.CMExit(var Message: TCMExit);
begin
  try
    CheckRange;
    FDataLink.UpdateRecord;
  except
    SelectAll;
    if CanFocus then SetFocus;
    raise;
  end;
  inherited;
end;

{$IFDEF COMPILER4_UP}
function TJvDBCalcEdit.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

function TJvDBCalcEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TJvDBCalcEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;
{$ENDIF}

{ TJvStatusDataLink }

type
  TJvStatusDataLink = class(TDataLink)
  private
    FLabel: TJvDBStatusLabel;
  protected
    procedure ActiveChanged; override;
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure LayoutChanged; override;
  public
    constructor Create(ALabel: TJvDBStatusLabel);
    destructor Destroy; override;
  end;

constructor TJvStatusDataLink.Create(ALabel: TJvDBStatusLabel);
begin
  inherited Create;
  FLabel := ALabel;
end;

destructor TJvStatusDataLink.Destroy;
begin
  FLabel := nil;
  inherited Destroy;
end;

procedure TJvStatusDataLink.ActiveChanged;
begin
  DataSetChanged;
end;

procedure TJvStatusDataLink.DataSetScrolled(Distance: Integer);
begin
  if (FLabel <> nil) and (FLabel.Style = lsRecordNo) then
    FLabel.UpdateStatus;
end;

procedure TJvStatusDataLink.EditingChanged;
begin
  if (FLabel <> nil) and (FLabel.Style <> lsRecordSize) then
    FLabel.UpdateStatus;
end;

procedure TJvStatusDataLink.DataSetChanged;
begin
  if (FLabel <> nil) then FLabel.UpdateData;
end;

procedure TJvStatusDataLink.LayoutChanged;
begin
  if (FLabel <> nil) and (FLabel.Style <> lsRecordSize) then
    DataSetChanged; { ??? }
end;

{ TJvDBStatusLabel }

const
  GlyphSpacing = 2;
  GlyphColumns = 7;

constructor TJvDBStatusLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ShadowSize := 0;
  Layout := tlCenter;
  ControlStyle := ControlStyle - [csSetCaption {$IFDEF WIN32},
    csReplicatable {$ENDIF}];
  FRecordCount := -1;
  FRecordNo := -1;
  ShowAccelChar := False;
  FDataSetName := EmptyStr;
  FDataLink := TJvStatusDataLink.Create(Self);
  FStyle := lsState;
  GlyphAlign := glGlyphLeft;
  FEditColor := clRed;
  FCaptions := TStringList.Create;
  TStringList(FCaptions).OnChange := CaptionsChanged;
  FGlyph := TBitmap.Create;
  FGlyph.Handle := LoadBitmap(HInstance, 'DS_STATES');
  Caption := '';
end;

destructor TJvDBStatusLabel.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  //DisposeStr(FDataSetName);
  TStringList(FCaptions).OnChange := nil;
  FCaptions.Free;
  FCaptions := nil;
  FCell.Free;
  FCell := nil;
  FGlyph.Free;
  FGlyph := nil;
  inherited Destroy;
end;

procedure TJvDBStatusLabel.Loaded;
begin
  inherited Loaded;
  UpdateData;
end;

function TJvDBStatusLabel.GetDefaultFontColor: TColor;
begin
  if (FStyle = lsState) and (FDatalink <> nil) and
    (GetDatasetState in [dsEdit, dsInsert]) then
    Result := FEditColor
  else Result := inherited GetDefaultFontColor;
end;

function TJvDBStatusLabel.GetLabelCaption: string;
begin
  if (csDesigning in ComponentState) and ((FStyle = lsState) or
    (FDatalink = nil) or not FDatalink.Active) then
    Result := Format('(%s)', [Name])
  else if ((FDatalink = nil) or (DataSource = nil)) then
    Result := ''
  else begin
    case FStyle of
      lsState:
        if FShowOptions in [doCaption, doBoth] then begin
          if DataSetName = '' then Result := GetCaption(DataSource.State)
          else Result := Format('%s: %s', [DataSetName, GetCaption(DataSource.State)]);
        end
        else { doGlyph } Result := '';
      lsRecordNo:
        if FDataLink.Active then begin
          if FRecordNo >= 0 then begin
            if FRecordCount >= 0 then
              Result := Format('%d:%d', [FRecordNo, FRecordCount])
            else Result := IntToStr(FRecordNo);
          end
          else begin
            if FRecordCount >= 0 then
              Result := Format('( %d )', [FRecordCount])
            else Result := '';
          end;
        end
        else Result := '';
      lsRecordSize:
        if FDatalink.Active then
          Result := IntToStr(FDatalink.DataSet.RecordSize)
        else Result := '';
    end;
  end;
end;

function TJvDBStatusLabel.GetDatasetState: TDataSetState;
begin
  if DataSource <> nil then
    Result := DataSource.State
  else Result := dsInactive;
end;

procedure TJvDBStatusLabel.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  if (csDesigning in ComponentState) then Invalidate;
end;

procedure TJvDBStatusLabel.SetCaptions(Value: TStrings);
begin
  FCaptions.Assign(Value);
end;

function TJvDBStatusLabel.GetStatusKind(State: TDataSetState): TDBStatusKind;
begin
{$IFDEF WIN32}
  if not (State in [Low(TDBStatusKind)..High(TDBStatusKind)]) then begin
    case State of
      dsFilter: Result := dsSetKey;
{$IFDEF COMPILER3_UP}
      dsNewValue, dsOldValue, dsCurValue: Result := dsEdit;
{$ELSE}
      dsUpdateNew, dsUpdateOld: Result := dsEdit;
{$ENDIF}
      else Result := TDBStatusKind(State);
    end;
  end
  else
{$ENDIF WIN32}
    Result := TDBStatusKind(State);
end;

function TJvDBStatusLabel.GetCaption(State: TDataSetState): string;
const
  StrIds: array[TDBStatusKind] of Word = (SInactiveData, SBrowseData,
    SEditData, SInsertData, SSetKeyData, SCalcFieldsData);
var
  Kind: TDBStatusKind;
begin
  Kind := GetStatusKind(State);
  if (FCaptions <> nil) and (Ord(Kind) < FCaptions.Count) and
    (FCaptions[Ord(Kind)] <> '') then Result := FCaptions[Ord(Kind)]
  else Result := LoadStr(StrIds[Kind]);
end;

procedure TJvDBStatusLabel.Paint;
var
  GlyphOrigin: TPoint;
begin
  inherited Paint;
  if (FStyle = lsState) and (FShowOptions in [doGlyph, doBoth]) and
    (FCell <> nil) then
  begin
    if GlyphAlign = glGlyphLeft then
      GlyphOrigin.X := GlyphSpacing
    else {glGlyphRight}
      GlyphOrigin.X := Left + ClientWidth - RightMargin + GlyphSpacing;
    case Layout of
      tlTop: GlyphOrigin.Y := 0;
      tlCenter: GlyphOrigin.Y := (ClientHeight - FCell.Height) div 2;
      else { tlBottom } GlyphOrigin.Y := ClientHeight - FCell.Height;
    end;
    DrawBitmapTransparent(Canvas, GlyphOrigin.X, GlyphOrigin.Y,
      FCell, FGlyph.TransparentColor);
  end;
end;

procedure TJvDBStatusLabel.CaptionsChanged(Sender: TObject);
begin
  TStringList(FCaptions).OnChange := nil;
  try
    while (Pred(FCaptions.Count) > Ord(High(TDBStatusKind))) do
      FCaptions.Delete(FCaptions.Count - 1);
  finally
    TStringList(FCaptions).OnChange := CaptionsChanged;
  end;
  if not (csDesigning in ComponentState) then Invalidate;
end;

procedure TJvDBStatusLabel.UpdateData;

  function IsSequenced: Boolean;
  begin
{$IFDEF COMPILER3_UP}
    Result := FDatalink.DataSet.IsSequenced;
{$ELSE}
    Result := not ((FDatalink.DataSet is TDBDataSet) and
      TDBDataSet(FDatalink.DataSet).Database.IsSQLBased);
{$ENDIF}
  end;

begin
  FRecordCount := -1;
  if (FStyle = lsRecordNo) and FDataLink.Active and
    (DataSource.State in [dsBrowse, dsEdit]) then
  begin
    if Assigned(FOnGetRecordCount) then
      FOnGetRecordCount(Self, FDataLink.DataSet, FRecordCount)
    else if (FCalcCount or IsSequenced) then
{$IFDEF COMPILER3_UP}
      FRecordCount := FDataLink.DataSet.RecordCount;
{$ELSE}
      FRecordCount := DataSetRecordCount(FDataLink.DataSet)
{$ENDIF}
  end;
  UpdateStatus;
end;

procedure TJvDBStatusLabel.UpdateStatus;
begin
  if DataSource <> nil then begin
    case FStyle of
      lsState:
        if FShowOptions in [doGlyph, doBoth] then begin
          if GlyphAlign = glGlyphLeft then begin
            RightMargin := 0;
            LeftMargin := (FGlyph.Width div GlyphColumns) + GlyphSpacing * 2;
          end
          else {glGlyphRight} begin
            LeftMargin := 0;
            RightMargin := (FGlyph.Width div GlyphColumns) + GlyphSpacing * 2;
          end;
          if FCell = nil then FCell := TBitmap.Create;
          AssignBitmapCell(FGlyph, FCell, GlyphColumns, 1,
            Ord(GetStatusKind(DataSource.State)));
        end
        else { doCaption } begin
          FCell.Free;
          FCell := nil;
          LeftMargin := 0;
          RightMargin := 0;
        end;
      lsRecordNo:
        begin
          FCell.Free;
          FCell := nil;
          LeftMargin := 0;
          RightMargin := 0;
          FRecordNo := -1;
          if FDataLink.Active then begin
            if Assigned(FOnGetRecNo) then
              FOnGetRecNo(Self, FDataLink.DataSet, FRecordNo) else
            try
{$IFDEF COMPILER3_UP}
              with FDatalink.DataSet do
                if not IsEmpty then FRecordNo := RecNo;
{$ELSE}
              FRecordNo := DataSetRecNo(FDatalink.DataSet);
{$ENDIF}
            except
            end;
          end;
        end;
      lsRecordSize:
        begin
          FCell.Free;
          FCell := nil;
          LeftMargin := 0;
          RightMargin := 0;
        end;
    end;
  end
  else begin
    FCell.Free;
    FCell := nil;
  end;
  AdjustBounds;
  Invalidate;
end;

procedure TJvDBStatusLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TJvDBStatusLabel.GetDataSetName: string;
begin
  Result := FDataSetName;
  if not (csDesigning in ComponentState) then begin
    if Assigned(FOnGetDataName) then Result := FOnGetDataName(Self)
    else if (Result = '') and (DataSource <> nil) and
      (DataSource.DataSet <> nil) then Result := DataSource.DataSet.Name;
  end;
end;

procedure TJvDBStatusLabel.SetDataSetName(Value: string);
begin
  FDataSetName := Value;
  Invalidate;
end;

function TJvDBStatusLabel.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvDBStatusLabel.SetDataSource(Value: TDataSource);
begin
{$IFDEF COMPILER4_UP}
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
{$ENDIF}
    FDataLink.DataSource := Value;
{$IFDEF WIN32}
  if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
  if not (csLoading in ComponentState) then UpdateData;
end;

procedure TJvDBStatusLabel.SetEditColor(Value: TColor);
begin
  if FEditColor <> Value then begin
    FEditColor := Value;
    if Style = lsState then Invalidate;
  end;
end;

procedure TJvDBStatusLabel.SetGlyphAlign(Value: TGlyphAlign);
begin
  if FGlyphAlign <> Value then begin
    FGlyphAlign := Value;
    UpdateStatus;
  end;
end;

procedure TJvDBStatusLabel.SetShowOptions(Value: TDBLabelOptions);
begin
  if FShowOptions <> Value then begin
    FShowOptions := Value;
    UpdateStatus;
  end;
end;

procedure TJvDBStatusLabel.SetCalcCount(Value: Boolean);
begin
  if FCalcCount <> Value then begin
    FCalcCount := Value;
    if not (csLoading in ComponentState) then UpdateData;
  end;
end;

procedure TJvDBStatusLabel.SetStyle(Value: TDBLabelStyle);
begin
  if FStyle <> Value then begin
    FStyle := Value;
    if not (csLoading in ComponentState) then UpdateData;
  end;
end;

{$IFDEF WIN32}
initialization
finalization
  DestroyLocals;
{$ELSE}
initialization
  AddExitProc(DestroyLocals);
{$ENDIF}
end.
