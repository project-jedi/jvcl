{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBGrid.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Contributor(s):
  Polaris Software
  Lionel Reynaud
  Flemming Brandt Clausen
  Frédéric Leneuf-Magaud
  Andreas Hausladen

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

-----------------------------------------------------------------------------

INFO: Draw events are triggered in this order:

- Title cells:
OnGetBtnParams
OnDrawColumnTitle

- Data cells:
OnGetCellParams
OnDrawColumnCell

OnGetCellProps and OnDrawDataCell are obsolete.

-----------------------------------------------------------------------------

KNOWN ISSUES:

- THE ColLines OPTION DOES NOT WORK WELL WITH HIDDEN COLUMNS - BUG SOURCE: DBGRID.PAS
  If a column is followed by hidden columns and ColLines is set to False, the display size
  of the column is smaller than its width. This is easy to notice when you give the focus
  to the cell (the focus rect is truncated) or when you use the AutoSize feature (there's
  a gap after the last column). This bug comes from DBGrid.pas.

-----------------------------------------------------------------------------
2004/07/08 - WPostma merged changes by Frédéric Leneuf-Magaud and ahuser.}

// $Id$

unit JvDBGrid;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Types,
  Windows, Messages, Classes, Graphics, Controls, Grids, Menus, DBGrids, DB,
  StdCtrls, Forms, Contnrs,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  JvTypes, {JvTypes contains Exception base class}
  JvAppStorage, JvFormPlacement, JvExDBGrids, JvDBUtils;

const
  DefJvGridOptions = [dgEditing, dgTitles, dgIndicator, dgColumnResize,
    dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit
    {$IFDEF COMPILER14_UP}
    , dgTitleClick, dgTitleHotTrack
    {$ENDIF COMPILER14_UP}];
  {$NODEFINE DefJvGridOptions}

  JvDefaultAlternateRowColor = TColor($00CCCCCC); // Light gray
  JvDefaultAlternateRowFontColor = TColor($00000000); // Black

  // Consts for AutoSizeColumnIndex
  JvGridResizeProportionally = -1;
  JvGridResizeLastVisibleCol = -2;

type
  TJvDBGrid = class;

  // Mantis 3895: The only way to lift an ambiguity in an event handler is to
  // redefine a type. A simple rename is not enough, hence the distinction
  // between BCB and the others.
  {$IFDEF BCB}
  TJvDBGridBitmap = class(TBitmap)
  end;
  {$ELSE}
  {$IFDEF DELPHI10_UP}
  TJvDBGridBitmap = class(TBitmap)
  end;
  {$ELSE}
  TJvDBGridBitmap = TBitmap;
  {$ENDIF DELPHI10_UP}
  {$ENDIF BCB}

  TJvDBGridColumnResize = (gcrNone, gcrGrid, gcrDataSet);
  TJvDBGridCellHintPosition = (gchpDefault, gchpMouse);

  TSelectColumn = (scDataBase, scGrid);
  TTitleClickEvent = procedure(Sender: TObject; ACol: Longint;
    Field: TField) of object;
  TCheckTitleBtnEvent = procedure(Sender: TObject; ACol: Longint;
    Field: TField; var Enabled: Boolean) of object;
  TGetCellParamsEvent = procedure(Sender: TObject; Field: TField;
    AFont: TFont; var Background: TColor; Highlight: Boolean) of object;
  TSortMarker = (smNone, smDown, smUp);
  TGetBtnParamsEvent = procedure(Sender: TObject; Field: TField;
    AFont: TFont; var Background: TColor; var ASortMarker: TSortMarker;
    IsDown: Boolean) of object;
  TGetCellPropsEvent = procedure(Sender: TObject; Field: TField;
    AFont: TFont; var Background: TColor) of object; { obsolete }
  TJvDBEditShowEvent = procedure(Sender: TObject; Field: TField;
    var AllowEdit: Boolean) of object;
  TDrawColumnTitleEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    ARect: TRect; AColumn: TColumn; var ASortMarker: TJvDBGridBitmap; IsDown: Boolean;
    var Offset: Integer; var DefaultDrawText,
    DefaultDrawSortMarker: Boolean) of object;
  TJvTitleHintEvent = procedure(Sender: TObject; Field: TField;
    var AHint: string; var ATimeOut: Integer) of object;
  TJvCellHintEvent = TJvTitleHintEvent;
  TJvDBColumnResizeEvent = procedure(Grid: TJvDBGrid; ACol: Longint; NewWidth: Integer) of object;
  TJvDBCheckIfBooleanFieldEvent = function(Grid: TJvDBGrid; Field: TField;
    var StringForTrue: string; var StringForFalse: string): Boolean of object;
  TJvDBCanEditCellEvent = procedure(Grid: TJvDBGrid; Field: TField; var AllowEdit: Boolean) of object;
  TJvDBSelectColumnsEvent = procedure(Grid: TJvDBGrid; var DefaultDialog: Boolean) of object;

  TJvDBGridLayoutChangeKind = (lcLayoutChanged, lcSizeChanged, lcTopLeftChanged);
  TJvDBGridLayoutChangeEvent = procedure(Grid: TJvDBGrid; Kind: TJvDBGridLayoutChangeKind) of object;
  TJvDBGridLayoutChangeLink = class
  private
    FOnChange: TJvDBGridLayoutChangeEvent;
  public
    procedure DoChange(Grid: TJvDBGrid; Kind: TJvDBGridLayoutChangeKind);
    property OnChange: TJvDBGridLayoutChangeEvent read FOnChange write FOnChange;
  end;

  EJVCLDbGridException = Class(EJVCLException);

  TJvSelectDialogColumnStrings = class(TPersistent)
  private
    FCaption: string;
    FRealNamesOption: string;
    FOK: string;
    FNoSelectionWarning: string;
  public
    constructor Create;
  published
    property Caption: string read FCaption write FCaption;
    property RealNamesOption: string read FRealNamesOption write FRealNamesOption;
    property OK: string read FOK write FOK;
    property NoSelectionWarning: string read FNoSelectionWarning write FNoSelectionWarning;
  end;

  TJvDBGridControlSize = (
    fcCellSize,     // Fit the control into the cell
    fcDesignSize,   // Leave the control as it was at design time
    fcBiggest       // Take the biggest size between Cell size and Design time size
  );

  TJvDBGridControl = class(TCollectionItem)
  private
    FControlName: string;
    FFieldName: string;
    FFitCell: TJvDBGridControlSize;
    FLeaveOnEnterKey: Boolean;
    FLeaveOnUpDownKey: Boolean;
    FDesignWidth: Integer;  // value set when needed by PlaceControl
    FDesignHeight: Integer; // value set when needed by PlaceControl
  public
    procedure Assign(Source: TPersistent); override;
  published
    property ControlName: string read FControlName write FControlName;
    property FieldName: string read FFieldName write FFieldName;
    property FitCell: TJvDBGridControlSize read FFitCell write FFitCell;
    property LeaveOnEnterKey: Boolean read FLeaveOnEnterKey write FLeaveOnEnterKey default False;
    property LeaveOnUpDownKey: Boolean read FLeaveOnUpDownKey write FLeaveOnUpDownKey default False;
  end;

  TJvDBGridControls = class(TCollection)
  private
    FParentDBGrid: TJvDBGrid;
    function GetItem(Index: Integer): TJvDBGridControl;
    procedure SetItem(Index: Integer; Value: TJvDBGridControl);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(ParentDBGrid: TJvDBGrid);
    function Add: TJvDBGridControl;
    function ControlByField(const FieldName: string): TJvDBGridControl;
    function ControlByName(const CtrlName: string): TJvDBGridControl;
    property Items[Index: Integer]: TJvDBGridControl read GetItem write SetItem; default;
  end;

  TCharList = TCharSet;

  TJvGridPaintInfo = record
    MouseInCol: Integer; // the column that the mouse is in
    ColPressed: Boolean; // a column has been pressed
    ColPressedIdx: Integer; // idx of the pressed column
    ColSizing: Boolean; // currently sizing a column
    ColMoving: Boolean; // currently moving a column
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvDBGrid = class(TJvExDBGrid, IJvDataControl)
  private
    FAutoSort: Boolean;
    FBeepOnError: Boolean;
    FAutoAppend: Boolean;
    FSizingIndex: Integer;
    FSizingOfs: Integer;
    FShowGlyphs: Boolean;
    FDefaultDrawing: Boolean;
    FReduceFlicker: Boolean;
    FMultiSelect: Boolean;
    FSelecting: Boolean;
    FClearSelection: Boolean;
    FTitleButtons: Boolean;
    FPressedCol: TColumn;
    FPressed: Boolean;
    FTracking: Boolean;
    FSwapButtons: Boolean;
    FIniLink: TJvIniLink;
    FDisableCount: Integer;
    FFixedCols: Integer;
    FOnCheckButton: TCheckTitleBtnEvent;
    FOnGetCellProps: TGetCellPropsEvent;
    FOnGetCellParams: TGetCellParamsEvent;
    FOnGetBtnParams: TGetBtnParamsEvent;
    FOnEditChange: TNotifyEvent;
    FOnTitleBtnClick: TTitleClickEvent;
    FOnTitleBtnDblClick: TTitleClickEvent;
    FOnTopLeftChanged: TNotifyEvent;
    FSelectionAnchor: {$IFDEF RTL200_UP}TBookmark{$ELSE}TBookmarkStr{$ENDIF RTL200_UP};
    FOnDrawColumnTitle: TDrawColumnTitleEvent;
    FWord: string;
    FShowTitleHint: Boolean;
    FSortedField: string;
    FPostOnEnterKey: Boolean;
    FSelectColumn: TSelectColumn;
    FTitleArrow: Boolean;
    FTitleArrowDown: Boolean;
    FTitlePopup: TPopupMenu;
    FOnShowTitleHint: TJvTitleHintEvent;
    FOnTitleArrowMenuEvent: TNotifyEvent;
    FAlternateRowColor: TColor;
    FAlternateRowFontColor: TColor;
    FAutoSizeColumns: Boolean;
    FAutoSizeColumnIndex: Integer;
    FMinColumnWidth: Integer;
    FMaxColumnWidth: Integer;
    FInAutoSize: Boolean;
    FSelectColumnsDialogStrings: TJvSelectDialogColumnStrings;
    FTitleColumn: TColumn;
    FOnColumnResized: TJvDBColumnResizeEvent;
    FSortMarker: TSortMarker;
    FShowCellHint: Boolean;
    FOnShowCellHint: TJvCellHintEvent;
    FCharList: TCharList;
    {$IFDEF COMPILER9_UP}
    FScrollBars: TScrollStyle;
    {$ENDIF COMPILER9_UP}
    FWordWrap: Boolean;
    FWordWrapAllFields: Boolean;
    FChangeLinks: TObjectList;
    FShowMemos: Boolean;
    FOnShowEditor: TJvDBEditShowEvent;
    FAlwaysShowEditor: Boolean;

    FControls: TJvDBGridControls;
    FCurrentControl: TWinControl;
    FOldControlWndProc: TWndMethod;
    FBooleanFieldToEdit: TField;
    FBooleanEditor: Boolean;
    FOnCheckIfBooleanField: TJvDBCheckIfBooleanFieldEvent;
    FStringForTrue: string;
    FStringForFalse: string;

    FAutoSizeRows: Boolean;
    FRowResize: Boolean;
    FRowsHeight: Integer;
    FTitleRowHeight: Integer;
    FCellHintPosition: TJvDBGridCellHintPosition;
    FCanDelete: Boolean;

    { Cancel edited record on mouse wheel or when resize column (double-click)}
    FCancelOnMouse: Boolean;

    { Resize column using mouse double clicking }
    FCanResizeColumn: Boolean;
    FResizeColumnIndex: Longint;
    FColumnResize: TJvDBGridColumnResize;

    // XP Theming
    {$IFNDEF COMPILER14_UP}
    FUseXPThemes: Boolean;
    {$ENDIF ~COMPILER14_UP}
    FUseThemedHighlighting: Boolean;
    FPaintInfo: TJvGridPaintInfo;
    FCell: TGridCoord; // currently selected cell

    FTitleButtonAllowMove: Boolean;
    FReadOnlyCellColor: TColor;
    FOnCanEditCell: TJvDBCanEditCellEvent;
    FOnSelectColumns: TJvDBSelectColumnsEvent;
    FOnBeforePaint: TNotifyEvent;
    FOnAfterPaint: TNotifyEvent;

    FDelphi2010OptionsMigrated: Boolean;
    procedure ReadDelphi2010OptionsMigrated(Reader: TReader);
    procedure WriteDelphi2010OptionsMigrated(Writer: TWriter);

    {$IFDEF COMPILER10_UP}
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    {$ENDIF COMPILER10_UP}
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

    procedure SetAutoSizeRows(Value: Boolean);
    procedure SetRowResize(Value: Boolean);
    procedure SetRowsHeight(Value: Integer);
    procedure SetTitleRowHeight(Value: Integer);

    procedure WriteCellText(ARect: TRect; DX, DY: Integer; const Text: string;
      Alignment: TAlignment; ARightToLeft: Boolean; FixCell: Boolean; Options: Integer = 0);
    function GetImageIndex(Field: TField): Integer;
    procedure SetShowGlyphs(Value: Boolean);
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
    function GetRow: Longint;
    procedure SetRow(Value: Longint);
    procedure SaveColumnsLayout(const AppStorage: TJvCustomAppStorage; const Section: string);
    procedure RestoreColumnsLayout(const AppStorage: TJvCustomAppStorage; const Section: string);
    function GetOptions: TDBGridOptions;
    procedure SetOptions(Value: TDBGridOptions);
    function GetMasterColumn(ACol, ARow: Longint): TColumn;
    function GetTitleOffset: Integer;
    procedure SetFixedCols(Value: Integer);
    function GetFixedCols: Integer;
    function CalcLeftColumn: Integer;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure WMCancelMode(var Msg: TMessage); message WM_CANCELMODE;
    procedure WMRButtonUp(var Msg: TWMMouse); message WM_RBUTTONUP;
    procedure CMHintShow(var Msg: TCMHintShow); message CM_HINTSHOW;
    procedure SetTitleArrow(const Value: Boolean);
    procedure SetAlternateRowColor(const Value: TColor);
    procedure ReadAlternateRowColor(Reader: TReader);
    procedure SetAlternateRowFontColor(const Value: TColor);
    procedure ReadAlternateRowFontColor(Reader: TReader);
    procedure SetAutoSizeColumnIndex(const Value: Integer);
    procedure SetAutoSizeColumns(const Value: Boolean);
    procedure SetMaxColumnWidth(const Value: Integer);
    procedure SetMinColumnWidth(const Value: Integer);
    procedure SetSelectColumnsDialogStrings(const Value: TJvSelectDialogColumnStrings);
    procedure SetSortedField(const Value: string);
    procedure SetSortMarker(const Value: TSortMarker);
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure SetShowMemos(const Value: Boolean);
    procedure SetBooleanEditor(const Value: Boolean);
    {$IFDEF COMPILER9_UP}
    procedure SetScrollBars(Value: TScrollStyle);
    {$ENDIF COMPILER9_UP}
    procedure ReadPostOnEnter(Reader: TReader);

    procedure SetControls(Value: TJvDBGridControls);
    procedure HideCurrentControl;
    procedure ControlWndProc(var Message: TMessage);
    procedure ChangeBoolean(const FieldValueChange: Shortint);
    function EditWithBoolBox(Field: TField): Boolean; {$IFDEF DELPHI9} inline; {$ENDIF DELPHI9}
    function DoKeyPress(var Msg: TWMChar): Boolean;
    procedure SetWordWrap(Value: Boolean);
    procedure SetWordWrapAllFields(Value: Boolean);
    procedure NotifyLayoutChange(const Kind: TJvDBGridLayoutChangeKind);

    // XP Theming
    function GetUseXPThemes: Boolean;
    procedure SetUseXPThemes(Value: Boolean);
    {$IFNDEF COMPILER14_UP}
    {$IFDEF JVCLThemesEnabled}
    function ColumnOffset: Integer; // col offset used for calculations. Is 1 if indicator is being displayed
    function ValidCell(ACell: TGridCoord): Boolean;
    {$ENDIF JVCLThemesEnabled}
    {$ENDIF ~COMPILER14_UP}

    function GetMaxDisplayText: string;
    function GetColumnMaxWidth: Integer;
  protected
    FCurrentDrawRow: Integer;
    procedure MouseLeave(Control: TControl); override;
    function AcquireFocus: Boolean;
    function CanEditShow: Boolean; override;
    function CanEditCell(AField: TField): Boolean; virtual;
    function CreateEditor: TInplaceEdit; override;
    procedure DblClick; override;
    function DoTitleBtnDblClick: Boolean; dynamic;
    procedure ShowSelectColumnClick; dynamic;

    procedure DoTitleClick(ACol: Longint; AField: TField); dynamic;
    procedure CheckTitleButton(ACol, ARow: Longint; var Enabled: Boolean); dynamic;
    function SortMarkerAssigned(const AFieldName: string): Boolean; dynamic;
    function ChangeSortMarker(const Value: TSortMarker): Boolean;
    procedure CallDrawCellEvent(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
    procedure DrawTitleCaption(Canvas: TCanvas; const TextRect: TRect; DrawColumn: TColumn);
    procedure DoDrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); virtual;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure DrawDataCell(const Rect: TRect; Field: TField; State: TGridDrawState); override; { obsolete from Delphi 2.0 }
    function DrawThemedHighlighting(ACanvas: TCanvas; R: TRect): Boolean;
    function GetPaintInfo: TJvGridPaintInfo;

    function BeginColumnDrag(var Origin: Integer; var Destination: Integer; const MousePt: TPoint): Boolean; override;
    procedure ColumnMoved(FromIndex: Integer; ToIndex: Integer); override;
    function AllowTitleClick: Boolean; virtual;

    procedure EditChanged(Sender: TObject); dynamic;
    procedure GetCellProps(Column: TColumn; AFont: TFont; var Background: TColor;
      Highlight: Boolean); dynamic;
    function HighlightCell(DataCol, DataRow: Integer; const Value: string;
      AState: TGridDrawState): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure SetColumnAttributes; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure Scroll(Distance: Integer); override;
    procedure LinkActive(Value: Boolean); override;
    {$IFDEF COMPILER9_UP}
    procedure UpdateScrollBar; override;
    {$ENDIF COMPILER9_UP}
    procedure LayoutChanged; override;
    procedure TopLeftChanged; override;
    procedure GridInvalidateRow(Row: Longint);
    procedure DrawColumnCell(const Rect: TRect; DataCol: Integer;
      Column: TColumn; State: TGridDrawState); override;
    procedure ColWidthsChanged; override;
    function DoEraseBackground(Canvas: TCanvas; Param: LPARAM): Boolean; override;
    procedure Paint; override;
    procedure CalcSizingState(X, Y: Integer; var State: TGridState;
      var Index: Longint; var SizingPos, SizingOfs: Integer;
      var FixedInfo: TGridDrawInfo); override;
    procedure DoDrawColumnTitle(ACanvas: TCanvas; ARect: TRect; AColumn: TColumn;
      var ASortMarker: TJvDBGridBitmap; IsDown: Boolean; var Offset: Integer;
      var DefaultDrawText, DefaultDrawSortMarker: Boolean); virtual;
    procedure ColEnter; override;
    procedure ColExit; override;

    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure EditButtonClick; override;
    procedure CellClick(Column: TColumn); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoMinColWidth; virtual;
    procedure DoMaxColWidth; virtual;
    procedure DoAutoSizeColumns; virtual;
    procedure Resize; override;
    procedure Loaded; override;
    function GetMinColWidth(Default: Integer): Integer;
    function GetMaxColWidth(Default: Integer): Integer;
    function LastVisibleColumn: Integer;
    function FirstVisibleColumn: Integer;
    procedure TitleClick(Column: TColumn); override;
    procedure DoGetBtnParams(Field: TField; AFont: TFont; var Background: TColor;
      var ASortMarker: TSortMarker; IsDown: Boolean); virtual;

    procedure PlaceControl(Control: TWinControl; ACol, ARow: Integer); virtual;
    procedure RowHeightsChanged; override;
    function GetDataLink: TDataLink; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure CreateParams(var Params: TCreateParams); override;
  public
    {$IFDEF SUPPORTS_CLASS_CTORDTORS}
    class destructor Destroy;
    {$ENDIF SUPPORTS_CLASS_CTORDTORS}

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DefaultDrawColumnCell(const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState); virtual;
    procedure DefaultDataCellDraw(const Rect: TRect; Field: TField; State: TGridDrawState);
    procedure DisableScroll;
    procedure EnableScroll;
    function ScrollDisabled: Boolean;
    procedure MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
    procedure SelectAll;
    procedure UnselectAll;
    procedure ToggleRowSelection;
    procedure GotoSelection(Index: Longint);
    procedure LoadFromAppStore(const AppStorage: TJvCustomAppStorage; const Path: string);
    procedure SaveToAppStore(const AppStorage: TJvCustomAppStorage; const Path: string);
    procedure Load;
    procedure Save;
    procedure UpdateTabStops(ALimit: Integer = -1);
    procedure ShowColumnsDialog;
    procedure CloseControl; // Hide the current edit control and give the focus to the grid
    procedure InitializeColumnsWidth(const MinWidth, MaxWidth: Integer;
      const DisplayWholeTitle: Boolean; const FixedWidths: array of Integer);
    procedure MouseWheelHandler(var Message: TMessage); override;

    procedure RegisterLayoutChangeLink(Link: TJvDBGridLayoutChangeLink);
    procedure UnregisterLayoutChangeLink(Link: TJvDBGridLayoutChangeLink);

    procedure BeginUpdate;
    procedure EndUpdate;
    function CellRect(ACol, ARow: Longint): TRect;

    property SelectedRows;
    property SelCount: Longint read GetSelCount;
    property Canvas;
    property Col;
    property InplaceEditor;
    property LeftCol;
    property Row: Longint read GetRow write SetRow;
    property CurrentDrawRow: Integer read FCurrentDrawRow;
    property VisibleRowCount;
    property VisibleColCount;
    property IndicatorOffset;
    property TitleOffset: Integer read GetTitleOffset;
    property CharList: TCharList read FCharList write FCharList;
  published
    property AutoAppend: Boolean read FAutoAppend write FAutoAppend default True;
    property SortMarker: TSortMarker read FSortMarker write SetSortMarker default smNone;
    property AutoSort: Boolean read FAutoSort write FAutoSort default True;
    property Options: TDBGridOptions read GetOptions write SetOptions default DefJvGridOptions;
    property FixedCols: Integer read GetFixedCols write SetFixedCols default 0;
    property ClearSelection: Boolean read FClearSelection write FClearSelection default True;
    property DefaultDrawing: Boolean read FDefaultDrawing write FDefaultDrawing default True;
    property IniStorage: TJvFormPlacement read GetStorage write SetStorage;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property ShowGlyphs: Boolean read FShowGlyphs write SetShowGlyphs default True;
    property TitleButtons: Boolean read FTitleButtons write SetTitleButtons default False;
    property TitleButtonAllowMove: Boolean read FTitleButtonAllowMove write FTitleButtonAllowMove default False; 
    property OnCheckButton: TCheckTitleBtnEvent read FOnCheckButton write FOnCheckButton;
    property OnGetCellProps: TGetCellPropsEvent read FOnGetCellProps write FOnGetCellProps; { obsolete }
    property OnGetCellParams: TGetCellParamsEvent read FOnGetCellParams write FOnGetCellParams;
    property OnGetBtnParams: TGetBtnParamsEvent read FOnGetBtnParams write FOnGetBtnParams;
    property OnEditChange: TNotifyEvent read FOnEditChange write FOnEditChange;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property OnShowEditor: TJvDBEditShowEvent read FOnShowEditor write FOnShowEditor;
    property OnTitleBtnClick: TTitleClickEvent read FOnTitleBtnClick write FOnTitleBtnClick;
    property OnTitleBtnDblClick: TTitleClickEvent read FOnTitleBtnDblClick write FOnTitleBtnDblClick;
    property OnTopLeftChanged: TNotifyEvent read FOnTopLeftChanged write FOnTopLeftChanged;
    property OnDrawColumnTitle: TDrawColumnTitleEvent read FOnDrawColumnTitle write FOnDrawColumnTitle;
    property OnContextPopup;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property BeepOnError: Boolean read FBeepOnError write FBeepOnError default True;
    property AlternateRowColor: TColor read FAlternateRowColor write SetAlternateRowColor default clNone;
    property AlternateRowFontColor: TColor read FAlternateRowFontColor write SetAlternateRowFontColor default clNone;
    property PostOnEnterKey: Boolean read FPostOnEnterKey write FPostOnEnterKey default False;
    {$IFDEF COMPILER9_UP}
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    {$ENDIF COMPILER9_UP}
    property SelectColumn: TSelectColumn read FSelectColumn write FSelectColumn default scDataBase;
    property SortedField: string read FSortedField write SetSortedField;
    property ShowTitleHint: Boolean read FShowTitleHint write FShowTitleHint default False;
    property TitleArrow: Boolean read FTitleArrow write SetTitleArrow default False;
    property TitlePopup: TPopupMenu read FTitlePopup write FTitlePopup;
    property OnShowTitleHint: TJvTitleHintEvent read FOnShowTitleHint write FOnShowTitleHint;
    property OnTitleArrowMenuEvent: TNotifyEvent read FOnTitleArrowMenuEvent write FOnTitleArrowMenuEvent;
    property ShowCellHint: Boolean read FShowCellHint write FShowCellHint default False;
    property OnShowCellHint: TJvCellHintEvent read FOnShowCellHint write FOnShowCellHint;
    property MaxColumnWidth: Integer read FMaxColumnWidth write SetMaxColumnWidth default 0;
    property MinColumnWidth: Integer read FMinColumnWidth write SetMinColumnWidth default 0;
    property AutoSizeColumns: Boolean read FAutoSizeColumns write SetAutoSizeColumns default False;
    property AutoSizeColumnIndex: Integer read FAutoSizeColumnIndex write SetAutoSizeColumnIndex
      default JvGridResizeProportionally;
    property SelectColumnsDialogStrings: TJvSelectDialogColumnStrings
      read FSelectColumnsDialogStrings write SetSelectColumnsDialogStrings;

    { Determines how cell hint position is calculated, check TJvDBGrid.CMHintShow (Mantis #5759) }
    property CellHintPosition: TJvDBGridCellHintPosition read FCellHintPosition write FCellHintPosition default gchpDefault;

    { Allows user to delete things using the "del" key }
    property CanDelete: Boolean read FCanDelete write FCanDelete default True;

    { CancelOnMouse: cancel current record when using mouse wheel or on column resizing using double-click }
    property CancelOnMouse: Boolean read FCancelOnMouse write FCancelOnMouse default False;
    { ColumnResize: columns can be resized on max Field.DisplayText using mouse double clicking }
    property ColumnResize: TJvDBGridColumnResize read FColumnResize write FColumnResize default gcrGrid;

    { EditControls: list of controls used to edit data }
    property EditControls: TJvDBGridControls read FControls write SetControls;
    { AutoSizeRows: are rows resized automatically ? }
    property AutoSizeRows: Boolean read FAutoSizeRows write SetAutoSizeRows default True;
    { ReduceFlicker: improve (but slow) the display when painting/scrolling ? }
    property ReduceFlicker: Boolean read FReduceFlicker write FReduceFlicker default True;
    { RowResize: can rows be resized with the mouse ? }
    property RowResize: Boolean read FRowResize write SetRowResize default False;
    { RowsHeight: data rows height }
    property RowsHeight: Integer read FRowsHeight write SetRowsHeight;
    { TitleRowHeight: title row height (cannot be resized with the mouse) }
    property TitleRowHeight: Integer read FTitleRowHeight write SetTitleRowHeight;
    { WordWrap: if true, titles, memo and string fields are displayed on several lines }
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    { WordWrapAllFields: if true and WordWrap is true, not only memo and string fields are displayed on several lines }
    property WordWrapAllFields: Boolean read FWordWrapAllFields write SetWordWrapAllFields default False;
    { ShowMemos: if true, memo fields are shown as text }
    property ShowMemos: Boolean read FShowMemos write SetShowMemos default True;
    { BooleanEditor: if true, a checkbox is used to edit boolean fields }
    property BooleanEditor: Boolean read FBooleanEditor write SetBooleanEditor default True;
    { UseXPThemes: if true, the grid is painted in the active XP theme style }
    property UseXPThemes: Boolean read GetUseXPThemes write SetUseXPThemes {$IFDEF COMPILER14_UP} stored False{$ENDIF} default True;
    { UseThemedHighlighting: if true, the grid's cell selection is painted with the styling color }
    property UseThemedHighlighting: Boolean read FUseThemedHighlighting write FUseThemedHighlighting default True;
    { OnCheckIfBooleanField: event used to treat integer fields and string fields as boolean fields }
    property OnCheckIfBooleanField: TJvDBCheckIfBooleanFieldEvent read FOnCheckIfBooleanField write FOnCheckIfBooleanField;
    { OnColumnResized: event triggered each time a column is resized with the mouse }
    property OnColumnResized: TJvDBColumnResizeEvent read FOnColumnResized write FOnColumnResized;

    { ReadOnlyCellColor: The color of the cells that are read only => OnCanEditCell, not Field.CanModify }  
    property ReadOnlyCellColor: TColor read FReadOnlyCellColor write FReadOnlyCellColor default clDefault;
    { OnCanEditCell: event used to control the appearance of editor and cell background }
    property OnCanEditCell: TJvDBCanEditCellEvent read FOnCanEditCell write FOnCanEditCell;
    { OnSelectColumns: event is triggered when the user clicks on the TitleArrow button. }
    property OnSelectColumns: TJvDBSelectColumnsEvent read FOnSelectColumns write FOnSelectColumns;

    { OnBeforePaint: event triggered before the grid is painted. }
    property OnBeforePaint: TNotifyEvent read FOnBeforePaint write FOnBeforePaint;
    { OnBeforePaint: event triggered after the grid was painted. }
    property OnAfterPaint: TNotifyEvent read FOnAfterPaint write FOnAfterPaint;
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
  Variants, SysUtils, Math, TypInfo, Dialogs, DBConsts, StrUtils,
  JvDBLookup,
  JvConsts, JvResources, JvThemes, JvJCLUtils, JvJVCLUtils,
  {$IFDEF COMPILER16_UP}
  Themes,
  {$ENDIF COMPILER16_UP}
  {$IFDEF COMPILER7_UP}
  // => TScrollDirection, DrawArray(must be after JvJVCLUtils)
  {$ENDIF COMPILER7_UP}
  JvDBGridSelectColumnForm, JclSysUtils;

{$R JvDBGrid.res}

type
  TBookmarks = class(TBookmarkList);
  TGridPicture = (gpBlob, gpMemo, gpPicture, gpOle, gpObject, gpData,
    gpNotEmpty, gpMarkDown, gpMarkUp, gpChecked, gpUnChecked, gpPopup);
  {$IFNDEF COMPILER7_UP}
  TScrollDirection = (sdLeft, sdRight, sdUp, sdDown);
  {$ENDIF ~COMPILER7_UP}

  TCustomGridAccess = class(TCustomGrid);

const
  GridBmpNames: array [TGridPicture] of PChar =
  ('JvDBGridBLOB', 'JvDBGridMEMO', 'JvDBGridPICT', 'JvDBGridOLE', 'JvDBGridOBJECT',
    'JvDBGridDATA', 'JvDBGridNOTEMPTY', 'JvDBGridSMDOWN', 'JvDBGridSMUP',
    'JvDBGridCHECKED', 'JvDBGridUNCHECKED', 'JvDBGridPOPUP');

  bmMultiDot = 'JvDBGridMSDOT';
  bmMultiArrow = 'JvDBGridMSARROW';

  // Consts for ChangeBoolean
  JvGridBool_INVERT = 9;
  JvGridBool_CHECK = 0;
  JvGridBool_UNCHECK = -1;

var
  GridBitmaps: array [TGridPicture] of TJvDBGridBitmap =
    (nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil);
  FirstGridBitmaps: Boolean = True;
  MsIndicators: TImageList;

procedure FinalizeGridBitmaps;
var
  I: TGridPicture;
begin
  FreeAndNil(MsIndicators);
  for I := Low(TGridPicture) to High(TGridPicture) do
    FreeAndNil(GridBitmaps[I]);
end;

function GetGridBitmap(BmpType: TGridPicture): TJvDBGridBitmap;
begin
  if GridBitmaps[BmpType] = nil then
  begin
    if FirstGridBitmaps then
      FirstGridBitmaps := False;
    GridBitmaps[BmpType] := TJvDBGridBitmap.Create;
    GridBitmaps[BmpType].LoadFromResourceName(HInstance, GridBmpNames[BmpType]);
  end;
  Result := GridBitmaps[BmpType];
end;

function DrawBiDiText(DC: HDC; const Text: string; var R: TRect; Flags: UINT;
  Alignment: TAlignment; RightToLeft: Boolean; CanvasOrientation: TCanvasOrientation): Integer;
const
  AlignFlags: array [TAlignment] of UINT = (DT_LEFT, DT_RIGHT, DT_CENTER);
  RTL: array [Boolean] of UINT = (0, DT_RTLREADING);
begin
  if CanvasOrientation = coRightToLeft then
    ChangeBiDiModeAlignment(Alignment);
  Result := Windows.DrawText(DC, PChar(Text), Length(Text), R,
    AlignFlags[Alignment] or RTL[RightToLeft] or Flags);
end;

function IsMemoField(AField: TField): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Result := AField.DataType in [ftMemo {$IFDEF COMPILER10_UP}, ftWideMemo {$ENDIF}];
end;

//=== { TInternalInplaceEdit } ===============================================

type
  TInternalInplaceEdit = class(TInplaceEditList)
  private
    FDataList: TJvDBLookupList; //  TDBLookupListBox
    FUseDataList: Boolean;
    FLookupSource: TDataSource;
  protected
    procedure CloseUp(Accept: Boolean); override;
    procedure DoEditButtonClick; override;
    procedure DropDown; override;
    procedure UpdateContents; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
  public
    constructor Create(Owner: TComponent); override;
    property DataList: TJvDBLookupList read FDataList; //  TDBLookupListBox
    property OnChange;
  end;

constructor TInternalInplaceEdit.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FLookupSource := TDataSource.Create(Self);
end;

procedure TInternalInplaceEdit.CloseUp(Accept: Boolean);
var
  MasterField: TField;
  ListValue: Variant;
begin
  if ListVisible then
  begin
    if GetCapture <> 0 then
      SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    if ActiveList = DataList then
      ListValue := DataList.KeyValue
    else
    if PickList.ItemIndex <> -1 then
      ListValue := PickList.Items[PickList.ItemIndex]
    else
      ListValue := Null;
    SetWindowPos(ActiveList.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    ListVisible := False;
    if Assigned(FDataList) then
      FDataList.LookupSource := nil; //  ListSource
    FLookupSource.DataSet := nil;
    Invalidate;
    if Accept then
      if ActiveList = DataList then
        with TCustomDBGrid(Grid), TDBGrid(Grid).Columns[SelectedIndex].Field do
        begin
          MasterField := DataSet.FieldByName(KeyFields);
          if MasterField.CanModify and (Grid as IJvDataControl).GetDataLink.Edit then
            MasterField.Value := ListValue;
        end
      else
      if (not VarIsNull(ListValue)) and EditCanModify then
        with TCustomDBGrid(Grid), TDBGrid(Grid).Columns[SelectedIndex].Field do
          Text := ListValue;
  end;
end;

procedure TInternalInplaceEdit.DoEditButtonClick;
begin
  TJvDBGrid(Grid).EditButtonClick; //   TCustomDBGrid
end;

procedure TInternalInplaceEdit.DropDown;
var
  Column: TColumn;
begin
  if not ListVisible then
  begin
    with TDBGrid(Grid) do
      Column := Columns[SelectedIndex];
    if ActiveList = FDataList then
      with Column.Field do
      begin
        FDataList.Color := Color;
        FDataList.Font := Font;
        FDataList.RowCount := Column.DropDownRows;
        FLookupSource.DataSet := LookupDataSet;
        FDataList.LookupField := LookupKeyFields; //  KeyField
        FDataList.LookupDisplay := LookupResultField; //  ListField
        FDataList.LookupSource := FLookupSource; //  ListSource
        FDataList.KeyValue := DataSet.FieldByName(KeyFields).Value;
      end
    else
    if ActiveList = PickList then
    begin
      PickList.Items.Assign(Column.PickList);
      DropDownRows := Column.DropDownRows;
    end;
  end;
  inherited DropDown;
end;

procedure TInternalInplaceEdit.UpdateContents;
var
  Column: TColumn;
begin
  inherited UpdateContents;
  if FUseDataList then
  begin
    if FDataList = nil then
    begin
      FDataList := TJvPopupDataList.Create(Self);
      FDataList.Visible := False;
      FDataList.Parent := Self;
      FDataList.OnMouseUp := ListMouseUp;
    end;
    ActiveList := FDataList;
  end;
  with TDBGrid(Grid) do
    Column := Columns[SelectedIndex];
  Self.ReadOnly := Column.ReadOnly;
  Font.Assign(Column.Font);
  ImeMode := Column.ImeMode;
  ImeName := Column.ImeName;
end;

type
  TSelection = record
    StartPos: Integer;
    EndPos: Integer;
  end;

procedure TInternalInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);

  procedure SendToParent;
  begin
    TJvDBGrid(Grid).KeyDown(Key, Shift);
    Key := 0;
  end;

  procedure ParentEvent;
  var
    GridKeyDown: TKeyEvent;
  begin
    GridKeyDown := TJvDBGrid(Grid).OnKeyDown;
    if Assigned(GridKeyDown) then
      GridKeyDown(Grid, Key, Shift);
  end;

  function ForwardMovement: Boolean;
  begin
    Result := dgAlwaysShowEditor in TJvDBGrid(Grid).Options;
  end;

  function Ctrl: Boolean;
  begin
    Result := (Shift * KeyboardShiftStates = [ssCtrl]);
  end;

  function Selection: TSelection;
  begin
    SendMessage(Handle, EM_GETSEL, WPARAM(@Result.StartPos), LPARAM(@Result.EndPos));
  end;

  function CaretPos: Integer;
  var
    P: TPoint;
  begin
    Windows.GetCaretPos(P);
    Result := SendMessage(Handle, EM_CHARFROMPOS, 0, MakeLong(P.X, P.Y));
  end;

  function RightSide: Boolean;
  begin
    with Selection do
      Result := {(CaretPos = GetTextLen) and  }
        ((StartPos = 0) or (EndPos = StartPos)) and (EndPos = GetTextLen);
  end;

  function LeftSide: Boolean;
  begin
    with Selection do
      Result := (CaretPos = 0) and (StartPos = 0) and
        ((EndPos = 0) or (EndPos = GetTextLen));
  end;

begin
  case Key of
    VK_LEFT:
      if ForwardMovement and (Ctrl or LeftSide) then
        SendToParent;
    VK_RIGHT:
      if ForwardMovement and (Ctrl or RightSide) then
        SendToParent;
  end;
  inherited KeyDown(Key, Shift);
end;

function TInternalInplaceEdit.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  DataLink: TDataLink;
begin
  // Do not validate a record by error
  DataLink := (Grid as IJvDataControl).GetDataLink;
  if DataLink.Active and (DataLink.DataSet.State <> dsBrowse) then
    DataLink.DataSet.Cancel;

  // Ideally we would transmit the action to the DataList but
  // DoMouseWheel is protected
  //  Result := FDataList.DoMouseWheel(Shift, WheelDelta, MousePos);
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

//=== { TJvDBGridLayoutChangeLink } ==========================================

procedure TJvDBGridLayoutChangeLink.DoChange(Grid: TJvDBGrid;
  Kind: TJvDBGridLayoutChangeKind);
begin
  if Assigned(OnChange) then
    OnChange(Grid, Kind);
end;

//=== { TJvDBGridControls } ==================================================

constructor TJvDBGridControls.Create(ParentDBGrid: TJvDBGrid);
begin
  inherited Create(TJvDBGridControl);
  FParentDBGrid := ParentDBGrid;
end;

procedure TJvDBGridControl.Assign(Source: TPersistent);
begin
  if Source is TJvDBGridControl then
  begin
    ControlName := TJvDBGridControl(Source).ControlName;
    FieldName := TJvDBGridControl(Source).FieldName;
    FitCell := TJvDBGridControl(Source).FitCell;
    LeaveOnEnterKey := TJvDBGridControl(Source).LeaveOnEnterKey;
    LeaveOnUpDownKey := TJvDBGridControl(Source).LeaveOnUpDownKey;
    FDesignWidth := 0;
    FDesignHeight := 0;
  end
  else
    inherited Assign(Source);
end;

function TJvDBGridControls.GetOwner: TPersistent;
begin
  Result := FParentDBGrid;
end;

function TJvDBGridControls.Add: TJvDBGridControl;
begin
  Result := TJvDBGridControl(inherited Add);
end;

function TJvDBGridControls.GetItem(Index: Integer): TJvDBGridControl;
begin
  Result := TJvDBGridControl(inherited GetItem(Index));
end;

procedure TJvDBGridControls.SetItem(Index: Integer; Value: TJvDBGridControl);
begin
  inherited SetItem(Index, Value);
end;

function TJvDBGridControls.ControlByField(const FieldName: string): TJvDBGridControl;
var
  Ctrl_Idx: Integer;
begin
  Result := nil;
  for Ctrl_Idx := 0 to Count - 1 do
    if AnsiSameText(Items[Ctrl_Idx].FieldName, FieldName) then
    begin
      Result := Items[Ctrl_Idx];
      Break;
    end;
end;

function TJvDBGridControls.ControlByName(const CtrlName: string): TJvDBGridControl;
var
  Ctrl_Idx: Integer;
begin
  Result := nil;
  for Ctrl_Idx := 0 to Count - 1 do
    if AnsiSameText(Items[Ctrl_Idx].ControlName, CtrlName) then
    begin
      Result := Items[Ctrl_Idx];
      Break;
    end;
end;

//=== { TJvDBGrid } ==========================================================

{$IFDEF SUPPORTS_CLASS_CTORDTORS}
class destructor TJvDBGrid.Destroy;
begin
  FinalizeGridBitmaps;
end;
{$ENDIF SUPPORTS_CLASS_CTORDTORS}

constructor TJvDBGrid.Create(AOwner: TComponent);
var
  Bmp: TBitmap;
begin
  inherited Create(AOwner);
  {$IFDEF COMPILER9_UP}
  FScrollBars := ssBoth;
  {$ENDIF COMPILER9_UP}

  // (obones): issue 3026: need to create FChangeLinks at the beginning
  // so that any change can access the object. It seems that on some
  // foreign systems, the assignment to the Options property triggers
  // NotifyLayoutChange, so it needs the FChangeLinks object
  FChangeLinks := TObjectList.Create(False);

  FAutoSort := True;
  FBeepOnError := True;
  if MsIndicators = nil then
  begin
    Bmp := TBitmap.Create;
    try
      Bmp.Handle := LoadBitmap(HInstance, bmMultiDot);
      MsIndicators := TImageList.CreateSize(Bmp.Width, Bmp.Height);
      MsIndicators.AddMasked(Bmp, clWhite);
      Bmp.Handle := LoadBitmap(HInstance, bmMultiArrow);
      MsIndicators.AddMasked(Bmp, clWhite);
    finally
      Bmp.Free;
    end;
  end;
  FIniLink := TJvIniLink.Create;
  FIniLink.OnSave := IniSave;
  FIniLink.OnLoad := IniLoad;
  FShowGlyphs := True;
  FDefaultDrawing := True;
  FReduceFlicker := True;
  FClearSelection := True;
  FAutoAppend := True;
  FAlternateRowColor := clNone;
  FAlternateRowFontColor := clNone;
  FSelectColumn := scDataBase;
  FAutoSizeColumnIndex := JvGridResizeProportionally;
  FSelectColumnsDialogStrings := TJvSelectDialogColumnStrings.Create;
  // Note to users: the second line may not compile on non western european
  // systems, in which case you should simply remove it and recompile.
  FCharList :=
    ['A'..'Z', 'a'..'z', ' ', '-', '+', '0'..'9', '.', ',', Backspace,
     'é', 'è', 'ê', 'ë', 'ô', 'ö', 'û', 'ù', 'â', 'à', 'ä', 'î', 'ï', 'ç'];

  FControls := TJvDBGridControls.Create(Self);
  FBooleanEditor := True;
  FStringForTrue := '1';
  FStringForFalse := '0';

  FAutoSizeRows := True;
  FRowsHeight := DefaultRowHeight;
  FTitleRowHeight := RowHeights[0];
  FShowMemos := True;
  FCanDelete := True;

  FReadOnlyCellColor := clDefault;

  FColumnResize := gcrGrid;

  // XP Theming
  {$IFNDEF COMPILER14_UP}
  FUseXPThemes := True;
  {$ENDIF ~COMPILER14_UP}
  FUseThemedHighlighting := True;

  FPaintInfo.ColPressed := False;
  FPaintInfo.MouseInCol := -1;
  FPaintInfo.ColPressedIdx := -1;
  FPaintInfo.ColMoving := False;
  FPaintInfo.ColSizing := False;
  FCell.X := -1;
  FCell.Y := -1;

  { properties with setters }
  inherited DefaultDrawing := False;
  inherited Options := inherited Options - [dgAlwaysShowEditor];
  Options := DefJvGridOptions;
end;

destructor TJvDBGrid.Destroy;
begin
  HideCurrentControl;
  FControls.Free;

  FIniLink.Free;
  FSelectColumnsDialogStrings.Free;

  FChangeLinks.Free;

  inherited Destroy;
end;

procedure TJvDBGrid.RegisterLayoutChangeLink(Link: TJvDBGridLayoutChangeLink);
begin
  if not (csDestroying in ComponentState) then
    FChangeLinks.Add(Link);
end;

procedure TJvDBGrid.UnregisterLayoutChangeLink(Link: TJvDBGridLayoutChangeLink);
begin
  if not (csDestroying in ComponentState) then
    FChangeLinks.Remove(Link);
end;

function TJvDBGrid.EditWithBoolBox(Field: TField): Boolean;
begin
  if FBooleanEditor then
  begin
    Result := (Field.DataType = ftBoolean);
    if not Result and Assigned(FOnCheckIfBooleanField) and
      (Field.DataType in [ftSmallint, ftInteger, ftLargeint, ftWord, ftString, ftWideString,
                          ftBCD, ftFMTBCD
                          {$IFDEF COMPILER12_UP},ftLongWord, ftShortint{$ENDIF COMPILER12_UP}]) then
    begin
      FStringForTrue := '1';
      FStringForFalse := '0';
      Result := FOnCheckIfBooleanField(Self, Field, FStringForTrue, FStringForFalse);
    end;
  end
  else
    Result := False;
end;

function TJvDBGrid.GetImageIndex(Field: TField): Integer;
begin
  Result := -1;
  if FShowGlyphs and Assigned(Field) then
  begin
    case Field.DataType of
      ftBytes, ftVarBytes, ftBlob, ftTypedBinary:
        Result := Ord(gpBlob);
      ftGraphic:
        Result := Ord(gpPicture);
      ftParadoxOle, ftDBaseOle:
        Result := Ord(gpOle);
      ftCursor, ftReference, ftDataSet:
        Result := Ord(gpData);
      {$IFDEF COMPILER10_UP}ftWideMemo,{$ENDIF COMPILER10_UP}
      ftMemo, ftFmtMemo:
        if not ShowMemos then
          Result := Ord(gpMemo);
      ftOraBlob, ftOraClob:
        Result := Ord(gpBlob);
      ftBoolean:
        if BooleanEditor and not Field.IsNull then
          if Field.AsBoolean then
            Result := Ord(gpChecked)
          else
            Result := Ord(gpUnChecked);
      {$IFDEF COMPILER10_UP}
      ftFixedWideChar,
      {$ENDIF COMPILER10_UP}
      ftString, ftWideString:
        if EditWithBoolBox(Field) and not Field.IsNull then
          if AnsiSameText(Field.AsString, FStringForFalse) then
            Result := Ord(gpUnChecked)
          else
            Result := Ord(gpChecked);
      {$IFDEF COMPILER12_UP}
      ftLongWord, ftShortint,
      {$ENDIF COMPILER12_UP}
      ftSmallint, ftInteger, ftLargeint, ftWord, ftBCD, ftFMTBCD:
        if EditWithBoolBox(Field) and not Field.IsNull then
          if Field.AsInteger = 0 then
            Result := Ord(gpUnChecked)
          else
            Result := Ord(gpChecked);
    end;
  end;
end;

function TJvDBGrid.ActiveRowSelected: Boolean;
var
  Index: Integer;
begin
  if MultiSelect and DataLink.Active then
    Result := SelectedRows.Find(DataLink.DataSet.Bookmark, Index)
  else
    Result := False;
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
  if MultiSelect and DataLink.Active then
    with SelectedRows do
      CurrentRowSelected := not CurrentRowSelected;
end;

function TJvDBGrid.GetSelCount: Longint;
begin
  if MultiSelect and (DataLink <> nil) and DataLink.Active then
    Result := SelectedRows.Count
  else
    Result := 0;
end;

function TJvDBGrid.GetRow: Longint;
begin
  Result := inherited Row;
end;

procedure TJvDBGrid.SetRow(Value: Longint);
begin
  if Value <> Row then
  begin
    if DataLink.Active and (Value >= TopRow) and (Value <= VisibleRowCount) then
      DataLink.DataSet.MoveBy(Value - Row)
    else
    if FBeepOnError then
      SysUtils.Beep;
  end;
end;

procedure TJvDBGrid.SelectAll;
var
  LBookmark: TBookmark;
begin
  if MultiSelect and DataLink.Active then
  begin
    with DataLink.DataSet do
    begin
      if Bof and Eof then
        Exit;
      DisableControls;
      try
        LBookmark := GetBookmark;
        try
          First;
          while not Eof do
          begin
            SelectedRows.CurrentRowSelected := True;
            Next;
          end;
        finally
          try
            GotoBookmark(LBookmark);
          except
          end;
          FreeBookmark(LBookmark);
        end;
      finally
        EnableControls;
      end;
    end;
  end;
end;

procedure TJvDBGrid.UnselectAll;
begin
  if MultiSelect then
  begin
    SelectedRows.Clear;
    FSelecting := False;
  end;
end;

procedure TJvDBGrid.GotoSelection(Index: Longint);
begin
  if MultiSelect and DataLink.Active and (Index < SelectedRows.Count) and
    (Index >= 0) then
    DataLink.DataSet.GotoBookmark(Pointer(SelectedRows[Index]));
end;

procedure TJvDBGrid.LayoutChanged;
var
  ACol: Longint;
begin
  ACol := Col;
  inherited LayoutChanged;
  if DataLink.Active and (FixedCols > 0) then
    Col := Min(Max(CalcLeftColumn, ACol), ColCount - 1);
  DoMinColWidth;
  DoMaxColWidth;
  DoAutoSizeColumns;

  NotifyLayoutChange(lcLayoutChanged);
end;

procedure TJvDBGrid.NotifyLayoutChange(const Kind: TJvDBGridLayoutChangeKind);
var
  I: Integer;
begin
  // We cannot trigger DataLink.LayoutChanged nor rely on it, so we notify any linked
  // control of the layout changes by calling DoChange on the registered
  // TJvDBGridLayoutChangeLink objects
  for I := 0 to FChangeLinks.Count-1 do
    TJvDBGridLayoutChangeLink(FChangeLinks[I]).DoChange(Self, Kind);

  if FCurrentControl <> nil then
    if FCurrentControl.Visible then
      PlaceControl(FCurrentControl, Col, Row);
end;

procedure TJvDBGrid.ColWidthsChanged;

  { VCL BUGFIX:
    The TCustomDBGrid.ColWidthsChanged method invokes DataLink.LayoutChanged/DataSource.OnDataChange
    for every column, regardless if it was resized or not.

    This causes a db-aware component or an DataSource.OnDataChange event handler to
    be triggered very often even if there was no actual change. This becomes worse
    when the assigned DataSet contains many visible fields (=>columns) and the DataChange
    event is used to update details data. }
    
  procedure FixedInheritedColWidthsChanged;
  var
    I, ChangeCount: Integer;
  begin
    //inherited TCustomGrid.ColWidthsChanged;
    inherited RowHeightsChanged; // does the same that TCustomGrid.ColWidthsChanged does.

    if (Datalink.Active or (Columns.State = csCustomized)) and
      AcquireLayoutLock then
    try
      ChangeCount := 0;
      for I := IndicatorOffset to ColCount - 1 do
        if Columns[I - IndicatorOffset].Width <> ColWidths[I] then
        begin
          Inc(ChangeCount);
          if ChangeCount > 1 then // we have what we need
            Break;
        end;
      if ChangeCount > 0 then
      begin
        if ChangeCount > 1 then
          DataLink.DataSet.DisableControls;
        try
          for I := IndicatorOffset to ColCount - 1 do
            if Columns[I - IndicatorOffset].Width <> ColWidths[I] then
              Columns[I - IndicatorOffset].Width := ColWidths[I];
        finally
          if ChangeCount > 1 then
            DataLink.DataSet.EnableControls;
        end;
      end;
    finally
      EndLayout;
    end;
  end;

var
  ACol: Longint;
begin
  ACol := Col;
  FixedInheritedColWidthsChanged;
  if DataLink.Active and (FixedCols > 0) then
    Col := Min(Max(CalcLeftColumn, ACol), ColCount - 1);
  DoMinColWidth;
  DoMaxColWidth;
  DoAutoSizeColumns;
end;

function TJvDBGrid.CreateEditor: TInplaceEdit;
begin
  Result := TInternalInplaceEdit.Create(Self);
  // replace the call to default constructor :
  //  Result := inherited CreateEditor;
  TInternalInplaceEdit(Result).OnChange := EditChanged;
end;

function TJvDBGrid.GetTitleOffset: Integer;
var
  I, J: Integer;
begin
  Result := 0;
  if dgTitles in Options then
  begin
    Result := 1;
    if (DataLink <> nil) and (DataLink.DataSet <> nil) and DataLink.DataSet.ObjectView then
      for I := 0 to Columns.Count - 1 do
      begin
        if Columns[I].Showing then
        begin
          J := Columns[I].Depth;
          if J >= Result then
            Result := J + 1;
        end;
      end;
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
      TabStops[I + IndicatorOffset - 1] := False;
  end;
  FFixedCols := FixCount - IndicatorOffset;
end;

function TJvDBGrid.GetFixedCols: Integer;
begin
  if DataLink.Active then
    Result := inherited FixedCols - IndicatorOffset
  else
    Result := FFixedCols;
end;

function TJvDBGrid.CalcLeftColumn: Integer;
begin
  Result := FixedCols + IndicatorOffset;
  while (Result < ColCount) and (ColWidths[Result] <= 0) do
    Inc(Result);
end;

procedure TJvDBGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  KeyDownEvent: TKeyEvent;

  procedure ClearSelections;
  begin
    if FMultiSelect then
    begin
      if FClearSelection then
        SelectedRows.Clear;
      FSelecting := False;
    end;
  end;

  procedure DoSelection(Select: Boolean; Direction: Integer);
  var
    AddAfter: Boolean;
  begin
    AddAfter := False;
    if MultiSelect and DataLink.Active then
      if Select and (Shift * KeyboardShiftStates = [ssShift]) then
      begin
        if not FSelecting then
        begin
          FSelectionAnchor := TBookmarks(SelectedRows).CurrentRow;
          SelectedRows.CurrentRowSelected := True;
          FSelecting := True;
          AddAfter := True;
        end
        else
        begin
          with TBookmarks(SelectedRows) do
          begin
            AddAfter := Compare(CurrentRow, FSelectionAnchor) <> -Direction;
            if not AddAfter then
              CurrentRowSelected := False;
          end;
        end;
      end
      else
        ClearSelections;
    if Direction <> 0 then
      DataLink.DataSet.MoveBy(Direction);
    if AddAfter then
      SelectedRows.CurrentRowSelected := True;
  end;

  procedure NextRow(Select: Boolean);
  begin
    with DataLink.DataSet do
    begin
      DoSelection(Select, 1);
      if AutoAppend and Eof and CanModify and not ReadOnly and (dgEditing in Options) then
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
    //BeginUpdate;
    inherited BeginUpdate;
    try
      while True do
      begin
        if GoForward then
          Inc(ACol)
        else
          Dec(ACol);
        if ACol >= ColCount then
        begin
          if MultiSelect then
            ClearSelections;
          NextRow(False);
          ACol := IndicatorOffset;
        end
        else if ACol < IndicatorOffset then
        begin
          if MultiSelect then
            ClearSelections;
          PriorRow(False);
          ACol := ColCount - IndicatorOffset;
        end;
        if ACol = Original then
          Exit;
        if TabStops[ACol] then
        begin
          //MoveCol(ACol, 0);
          SelectedIndex := ACol - IndicatorOffset;
          Break;
        end;
      end;
    finally
      inherited EndUpdate;
      //EndUpdate; // => EndLayout => ... => Initialize => Col := FixedCols + 1
    end;
  end;

  function DeletePrompt: Boolean;
  var
    S: string;
  begin
    if SelectedRows.Count > 1 then
      S := SDeleteMultipleRecordsQuestion
    else
      S := SDeleteRecordQuestion;
    Result := not (dgConfirmDelete in Options) or
      (MessageDlg(S, mtConfirmation, [mbYes, mbNo], 0) = mrYes);
  end;

begin
  KeyDownEvent := OnKeyDown;
  if Assigned(KeyDownEvent) then
    KeyDownEvent(Self, Key, Shift);
  if not DataLink.Active or not CanGridAcceptKey(Key, Shift) then
    Exit;
  with DataLink.DataSet do
    if ssCtrl in Shift then
    begin
      if Key in [VK_UP, VK_PRIOR, VK_DOWN, VK_NEXT, VK_HOME, VK_END] then
        ClearSelections;
      case Key of
        VK_LEFT:
          if FixedCols > 0 then
          begin
            SelectedIndex := CalcLeftColumn - IndicatorOffset;
            Exit;
          end;
        VK_DELETE:
          if CanDelete and not ReadOnly and CanModify and not
            IsDataSetEmpty(DataLink.DataSet) then
          begin
            if DeletePrompt then
            begin
              if SelectedRows.Count > 0 then
                SelectedRows.Delete
              else
                Delete;
            end;
            Exit;
          end
          else
          begin
            // Mantis 4231: Do not pass delete to inherited grid as it would
            // allow deleting the row while having CanDelete set to False. 
            Exit;
          end;
      end;
    end
    else
    begin
      case Key of
        VK_LEFT:
          if (FixedCols > 0) and not (dgRowSelect in Options) then
            if SelectedIndex <= CalcLeftColumn - IndicatorOffset then
              Exit;
        VK_HOME:
          if (FixedCols > 0) and (ColCount <> IndicatorOffset + 1) and
            not (dgRowSelect in Options) then
          begin
            SelectedIndex := CalcLeftColumn - IndicatorOffset;
            Exit;
          end;
      end;
      if DataLink.DataSet.State <> dsInsert then
        case Key of
          VK_UP:
            begin
              PriorRow(True);
              Exit;
            end;
          VK_DOWN:
            begin
              NextRow(True);
              Exit;
            end;
        end;
      if ((Key in [VK_LEFT, VK_RIGHT]) and (dgRowSelect in Options)) or
        ((Key in [VK_HOME, VK_END]) and ((ColCount = IndicatorOffset + 1) or
        (dgRowSelect in Options))) or (Key in [VK_ESCAPE, VK_NEXT, VK_PRIOR]) or
        ((Key = VK_INSERT) and CanModify and (not ReadOnly) and (dgEditing in Options)) then
        ClearSelections
      else
      if (Key = VK_TAB) and not (ssAlt in Shift) then
      begin
        CheckTab(not (ssShift in Shift));
        Exit;
      end;
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
  if FShowGlyphs <> Value then
  begin
    FShowGlyphs := Value;
    Invalidate;
  end;
end;

procedure TJvDBGrid.SetAutoSizeRows(Value: Boolean);
begin
  if FAutoSizeRows <> Value then
  begin
    FAutoSizeRows := Value;
    if FAutoSizeRows then
    begin
      RowResize := False;
      LayoutChanged; // Recalculate DefaultRowHeight
    end;
  end;
end;

procedure TJvDBGrid.SetRowsHeight(Value: Integer);
begin
  if (DefaultRowHeight <> Value) and not AutoSizeRows then
  begin
    FRowsHeight := Value;
    DefaultRowHeight := Value;
    if dgTitles in Options then
      RowHeights[0] := TitleRowHeight;
    if HandleAllocated then
      Perform(WM_SIZE, SIZE_RESTORED, MakeLong(ClientWidth, ClientHeight));
  end
  else
    FRowsHeight := DefaultRowHeight;
end;

procedure TJvDBGrid.SetTitleRowHeight(Value: Integer);
begin
  if (dgTitles in Options) and (RowHeights[0] <> Value) and not AutoSizeRows then
  begin
    FTitleRowHeight := Value;
    RowHeights[0] := Value;
    if HandleAllocated then
      Perform(WM_SIZE, SIZE_RESTORED, MakeLong(ClientWidth, ClientHeight));
  end
  else
    FTitleRowHeight := RowHeights[0];
end;

procedure TJvDBGrid.RowHeightsChanged;
var
  RowIdx,
  FirstRow: Integer;
begin
  if DefaultRowHeight <> RowsHeight then
    SetRowsHeight(RowsHeight);
  if RowHeights[0] <> TitleRowHeight then
    SetTitleRowHeight(TitleRowHeight);

  if RowResize then
  begin
    if dgTitles in Options then
      FirstRow := 1
    else
      FirstRow := 0;
    for RowIdx := FirstRow to VisibleRowCount + 1 do
      if RowHeights[RowIdx] <> RowsHeight then
      begin
        SetRowsHeight(RowHeights[RowIdx]);
        Break;
      end;
  end;

  inherited RowHeightsChanged;
end;

function TJvDBGrid.GetDataLink: TDataLink;
begin
  Result := DataLink;
end;

function TJvDBGrid.GetPaintInfo: TJvGridPaintInfo;
begin
  Result := FPaintInfo;
end;

procedure TJvDBGrid.SetRowResize(Value: Boolean);
begin
  if FRowResize <> Value then
  begin
    if AutoSizeRows then
      FRowResize := False
    else
      FRowResize := Value;
    SetOptions(Options);
  end;
end;

function TJvDBGrid.GetOptions: TDBGridOptions;
begin
  Result := inherited Options;
  if FMultiSelect then
    Result := Result + [dgMultiSelect]
  else
    Result := Result - [dgMultiSelect];

  if FAlwaysShowEditor then
    Result := Result + [dgAlwaysShowEditor]
  else
    Result := Result - [dgAlwaysShowEditor];
end;

procedure TJvDBGrid.SetOptions(Value: TDBGridOptions);
var
  NewOptions: TGridOptions;
begin
  { The AlwaysShowEditor option is not compatible with the custom inplace edit
    controls. But if the EditorMode is set to True in ColEnter() it emulates the
    AlwaysShowEditor option. }
  inherited Options := Value - [dgMultiSelect, dgAlwaysShowEditor];
  FAlwaysShowEditor := dgAlwaysShowEditor in Value;

  NewOptions := TDrawGrid(Self).Options;
  {
  if FTitleButtons then
  begin
    TDrawGrid(Self).Options := NewOptions + [goFixedHorzLine, goFixedVertLine];
  end
  else
  }
  begin
    if RowResize then
      Include(NewOptions, goRowSizing)
    else
      Exclude(NewOptions, goRowSizing);
    if not (dgColLines in Value) then
      NewOptions := NewOptions - [goFixedVertLine];
    if not (dgRowLines in Value) then
      NewOptions := NewOptions - [goFixedHorzLine];
    TDrawGrid(Self).Options := NewOptions;
  end;
  SetMultiSelect(dgMultiSelect in Value);
end;

function TJvDBGrid.DoEraseBackground(Canvas: TCanvas; Param: LPARAM): Boolean;
var
  R: TRect;
  Size: TSize;
begin
  if Param = 0 then // don't optimize for painting child control backgrounds
  begin
    { Fill the area between the two scroll bars. }
    Size.cx := GetSystemMetrics(SM_CXVSCROLL);
    Size.cy := GetSystemMetrics(SM_CYHSCROLL);
    if UseRightToLeftAlignment then
      R := Bounds(0, Height - Size.cy, Size.cx, Size.cy)
    else
      R := Bounds(Width - Size.cx, Height - Size.cy, Size.cx, Size.cy);
    Canvas.Brush.Color := Color;
    Canvas.FillRect(R);

    Result := True;
  end
  else
    Result := inherited DoEraseBackground(Canvas, Param);
end;

procedure TJvDBGrid.Paint;
begin
  if Assigned(FOnBeforePaint) then
    FOnBeforePaint(Self);
  {$IFNDEF COMPILER14_UP}
  {$IFDEF JVCLThemesEnabled}
  if UseXPThemes and StyleServices.Enabled then
  begin
    // reset the inherited options but remove the goFixedVertLine and goFixedHorzLine values
    // as that causes the titles and indicator panels to have a black border
    TStringGrid(Self).Options := TStringGrid(Self).Options - [goFixedVertLine, goFixedHorzLine];
  end;
  {$ENDIF JVCLThemesEnabled}
  {$ENDIF ~COMPILER14_UP}
  inherited Paint;
  if not (csDesigning in ComponentState) and
    (dgRowSelect in Options) and DefaultDrawing and Focused then
  begin
    Canvas.Font.Color := clWindowText;
    with Selection do
      DrawFocusRect(Canvas.Handle, BoxRect(Left, Top, Right, Bottom));
  end;
  if Assigned(FOnAfterPaint) then
    FOnAfterPaint(Self);
end;

procedure TJvDBGrid.SetTitleButtons(Value: Boolean);
begin
  if FTitleButtons <> Value then
  begin
    FTitleButtons := Value;
    Invalidate;
    SetOptions(Options);
  end;
end;

procedure TJvDBGrid.SetMultiSelect(Value: Boolean);
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;
    if not Value then
      SelectedRows.Clear;
  end;
end;

function TJvDBGrid.GetStorage: TJvFormPlacement;
begin
  Result := FIniLink.Storage;
end;

procedure TJvDBGrid.SetStorage(Value: TJvFormPlacement);
begin
  if not (csDestroying in ComponentState) then
    FIniLink.Storage := Value;
end;

function TJvDBGrid.AcquireFocus: Boolean;
begin
  Result := True;
  if FAcquireFocus and CanFocus and not (csDesigning in ComponentState) then
  begin
    SetFocus;
    Result := Focused or ((InplaceEditor <> nil) and InplaceEditor.Focused) or
                         ((FCurrentControl <> nil) and FCurrentControl.Focused);
  end;
end;

function TJvDBGrid.CanEditShow: Boolean;

  function UseDefaultEditor: Boolean;
  const
    ude_DEFAULT_EDITOR = 0;
    ude_BOOLEAN_EDITOR = 1;
    ude_CUSTOM_EDITOR = 2;
  var
    F: TField;
    Editor: Shortint;
    Control: TJvDBGridControl;
    EditControl: TWinControl;

    function IsReadOnlyField: Boolean;
    begin
      Result := ReadOnly or Columns[SelectedIndex].ReadOnly or F.ReadOnly or
        not F.DataSet.CanModify;
    end;

  begin
    // Is there an editor for the selected field ?
    F := SelectedField;
    Control := FControls.ControlByField(F.FieldName);
    if Assigned(Control) then
      Editor := ude_CUSTOM_EDITOR
    else
    if EditWithBoolBox(F) then
      Editor := ude_BOOLEAN_EDITOR
    else
    begin
      Editor := ude_DEFAULT_EDITOR;

      // The default editor cannot modify a binary or memo field
      if (F.DataType in [ftUnknown, ftBytes, ftVarBytes, ftAutoInc, ftBlob,
        ftMemo, ftFmtMemo{$IFDEF COMPILER10_UP}, ftWideMemo{$ENDIF COMPILER10_UP}, ftGraphic, ftTypedBinary, ftDBaseOle, ftParadoxOle,
        ftCursor, ftADT, ftReference, ftDataSet, ftOraBlob, ftOraClob]) then
      begin
        Result := False;
        HideCurrentControl;
        HideEditor;
        Exit;
      end;
    end;

    if not CanEditCell(F) then
    begin
      HideCurrentControl;
      HideEditor;
      Exit;
    end;

    // There is an editor, so we trigger the OnShowEditor event
    Result := True;
    if Assigned(OnShowEditor) and
      not (Assigned(InplaceEditor) and InplaceEditor.Visible) then
    begin
      // This event can be triggered twice with the default editor because of the
      // two successive calls to CanEditShow in the UpdateEdit function of Grids.pas
      OnShowEditor(Self, F, Result);
      if not Result then
      begin
        HideCurrentControl;
        HideEditor;
        Exit;
      end;
    end;

    // Is it a customized editor ?
    if Editor = ude_CUSTOM_EDITOR then
    begin
      Result := False;
      HideEditor;
      EditControl := TWinControl(Owner.FindComponent(Control.ControlName));
      if not Assigned(EditControl) then
      begin
        Control.FieldName := '';
        raise EJVCLDbGridException.CreateRes(@RsEJvDBGridControlPropertyNotAssigned);
      end;
      if IsPublishedProp(EditControl, 'ReadOnly') then
      begin
        SetOrdProp(EditControl, 'ReadOnly', Ord(IsReadOnlyField));
        PlaceControl(EditControl, Col, Row);
      end
      else
      if IsReadOnlyField then
        HideCurrentControl
      else
        PlaceControl(EditControl, Col, Row);
    end
    else
    if Editor = ude_BOOLEAN_EDITOR then
    begin
      // Boolean editor
      Result := False;
      HideCurrentControl;
      HideEditor;
      if not IsReadOnlyField then
        FBooleanFieldToEdit := F;
    end
    else
      // Default editor
      HideCurrentControl;
  end;

begin
  Result := False;
  if (inherited CanEditShow) and Assigned(SelectedField) and
    (SelectedIndex >= 0) and (SelectedIndex < Columns.Count) then
  begin
    FBooleanFieldToEdit := nil;
    Result := UseDefaultEditor;
  end
  else
  begin
    if not FAlwaysShowEditor or ([dgRowSelect, dgEditing] * Options <> [dgEditing]) then
      if HandleAllocated and not (Assigned(InplaceEditor) and InplaceEditor.Visible) then
        HideEditor;
  end;
end;

function TJvDBGrid.CanEditCell(AField: TField): Boolean;
begin
  Result := True;
  if Assigned(FOnCanEditCell) then
    FOnCanEditCell(Self, AField, Result);
end;

procedure TJvDBGrid.GetCellProps(Column: TColumn; AFont: TFont;
  var Background: TColor; Highlight: Boolean);

  function IsAfterFixedCols: Boolean;
  begin
    Result := Column.Index >= FixedCols;
  end;

begin
  if IsAfterFixedCols and (FCurrentDrawRow >= FixedRows) then
  begin
    if Odd(FCurrentDrawRow + FixedRows) then
    begin
      if (FAlternateRowColor <> clNone) and (FAlternateRowColor <> Color) then
      begin
        // Prefer the column's color
        if not ((cvColor in Column.AssignedValues) and (Column.Color <> Column.DefaultColor)) then
          Background := AlternateRowColor;
      end;
      if FAlternateRowFontColor <> clNone then
      begin
        // Prefer the column's font.color if it has a prefered color
        if not ((cvColor in Column.AssignedValues) and (Column.Color <> Column.DefaultColor)) then
          AFont.Color := AlternateRowFontColor;
      end;
    end;
  end(*
  else
    Background := FixedColor*);

  if Highlight then
  begin
    AFont.Color := clHighlightText;
    Background := clHighlight;
  end;
  if Assigned(FOnGetCellParams) then
    FOnGetCellParams(Self, Column.Field, AFont, Background, Highlight)
  else
  if Assigned(FOnGetCellProps) then
    FOnGetCellProps(Self, Column.Field, AFont, Background);
end;

procedure TJvDBGrid.DoTitleClick(ACol: Longint; AField: TField);
// Fred: This function has a few known bugs, so don't complain about them and use
// JvDBUltimGrid instead if you're looking for an improved sorting function.
const
  cIndexName = 'IndexName';
  cIndexDefs = 'IndexDefs';
  cDirection: array [Boolean] of TSortMarker = (smDown, smUp);
var
  IndexDefs: TIndexDefs;
  LIndexName: string;
  Descending: Boolean;
  IndexFound: Boolean;

  function GetIndexOf(AFieldName: string; var AIndexName: string; var Descending: Boolean): Boolean;
  var
    I: Integer;
    IsDescending: Boolean;
  begin
    Result := False;
    for I := 0 to IndexDefs.Count - 1 do
    begin
      if Pos(AFieldName, IndexDefs[I].Fields) >= 1 then
      begin
        AIndexName := IndexDefs[I].Name; // best match so far
        IsDescending := (ixDescending in IndexDefs[I].Options);
        Result := True;
        if Descending <> IsDescending then
          // we've found an index that is the opposite direction of the previous one, so we return now
        begin
          Descending := IsDescending;
          Exit;
        end;
      end;
      // if we get here and Result is True, it means we've found a matching index but it
      // might be the same as the previous one...
    end;
  end;

begin
  IndexFound := False;

  if AutoSort and IsPublishedProp(DataSource.DataSet, cIndexDefs) and
    IsPublishedProp(DataSource.DataSet, cIndexName) then
    IndexDefs := TIndexDefs(GetObjectProp(DataSource.DataSet, cIndexDefs))
  else
    IndexDefs := nil;
  if Assigned(IndexDefs) and Assigned(AField) then
  begin
    Descending := SortMarker = smUp;
    if GetIndexOf(AField.FieldName, LIndexName, Descending) then
    begin
      IndexFound := True;
      SortedField := AField.FieldName;
      SortMarker := cDirection[Descending];
      try
        SetStrProp(DataSource.DataSet, cIndexName, LIndexName);
      except
      end;
    end;
  end;
  //--------------------------------------------------------------------------
  // FBC: 2004-02-18
  // Following code handles the sortmarker if no Index is found.
  // the actual data-sorting must be implemented by the user in
  // event OnTitleBtnClick. Of course, we need a field (Mantis 3845)
  //--------------------------------------------------------------------------
  if AutoSort and not IndexFound and Assigned(AField) then
  begin
    if SortedField = AField.FieldName then
    begin
      case Self.SortMarker of
        smUp:
          Self.SortMarker := smDown;
        smDown:
          Self.SortMarker := smUp;
      end;
    end
    else
    begin
      SortedField := AField.FieldName;
      Self.SortMarker := smUp;
    end;
  end;
  if Assigned(FOnTitleBtnClick) then
    FOnTitleBtnClick(Self, ACol, AField);
end;

procedure TJvDBGrid.CheckTitleButton(ACol, ARow: Longint; var Enabled: Boolean);
var
  Field: TField;
begin
  if (ACol >= 0) and (ACol < Columns.Count) then
  begin
    if Assigned(FOnCheckButton) then
    begin
      Field := Columns[ACol].Field;
      if ColumnAtDepth(Columns[ACol], ARow) <> nil then
        Field := ColumnAtDepth(Columns[ACol], ARow).Field;
      FOnCheckButton(Self, ACol, Field, Enabled);
    end;
  end
  else
    Enabled := False;
end;

procedure TJvDBGrid.DisableScroll;
begin
  Inc(FDisableCount);
end;

type
  TGridDataLinkAccessProtected = class(TGridDataLink);

procedure TJvDBGrid.EnableScroll;
begin
  if FDisableCount <> 0 then
  begin
    Dec(FDisableCount);
    if (FDisableCount = 0) and DataLink.Active then
      TGridDataLinkAccessProtected(DataLink).DataSetScrolled(0);
  end;
end;

function TJvDBGrid.ScrollDisabled: Boolean;
begin
  Result := FDisableCount <> 0;
end;

procedure TJvDBGrid.Scroll(Distance: Integer);
begin
  if FDisableCount = 0 then
  begin
    inherited Scroll(Distance);
    if ((AlternateRowColor <> clNone) and (AlternateRowColor <> Color)) or
       ((AlternateRowFontColor <> clNone) and (AlternateRowFontColor <> Font.Color)) then
      Invalidate;

    if FAlwaysShowEditor and HandleAllocated and ([dgRowSelect, dgEditing] * Options = [dgEditing]) and
       Focused then
    begin
      ShowEditor;
      InvalidateCol(Col);
    end;
  end;
end;

function TJvDBGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
var
  Distance: Integer;
begin
  Result := False;
  if Assigned(OnMouseWheelDown) then
    OnMouseWheelDown(Self, Shift, MousePos, Result);
  if not Result then
  begin
    if not AcquireFocus then
      Exit;
    if ssCtrl in Shift then
      Distance := VisibleRowCount - 1
    else
    if ssShift in Shift then
      Distance := 1
    else
      Distance := 2;
    if DataLink.Active then
      Result := DataLink.DataSet.MoveBy(Distance) <> 0;
  end;
end;

function TJvDBGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
var
  Distance: Integer;
begin
  Result := False;
  if Assigned(OnMouseWheelUp) then
    OnMouseWheelUp(Self, Shift, MousePos, Result);
  if not Result then
  begin
    if not AcquireFocus then
      Exit;
    if Shift * KeyboardShiftStates = [ssCtrl] then
      Distance := VisibleRowCount - 1
    else
    if ssShift in Shift then
      Distance := 1
    else
      Distance := 2;
    if DataLink.Active then
      Result := DataLink.DataSet.MoveBy(-Distance) <> 0;
  end;
end;

procedure TJvDBGrid.EditChanged(Sender: TObject);
begin
  if Assigned(FOnEditChange) then
    FOnEditChange(Self);
end;

procedure TJvDBGrid.GridInvalidateRow(Row: Longint);
var
  I: Longint;
begin
  for I := 0 to ColCount - 1 do
    InvalidateCell(I, Row);
end;

procedure TJvDBGrid.LinkActive(Value: Boolean);
begin
  inherited LinkActive(Value);
  if Value and FAlwaysShowEditor then
    ShowEditor;
end;

{$IFDEF COMPILER9_UP}
procedure TJvDBGrid.UpdateScrollBar;
begin
  if HandleAllocated then
  begin
    // The grid can only handle ssNone and ssHorizontal. We have to emulate the other modes.
    if not (FScrollBars in [ssNone, ssHorizontal]) then
      inherited UpdateScrollBar;
    if FScrollBars = ssVertical then
      ShowScrollBar(Handle, SB_HORZ, False);

    // UpdateScrollBar is the only virtual method that is called from TDBGrid.DataChanged
    if FAlwaysShowEditor and ([dgRowSelect, dgEditing] * Options = [dgEditing]) and
       Focused then
    begin
      ShowEditor;
      InvalidateCol(Col);
    end;
  end;
end;
{$ENDIF COMPILER9_UP}

procedure TJvDBGrid.TopLeftChanged;
begin
  if (dgRowSelect in Options) and DefaultDrawing then
    GridInvalidateRow(Self.Row);
  inherited TopLeftChanged;
  if FTracking then
    StopTracking;
  if Assigned(FOnTopLeftChanged) then
    FOnTopLeftChanged(Self);

  NotifyLayoutChange(lcTopLeftChanged);
end;

procedure TJvDBGrid.StopTracking;
begin
  if FTracking or FSwapButtons then
  begin
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
  NewPressed := Windows.PtInRect(Rect(0, 0, ClientWidth, ClientHeight), {Types.} Point(X, Y)) and
    (FPressedCol = GetMasterColumn(Cell.X, Cell.Y)) and (Cell.Y < Offset);
  if FPressed <> NewPressed then
  begin
    FPressed := NewPressed;
    for I := 0 to Offset - 1 do
      GridInvalidateRow(I);
  end;
end;

procedure TJvDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Cell, LastCell: TGridCoord;
  MouseDownEvent: TMouseEvent;
  EnableClick: Boolean;
  CursorPos: TPoint;
  lLastSelected, lNewSelected: {$IFDEF RTL200_UP}TBookmark{$ELSE}TBookmarkStr{$ENDIF RTL200_UP};
  lCompare: Integer;
  WasAlwaysShowEditor: Boolean;
  WasRowResizing: Boolean;
  InheritedCalled: Boolean;
begin
  if not AcquireFocus then
    Exit;
  if (ssDouble in Shift) and (Button = mbLeft) then
  begin
    DblClick;
    Exit;
  end;
  FAcquireFocus := False;
  try
    { XP Theming }
    {$IFNDEF COMPILER14_UP}
    {$IFDEF JVCLThemesEnabled}
    if not (csDesigning in ComponentState) and UseXPThemes and StyleServices.Enabled then
    begin
      FPaintInfo.ColSizing := Sizing(X, Y);
      if not FPaintInfo.ColSizing then
      begin
        FPaintInfo.ColPressedIdx := -1;
        FPaintInfo.ColPressed := False;
        if AllowTitleClick then
          FPaintInfo.MouseInCol := -1;
        Cell := MouseCoord(X, Y);
        if (Button = mbLeft) and (Cell.X >= IndicatorOffset) and (Cell.Y >= 0) and AllowTitleClick then
        begin
          FPaintInfo.ColPressed := Cell.Y < TitleOffset;
          if FPaintInfo.ColPressed then
            FPaintInfo.ColPressedIdx := Columns[RawToDataColumn(Cell.X)].Index + ColumnOffset;
          if ValidCell(FCell) then
            InvalidateCell(FCell.X, FCell.Y);
          FCell := Cell;
        end;
      end;
    end;
    {$ENDIF JVCLThemesEnabled}
    {$ENDIF ~COMPILER14_UP}

    if Sizing(X, Y) then
      inherited MouseDown(Button, Shift, X, Y)
    else
    begin
      Cell := MouseCoord(X, Y);
      LastCell.X := Col;
      LastCell.Y := Row;

      if (Button = mbLeft) and FTitleArrow and (Cell.X = 0) and (Cell.Y = 0) and
         (dgTitles in Options) and (dgIndicator in Options) then
        FTitleArrowDown := True;

      if (Button = mbRight) and
        (dgTitles in Options) and (dgIndicator in Options) and
        (Cell.Y = 0) then
      begin
        if (Cell.X = 0) and FTitleArrow and Assigned(FOnTitleArrowMenuEvent) then
        begin
          FOnTitleArrowMenuEvent(Self);
          Exit;
        end;

        // Display TitlePopup if it exists
        if Assigned(FTitlePopup) then
        begin
          GetCursorPos(CursorPos);
          FTitlePopup.PopupComponent := Self;
          FTitlePopup.Popup(CursorPos.X, CursorPos.Y);
          Exit;
        end;
      end;

      if (DragKind = dkDock) and (Cell.X < IndicatorOffset) and
        (Cell.Y < TitleOffset) and not (csDesigning in ComponentState) then
      begin
        BeginDrag(False);
        Exit;
      end;
      if FTitleButtons and (DataLink <> nil) and DataLink.Active and
        (Cell.Y < TitleOffset) and (Cell.X >= IndicatorOffset) and
        not (csDesigning in ComponentState) then
      begin
        if ((dgColumnResize in Options) or (csDesigning in ComponentState)) and (Button = mbRight) then
        begin
          Button := mbLeft;
          FSwapButtons := True;
          MouseCapture := True;
          FPressedCol := GetMasterColumn(Cell.X, Cell.Y);
          TrackButton(X, Y);
        end
        else
        if Button = mbLeft then
        begin
          EnableClick := True;
          CheckTitleButton(Cell.X - IndicatorOffset, Cell.Y, EnableClick);
          if EnableClick then
          begin
            MouseCapture := True;
            FTracking := True;
            FPressedCol := GetMasterColumn(Cell.X, Cell.Y);
            TrackButton(X, Y);
          end
          else
          if FBeepOnError then
            SysUtils.Beep;
          if not TitleButtonAllowMove then
            Exit;
        end;
      end;
      InheritedCalled := False;
      if Cell.Y >= 0 then
      begin
        if (Cell.X < FixedCols + IndicatorOffset) and DataLink.Active then
        begin
          if dgIndicator in Options then
          begin
            inherited MouseDown(Button, Shift, 1, Y);
            InheritedCalled := True;
          end
          else
          if Cell.Y >= TitleOffset then
            if Cell.Y - Row <> 0 then
              DataLink.DataSet.MoveBy(Cell.Y - Row);
        end
        else
        begin
          { Do not show the editor if the user right clicks on the cell. Otherwise
            the grid's popup menu will never show. }
          WasAlwaysShowEditor := dgAlwaysShowEditor in Options;
          if WasAlwaysShowEditor and (Button = mbRight) {and (PopupMenu <> nil)} then
            Options := Options - [dgAlwaysShowEditor];
          try
            //-------------------------------------------------------------------------------
            // Prevents the grid from going back to the first column when dgRowSelect is True
            // Does not work if there's no indicator column
            //-------------------------------------------------------------------------------
            if (dgRowSelect in Options) and (Cell.Y >= TitleOffset) then
            begin
              // Why do we always have to work around the VCL. If we use the original X the
              // Grid will scroll back to the first column. But if we don't use the original X
              // and goRowSizing is enabled, the user can start resizing rows in the wild.
              WasRowResizing := goRowSizing in TCustomGridAccess(Self).Options;
              try
                // Disable goRowSizing without all the code that SetOptions executes.
                TGridOptions(Pointer(@TCustomGridAccess(Self).Options)^) := TCustomGridAccess(Self).Options - [goRowSizing];
                inherited MouseDown(Button, Shift, 1, Y);
                InheritedCalled := True;
              finally
                if WasRowResizing then
                  TGridOptions(Pointer(@TCustomGridAccess(Self).Options)^) := TCustomGridAccess(Self).Options + [goRowSizing];
              end;
            end
            else
            begin
              inherited MouseDown(Button, Shift, X, Y);
              InheritedCalled := True;
            end;
            if (Col = LastCell.X) and (Row <> LastCell.Y) then
            begin
              { ColEnter is not invoked when switching between rows staying in the
                same column. }
              if FAlwaysShowEditor and not EditorMode then
                ShowEditor;
            end;
          finally
            if WasAlwaysShowEditor and (Button = mbRight) {and (PopupMenu <> nil)} then
              Options := Options + [dgAlwaysShowEditor];
          end;
        end;
      end;
      MouseDownEvent := OnMouseDown;
      if Assigned(MouseDownEvent) and not InheritedCalled then
        MouseDownEvent(Self, Button, Shift, X, Y);
      if not (((csDesigning in ComponentState) or (dgColumnResize in Options)) and
        (Cell.Y < TitleOffset)) and (Button = mbLeft) then
      begin
        if MultiSelect and DataLink.Active then
          with SelectedRows do
          begin
            // must refresh the selected rows or we might have invalid bookmarks in it following record deletion
            Refresh;
            FSelecting := False;
            if Shift * KeyboardShiftStates = [ssCtrl] then
              CurrentRowSelected := not CurrentRowSelected
            else
            begin
              if (Shift * KeyboardShiftStates = [ssShift]) and (Count > 0) then
              begin
                lLastSelected := Items[Count - 1];
                CurrentRowSelected := not CurrentRowSelected;
                if CurrentRowSelected then
                begin
                  with DataLink.DataSet do
                  begin
                    DisableControls;
                    try
                      lNewSelected := Bookmark;
                      lCompare := CompareBookmarks(Pointer(lNewSelected), Pointer(lLastSelected));
                      if lCompare > 0 then
                      begin
                        GotoBookmark(Pointer(lLastSelected));
                        Next;
                        while not Eof and not (CurrentRowSelected and ({$IFDEF RTL200_UP}CompareBookmarks(Bookmark, lNewSelected) = 0{$ELSE}Bookmark = lNewSelected{$ENDIF RTL200_UP})) do
                        begin
                          CurrentRowSelected := True;
                          Next;
                        end;
                      end
                      else
                      if lCompare < 0 then
                      begin
                        GotoBookmark(Pointer(lLastSelected));
                        Prior;
                        while not Bof and not (CurrentRowSelected and ({$IFDEF RTL200_UP}CompareBookmarks(Bookmark, lNewSelected) = 0{$ELSE}Bookmark = lNewSelected{$ENDIF RTL200_UP})) do
                        begin
                          CurrentRowSelected := True;
                          Prior;
                        end;
                      end;
                    finally
                      EnableControls;
                    end;
                  end;
                end;
              end
              else
              begin
                Clear;
                if FClearSelection then
                  CurrentRowSelected := True;
              end;
            end;
          end;
      end;
    end;
  finally
    FAcquireFocus := True;
  end;
end;

procedure TJvDBGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
{$IFNDEF COMPILER14_UP}
{$IFDEF JVCLThemesEnabled}
var
  Cell: TGridCoord;
  MouseInCol: Integer;
{$ENDIF JVCLThemesEnabled}
{$ENDIF ~COMPILER14_UP}
begin
  { XP Theming }
  {$IFNDEF COMPILER14_UP}
  {$IFDEF JVCLThemesEnabled}
  if not (csDesigning in ComponentState) and UseXPThemes and StyleServices.Enabled then
  begin
    if not FPaintInfo.ColSizing and not FPaintInfo.ColMoving then
    begin
      FPaintInfo.MouseInCol := -1;
      Cell := MouseCoord(X, Y);
      if (Cell.X >= IndicatorOffset) and (Cell.Y >= 0) then
      begin
        if (Cell.Y < TitleOffset) then
        begin
          MouseInCol := Columns[RawToDataColumn(Cell.X)].Index + ColumnOffset;
          if MouseInCol <> FPaintInfo.MouseInCol then
          begin
            InvalidateCell(Cell.X, Cell.Y);
            FPaintInfo.MouseInCol := MouseInCol;
          end;
        end
      end;
      if ValidCell(FCell) then
        InvalidateCell(FCell.X, FCell.Y);
      FCell := Cell;
    end;
  end;
  {$ENDIF JVCLThemesEnabled}
  {$ENDIF ~COMPILER14_UP}

  if FTracking and not FSwapButtons then
    TrackButton(X, Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TJvDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Cell: TGridCoord;
  ACol: Longint;
  DoClick: Boolean;
  ALeftCol: Integer;
begin
  Cell := MouseCoord(X, Y);
  if FTracking and (FPressedCol <> nil) then
  begin
    DoClick := PtInRect(Rect(0, 0, ClientWidth, ClientHeight), {Types.} Point(X, Y)) and
      (Cell.Y < TitleOffset) and
      (FPressedCol = GetMasterColumn(Cell.X, Cell.Y));
    StopTracking;
    if DoClick then
    begin
      ACol := Cell.X;
      if dgIndicator in Options then
        Dec(ACol, IndicatorOffset);
      if (DataLink <> nil) and DataLink.Active and (ACol >= 0) and
        (ACol < Columns.Count) then
        DoTitleClick(FPressedCol.Index, FPressedCol.Field);
    end;
  end
  else
  if FSwapButtons then
  begin
    StopTracking;
    FSwapButtons := False;
    MouseCapture := False;
    if Button = mbRight then
      Button := mbLeft;
  end;
  if (Button = mbLeft) and (FGridState = gsColSizing) and
    (FSizingIndex + Byte(not (dgIndicator in Options)) <= FixedCols) then
  begin
    ColWidths[FSizingIndex] := GetMinColWidth(X - FSizingOfs - CellRect(FSizingIndex, 0).Left);
    FGridState := gsNormal;
  end;

  if FTitleArrowDown and (Button = mbLeft) then
  begin
    FTitleArrowDown := False;
    if FTitleArrow and (dgTitles in Options) and (dgIndicator in Options) and
       (Cell.X = 0) and (Cell.Y = 0) and (Columns.Count > 0) then
      ShowSelectColumnClick; // Selection of columns
  end;

  if (Button = mbLeft) and (FGridState = gsColSizing) then
  begin
    ALeftCol := LeftCol;
    inherited MouseUp(Button, Shift, X, Y);
    if (dgRowSelect in Options) then
      LeftCol := ALeftCol;
    if Assigned(OnColumnResized) then
      OnColumnResized(Self, FSizingIndex + Byte(not (dgIndicator in Options)) - 1,
        ColWidths[FSizingIndex]);
  end
  else
    inherited MouseUp(Button, Shift, X, Y);
  DoAutoSizeColumns;

  { XP Theming }
  {$IFNDEF COMPILER14_UP}
  {$IFDEF JVCLThemesEnabled}
  if UseXPThemes and StyleServices.Enabled then
  begin
    FPaintInfo.ColSizing := False;
    FPaintInfo.ColMoving := False;
    FPaintInfo.ColPressedIdx := -1;
    Invalidate;
  end;
  {$ENDIF JVCLThemesEnabled}
  {$ENDIF ~COMPILER14_UP}
end;

procedure TJvDBGrid.WMRButtonUp(var Msg: TWMMouse);
begin
  if not (FGridState in [gsColMoving, gsRowMoving]) then
    inherited
  else
  if not (csNoStdEvents in ControlStyle) then
    with Msg do
      MouseUp(mbRight, KeysToShiftState(Keys), XPos, YPos);
end;

procedure TJvDBGrid.WMCancelMode(var Msg: TMessage);
begin
  StopTracking;
  inherited;
end;

type
  TWinControlAccessProtected = class(TWinControl);

function TJvDBGrid.DoKeyPress(var Msg: TWMChar): Boolean;
var
  Form: TCustomForm;
  Ch: Char;
begin
  Result := True;
  Form := GetParentForm(Self);
  if Form <> nil then
    if Form.KeyPreview and TWinControlAccessProtected(Form).DoKeyPress(Msg) then
      Exit;

  with Msg do
  begin
    if Assigned(OnKeyPress) then
    begin
      Ch := Char(CharCode);
      OnKeyPress(Self, Ch);
      CharCode := Word(Ch);
    end;
    if CharCode = 0 then
      Exit;
  end;
  Result := False;
end;

procedure TJvDBGrid.WMChar(var Msg: TWMChar);
begin
  if Assigned(SelectedField) and EditWithBoolBox(SelectedField) and
    ((Char(Msg.CharCode) = Backspace) or (Msg.CharCode >= 32)) then
  begin
    if not DoKeyPress(Msg) then
      case Char(Msg.CharCode) of
        #32:
          begin
            ShowEditor;
            ChangeBoolean(JvGridBool_INVERT);
          end;
        Backspace, '0', '-':
          begin
            ShowEditor;
            ChangeBoolean(JvGridBool_UNCHECK);
          end;
        '1', '+':
          begin
            ShowEditor;
            ChangeBoolean(JvGridBool_CHECK);
          end;
      end;
  end
  else
  begin
    inherited;

    if Assigned(FCurrentControl) then
    begin
      if FCurrentControl.Visible then
        PostMessage(FCurrentControl.Handle, WM_CHAR, Msg.CharCode, Msg.KeyData);
    end
    else
      if InplaceEditor = nil then
        DoKeyPress(Msg); // This is needed to trigger an onKeyPressed event when the
                         // default editor hasn't been created because the data type
                         // of the selected field is binary or memo.
  end;
end;

procedure TJvDBGrid.KeyPress(var Key: Char);
var
  lWord: string;
  lMasterField: TField;
  I, deb: Integer;
  Found: Boolean;

  procedure CharsToFind;
  begin
    if Pos(AnsiUpperCase(FWord), AnsiUpperCase(InplaceEditor.EditText)) <> 1 then
      FWord := '';
    if Key = Backspace then
      if (FWord = '') or (Length(FWord) = 1) then
      begin
        lWord := '';
        FWord := '';
      end
      else
        lWord := Copy(FWord, 1, Length(FWord) - 1)
    else
      lWord := FWord + Key;
  end;

begin
  if (Key = Cr) and PostOnEnterKey and not ReadOnly then
    DataSource.DataSet.CheckBrowseMode;

  if not Assigned(FCurrentControl) then
    inherited KeyPress(Key);

  if EditorMode then
  begin
    // Goal: Allow to go directly into the InplaceEditor when one types the first
    // characters of a word found in the list.
    // Remark: InplaceEditor is protected in TCustomGrid, published in TJvDBGrid.
    if DataSource.DataSet.CanModify and not (ReadOnly or
      Columns[SelectedIndex].ReadOnly or Columns[SelectedIndex].Field.ReadOnly) then
    with Columns[SelectedIndex].Field do
      if (FieldKind = fkLookup) and CharInSet(Key, CharList) then
      begin
        CharsToFind;
        LookupDataSet.DisableControls;
        try
          try
            if LookupDataSet.Locate(LookupResultField, lWord, [loCaseInsensitive, loPartialKey]) then
            begin
              DataSet.Edit;
              lMasterField := DataSet.FieldByName(KeyFields);
              if lMasterField.CanModify then
              begin
                lMasterField.Value := LookupDataSet.FieldValues[LookupKeyFields];
                FWord := lWord;
                InplaceEditor.SelStart := Length(FWord);
                InplaceEditor.SelLength := Length(InplaceEditor.EditText) - Length(FWord);
              end;
            end;
          except
           { If you attempt to search for a string larger than what the field
             can hold, and exception will be raised. Just trap it. }
          end;
        finally
          LookupDataSet.EnableControls;
        end;
      end
      else
      if FieldKind = fkData then
      begin
        if DataType in [DB.ftFloat{$IFDEF COMPILER12_UP},DB.ftExtended{$ENDIF COMPILER12_UP}] then
          if CharInSet(Key, ['.', ',']) then
            Key := JclFormatSettings.DecimalSeparator;

        if CharInSet(Key, CharList) and (Columns[SelectedIndex].PickList.Count <> 0) then
        begin
          FWord := InplaceEditor.EditText;
          deb := InplaceEditor.SelStart + InplaceEditor.SelLength;
          if Key = Backspace then
          begin
            if (InplaceEditor.SelLength = 0) then
            begin
              lWord := Copy(FWord, 1, InplaceEditor.SelStart - 1)
                     + Copy(FWord, deb + 1, Length(FWord) - deb + 1);
              deb := InplaceEditor.SelStart - 1;
            end
            else
            begin
              lWord := Copy(FWord, 1, InplaceEditor.SelStart)
                     + Copy(FWord, deb + 1, Length(FWord) - deb);
              deb := InplaceEditor.SelStart;
            end;
          end
          else
          begin
            lWord := Copy(FWord, 1, InplaceEditor.SelStart) + Key
                   + Copy(FWord, deb + 1, Length(FWord) - deb);
            deb := InplaceEditor.SelStart + 1;
          end;

          Found := False;
          with Columns[SelectedIndex].PickList do
            for I := 0 to Count - 1 do
            begin
              if AnsiStartsText(lWord, Strings[I]) then
              begin
                DataSet.Edit;

                InplaceEditor.EditText := Strings[I];
                Columns[SelectedIndex].Field.Text := Strings[I];
                InplaceEditor.SelStart := deb;
                InplaceEditor.SelLength := Length(Text) - deb;
                Found := True;

                Break;
              end;
            end;

          if Found then
            Key := #0;
        end;
      end;
  end
  else
    // This fixes a bug coming from DBGrids.pas when a field is not editable.
    // This ensures that nothing else will process the keys pressed.
    Key := #0;
end;

procedure TJvDBGrid.DefaultDrawColumnCell(const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  MemoText: string;
begin
  if Assigned(Column.Field) and
    (WordWrapAllFields or (Column.Field is TStringField) or (ShowMemos and IsMemoField(Column.Field))) then
  begin
    MemoText := Column.Field.DisplayText;
    if FShowMemos and IsMemoField(Column.Field) then
    begin
      // The MemoField's default DisplayText is '(Memo)' but we want the content
      if not Assigned(Column.Field.OnGetText) then
        MemoText := Column.Field.AsString;
    end;
    WriteCellText(Rect, 2, 2, MemoText, Column.Alignment,
      UseRightToLeftAlignmentForField(Column.Field, Column.Alignment), False);
  end
  else if GetImageIndex(Column.Field) < 0 then  // Mantis 5013: Must not call inherited drawer, or the text will be painted over
  begin
    if DrawThemedHighlighting(Canvas, Rect) then
      Canvas.Brush.Style := bsClear;
    inherited DefaultDrawColumnCell(Rect, DataCol, Column, State);
  end;
end;

procedure TJvDBGrid.DefaultDataCellDraw(const Rect: TRect; Field: TField;
  State: TGridDrawState);
begin
  DefaultDrawDataCell(Rect, Field, State);
end;

function TJvDBGrid.GetMasterColumn(ACol, ARow: Longint): TColumn;
begin
  if dgIndicator in Options then
    Dec(ACol, IndicatorOffset);
  if (DataLink <> nil) and DataLink.Active and (ACol >= 0) and (ACol < Columns.Count) then
  begin
    Result := Columns[ACol];
    Result := ColumnAtDepth(Result, ARow);
  end
  else
    Result := nil;
end;

function TJvDBGrid.SortMarkerAssigned(const AFieldName: string): Boolean;
begin
  Result := AnsiSameText(AFieldName, SortedField);
end;

function TJvDBGrid.DrawThemedHighlighting(ACanvas: TCanvas; R: TRect): Boolean;
begin
  {$IFDEF JVCLThemesEnabled}
  if UseThemedHighlighting and UseXPThemes and StyleServices.Enabled and (ACanvas.Brush.Color = clHighlight) then
  begin
    if ACanvas.Brush.Style <> bsSolid then
      ACanvas.Brush.Style := bsSolid;
    {$IFDEF COMPILER16_UP}
    if CheckWin32Version(6, 0) then
    begin
      ACanvas.Brush.Color := Self.Color;
      ACanvas.FillRect(R);
      if MultiSelect and ActiveRowSelected then
        InflateRect(R, 1, 0);
      StyleServices.DrawElement(ACanvas.Handle, StyleServices.GetElementDetails(tmPopupItemHot), R, nil);
    end
    else
    {$ENDIF COMPILER16_UP}
    begin
      ACanvas.Brush.Color := $FFEFDE;
      ACanvas.Pen.Color := $ECB57F;
      ACanvas.Rectangle(R);
    end;
    ACanvas.Brush.Color := clHighlight;
    if ACanvas.Font.Color = clHighlightText then
      ACanvas.Font.Color := StyleServices.GetSystemColor(clWindowText);
    Result := True;
  end
  else
  {$ENDIF JVCLThemesEnabled}
    Result := False;
end;

procedure TJvDBGrid.WriteCellText(ARect: TRect; DX, DY: Integer; const Text: string;
  Alignment: TAlignment; ARightToLeft: Boolean; FixCell: Boolean; Options: Integer = 0);
const
  AlignFlags: array [TAlignment] of Integer =
    (DT_LEFT or DT_EXPANDTABS or DT_NOPREFIX,
     DT_RIGHT or DT_EXPANDTABS or DT_NOPREFIX,
     DT_CENTER or DT_EXPANDTABS or DT_NOPREFIX);
  RTL: array [Boolean] of Integer = (0, DT_RTLREADING);
var
  DrawBitmap: TBitmap;
  Hold: Integer;
  B, R: TRect;
  DrawOptions: Integer;

  procedure DrawAText(CellCanvas: TCanvas);
  begin
    DrawOptions := DT_EXPANDTABS or DT_NOPREFIX;
    if Options <> 0 then
      DrawOptions := DrawOptions or Options;
    if WordWrap then
      DrawOptions := DrawOptions or DT_WORDBREAK;
    {$IFDEF JVCLThemesEnabled}
    if not FixCell or not (UseXPThemes and StyleServices.Enabled) then
    {$ENDIF JVCLThemesEnabled}
      {$IFDEF COMPILER14_UP}
      if not FixCell or (DrawingStyle in [gdsClassic, gdsThemed]) then
      {$ENDIF COMPILER14_UP}
      begin
        if CellCanvas.Brush.Style <> bsSolid then
          CellCanvas.Brush.Style := bsSolid;
        if not DrawThemedHighlighting(CellCanvas, B) then
          CellCanvas.FillRect(B);
      end;
    SetBkMode(CellCanvas.Handle, TRANSPARENT);
    DrawBiDiText(CellCanvas.Handle, Text, R, DrawOptions, Alignment, ARightToLeft, Canvas.CanvasOrientation);
  end;

begin
  if ReduceFlicker
     {$IFDEF COMPILER14_UP} and not FixCell {$ENDIF}
     {$IFDEF JVCLThemesEnabled} and not (UseXPThemes and StyleServices.Enabled) {$ENDIF} then
  begin
    // Use offscreen bitmap to eliminate flicker and
    // brush origin tics in painting / scrolling.
    DrawBitmap := TBitmap.Create;
    try
      DrawBitmap.Canvas.Lock;
      try
        DrawBitmap.Width := Max(DrawBitmap.Width, ARect.Right - ARect.Left);
        DrawBitmap.Height := Max(DrawBitmap.Height, ARect.Bottom - ARect.Top);
        R := Rect(DX, DY, ARect.Right - ARect.Left - 1, ARect.Bottom - ARect.Top - 1);
        B := Rect(0, 0, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
        DrawBitmap.Canvas.Font := Canvas.Font;
        DrawBitmap.Canvas.Font.Color := Canvas.Font.Color;
        DrawBitmap.Canvas.Brush := Canvas.Brush;

        DrawAText(DrawBitmap.Canvas);
        if Canvas.CanvasOrientation = coRightToLeft then
        begin
          Hold := ARect.Left;
          ARect.Left := ARect.Right;
          ARect.Right := Hold;
        end;
        Canvas.CopyRect(ARect, DrawBitmap.Canvas, B);
      finally
        DrawBitmap.Canvas.Unlock;
      end;
    finally
      DrawBitmap.Free;
    end;
  end
  else
  begin
    // No offscreen bitmap - The display is faster but flickers
    if IsRightToLeft then
      R := Rect(ARect.Left, ARect.Top, ARect.Right - 1 - DX, ARect.Bottom - DY - 1)
    else
      R := Rect(ARect.Left + DX, ARect.Top + DY, ARect.Right - 1, ARect.Bottom - 1);
    B := ARect;
    DrawAText(Canvas);
  end;
end;

procedure TJvDBGrid.CallDrawCellEvent(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
begin
  inherited DrawCell(ACol, ARow, ARect, AState);
end;

procedure TJvDBGrid.DoDrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
{$IFNDEF COMPILER14_UP}
{$IFDEF JVCLThemesEnabled}
var
  Details: TThemedElementDetails;
  lCaptionRect: TRect;
  lCellRect: TRect;
  Bmp: TBitmap;
  DC: HDC;
{$ENDIF JVCLThemesEnabled}
{$ENDIF ~COMPILER14_UP}
begin
  {$IFNDEF COMPILER14_UP}
  {$IFDEF JVCLThemesEnabled}
  if UseXPThemes and StyleServices.Enabled then
  begin
    lCellRect := ARect;
    if StyleServices.Enabled and (ARow = 0) and (ACol - ColumnOffset >= 0) and (dgTitles in Options) then
    begin
      lCaptionRect := ARect;
      if not FPaintInfo.ColPressed or (FPaintInfo.ColPressedIdx <> ACol) then
      begin
        if (FPaintInfo.MouseInCol = -1) or (FPaintInfo.MouseInCol <> ACol) or (csDesigning in ComponentState) then
          Details := StyleServices.GetElementDetails(thHeaderItemNormal)
        else
          Details := StyleServices.GetElementDetails(thHeaderItemHot);
        lCellRect.Right := lCellRect.Right + 1;
        lCellRect.Bottom := lCellRect.Bottom + 2;
      end
      else if AllowTitleClick then
      begin
        Details := StyleServices.GetElementDetails(thHeaderItemPressed);
        InflateRect(lCaptionRect, -1, 1);
      end
      else
      begin
        if FPaintInfo.MouseInCol = ACol then
          Details := StyleServices.GetElementDetails(thHeaderItemHot)
        else
          Details := StyleServices.GetElementDetails(thHeaderItemNormal);
      end;
      StyleServices.DrawElement(Canvas.Handle, Details, lCellRect);
      { The column title isn't painted by DrawCell if the DataLink is not active. }
      if (DataLink = nil) or not DataLink.Active then
        if (ACol - ColumnOffset >= 0) and (ACol - ColumnOffset < Columns.Count) then
          DrawTitleCaption(Canvas, lCaptionRect, Columns[ACol - ColumnOffset]);
    end
    else if (ACol = 0) and (dgIndicator in Options) and StyleServices.Enabled then
    begin
      // indicator column
      if ARow < TitleOffset then
        Details := StyleServices.GetElementDetails(thHeaderItemNormal)
      else
        Details := StyleServices.GetElementDetails(thHeaderRoot);
      lCellRect.Right := lCellRect.Right + 1;
      lCellRect.Bottom := lCellRect.Bottom + 2;
      StyleServices.DrawElement(Canvas.Handle, Details, lCellRect);
      // draw the indicator
      if (DataLink.Active) and (ARow - TitleOffset = Datalink.ActiveRecord) then
      begin
        // Unfortunatelly the TDBGrid.FIndicators: TImageList is a private field so we have to
        // call the original painter for the indicator and draw it into a transparent bitmap
        // without the 3D border.
        Bmp := TBitmap.Create;
        try
//          Bmp.Canvas.Brush.Color := FixedColor;
          Bmp.Width := lCellRect.Right - lCellRect.Left;
          Bmp.Height := lCellRect.Bottom - lCellRect.Top;
          DC := Canvas.Handle;
          try
            Canvas.Handle := Bmp.Canvas.Handle;
            IntersectClipRect(Canvas.Handle, 2, 2, Bmp.Width - 2, Bmp.Height - 2);
//            CallDrawCellEvent(ACol, ARow, Rect(0, 0, Bmp.Width - 1, Bmp.Height - 1), [gdFixed]);
          finally
            Canvas.Handle := DC;
          end;
//          Bmp.TransparentColor := FixedColor;
          Bmp.Transparent := True;
//          Canvas.Draw(lCellRect.Left, lCellRect.Top, Bmp);
        finally
          Bmp.Free;
        end;
      end;
    end
    else
      CallDrawCellEvent(ACol, ARow, ARect, AState);
  end
  else
  {$ENDIF JVCLThemesEnabled}
  {$ENDIF ~COMPILER14_UP}
    CallDrawCellEvent(ACol, ARow, ARect, AState);
end;

procedure TJvDBGrid.DrawTitleCaption(Canvas: TCanvas; const TextRect: TRect; DrawColumn: TColumn);
const
  AlignFlags: array [TAlignment] of Integer =
    (DT_LEFT or DT_EXPANDTABS or DT_NOPREFIX,
     DT_RIGHT or DT_EXPANDTABS or DT_NOPREFIX,
     DT_CENTER or DT_EXPANDTABS or DT_NOPREFIX);
  RTL: array [Boolean] of Integer = (0, DT_RTLREADING);
const
  MinOffs = 1;
var
  CalcRect: TRect;
  TitleSpace,
  TitleOptions: Integer;
begin
  with DrawColumn.Title do
  begin
    TitleOptions := DT_END_ELLIPSIS;
    if WordWrap then
    begin
      CalcRect := TextRect;
      Dec(CalcRect.Right, MinOffs + 1);
      DrawBiDiText(Canvas.Handle, Caption, CalcRect,
        DT_EXPANDTABS or DT_NOPREFIX or DT_CALCRECT or DT_WORDBREAK,
        Alignment, IsRightToLeft, Canvas.CanvasOrientation);
      if CalcRect.Bottom > TextRect.Bottom then
      begin
        TitleOptions := DT_END_ELLIPSIS or DT_SINGLELINE;
        TitleSpace := TextRect.Bottom - TextRect.Top - Canvas.TextHeight('^g');
      end
      else
      begin
        if (CalcRect.Bottom - CalcRect.Top) > Canvas.TextHeight('^g') then
          TitleOptions := 0;
        TitleSpace := TextRect.Bottom - CalcRect.Bottom;
      end;
    end
    else
      TitleSpace := TextRect.Bottom - TextRect.Top - Canvas.TextHeight('^g');
    WriteCellText(TextRect, MinOffs, Max(MinOffs, TitleSpace div 2), Caption, Alignment,
      IsRightToLeft, True, TitleOptions);
  end;
end;

procedure TJvDBGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
const
  EdgeFlag: array [Boolean] of UINT = (BDR_RAISEDINNER, BDR_SUNKENINNER);
  MinOffs = 1;
var
  FrameOffs: Byte;
  BackColor: TColor;
  ASortMarker: TSortMarker;
  Indicator, ALeft: Integer;
  Down: Boolean;
  Bmp: TJvDBGridBitmap;
  SavePen: TColor;
  OldActive: Longint;
  MultiSelected: Boolean;
  FixRect: TRect;
  TitleRect, TextRect: TRect;
  AField: TField;
  MasterCol: TColumn;
  InBiDiMode: Boolean;
  DrawColumn: TColumn;
  DefaultDrawText, DefaultDrawSortMarker: Boolean;
  OSOffset: Integer;

  function CalcTitleRect(Col: TColumn; ARow: Integer; var MasterCol: TColumn): TRect;
    { copied from CodeGear's DbGrids.pas }
  var
    I, J: Integer;
    InBiDiMode: Boolean;
    DrawInfo: TGridDrawInfo;
  begin
    MasterCol := ColumnAtDepth(Col, ARow);
    if MasterCol = nil then
      Exit;
    I := DataToRawColumn(MasterCol.Index);
    if I >= LeftCol then
      J := MasterCol.Depth
    else
    begin
      if (FixedCols > 0) and (MasterCol.Index < FixedCols) then
      begin
        J := MasterCol.Depth;
      end
      else
      begin
        I := LeftCol;
        if Col.Depth > ARow then
          J := ARow
        else
          J := Col.Depth;
      end;
    end;
    Result := CellRect(I, J);
    InBiDiMode := UseRightToLeftAlignment and (Canvas.CanvasOrientation = coLeftToRight);
    for I := Col.Index to Columns.Count - 1 do
    begin
      if ColumnAtDepth(Columns[I], ARow) <> MasterCol then
        Break;
      if not InBiDiMode then
      begin
        J := CellRect(DataToRawColumn(I), ARow).Right;
        if J = 0 then
          Break;
        Result.Right := Max(Result.Right, J);
      end
      else
      begin
        J := CellRect(DataToRawColumn(I), ARow).Left;
        if J >= ClientWidth then
          Break;
        Result.Left := J;
      end;
    end;
    J := Col.Depth;
    if (J <= ARow) and (J < FixedRows - 1) then
    begin
      CalcFixedInfo(DrawInfo);
      Result.Bottom := DrawInfo.Vert.FixedBoundary -
        DrawInfo.Vert.EffectiveLineWidth;
    end;
    Result.Left := Result.Left + 1;
    if Win32MajorVersion >= 6 then // Windows 7+
      Result.Right := Result.Right - 1;
  end;

  procedure DrawExpandBtn(var TitleRect, TextRect: TRect; InBiDiMode: Boolean;
    Expanded: Boolean); { copied from CodeGear's DbGrids.pas }
  const
    ScrollArrows: array [Boolean, Boolean] of Integer =
      ((DFCS_SCROLLRIGHT, DFCS_SCROLLLEFT), (DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT));
  var
    ButtonRect: TRect;
    I: Integer;
  begin
    I := GetSystemMetrics(SM_CXHSCROLL);
    if (TextRect.Right - TextRect.Left) > I then
    begin
      Dec(TextRect.Right, I);
      ButtonRect := TitleRect;
      ButtonRect.Left := TextRect.Right;
      I := SaveDC(Canvas.Handle);
      try
        Canvas.FillRect(ButtonRect);
        InflateRect(ButtonRect, -1, -1);
        IntersectClipRect(Canvas.Handle, ButtonRect.Left, ButtonRect.Top, ButtonRect.Right, ButtonRect.Bottom);
        InflateRect(ButtonRect, 1, 1);
        { DrawFrameControl doesn't draw properly when orientation has changed.
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

begin
  FCurrentDrawRow := ARow;
  Canvas.Font := Self.Font;
  if (DataLink <> nil) and DataLink.Active and (ACol >= 0) and
    (ACol < Columns.Count) then
  begin
    DrawColumn := Columns[ACol];
    if DrawColumn <> nil then
      Canvas.Font := DrawColumn.Font;
  end;

  DoDrawCell(ACol, ARow, ARect, AState);
  if FTitleArrow and (ARow = 0) and (ACol = 0) and
    (dgIndicator in Options) and (dgTitles in Options) then
  begin
    Bmp := GetGridBitmap(gpPopup);
    DrawBitmapTransparent(Canvas, (ARect.Left + ARect.Right - Bmp.Width) div 2,
      (ARect.Top + ARect.Bottom - Bmp.Height) div 2, Bmp, clWhite);
  end;

  InBiDiMode := Canvas.CanvasOrientation = coRightToLeft;
  if (dgIndicator in Options) and (ACol = 0) and (ARow - TitleOffset >= 0) and
    MultiSelect and (DataLink <> nil) and DataLink.Active and
    (DataLink.DataSet.State = dsBrowse) then
  begin { draw multiselect indicators if needed }
    FixRect := ARect;
    if [dgRowLines, dgColLines] * Options = [dgRowLines, dgColLines] then
    begin
      InflateRect(FixRect, -1, -1);
      FrameOffs := 1;
    end
    else
      FrameOffs := 2;
    OldActive := DataLink.ActiveRecord;
    try
      DataLink.ActiveRecord := ARow - TitleOffset;
      MultiSelected := ActiveRowSelected;
    finally
      DataLink.ActiveRecord := OldActive;
    end;
    if MultiSelected then
    begin
      if ARow - TitleOffset <> DataLink.ActiveRecord then
        Indicator := 0
      else
        Indicator := 1; { multiselected and current row }
//      MsIndicators.BkColor := FixedColor;
      ALeft := FixRect.Right - MsIndicators.Width - FrameOffs;
      if InBiDiMode then
        Inc(ALeft);
      MsIndicators.Draw(Self.Canvas, ALeft, (FixRect.Top +
        FixRect.Bottom - MsIndicators.Height) shr 1, Indicator);
    end;
  end
  else
  if not (csLoading in ComponentState) and
    (gdFixed in AState) and (dgTitles in Options) and (ARow < TitleOffset) then
  begin
    SavePen := Canvas.Pen.Color;
    try
      Canvas.Pen.Color := clWindowFrame;
      if dgIndicator in Options then
        Dec(ACol, IndicatorOffset);
      AField := nil;
      ASortMarker := smNone;
      if (DataLink <> nil) and DataLink.Active and (ACol >= 0) and
        (ACol < Columns.Count) then
      begin
        DrawColumn := Columns[ACol];
        AField := DrawColumn.Field;
      end
      else
        DrawColumn := nil;
      if Assigned(DrawColumn) and not DrawColumn.Showing then
        Exit;
      TitleRect := CalcTitleRect(DrawColumn, ARow, MasterCol);
      if Win32MajorVersion >= 6 then // Windows 7+
        OSOffset := 1
      else
        OSOffset := 0;
      if TitleRect.Right < ARect.Right - OSOffset then
        TitleRect.Right := ARect.Right - OSOffset;
      if MasterCol = nil then
        Exit
      else
      if MasterCol <> DrawColumn then
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
      Down := FPressed and FTitleButtons and (FPressedCol = DrawColumn);
      if FTitleButtons or ([dgRowLines, dgColLines] * Options = [dgRowLines, dgColLines]) then
      begin
        {$IFDEF JVCLThemesEnabled}
        if not (UseXPThemes and StyleServices.Enabled) then
        {$ENDIF JVCLThemesEnabled}
        begin
          DrawEdge(Canvas.Handle, TitleRect, EdgeFlag[Down], BF_BOTTOMRIGHT);
          DrawEdge(Canvas.Handle, TitleRect, EdgeFlag[Down], BF_TOPLEFT);
          InflateRect(TitleRect, -1, -1);
        end;
      end;
      Canvas.Font := TitleFont;
      Canvas.Brush.Color := FixedColor;
      if DrawColumn <> nil then
      begin
        Canvas.Font := DrawColumn.Title.Font;
        Canvas.Brush.Color := DrawColumn.Title.Color;
      end;
      if FTitleButtons and (AField <> nil) then
      begin
        BackColor := Canvas.Brush.Color;
        //-----------------------------------------
        // FBC -fix SortMarker
        // Not so elegant, but it works.
        //-----------------------------------------
        if SortMarkerAssigned(AField.FieldName) then
        begin
          ASortMarker := Self.SortMarker;
          DoGetBtnParams(AField, Canvas.Font, BackColor, ASortMarker, Down);
          Self.SortMarker := ASortMarker;
        end
        else
          DoGetBtnParams(AField, Canvas.Font, BackColor, ASortMarker, Down);
        Canvas.Brush.Color := BackColor;
      end;
      if Down then
        OffsetRect(TitleRect, 1, 1);
      ARect := TitleRect;
      if (DataLink = nil) or not DataLink.Active then
      begin
        {$IFDEF COMPILER14_UP}
        DrawCellBackground(TitleRect, FixedColor, AState, ACol, ARow - TitleOffset);
        {$ELSE}
          {$IFDEF JVCLThemesEnabled}
        if not (UseXPThemes and StyleServices.Enabled) then
          {$ENDIF JVCLThemesEnabled}
          Canvas.FillRect(TitleRect);
        {$ENDIF COMPILER14_UP}
      end
      else
      if DrawColumn <> nil then
      begin
        {$IFDEF COMPILER14_UP}
        DrawCellBackground(TitleRect, FixedColor, AState, ACol, ARow - TitleOffset);
        {$ELSE}
//          {$IFDEF JVCLThemesEnabled}
//        if not (UseXPThemes and StyleServices.Enabled) then
//          {$ENDIF JVCLThemesEnabled}
//          Canvas.FillRect(TitleRect);
        {$ENDIF COMPILER14_UP}
        case ASortMarker of
          smDown:
            Bmp := GetGridBitmap(gpMarkDown);
          smUp:
            Bmp := GetGridBitmap(gpMarkUp);
        else
          Bmp := nil;
        end;
        if Bmp <> nil then
          Indicator := Bmp.Width + 6
        else
          Indicator := 1;
        DefaultDrawText := True;
        DefaultDrawSortMarker := True;
        DoDrawColumnTitle(Canvas, TitleRect, DrawColumn, Bmp, Down, Indicator,
          DefaultDrawText, DefaultDrawSortMarker);
        TextRect := TitleRect;
        if ASortMarker <> smNone then
          Dec(TextRect.Right, Bmp.Width + 4);
        if DefaultDrawText then
        begin
          if DrawColumn.Expandable then
            DrawExpandBtn(TitleRect, TextRect, InBiDiMode, DrawColumn.Expanded);
          DrawTitleCaption(Canvas, TextRect, DrawColumn);
        end;
        if DefaultDrawSortMarker then
        begin
          if Bmp <> nil then
          begin
            ALeft := TitleRect.Right - Indicator + 3;
            if IsRightToLeft then
              ALeft := TitleRect.Left + 3;
            {$IFDEF COMPILER14_UP}
            //DrawCellBackground(Rect(TextRect.Right, TitleRect.Top, TitleRect.Right, TitleRect.Bottom), FixedColor, AState, ACol, ARow - TitleOffset);
            {$ELSE}
              {$IFDEF JVCLThemesEnabled}
            if not (UseXPThemes and StyleServices.Enabled) then
              {$ENDIF JVCLThemesEnabled}
              Canvas.FillRect(Rect(TextRect.Right, TitleRect.Top, TitleRect.Right, TitleRect.Bottom));
            {$ENDIF COMPILER14_UP}
            if (ALeft > TitleRect.Left) and (ALeft + Bmp.Width < TitleRect.Right) then
              DrawBitmapTransparent(Canvas, ALeft, (TitleRect.Bottom +
                TitleRect.Top - Bmp.Height) div 2, Bmp, clFuchsia);
          end;
        end;
      end
      else
        WriteCellText(ARect, MinOffs, MinOffs, '', taLeftJustify, False, IsRightToLeft);
      {$IFDEF COMPILER14_UP}
      if ([dgRowLines, dgColLines] * Options = [dgRowLines, dgColLines]) and
         ((DrawingStyle = gdsClassic) or ((DrawingStyle = gdsThemed) and not StyleServices.Enabled)) and
         not (gdPressed in AState) then
      begin
        InflateRect(TitleRect, 1, 1);
        DrawEdge(Canvas.Handle, TitleRect, BDR_RAISEDINNER, BF_BOTTOMRIGHT);
        DrawEdge(Canvas.Handle, TitleRect, BDR_RAISEDINNER, BF_TOPLEFT);
      end;
      {$ENDIF COMPILER14_UP}
    finally
      Canvas.Pen.Color := SavePen;
    end;
  end
  else
  begin
    Canvas.Font := Self.Font;
    if (DataLink <> nil) and DataLink.Active and (ACol >= 0) and
      (ACol < Columns.Count) then
    begin
      DrawColumn := Columns[ACol];
      if DrawColumn <> nil then
        Canvas.Font := DrawColumn.Font;
    end;
  end;
end;

procedure TJvDBGrid.DrawColumnCell(const Rect: TRect; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
var
  I: Integer;
  NewBackgrnd: TColor;
  Highlight: Boolean;
  Bmp: TBitmap;
  Field, ReadOnlyTestField: TField;
  R: TRect;
begin
  Field := Column.Field;
  if Assigned(DataSource) and Assigned(DataSource.DataSet) and DataSource.DataSet.Active and
    (SelectedRows.IndexOf(DataSource.DataSet.Bookmark) > -1) then
    Include(State, gdSelected);
  NewBackgrnd := Canvas.Brush.Color;
  Highlight := (gdSelected in State) and ((dgAlwaysShowSelection in Options) or Focused);
  GetCellProps(Column, Canvas.Font, NewBackgrnd, Highlight or ActiveRowSelected);
  if not Highlight and (ReadOnlyCellColor <> clDefault) and
     (Field <> nil) and (not Field.CanModify or not CanEditCell(Field)) then
  begin
    if (gdSelected in State) and (Focused xor MultiSelect) then
      Canvas.Brush.Color := NewBackgrnd
    else
    begin
      Canvas.Brush.Color := ReadOnlyCellColor;

      { Lookup fields do not have a FieldNo. In this case CanModify returns False } 
      if Field.Lookup and (Field.LookupDataSet <> nil) and (Field.LookupResultField <> '')
         and (Field.LookupKeyFields <> '') and (Field.KeyFields <> '') then
      begin
        I := 1;
        ReadOnlyTestField := Field.DataSet.FieldByName(ExtractFieldName(Field.KeyFields, I));
        if ReadOnlyTestField.CanModify and CanEditCell(ReadOnlyTestField) then
          Canvas.Brush.Color := NewBackgrnd
      end;
    end;
  end
  else
    Canvas.Brush.Color := NewBackgrnd;
  if DefaultDrawing then
  begin
    I := GetImageIndex(Field);
    if I >= 0 then
    begin
      Bmp := GetGridBitmap(TGridPicture(I));
      Canvas.FillRect(Rect);
      DrawBitmapTransparent(Canvas, (Rect.Left + Rect.Right + 1 - Bmp.Width) div 2,
        (Rect.Top + Rect.Bottom + 1 - Bmp.Height) div 2, Bmp, clOlive);
    end
    else
    begin
      DefaultDrawColumnCell(Rect, DataCol, Column, State);
    end;
  end;
  if (Columns.State = csDefault) or not DefaultDrawing or (csDesigning in ComponentState) then
    inherited DrawDataCell(Rect, Field, State);
  inherited DrawColumnCell(Rect, DataCol, Column, State);
  if DefaultDrawing and (gdFocused in State) and not (csDesigning in ComponentState) and
    not (dgRowSelect in Options) and
    (ValidParentForm(Self).ActiveControl = Self) then
  begin
    R := Rect;
    {$IFDEF JVCLThemesEnabled}
    if UseThemedHighlighting and UseXPThemes and StyleServices.Enabled then
    begin
      InflateRect(R, -1, -1);
      Bmp := TBitmap.Create;
      try
        Bmp.Canvas.Brush.Color := clWhite;
        Bmp.Width := R.Right - R.Left;
        Bmp.Height := R.Bottom - R.Top;
        Bmp.Canvas.DrawFocusRect(Types.Rect(0, 0, Bmp.Width, Bmp.Height));
        Bmp.TransparentColor := clWhite;
        Bmp.Transparent := True;
        Canvas.Draw(R.Left, R.Top, Bmp);
      finally
        Bmp.Free;
      end;
    end
    else
    {$ENDIF JVCLThemesEnabled}
      Canvas.DrawFocusRect(R);
  end;
end;

procedure TJvDBGrid.DrawDataCell(const Rect: TRect; Field: TField;
  State: TGridDrawState);
begin
end;

procedure TJvDBGrid.MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
var
  Coord: TGridCoord;
begin
  Coord := MouseCoord(X, Y);
  ACol := Coord.X;
  ARow := Coord.Y;
end;

procedure TJvDBGrid.SaveColumnsLayout(const AppStorage: TJvCustomAppStorage;
  const Section: string);
var
  I: Integer;
  SectionName: string;
begin
  if Section <> '' then
    SectionName := Section
  else
    SectionName := GetDefaultSection(Self);
  if Assigned(AppStorage) then
  begin
    AppStorage.BeginUpdate;
    try
    AppStorage.DeleteSubTree(SectionName);
      for I := 0 to Columns.Count - 1 do
        AppStorage.WriteString(AppStorage.ConcatPaths([SectionName, Format('%s.%s', [Name, Columns.Items[I].FieldName])]),
          Format('%d,%d', [Columns.Items[I].Index, Columns.Items[I].Width]));
    finally
      AppStorage.EndUpdate;
    end;
  end;
end;

procedure TJvDBGrid.RestoreColumnsLayout(const AppStorage: TJvCustomAppStorage;
  const Section: string);
const
  Delims = [' ', ','];
type
  TColumnInfo = record
    Column: TColumn;
    EndIndex: Integer;
  end;
  TColumnArray = array of TColumnInfo;
var
  I, J: Integer;
  SectionName, S: string;
  ColumnArray: TColumnArray;
begin
  if Section <> '' then
    SectionName := Section
  else
    SectionName := GetDefaultSection(Self);
  if Assigned(AppStorage) then
  begin
    AppStorage.BeginUpdate;
    try
    SetLength(ColumnArray, Columns.Count);
    for I := 0 to Columns.Count - 1 do
    begin
      S := AppStorage.ReadString(AppStorage.ConcatPaths([SectionName,
        Format('%s.%s', [Name, Columns.Items[I].FieldName])]));
      ColumnArray[I].Column := Columns.Items[I];
      ColumnArray[I].EndIndex := Columns.Items[I].Index;
      if S <> '' then
      begin
        ColumnArray[I].EndIndex := StrToIntDef(ExtractWord(1, S, Delims), ColumnArray[I].EndIndex);
        S := ExtractWord(2, S, Delims);
        Columns.Items[I].Width := StrToIntDef(S, Columns.Items[I].Width);
        Columns.Items[I].Visible := (S <> '-1');
      end;
    end;
    for I := 0 to Columns.Count - 1 do
      for J := 0 to Columns.Count - 1 do
        if ColumnArray[J].EndIndex = I then
        begin
          ColumnArray[J].Column.Index := ColumnArray[J].EndIndex;
          Break;
        end;
    finally
      AppStorage.EndUpdate;
    end;

  end;
end;

procedure TJvDBGrid.LoadFromAppStore(const AppStorage: TJvCustomAppStorage; const Path: string);
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
  begin
    HandleNeeded;
    BeginLayout;
    try
      if StoreColumns then
        RestoreColumnsLayout(AppStorage, Path)
      else
        InternalRestoreFields(DataSource.DataSet, AppStorage, Path, False);
    finally
      EndLayout;
    end;
  end;
end;

procedure TJvDBGrid.SaveToAppStore(const AppStorage: TJvCustomAppStorage; const Path: string);
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    if StoreColumns then
      SaveColumnsLayout(AppStorage, Path)
    else
      InternalSaveFields(DataSource.DataSet, AppStorage, Path);
end;

procedure TJvDBGrid.Load;
begin
  IniLoad(nil);
end;

procedure TJvDBGrid.Save;
begin
  IniSave(nil);
end;

procedure TJvDBGrid.IniSave(Sender: TObject);
var
  Section: string;
begin
  if (Name <> '') and Assigned(IniStorage) then
  begin
    if StoreColumns then
      Section := IniStorage.AppStorage.ConcatPaths([IniStorage.AppStoragePath, GetDefaultSection(Self)])
    else
    if (DataSource <> nil) and
      (DataSource.DataSet <> nil) then
      Section := IniStorage.AppStorage.ConcatPaths([IniStorage.AppStoragePath, DataSetSectionName(DataSource.DataSet)])
    else
      Section := '';
    SaveToAppStore(IniStorage.AppStorage, Section);
  end;
end;

procedure TJvDBGrid.IniLoad(Sender: TObject);
var
  Section: string;
begin
  if (Name <> '') and Assigned(IniStorage) then
  begin
    if StoreColumns then
      Section := IniStorage.AppStorage.ConcatPaths([IniStorage.AppStoragePath, GetDefaultSection(Self)])
    else
    if (DataSource <> nil) and
      (DataSource.DataSet <> nil) then
      Section := IniStorage.AppStorage.ConcatPaths([IniStorage.AppStoragePath, DataSetSectionName(DataSource.DataSet)])
    else
      Section := '';
    LoadFromAppStore(IniStorage.AppStorage, Section);
  end;
end;

procedure TJvDBGrid.CalcSizingState(X, Y: Integer; var State: TGridState;
  var Index: Longint; var SizingPos, SizingOfs: Integer; var FixedInfo: TGridDrawInfo);
var
  Coord: TGridCoord;
  DrawInfo: TGridDrawInfo;
begin
  inherited CalcSizingState(X, Y, State, Index, SizingPos, SizingOfs, FixedInfo);

  // do nothing if not authorized to size columns
  if not (dgColumnResize in Options) and not (csDesigning in ComponentState) then
    Exit;

  FCanResizeColumn := State = gsColSizing; //  If true, mouse double clicking can resize column.

  { Mantis 5818: Index-out-of-range error when there is only one visible column partially displayed }
  CalcDrawInfo(DrawInfo);
  if (DrawInfo.Horz.FullVisBoundary = DrawInfo.Horz.FixedBoundary) then
    Index := Index - 1; // Index from inherited code has the value of 2.

  { Store the column index to resize }
  if dgIndicator in Options then
    FResizeColumnIndex := Index - 1
  else
    FResizeColumnIndex := Index; // Mantis 6061

  if (State = gsNormal) and (Y <= RowHeights[0]) then
  begin
    Coord := MouseCoord(X, Y);
    CalcDrawInfo(FixedInfo);
    if CellRect(Coord.X, 0).Right - 5 < X then
    begin
      State := gsColSizing;
      Index := Coord.X;
      SizingPos := X;
      SizingOfs := X - CellRect(Coord.X, 0).Right;
    end;
    if CellRect(Coord.X, 0).Left + 5 > X then
    begin
      State := gsColSizing;
      Index := Coord.X - 1;
      SizingPos := X;
      SizingOfs := X - CellRect(Coord.X, 0).Left;
    end;
    if Index <= Byte(dgIndicator in Options) - 1 then
      State := gsNormal;
  end;
  FSizingIndex := Index;
  FSizingOfs := SizingOfs;
end;

procedure TJvDBGrid.DoDrawColumnTitle(ACanvas: TCanvas; ARect: TRect;
  AColumn: TColumn; var ASortMarker: TJvDBGridBitmap; IsDown: Boolean; var Offset: Integer;
  var DefaultDrawText, DefaultDrawSortMarker: Boolean);
begin
  if Assigned(FOnDrawColumnTitle) then
  begin
    FOnDrawColumnTitle(Self, ACanvas, ARect, AColumn, ASortMarker, IsDown, Offset,
      DefaultDrawText, DefaultDrawSortMarker);
  end;
end;

procedure TJvDBGrid.ChangeBoolean(const FieldValueChange: Shortint);
// FieldValueChange = 9 -> invert, 0 -> check (true), -1 -> uncheck (false)
begin
  if Assigned(FBooleanFieldToEdit) and BooleanEditor then
  begin
    DataLink.Edit;
    if DataLink.Editing then
    begin
      if FBooleanFieldToEdit.IsNull or (FieldValueChange <> JvGridBool_INVERT) then
      begin
        case FBooleanFieldToEdit.DataType of
          ftBoolean:
            FBooleanFieldToEdit.Value := (FieldValueChange = JvGridBool_CHECK);
          {$IFDEF COMPILER10_UP}ftFixedWideChar,{$ENDIF COMPILER10_UP}
          ftString, ftWideString, ftBCD, ftFMTBCD:
            begin
              if FieldValueChange = JvGridBool_CHECK then
                FBooleanFieldToEdit.Value := FStringForTrue
              else
                FBooleanFieldToEdit.Value := FStringForFalse;
            end;
        else
          //FBooleanFieldToEdit.Value := FieldValueChange + 1;
          if FieldValueChange <> JvGridBool_INVERT then
            FBooleanFieldToEdit.Value := FieldValueChange + 1
          else
            FBooleanFieldToEdit.Value := 1;
        end;
      end
      else
        case FBooleanFieldToEdit.DataType of
          ftBoolean:
            FBooleanFieldToEdit.Value := not FBooleanFieldToEdit.AsBoolean;
          {$IFDEF COMPILER10_UP}ftFixedWideChar,{$ENDIF COMPILER10_UP}
          ftString, ftWideString, ftBCD, ftFMTBCD:
            begin
              if AnsiSameText(FBooleanFieldToEdit.AsString, FStringForTrue) then
                FBooleanFieldToEdit.Value := FStringForFalse
              else
                FBooleanFieldToEdit.Value := FStringForTrue;
            end;
        else
          FBooleanFieldToEdit.Value := 1 - Abs(FBooleanFieldToEdit.AsInteger);
        end;
      InvalidateCell(Col, Row);
    end;
  end;
  FBooleanFieldToEdit := nil;
end;

procedure TJvDBGrid.CellClick(Column: TColumn);
begin
  FTitleColumn := nil;
  inherited CellClick(Column);

  if Assigned(Column.Field) and (FBooleanFieldToEdit = Column.Field) then
    ChangeBoolean(JvGridBool_INVERT); // Invert the field value
end;

procedure TJvDBGrid.EditButtonClick;
begin
  // Just to have it here for the call in TJvDBInplaceEdit
  inherited EditButtonClick;
end;

procedure TJvDBGrid.MouseLeave(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  inherited MouseLeave(Control);
end;

procedure TJvDBGrid.DoGetBtnParams(Field: TField;
  AFont: TFont; var Background: TColor; var ASortMarker: TSortMarker;
  IsDown: Boolean);
begin
  if Assigned(FOnGetBtnParams) then
    FOnGetBtnParams(Self, Field, AFont, Background, ASortMarker, IsDown);
end;

procedure TJvDBGrid.ColEnter;
begin
  FWord := '';
  inherited ColEnter;
  if FAlwaysShowEditor and not EditorMode then
    ShowEditor;
end;

function TJvDBGrid.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  if FCancelOnMouse then
  begin
    // Do not validate a record by error
    if DataLink.Active and (DataLink.DataSet.State <> dsBrowse) then
      DataLink.DataSet.Cancel;
  end;
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

procedure TJvDBGrid.UpdateTabStops(ALimit: Integer = -1);
var
  I: Integer;
begin
  for I := 0 to Columns.Count - 1 do
    with Columns[I] do
      if ALimit = -1 then
        TabStops[I + IndicatorOffset] := True
      else
        TabStops[I + IndicatorOffset] := (I < ALimit);
end;

procedure TJvDBGrid.SetTitleArrow(const Value: Boolean);
begin
  if FTitleArrow <> Value then
  begin
    FTitleArrow := Value;
    Invalidate;
  end;
end;

procedure TJvDBGrid.ReadDelphi2010OptionsMigrated(Reader: TReader);
begin
  FDelphi2010OptionsMigrated := Reader.ReadBoolean;
end;

procedure TJvDBGrid.WriteDelphi2010OptionsMigrated(Writer: TWriter);
begin
  Writer.WriteBoolean(True);
end;

procedure TJvDBGrid.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('AlternRowColor', ReadAlternateRowColor, nil, False);
  Filer.DefineProperty('AlternRowFontColor', ReadAlternateRowFontColor, nil, False);
  Filer.DefineProperty('PostOnEnter', ReadPostOnEnter, nil, False);

  // We need to migrate the Options set for Delphi 2010 due to the added flags
  Filer.DefineProperty('Delphi2010OptionsMigrated', ReadDelphi2010OptionsMigrated, WriteDelphi2010OptionsMigrated,
    {$IFDEF COMPILER14_UP}
    [dgTitleClick, dgTitleHotTrack] * Options = [] // if one of them is set we already know that we are migrated
    {$ELSE}
    False
    {$ENDIF COMPILER14_UP}
  );
end;

procedure TJvDBGrid.ReadPostOnEnter(Reader: TReader);
begin
  PostOnEnterKey := Reader.ReadBoolean;
end;

procedure TJvDBGrid.ReadAlternateRowColor(Reader: TReader);
begin
  if Reader.ReadBoolean then
    AlternateRowColor := JvDefaultAlternateRowColor // this was the previous default row color
  else
    AlternateRowColor := clNone;
end;

procedure TJvDBGrid.SetAlternateRowColor(const Value: TColor);
begin
  if FAlternateRowColor <> Value then
  begin
    FAlternateRowColor := Value;
    Invalidate;
  end;
end;

procedure TJvDBGrid.ReadAlternateRowFontColor(Reader: TReader);
begin
  if Reader.ReadBoolean then
    AlternateRowFontColor := JvDefaultAlternateRowFontColor
  else
    AlternateRowFontColor := clNone;
end;

procedure TJvDBGrid.SetAlternateRowFontColor(const Value: TColor);
begin
  if FAlternateRowFontColor <> Value then
  begin
    FAlternateRowFontColor := Value;
    Invalidate;
  end;
end;

procedure TJvDBGrid.DoAutoSizeColumns;
// This function ignores Min and Max column widths because these values
// bring about two problems:
// - if (min. width * nb. of columns) > total width --> result too large
// - if (max. width * nb. of columns) < total width --> result too small
var
  ColLineWidth, AvailableWidth, TotalColWidth, AWidth: Integer;
  I, ALeftCol, LastColIndex: Integer;
  ScaleFactor: Double;
begin
  if not AutoSizeColumns or FInAutoSize or (Columns.Count = 0) or (FGridState = gsColSizing) then
    Exit;
  FInAutoSize := True;
  ALeftCol := LeftCol;
  try
    // Get useable width
    ColLineWidth := Ord(dgColLines in Options) * GridLineWidth;
    AvailableWidth := ClientWidth;
    if (dgIndicator in Options) then
      Dec(AvailableWidth, IndicatorWidth + ColLineWidth);
    TotalColWidth := 0;
    if FixedCols = 0 then
      BeginLayout;
    try
      // Autosize all columns proportionally
      if AutoSizeColumnIndex = JvGridResizeProportionally then
      begin
        // Get width currently occupied by visible columns
        for I := 0 to Columns.Count - 1 do
          if Columns[I].Visible then
          begin
            Inc(TotalColWidth, Columns[I].Width);
            Dec(AvailableWidth, ColLineWidth);
          end;
        if TotalColWidth = 0 then
          TotalColWidth := 1;
        // Calculate the relationship between what's available and what's in use
        ScaleFactor := AvailableWidth / TotalColWidth;
        if ScaleFactor = 1.0 then
          Exit; // No need to continue - resizing won't change anything
        // Adjust the columns width
        for I := 0 to Columns.Count - 1 do
          if Columns[I].Visible then
          begin
            if I = LastVisibleColumn then
              Columns[I].Width := AvailableWidth
            else
            begin
              AWidth := Round(ScaleFactor * Columns[I].Width);
              if AWidth < 1 then
                AWidth := 1;
              Columns[I].Width := AWidth;
              Dec(AvailableWidth, AWidth);
            end;
          end;
      end
      else
      // Autosize the last visible column
      if AutoSizeColumnIndex = JvGridResizeLastVisibleCol then
      begin
        LastColIndex := LastVisibleColumn;
        if LastColIndex < 0 then
          Exit;
        for I := 0 to Columns.Count - 1 do
          if Columns[I].Visible and (I < LastColIndex) then
            Inc(TotalColWidth, Columns[I].Width + ColLineWidth);
        AWidth := AvailableWidth - TotalColWidth - ColLineWidth;
        if AWidth > 0 then
          Columns[LastColIndex].Width := AWidth;
      end
      else
      // Only autosize one column
      if AutoSizeColumnIndex <= LastVisibleColumn then
      begin
        for I := 0 to Columns.Count - 1 do
          if Columns[I].Visible and (I <> AutoSizeColumnIndex) then
            Inc(TotalColWidth, Columns[I].Width + ColLineWidth);
        AWidth := AvailableWidth - TotalColWidth - ColLineWidth;
        if AWidth > 0 then
          Columns[AutoSizeColumnIndex].Width := AWidth;
      end;
    finally
      if FixedCols = 0 then
        EndLayout;
    end;
  finally
    FInAutoSize := False;
    LeftCol := ALeftCol;
  end;
end;

procedure TJvDBGrid.DoMaxColWidth;
var
  I: Integer;
begin
  if AutoSizeColumns or (MaxColumnWidth <= 0) then
    Exit;
  BeginLayout;
  try
    for I := 0 to Columns.Count - 1 do
      if Columns[I].Visible and (Columns[I].Width > MaxColumnWidth) then
        Columns[I].Width := MaxColumnWidth;
  finally
    EndLayout;
  end;
end;

procedure TJvDBGrid.DoMinColWidth;
var
  I: Integer;
begin
  if AutoSizeColumns or (MinColumnWidth <= 0) then
    Exit;
  BeginLayout;
  try
    for I := 0 to Columns.Count - 1 do
      if Columns[I].Visible and (Columns[I].Width < MinColumnWidth) then
        Columns[I].Width := MinColumnWidth;
  finally
    EndLayout;
  end;
end;

procedure TJvDBGrid.SetAutoSizeColumnIndex(const Value: Integer);
begin
  if FAutoSizeColumnIndex <> Value then
  begin
    FAutoSizeColumnIndex := Value;
    DoAutoSizeColumns;
  end;
end;

procedure TJvDBGrid.SetAutoSizeColumns(const Value: Boolean);
begin
  if FAutoSizeColumns <> Value then
  begin
    FAutoSizeColumns := Value;
    DoAutoSizeColumns;
  end;
end;

procedure TJvDBGrid.SetMaxColumnWidth(const Value: Integer);
begin
  if FMaxColumnWidth <> Value then
  begin
    FMaxColumnWidth := Value;
    DoMaxColWidth;
  end;
end;

procedure TJvDBGrid.SetMinColumnWidth(const Value: Integer);
begin
  if FMinColumnWidth <> Value then
  begin
    FMinColumnWidth := Value;
    DoMinColWidth;
  end;
end;

procedure TJvDBGrid.InitializeColumnsWidth(const MinWidth, MaxWidth: Integer;
  const DisplayWholeTitle: Boolean; const FixedWidths: array of Integer);
var
  SavedValue: Boolean;
  I, J,
  AWidth: Integer;
begin
  // Resize the grid columns with the given widths (0 = default width) and
  // ensure they are wide enough for the title caption (optional).
  // If there are more columns than widths in FixedWidths, the last given width
  // is used for the remaining columns.
  // If Min/MaxWidth < 0, the Min/MaxColumnWidth value is set automatically.
  // If Min/MaxWidth = 0, the Min/MaxColumnWidth value is not modified.
  // If Min/MaxWidth > 0, the Min/MaxColumnWidth value is set to the given value.
  SavedValue := AutoSizeColumns;
  FAutoSizeColumns := False;
  try
    J := Low(FixedWidths);
    if MinWidth > 0 then
      FMinColumnWidth := MinWidth
    else
    if MinWidth < 0 then
      FMinColumnWidth := FixedWidths[J];
    if MaxWidth > 0 then
      FMaxColumnWidth := MaxWidth
    else
    if MaxWidth < 0 then
      FMaxColumnWidth := FixedWidths[J];
    for I := 0 to Columns.Count - 1 do
      if Columns[I].Visible then
      begin
        if FixedWidths[J] < 1 then
          AWidth := Columns[I].DefaultWidth
        else
        begin
          AWidth := FixedWidths[J];
          if (dgTitles in Options) and DisplayWholeTitle then
          begin
            Canvas.Font.Assign(Columns[I].Title.Font);
            if Canvas.TextWidth(Columns[I].Title.Caption) + 4 > AWidth then
              AWidth := Canvas.TextWidth(Columns[I].Title.Caption) + 4;
          end;
        end;
        if AWidth < MinColumnWidth then
        begin
          if MinWidth < 0 then
            FMinColumnWidth := AWidth
          else
          if MinColumnWidth > 0 then
            AWidth := MinColumnWidth;
        end;
        if AWidth > MaxColumnWidth then
        begin
          if MaxWidth < 0 then
            FMaxColumnWidth := AWidth
          else
          if MaxColumnWidth > 0 then
            AWidth := MaxColumnWidth;
        end;
        Columns[I].Width := AWidth;
        if J < High(FixedWidths) then
          J := J + 1;
      end;
  finally
    AutoSizeColumns := SavedValue;
  end;
end;

procedure TJvDBGrid.Resize;
begin
  inherited Resize;
  DoAutoSizeColumns;

  NotifyLayoutChange(lcSizeChanged);
end;

procedure TJvDBGrid.Loaded;
var
  Ctrl_Idx: Integer;
  WinControl: TWinControl;
begin
  inherited Loaded;
  {$IFDEF COMPILER14_UP}
  // Fix the bug that Embarcadero has introduced when they added new flags to the Options set
  if not FDelphi2010OptionsMigrated and ([dgTitleClick, dgTitleHotTrack] * Options = []) then
  begin
    FDelphi2010OptionsMigrated := True;
    Options := Options + [dgTitleClick, dgTitleHotTrack];
  end;
  {$ENDIF COMPILER14_UP}

  // Edit controls are hidden
  for Ctrl_Idx := 0 to FControls.Count - 1 do
  begin
    WinControl := TWinControl(Owner.FindComponent(FControls.Items[Ctrl_Idx].ControlName));
    if WinControl <> nil then
      WinControl.Visible := False;
  end;

  DoAutoSizeColumns;
end;

function TJvDBGrid.GetMaxColWidth(Default: Integer): Integer;
begin
  if (MaxColumnWidth > 0) and (Default > MaxColumnWidth) then
    Result := MaxColumnWidth
  else
    Result := Default;
end;

function TJvDBGrid.GetMinColWidth(Default: Integer): Integer;
begin
  if (MinColumnWidth > 0) and (Default < MinColumnWidth) then
    Result := MinColumnWidth
  else
    Result := Default;
end;

function TJvDBGrid.FirstVisibleColumn: Integer;
begin
  for Result := 0 to Columns.Count - 1 do
    if Columns[Result].Visible then
      Exit;
  Result := -1;
end;

function TJvDBGrid.LastVisibleColumn: Integer;
begin
  for Result := Columns.Count - 1 downto 0 do
    if Columns[Result].Visible then
      Exit;
  Result := -1;
end;

function TJvDBGrid.GetMaxDisplayText: string;

  procedure CheckText;
  var
    S: string;
  begin
    // if IsMemoField use AsString property
    if FShowMemos and IsMemoField(Columns[FResizeColumnIndex].Field) then
    begin
      S := Columns[FResizeColumnIndex].Field.AsString;
      if Length(Result) < Length(S) then
        Result := S;
    end
    else
    begin
      S := Columns[FResizeColumnIndex].Field.DisplayText;
      if Length(Result) < Length(S) then
        Result := S;
    end;
  end;

const
  MaxRecords = 100; { value between 100 - 1000, or maybe calculated, or user input }
var
  DSet: TDataSet;
  LBookmark: TBookmark;
  I, ActiveRec: Integer;
  LastCursor: TCursor;
begin
  Result := '';

  DSet := DataSource.DataSet;

  if (DSet.State in dsEditModes) and not FCancelOnMouse then
    DSet.CheckBrowseMode;

  // Start location
  LBookmark := DSet.GetBookmark;
  ActiveRec := DataLink.ActiveRecord;
  DSet.DisableControls;
  LastCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;

  // The iteration begins...
    if (FColumnResize = gcrDataSet) and (DSet.RecordCount <= MaxRecords) then
    begin
      { Iterate all records in dataset. *** Very slow for thousands of records. *** }
      DSet.First;
      while not DSet.Eof do
      begin
        CheckText;
        DSet.Next;
      end;
    end
    else
    begin
      { Iterate only the rows shown by the grid. *** This is the faster approach. ***}
      for I := 0 to DataLink.RecordCount{BufferCount} - 1 do
      begin
        DataLink.ActiveRecord := I;
        CheckText;
      end;
    end;
  finally
    // ActiveRecord must be set BEFORE GotoBookmark
    DataLink.ActiveRecord := ActiveRec;
    try
      GotoBookmarkEx(DSet, LBookmark, [rmExact], False); // Do not center current record
    except
    end;
    DSet.FreeBookmark(LBookmark);

    DSet.EnableControls;
    Screen.Cursor := LastCursor;
  end;
end;

function TJvDBGrid.GetColumnMaxWidth: Integer;
const
  Space = 7; // Some space needed to distinguish field data between columns.
var
  S: string;
  RestoreCanvas: Boolean;
  TempDC: HDC;
  TM: TTextMetric;
begin
  if Columns[FResizeColumnIndex].Field <> nil then
  begin
    { Iterate through the recordset }
    S := GetMaxDisplayText;
    if S <> '' then
    begin
      RestoreCanvas := not HandleAllocated;
      if RestoreCanvas then
        Canvas.Handle := GetDC(0);
      try
        Canvas.Font := Font;
        GetTextMetrics(Canvas.Handle, TM);
        Result := Canvas.TextWidth(S) + TM.tmOverhang + 4 + Space;
        if Result < DefaultColWidth then
          Result := DefaultColWidth;
      finally
        if RestoreCanvas then
        begin
          TempDC := Canvas.Handle;
          Canvas.Handle := 0;
          ReleaseDC(0, TempDc);
        end;
      end;
    end
    else
      Result := DefaultColWidth;
  end
  else { Field is not assigned }
    Result := DefaultColWidth;
end;

procedure TJvDBGrid.DblClick;
begin
  { Resize column (double-click) }
  if FCanResizeColumn then
  begin
    if FColumnResize <> gcrNone then
      Columns[FResizeColumnIndex].Width := GetColumnMaxWidth;
  end
  else // When resize column (double-click) DO NOT trigger title DblClick event.
  if not DoTitleBtnDblClick then
    inherited DblClick;
  FTitleColumn := nil;
end;

function TJvDBGrid.DoTitleBtnDblClick: Boolean;
begin
  Result := Assigned(FOnTitleBtnDblClick) and Assigned(FTitleColumn);
  if Result then
    FOnTitleBtnDblClick(Self, FTitleColumn.Index, FTitleColumn.Field);
end;

procedure TJvDBGrid.TitleClick(Column: TColumn);
begin
  { When resize DO NOT trigger title click event }
  if FCanResizeColumn then
    Exit;
  
  FTitleColumn := Column;
  inherited TitleClick(Column);
  if AllowTitleClick then
  begin
    FPaintInfo.ColPressed := False;
    FPaintInfo.ColPressedIdx := -1;
    {$IFNDEF COMPILER14_UP}
    {$IFDEF JVCLThemesEnabled}
    if UseXPThemes and StyleServices.Enabled then
      if ValidCell(FCell) then
        InvalidateCell(FCell.X, FCell.Y);
    {$ENDIF JVCLThemesEnabled}
    {$ENDIF ~COMPILER14_UP}
  end;
end;

procedure TJvDBGrid.SetSortedField(const Value: string);
begin
  if FSortedField <> Value then
  begin
    FSortedField := Value;
    Invalidate;
  end;
end;

function TJvDBGrid.ChangeSortMarker(const Value: TSortMarker): Boolean;
begin
  Result := (FSortMarker <> Value);
  if Result then
    FSortMarker := Value;
end;

procedure TJvDBGrid.SetSortMarker(const Value: TSortMarker);
begin
  if ChangeSortMarker(Value) then
    Invalidate;
end;

procedure TJvDBGrid.CMHintShow(var Msg: TCMHintShow);
const
  C_TIMEOUT = 250;
var
  ACol, ARow, ATimeOut, SaveRow: Integer;
  AtCursorPosition: Boolean;
  CalcOptions: Integer;
  InitialMousePos: TPoint;
  HintRect: TRect;
begin
  AtCursorPosition := True;
  with Msg.HintInfo^ do
  begin
    { Save the position of mouse cursor }
    InitialMousePos := Mouse.CursorPos;

    HintStr := GetShortHint(Hint);
    ATimeOut := HideTimeOut;
    Self.MouseToCell(CursorPos.X, CursorPos.Y, ACol, ARow);

    //-------------------------------------------------------------------------
    // ARow <= -1 if 'outside' a valid cell;
    // Adjust CursorRect
    //-------------------------------------------------------------------------
    if FShowTitleHint or FShowCellHint then
    begin
      if (ARow <= -1) or ((ARow >= 1) and not FShowCellHint) then
      begin
        if FShowCellHint then
        begin
          CursorRect.Left := CellRect(0, Self.RowCount - 1).Left;
          CursorRect.Top := CellRect(0, Self.RowCount - 1).Bottom;
        end
        else
        begin
          CursorRect.Left := CellRect(0, 0).Left;
          CursorRect.Top := CellRect(0, 0).Bottom;
        end;
      end
      else
        CursorRect := CellRect(ACol, ARow);
    end;

    if dgIndicator in Options then
      Dec(ACol, IndicatorOffset);
    if dgTitles in Options then
      Dec(ARow, TitleOffset);

    if FShowTitleHint and (ACol >= 0) and (ARow <= -1) then
    begin
      AtCursorPosition := FCellHintPosition = gchpMouse;
      HintStr := Columns[ACol].FieldName;
      ATimeOut := Max(ATimeOut, Length(HintStr) * C_TIMEOUT);
      if Assigned(FOnShowTitleHint) and DataLink.Active then
        FOnShowTitleHint(Self, Columns[ACol].Field, HintStr, ATimeOut);
      HideTimeOut := ATimeOut;
    end;

    if FShowCellHint and (ACol >= 0) and DataLink.Active and
      ((ARow >= 0) or not FShowTitleHint) then
    begin
      AtCursorPosition := FCellHintPosition = gchpMouse;
      HintStr := Hint;
      SaveRow := DataLink.ActiveRecord;
      try
        CalcOptions := DT_CALCRECT or DT_LEFT or DT_NOPREFIX or DrawTextBiDiModeFlagsReadingOnly;
        if ARow <= -1 then // can be less than -1 if the column header is multiline (AdtField)
        begin
          Canvas.Font.Assign(Columns[ACol].Title.Font);
          HintStr := Columns[ACol].Title.Caption;
          if WordWrap then
            CalcOptions := CalcOptions or DT_WORDBREAK;
        end
        else
        with Columns[ACol] do
        begin
          Canvas.Font.Assign(Font);
          DataLink.ActiveRecord := ARow;
          if Field <> nil then
          begin
            if WordWrap and
               (WordWrapAllFields or (Field is TStringField) or (FShowMemos and IsMemoField(Field))) then
              CalcOptions := CalcOptions or DT_WORDBREAK;

            HintStr := Field.DisplayText;
            // MemoField's DisplayText is '(Memo)'
            if not Assigned(Field.OnGetText) and IsMemoField(Field) then
              HintStr := Field.AsString
            else
            if (Field is TBlobField) or EditWithBoolBox(Field) then
              HintStr := '';
          end;
        end;

        if HintStr <> '' then
        begin
          HintRect := Rect(0, 0, Columns[ACol].Width - 4, 0);
          Windows.DrawText(Canvas.Handle, PChar(HintStr), -1, HintRect, CalcOptions);
          if ((HintRect.Bottom - HintRect.Top + 2) < RowHeights[ARow + 1]) and
            ((HintRect.Right - HintRect.Left) < Columns[ACol].Width - 2) then
            HintStr := '';
        end;

        ATimeOut := Max(ATimeOut, Length(HintStr) * C_TIMEOUT);
        if Assigned(FOnShowCellHint) and DataLink.Active then
          FOnShowCellHint(Self, Columns[ACol].Field, HintStr, ATimeOut);
        HideTimeOut := ATimeOut;
      finally
        if DataLink.ActiveRecord <> SaveRow then
          DataLink.ActiveRecord := SaveRow;
      end;
    end;

    if not AtCursorPosition and HintWindowClass.ClassNameIs('THintWindow') then
      HintPos := ClientToScreen(CursorRect.TopLeft)
    else
      HintPos := InitialMousePos;
  end;
  inherited;
end;

procedure TJvDBGrid.WMVScroll(var Msg: TWMVScroll);
var
  ALeftCol: Integer;
begin
  if dgRowSelect in Options then
  begin
    ALeftCol := LeftCol;
    inherited;
    LeftCol := ALeftCol;
  end
  else
    inherited;
end;

procedure TJvDBGrid.SetWordWrap(Value: Boolean);
begin
  if Value <> FWordWrap then
  begin
    FWordWrap := Value;
    Invalidate;
  end;
end;

procedure TJvDBGrid.SetWordWrapAllFields(Value: Boolean);
begin
  if Value <> FWordWrapAllFields then
  begin
    FWordWrapAllFields := Value;
    if WordWrap then
      Invalidate;
  end;
end;

procedure TJvDBGrid.PlaceControl(Control: TWinControl; ACol, ARow: Integer);
var
  R: TRect;
  GridControl: TJvDBGridControl;
  ClientTopLeft: TPoint;
  ParentForm: TCustomForm;
begin
  // Do not test for Assigned(Control) here or you will end
  // up with an infinite loop of error messages. This check must
  // be done in UseDefaultEditor

  if ReadOnly or (DataLink.DataSet = nil) or not (Control.Enabled and DataLink.DataSet.CanModify) then
  begin
    HideCurrentControl;
    Exit;
  end;

  if Control <> FCurrentControl then
  begin
    HideCurrentControl;
    FCurrentControl := Control;
    if FCurrentControl <> nil then
      FCurrentControl.FreeNotification(Self);
    FOldControlWndProc := FCurrentControl.WindowProc;
    FCurrentControl.WindowProc := ControlWndProc;
  end;

  if Control.Parent <> Self.Parent then
    Control.Parent := Self.Parent;

  R := CellRect(ACol, ARow);
  if ((R.Right - R.Left) < 1) or ((R.Bottom - R.Top) < 1) then
    // Cell too small to be drawn -> the control is not drawn
    Control.BoundsRect := Rect(0, 0, 0, 0)
  else
  begin
    R.TopLeft := ClientToScreen(R.TopLeft);
    R.TopLeft := TControl(Control.Parent).ScreenToClient(R.TopLeft);
    R.BottomRight := ClientToScreen(R.BottomRight);
    R.BottomRight := TControl(Control.Parent).ScreenToClient(R.BottomRight);

    // Fred: I removed this code because moving a control away from the topleft corner
    // of the cell lets appear the cell and its focus rectangle behind.

    //if Control is TCustomEdit then
    //begin
    //  { The edit control's text is not painted at good position when the control
    //    has no border }
    //  if TOpenCustomEdit(Control).BorderStyle = bsNone then
    //  begin
    //    Inc(R.Left, 2);
    //    Inc(R.Top, 2);
    //  end;
    //end;

    ClientTopLeft := TControl(Control.Parent).ScreenToClient(Self.ClientOrigin);
    GridControl := FControls.ControlByName(Control.Name);
    if GridControl.FitCell in [fcDesignSize, fcBiggest] then
    begin
      if GridControl.FitCell = fcBiggest then
      begin
        // We choose the biggest size between cell size and design size
        if GridControl.FDesignWidth = 0 then
          GridControl.FDesignWidth := Control.Width;
        if (R.Right - R.Left) > GridControl.FDesignWidth then
          Control.Width := R.Right - R.Left
        else
          Control.Width := GridControl.FDesignWidth;
        if GridControl.FDesignHeight = 0 then
          GridControl.FDesignHeight := Control.Height;
        if (R.Bottom - R.Top) > GridControl.FDesignHeight then
          Control.Height := R.Bottom - R.Top
        else
          Control.Height := GridControl.FDesignHeight;
      end;
      // Horizontal alignment of the control
      if (R.Left + Control.Width) > (ClientTopLeft.X + Self.ClientWidth) then
      begin
        Control.Left := (R.Right - Control.Width);  // Right align
        if Control.Left < ClientTopLeft.X then
          Control.Left := ClientTopLeft.X;
      end
      else
        Control.Left := R.Left;                     // Left align
      // Vertical alignment of the control
      if (R.Top + Control.Height) > (ClientTopLeft.Y + Self.ClientHeight) then
      begin
        Control.Top := (R.Bottom - Control.Height); // Bottom align
        if Control.Top < ClientTopLeft.Y then
          Control.Top := ClientTopLeft.Y;
      end
      else
        Control.Top := R.Top;                       // Top align
    end
    else
      // Control drawn at cell size
      Control.BoundsRect := R;
  end;
  Control.BringToFront;
  Control.Show;

  ParentForm := GetParentForm(Self);
  if Self.Visible and Control.Visible and (Self.Parent <> nil) and
     Self.Parent.Visible and (ParentForm <> nil) and ParentForm.Visible then
  begin
    if dgCancelOnExit in Options then
    begin // Don't cancel the empty record while moving focus
      Options := Options - [dgCancelOnExit];
      Control.SetFocus;
      Options := Options + [dgCancelOnExit];
    end
    else
      Control.SetFocus;
  end;
end;

procedure TJvDBGrid.SetControls(Value: TJvDBGridControls);
begin
  FControls.Assign(Value);
end;

procedure TJvDBGrid.HideCurrentControl;
begin
  if FCurrentControl <> nil then
  begin
    FCurrentControl.WindowProc := FOldControlWndProc;
    if FCurrentControl.HandleAllocated then
    begin
      SendMessage(FCurrentControl.Handle, WM_KILLFOCUS, 0, 0); // can free the FCurrentControl
      if FCurrentControl <> nil then
        FCurrentControl.Hide;
    end;
    if FCurrentControl <> nil then
      FCurrentControl.RemoveFreeNotification(Self);
    FCurrentControl := nil;
  end;
  FOldControlWndProc := nil;
end;

procedure TJvDBGrid.CloseControl;
begin
  { Do not hide the control if it has the focus because then the WM_KILLFOCUS
    ControlWndProc hook will hide it. }
  if not Visible or (FCurrentControl = nil) or not FCurrentControl.HandleAllocated or
     not FCurrentControl.Focused then
    HideCurrentControl;
  if Visible then
  begin
    SetFocus;
    { If the grid does not have the focus after a SetFocus, one of the executed
      CM_EXIT has failed with an exception or has set the focus to another control.
      In that case the CurrentControl is still active. }
    if (FCurrentControl <> nil) and FCurrentControl.Focused then
      Abort;
  end;
end;

procedure TJvDBGrid.ControlWndProc(var Message: TMessage);
var
  EscapeKey: Boolean;
  CurrentEditor: TJvDBGridControl;
begin
  if Message.Msg = WM_CHAR then
  begin
    if not DoKeyPress(TWMChar(Message)) then
      with TWMKey(Message) do
      begin
        CurrentEditor := FControls.ControlByName(FCurrentControl.Name);
        if (CharCode = VK_RETURN) and (PostOnEnterKey or CurrentEditor.LeaveOnEnterKey) then
        begin
          CloseControl;
          if PostOnEnterKey then
            DataSource.DataSet.CheckBrowseMode;
        end
        else
        if CharCode = VK_TAB then
        begin
          CloseControl;
          PostMessage(Handle, WM_KEYDOWN, VK_TAB, KeyData);
        end
        else
        begin
          EscapeKey := (CharCode = VK_ESCAPE);
          FOldControlWndProc(Message);
          if EscapeKey then
          begin
            CloseControl;
            if Assigned(SelectedField) then
            begin
              // OldValue is only available when State=dsEdit, otherwise it can throw an AV.
              if (SelectedField.DataSet.State = dsEdit) and (SelectedField.OldValue <> SelectedField.Value) then
                SelectedField.Value := SelectedField.OldValue
              else if (SelectedField.DataSet.State = dsInsert) and not SelectedField.IsNull then
                SelectedField.Clear;
            end;
          end;
        end;
      end;
  end
  else
  if Message.Msg = WM_KEYDOWN then
  begin
    with TWMKey(Message) do
    begin
      CurrentEditor := FControls.ControlByName(FCurrentControl.Name);
      if (CurrentEditor <> nil) and CurrentEditor.LeaveOnUpDownKey and
         ((CharCode = VK_UP) or (CharCode = VK_DOWN)) and (KeyDataToShiftState(KeyData) = []) then
      begin
        CloseControl;
        DataSource.DataSet.CheckBrowseMode;
        PostMessage(Handle, WM_KEYDOWN, CharCode, KeyData);
      end
      else
        FOldControlWndProc(Message);
    end;
  end
  else
  begin
    FOldControlWndProc(Message);
    case Message.Msg Of
      WM_GETDLGCODE:
        begin
          CurrentEditor := FControls.ControlByName(FCurrentControl.Name);
          if CurrentEditor <> nil then
          begin
            Message.Result := Message.Result or DLGC_WANTTAB;
            if CurrentEditor.LeaveOnUpDownKey then
              Message.Result := Message.Result or DLGC_WANTARROWS;
          end;
        end;
      CM_EXIT:
        HideCurrentControl;
    end;
  end;
end;

//=== { TJvSelectDialogColumnStrings } =======================================

constructor TJvSelectDialogColumnStrings.Create;
begin
  inherited Create;
  Caption := RsJvDBGridSelectTitle;
  RealNamesOption := '';//RsJvDBGridSelectOption;
  OK := RsButtonOKCaption;
  NoSelectionWarning := RsJvDBGridSelectWarning;
end;

procedure TJvDBGrid.ShowSelectColumnClick;
begin
  ShowColumnsDialog;
end;

procedure TJvDBGrid.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  {$IFDEF COMPILER9_UP}
  // The grid can only handle ssNone and ssHorizontal. We have to emulate the other modes.
  if FScrollBars = ssVertical then
    Params.Style := Params.Style and not WS_HSCROLL;
  {$ENDIF COMPILER9_UP}
end;

{$IFDEF COMPILER9_UP}
procedure TJvDBGrid.SetScrollBars(Value: TScrollStyle);
begin
  if Value <> FScrollBars then
  begin
    FScrollBars := Value;
    // The grid can only handle ssNone and ssHorizontal. We have to emulate the other modes.
    if Value in [ssVertical, ssBoth] then
      Value := ssHorizontal;

    if Value = inherited ScrollBars then
      RecreateWnd
    else
      inherited ScrollBars := Value;

    if (FScrollBars = ssVertical) and HandleAllocated then
      ShowScrollBar(Handle, SB_HORZ, False);
  end;
end;
{$ENDIF COMPILER9_UP}

procedure TJvDBGrid.SetSelectColumnsDialogStrings(const Value: TJvSelectDialogColumnStrings);
begin
  // do nothing
end;

procedure TJvDBGrid.ShowColumnsDialog;
var
  R, WorkArea: TRect;
  Frm: TfrmSelectColumn;
  Pt: TPoint;
  DefaultDialog: Boolean;
begin
  DefaultDialog := True;
  if Assigned(FOnSelectColumns) then
    FOnSelectColumns(Self, DefaultDialog);
  if DefaultDialog then
  begin
    R := CellRect(0, 0);
    Frm := TfrmSelectColumn.Create(Application);
    try
      if not IsRectEmpty(R) then
      begin
        Pt := ClientToScreen(Point(R.Left, R.Bottom + 1));
        WorkArea := Screen.MonitorFromWindow(Handle).WorkareaRect;
        { force the form the be in the working area }
        if Pt.X + Frm.Width > WorkArea.Right then
          Pt.X := WorkArea.Right - Frm.Width;
        if Pt.Y + Frm.Height > WorkArea.Bottom then
          Pt.Y := WorkArea.Bottom - Frm.Height;
        Frm.SetBounds(Pt.X, Pt.Y, Frm.Width, Frm.Height);
      end;
      Frm.Grid := Self;
      Frm.DataSource := DataLink.DataSource;
      Frm.SelectColumn := SelectColumn;
      Frm.Caption := SelectColumnsDialogStrings.Caption;
      Frm.cbWithFieldName.Caption := SelectColumnsDialogStrings.RealNamesOption;
      Frm.ButtonOK.Caption := SelectColumnsDialogStrings.OK;
      Frm.NoSelectionWarning := SelectColumnsDialogStrings.NoSelectionWarning;
      Frm.ShowModal;
    finally
      Frm.Free;
    end;
  end;
  Invalidate;
end;

procedure TJvDBGrid.SetBooleanEditor(const Value: Boolean);
begin
  if FBooleanEditor <> Value then
  begin
    FBooleanEditor := Value;
    Invalidate;
  end;
end;

procedure TJvDBGrid.SetShowMemos(const Value: Boolean);
begin
  if FShowMemos <> Value then
  begin
    FShowMemos := Value;
    Invalidate;
  end;
end;

function TJvDBGrid.GetUseXPThemes: Boolean;
begin
  {$IFDEF COMPILER14_UP}
  Result := DrawingStyle = gdsThemed;
  {$ELSE}
  Result := FUseXPThemes;
  {$ENDIF COMPILER14_UP}
end;

procedure TJvDBGrid.SetUseXPThemes(Value: Boolean);
begin
  if Value <> UseXPThemes then
  begin
    {$IFDEF COMPILER14_UP}
    if Value then
      DrawingStyle := gdsThemed
    else
      DrawingStyle := gdsClassic;
    {$ELSE}
    FUseXPThemes := Value;
    Invalidate;
    {$ENDIF COMPILER14_UP}
  end;
end;

{$IFNDEF COMPILER14_UP}
{$IFDEF JVCLThemesEnabled}
function TJvDBGrid.ColumnOffset: Integer;
begin
  if dgIndicator in Options then
    Result := IndicatorOffset
  else
    Result := 0;
end;

function TJvDBGrid.ValidCell(ACell: TGridCoord): Boolean;
begin
  Result := (ACell.X <> -1) and (ACell.Y <> -1);
end;
{$ENDIF JVCLThemesEnabled}
{$ENDIF ~COMPILER14_UP}

function TJvDBGrid.BeginColumnDrag(var Origin: Integer; var Destination: Integer; const MousePt: TPoint): Boolean;
begin
  Result := inherited BeginColumnDrag(Origin, Destination, MousePt);
  FPaintInfo.ColMoving := Result;
end;

{$IFDEF COMPILER10_UP}
procedure TJvDBGrid.WMPaint(var Message: TWMPaint);
var
  R: TRect;
begin
  if UseRightToLeftAlignment then
  begin
    { Workaround for a RightToLeft painting bug (QC #70075)
      Side effect: The grid needs more time to paint }
    R.TopLeft := ClientRect.TopLeft;
    R.BottomRight := ClientRect.BottomRight;
    Windows.InvalidateRect(Handle, @R, False);
  end;
  inherited;
end;
{$ENDIF COMPILER10_UP}

procedure TJvDBGrid.CMMouseEnter(var Message: TMessage);
{$IFNDEF COMPILER14_UP}
{$IFDEF JVCLThemesEnabled}
var
  Cell: TGridCoord;
  lPt: TPoint;
{$ENDIF JVCLThemesEnabled}
{$ENDIF ~COMPILER14_UP}
begin
  inherited;
  {$IFNDEF COMPILER14_UP}
  {$IFDEF JVCLThemesEnabled}
  lPt := Point(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  Cell := MouseCoord(lPt.X, lPt.Y);
  if UseXPThemes and StyleServices.Enabled then
    if (dgTitles in Options) and (Cell.Y = 0) then
      InvalidateCell(Cell.X, Cell.Y);
  {$ENDIF JVCLThemesEnabled}
  {$ENDIF ~COMPILER14_UP}
end;

procedure TJvDBGrid.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  {$IFNDEF COMPILER14_UP}
  {$IFDEF JVCLThemesEnabled}
  if UseXPThemes and StyleServices.Enabled then
    if ValidCell(FCell) then
      InvalidateCell(FCell.X, FCell.Y);
  {$ENDIF JVCLThemesEnabled}
  {$ENDIF ~COMPILER14_UP}
  FCell.X := -1;
  FCell.Y := -1;
  FPaintInfo.MouseInCol := -1;
  FPaintInfo.ColPressedIdx := -1;
end;

procedure TJvDBGrid.ColExit;
begin
  inherited ColExit;
  FPaintInfo.MouseInCol := -1;
  {$IFNDEF COMPILER14_UP}
  {$IFDEF JVCLThemesEnabled}
  if UseXPThemes and StyleServices.Enabled then
    if ValidCell(FCell) then
      InvalidateCell(FCell.X, FCell.Y);
  {$ENDIF JVCLThemesEnabled}
  {$ENDIF ~COMPILER14_UP}
end;

function TJvDBGrid.AllowTitleClick: Boolean;
begin
  Result := Assigned(FOnTitleBtnClick) or AutoSort;
end;

procedure TJvDBGrid.ColumnMoved(FromIndex, ToIndex: Integer);
begin
  inherited ColumnMoved(FromIndex, ToIndex);
  FPaintInfo.ColMoving := False;
  {$IFNDEF COMPILER14_UP}
  {$IFDEF JVCLThemesEnabled}
  if UseXPThemes and StyleServices.Enabled then
    Invalidate;
  {$ENDIF JVCLThemesEnabled}
  {$ENDIF ~COMPILER14_UP}
end;

procedure TJvDBGrid.MouseWheelHandler(var Message: TMessage);
var
  LastRow: Integer;
begin
  LastRow := Row;
  inherited MouseWheelHandler(Message);
  if (Row <> LastRow) and (DataLink <> nil) and DataLink.Active then
    InvalidateCell(IndicatorOffset - 1, LastRow);
end;

procedure TJvDBGrid.BeginUpdate;
begin
  BeginLayout;
end;

procedure TJvDBGrid.EndUpdate;
begin
  EndLayout;
end;

function TJvDBGrid.CellRect(ACol, ARow: Longint): TRect;
begin
  Result := inherited CellRect(ACol, ARow);
end;

procedure TJvDBGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FCurrentControl) then
  begin
    FCurrentControl.RemoveFreeNotification(Self);
    FCurrentControl := nil;
  end;
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFNDEF SUPPORTS_CLASS_CTORDTORS}
  FinalizeGridBitmaps;
  {$ENDIF ~SUPPORTS_CLASS_CTORDTORS}

  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
