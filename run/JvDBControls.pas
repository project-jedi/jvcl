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

Contributor(s):
  Polaris Software

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:

//=== NEW IN JVCL 3.0 ==

    TJvDBMaskEdit is a new control, added by Warren Postma.

    Major Issues:
        EditMask property enables operation as masked edit, which doesn't
        work properly in a Control Grid, yet, if you set the EditMask.
        You can use it as a generic editor control inside a control grid.
          -- Warren Postma (warrenpstma@hotmail.com) 

-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvDBControls;

interface

uses
  Windows, 
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF}
  Messages, Classes, Controls, Forms, Grids, Graphics, Buttons, Menus,
  StdCtrls, Mask, IniFiles, DB, DBGrids,
  JvAppStorage, JvSecretPanel, JvLabel, JvToolEdit, JvFormPlacement, JvJCLUtils, DBCtrls,
  JvMaskEdit, JvBaseEdits;

{ TJvDBGrid }

const
  DefJvGridOptions = [dgEditing, dgTitles, dgIndicator, dgColumnResize,
    dgColLines, dgRowLines, dgConfirmDelete, dgCancelOnExit];

  {$IFDEF BCB}
  {$NODEFINE DefJvGridOptions}
  {$ENDIF BCB}

type
  { NEW VALIDATION EVENT }
  TJvDbAcceptValueEvent  = procedure (Sender:TObject; oldValue:String;var newValue:String; var Accept,Post:Boolean) of Object;
   
  TTitleClickEvent = procedure(Sender: TObject; ACol: Longint;
    Field: TField) of object;
  TCheckTitleBtnEvent = procedure(Sender: TObject; ACol: Longint;
    Field: TField; var Enabled: Boolean) of object;
  TGetCellParamsEvent = procedure(Sender: TObject; Field: TField;
    AFont: TFont; var Background: TColor; Highlight: Boolean) of object;
  TSortMarker = (smNone, smDown, smUp);
  TGetBtnParamsEvent = procedure(Sender: TObject; Field: TField;
    AFont: TFont; var Background: TColor; var SortMarker: TSortMarker;
    IsDown: Boolean) of object;
  TGetCellPropsEvent = procedure(Sender: TObject; Field: TField;
    AFont: TFont; var Background: TColor) of object; { obsolete }
  TJvDBEditShowEvent = procedure(Sender: TObject; Field: TField;
    var AllowEdit: Boolean) of object;
  TDrawColumnTitleEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    ARect: TRect; Field: TField; ASortMarker: TBitmap; IsDown: Boolean;
    var Offset: integer; var DeafaultDrawText,
    DefaultDrawSortMarker: boolean) of object;

  {NEW IN JVCL3.0 - Enhanced DBEdit/DBMaskEdit }
  TJvDBMaskEdit = class(TJvCustomMaskEdit) // same base as TJvMaskEdit, plus data aware.
  private
    {Standard data-aware crap}
    FDataLink: TFieldDataLink;
    FCanvas: TControlCanvas;
    FAlignment: TAlignment;
    FFocused: Boolean;
    FBeepOnError : Boolean; { allows us to get rid of the standard beep on error yuckiness if we want}

    {new: Specific to this component}
      // value of text in the edit control at the time
      // that keyboard focus enters the control:
    FOriginalValue : String;
    // Validation/event.
    FOnAcceptNewValue: TJvDbAcceptValueEvent;
    procedure ActiveChange(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetCanvas: TCanvas;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    function GetTextMargins: TPoint;
    procedure ResetMaxLength;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetFocused(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMUndo(var Message: TMessage); message WM_UNDO;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT; 
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure Change; override;
    function EditCanModify: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Reset; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
    property Canvas: TCanvas read GetCanvas;
  published
    { Here are the common designtime properties, exactly like the VCL TDBEdit  }
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    { designtime properties SPECIFIC to only JvDBMaskEdit: }
    property EditMask; { from TJvCustomMaskEdit }

     {new event}
     // This event is fired when a new value has been entered, and the Enter key is
     // hit, and the mask checking worked, and we are asking the user
     // for whether to accept the entry, or not, and if so, the end
     // user may also want to automatically set a flag to cause an automatic Post
     // after the db control does a write to the fieldlink.
     property OnAcceptNewValue: TJvDbAcceptValueEvent read FOnAcceptNewValue write FOnAcceptNewValue;


    {Common JEDI Niceties}
    property BeepOnError : Boolean read FBeepOnError  write FBeepOnError default true; { allows us to get rid of the standard beep on error yuckiness if we want}



  end;
  {END TJvDBMaskEdit}
 
  TJvDBGrid = class(TDBGrid)
  private
    FBeepOnError : Boolean; //WAP
    FAutoAppend: Boolean; // Polaris
    FSizingIndex: Integer; // Polaris
    FSizingOfs: Integer; // Polaris
    FShowGlyphs: Boolean;
    FDefaultDrawing: Boolean;
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
    FMsIndicators: TImageList;
    FOnCheckButton: TCheckTitleBtnEvent;
    FOnGetCellProps: TGetCellPropsEvent;
    FOnGetCellParams: TGetCellParamsEvent;
    FOnGetBtnParams: TGetBtnParamsEvent;
    FOnEditChange: TNotifyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnTitleBtnClick: TTitleClickEvent;
    FOnShowEditor: TJvDBEditShowEvent;
    FOnTopLeftChanged: TNotifyEvent;
    FSelectionAnchor: TBookmarkStr;
    FOnDrawColumnTitle: TDrawColumnTitleEvent;
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
    procedure SaveColumnsLayout(const AppStorage: TJvCustomAppStorage; const Section: string);
    procedure RestoreColumnsLayout(const AppStorage: TJvCustomAppStorage; const Section: string);
    function GetOptions: TDBGridOptions;
    procedure SetOptions(Value: TDBGridOptions);
    function GetMasterColumn(ACol, ARow: Longint): TColumn;
    function GetTitleOffset: Byte;
    procedure SetFixedCols(Value: Integer);
    function GetFixedCols: Integer;
    function CalcLeftColumn: Integer;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure WMCancelMode(var Msg: TMessage); message WM_CANCELMODE;
    procedure WMRButtonUp(var Msg: TWMMouse); message WM_RBUTTONUP;
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
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure Scroll(Distance: Integer); override;
    procedure LayoutChanged; override;
    procedure TopLeftChanged; override;
    procedure DrawColumnCell(const Rect: TRect; DataCol: Integer;
      Column: TColumn; State: TGridDrawState); override;
    procedure ColWidthsChanged; override;
    procedure Paint; override;
    // Polaris
    procedure CalcSizingState(X, Y: Integer; var State: TGridState;
      var Index: Longint; var SizingPos, SizingOfs: Integer;
      var FixedInfo: TGridDrawInfo); override;
    procedure DoDrawColumnTitle(Canvas: TCanvas; ARect: TRect; AField: TField;
      ASortMarker: TBitmap; IsDown: Boolean; var Offset: integer;
      var DefaultDrawText, DefaultDrawSortMarker: boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultDataCellDraw(const Rect: TRect; Field: TField;
      State: TGridDrawState);
    procedure DisableScroll;
    procedure EnableScroll;
    function ScrollDisabled: Boolean;
    procedure MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
    procedure SelectAll;
    procedure UnselectAll;
    procedure ToggleRowSelection;
    procedure GotoSelection(Index: Longint);
    procedure LoadFromAppStorage(const AppStorage: TJvCustomAppStorage; const Path: string);
    procedure SaveToAppStorage(const AppStorage: TJvCustomAppStorage; const Path: string);
    procedure Load;
    procedure Save;
    property SelectedRows;
    property SelCount: Longint read GetSelCount;
    property Canvas;
    property Col;
    property InplaceEditor;
    property LeftCol;
    property Row;
    property VisibleRowCount;
    property VisibleColCount;
    property IndicatorOffset;
    property TitleOffset: Byte read GetTitleOffset;
  published
    property AutoAppend: Boolean read FAutoAppend write FAutoAppend default True; // Polaris
    property Options: TDBGridOptions read GetOptions write SetOptions
      default DefJvGridOptions;
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
    property OnShowEditor: TJvDBEditShowEvent read FOnShowEditor write FOnShowEditor;
    property OnTitleBtnClick: TTitleClickEvent read FOnTitleBtnClick write FOnTitleBtnClick;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnTopLeftChanged: TNotifyEvent read FOnTopLeftChanged write FOnTopLeftChanged;
    property OnDrawColumnTitle: TDrawColumnTitleEvent read FOnDrawColumnTitle write FOnDrawColumnTitle;
    property OnContextPopup;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property BeepOnError:Boolean read FBeepOnError write FBeepOnError default true; // WAP.
  end;

  TJvDBComboEdit = class(TJvCustomComboEdit)
  private
    FDataLink: TFieldDataLink;
    FCanvas: TControlCanvas;
    FFocused: Boolean;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetCanvas: TCanvas;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetFocused(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure WMCut(var Msg: TMessage); message WM_CUT;
    procedure WMPaste(var Msg: TMessage); message WM_PASTE;
    procedure CMEnter(var Msg: TCMEnter); message CM_ENTER;
    procedure CMExit(var Msg: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Msg: TMessage); message CM_GETDATALINK;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
  protected
    procedure Change; override;
    function EditCanModify: Boolean; override;
    function GetReadOnly: Boolean; override;
    procedure SetReadOnly(Value: Boolean);override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Reset; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Button;
    property Field: TField read GetField;
    property Canvas: TCanvas read GetCanvas;
  published
    //Polaris
    property Align;

    property AutoSelect;
    property BorderStyle;
    property ButtonHint;
    property CharCase;
    property ClickKey;
    property Color;
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
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property NumGlyphs;
    property ParentColor;
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
    property OnStartDrag;
    property OnContextPopup;
    property OnEndDock;
    property OnStartDock;
    (* ++ RDB ++ *)
    property ClipboardCommands;
    property DisabledTextColor;
    property DisabledColor;
    (* -- RDB -- *)
  end;

  TJvDBDateEdit = class(TJvCustomDateEdit)
  private
    FInReset: Boolean; // Polaris
    FDataLink: TFieldDataLink;
    FCanvas: TControlCanvas;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetCanvas: TCanvas;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure UpdateData(Sender: TObject);
    procedure AfterPopup(Sender: TObject; var Date: TDateTime; var Action: Boolean);
    procedure WMCut(var Msg: TMessage); message WM_CUT;
    procedure WMPaste(var Msg: TMessage); message WM_PASTE;
    procedure CMEnter(var Msg: TCMEnter); message CM_ENTER;
    procedure CMExit(var Msg: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Msg: TMessage); message CM_GETDATALINK;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
  protected
    procedure AcceptValue(const Value: Variant); override;
    procedure ApplyDate(Value: TDateTime); override;
    function GetReadOnly: Boolean; override;
    procedure SetReadOnly(Value: Boolean);override;
    procedure Change; override;
    function EditCanModify: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Reset; override;

    // Polaris
    procedure SetDate(Value: TDateTime); override;
    function IsValidDate(Value: TDateTime): Boolean;
    // Polaris
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateMask; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
    property Canvas: TCanvas read GetCanvas;
  published
    // Polaris
    property DateAutoBetween;
    property MinDate;
    property MaxDate;
    property Align;
    // Polaris
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
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property NumGlyphs;
    property ParentColor;
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
    property OnStartDrag;
    property OnContextPopup;
    property OnEndDock;
    property OnStartDock;
    (* ++ RDB ++ *)
    property ClipboardCommands;
    property DisabledTextColor;
    property DisabledColor;
    (* -- RDB -- *)

  end;

  TJvDBCalcEdit = class(TJvCalcEdit)
  private
    FDataLink: TFieldDataLink;
    FDefaultParams: Boolean;

    //Polaris
    FLEmptyIsNull,
      FEmptyIsNull: Boolean;
    procedure SetEmptyIsNull(Value: Boolean);
    function GetZeroEmpty: Boolean;
    procedure SetZeroEmpty(Value: Boolean);
    function StoreEmptyIsNull: Boolean;
    //Polaris

    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetDefaultParams(Value: Boolean);
    procedure UpdateFieldData(Sender: TObject);
    procedure WMCut(var Msg: TMessage); message WM_CUT;
    procedure WMPaste(var Msg: TMessage); message WM_PASTE;
    //Polaris procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Msg: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Msg: TMessage); message CM_GETDATALINK;
  protected
    procedure AcceptValue(const Value: Variant); override;
    function GetDisplayText: string; override;
    function GetReadOnly: Boolean; override;
    procedure SetReadOnly(Value: Boolean);override;
    procedure Change; override;

    procedure DataChanged; override; //Polaris

    function EditCanModify: Boolean; override;
    function IsValidChar(Key: Char): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Reset; override;
    procedure UpdatePopup; override;

    //Polaris
    procedure Loaded; override;
    //Polaris
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateFieldParams;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
    property Value;
  published
    //Polaris
    property Align;
    property DecimalPlaceRound;

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
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property MaxValue;
    property MinValue;
    property NumGlyphs;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupAlign;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    //Polaris
    property EmptyIsNull: Boolean read FEmptyIsNull write SetEmptyIsNull stored StoreEmptyIsNull;
    property ZeroEmpty: Boolean read GetZeroEmpty write SetZeroEmpty default True;
    //Polaris
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
    property OnStartDrag;
    property OnContextPopup;
    property OnEndDock;
    property OnStartDock;
    (* ++ RDB ++ *)
    property ClipboardCommands;
    property DisabledTextColor;
    property DisabledColor;
    (* -- RDB -- *)
  end;

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
    FDataSetName: string;
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
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
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
    property OnStartDrag;
    property OnContextPopup;
    property OnEndDock;
    property OnStartDock;
  end;

implementation

uses
  SysUtils, Dialogs, DbConsts, Math,
  JvDBUtils, JvJVCLUtils, JvCalc, JvConsts, JvResources, JvTypes;

{$R ..\resources\JvDBCtrl.res}

type
  TGridPicture = (gpBlob, gpMemo, gpPicture, gpOle, gpObject, gpData,
    gpNotEmpty, gpMarkDown, gpMarkUp);

const
  GridBmpNames: array [TGridPicture] of PChar =
  ('JV_DBG_BLOB', 'JV_DBG_MEMO', 'JV_DBG_PICT', 'JV_DBG_OLE', 'JV_DBG_OBJECT',
    'JV_DBG_DATA', 'JV_DBG_NOTEMPTY', 'JV_DBG_SMDOWN', 'JV_DBG_SMUP');

// (rom) changed to var
var
  GridBitmaps: array [TGridPicture] of TBitmap =
  (nil, nil, nil, nil, nil, nil, nil, nil, nil);

const
  bmMultiDot = 'JV_DBG_MSDOT';
  bmMultiArrow = 'JV_DBG_MSARROW';

function GetGridBitmap(BmpType: TGridPicture): TBitmap;
begin
  if GridBitmaps[BmpType] = nil then
  begin
    GridBitmaps[BmpType] := TBitmap.Create;
    GridBitmaps[BmpType].Handle := LoadBitmap(HInstance, GridBmpNames[BmpType]);
  end;
  Result := GridBitmaps[BmpType];
end;

procedure DestroyLocals; 
var
  I: TGridPicture;
begin
  for I := Low(TGridPicture) to High(TGridPicture) do
    GridBitmaps[I].Free;
end;

procedure GridInvalidateRow(Grid: TJvDBGrid; Row: Longint);
var
  I: Longint;
begin
  for I := 0 to Grid.ColCount - 1 do
    Grid.InvalidateCell(I, Row);
end;

//=== TBookmarkList ==========================================================


//=== NEW IN JVCL 3.0 ==
//=== TJvDBMaskEdit ==============================================================
procedure TJvDBMaskEdit.ResetMaxLength;
var
  F: TField;
begin
  if (MaxLength > 0) and Assigned(DataSource) and Assigned(DataSource.DataSet) then
  begin
    F := DataSource.DataSet.FindField(DataField);
    if Assigned(F) and (F.DataType in [ftString, ftWideString]) and (F.Size = MaxLength) then
      MaxLength := 0;
  end;
end;

constructor TJvDBMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnActiveChange := ActiveChange;
  // new stuff that isn't in the VCL version.
  FBeepOnError := True;
  inherited ReadOnly := True;
end;

destructor TJvDBMaskEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
  // (rom) destroy Canvas AFTER inherited Destroy
  FCanvas.Free;
end;

procedure TJvDBMaskEdit.Loaded;
begin
  inherited Loaded;
  ResetMaxLength;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TJvDBMaskEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TJvDBMaskEdit.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

procedure TJvDBMaskEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
    FDataLink.Edit;
end;

procedure TJvDBMaskEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key in [#32..#255]) and (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
    if FBeepOnError then
      MessageBeep(0);
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

function TJvDBMaskEdit.EditCanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;

procedure TJvDBMaskEdit.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;

procedure TJvDBMaskEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if (FAlignment <> taLeftJustify) and not IsMasked then Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TJvDBMaskEdit.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;

function TJvDBMaskEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvDBMaskEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TJvDBMaskEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TJvDBMaskEdit.SetDataField(const Value: string);
begin
  if not (csDesigning in ComponentState) then
    ResetMaxLength;
  FDataLink.FieldName := Value;
end;

function TJvDBMaskEdit.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

function TJvDBMaskEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TJvDBMaskEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TJvDBMaskEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TJvDBMaskEdit.ActiveChange(Sender: TObject);
begin
  ResetMaxLength;
end;

procedure TJvDBMaskEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    if FAlignment <> FDataLink.Field.Alignment then
    begin
      EditText := '';  {forces update}
      FAlignment := FDataLink.Field.Alignment;
    end;
    EditMask := FDataLink.Field.EditMask;
    if not (csDesigning in ComponentState) then
    begin
      if (FDataLink.Field.DataType in [ftString, ftWideString]) and (MaxLength = 0) then
        MaxLength := FDataLink.Field.Size;
    end;
    if FFocused and FDataLink.CanModify then
      Text := FDataLink.Field.Text
    else
    begin
      EditText := FDataLink.Field.DisplayText;
      if FDataLink.Editing {and FDataLink.FModified XXX } then
        Modified := True;
    end;
  end else
  begin
    FAlignment := taLeftJustify;
    EditMask := '';
    if csDesigning in ComponentState then
      EditText := Name else
      EditText := '';
  end;
end;

procedure TJvDBMaskEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
end;

procedure TJvDBMaskEdit.UpdateData(Sender: TObject);
begin
  ValidateEdit;
  FDataLink.Field.Text := Text;
end;

procedure TJvDBMaskEdit.WMUndo(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TJvDBMaskEdit.WMPaste(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TJvDBMaskEdit.WMCut(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TJvDBMaskEdit.CMEnter(var Message: TCMEnter);
begin
  FOriginalValue := Self.Text;
  SetFocused(True);
  inherited;
  if SysLocale.FarEast and FDataLink.CanModify then
    inherited ReadOnly := False;
end;

procedure TJvDBMaskEdit.CMExit(var Message: TCMExit);
var
 newValue:String;
 Accept,Post:Boolean;
begin
  Accept := true;
  Post := false;
  newValue := Text;
  // When we hit enter, check if there was a change, and if so,
  // we can fire the confirmation event.
  if (FOriginalValue<>newValue) then
      if Assigned(FOnAcceptNewValue) then begin


          FOnAcceptNewValue(Self,FOriginalValue,newValue,Accept,Post);
          if not Accept then begin
              Text := FOriginalValue;
          end;
      end;
  try
     if Accept then
        FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  SetFocused(False);
  CheckCursor;
  if Accept then
      DoExit;

  { A nifty little way to keep simple database applications happy.
    Just set POST flag in your validation, and the dataset is updated.
    If you don't like this feature, just DON'T set Post to true, it
    defaults to false.
  }
    if (Accept and Post) and (Assigned(DataSource)) then
       if Assigned(DataSource.DataSet) and (DataSource.DataSet.Active) then
          if DataSource.DataSet.State = dsEdit then
              DataSource.DataSet.Post;

end;

procedure TJvDBMaskEdit.WMPaint(var Message: TWMPaint);
(*const
  AlignmentValues: array[False..True, TAlignment] of TAlignment = (
    (taLeftJustify, taRightJustify, taCenter),
    (taRightJustify, taLeftJustify, taCenter)
  ); *)
const
  AlignStyle : array[Boolean, TAlignment] of DWORD =
   ((WS_EX_LEFT, WS_EX_RIGHT, WS_EX_LEFT),
    (WS_EX_RIGHT, WS_EX_LEFT, WS_EX_LEFT));
var
  Left: Integer;
  Margins: TPoint;
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
  AAlignment: TAlignment;
  ExStyle: DWORD;
begin
  if csDestroying in ComponentState then
    Exit;
  AAlignment := FAlignment;
  if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
  if ((AAlignment = taLeftJustify) or FFocused) and
    not (csPaintCopy in ControlState) then
  begin
    if SysLocale.MiddleEast and HandleAllocated and (IsRightToLeft) then
    begin { This keeps the right aligned text, right aligned }
      ExStyle := DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) and (not WS_EX_RIGHT) and
        (not WS_EX_RTLREADING) and (not WS_EX_LEFTSCROLLBAR);
      if UseRightToLeftReading then ExStyle := ExStyle or WS_EX_RTLREADING;
      if UseRightToLeftScrollbar then ExStyle := ExStyle or WS_EX_LEFTSCROLLBAR;
      ExStyle := ExStyle or
        AlignStyle[UseRightToLeftAlignment, AAlignment];
      if DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) <> ExStyle then
        SetWindowLong(Handle, GWL_EXSTYLE, ExStyle);
    end;
    // MAIN THING FOR MOST PEOPLE IS WE JUST CALL OUR BASE CLASS METHOD HERE:
    inherited; // This is where the main Non Control-Grid Paint Code lives.
    Exit;
  end;
{ Handler code here is for
  Data Aware Controls drawing themselves into their own internal
  canvas, for purpose of being displayed in a DBControl Grid:
}
  DC := Message.DC;
  if DC = 0 then DC := BeginPaint(Handle, PS);
  FCanvas.Handle := DC;
  try
    FCanvas.Font := Font;
    with FCanvas do
    begin
      R := ClientRect;
      if not (NewStyleControls and Ctl3D) and (BorderStyle = bsSingle) then
      begin
        Brush.Color := clWindowFrame;
        FrameRect(R);
        InflateRect(R, -1, -1);
      end;
      Brush.Color := Color;
      if not Enabled then
        Font.Color := clGrayText;
      if (csPaintCopy in ControlState) and (FDataLink.Field <> nil) then
      begin
        S := FDataLink.Field.DisplayText;
        case CharCase of
          ecUpperCase: S := AnsiUpperCase(S);
          ecLowerCase: S := AnsiLowerCase(S);
        end;
      end else
        S := EditText;
      if PasswordChar <> #0 then FillChar(S[1], Length(S), PasswordChar);
      Margins := GetTextMargins;
      case AAlignment of
        taLeftJustify: Left := Margins.X;
        taRightJustify: Left := ClientWidth - TextWidth(S) - Margins.X - 1;
      else
        Left := (ClientWidth - TextWidth(S)) div 2;
      end;
      if SysLocale.MiddleEast then
        UpdateTextFlags;
      TextRect(R, Left, Margins.Y, S);
    end;
  finally
    FCanvas.Handle := 0;
    if Message.DC = 0 then EndPaint(Handle, PS);
  end;
end;  

procedure TJvDBMaskEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TJvDBMaskEdit.GetTextMargins: TPoint;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  if NewStyleControls then
  begin
    if BorderStyle = bsNone then I := 0 else
      if Ctl3D then I := 1 else I := 2;
    Result.X := SendMessage(Handle, EM_GETMARGINS, 0, 0) and $0000FFFF + I;
    Result.Y := I;
  end else
  begin
    if BorderStyle = bsNone then I := 0 else
    begin
      DC := GetDC(0);
      GetTextMetrics(DC, SysMetrics);
      SaveFont := SelectObject(DC, Font.Handle);
      GetTextMetrics(DC, Metrics);
      SelectObject(DC, SaveFont);
      ReleaseDC(0, DC);
      I := SysMetrics.tmHeight;
      if I > Metrics.tmHeight then I := Metrics.tmHeight;
      I := I div 4;
    end;
    Result.X := I;
    Result.Y := I;
  end;
end;

function TJvDBMaskEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TJvDBMaskEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;


//=== TJvDBGrid ==============================================================

type
  TBookmarks = class(TBookmarkList);

constructor TJvDBGrid.Create(AOwner: TComponent);
var
  Bmp: TBitmap;
begin
  inherited Create(AOwner);
  inherited DefaultDrawing := False;
  FBeepOnError := true;
  Options := DefJvGridOptions;
  Bmp := TBitmap.Create;
  try
    Bmp.Handle := LoadBitmap(HInstance, bmMultiDot);
    FMsIndicators := TImageList.CreateSize(Bmp.Width, Bmp.Height);
    FMsIndicators.AddMasked(Bmp, clWhite);
    Bmp.Handle := LoadBitmap(HInstance, bmMultiArrow);
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
  FAutoAppend := True; // Polaris
end;

destructor TJvDBGrid.Destroy;
begin
  FIniLink.Free;
  FMsIndicators.Free;
  inherited Destroy;
end;

function TJvDBGrid.GetImageIndex(Field: TField): Integer;
var
  AOnGetText: TFieldGetTextEvent;
  AOnSetText: TFieldSetTextEvent;
begin
  Result := -1;
  if FShowGlyphs and Assigned(Field) then
  begin
    if not ReadOnly and Field.CanModify then
    begin
      { Allow editing of memo fields if OnSetText and OnGetText
        events are assigned }
      AOnGetText := Field.OnGetText;
      AOnSetText := Field.OnSetText;
      if Assigned(AOnSetText) and Assigned(AOnGetText) then
        Exit;
    end;
    case Field.DataType of
      ftBytes, ftVarBytes, ftBlob: Result := Ord(gpBlob);
      ftMemo: Result := Ord(gpMemo);
      ftGraphic: Result := Ord(gpPicture);
      ftTypedBinary: Result := Ord(gpBlob);
      ftFmtMemo: Result := Ord(gpMemo);
      ftParadoxOle, ftDBaseOle: Result := Ord(gpOle);
      ftCursor: Result := Ord(gpData);
      ftReference, ftDataSet: Result := Ord(gpData);
      ftOraClob: Result := Ord(gpMemo);
      ftOraBlob: Result := Ord(gpBlob);
    end;
  end;
end;

function TJvDBGrid.ActiveRowSelected: Boolean;
var
  Index: Integer;
begin
  Result := False;
  if MultiSelect and Datalink.Active then
  begin
    Result := SelectedRows.Find(Datalink.DataSet.Bookmark, Index);
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
    with SelectedRows do
      CurrentRowSelected := not CurrentRowSelected;
end;

function TJvDBGrid.GetSelCount: Longint;
begin
  if MultiSelect and (Datalink <> nil) and Datalink.Active then
    Result := SelectedRows.Count
  else
    Result := 0;
end;

procedure TJvDBGrid.SelectAll;
var
  ABookmark: TBookmark;
begin
  if MultiSelect and DataLink.Active then
  begin
    with Datalink.Dataset do
    begin
      if BOF and EOF then
        Exit;
      DisableControls;
      try
        ABookmark := GetBookmark;
        try
          First;
          while not EOF do
          begin
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
    Datalink.DataSet.GotoBookmark(Pointer(SelectedRows[Index]));
end;

procedure TJvDBGrid.LayoutChanged;
var
  ACol: Longint;
begin
  ACol := Col;
  inherited LayoutChanged;
  if Datalink.Active and (FixedCols > 0) then
    Col := Min(Max(CalcLeftColumn, ACol), ColCount - 1);
end;

procedure TJvDBGrid.ColWidthsChanged;
var
  ACol: Longint;
begin
  ACol := Col;
  inherited ColWidthsChanged;
  if Datalink.Active and (FixedCols > 0) then
    Col := Min(Max(CalcLeftColumn, ACol), ColCount - 1);
end;

function TJvDBGrid.CreateEditor: TInplaceEdit;
begin
  Result := inherited CreateEditor;
  TEdit(Result).OnChange := EditChanged;
end;

function TJvDBGrid.GetTitleOffset: Byte;
var
  I, J: Integer;
begin
  Result := 0;
  if dgTitles in Options then
  begin
    Result := 1;
    if (Datalink <> nil) and (Datalink.Dataset <> nil) and
      Datalink.Dataset.ObjectView then
    begin
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
    BeginUpdate;
    try
      if MultiSelect and DataLink.Active then
        if Select and (ssShift in Shift) then
        begin
          if not FSelecting then
          begin
            FSelectionAnchor := TBookmarks(SelectedRows).CurrentRow;
            SelectedRows.CurrentRowSelected := True;
            FSelecting := True;
            AddAfter := True;
          end
          else
            with TBookmarks(SelectedRows) do
            begin
              AddAfter := Compare(CurrentRow, FSelectionAnchor) <> -Direction;
              if not AddAfter then
                CurrentRowSelected := False;
            end
        end
        else
          ClearSelections;
      if Direction <> 0 then
        Datalink.DataSet.MoveBy(Direction);
      if AddAfter then
        SelectedRows.CurrentRowSelected := True;
    finally
      EndUpdate;
    end;
  end;

  procedure NextRow(Select: Boolean);
  begin
    with Datalink.Dataset do
    begin
      DoSelection(Select, 1);
      // Polaris
      if AutoAppend and EOF and CanModify and (not ReadOnly) and (dgEditing in Options) then
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
      while True do
      begin
        if GoForward then
          Inc(ACol)
        else
          Dec(ACol);
        if ACol >= ColCount then
        begin
          ClearSelections;
          ACol := IndicatorOffset;
        end
        else
        if ACol < IndicatorOffset then
        begin
          ClearSelections;
          ACol := ColCount;
        end;
        if ACol = Original then
          Exit;
        if TabStops[ACol] then
          Exit;
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
  if not Datalink.Active or not CanGridAcceptKey(Key, Shift) then
    Exit;
  with Datalink.DataSet do
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
          if not ReadOnly and CanModify and not
            IsDataSetEmpty(Datalink.DataSet) then
          begin
            if DeletePrompt then
            begin
              if SelectedRows.Count > 0 then
                SelectedRows.Delete
              else
                Delete;
            end;
            Exit;
          end;
      end
    end
    else
    begin
      case Key of
        VK_LEFT:
          if (FixedCols > 0) and not (dgRowSelect in Options) then
          begin
            if SelectedIndex <= CalcLeftColumn - IndicatorOffset then
              Exit;
          end;
        VK_HOME:
          if (FixedCols > 0) and (ColCount <> IndicatorOffset + 1) and
            not (dgRowSelect in Options) then
          begin
            SelectedIndex := CalcLeftColumn - IndicatorOffset;
            Exit;
          end;
      end;
      if Datalink.DataSet.State = dsBrowse then
      begin
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
      end;
      if ((Key in [VK_LEFT, VK_RIGHT]) and (dgRowSelect in Options)) or
        ((Key in [VK_HOME, VK_END]) and ((ColCount = IndicatorOffset + 1) or
        (dgRowSelect in Options))) or (Key in [VK_ESCAPE, VK_NEXT,
        VK_PRIOR]) or ((Key = VK_INSERT) and (CanModify and
        (not ReadOnly) and (dgEditing in Options))) then
        ClearSelections
      else
      if (Key = VK_TAB) and not (ssAlt in Shift) then
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
  if FShowGlyphs <> Value then
  begin
    FShowGlyphs := Value;
    Invalidate;
  end;
end;

procedure TJvDBGrid.SetRowsHeight(Value: Integer);
begin
  if not (csDesigning in ComponentState) and (DefaultRowHeight <> Value) then
  begin
    DefaultRowHeight := Value;
    if dgTitles in Options then
      RowHeights[0] := Value + 2;
    if HandleAllocated then
      Perform(WM_SIZE, SIZE_RESTORED, MakeLong(ClientWidth, ClientHeight));
  end;
end;

function TJvDBGrid.GetRowsHeight: Integer;
begin
  Result := DefaultRowHeight;
end;


function TJvDBGrid.GetOptions: TDBGridOptions;
begin
  Result := inherited Options;
  if FMultiSelect then
    Result := Result + [dgMultiSelect]
  else
    Result := Result - [dgMultiSelect];
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
    if F <> nil then
      Result := GetImageIndex(F) < 0;
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
  else
  if Assigned(FOnGetCellProps) then
  begin
    if Highlight then
    begin
      AColor := AFont.Color;
      FOnGetCellProps(Self, Field, AFont, ABack);
      AFont.Color := AColor;
    end
    else
      FOnGetCellProps(Self, Field, AFont, Background);
  end;
end;

procedure TJvDBGrid.DoTitleClick(ACol: Longint; AField: TField);
begin
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
  THackLink = class(TGridDataLink);

procedure TJvDBGrid.EnableScroll;
begin
  if FDisableCount <> 0 then
  begin
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
begin
  if FDisableCount = 0 then
  begin
    inherited Scroll(Distance);
  end;
end;

function TJvDBGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(OnMouseWheelDown) then
    OnMouseWheelDown(Self, Shift, MousePos, Result);
  if not Result then
  begin
    if not AcquireFocus then
      Exit;
    if Datalink.Active then
    begin
      Result := Datalink.DataSet.MoveBy(1) <> 0;
    end;
  end;
end;

function TJvDBGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(OnMouseWheelUp) then
    OnMouseWheelUp(Self, Shift, MousePos, Result);
  if not Result then
  begin
    if not AcquireFocus then
      Exit;
    if Datalink.Active then
    begin
      Result := Datalink.DataSet.MoveBy(-1) <> 0;
    end;
  end;
end;

procedure TJvDBGrid.EditChanged(Sender: TObject);
begin
  if Assigned(FOnEditChange) then
    FOnEditChange(Self);
end;

procedure TJvDBGrid.TopLeftChanged;
begin
  if (dgRowSelect in Options) and DefaultDrawing then
    GridInvalidateRow(Self, Self.Row);
  inherited TopLeftChanged;
  if FTracking then
    StopTracking;
  if Assigned(FOnTopLeftChanged) then
    FOnTopLeftChanged(Self);
end;

procedure TJvDBGrid.StopTracking;
begin
  if FTracking then
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
  NewPressed := PtInRect(Rect(0, 0, ClientWidth, ClientHeight), Point(X, Y)) and
    (FPressedCol = GetMasterColumn(Cell.X, Cell.Y)) and (Cell.Y < Offset);
  if FPressed <> NewPressed then
  begin
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
  if not AcquireFocus then
    Exit;
  if (ssDouble in Shift) and (Button = mbLeft) then
  begin
    DblClick;
    Exit;
  end;
  if Sizing(X, Y) then
    inherited MouseDown(Button, Shift, X, Y)
  else
  begin
    Cell := MouseCoord(X, Y);
    if (DragKind = dkDock) and (Cell.X < IndicatorOffset) and
      (Cell.Y < TitleOffset) and (not (csDesigning in ComponentState)) then
    begin
      BeginDrag(False);
      Exit;
    end;
    if FTitleButtons and (Datalink <> nil) and Datalink.Active and
      (Cell.Y < TitleOffset) and (Cell.X >= IndicatorOffset) and
      not (csDesigning in ComponentState) then
    begin
      if (dgColumnResize in Options) and (Button = mbRight) then
      begin
        Button := mbLeft;
        FSwapButtons := True;
        MouseCapture := True;
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
              Beep;
        Exit;
      end;
    end;
    if (Cell.X < FixedCols + IndicatorOffset) and Datalink.Active then
    begin
      if dgIndicator in Options then
        inherited MouseDown(Button, Shift, 1, Y)
      else
      if Cell.Y >= TitleOffset then
        if Cell.Y - Row <> 0 then
          Datalink.Dataset.MoveBy(Cell.Y - Row);
    end
    else
      inherited MouseDown(Button, Shift, X, Y);
    MouseDownEvent := OnMouseDown;
    if Assigned(MouseDownEvent) then
      MouseDownEvent(Self, Button, Shift, X, Y);
    if not (((csDesigning in ComponentState) or (dgColumnResize in Options)) and
      (Cell.Y < TitleOffset)) and (Button = mbLeft) then
    begin
      if MultiSelect and Datalink.Active then
        with SelectedRows do
        begin
          FSelecting := False;
          if ssCtrl in Shift then
            CurrentRowSelected := not CurrentRowSelected
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

procedure TJvDBGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FTracking then
    TrackButton(X, Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TJvDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Cell: TGridCoord;
  ACol: Longint;
  DoClick: Boolean;
begin
  if FTracking and (FPressedCol <> nil) then
  begin
    Cell := MouseCoord(X, Y);
    DoClick := PtInRect(Rect(0, 0, ClientWidth, ClientHeight), Point(X, Y)) and
      (Cell.Y < TitleOffset) and
    (FPressedCol = GetMasterColumn(Cell.X, Cell.Y));
    StopTracking;
    if DoClick then
    begin
      ACol := Cell.X;
      if dgIndicator in Options then
        Dec(ACol);
      if (DataLink <> nil) and DataLink.Active and (ACol >= 0) and
        (ACol < Columns.Count) then
      begin
        DoTitleClick(FPressedCol.Index, FPressedCol.Field);
      end;
    end;
  end
  else
  if FSwapButtons then
  begin
    FSwapButtons := False;
    MouseCapture := False;
    if Button = mbRight then
      Button := mbLeft;
  end;
  // Polaris
  if (Button = mbLeft) and (FGridState = gsColSizing) and
    (FSizingIndex + Byte(not (dgIndicator in Options)) <= FixedCols) then
  begin
    ColWidths[FSizingIndex] := X - FSizingOfs - CellRect(FSizingIndex, 0).Left;
    FGridState := gsNormal;
    Exit
  end;
  // Polaris
  inherited MouseUp(Button, Shift, X, Y);
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
      TJvHack(Form).DoKeyPress(Msg) then
      Exit;
    with Msg do
    begin
      if Assigned(FOnKeyPress) then
      begin
        Ch := Char(CharCode);
        FOnKeyPress(Self, Ch);
        CharCode := Word(Ch);
      end;
      if Char(CharCode) = #0 then
        Exit;
    end;
    Result := False;
  end;

begin
  if EditorMode or not DoKeyPress(Msg) then
    inherited;
end;

procedure TJvDBGrid.KeyPress(var Key: Char);
begin
  if EditorMode then
    inherited OnKeyPress := FOnKeyPress;
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

function TJvDBGrid.GetMasterColumn(ACol, ARow: Longint): TColumn;
begin
  if dgIndicator in Options then
    Dec(ACol, IndicatorOffset);
  if (Datalink <> nil) and Datalink.Active and (ACol >= 0) and (ACol < Columns.Count) then
  begin
    Result := Columns[ACol];
    Result := ColumnAtDepth(Result, ARow);
  end
  else
    Result := nil;
end;

procedure TJvDBGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);

  function CalcTitleRect(Col: TColumn; ARow: Integer; var MasterCol: TColumn): TRect;
    { copied from Inprise's DbGrids.pas }
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
    if (TextRect.Right - TextRect.Left) > I then
    begin
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
  MasterCol: TColumn;
  InBiDiMode: Boolean;
  DrawColumn: TColumn;
  DefaultDrawText, DefaultDrawSortMarker: boolean;
const
  EdgeFlag: array [Boolean] of UINT = (BDR_RAISEDINNER, BDR_SUNKENINNER);
begin
  if gdFixed in AState then
    Canvas.Brush.Color := FixedColor;
  inherited DrawCell(ACol, ARow, ARect, AState);
  InBiDiMode := Canvas.CanvasOrientation = coRightToLeft;
  if (dgIndicator in Options) and (ACol = 0) and (ARow - TitleOffset >= 0) and
    MultiSelect and (DataLink <> nil) and DataLink.Active and
    (Datalink.DataSet.State = dsBrowse) then
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
      Datalink.ActiveRecord := ARow - TitleOffset;
      MultiSelected := ActiveRowSelected;
    finally
      Datalink.ActiveRecord := OldActive;
    end;
    if MultiSelected then
    begin
      if ARow - TitleOffset <> Datalink.ActiveRecord then
        Indicator := 0
      else
        Indicator := 1; { multiselected and current row }
      FMsIndicators.BkColor := FixedColor;
      ALeft := FixRect.Right - FMsIndicators.Width - FrameOffs;
      if InBiDiMode then
        Inc(ALeft);
      FMsIndicators.Draw(Self.Canvas, ALeft, (FixRect.Top +
        FixRect.Bottom - FMsIndicators.Height) shr 1, Indicator);
    end;
  end
  else
  if not (csLoading in ComponentState) and
    (FTitleButtons or (FixedCols > 0)) and
    (gdFixed in AState) and (dgTitles in Options) and (ARow < TitleOffset) then
  begin
    SavePen := Canvas.Pen.Color;
    try
      Canvas.Pen.Color := clWindowFrame;
      if dgIndicator in Options then
        Dec(ACol, IndicatorOffset);
      AField := nil;
      SortMarker := smNone;
      if (Datalink <> nil) and Datalink.Active and (ACol >= 0) and
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
      if TitleRect.Right < ARect.Right then
        TitleRect.Right := ARect.Right;
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
      if FTitleButtons or ([dgRowLines, dgColLines] * Options =
        [dgRowLines, dgColLines]) then
      begin
        DrawEdge(Canvas.Handle, TitleRect, EdgeFlag[Down], BF_BOTTOMRIGHT);
        DrawEdge(Canvas.Handle, TitleRect, EdgeFlag[Down], BF_TOPLEFT);
        InflateRect(TitleRect, -1, -1);
      end;
      Canvas.Font := TitleFont;
      Canvas.Brush.Color := FixedColor;
      if DrawColumn <> nil then
      begin
        Canvas.Font := DrawColumn.Title.Font;
        Canvas.Brush.Color := DrawColumn.Title.Color;
      end;
      if FTitleButtons and (AField <> nil) and Assigned(FOnGetBtnParams) then
      begin
        BackColor := Canvas.Brush.Color;
        FOnGetBtnParams(Self, AField, Canvas.Font, BackColor, SortMarker, Down);
        Canvas.Brush.Color := BackColor;
      end;
      if Down then
      begin
        Inc(TitleRect.Left);
        Inc(TitleRect.Top);
      end;
      ARect := TitleRect;
      if (DataLink = nil) or not DataLink.Active then
        Canvas.FillRect(TitleRect)
      else
      if DrawColumn <> nil then
      begin
        case SortMarker of
          smDown: Bmp := GetGridBitmap(gpMarkDown);
          smUp: Bmp := GetGridBitmap(gpMarkUp);
        else
          Bmp := nil;
        end;
        if Bmp <> nil then
          Indicator := Bmp.Width + 6
        else
          Indicator := 1;
        DefaultDrawText:=True;
        DefaultDrawSortMarker:=True;
        DoDrawColumnTitle(Canvas,TitleRect,AField,Bmp,Down,Indicator,
          DefaultDrawText,DefaultDrawSortMarker);
        TextRect := TitleRect;
        if DefaultDrawText then
          begin
            if DrawColumn.Expandable then
              DrawExpandBtn(TitleRect, TextRect, InBiDiMode, DrawColumn.Expanded);
            with DrawColumn.Title do
              DrawCellText(Self, ACol, ARow, MinimizeText(Caption, Canvas,
                RectWidth(TextRect) - Indicator), TextRect, Alignment,
                vaCenterJustify, IsRightToLeft);
          end;
        if DefaultDrawSortMarker then
          begin
            if Bmp <> nil then
            begin
    //          ALeft := TitleRect.Right - Bmp.Width - 3;
              ALeft := TitleRect.Right - Indicator + 3;
              if Down then
                Inc(ALeft);
              if IsRightToLeft then
                ALeft := TitleRect.Left + 3;
              if (ALeft > TitleRect.Left) and (ALeft + Bmp.Width < TitleRect.Right) then
                DrawBitmapTransparent(Canvas, ALeft, (TitleRect.Bottom +
                  TitleRect.Top - Bmp.Height) div 2, Bmp, clFuchsia);
            end;
          end;
      end
      else
        DrawCellText(Self, ACol, ARow, '', ARect, taLeftJustify, vaCenterJustify);
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
  Field: TField;
begin
  Field := Column.Field;
  if Assigned(DataSource) and Assigned(DataSource.Dataset) and DataSource.Dataset.Active and
    (SelectedRows.IndexOf(DataSource.DataSet.Bookmark) > -1) then
    Include(State, gdSelected);
  NewBackgrnd := Canvas.Brush.Color;
  Highlight := (gdSelected in State) and ((dgAlwaysShowSelection in Options) or
    Focused);
  GetCellProps(Field, Canvas.Font, NewBackgrnd, Highlight or ActiveRowSelected);
  Canvas.Brush.Color := NewBackgrnd;
  if FDefaultDrawing then
  begin
    I := GetImageIndex(Field);
    if I >= 0 then
    begin
      Bmp := GetGridBitmap(TGridPicture(I));
      Canvas.FillRect(Rect);
      DrawBitmapTransparent(Canvas, (Rect.Left + Rect.Right - Bmp.Width) div 2,
        (Rect.Top + Rect.Bottom - Bmp.Height) div 2, Bmp, clOlive);
    end
    else
      DefaultDrawColumnCell(Rect, DataCol, Column, State);
  end;
  if Columns.State = csDefault then
    inherited DrawDataCell(Rect, Field, State);
  inherited DrawColumnCell(Rect, DataCol, Column, State);
  if FDefaultDrawing and Highlight and not (csDesigning in ComponentState)
    and not (dgRowSelect in Options)
    and (ValidParentForm(Self).ActiveControl = Self) then
    Canvas.DrawFocusRect(Rect);
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
  if Assigned (AppStorage) then
  begin
    AppStorage.DeleteSubTree(SectionName);
    with Columns do
    begin
      for I := 0 to Count - 1 do
      begin
        AppStorage.WriteString(AppStorage.ConcatPaths([SectionName, Format('%s.%s', [Name, Items[I].FieldName])]),
          Format('%d,%d', [Items[I].Index, Items[I].Width]));
      end;
    end;
  end;
end;

procedure TJvDBGrid.RestoreColumnsLayout(const AppStorage: TJvCustomAppStorage;
  const Section: string);
type
  TColumnInfo = record
    Column: TColumn;
    EndIndex: Integer;
  end;
  PColumnArray = ^TColumnArray;
  TColumnArray = array [0..0] of TColumnInfo;
const
  Delims = [' ', ','];
var
  I, J: Integer;
  SectionName, S: string;
  ColumnArray: PColumnArray;
begin
  if Section <> '' then
    SectionName := Section
  else
    SectionName := GetDefaultSection(Self);
  if Assigned(AppStorage) then
    with Columns do
    begin
      ColumnArray := AllocMemo(Count * SizeOf(TColumnInfo));
      try
        for I := 0 to Count - 1 do
        begin
          S := AppStorage.ReadString(AppStorage.ConcatPaths([SectionName,
            Format('%s.%s', [Name, Items[I].FieldName])]));
          ColumnArray^[I].Column := Items[I];
          ColumnArray^[I].EndIndex := Items[I].Index;
          if S <> '' then
          begin
            ColumnArray^[I].EndIndex := StrToIntDef(ExtractWord(1, S, Delims),
              ColumnArray^[I].EndIndex);
            Items[I].Width := StrToIntDef(ExtractWord(2, S, Delims),
              Items[I].Width);
          end;
        end;
        for I := 0 to Count - 1 do
        begin
          for J := 0 to Count - 1 do
          begin
            if ColumnArray^[J].EndIndex = I then
            begin
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

procedure TJvDBGrid.LoadFromAppStorage(const AppStorage: TJvCustomAppStorage; const Path: string);
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

procedure TJvDBGrid.SaveToAppStorage(const AppStorage: TJvCustomAppStorage; const Path: string);
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
  if (Name <> '') and IniStorage.IsActive then
  begin
    if StoreColumns then
      Section := IniStorage.AppStorage.ConcatPaths([IniStorage.AppStoragePath, GetDefaultSection(Self)])
    else
      if (DataSource <> nil) and
      (DataSource.DataSet <> nil) then
      Section := IniStorage.AppStorage.ConcatPaths([IniStorage.AppStoragePath, DataSetSectionName(DataSource.DataSet)])
    else
      Section := '';
    SaveToAppStorage(IniStorage.AppStorage, Section);
  end;
end;

procedure TJvDBGrid.IniLoad(Sender: TObject);
var
  Section: string;
begin
  if (Name <> '') and IniStorage.IsActive then
  begin
    if StoreColumns then
      Section := IniStorage.AppStorage.ConcatPaths([IniStorage.AppStoragePath, GetDefaultSection(Self)])
    else
      if (DataSource <> nil) and
      (DataSource.DataSet <> nil) then
      Section := IniStorage.AppStorage.ConcatPaths([IniStorage.AppStoragePath, DataSetSectionName(DataSource.DataSet)])
    else
      Section := '';
    LoadFromAppStorage(IniStorage.AppStorage, Section);
  end;
end;

// Polaris
procedure TJvDBGrid.CalcSizingState(X, Y: Integer; var State: TGridState;
  var Index: Longint; var SizingPos, SizingOfs: Integer;
  var FixedInfo: TGridDrawInfo);
var
  Coord: TGridCoord;
begin
  inherited CalcSizingState(X, Y, State, Index, SizingPos, SizingOfs, FixedInfo);

  // do nothing if not authorized to size columns
  if not (dgColumnResize in Options) then
    exit;

  if (State = gsNormal) and (Y <= RowHeights[0]) then
  begin
    Coord := MouseCoord(X, Y);
    CalcDrawInfo(FixedInfo);
    if (CellRect(Coord.X, 0).Right - 5 < X) then
    begin
      State := gsColSizing;
      Index := Coord.X;
      SizingPos := X;
      SizingOfs := X - CellRect(Coord.X, 0).Right;
    end;
    if (CellRect(Coord.X, 0).Left + 5 > X) then
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

procedure TJvDBGrid.DoDrawColumnTitle(Canvas: TCanvas; ARect: TRect;
  AField: TField; ASortMarker: TBitmap; IsDown: Boolean; var Offset: integer;
  var DefaultDrawText, DefaultDrawSortMarker: boolean);
begin
  if Assigned(FOnDrawColumnTitle) then
    begin
      FOnDrawColumnTitle(Self,Canvas,ARect,AField,ASortMarker,IsDown,Offset,
        DefaultDrawText,DefaultDrawSortMarker);
    end;
end;

//=== TJvDBComboEdit =========================================================

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
        (F.Size = MaxLength) then
        MaxLength := 0;
    end;
end;

constructor TJvDBComboEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  inherited SetReadOnly(True);
  AlwaysEnable := True;
end;

destructor TJvDBComboEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
  // (rom) destroy Canvas AFTER inherited Destroy
  FCanvas.Free;
end;

procedure TJvDBComboEdit.Loaded;
begin
  inherited Loaded;
  ResetMaxLength(Self);
  if csDesigning in ComponentState then
    DataChange(Self);
end;

procedure TJvDBComboEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;
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
  if FFocused <> Value then
  begin
    FFocused := Value;
    if (Alignment <> taLeftJustify) and not IsMasked then
      Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TJvDBComboEdit.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;

function TJvDBComboEdit.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

function TJvDBComboEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvDBComboEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

function TJvDBComboEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TJvDBComboEdit.SetDataField(const Value: string);
begin
  if not (csDesigning in ComponentState) then
    ResetMaxLength(Self);
  FDataLink.FieldName := Value;
end;

function TJvDBComboEdit.GetReadOnly: Boolean;
begin
  if FDataLink <> nil then
    Result := FDataLink.ReadOnly
  else
    Result := true;
end;

procedure TJvDBComboEdit.SetReadOnly(Value: Boolean);
begin
  if FDataLink <> nil then
    FDataLink.ReadOnly := Value;
end;

function TJvDBComboEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TJvDBComboEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    if Alignment <> FDataLink.Field.Alignment then
    begin
      EditText := ''; {forces update}
      Alignment := FDataLink.Field.Alignment;
    end;
    EditMask := FDataLink.Field.EditMask;
    if not (csDesigning in ComponentState) then
    begin
      if (FDataLink.Field.DataType = ftString) and (MaxLength = 0) then
        MaxLength := FDataLink.Field.Size;
    end;
    if FFocused and FDataLink.CanModify then
      Text := FDataLink.Field.Text
    else
    begin
      EditText := FDataLink.Field.DisplayText;
      {if FDataLink.Editing then Modified := True;}
    end;
  end
  else
  begin
    Alignment := taLeftJustify;
    EditMask := '';
    if csDesigning in ComponentState then
      EditText := Name
    else
      EditText := '';
  end;
end;

procedure TJvDBComboEdit.EditingChange(Sender: TObject);
begin
  inherited SetReadOnly(not FDataLink.Editing);
end;

procedure TJvDBComboEdit.UpdateData(Sender: TObject);
begin
  ValidateEdit;
  FDataLink.Field.Text := Text;
end;

procedure TJvDBComboEdit.WMPaste(var Msg: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TJvDBComboEdit.WMCut(var Msg: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TJvDBComboEdit.CMEnter(var Msg: TCMEnter);
begin
  SetFocused(True);
  inherited;
  if SysLocale.FarEast and FDataLink.CanModify then
    inherited SetReadOnly(False);
end;

procedure TJvDBComboEdit.CMExit(var Msg: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SelectAll;
    if CanFocus then
      SetFocus;
    raise;
  end;
  SetFocused(False);
  CheckCursor;
  DoExit;
end;

procedure TJvDBComboEdit.WMPaint(var Msg: TWMPaint);
var
  S: string;
begin
  if csDestroying in ComponentState then
    Exit;
  if (csPaintCopy in ControlState) and (FDataLink.Field <> nil) then
  begin
    S := FDataLink.Field.DisplayText;
    case CharCase of
      ecUpperCase:
        S := AnsiUpperCase(S);
      ecLowerCase:
        S := AnsiLowerCase(S);
    end;
  end
  else
    S := EditText;
  if not PaintComboEdit(Self, S, Alignment, True, FCanvas, Msg) then
    inherited;
end;

procedure TJvDBComboEdit.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := Integer(FDataLink);
end;

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

//=== TJvDBDateEdit ==========================================================

constructor TJvDBDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  FInReset := False; // Polaris
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  Self.OnAcceptDate := AfterPopup;
  AlwaysEnable := True;
  inherited SetReadOnly(True);
  UpdateMask;
end;

destructor TJvDBDateEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
  // (rom) destroy Canvas AFTER inherited Destroy
  FCanvas.Free;
end;

procedure TJvDBDateEdit.AfterPopup(Sender: TObject; var Date: TDateTime;
  var Action: Boolean);
begin
  Action := Action and (DataSource <> nil) and (DataSource.DataSet <> nil) and
    DataSource.DataSet.CanModify;
  if Action then
    Action := EditCanModify;
end;

procedure TJvDBDateEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then
    DataSource := nil;
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
    not (Key in DigitChars) and (Key <> DateSeparator) then
  begin
    Beep;
    Key := #0;
  end;
  case Key of
    ^H, ^V, ^X, '0'..'9':
      FDataLink.Edit;
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
  FInReset := True; // Polaris
  try
    FDataLink.Reset;
    SelectAll;
  finally
    FInReset := False; // Polaris
  end;
end;

// Polaris begin

function TJvDBDateEdit.IsValidDate(Value: TDateTime): Boolean;
begin
  Result := FDateAutoBetween;
  if not Result then
    if not FInReset and FDataLink.Editing then
    try
      if Value <> NullDate then
      begin
        if ((MinDate <> NullDate) and (MaxDate <> NullDate) and
          ((Value < MinDate) or (Value > MaxDate))) then
          raise EJVCLException.CreateFmt(RsEDateOutOfRange, [FormatDateTime(GetDateFormat, Value),
            FormatDateTime(GetDateFormat, MinDate), FormatDateTime(GetDateFormat, MaxDate)])
        else
        if ((MinDate <> NullDate) and (Value < MinDate)) then
          raise EJVCLException.CreateFmt(RsEDateOutOfMin, [FormatDateTime(GetDateFormat, Value),
            FormatDateTime(GetDateFormat, MinDate)])
        else
        if ((MaxDate <> NullDate) and (Value > MaxDate)) then
          raise EJVCLException.CreateFmt(RsEDateOutOfMax, [FormatDateTime(GetDateFormat, Value),
            FormatDateTime(GetDateFormat, MaxDate)]);
      end;
      Result := True;
    except
      Reset;
      raise;
    end;
end;

procedure TJvDBDateEdit.SetDate(Value: TDateTime);
begin
  IsValidDate(Value);
  inherited SetDate(Value);
end;

// Polaris end

procedure TJvDBDateEdit.Change;
begin
  if not Formatting then
    FDataLink.Modified;
  inherited Change;
end;

function TJvDBDateEdit.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

function TJvDBDateEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvDBDateEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
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
  if FDataLink.Field <> nil then
  begin
    EditMask := GetDateMask;
    // Polaris
    inherited SetDate(FDataLink.Field.AsDateTime);
    //    Self.Date := FDataLink.Field.AsDateTime;
    //    SetDate(FDataLink.Field.AsDateTime);
    // Polaris
  end
  else
  begin
    if csDesigning in ComponentState then
    begin
      EditMask := '';
      EditText := Name;
    end
    else
    begin
      EditMask := GetDateMask;
      if DefaultToday then
        Date := SysUtils.Date
      else
        Date := NullDate;
    end;
  end;
end;

procedure TJvDBDateEdit.EditingChange(Sender: TObject);
begin
  inherited SetReadOnly(not FDataLink.Editing);
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
  begin // Polaris
    if Int(FDataLink.Field.AsDateTime) <> D then
      FDataLink.Field.AsDateTime := D + Frac(FDataLink.Field.AsDateTime)
  end
  else
    FDataLink.Field.Clear;
end;

procedure TJvDBDateEdit.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := Integer(FDataLink);
end;

procedure TJvDBDateEdit.WMPaint(var Msg: TWMPaint);
var
  S: string;
begin
  if csDestroying in ComponentState then
    Exit;
  if (csPaintCopy in ControlState) and (FDataLink.Field <> nil) then
  begin
    if FDataLink.Field.IsNull then
    begin
      S := GetDateFormat;
      S := ReplaceStr(ReplaceStr(ReplaceStr(ReplaceStr(S, '/', DateSeparator),
        'Y', ' '), 'M', ' '), 'D', ' ');
    end
    else
      S := FormatDateTime(GetDateFormat, FDataLink.Field.AsDateTime);
  end
  else
    S := EditText;
  if not PaintComboEdit(Self, S, Alignment, True, FCanvas, Msg) then
    inherited;
end;

procedure TJvDBDateEdit.AcceptValue(const Value: Variant);
begin
  if VarIsNull(Value) or VarIsEmpty(Value) then
    FDataLink.Field.Clear
  else
    FDataLink.Field.AsDateTime :=
      VarToDateTime(Value) + Frac(FDataLink.Field.AsDateTime);
  DoChange;
end;

procedure TJvDBDateEdit.ApplyDate(Value: TDateTime);
begin
  FDataLink.Edit;
  inherited ApplyDate(Value);
end;

procedure TJvDBDateEdit.WMPaste(var Msg: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TJvDBDateEdit.WMCut(var Msg: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TJvDBDateEdit.CMEnter(var Msg: TCMEnter);
begin
  inherited;
end;

procedure TJvDBDateEdit.CMExit(var Msg: TCMExit);
begin
  try
    if not (csDesigning in ComponentState) and CheckOnExit then
      CheckValidDate;
    FDataLink.UpdateRecord;
  except
    SelectAll;
    if CanFocus then
      SetFocus;
    raise;
  end;
  CheckCursor;
  DoExit;
end;

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

//Polaris

//=== TJvDBCalcEdit ==========================================================

constructor TJvDBCalcEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  //Polaris
  FEmptyIsNull := ZeroEmpty;
  FLEmptyIsNull := True;
  //Polaris
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateFieldData;
  inherited SetReadOnly(True);
  AlwaysEnable := True;
end;

destructor TJvDBCalcEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TJvDBCalcEdit.Loaded;
begin
  inherited Loaded;
  FLEmptyIsNull := True;
end;

procedure TJvDBCalcEdit.SetEmptyIsNull(Value: Boolean);
begin
  if Value <> FEmptyIsNull then
  begin
    FEmptyIsNull := Value;
    if csLoading in ComponentState then
      FLEmptyIsNull := False;
  end;
end;

function TJvDBCalcEdit.GetZeroEmpty: Boolean;
begin
  Result := inherited ZeroEmpty;
end;

procedure TJvDBCalcEdit.SetZeroEmpty(Value: Boolean);
begin
  inherited ZeroEmpty := Value;
  if FLEmptyIsNull then
    SetEmptyIsNull(ZeroEmpty)
end;

function TJvDBCalcEdit.StoreEmptyIsNull: Boolean;
begin
  Result := FEmptyIsNull <> ZeroEmpty;
end;

//Polaris

procedure TJvDBCalcEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TJvDBCalcEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if not ReadOnly and ((Key = VK_DELETE) or ((Key = VK_INSERT)
    and (ssShift in Shift))) then
    FDataLink.Edit;
end;

procedure TJvDBCalcEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    ^H, ^V, ^X, #32..#255:
      if not PopupVisible then
        FDataLink.Edit;
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

function TJvDBCalcEdit.GetDisplayText: string;
var
  E: Extended;
begin
  if (csPaintCopy in ControlState) and (FDatalink.Field <> nil) then
  begin
    if FDataLink.Field.IsNull then
      E := 0.0
    else
    if FDataLink.Field.DataType in [ftSmallint, ftInteger, ftWord] then
      E := FDataLink.Field.AsInteger
    else
    if FDataLink.Field.DataType = ftBoolean then
      E := Ord(FDataLink.Field.AsBoolean)
    else
    if FDataLink.Field is TLargeintField then
      E := TLargeintField(FDataLink.Field).AsLargeInt
    else
      E := FDataLink.Field.AsFloat;
    if FDataLink.Field.IsNull then
      Result := ''
    else
      Result := FormatDisplayText(E);
  end
  else
  begin
    if FDataLink.Field = nil then
    begin
      if csDesigning in ComponentState then
        Result := Format('(%s)', [Name])
      else
        Result := '';
    end
    else {//Polaris Result := inherited GetDisplayText;}
    if FDataLink.Field.IsNull then
      Result := ''
    else
      Result := inherited GetDisplayText;
    //Polaris
  end;
end;

procedure TJvDBCalcEdit.Reset;
begin
  FDataLink.Reset;
  inherited Reset;
end;

procedure TJvDBCalcEdit.Change;
begin
  if not Formatting then
    FDataLink.Modified;
  inherited Change;
end;

//Polaris

procedure TJvDBCalcEdit.DataChanged;
begin
  inherited;
  if Assigned(FDataLink) and Assigned(FDataLink.Field) {and DecimalPlaceRound} then
  begin
    EditText := DisplayText;
    try
      if EditText <> '' then
        if (StrToFloat(EditText) = 0) and ZeroEmpty then
          EditText := '';
    except
    end;
  end;
end;
//Polaris

function TJvDBCalcEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvDBCalcEdit.SetDataSource(Value: TDataSource);
begin
  if FDataLink.DataSource <> Value then
  begin
    if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
      FDataLink.DataSource := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
    UpdateFieldParams;
  end;
end;

function TJvDBCalcEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TJvDBCalcEdit.SetDataField(const Value: string);
begin
  if FDataLink.FieldName <> Value then
  begin
    FDataLink.FieldName := Value;
    UpdateFieldParams;
  end;
end;

procedure TJvDBCalcEdit.SetDefaultParams(Value: Boolean);
begin
  if DefaultParams <> Value then
  begin
    FDefaultParams := Value;
    if FDefaultParams then
      UpdateFieldParams;
  end;
end;

procedure TJvDBCalcEdit.UpdateFieldParams;
begin
  if FDatalink.Field <> nil then
  begin
    if FDatalink.Field is TNumericField then
    begin
      if TNumericField(FDatalink.Field).DisplayFormat <> '' then
        DisplayFormat := TNumericField(FDatalink.Field).DisplayFormat;
      Alignment := TNumericField(FDatalink.Field).Alignment;
    end;
    if FDatalink.Field is TLargeintField then
    begin
      MaxValue := TLargeintField(FDatalink.Field).MaxValue;
      MinValue := TLargeintField(FDatalink.Field).MinValue;
      DecimalPlaces := 0;
      if DisplayFormat = '' then
        DisplayFormat := ',#';
    end
    else
    if FDatalink.Field is TIntegerField then
    begin
      MaxValue := TIntegerField(FDatalink.Field).MaxValue;
      MinValue := TIntegerField(FDatalink.Field).MinValue;
      DecimalPlaces := 0;
      if DisplayFormat = '' then
        DisplayFormat := ',#';
    end
    else
    if FDatalink.Field is TBCDField then
    begin
      MaxValue := TBCDField(FDatalink.Field).MaxValue;
      MinValue := TBCDField(FDatalink.Field).MinValue;
    end
    else
    if FDatalink.Field is TFloatField then
    begin
      MaxValue := TFloatField(FDatalink.Field).MaxValue;
      MinValue := TFloatField(FDatalink.Field).MinValue;
        //Polaris      DecimalPlaces := TFloatField(FDatalink.Field).Precision;
      DecimalPlaces := Min(DecimalPlaces, TFloatField(FDatalink.Field).Precision);
    end
    else
    if FDatalink.Field is TBooleanField then
    begin
      MinValue := 0;
      MaxValue := 1;
      DecimalPlaces := 0;
      if DisplayFormat = '' then
        DisplayFormat := ',#';
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
  if FDefaultParams then
    UpdateFieldParams;
  if FDataLink.Field <> nil then
  begin
    if FDataLink.Field.IsNull then
    begin
      Self.Value := 0.0;
      EditText := '';
    end
    else
    if FDataLink.Field.DataType in [ftSmallint, ftInteger, ftWord] then
      Self.AsInteger := FDataLink.Field.AsInteger
    else
    if FDataLink.Field.DataType = ftBoolean then
      Self.AsInteger := Ord(FDataLink.Field.AsBoolean)
    else
    if FDataLink.Field is TLargeintField then
      Self.Value := TLargeintField(FDataLink.Field).AsLargeInt
    else
      Self.Value := FDataLink.Field.AsFloat;
    DataChanged;
  end
  else
  begin
    if csDesigning in ComponentState then
    begin
      Self.Value := 0;
      EditText := Format('(%s)', [Name]);
    end
    else
      Self.Value := 0;
  end;
end;

procedure TJvDBCalcEdit.EditingChange(Sender: TObject);
begin
  inherited SetReadOnly(not FDataLink.Editing);
end;

procedure TJvDBCalcEdit.UpdateFieldData(Sender: TObject);
begin
  inherited UpdateData;
  //Polaris  if (Value = 0) and ZeroEmpty then FDataLink.Field.Clear
  if (Trim(Text) = EmptyStr) and FEmptyIsNull then
    FDataLink.Field.Clear
      //if (Value = 0) and ZeroEmpty then
//  FDataLink.Field.Clear
  else
  if FDataLink.Field.DataType in [ftSmallint, ftInteger, ftWord] then
    FDataLink.Field.AsInteger := Self.AsInteger
  else
  if FDataLink.Field.DataType = ftBoolean then
    FDataLink.Field.AsBoolean := Boolean(Self.AsInteger)
  else
    FDataLink.Field.AsFloat := Self.Value;
end;

procedure TJvDBCalcEdit.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := Integer(FDataLink);
end;

procedure TJvDBCalcEdit.AcceptValue(const Value: Variant);
begin
  if VarIsNull(Value) or VarIsEmpty(Value) then
    FDataLink.Field.Clear
  else
    FDataLink.Field.Value := CheckValue(Value, False);
  DoChange;
end;

procedure TJvDBCalcEdit.WMPaste(var Msg: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TJvDBCalcEdit.WMCut(var Msg: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

// Polaris
{procedure TJvDBCalcEdit.CMEnter(var Msg: TCMEnter);
begin
  inherited;
end;}

procedure TJvDBCalcEdit.CMExit(var Msg: TCMExit);
begin
  try
    CheckRange;
    FDataLink.UpdateRecord;
  except
    SelectAll;
    if CanFocus then
      SetFocus;
    raise;
  end;
  inherited;
end;

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

//=== TJvStatusDataLink ======================================================

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
  if FLabel <> nil then
    FLabel.UpdateData;
end;

procedure TJvStatusDataLink.LayoutChanged;
begin
  if (FLabel <> nil) and (FLabel.Style <> lsRecordSize) then
    DataSetChanged; { ??? }
end;

//=== TJvDBStatusLabel =======================================================

const
  GlyphSpacing = 2;
  GlyphColumns = 7;

constructor TJvDBStatusLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ShadowSize := 0;
  Layout := tlCenter;
  ControlStyle := ControlStyle - [csSetCaption , csReplicatable];
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
  else
    Result := inherited GetDefaultFontColor;
end;

function TJvDBStatusLabel.GetLabelCaption: string;
begin
  if (csDesigning in ComponentState) and ((FStyle = lsState) or
    (FDatalink = nil) or not FDatalink.Active) then
    Result := Format('(%s)', [Name])
  else
  if (FDatalink = nil) or (DataSource = nil) then
    Result := ''
  else
  begin
    case FStyle of
      lsState:
        if FShowOptions in [doCaption, doBoth] then
        begin
          if DataSetName = '' then
            Result := GetCaption(DataSource.State)
          else
            Result := Format('%s: %s', [DataSetName, GetCaption(DataSource.State)]);
        end
        else { doGlyph }
          Result := '';
      lsRecordNo:
        if FDataLink.Active then
        begin
          if FRecordNo >= 0 then
          begin
            if FRecordCount >= 0 then
              Result := Format('%d:%d', [FRecordNo, FRecordCount])
            else
              Result := IntToStr(FRecordNo);
          end
          else
          begin
            if FRecordCount >= 0 then
              Result := Format('( %d )', [FRecordCount])
            else
              Result := '';
          end;
        end
        else
          Result := '';
      lsRecordSize:
        if FDatalink.Active then
          Result := IntToStr(FDatalink.DataSet.RecordSize)
        else
          Result := '';
    end;
  end;
end;

function TJvDBStatusLabel.GetDatasetState: TDataSetState;
begin
  if DataSource <> nil then
    Result := DataSource.State
  else
    Result := dsInactive;
end;

procedure TJvDBStatusLabel.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  if csDesigning in ComponentState then
    Invalidate;
end;

procedure TJvDBStatusLabel.SetCaptions(Value: TStrings);
begin
  FCaptions.Assign(Value);
end;

function TJvDBStatusLabel.GetStatusKind(State: TDataSetState): TDBStatusKind;
begin
  if not (State in [Low(TDBStatusKind)..High(TDBStatusKind)]) then
  begin
    case State of
      dsFilter: Result := dsSetKey;
      dsNewValue, dsOldValue, dsCurValue: Result := dsEdit;
    else
      Result := TDBStatusKind(State);
    end;
  end
  else
    Result := TDBStatusKind(State);
end;

function TJvDBStatusLabel.GetCaption(State: TDataSetState): string;
const
  StrIds: array [TDBStatusKind] of string = (RsInactiveData, RsBrowseData,
    RsEditData, RsInsertData, RsSetKeyData, RsCalcFieldsData);
var
  Kind: TDBStatusKind;
begin
  Kind := GetStatusKind(State);
  if (FCaptions <> nil) and (Ord(Kind) < FCaptions.Count) and
    (FCaptions[Ord(Kind)] <> '') then
    Result := FCaptions[Ord(Kind)]
  else
    Result := StrIds[Kind];
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
      tlTop:
        GlyphOrigin.Y := 0;
      tlCenter:
        GlyphOrigin.Y := (ClientHeight - FCell.Height) div 2;
    else { tlBottom }
      GlyphOrigin.Y := ClientHeight - FCell.Height;
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
  if not (csDesigning in ComponentState) then
    Invalidate;
end;

procedure TJvDBStatusLabel.UpdateData;

  function IsSequenced: Boolean;
  begin
    Result := FDatalink.DataSet.IsSequenced;
  end;

begin
  FRecordCount := -1;
  if (FStyle = lsRecordNo) and FDataLink.Active and
    (DataSource.State in [dsBrowse, dsEdit]) then
  begin
    if Assigned(FOnGetRecordCount) then
      FOnGetRecordCount(Self, FDataLink.DataSet, FRecordCount)
    else
    if FCalcCount or IsSequenced then
      FRecordCount := FDataLink.DataSet.RecordCount;
  end;
  UpdateStatus;
end;

procedure TJvDBStatusLabel.UpdateStatus;
begin
  if DataSource <> nil then
  begin
    case FStyle of
      lsState:
        if FShowOptions in [doGlyph, doBoth] then
        begin
          if GlyphAlign = glGlyphLeft then
          begin
            RightMargin := 0;
            LeftMargin := (FGlyph.Width div GlyphColumns) + GlyphSpacing * 2;
          end
          else {glGlyphRight}
          begin
            LeftMargin := 0;
            RightMargin := (FGlyph.Width div GlyphColumns) + GlyphSpacing * 2;
          end;
          if FCell = nil then
            FCell := TBitmap.Create;
          AssignBitmapCell(FGlyph, FCell, GlyphColumns, 1,
            Ord(GetStatusKind(DataSource.State)));
        end
        else { doCaption }
        begin
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
          if FDataLink.Active then
          begin
            if Assigned(FOnGetRecNo) then
              FOnGetRecNo(Self, FDataLink.DataSet, FRecordNo)
            else
            try
              with FDatalink.DataSet do
                if not IsEmpty then
                  FRecordNo := RecNo;
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
  else
  begin
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
    (AComponent = DataSource) then
    DataSource := nil;
end;

function TJvDBStatusLabel.GetDataSetName: string;
begin
  Result := FDataSetName;
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FOnGetDataName) then
      Result := FOnGetDataName(Self)
    else
    if (Result = '') and (DataSource <> nil) and (DataSource.DataSet <> nil) then
      Result := DataSource.DataSet.Name;
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
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
  if not (csLoading in ComponentState) then
    UpdateData;
end;

procedure TJvDBStatusLabel.SetEditColor(Value: TColor);
begin
  if FEditColor <> Value then
  begin
    FEditColor := Value;
    if Style = lsState then
      Invalidate;
  end;
end;

procedure TJvDBStatusLabel.SetGlyphAlign(Value: TGlyphAlign);
begin
  if FGlyphAlign <> Value then
  begin
    FGlyphAlign := Value;
    UpdateStatus;
  end;
end;

procedure TJvDBStatusLabel.SetShowOptions(Value: TDBLabelOptions);
begin
  if FShowOptions <> Value then
  begin
    FShowOptions := Value;
    UpdateStatus;
  end;
end;

procedure TJvDBStatusLabel.SetCalcCount(Value: Boolean);
begin
  if FCalcCount <> Value then
  begin
    FCalcCount := Value;
    if not (csLoading in ComponentState) then
      UpdateData;
  end;
end;

procedure TJvDBStatusLabel.SetStyle(Value: TDBLabelStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    if not (csLoading in ComponentState) then
      UpdateData;
  end;
end;

initialization

finalization
  DestroyLocals;

end.

