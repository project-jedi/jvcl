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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
- THE AlwaysShowEditor OPTION IS NOT COMPATIBLE WITH CUSTOM EDIT CONTROLS AND BOOLEAN EDITOR.
  Custom edit controls and boolean editor are deactivated when this option is set to True.

2004/07/08 - WPostma merged changes by Frédéric Leneuf-Magaud and ahuser. 
-----------------------------------------------------------------------------}
// $Id$

unit JvDBGrid;

{$I jvcl.inc}

interface

uses
  {$IFDEF VCL}
  Windows, Messages,
  {$ENDIF VCL}
  Classes, Graphics, Controls, Grids, Menus, DBGrids, DB,
  JvTypes, {JvTypes contains Exception base class}
  JvAppStorage, JvFormPlacement, JvExDBGrids;

const
  DefJvGridOptions = [dgEditing, dgTitles, dgIndicator, dgColumnResize,
    dgColLines, dgRowLines, dgConfirmDelete, dgCancelOnExit];

  {$IFDEF BCB}
  {$NODEFINE DefJvGridOptions}
  {$ENDIF BCB}


  JvDefaultAlternateRowColor = TColor($00DDDDDD);

type
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
    ARect: TRect; Field: TField; ASortMarker: TBitmap; IsDown: Boolean;
    var Offset: Integer; var DefaultDrawText,
    DefaultDrawSortMarker: Boolean) of object;
  TJvTitleHintEvent = procedure(Sender: TObject; Field: TField;
    var AHint: string; var ATimeOut: Integer) of object;
  TJvCellHintEvent = TJvTitleHintEvent;

  EJVCLDbGridException = Class(EJVCLException);

  TJvSelectDialogColumnStrings = class(TPersistent)
  private
    FNoSelectionWarning: string;
    FCaption: string;
    FOK: string;
  public
    constructor Create;
  published
    property Caption: string read FCaption write FCaption;
    property OK: string read FOK write FOK;
    property NoSelectionWarning: string read FNoSelectionWarning write FNoSelectionWarning;
  end;

  TJvDBGrid = class;

  TJvDBGridControl = class(TCollectionItem)
  private
    FControlName: string;
    FFieldName: string;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property ControlName: string read FControlName write FControlName;
    property FieldName: string read FFieldName write FFieldName;
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
    function ControlByField(FieldName: string): TJvDBGridControl;
    property Items[Index: Integer]: TJvDBGridControl read GetItem write SetItem;
  end;

  TJvDBGrid = class(TJvExDBGrid)
  private
    FAutoSort: Boolean;
    FBeepOnError: Boolean; // WAP
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
    // FOnKeyPress: TKeyPressEvent; // do not declare this, exists in base class!
    FOnTitleBtnClick: TTitleClickEvent;
    FOnTitleBtnDblClick: TTitleClickEvent;
    FOnShowEditor: TJvDBEditShowEvent;
    FOnTopLeftChanged: TNotifyEvent;
    FSelectionAnchor: TBookmarkStr;
    FOnDrawColumnTitle: TDrawColumnTitleEvent;
    FWord: string;
    FShowTitleHint: Boolean;
    FSortedField: string;
    FPostOnEnter: Boolean;
    FSelectColumn: TSelectColumn;
    FTitleArrow: Boolean;
    FTitlePopup: TPopupMenu;
    FOnShowTitleHint: TJvTitleHintEvent;
    FOnTitleArrowMenuEvent: TNotifyEvent;
    FAlternateRowColor: TColor;
    FAutoSizeColumns: Boolean;
    FAutoSizeColumnIndex: Integer;
    FMinColumnWidth: Integer;
    FMaxColumnWidth: Integer;
    FInAutoSize: Boolean;
    FSelectColumnsDialogStrings: TJvSelectDialogColumnStrings;
    FTitleColumn: TColumn;
    FSortMarker: TSortMarker;
    FShowCellHint: Boolean;
    FOnShowCellHint: TJvCellHintEvent;

    FControls: TJvDBGridControls;
    FCurrentControl: TWinControl;
    FOldControlWndProc: TWndMethod;
    FBooleanEditor: TField;
    FWordWrap: Boolean;

    FAutoSizeRows: Boolean;
    FRowResize: Boolean;
    FRowsHeight: Integer;
    procedure SetAutoSizeRows(Value: Boolean);
    procedure SetRowResize(Value: Boolean);
    procedure SetRowsHeight(Value: Integer);

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
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure SetTitleArrow(const Value: Boolean);
    procedure ShowSelectColumnClick;
    procedure SetAlternateRowColor(const Value: TColor);
    procedure ReadAlternateRowColor(Reader: TReader);
    procedure SetAutoSizeColumnIndex(const Value: Integer);
    procedure SetAutoSizeColumns(const Value: Boolean);
    procedure SetMaxColumnWidth(const Value: Integer);
    procedure SetMinColumnWidth(const Value: Integer);
    procedure SetSelectColumnsDialogStrings(const Value: TJvSelectDialogColumnStrings);
    procedure SetSortedField(const Value: string);
    procedure SetSortMarker(const Value: TSortMarker);
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;

    procedure SetControls(Value: TJvDBGridControls);
    procedure HideCurrentControl;
    procedure ControlWndProc(var Message: TMessage);
    function UseDefaultEditor: Boolean;
    procedure ChangeBoolean(const FieldValueChange: Shortint);
    function DoKeyPress(var Msg: TWMChar): Boolean;
    procedure SetWordWrap(Value: Boolean);

  protected
    FCurrentDrawRow: Integer;
    procedure MouseLeave(Control: TControl); override;
    function AcquireFocus: Boolean;
    function CanEditShow: Boolean; override;
    function CreateEditor: TInplaceEdit; override;
    procedure DblClick; override;
    function DoTitleBtnDblClick: Boolean; dynamic;

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
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
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
      ASortMarker: TBitmap; IsDown: Boolean; var Offset: Integer;
      var DefaultDrawText, DefaultDrawSortMarker: Boolean); virtual;
    procedure ColEnter; override;

    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure EditButtonClick; override;
    {$IFNDEF COMPILER6_UP}
    procedure FocusCell(ACol, ARow: Longint; MoveAnchor: Boolean);
    {$ENDIF !COMPILER6_UP}
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
    procedure LoadFromAppStore(const AppStorage: TJvCustomAppStorage; const Path: string);
    procedure SaveToAppStore(const AppStorage: TJvCustomAppStorage; const Path: string);
    procedure Load;
    procedure Save;
    procedure UpdateTabStops(ALimit: Integer = -1);
    procedure ShowColumnsDialog;
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
    property OnCheckButton: TCheckTitleBtnEvent read FOnCheckButton write FOnCheckButton;
    property OnGetCellProps: TGetCellPropsEvent read FOnGetCellProps write FOnGetCellProps; { obsolete }
    property OnGetCellParams: TGetCellParamsEvent read FOnGetCellParams write FOnGetCellParams;
    property OnGetBtnParams: TGetBtnParamsEvent read FOnGetBtnParams write FOnGetBtnParams;
    property OnEditChange: TNotifyEvent read FOnEditChange write FOnEditChange;
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
    property BeepOnError: Boolean read FBeepOnError write FBeepOnError default True; // WAP.
    property AlternateRowColor: TColor read FAlternateRowColor write SetAlternateRowColor default clNone;
    property PostOnEnter: Boolean read FPostOnEnter write FPostOnEnter default False;
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
    property AutoSizeColumnIndex: Integer read FAutoSizeColumnIndex write SetAutoSizeColumnIndex default -1;
    property SelectColumnsDialogStrings: TJvSelectDialogColumnStrings read FSelectColumnsDialogStrings write
      SetSelectColumnsDialogStrings;
    { EditControls: list of controls used to edit data }
    property EditControls: TJvDBGridControls read FControls write SetControls;
    { AutoSizeRows: are rows resized automatically ? }
    property AutoSizeRows: Boolean read FAutoSizeRows write SetAutoSizeRows default True;
    { RowResize: can rows be resized with the mouse ? }
    property RowResize: Boolean read FRowResize write SetRowResize default False;
    { RowsHeight: rows height (to change the title row height, use RowHeights[0] instead) }
    property RowsHeight: Integer read FRowsHeight write SetRowsHeight;
    { WordWrap: are memo and string fields displayed on many lines ? }
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property OnKeyPress; // Expose TWinControl event.
  end;

implementation

uses
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  SysUtils, Math, TypInfo, Forms, StdCtrls, Dialogs, DBConsts,
  JvConsts, JvResources, JvDBLookup, JvDBUtils, JvJCLUtils,
  JvJVCLUtils, JvDBGridSelectColumnForm, JvFinalize;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvDBGrid.res}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvDBGrid.res}
{$ENDIF LINUX}

const
  sUnitName = 'JvDBGrid';

type
  TBookmarks = class(TBookmarkList);
  TGridPicture = (gpBlob, gpMemo, gpPicture, gpOle, gpObject, gpData,
    gpNotEmpty, gpMarkDown, gpMarkUp, gpChecked, gpUnChecked, gpPopup);

const
  GridBmpNames: array [TGridPicture] of PChar =
  ('JV_DBG_BLOB', 'JV_DBG_MEMO', 'JV_DBG_PICT', 'JV_DBG_OLE', 'JV_DBG_OBJECT',
    'JV_DBG_DATA', 'JV_DBG_NOTEMPTY', 'JV_DBG_SMDOWN', 'JV_DBG_SMUP',
    'JV_DBG_CHECKED', 'JV_DBG_UNCHECKED', 'JV_DBG_POPUP');

  bmMultiDot = 'JV_DBG_MSDOT';
  bmMultiArrow = 'JV_DBG_MSARROW';

var
  GridBitmaps: array [TGridPicture] of TBitmap =
    (nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil);
  FirstGridBitmaps: Boolean = True;

procedure FinalizeGridBitmaps;
var
  I: TGridPicture;
begin
  for I := Low(TGridPicture) to High(TGridPicture) do
    FreeAndNil(GridBitmaps[I]);
end;

function GetGridBitmap(BmpType: TGridPicture): TBitmap;
begin
  if GridBitmaps[BmpType] = nil then
  begin
    if FirstGridBitmaps then
    begin
      FirstGridBitmaps := False;
      AddFinalizeProc(sUnitName, FinalizeGridBitmaps);
    end;
    GridBitmaps[BmpType] := TBitmap.Create;
    GridBitmaps[BmpType].LoadFromResourceName(HInstance, GridBmpNames[BmpType]);
  end;
  Result := GridBitmaps[BmpType];
end;

procedure GridInvalidateRow(Grid: TJvDBGrid; Row: Longint);
var
  I: Longint;
begin
  for I := 0 to Grid.ColCount - 1 do
    Grid.InvalidateCell(I, Row);
end;

//=== { TInternalInplaceEdit } ===============================================

{$IFDEF COMPILER6_UP}

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
      ListValue := PickList.Items[PickList.ItemIndex];
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
          if MasterField.CanModify and TJvDBGrid(Grid).DataLink.Edit then
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
    Result := dgAlwaysShowEditor in TDBGrid(Grid).Options;
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
begin
  // Do not validate a record by error
  with TJvDBGrid(Grid) do
    if DataLink.Active and (DataLink.DataSet.State <> dsBrowse) then
      DataLink.DataSet.Cancel;

  // Ideally we would transmit the action to the DatalList but
  // DoMouseWheel is protected
  // Result := FDataList.DoMouseWheel(Shift, WheelDelta, MousePos);
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

{$ENDIF COMPILER6_UP}

//=== { TJvDBGridControls } ==========================================================

procedure TJvDBGridControl.Assign(Source: TPersistent);
begin
  if Source is TJvDBGridControl then
  begin
    ControlName := TJvDBGridControl(Source).ControlName;
    FieldName := TJvDBGridControl(Source).FieldName;
  end
  else
    inherited Assign(Source);
end;

constructor TJvDBGridControls.Create(ParentDBGrid: TJvDBGrid);
begin
  inherited Create(TJvDBGridControl);
  FParentDBGrid := ParentDBGrid;
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

function TJvDBGridControls.ControlByField(FieldName: string): TJvDBGridControl;
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

//=== { TJvDBGrid } ==========================================================

constructor TJvDBGrid.Create(AOwner: TComponent);
var
  Bmp: TBitmap;
begin
  inherited Create(AOwner);
  inherited DefaultDrawing := False;
  FAutoSort := True;
  FBeepOnError := True;
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
  FShowTitleHint := False;
  FShowCellHint := False;
  FAlternateRowColor := clNone;
  FSelectColumn := scDataBase;
  FTitleArrow := False;
  FPostOnEnter := False;
  FAutoSizeColumnIndex := -1;
  FSelectColumnsDialogStrings := TJvSelectDialogColumnStrings.Create;

  FControls := TJvDBGridControls.Create(Self);
  FCurrentControl := nil;
  FOldControlWndProc := nil;
  FBooleanEditor := nil;
  FWordWrap := False;

  FAutoSizeRows := True;
  FRowResize := False;
  FRowsHeight := DefaultRowHeight;
end;

destructor TJvDBGrid.Destroy;
begin
  HideCurrentControl;
  FControls.Free;

  FIniLink.Free;
  FMsIndicators.Free;
  FSelectColumnsDialogStrings.Free;
  inherited Destroy;
end;

function TJvDBGrid.GetImageIndex(Field: TField): Integer;
begin
  Result := -1;
  if FShowGlyphs and Assigned(Field) then
  begin
    case Field.DataType of
      ftBytes, ftVarBytes, ftBlob:
        Result := Ord(gpBlob);
      ftGraphic:
        Result := Ord(gpPicture);
      ftTypedBinary:
        Result := Ord(gpBlob);
      ftParadoxOle, ftDBaseOle:
        Result := Ord(gpOle);
      ftCursor:
        Result := Ord(gpData);
      ftReference, ftDataSet:
        Result := Ord(gpData);
      ftOraClob:
        Result := Ord(gpMemo);
      ftOraBlob:
        Result := Ord(gpBlob);
      ftBoolean:
        if not Field.IsNull then
          if Field.AsBoolean then
            Result := Ord(gpChecked)
          else
            Result := Ord(gpUnChecked);
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

procedure TJvDBGrid.SelectAll;
var
  ABookmark: TBookmark;
begin
  if MultiSelect and DataLink.Active then
  begin
    with DataLink.DataSet do
    begin
      if Bof and Eof then
        Exit;
      DisableControls;
      try
        ABookmark := GetBookmark;
        try
          First;
          while not Eof do
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
end;

procedure TJvDBGrid.ColWidthsChanged;
var
  ACol: Longint;
begin
  ACol := Col;
  inherited ColWidthsChanged;
  if DataLink.Active and (FixedCols > 0) then
    Col := Min(Max(CalcLeftColumn, ACol), ColCount - 1);
  DoMinColWidth;
  DoMaxColWidth;
  DoAutoSizeColumns;
end;

function TJvDBGrid.CreateEditor: TInplaceEdit;
begin
  {$IFDEF COMPILER6_UP}
  Result := TInternalInplaceEdit.Create(Self);
  // replace the call to default constructor :
  //  Result := inherited CreateEditor;
  TEdit(Result).OnChange := EditChanged;
  {$ELSE}
  Result := inherited CreateEditor;
  {$ENDIF COMPILER6_UP}
end;

function TJvDBGrid.GetTitleOffset: Byte;
var
  I, J: Integer;
begin
  Result := 0;
  if dgTitles in Options then
  begin
    Result := 1;
    if (DataLink <> nil) and (DataLink.DataSet <> nil) and
      DataLink.DataSet.ObjectView then
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
            with TBookmarks(SelectedRows) do
            begin
              AddAfter := Compare(CurrentRow, FSelectionAnchor) <> -Direction;
              if not AddAfter then
                CurrentRowSelected := False;
            end;
        end
        else
          ClearSelections;
      if Direction <> 0 then
        DataLink.DataSet.MoveBy(Direction);
      if AddAfter then
        SelectedRows.CurrentRowSelected := True;
    finally
      EndUpdate;
    end;
  end;

  procedure NextRow(Select: Boolean);
  begin
    with DataLink.DataSet do
    begin
      DoSelection(Select, 1);
      // Polaris
      if AutoAppend and Eof and CanModify and (not ReadOnly) and (dgEditing in Options) then
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
  if not DataLink.Active or not CanGridAcceptKey(Key, Shift) then
    Exit;
  with DataLink.DataSet do
    if Shift * KeyboardShiftStates = [ssCtrl] then
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
          end;
      end
    end
    else
    begin
      if Shift * KeyboardShiftStates = [] then
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
      if (DataLink.DataSet.State = dsBrowse) and (Shift * KeyboardShiftStates = []) then
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
        ((Key = VK_INSERT) and (CanModify and (not ReadOnly) and (dgEditing in Options))) then
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
var
  TitleHeight: Integer;
begin
  if (DefaultRowHeight <> Value) and not AutoSizeRows then
  begin
    TitleHeight := RowHeights[0];
    FRowsHeight := Value;
    DefaultRowHeight := Value;
    if dgTitles in Options then
      RowHeights[0] := TitleHeight;
    if HandleAllocated then
      Perform(WM_SIZE, SIZE_RESTORED, MakeLong(ClientWidth, ClientHeight));
  end
  else
    FRowsHeight := DefaultRowHeight;
end;

procedure TJvDBGrid.RowHeightsChanged;
var
  RowIdx,
  FirstRow: Integer;
begin
  if DefaultRowHeight <> RowsHeight then
    SetRowsHeight(RowsHeight);

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
end;

procedure TJvDBGrid.SetOptions(Value: TDBGridOptions);
var
  NewOptions: TGridOptions;
begin
  inherited Options := Value - [dgMultiSelect];
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

function TJvDBGrid.UseDefaultEditor: Boolean;
var
  F: TField;
  Control: TJvDBGridControl;
  EditControl : TWinControl;
begin
  FBooleanEditor := nil;

  Result := Assigned(DataLink) and DataLink.Active and
    (SelectedIndex >= 0) and (SelectedIndex < Columns.Count);

  if not Result then
  begin
    EditorMode := False;
    Exit;
  end;

  F := SelectedField;
  Result := (F <> nil);

  if not Result then
  begin
    EditorMode := False;
    Exit;
  end;

  Control := FControls.ControlByField(F.FieldName);
  Result := (Control <> nil);
  if not Result then
    Result := not (F.DataType in [ftUnknown, ftBytes, ftVarBytes, ftBlob,
      ftMemo, ftFmtMemo, ftGraphic, ftTypedBinary, ftParadoxOle, ftDBaseOle,
        ftCursor, ftReference, ftDataSet, ftOraClob, ftOraBlob]);
  Result := Result and F.CanModify and not Columns[SelectedIndex].ReadOnly;

  if not Result then
  begin
    EditorMode := False;
    Exit;
  end;

  if Assigned(OnShowEditor) then
  begin
    OnShowEditor(Self, F, Result);
    if not Result then
    begin
      EditorMode := False;
      Exit;
    end;
  end;

  // Any customized editor to use ?
  if not (dgAlwaysShowEditor in Options) then
  begin
    if Control <> nil then
    begin
      EditControl := TWinControl(Owner.FindComponent(Control.ControlName));
      PlaceControl(EditControl, Col, Row);
      Result := False;
    end
    else
    if F.DataType = ftBoolean then
    begin
      FBooleanEditor := F;
      Result := False;
    end;
  end;
end;

function TJvDBGrid.CanEditShow: Boolean;
begin
  if (dgAlwaysShowEditor in Options) and not EditorMode then
    EditorMode := True;
  Result := (inherited CanEditShow) and UseDefaultEditor;
end;

procedure TJvDBGrid.GetCellProps(Field: TField; AFont: TFont;
  var Background: TColor; Highlight: Boolean);

  function IsAfterFixedCols: Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := 0 to FixedCols - 1 do
      if Columns.Items[I].FieldName = Field.FieldName then
      begin
        Result := False;
        Break;
      end;
  end;

begin
  if (FCurrentDrawRow >= FixedRows) and Odd(FCurrentDrawRow + FixedRows) and
    (FAlternateRowColor <> clNone) and (FAlternateRowColor <> Color) and
    IsAfterFixedCols then
    Background := AlternateRowColor;

  if Highlight then
  begin
    AFont.Color := clHighlightText;
    Background := clHighlight;
  end;
  if Assigned(FOnGetCellParams) then
    FOnGetCellParams(Self, Field, AFont, Background, Highlight)
  else
  if Assigned(FOnGetCellProps) then
    FOnGetCellProps(Self, Field, AFont, Background);
end;

procedure TJvDBGrid.DoTitleClick(ACol: Longint; AField: TField);
const
  cIndexName = 'IndexName';
  cIndexDefs = 'IndexDefs';
  cDirection: array [Boolean] of TSortMarker = (smDown, smUp);
var
  IndexDefs: TIndexDefs;
  lIndexName: string;
  Descending: Boolean;
  IndexFound: Boolean;

  function GetIndexOf(aFieldName: string; var aIndexName: string; var Descending: Boolean): Boolean;
  var
    I: Integer;
    IsDescending: Boolean;

  begin
    Result := False;
    for I := 0 to IndexDefs.Count - 1 do
    begin
      if Pos(aFieldName, IndexDefs[I].Fields) >= 1 then
      begin
        aIndexName := IndexDefs[I].Name; // best match so far
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
    IndexDefs := TIndexDefs(GetOrdProp(DataSource.DataSet, cIndexDefs))
  else
    IndexDefs := nil;
  if Assigned(IndexDefs) then
  begin
    Descending := SortMarker = smUp;
    if GetIndexOf(AField.FieldName, lIndexName, Descending) then
    begin
      IndexFound := True;
      SortedField := AField.FieldName;
      SortMarker := cDirection[Descending];
      try
        SetStrProp(DataSource.DataSet, cIndexName, lIndexName);
      except
      end;
    end;
  end;
  //--------------------------------------------------------------------------
  // FBC: 2004-02-18
  // Following code handles the sortmarker if no Index is found.
  // the actual data-sorting must be implemented by the user in
  // event OnTitleBtnClick.
  //--------------------------------------------------------------------------
  if AutoSort and not IndexFound then
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
    if FDisableCount = 0 then
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
    if (AlternateRowColor <> clNone) and (AlternateRowColor <> Color) then
      Invalidate;
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
      Distance := 1;
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
      Distance := 1;
    if DataLink.Active then
      Result := DataLink.DataSet.MoveBy(-Distance) <> 0;
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
  NewPressed := Windows.PtInRect(Rect(0, 0, ClientWidth, ClientHeight), {Types.} Point(X, Y)) and
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
  CursorPos: TPoint;
  lLastSelected, lNewSelected: TBookmarkStr;
  lCompare: Integer;
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

    if (Button = mbRight) and FTitleArrow and
      (dgTitles in Options) and (dgIndicator in Options) and
      (Cell.X = 0) and (Cell.Y = 0) then
    begin
      if Assigned(FOnTitleArrowMenuEvent) then
        FOnTitleArrowMenuEvent(Self);

      // Display TitlePopup if it exists
      if Assigned(FTitlePopup) then
      begin
        GetCursorPos(CursorPos);
        FTitlePopup.PopupComponent := Self;
        FTitlePopup.Popup(CursorPos.X, CursorPos.Y);
      end;
      Exit;
    end;

    if (DragKind = dkDock) and (Cell.X < IndicatorOffset) and
      (Cell.Y < TitleOffset) and (not (csDesigning in ComponentState)) then
    begin
      BeginDrag(False);
      Exit;
    end;
    if FTitleButtons and (DataLink <> nil) and DataLink.Active and
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
          SysUtils.Beep;
        Exit;
      end;
    end;
    if (Cell.X < FixedCols + IndicatorOffset) and DataLink.Active then
    begin
      if dgIndicator in Options then
        inherited MouseDown(Button, Shift, 1, Y)
      else
      if Cell.Y >= TitleOffset then
        if Cell.Y - Row <> 0 then
          DataLink.DataSet.MoveBy(Cell.Y - Row);
    end
    else
    begin
      //-----------------------------------------------------------------------
      // FBC: do not move column if RowSelect = True
      //-----------------------------------------------------------------------
      if (dgRowSelect in Options) and (Columns.State <> csCustomized) then
        inherited MouseDown(Button, Shift, 1, Y)
      else
        inherited MouseDown(Button, Shift, X, Y)
    end;  
    MouseDownEvent := OnMouseDown;
    if Assigned(MouseDownEvent) then
      MouseDownEvent(Self, Button, Shift, X, Y);
    if not (((csDesigning in ComponentState) or (dgColumnResize in Options)) and
      (Cell.Y < TitleOffset)) and (Button = mbLeft) then
    begin
      if MultiSelect and DataLink.Active then
        with SelectedRows do
        begin
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
                      while not (CurrentRowSelected and (Bookmark = lNewSelected)) do
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
                      while not (CurrentRowSelected and (Bookmark = lNewSelected)) do
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
        Dec(ACol);
      if (DataLink <> nil) and DataLink.Active and (ACol >= 0) and
        (ACol < Columns.Count) then
        DoTitleClick(FPressedCol.Index, FPressedCol.Field);
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
//    and not AutoSizeColumns
  begin
    ColWidths[FSizingIndex] := GetMinColWidth(X - FSizingOfs - CellRect(FSizingIndex, 0).Left);
    FGridState := gsNormal;
//    Exit;
  end;
  // Polaris

  if FTitleArrow and (Button = mbLeft) and
    (dgTitles in Options) and (dgIndicator in Options) and
    (Cell.X = 0) and (Cell.Y = 0) and DataLink.Active then
    ShowSelectColumnClick; // Selection of columns

  inherited MouseUp(Button, Shift, X, Y);
  DoAutoSizeColumns;
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
  if (dgEditing in Options) and (Char(Msg.CharCode) in [Backspace, #32..#255]) and
    Assigned(SelectedField) and (SelectedField is TBooleanField) then
  begin
    if not DoKeyPress(Msg) then
    begin
      EditorMode := ((not UseDefaultEditor) and Assigned(FBooleanEditor));
      if EditorMode then
        case Char(Msg.CharCode) of
          #32:
            ChangeBoolean(0);  // invert
          Backspace, '0', '-':
            ChangeBoolean(-1); // uncheck
          '1', '+':
            ChangeBoolean(1);  // check
        end;
    end;
    Exit;
  end;

  inherited;

  if Assigned(FCurrentControl) then
    if FCurrentControl.Visible then
      PostMessage(FCurrentControl.Handle, WM_CHAR, Msg.CharCode, Msg.KeyData);
end;

procedure TJvDBGrid.KeyPress(var Key: Char);
const
  cChar = ['A'..'Z', 'a'..'z', ' ', '-', '+', '0'..'9', '.', ',',
    'é', 'è', 'ê', 'ë', 'ô', 'û', 'ù', 'â', 'à', 'î', 'ï', 'ç', Backspace];
var
  lWord: string;
  lMasterField: TField;
  I, deb: Integer;

  procedure FindWord;
  begin
    if Pos(UpperCase(FWord), UpperCase(InplaceEditor.EditText)) <> 1 then
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
  if (Key = Cr) and PostOnEnter and not ReadOnly then
    DataSource.DataSet.CheckBrowseMode;

  inherited KeyPress(Key);

  if EditorMode then
  begin
    // Goal: Allow to go directly into the InplaceEditor when one types the first
    // characters of a word found in the list.
    // Remark: InplaceEditor is protected in TCustomGrid, published in TJvDBGrid.
    with Columns[SelectedIndex].Field do
      if (FieldKind = fkLookup) and (Key in cChar) then
      begin
        FindWord;
        LookupDataSet.DisableControls;
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
        finally
          LookupDataSet.EnableControls;
        end;
      end
      else
      if FieldKind = fkData then
      begin
        if DataType = ftFloat then
          if Key in ['.', ','] then
            Key := DecimalSeparator;

        if (Key in cChar) and (Columns[SelectedIndex].PickList.Count <> 0) then
        begin
          FindWord;
          Key := #0; // always suppress char
          with Columns[SelectedIndex].PickList do
            for I := 0 to Count - 1 do
            begin
              deb := Length(lWord);
              if UpperCase(lWord) = UpperCase(Copy(Strings[I], 1, deb)) then
              begin
                DataSet.Edit;

                InplaceEditor.EditText := Strings[I];
                Columns[SelectedIndex].Field.Text := Strings[I];
                InplaceEditor.SelStart := deb;
                InplaceEditor.SelLength := Length(Text) - deb;
                FWord := lWord;

                Break;
              end;
            end;
        end;
      end;
  end
  else
    // This fixes a bug coming from DBGrids.pas when a field is not editable.
    // This ensures that nothing else will process the keys pressed.
    Key := #0;
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

procedure TJvDBGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
const
  EdgeFlag: array [Boolean] of UINT = (BDR_RAISEDINNER, BDR_SUNKENINNER);
var
  FrameOffs: Byte;
  BackColor: TColor;
  ASortMarker: TSortMarker;
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
  DefaultDrawText, DefaultDrawSortMarker: Boolean;

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
//  if gdFixed in AState then
//    Canvas.Brush.Color := FixedColor;
  FCurrentDrawRow := ARow;
  Canvas.Font := Self.Font;
  if (DataLink <> nil) and DataLink.Active and (ACol >= 0) and
    (ACol < Columns.Count) then
  begin
    DrawColumn := Columns[ACol];
    if DrawColumn <> nil then
      Canvas.Font := DrawColumn.Font;
  end;

  inherited DrawCell(ACol, ARow, ARect, AState);
  with ARect do
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
      FMsIndicators.BkColor := FixedColor;
      ALeft := FixRect.Right - FMsIndicators.Width - FrameOffs;
      if InBiDiMode then
        Inc(ALeft);
      FMsIndicators.Draw(Self.Canvas, ALeft, (FixRect.Top +
        FixRect.Bottom - FMsIndicators.Height) shr 1, Indicator);
    end;
  end
  else
  if not (csLoading in ComponentState) and (FTitleButtons or (FixedCols > 0)) and
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
      if FTitleButtons or ([dgRowLines, dgColLines] * Options = [dgRowLines, dgColLines]) then
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
      if FTitleButtons and (AField <> nil) then
      begin
        BackColor := Canvas.Brush.Color;
        //-----------------------------------------
        // FBC -fix SortMarker
        // Not so elegant, but it works.
        //-----------------------------------------
        if AnsiSameText(AField.FieldName, SortedField) then
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
        case ASortMarker of
          smDown: Bmp := GetGridBitmap(gpMarkDown);
          smUp: Bmp := GetGridBitmap(gpMarkUp);
        else
          Bmp := nil;
        end;
        if Bmp <> nil then
          Indicator := Bmp.Width + 6
        else
          Indicator := 1;
        DefaultDrawText := True;
        DefaultDrawSortMarker := True;
        DoDrawColumnTitle(Canvas, TitleRect, AField, Bmp, Down, Indicator,
          DefaultDrawText, DefaultDrawSortMarker);
        TextRect := TitleRect;
        if (ASortMarker <> smNone) and ((DrawColumn.Title.Alignment = taRightJustify) or (Canvas.TextWidth(Caption) >=
          (TextRect.Right - TextRect.Left))) then
          Dec(TextRect.Right, Bmp.Width + 4);
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
            Canvas.FillRect(Rect(TextRect.Right, TitleRect.Top, TitleRect.Right, TitleRect.Bottom));
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

const
  AlignFlags : array [TAlignment] of Integer =
    ( DT_LEFT or DT_EXPANDTABS or DT_NOPREFIX,
      DT_RIGHT or DT_EXPANDTABS or DT_NOPREFIX,
      DT_CENTER or DT_EXPANDTABS or DT_NOPREFIX );
  RTL: array [Boolean] of Integer = (0, DT_RTLREADING);

var
  I: Integer;
  NewBackgrnd: TColor;
  Highlight: Boolean;
  Bmp: TBitmap;
  Field: TField;

  MemoText: string;
  DrawBitmap: TBitmap;
  DrawRect, B, R: TRect;
  Alignment: TAlignment;
  Hold,
  DrawOptions: Integer;
  
begin
  Field := Column.Field;
  if Assigned(DataSource) and Assigned(DataSource.DataSet) and DataSource.DataSet.Active and
    (SelectedRows.IndexOf(DataSource.DataSet.Bookmark) > -1) then
    Include(State, gdSelected);
  NewBackgrnd := Canvas.Brush.Color;
  Highlight := (gdSelected in State) and ((dgAlwaysShowSelection in Options) or Focused);
  GetCellProps(Field, Canvas.Font, NewBackgrnd, Highlight or ActiveRowSelected);
  Canvas.Brush.Color := NewBackgrnd;
  if DefaultDrawing then
  begin
    I := GetImageIndex(Field);
    if (I >= 0) then
    begin
      Bmp := GetGridBitmap(TGridPicture(I));
      Canvas.FillRect(Rect);
      DrawBitmapTransparent(Canvas, (Rect.Left + Rect.Right + 1 - Bmp.Width) div 2,
        (Rect.Top + Rect.Bottom + 1 - Bmp.Height) div 2, Bmp, clOlive);
    end
    else
    begin
      if (Field is TStringField) or (Field is TMemoField) then
      begin
        if Assigned(Field.OnGetText) then
          MemoText := Field.DisplayText
        else
          MemoText := Field.AsString;
        DrawBitmap := TBitmap.Create;
        try
          DrawBitmap.Canvas.Lock;
          try
            DrawRect := Rect;
            with DrawBitmap, DrawRect do { Use offscreen bitmap to eliminate flicker and }
            begin                        { brush origin tics in painting / scrolling.    }
              Width := Max(Width, Right - Left);
              Height := Max(Height, Bottom - Top);
              R := Classes.Rect(2, 2, Right - Left - 1, Bottom - Top - 1);
              B := Classes.Rect(0, 0, Right - Left, Bottom - Top);
            end;
            Alignment := Column.Alignment;
            with DrawBitmap.Canvas do
            begin
              Font := Canvas.Font;
              Font.Color := Canvas.Font.Color;
              Brush := Canvas.Brush;
              Brush.Style := bsSolid;
              FillRect(B);
              SetBkMode(Handle, TRANSPARENT);
              if (Canvas.CanvasOrientation = coRightToLeft) then
                ChangeBiDiModeAlignment(Alignment);
              DrawOptions := AlignFlags[Alignment] or
                RTL[UseRightToLeftAlignmentForField(Field, Alignment)];
              if WordWrap then
                DrawOptions := DrawOptions or DT_WORDBREAK;
              Windows.DrawText(Handle, PChar(MemoText), Length(MemoText), R, DrawOptions);
            end;
            if (Canvas.CanvasOrientation = coRightToLeft) then
            begin
              Hold := DrawRect.Left;
              DrawRect.Left := DrawRect.Right;
              DrawRect.Right := Hold;
            end;
            Canvas.CopyRect(DrawRect, DrawBitmap.Canvas, B);
          finally
            DrawBitmap.Canvas.Unlock;
          end;
        finally
          DrawBitmap.Free;
        end;
      end
      else
        DefaultDrawColumnCell(Rect, DataCol, Column, State);
    end;
  end;
  if (Columns.State = csDefault) or not DefaultDrawing or (csDesigning in ComponentState) then
    inherited DrawDataCell(Rect, Field, State);
  inherited DrawColumnCell(Rect, DataCol, Column, State);
  if DefaultDrawing and Highlight and not (csDesigning in ComponentState) and
    not (dgRowSelect in Options) and
    (ValidParentForm(Self).ActiveControl = Self) then
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
  if Assigned(AppStorage) then
  begin
    AppStorage.DeleteSubTree(SectionName);
    with Columns do
      for I := 0 to Count - 1 do
        AppStorage.WriteString(AppStorage.ConcatPaths([SectionName, Format('%s.%s', [Name, Items[I].FieldName])]),
          Format('%d,%d', [Items[I].Index, Items[I].Width]));
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
    with Columns do
    begin
      SetLength(ColumnArray, Count);
      try
        for I := 0 to Count - 1 do
        begin
          S := AppStorage.ReadString(AppStorage.ConcatPaths([SectionName,
            Format('%s.%s', [Name, Items[I].FieldName])]));
          ColumnArray[I].Column := Items[I];
          ColumnArray[I].EndIndex := Items[I].Index;
          if S <> '' then
          begin
            ColumnArray[I].EndIndex := StrToIntDef(ExtractWord(1, S, Delims), ColumnArray[I].EndIndex);
            Items[I].Width := StrToIntDef(ExtractWord(2, S, Delims), Items[I].Width);
            Items[I].Visible := (S <> '-1');
          end;
        end;
        for I := 0 to Count - 1 do
          for J := 0 to Count - 1 do
            if ColumnArray[J].EndIndex = I then
            begin
              ColumnArray[J].Column.Index := ColumnArray[J].EndIndex;
              Break;
            end;
      finally
        Finalize(ColumnArray);
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
    Exit;

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

procedure TJvDBGrid.DoDrawColumnTitle(Canvas: TCanvas; ARect: TRect;
  AField: TField; ASortMarker: TBitmap; IsDown: Boolean; var Offset: Integer;
  var DefaultDrawText, DefaultDrawSortMarker: Boolean);
begin
  if Assigned(FOnDrawColumnTitle) then
  begin
    FOnDrawColumnTitle(Self, Canvas, ARect, AField, ASortMarker, IsDown, Offset,
      DefaultDrawText, DefaultDrawSortMarker);
  end;
end;

{$IFNDEF COMPILER6_UP}
procedure TJvDBGrid.FocusCell(ACol, ARow: Longint; MoveAnchor: Boolean);
begin
  MoveColRow(ACol, ARow, MoveAnchor, True);
  InvalidateEditor;
  Click;
end;
{$ENDIF !COMPILER6_UP}

procedure TJvDBGrid.ChangeBoolean(const FieldValueChange: Shortint);
begin
  if not Assigned(FBooleanEditor) then
    Exit;
  if (inherited CanEditShow) and Assigned(DataLink) and DataLink.Active and
    FBooleanEditor.CanModify and not ReadOnly then
  begin
    DataLink.Edit;
    // FieldValueChange = 0 -> invert, 1 -> check, -1 -> uncheck
    if (FBooleanEditor.Value = Null) or (FieldValueChange <> 0) then
      FBooleanEditor.Value := (FieldValueChange <> -1)
    else
      FBooleanEditor.Value := not FBooleanEditor.Value;
    DrawCell(Col, Row, CellRect(Col, Row), [gdSelected, gdFocused]);
  end
  else
    FBooleanEditor := nil;
end;

procedure TJvDBGrid.CellClick(Column: TColumn);
begin
  FTitleColumn := nil;
  inherited CellClick(Column);

  if FBooleanEditor = Column.Field then
    ChangeBoolean(0); // Invert value
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
end;

function TJvDBGrid.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  // Do not validate a record by error
  if DataLink.Active and (DataLink.DataSet.State <> dsBrowse) then
    DataLink.DataSet.Cancel;
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

procedure TJvDBGrid.ShowSelectColumnClick;
var
  R: TRect;
  frm: TfrmSelectColumn;
begin
  R := CellRect(0, 0);
  frm := TfrmSelectColumn.Create(Application);
  try
    if not IsRectEmpty(R) then
      with ClientToScreen(Point(R.Left, R.Bottom + 1)) do
        frm.SetBounds(X, Y, frm.Width, frm.Height);
    frm.Grid := TJvDBGrid(Self);
    frm.DataSource := DataLink.DataSource;
    frm.SelectColumn := FSelectColumn;
    frm.Caption := SelectColumnsDialogStrings.Caption;
    frm.ButtonOK.Caption := SelectColumnsDialogStrings.OK;
    frm.NoSelectionWarning := SelectColumnsDialogStrings.NoSelectionWarning;
    frm.ShowModal;
  finally
    frm.Free;
  end;
  Invalidate;
end;

procedure TJvDBGrid.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('AlternRowColor', ReadAlternateRowColor, nil, False);
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

procedure TJvDBGrid.DoAutoSizeColumns;

procedure DoIt;
var
  TotalWidth, OrigWidth: Integer;
  I: Integer;
  ScaleFactor: Double;
  AWidth, AUsedWidth, ALeftCol: Integer;
begin
  if not AutoSizeColumns or FInAutoSize or (Columns.Count = 0) or (FGridState = gsColSizing) then
    Exit;
  FInAutoSize := True;
  ALeftCol := LeftCol;
  try
    // get useable width
    TotalWidth := ClientWidth - (Ord(dgIndicator in Options) * IndicatorWidth) - (Ord(dgColLines in Options) *
      Columns.Count * GridLineWidth);
    OrigWidth := 0;
    // get width currently occupied by columns
    if FixedCols = 0 then
      BeginLayout;
    try
      // autosize all columns proportionally
      if AutoSizeColumnIndex = -1 then
      begin
        for I := 0 to Columns.Count - 1 do
          if Columns[I].Visible then
            Inc(OrigWidth, Columns[I].Width);
        if OrigWidth = 0 then
          OrigWidth := 1;
        // calculate the realtionship between what's available and what's in use
        ScaleFactor := TotalWidth / OrigWidth;
        if ScaleFactor = 1.0 then
          Exit; // no need to continue - resizing won't change anything
        AUsedWidth := 0;
        for I := 0 to Columns.Count - 1 do
          if Columns[I].Visible then
          begin
            if I = LastVisibleColumn then
              Columns[I].Width := TotalWidth - AUsedWidth
            else
            begin
              AWidth := GetMaxColWidth(GetMinColWidth(round(ScaleFactor * Columns[I].Width)));
              if AWidth < 1 then
                AWidth := 1;
              Columns[I].Width := AWidth;
              Inc(AUsedWidth, AWidth);
            end;
          end;
      end
      // a index of -2 indicates that we want to autosize the last visible column
      else
      if AutoSizeColumnIndex = -2 then // auto size last visible
      begin
        // reuse AUsedWidth as the actual resize column index
        AUsedWidth := LastVisibleColumn;
        if AUsedWidth < 0 then
          Exit;
        OrigWidth := 0;
        for I := 0 to Columns.Count - 1 do
          if Columns[I].Visible and (I <> AUsedWidth) then
            Inc(OrigWidth, Columns[I].Width);
        AWidth := GetMaxColWidth(GetMinColWidth(TotalWidth - OrigWidth));
        if AWidth > 0 then
          Columns[AUsedWidth].Width := AWidth;
      end
      // only auto size one column
      else
      if AutoSizeColumnIndex <= LastVisibleColumn then
      begin
        OrigWidth := 0;
        for I := 0 to Columns.Count - 1 do
          if Columns[I].Visible and (I <> AutoSizeColumnIndex) then
            Inc(OrigWidth, Columns[I].Width);
        AWidth := GetMaxColWidth(GetMinColWidth(TotalWidth - OrigWidth));
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

begin
  DoIt;

  if FCurrentControl <> nil then
    if FCurrentControl.Visible then
      PlaceControl(FCurrentControl, Col, Row);
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

procedure TJvDBGrid.Resize;
begin
  inherited Resize;
  DoAutoSizeColumns;
end;

procedure TJvDBGrid.Loaded;
var
  Ctrl_Idx: Integer;
  WinControl: TWinControl;
begin
  inherited Loaded;

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

procedure TJvDBGrid.ShowColumnsDialog;
begin
  ShowSelectColumnClick;
end;

procedure TJvDBGrid.SetSelectColumnsDialogStrings(
  const Value: TJvSelectDialogColumnStrings);
begin
  // do nothing
end;

procedure TJvDBGrid.DblClick;
begin
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
  FTitleColumn := Column;
  inherited TitleClick(Column);
end;

//=== { TJvSelectDialogColumnStrings } =======================================

constructor TJvSelectDialogColumnStrings.Create;
begin
  inherited Create;
  Caption := RsJvDBGridSelectTitle;
  OK := RsJvDBGridSelectOK;
  NoSelectionWarning := RsJvDBGridSelectWarning;
end;

procedure TJvDBGrid.SetSortedField(const Value: string);
begin
  if FSortedField <> Value then
  begin
    FSortedField := Value;
    Invalidate;
  end;
end;

procedure TJvDBGrid.SetSortMarker(const Value: TSortMarker);
begin
  if FSortMarker <> Value then
  begin
    FSortMarker := Value;
    Invalidate;
  end;
end;

procedure TJvDBGrid.CMHintShow(var Msg: TMessage);
const
  C_TIMEOUT = 250;
var
  ACol, ARow, ATimeOut, SaveRow: Integer;
  AtCursorPosition: Boolean;
  CalcOptions: Integer;
  HintRect: TRect;
begin
  AtCursorPosition := True;
  with PHintInfo(Msg.LParam)^ do
  begin
    HintStr := Hint;
    ATimeOut := HideTimeOut;
    Self.MouseToCell(CursorPos.X, CursorPos.Y, ACol, ARow);

    //-------------------------------------------------------------------------
    // ARow = -1 if 'outside' a valid cell;
    // Adjust CursorRect
    //-------------------------------------------------------------------------
    if (FShowTitleHint or FShowCellHint) then
    begin
      if (ARow = -1) or ((ARow >= 1) and not FShowCellHint) then
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
      Dec(ACol);
    if dgTitles in Options then
      Dec(ARow);

    if FShowTitleHint and (ACol >= 0) and (ARow = -1) then
    begin
      AtCursorPosition := False;
      HintStr := Columns[ACol].FieldName;
      ATimeOut := Max(ATimeOut, Length(HintStr) * C_TIMEOUT);
      if Assigned(FOnShowTitleHint) and DataLink.Active then
        FOnShowTitleHint(Self, Columns[ACol].Field, HintStr, ATimeOut);
      HideTimeOut := ATimeOut;
    end;

    if FShowCellHint and (ACol >= 0) and DataLink.Active and
      ((ARow >= 0) or (not FShowTitleHint)) then
    begin
      AtCursorPosition := False;
      HintStr := Hint;
      SaveRow := DataLink.ActiveRecord;
      try
        // Only calculation, no drawing
        CalcOptions := DT_CALCRECT or DT_LEFT or DT_NOPREFIX or DrawTextBiDiModeFlagsReadingOnly;
        if (ARow = -1) then
        begin
          Canvas.Font.Assign(Columns[ACol].Title.Font);
          HintStr := Columns[ACol].Title.Caption;
        end
        else
        with Columns[ACol] do
        begin
          Canvas.Font.Assign(Font);
          DataLink.ActiveRecord := ARow;
          if Field <> nil then
          begin
            if Assigned(Field.OnGetText) then
              HintStr := Field.DisplayText
            else
            begin
              if (Field is TStringField) or (Field is TMemoField) then
              begin
                HintStr := Field.AsString;
                if WordWrap then
                  CalcOptions := CalcOptions or DT_WORDBREAK;
              end
              else
              if not (Field is TBlobField) then
                HintStr := Field.DisplayText
              else
                HintStr := '';
            end;
          end;
        end;

        HintRect := Rect(0, 0, Columns[ACol].Width - 4, 0);
        Windows.DrawText(Canvas.Handle, PChar(HintStr), -1, HintRect, CalcOptions);
        if ((HintRect.Bottom - HintRect.Top + 2) < RowHeights[ARow + 1]) and
         ((HintRect.Right - HintRect.Left) < Columns[ACol].Width - 2) then
          HintStr := '';

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
    begin
      HintPos := ClientToScreen(CursorRect.TopLeft);
    end;
  end;
  inherited;
end;

procedure TJvDBGrid.WMVScroll(var Msg: TWMVScroll);
var
  ALeftCol: Integer;
begin
  ALeftCol := LeftCol;
  inherited;
  LeftCol := ALeftCol;
end;

procedure TJvDBGrid.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Invalidate;
  end;
end;

procedure TJvDBGrid.PlaceControl(Control: TWinControl; ACol, ARow: Integer);
var
  R: TRect;
begin
  if not Assigned(Control) then
    raise EJVCLDbGridException.CreateRes(@RsEJvDBGridControlPropertyNotAssigned);
  if not DataLink.Edit then
    Exit;

  if Control <> FCurrentControl then
  begin
    HideCurrentControl;
    FCurrentControl := Control;
    FOldControlWndProc := FCurrentControl.WindowProc;
    FCurrentControl.WindowProc := ControlWndProc;
  end;


  if Control.Parent <> Self.Parent then
    Control.Parent := Self.Parent;
  R := CellRect(ACol, ARow);
  R.TopLeft := ClientToScreen(R.TopLeft);
  R.TopLeft := TControl(Control.Parent).ScreenToClient(R.TopLeft);
  R.BottomRight := ClientToScreen(R.BottomRight);
  R.BottomRight := TControl(Control.Parent).ScreenToClient(R.BottomRight);
  Control.BoundsRect := R;
  Control.BringToFront;
  Control.Show;

  if Self.Visible and Control.Visible and Self.Parent.Visible and GetParentForm(Self).Visible then
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
      SendMessage(FCurrentControl.Handle, WM_KILLFOCUS, 0, 0);
      FCurrentControl.Hide;
    end;
    FCurrentControl := nil;
  end;
  FOldControlWndProc := nil;
end;

procedure TJvDBGrid.ControlWndProc(var Message: TMessage);
var
  EscapeKey: Boolean;

  procedure BackToGrid;
  begin
    HideCurrentControl;
    if Self.Visible then
      Self.SetFocus;
  end;

begin
  if Message.Msg = WM_CHAR then
  begin
    if not DoKeyPress(TWMChar(Message)) then
      with TWMKey(Message) do
      begin
        if (CharCode = VK_RETURN) and PostOnEnter then
        begin
          BackToGrid;
          DataSource.DataSet.CheckBrowseMode;
        end
        else
        if CharCode = VK_TAB then
        begin
          BackToGrid;
          PostMessage(Handle, WM_KEYDOWN, CharCode, KeyData);
        end
        else
        begin
          EscapeKey := (CharCode = VK_ESCAPE);
          FOldControlWndProc(Message);
          if EscapeKey then
            BackToGrid;
        end;
      end;
  end
  else
  begin
    FOldControlWndProc(Message);
    case Message.Msg of
      WM_GETDLGCODE:
        Message.Result := Message.Result or DLGC_WANTTAB;
      CM_EXIT:
        HideCurrentControl;
    End;
  end;
end;

initialization

finalization
  FinalizeUnit(sUnitName);

end.
