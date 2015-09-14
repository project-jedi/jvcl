{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain A copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStringGrid.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sebastien Buysse [sbuysse att buypin dott com]
Portions created by Sebastien Buysse are Copyright (C) 2001 S?stien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvStringGrid;

{$I jvcl.inc}

//---------------------------------------------------------------
// The inplace-edit-list feature is enabled dynamically when
// compiling JVCL, if the underlying JVCL and VCL base classes
// support it. 
//---------------------------------------------------------------

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Grids, StdCtrls,
  JvJCLUtils, JvExGrids;

const
  GM_ACTIVATECELL = WM_USER + 123;

type
  TGMActivateCell = record
    Msg: Cardinal;
    {$IFDEF COMPILER16_UP}
	MsgFiller: TDWordFiller;
    {$ENDIF COMPILER16_UP}
    Column: Integer;
    {$IFDEF COMPILER16_UP}
	WParamFiller: TDWordFiller;
    {$ENDIF COMPILER16_UP}
    Row: Integer;
    {$IFDEF COMPILER16_UP}
	LParamFiller: TDWordFiller;
    {$ENDIF COMPILER16_UP}
    Result: LRESULT;
  end;

  TJvStringGrid = class;
  TExitCellEvent = procedure(Sender: TJvStringGrid; AColumn, ARow: Integer;
    const EditText: string) of object;
  TGetCellAlignmentEvent = procedure(Sender: TJvStringGrid; AColumn, ARow: Integer;
    State: TGridDrawState; var CellAlignment: TAlignment) of object;
  TCaptionClickEvent = procedure(Sender: TJvStringGrid; AColumn, ARow: Integer) of object;
  TEditShowEvent = procedure(Sender: TJvStringGrid; ACol, ARow: Longint;
    var AllowEdit: Boolean) of object;
  TJvSortType = (stNone, stAutomatic, stClassic, stCaseSensitive, stNumeric, stDate, stCurrency);
  TProgress = procedure(Sender: TObject; Progression, Total: Integer) of object;

  TJvOnGetEditStyleEvent = procedure(Sender: TJvStringGrid; AColumn, ARow: Integer; PickListStrings: TStrings; var EditStyle: TEditStyle) of object;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvStringGrid = class(TJvExStringGrid)
  private
    FAlignment: TAlignment;
    FOnSetCanvasProperties: TDrawCellEvent;
    FOnGetCellAlignment: TGetCellAlignmentEvent;
    FOnColWidthsChanged: TNotifyEvent;
    FCaptionClick: TCaptionClickEvent;
    FCellOnMouseDown: TGridCoord;
    FOnExitCell: TExitCellEvent;
    FOnLoadProgress: TProgress;
    FOnSaveProgress: TProgress;
    FOnHorizontalScroll: TNotifyEvent;
    FOnVerticalScroll: TNotifyEvent;
    FOnShowEditor: TEditShowEvent;

    FCustomInplaceEditStyle: TEditStyle; // NEW
    FOnGetEditStyle: TJvOnGetEditStyleEvent;
    FPickListStrings: TStringList;
    FOnEditButtonClick: TNotifyEvent;
    FOnListBoxCloseUp: TNotifyEvent;

    FFixedFont: TFont;
    procedure SetAlignment(const Value: TAlignment);
    procedure GMActivateCell(var Msg: TGMActivateCell); message GM_ACTIVATECELL;
    procedure WMCommand(var Msg: TWMCommand); message WM_COMMAND;
    procedure SetFixedFont(const Value: TFont);
    procedure DoFixedFontChange(Sender: TObject);

    procedure EditButtonClick(Sender: TObject); dynamic;
    procedure ListBoxCloseUp(Sender: TObject); dynamic;
  protected
    function CreateEditor: TInplaceEdit; override;
    function CanEditShow: Boolean; override;
    procedure ColWidthsChanged; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure ExitCell(const EditText: string; AColumn, ARow: Integer); virtual;
    procedure SetCanvasProperties(AColumn, ARow: Longint; Rect: TRect; State: TGridDrawState); virtual;
    procedure DrawCell(AColumn, ARow: Longint; Rect: TRect; State: TGridDrawState); override;

    // NEW: Override to provide dropdown list editing as an event-handler in TJvStringGrid.
    function GetEditStyle(ACol, ARow: Longint): TEditStyle; override;

    procedure CaptionClick(AColumn, ARow: Longint); dynamic;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure DoLoadProgress(Position, Count: Integer);
    procedure DoSaveProgress(Position, Count: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetCellAlignment(AColumn, ARow: Longint;
      State: TGridDrawState): TAlignment; virtual;
    procedure DefaultDrawCell(AColumn, ARow: Longint;
      Rect: TRect; State: TGridDrawState); virtual;
    procedure ActivateCell(AColumn, ARow: Integer);

    // protected => public
    procedure InvalidateCell(AColumn, ARow: Integer);
    procedure InvalidateCol(AColumn: Integer);
    procedure InvalidateRow(ARow: Integer);
    procedure MoveColumn(FromIndex, ToIndex: Integer);
    procedure MoveRow(FromIndex, ToIndex: Longint);

    property GridState: TGridState read FGridState;
    property InplaceEditor;

    // Calculates and sets the width of a specific column or all columns if Index < 0
    // based on the text in the affected Cells.
    // MinWidth is the minimum width of the column(s). If MinWidth is < 0,
    // DefaultColWidth is used instead
    procedure AutoSizeCol(Index, MinWidth: Integer; AColumnPadding: Integer = 8);

    // Inserts a new row at the specified Index and moves all existing rows >= Index down one step
    // Returns the inserted row as an empty TStrings
    function InsertRow(Index: Integer): TStrings;

    // Inserts a new column at the specified Index and moves all existing columns >= Index to the right
    // Returns the inserted column as an empty TStrings
    function InsertCol(Index: Integer): TStrings;

    // Removes the row at Index and moves all rows > Index up one step
    procedure RemoveRow(Index: Integer);

    // Removes the column at Index and moves all cols > Index to the left
    procedure RemoveCol(Index: Integer);

    // Hides the row at Index by setting it's height = -1
    // Calling this method repeatedly does nothing (the row retains it's Index even if it's hidden)
    procedure HideRow(Index: Integer);

    // Shows the row at Index by setting it's height to AHeight
    // if AHeight <= 0, DefaultRowHeight is used instead
    procedure ShowRow(Index, AHeight: Integer);

    // Hides the column at Index by setting it's ColWidth = -1
    // Calling this method repeatedly does nothing (the column retains it's Index even if it's hidden)
    procedure HideCol(Index: Integer);

    // Returns True if the Cell at ACol/ARow is hidden, i.e if it's RowHeight or ColWidth < 0
    function IsHidden(ACol, ARow: Integer): Boolean;

    // Shows the column at Index by setting it's width to AWidth
    // If AWidth <= 0, DefaultColWidth is used instead
    procedure ShowCol(Index, AWidth: Integer);

    // HideCell hides a cell by hiding the row and column that it belongs to.
    // This means that both a row and a column is hidden
    procedure HideCell(ACol, ARow: Integer);

    // ShowCell shows a previously hidden cell by showing it's corresponding row and column and
    // using AWidth/AHeight to set it's size. If AWidth < 0, DefaultColWidth is used instead.
    // If AHeight < 0, DefaultRowHeight is used instead. If one dimension of the Cell wasn't
    // hidden, nothing happens to that dimension (i.e if ColWidth < 0 but RowHeight := 24, only ColWidth is
    // changed to AWidth
    procedure ShowCell(ACol, ARow, AWidth, AHeight: Integer);

    // Hides all rows and columns
    procedure HideAll;

    // Shows all hidden rows and columns, setting their width/height to AWidth/AHeight as necessary
    // If AWidth < 0, DefaultColWidth is used. If AHeight < 0, DefaultRowHeight is used
    procedure ShowAll(AWidth, AHeight: Integer);

    // Removes the content in the Cells but does not remove any rows or columns
    procedure Clear;

    // Clears selection rectangle!
    procedure ClearSelection;

    procedure SortGrid(Column: Integer; Ascending: Boolean = True; Fixed: Boolean = False;
      SortType: TJvSortType = stClassic; BlankTop: Boolean = True);

    // Sort grid using the column indices in ColOrder. For example if ColOrder contains
    // [1, 3, 0, 2], column 3 is used when the items in column 1 are identical
    procedure SortGridByCols(ColOrder: array of Integer; Fixed: Boolean = False);

    procedure SaveToFile(const FileName: string {$IFDEF UNICODE}; Encoding: TEncoding = nil{$ENDIF});
    procedure LoadFromFile(const FileName: string {$IFDEF UNICODE}; Encoding: TEncoding = nil{$ENDIF});
    procedure LoadFromCSV(const FileName: string; Separator: Char = ';'; QuoteChar: Char = '"'; StripQuotes: Boolean = True {$IFDEF UNICODE}; Encoding: TEncoding = nil{$ENDIF});
    procedure SaveToCSV(const FileName: string; Separator: Char = ';'; QuoteChar: Char = '"' {$IFDEF UNICODE}; Encoding: TEncoding = nil{$ENDIF});
    procedure LoadFromStream(Stream: TStream {$IFDEF UNICODE}; Encoding: TEncoding = nil{$ENDIF});
    procedure SaveToStream(Stream: TStream {$IFDEF UNICODE}; Encoding: TEncoding = nil{$ENDIF});
  published
    property HintColor;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property FixedFont: TFont read FFixedFont write SetFixedFont;
    property OnExitCell: TExitCellEvent read FOnExitCell write FOnExitCell;

    property OnSetCanvasProperties: TDrawCellEvent read FOnSetCanvasProperties write FOnSetCanvasProperties;
    property OnGetCellAlignment: TGetCellAlignmentEvent read FOnGetCellAlignment write FOnGetCellAlignment;
    property OnCaptionClick: TCaptionClickEvent read FCaptionClick write FCaptionClick;
    property OnColWidthsChanged: TNotifyEvent read FOnColWidthsChanged write FOnColWidthsChanged;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property OnLoadProgress: TProgress read FOnLoadProgress write FOnLoadProgress;
    property OnSaveProgress: TProgress read FOnSaveProgress write FOnSaveProgress;
    property OnVerticalScroll: TNotifyEvent read FOnVerticalScroll write FOnVerticalScroll;
    property OnHorizontalScroll: TNotifyEvent read FOnHorizontalScroll write FOnHorizontalScroll;
    property OnShowEditor: TEditShowEvent read FOnShowEditor write FOnShowEditor;

    property OnGetEditStyle: TJvOnGetEditStyleEvent read FOnGetEditStyle write FOnGetEditStyle;
    property OnEditButtonClick: TNotifyEvent read FOnEditButtonClick write FOnEditButtonClick; // User clicks on Ellipsis button, get event fired!
    property OnListBoxCloseUp: TNotifyEvent read FOnListBoxCloseUp write FOnListBoxCloseUp; // Listbox close up.
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
  Math,
  JclBase, // TBytes for Pre-Delphi 2007
  JvJVCLUtils;

//=== { TExInplaceEditList } =================================================

// If the feature exists in the VCL base classes, we can enable the
// feature here.
type
  TExInplaceEditList = class(TJvExPubInplaceEditList) // was inheriting from TJvExInplaceEdit.-WAP
  private
    // Important: Style of this inplace editor is set in TInplaceEditList
    // FEditStyle     - inherited - See VCL Source: ($delphi)\Source\vcl\Grids.pas
    //FActiveList    : TWinControl;    // WP-New: Listbox control stuff
    //FPickListLoaded: Boolean;        // WP-New
    //FPickList      : TCustomListbox; // WP-New
    //FListVisible   : Boolean;        // WP-New
    FLastCol: Integer;
    FLastRow: Integer;
  protected
    procedure CloseUp(Accept: Boolean); override; // fire event on close up!
    procedure DoEditButtonClick; override;
    procedure UpdateContents; override; //WP-New! - Put items into listbox!
    procedure FocusKilled(NextWnd: THandle); override;
    procedure FocusSet(PrevWnd: THandle); override;
  public
    constructor Create(Owner: TComponent); override;//WP-New!
    //property ActiveList: TWinControl read FActiveList write FActiveList;//WP-New!
    procedure CreateParams(var Params: TCreateParams); override;
  end;

constructor TExInplaceEditList.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  // todo: tweakage!
end;

//NEW!
procedure TExInplaceEditList.UpdateContents;
var
 OwnerGrid: TJvStringGrid;
begin
  inherited UpdateContents;
  if EditStyle = esPickList then
  begin
    ActiveList := PickList;
    // Populate the listbox:
    Assert(Assigned(Grid));
    OwnerGrid := (Grid as TJvStringGrid);
    PickList.Items.Assign(OwnerGrid.FPickListStrings);
  end;
end;

procedure TExInplaceEditList.CreateParams(var Params: TCreateParams);
const
  Flags: array [TAlignment] of DWORD = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or Flags[TJvStringGrid(Grid).Alignment];
end;

procedure TExInplaceEditList.FocusKilled(NextWnd: THandle);
begin
  TJvStringGrid(Grid).ExitCell(Text, FLastCol, FLastRow);
  inherited FocusKilled(NextWnd);
end;

procedure TExInplaceEditList.FocusSet(PrevWnd: THandle);
begin
  FLastCol := TJvStringGrid(Grid).Col;
  FLastRow := TJvStringGrid(Grid).Row;
  inherited FocusSet(PrevWnd);
end;

procedure TExInplaceEditList.CloseUp(Accept: Boolean); //override; // fire event on close up!
begin
  inherited CloseUp(Accept);
  if Assigned(Grid) then
    TJvStringGrid(Grid).ListBoxCloseUp(Self);
end;

procedure TExInplaceEditList.DoEditButtonClick;
begin
  if Assigned(Grid) then
    TJvStringGrid(Grid).EditButtonClick(Self);
end;

//=== { TJvStringGrid } ======================================================

constructor TJvStringGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFixedFont := TFont.Create;
  FFixedFont.Assign(Font);
  FFixedFont.OnChange := DoFixedFontChange;
  // ControlStyle := ControlStyle + [csAcceptsControls];
  FPickListStrings := TStringList.Create;
end;

destructor TJvStringGrid.Destroy;
begin
  FreeAndNil(FFixedFont);
  FreeAndNil(FPickListStrings);
  inherited Destroy;
end;

function TJvStringGrid.CanEditShow: Boolean;
begin
  Result := inherited CanEditShow;
  if Result and Assigned(FOnShowEditor) then
  begin
    FOnShowEditor(Self, Col, Row, Result);
    if not Result then
      EditorMode := False;
  end;
end;

procedure TJvStringGrid.ColWidthsChanged;
begin
  inherited;
  if Assigned(FOnColWidthsChanged) then
    FOnColWidthsChanged(Self);
end;

procedure TJvStringGrid.SortGrid(Column: Integer;
  Ascending, Fixed: Boolean; SortType: TJvSortType; BlankTop: Boolean);
const
  cFloatDelta = 0.01;
var
  St: string;
  TmpC: Currency;
  TmpF: Extended;
  TmpD: TDateTime;
  LStart: Integer;
  LEnd: Integer;

  procedure ExchangeGridRows(I, J: Integer);
  var
    K: Integer;
  begin
    if Fixed then
      for K := 0 to ColCount - 1 do
        Cols[K].Exchange(I, J)
    else
      for K := FixedCols to ColCount - 1 do
        Cols[K].Exchange(I, J);
  end;

  function IsSmaller(First, Second: string): Boolean;

    function DetectType(const S1, S2: string): TJvSortType;
    var
      ExtValue: Extended;
      CurrValue: Currency;
      DateValue: TDateTime;
    begin
      if TextToFloat(PChar(S1), ExtValue, fvExtended) and TextToFloat(PChar(S2), ExtValue, fvExtended) then
        Result := stNumeric
      else
      if TextToFloat(PChar(S1), CurrValue, fvCurrency) and TextToFloat(PChar(S2), CurrValue, fvCurrency) then
        Result := stCurrency
      else
      if TryStrToDateTime(S1, DateValue) and TryStrToDateTime(S2, DateValue) then
        Result := stDate
      else
        Result := stClassic;
    end;
  begin
    case DetectType(First, Second) of
      stNumeric:
        Result := StrToFloat(First) < StrToFloat(Second);
      stCurrency:
        Result := StrToCurr(First) < StrToCurr(Second);
      stDate:
        Result := StrToDateTime(First) < StrToDateTime(Second);
      stClassic:
        Result := AnsiCompareText(First, Second) < 0;
    else
      Result := First > Second;
    end;
  end;

  function IsBigger(First, Second: string): Boolean;
  begin
    Result := IsSmaller(Second, First);
  end;
  // (rom) A HeapSort has no worst case for O(X)
  // (rom) I donated one a long time ago to JCL
  // (p3) maybe implemented a secondary sort index when items are equal?
  // (p3) ...or use another stable sort method, like heapsort

  procedure QuickSort(L, R: Integer);
  var
    I, J, m: Integer;
  begin
    repeat
      I := L;
      J := R;
      m := (L + R) div 2;
      St := Cells[Column, m];
      repeat
        case SortType of
          stClassic:
            begin
              while AnsiCompareText(Cells[Column, I], St) < 0 do
                Inc(I);
              while AnsiCompareText(Cells[Column, J], St) > 0 do
                Dec(J);
            end;
          stCaseSensitive:
            begin
              while AnsiCompareStr(Cells[Column, I], St) < 0 do
                Inc(I);
              while AnsiCompareStr(Cells[Column, J], St) > 0 do
                Dec(J);
            end;
          stNumeric:
            begin
              TmpF := JvSafeStrToFloatDef(St, 0);  // formerly StrToFloatDefIgnoreInvalidCharacters
              while JvSafeStrToFloatDef(Cells[Column, I], 0) < TmpF do
                Inc(I);
              while JvSafeStrToFloatDef(Cells[Column, J], 0) > TmpF do
                Dec(J);
            end;
          stDate:
            begin
              TmpD := StrToDateTimeDef(St, 0);
              while StrToDateTimeDef(Cells[Column, I], 0) < TmpD do
                Inc(I);
              while StrToDateTimeDef(Cells[Column, J], 0) > TmpD do
                Dec(J);
            end;
          stCurrency:
            begin
              TmpC := StrToCurrDef(St, 0);
              while StrToCurrDef(Cells[Column, I], 0) < TmpC do
                Inc(I);
              while StrToCurrDef(Cells[Column, J], 0) > TmpC do
                Dec(J);
            end;
          stAutomatic:
            begin
              while IsSmaller(Cells[Column, I], St) do
                Inc(I);
              while IsBigger(Cells[Column, J], St) do
                Dec(J);
            end;
        end;
        if I <= J then
        begin
          if I <> J then
            ExchangeGridRows(I, J);
          Inc(I);
          Dec(J);
        end;
      until (I > J);
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

  procedure InvertGrid;
  var
    I, J: Integer;
  begin
    I := FixedRows;
    J := RowCount - 1;
    while I < J do
    begin
      ExchangeGridRows(I, J);
      Inc(I);
      Dec(J);
    end;
  end;

  function MoveBlankTop: Integer;
  var
    I, J: Integer;
  begin
    I := FixedRows;
    Result := I;
    J := RowCount - 1;
    while I <= J do
    begin
      if Trim(Cells[Column, I]) = '' then
      begin
        ExchangeGridRows(Result, I);
        Inc(Result);
      end;
      Inc(I);
    end;
  end;

  procedure MoveBlankBottom;
  var
    I, J: Integer;
    DoSort: Boolean;
  begin
    I := FixedRows;
    DoSort := False;
    // avoid empty columns
    for J := I to RowCount - 1 do
      if Cells[Column, J] <> '' then
      begin
        DoSort := True;
        Break;
      end;
    if not DoSort then
      Exit;
    // this is already sorted, so blank items should be at top
    while Trim(Cells[Column, I]) = '' do
    begin
      InsertRow(RowCount).Assign(Rows[I]);
      DeleteRow(I);
      Inc(J);
      if J >= RowCount then
        Exit;
    end;
  end;

begin
  // (p3) NB!! sorting might trigger the OnExitCell, OnGetEditText and OnSetEditText events!
  // make sure you don't do anything in these events
  if (Column >= 0) and (Column < ColCount) and (SortType <> stNone) then
  begin
    LStart := FixedRows;
    LEnd := RowCount - 1;
    if BlankTop then
      LStart := MoveBlankTop;
    if LStart < LEnd then
    begin
      QuickSort(LStart, LEnd);
      if not BlankTop then
        MoveBlankBottom;
      if not Ascending then
        InvertGrid;
    end;
  end;
end;

procedure TJvStringGrid.LoadFromFile(const FileName: string {$IFDEF UNICODE}; Encoding: TEncoding{$ENDIF});
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream {$IFDEF UNICODE}, Encoding{$ENDIF});
  finally
    Stream.Free;
  end;
end;

procedure TJvStringGrid.LoadFromCSV(const FileName: string; Separator: Char = ';'; QuoteChar: Char = '"'; StripQuotes: Boolean = True {$IFDEF UNICODE}; Encoding: TEncoding = nil{$ENDIF});
var
  I: Longint;
  Lines, Fields: TStringList;

  procedure SplitLine(const Line: string; Result: TStrings; Delimiter, QuoteChar: Char; StripQuotes: Boolean);
  var
    I, SLen, QuoteCount: Integer;
    S: string;
    IgnoreDelim: Boolean;
    QuotedStr: PChar;
  begin
    S := '';
    SLen := Length(Line);
    IgnoreDelim := False;
    QuoteCount := 0;
    Result.Clear;
    for I := 1 to SLen do
    begin
      if Line[I] = QuoteChar then
      begin
        Inc(QuoteCount);
        {* A Delimiter surrounded by a pair of QuoteChar has to be ignored.
           See example above: "FirstName, LastName"
           therefor: *}
        IgnoreDelim := QuoteCount mod 2 <> 0;
      end;

      if IgnoreDelim then
        S := S + Line[I]
      else
      if Line[I] <> Delimiter then
        S := S + Line[I]
      else
      begin
        if S <> '' then
        begin
          if StripQuotes and (S[1] = QuoteChar) then
          begin
            QuotedStr := PChar(S);
            Result.Add(AnsiExtractQuotedStr(QuotedStr, QuoteChar));
          end
          else
            Result.Add(S);
        end
        else
          Result.Add(S);

        S := '';
      end;
    end;
    if S <> '' then
    begin
      if StripQuotes and (S[1] = QuoteChar) then
      begin
        QuotedStr := PChar(S);
        Result.Add(AnsiExtractQuotedStr(QuotedStr, QuoteChar));
      end
      else
        Result.Add(S);
    end
    else
      Result.Add(S);
  end;

begin
  Lines := TStringList.Create;
  Fields := TStringList.Create;
  try
    Lines.LoadFromFile(FileName {$IFDEF UNICODE}, Encoding{$ENDIF});
    DoLoadProgress(0, Lines.Count);
    RowCount := Lines.Count;
    ColCount := FixedCols + 1;
    for I := 0 to Lines.Count - 1 do
    begin
      {* added John *}
      SplitLine(Lines[I], Fields, Separator, QuoteChar, StripQuotes);
      DoLoadProgress(I, Lines.Count);

      if Fields.Count > ColCount then
        ColCount := Fields.Count;
      Rows[I].Assign(Fields);
    end;
    DoLoadProgress(Lines.Count, Lines.Count);
  finally
    Fields.Free;
    Lines.Free;
  end;
end;

procedure TJvStringGrid.LoadFromStream(Stream: TStream {$IFDEF UNICODE}; Encoding: TEncoding{$ENDIF});
const
  BufSize = 4096;
  GrowSize = 1024;
var
  Col, Row, I, Count: Integer;
  Buffer: array [0..BufSize - 1] of Byte;
  Bytes: TBytes;
  Len: Integer;
  Size: Int64;

  procedure SetCell;
  var
    St: string;
  begin
    {$IFDEF UNICODE}
    St := Encoding.GetString(Bytes, 0, Len);
    {$ELSE}
    SetString(St, PAnsiChar(@Bytes[0]), Len);
    {$ENDIF UNICODE}
    Cells[Col - 1, Row - 1] := St;

    if Length(Bytes) > BufSize then
      SetLength(Bytes, BufSize);
    Len := 0;
  end;

begin
  {$IFDEF UNICODE}
  if Encoding = nil then
    Encoding := TEncoding.Default;
  {$ENDIF UNICODE}
  Col := 0;
  Row := 1;
  Size := Stream.Size;
  DoLoadProgress(0, Size);
  Len := 0;
  SetLength(Bytes, BufSize);
  while Stream.Position < Size do
  begin
    Count := Stream.Read(Buffer, BufSize);
    DoLoadProgress(Stream.Position, Size);
    for I := 0 to Count - 1 do
      case Buffer[I] of
        0:
          begin
            Inc(Col);
            if Row > RowCount then
              RowCount := Row;
            if Col > ColCount then
              ColCount := Col;
            SetCell;
          end;
        1:
          begin
            Inc(Col);
            if Col > ColCount then
              ColCount := Col;
            SetCell;
            Inc(Row);
            if Row > RowCount then
              RowCount := Row;
            Col := 0;
          end;
      else
        if Len >= Length(Bytes) then
          SetLength(Bytes, Len + GrowSize);
        Bytes[Len] := Buffer[I];
        Inc(Len);
      end;
  end;
  RowCount := RowCount - 1;
  DoLoadProgress(Stream.Size, Stream.Size);
end;

procedure TJvStringGrid.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  if Assigned(FOnHorizontalScroll) then
    FOnHorizontalScroll(Self);
end;

procedure TJvStringGrid.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  if Assigned(FOnVerticalScroll) then
    FOnVerticalScroll(Self);
end;

procedure TJvStringGrid.SaveToFile(const FileName: string {$IFDEF UNICODE}; Encoding: TEncoding{$ENDIF});
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
  try
    SaveToStream(Stream {$IFDEF UNICODE}, Encoding{$ENDIF});
  finally
    Stream.Free;
  end;
end;

procedure TJvStringGrid.SaveToCSV(const FileName: string; Separator: Char = ';'; QuoteChar: Char = '"' {$IFDEF UNICODE}; Encoding: TEncoding = nil{$ENDIF});
var
  I, J: Longint;
  BufStr, Value: string;
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  DoSaveProgress(0, RowCount);
  try
    Lines.Clear;
    for I := 0 to RowCount - 1 do
    begin
      BufStr := '';
      DoSaveProgress(I, RowCount);
      for J := 0 to ColCount - 1 do
      begin
        Value := Cells[J, I];
        if Pos(Separator, Value) > 0 then
          Value := AnsiQuotedStr(Value, QuoteChar);
        BufStr := BufStr + Value;
        if J <> (ColCount - 1) then
          BufStr := BufStr + Separator;
      end;
      Lines.Add(BufStr);
    end;
    DoSaveProgress(RowCount, RowCount);
    Lines.SaveToFile(FileName {$IFDEF UNICODE}, Encoding{$ENDIF});
  finally
    Lines.Free;
  end;
end;

procedure TJvStringGrid.SaveToStream(Stream: TStream {$IFDEF UNICODE}; Encoding: TEncoding{$ENDIF});
var
  I, J, ATotal: Integer;
  {$IFNDEF UNICODE}
  K: Integer;
  {$ENDIF ~UNICODE}
  Bytes: TBytes;
  St: string;
  Len: Integer;
  A, B: Byte;
begin
  {$IFDEF UNICODE}
  if Encoding = nil then
    Encoding := TEncoding.Default;
  {$ENDIF UNICODE}
  A := 0;
  B := 1; // A for end of string, B for end of line
  ATotal := RowCount * ColCount;
  DoSaveProgress(0, ATotal);
  for I := 0 to RowCount - 1 do
  begin
    for J := 0 to ColCount - 1 do
    begin
      DoSaveProgress(I * ColCount + J, ATotal);
      St := Cells[J, I];
      {$IFDEF UNICODE}
      Bytes := Encoding.GetBytes(St);
      Len := Length(Bytes);
      {$ELSE}
      Len := Length(St);
      if Length(Bytes) < Len then
        SetLength(Bytes, Len);
      for K := 0 to Len - 1 do
        if (St[K + 1] = #1) or (St[K + 1] = #0) then
          Bytes[K] := 32
        else
          Bytes[K] := Byte(St[K + 1]);
      {$ENDIF}
      if Len > 0 then
        Stream.Write(Bytes[0], Len);
      if J <> ColCount - 1 then
        Stream.Write(A, 1);
    end;
    Stream.Write(B, 1);
  end;
  DoSaveProgress(ATotal, ATotal);
end;

procedure TJvStringGrid.ActivateCell(AColumn, ARow: Integer);
begin
  PostMessage(Handle, GM_ACTIVATECELL, AColumn, ARow);
end;

procedure TJvStringGrid.CaptionClick(AColumn, ARow: Integer);
begin
  if Assigned(FCaptionClick) then
    FCaptionClick(Self, AColumn, ARow);
end;

function TJvStringGrid.CreateEditor: TInplaceEdit;
begin
  Result := TExInplaceEditList.Create(Self);
end;

procedure TJvStringGrid.DefaultDrawCell(AColumn, ARow: Integer; Rect: TRect;
  State: TGridDrawState);
const
  Flags: array [TAlignment] of DWORD = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  S: string;
begin
  if RowHeights[ARow] < CanvasMaxTextHeight(Canvas) then
    Exit;
  Canvas.FillRect(Rect);
  S := Cells[AColumn, ARow];
  if Length(S) > 0 then
  begin
    InflateRect(Rect, -2, -2);
    DrawText(Canvas, S, Length(S), Rect,
      DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER or
      Flags[GetCellAlignment(AColumn, ARow, State)]);
  end;
end;

function TJvStringGrid.GetEditStyle(ACol, ARow: Longint): TEditStyle; //override;
begin
   FCustomInplaceEditStyle := esSimple;
   if Assigned(FOnGetEditStyle) then
   begin
     FPickListStrings.Clear;
     FOnGetEditStyle(Self, ACol, ARow, FPickListStrings, FCustomInplaceEditStyle);
   end;
   Result := FCustomInplaceEditStyle;
end;

procedure TJvStringGrid.DrawCell(AColumn, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  if (AColumn < FixedCols) or (ARow < FixedRows) then
    Canvas.Font := FixedFont;
  if Assigned(OnDrawCell) then
    inherited DrawCell(AColumn, ARow, Rect, State)
  else
  begin
    SetCanvasProperties(AColumn, ARow, Rect, State);
    DefaultDrawCell(AColumn, ARow, Rect, State);
    Canvas.Font := Font;
    Canvas.Brush := Brush;
  end;
end;

procedure TJvStringGrid.ExitCell(const EditText: string;
  AColumn, ARow: Integer);
begin
  if Assigned(FOnExitCell) then
    FOnExitCell(Self, AColumn, ARow, EditText);
end;

function TJvStringGrid.GetCellAlignment(AColumn, ARow: Integer;
  State: TGridDrawState): TAlignment;
begin
  Result := FAlignment;
  if Assigned(FOnGetCellAlignment) then
    FOnGetCellAlignment(Self, AColumn, ARow, State, Result);
end;

procedure TJvStringGrid.GMActivateCell(var Msg: TGMActivateCell);
begin
  Col := Msg.Column;
  Row := Msg.Row;
  EditorMode := True;
  if Assigned(InplaceEditor) then
    InplaceEditor.SelectAll;
end;

procedure TJvStringGrid.InvalidateCell(AColumn, ARow: Integer);
begin
  inherited InvalidateCell(AColumn, ARow);
end;

procedure TJvStringGrid.InvalidateCol(AColumn: Integer);
begin
  inherited InvalidateCol(AColumn);
end;

procedure TJvStringGrid.InvalidateRow(ARow: Integer);
begin
  inherited InvalidateRow(ARow);
end;

procedure TJvStringGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
    MouseToCell(X, Y, FCellOnMouseDown.X, FCellOnMouseDown.Y)
  else
    FCellOnMouseDown := TGridCoord(Point(-1, -1));
end;

procedure TJvStringGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Cell: TGridCoord;
begin
  if Button = mbLeft then
    MouseToCell(X, Y, Cell.X, Cell.Y);
  if CompareMem(@Cell, @FCellOnMouseDown, SizeOf(Cell)) and
    ((Cell.X < FixedCols) or (Cell.Y < FixedRows)) then
    CaptionClick(Cell.X, Cell.Y);
  FCellOnMouseDown := TGridCoord(Point(-1, -1));
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvStringGrid.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
    if Assigned(InplaceEditor) then
      TExInplaceEditList(InplaceEditor).RecreateWnd;
  end;
end;

procedure TJvStringGrid.SetCanvasProperties(AColumn, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
  if Assigned(FOnSetCanvasProperties) then
    FOnSetCanvasProperties(Self, AColumn, ARow, Rect, State);
end;

procedure TJvStringGrid.WMCommand(var Msg: TWMCommand);
begin
  if EditorMode and (Msg.Ctl = InplaceEditor.Handle) then
    inherited
  else
  if Msg.Ctl <> 0 then
    Msg.Result := SendMessage(Msg.Ctl, CN_COMMAND, TMessage(Msg).wParam, TMessage(Msg).lParam);
end;

function TJvStringGrid.InsertCol(Index: Integer): TStrings;
var
  I: Integer;
  LStr: TStrings;
begin
  ColCount := ColCount + 1;
  if Index < 0 then
    Index := 0;
  if Index >= ColCount then
    Index := ColCount - 1;
  Result := Cols[Index];
  if ColCount = 1 then
    Exit;
  for I := ColCount - 2 downto Index do
  begin
    LStr := Cols[I];
    Cols[I + 1] := LStr;
  end;
  Result := Cols[Index];
  Result.Clear;
end;

function TJvStringGrid.InsertRow(Index: Integer): TStrings;
var
  I: Integer;
  LStr: TStrings;
begin
  RowCount := RowCount + 1;
  if Index < 0 then
    Index := 0;
  if Index >= RowCount then
    Index := RowCount - 1;
  Result := Rows[Index];
  if RowCount = 1 then
    Exit;
  for I := RowCount - 2 downto Index do
  begin
    LStr := Rows[I];
    Rows[I + 1] := LStr;
  end;
  Result.Clear;
end;

procedure TJvStringGrid.RemoveCol(Index: Integer);
var
  I: Integer;
  LStr: TStrings;
begin
  if Index < 0 then
    Index := 0;
  if Index >= ColCount then
    Index := ColCount - 1;
  for I := Index + 1 to ColCount - 1 do
  begin
    LStr := Cols[I];
    Cols[I - 1] := LStr;
  end;
  if ColCount > 1 then
    ColCount := ColCount - 1;
end;

procedure TJvStringGrid.RemoveRow(Index: Integer);
var
  I: Integer;
  LStr: TStrings;
begin
  if Index < 0 then
    Index := 0;
  if Index >= RowCount then
    Index := RowCount - 1;
  for I := Index + 1 to RowCount - 1 do
  begin
    LStr := Rows[I];
    Rows[I - 1] := LStr;
  end;
  if RowCount > 1 then
    RowCount := RowCount - 1;
end;

procedure TJvStringGrid.Clear;
var
  I: Integer;
begin
  for I := 0 to ColCount - 1 do
    Cols[I].Clear;
end;

procedure TJvStringGrid.HideCol(Index: Integer);
begin
  ColWidths[Index] := -1;
end;

procedure TJvStringGrid.HideRow(Index: Integer);
begin
  RowHeights[Index] := -1;
end;

procedure TJvStringGrid.ShowCol(Index, AWidth: Integer);
begin
  if AWidth <= 0 then
    AWidth := DefaultColWidth;
  ColWidths[Index] := AWidth;
end;

procedure TJvStringGrid.ShowRow(Index, AHeight: Integer);
begin
  if AHeight <= 0 then
    AHeight := DefaultRowHeight;
  RowHeights[Index] := AHeight;
end;

procedure TJvStringGrid.SetFixedFont(const Value: TFont);
begin
  FFixedFont.Assign(Value);
end;

procedure TJvStringGrid.DoFixedFontChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvStringGrid.ListBoxCloseUp(Sender: TObject);
begin
  // invoked dynamically from the TInplaceEditList
  if Assigned(FOnListBoxCloseUp) then
    FOnListBoxCloseUp(Self)
end;

procedure TJvStringGrid.EditButtonClick(Sender: TObject);
begin
  if Assigned(FOnEditButtonClick) then
    FOnEditButtonClick(Self)
end;

procedure TJvStringGrid.AutoSizeCol(Index, MinWidth: Integer; AColumnPadding: Integer);

  // We must use the cell's font for the GetTextExtentPoint32() call.
  procedure SetCanvasPropertiesForCell(AColumn, ARow: Integer);
  begin
    if (AColumn < FixedCols) or (ARow < FixedRows) then
    begin
      Canvas.Font := FixedFont;
      SetCanvasProperties(AColumn, ARow, CellRect(AColumn, ARow), [gdFixed]);
    end
    else
    begin
      Canvas.Font := Font;
      SetCanvasProperties(AColumn, ARow, CellRect(AColumn, ARow), []);
    end;
  end;

var
  I, J, AColWidth: Integer;
  ASize: TSize;
begin
  if (Index >= 0) and (Index < ColCount) then
  begin
    if MinWidth < 0 then
      AColWidth := DefaultColWidth
    else
      AColWidth := MinWidth;
    for J := 0 to RowCount - 1 do
    begin
      SetCanvasPropertiesForCell(Index, J);
      if GetTextExtentPoint32(Canvas.Handle, PChar(Cells[Index, J]), Length(Cells[Index, J]), ASize) then
        AColWidth := Max(AColWidth, ASize.cx + AColumnPadding);
    end;
    ColWidths[Index] := AColWidth;
  end
  else
  begin
    for I := 0 to ColCount - 1 do
    begin
      if MinWidth < 0 then
        AColWidth := DefaultColWidth
      else
        AColWidth := MinWidth;
      for J := 0 to RowCount - 1 do
      begin
        SetCanvasPropertiesForCell(Index, J);
        if GetTextExtentPoint32(Canvas.Handle, PChar(Cells[I, J]), Length(Cells[I, J]), ASize) then
          AColWidth := Max(AColWidth, ASize.cx + AColumnPadding);
      end;
      ColWidths[I] := AColWidth;
    end;
  end;
end;

procedure TJvStringGrid.HideAll;
var
  I: Integer;
begin
  if ColCount < RowCount then
    for I := 0 to ColCount - 1 do
      ColWidths[I] := -1
  else
    for I := 0 to RowCount - 1 do
      RowHeights[I] := -1;
end;

procedure TJvStringGrid.ClearSelection; // Clears selection rectangle!
var
 S: TGridRect;
begin
  S.Left := -1;
  S.Top := -1;
  S.Right := -1;
  S.Bottom := -1;
  Self.Selection  := S;
  Refresh;
end;

procedure TJvStringGrid.ShowAll(AWidth, AHeight: Integer);
var
  I: Integer;
begin
  if AWidth < 0 then
    AWidth := DefaultColWidth;
  if AHeight < 0 then
    AHeight := DefaultRowHeight;
  for I := 0 to ColCount - 1 do
    if ColWidths[I] < 0 then
      ColWidths[I] := AWidth;
  for I := 0 to RowCount - 1 do
    if RowHeights[I] < 0 then
      RowHeights[I] := AHeight;
end;

function TJvStringGrid.IsHidden(ACol, ARow: Integer): Boolean;
begin
  Result := (ColWidths[ACol] < 0) or (RowHeights[ARow] < 0);
end;

procedure TJvStringGrid.HideCell(ACol, ARow: Integer);
begin
  ColWidths[ACol] := -1;
  RowHeights[ARow] := -1;
end;

procedure TJvStringGrid.ShowCell(ACol, ARow, AWidth, AHeight: Integer);
begin
  if AWidth < 0 then
    AWidth := DefaultColWidth;
  if AHeight < 0 then
    AWidth := DefaultRowHeight;
  if ColWidths[ACol] < 0 then
    ColWidths[ACol] := AWidth;
  if RowHeights[ARow] < 0 then
    RowHeights[ARow] := AHeight;
end;

procedure TJvStringGrid.DoLoadProgress(Position, Count: Integer);
begin
  if Assigned(FOnLoadProgress) then
    FOnLoadProgress(Self, Position, Count);
end;

procedure TJvStringGrid.DoSaveProgress(Position, Count: Integer);
begin
  if Assigned(FOnSaveProgress) then
    FOnSaveProgress(Self, Position, Count);
end;

procedure TJvStringGrid.SortGridByCols(ColOrder: array of Integer; Fixed: Boolean);
var
  I, J, FirstRow: Integer;
  Sorted: Boolean;

  function Sort(Row1, Row2: Integer): Integer;
  var
    C: Integer;
  begin
    C := 0;
    Result := AnsiCompareStr(Cols[ColOrder[C]][Row1], Cols[ColOrder[C]][Row2]);
    if Result = 0 then
    begin
      Inc(C);
      while (C <= High(ColOrder)) and (Result = 0) do
      begin
        Result := AnsiCompareStr(Cols[ColOrder[C]][Row1], Cols[ColOrder[C]][Row2]);
        Inc(C);
      end;
    end;
  end;

begin
  for I := 0 to High(ColOrder) do
    if (ColOrder[I] < 0) or (ColOrder[I] >= ColCount) then
      Exit;

  if Fixed then
    FirstRow := 0
  else
    FirstRow := FixedRows;

  J := FirstRow;
  Sorted := True;
  repeat
    Inc(J);
    for I := FirstRow to RowCount - 2 do
      if Sort(I, I + 1) > 0 then
      begin
        MoveRow(I + 1, I);
        Sorted := False;
      end;
  until Sorted or (J >= RowCount + 1000);
  Repaint;
end;

procedure TJvStringGrid.MoveColumn(FromIndex, ToIndex: Integer);
begin
  inherited MoveColumn(FromIndex, ToIndex);
end;

procedure TJvStringGrid.MoveRow(FromIndex, ToIndex: Integer);
begin
  inherited MoveRow(FromIndex, ToIndex);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.


