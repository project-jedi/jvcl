{
-----------------------------------------------------------------------------

                               UltimDBGrid 1.3

                        VCL for Delphi and C++Builder
                       (C) Frédéric Leneuf-Magaud 2001

-----------------------------------------------------------------------------

 Please read ReadMe.txt for conditions of use

 This component is provided 'as-is', without any express or implied warranty.
 In no event shall the author be held liable for any damages arising from the
 use of this component.

 Special thanks go to:
 - Giles Lindsay (ExDBGrid)
 - Gerald Nunn (GXDBGrid)

-----------------------------------------------------------------------------

History of changes / Historique des modifications:

   1.1 : 07/03/2001
         - Fixed display bug in DrawColumnCell & PopUpHint
         - Fixed loading bug in AutoWidth
         - Fixed bad font assignments
         - Fixed bad index in CanEditField

   1.2 : 26/05/2001
         - No more sorting of calculated fields and aggregates

   1.3 : 13/09/2001
         - Display of JPEG, GIF, ICO, EMF and WMF images
         - New property: StretchDraw
         - New procedure: CloneLayout
         - New function: FillWithSelectedRecords
         - Fixed bug in SaveGridConfig
         - Added font style in config files

-----------------------------------------------------------------------------
}

unit UltimDBGrid;

interface

{$I OPTIONS.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Menus, Dialogs, Grids, DB, DBGrids,
  RxDBCtrl, RxMemDS,
  {$IFDEF DELPHI_6} //1.3
  RxStrUtils, Variants,
  {$ELSE}
  StrUtils,
  {$ENDIF}
  AppUtils, IniFiles,
  {$IFDEF ADO}
  ADODb,
  {$ENDIF}
  {$IFDEF BDE}
  BDE, DBTables,
  {$ENDIF}
  {$IFDEF IMG_JPEG}
  jpeg, //1.3
  {$ENDIF}
  {$IFDEF IMG_GIF}
  RXGif, //1.3
  {$ENDIF}
  {$IFDEF QREPORT}
  UltimDBReport,
  {$ENDIF}
  UltimDBSelect, UltimDBSort, UltimDBFilter, UltimDBConfig, UltimDBExport;

{$I LANG_STR.INC}

type
  TOptionMenuItem = (mnuAutoWidth, mnuAutoWidthOnResize, mnuAllowColResizing,
    mnuAllowRowResizing, mnuSetFixedColumns, mnuShowHideColumns,
    mnuSortColumns, mnuSetFilter, mnuUseFilter, mnuAddCurrentFilter,
    mnuDisplayBooleans, mnuDisplayImages, mnuDisplayMemo, mnuFullSizeMemo,
    mnuUseRowColors, mnuShowCellHints, mnuShowTextEllipsis, mnuMultiLinesCell,
    mnuEnterKeyAsTab, mnuExportGrid, mnuPrintGrid, mnuReadOnly,
    mnuSaveConfiguration, mnuRestoreConfiguration);
  TOptionsMenuItems = set of TOptionMenuItem;

const
  CheckBoxSize = 13;
  clRowColor1 = clInfoBk;
  clRowColor2 = clWhite;
  soSortASC = True;
  soSortUP = True;
  soSortDESC = False;
  soSortDOWN = False;
  DefaultMenuItems = [mnuAutoWidth, mnuAutoWidthOnResize, mnuSetFixedColumns, mnuShowHideColumns,
    mnuSortColumns, mnuSetFilter, mnuUseFilter, mnuAddCurrentFilter, mnuDisplayImages,
    mnuDisplayMemo, mnuFullSizeMemo, mnuShowCellHints, mnuMultiLinesCell, mnuExportGrid,
    mnuPrintGrid, mnuSaveConfiguration, mnuRestoreConfiguration];
  DefaultDelimiter = ';';
  DefaultExportName = 'UGExport.htm';
  DefaultIniName = 'UGConfig.ini';
  tagSAVE = 0;
  tagRESTORE = 1;

type
  TSortField = record
    Name: string;
    Order: Boolean;
  end;
  TSortFields = array of TSortField;

  TUltimFilter = record
    Name: string;
    Filter: string;
  end;
  TFilterList = array of TUltimFilter;

  TConfigOptions = (cfgLayout, cfgSortSettings, cfgFilters);
  TExportFormat = (efHTML, efSYLK, efText);

  TUltimDBGrid = class;

  TUltimControl = class(TCollectionItem)
  private
    FControlName: string;
    FFieldName: string;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property ControlName: string read FControlName write FControlName;
    property FieldName: string read FFieldName write FFieldName;
  end;

  TUltimControls = class(TCollection)
  private
    FUltimGrid: TUltimDBGrid;
    function GetItem(Index: Integer): TUltimControl;
    procedure SetItem(Index: Integer; Value: TUltimControl);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(UltimGrid: TUltimDBGrid);
    function Add: TUltimControl;
    function ControlByField(FieldName: string): TUltimControl;
    property Items[Index: Integer]: TUltimControl read GetItem write SetItem;
  end;

  TAutoWidth = (awNone, awDefault, awProportional, awUniform, awWidestValue, awWidestValueTitle);
  TAutoWidthEvent = procedure(Sender: TObject; var NewValue: TAutoWidth) of object;

  TUltimDBGrid = class(TRxDBGrid)
  private
    FBackBuffer: TBitmap;
    FNoFlickering: Boolean;
    FControls: TUltimControls;
    FCurrentControl: TWinControl;
    FOldControlWndProc: TWndMethod;
    FCustomEditor: Boolean;
    FBooleanEditor: Boolean;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FAutoWidth: TAutoWidth;
    FOnAutoWidthChange: TAutoWidthEvent;
    FAutoWidthOnResize: Boolean;
    FDisplayBoolean: Boolean;
    FDisplayImages: Boolean;
    FDisplayMemo: Boolean;
    FFullSizeMemo: Boolean;
    FUseRowColors: Boolean;
    FRowColors: array[0..1] of TColor;
    FRowSizingAllowed: Boolean;
    FLockedHeight: Word;
    FTitleRowHeight: Integer;
    FNewDefRowHeight: Integer;
    FLockedWidth: Boolean;
    FHintWnd: THintWindow;
    FCellHints: Boolean;
    FShowTextEllipsis: Boolean;
    FMultiLines: Boolean;
    FSortOnTitleClick: Boolean;
    FSortedFields: TSortFields;
    FOldGetBtnParams: TGetBtnParamsEvent;
    FEnterAsTab: Boolean;
    FGridPopUpMenu: TPopUpMenu;
    FOptionsMenu: Boolean;
    FOptionsMenuItems: TOptionsMenuItems;
    FSavedBookmark: TBookmarkStr;
    FSavedRowPos: Integer;
    FValueToSearch: Variant;
    FSearchFields: TStringList;
    FOnMenuPrintGrid: TNotifyEvent;
    FReportTitle: string;
    FStretchDraw: Boolean; //1.3
    FFooter: TWinControl;
    procedure SendSizeMessage;
    function CanEditField: Boolean;
    procedure ChangeBoolean(Field: TField);
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure SetNoFlickering(Value: Boolean);
    procedure SetControls(Value: TUltimControls);
    procedure HideCurrentControl;
    procedure ControlWndProc(var Message: TMessage);
    function MaxColIndex: Integer;
    procedure SetAutoWidth(Value: TAutoWidth);
    procedure SetAutoWidthOnResize(Value: Boolean);
    procedure SetDisplayBoolean(Value: Boolean);
    procedure SetDisplayImages(Value: Boolean);
    procedure SetDisplayMemo(Value: Boolean);
    procedure SetFullSizeMemo(Value: Boolean);
    procedure SetUseRowColors(Value: Boolean);
    procedure SetRowColor(Index: Integer; Value: TColor);
    procedure SetRowSizingAllowed(Value: Boolean);
    procedure SetStretchDraw(Value: Boolean); //1.3
    procedure LockHeight;
    procedure UnLockHeight;
    procedure CalcTitleHeight;
    function GetDefaultRowHeight: Integer;
    procedure SetDefaultRowHeight(Value: Integer);
    procedure PopUpHint(ClientX, ClientY: Integer);
    procedure SetCellHints(Value: Boolean);
    procedure SetShowTextEllipsis(Value: Boolean);
    procedure SetMultiLines(Value: Boolean);
    procedure SetSortOnTitleClick(Value: Boolean);
    procedure ShowPopUpMenu(X, Y: Integer);
    procedure SetOptionsMenu(Value: Boolean);
    procedure MenuAWDefault(Sender: TObject);
    procedure MenuAWProportional(Sender: TObject);
    procedure MenuAWUniform(Sender: TObject);
    procedure MenuAWWidestValue(Sender: TObject);
    procedure MenuAWWidestValueTitle(Sender: TObject);
    procedure MenuAWOnResize(Sender: TObject);
    procedure MenuAllowColResizing(Sender: TObject);
    procedure MenuAllowRowResizing(Sender: TObject);
    procedure MenuSetFixedCols(Sender: TObject);
    procedure MenuShowHideColumns(Sender: TObject);
    procedure MenuSortColumns(Sender: TObject);
    procedure MenuActivateFilter(Sender: TObject);
    procedure MenuAddFilter(Sender: TObject);
    procedure MenuDisplayBoolean(Sender: TObject);
    procedure MenuDisplayImages(Sender: TObject);
    procedure MenuDisplayMemo(Sender: TObject);
    procedure MenuFullSizeMemo(Sender: TObject);
    procedure MenuUseRowColors(Sender: TObject);
    procedure MenuSetCellHints(Sender: TObject);
    procedure MenuShowTextEllipsis(Sender: TObject);
    procedure MenuSetMultiLines(Sender: TObject);
    procedure MenuSetEnterAsTab(Sender: TObject);
    procedure MenuSetReadOnly(Sender: TObject);
    procedure MenuConfig(Sender: TObject);
    function PrivateSearch(var ResultCol, ResultRecord: Integer; Next: Boolean): Boolean;
    procedure SendFooterMsg(Msg: Integer);
  protected
    procedure Loaded; override;
    function CanEditShow: Boolean; override;
    procedure PlaceControl(Control: TWinControl; ACol, ARow: Integer); virtual;
    procedure GetCellProps(Field: TField; AFont: TFont; var Background: TColor;
      Highlight: Boolean); override;
    procedure DrawColumnCell(const Rect: TRect; DataCol: Integer; Column: TColumn;
      State: TGridDrawState); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CellClick(Column: TColumn); override;
    procedure DblClick; override;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MouseLeave;
    procedure Scroll(Distance: Integer); override;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure LinkActive(Value: Boolean); override;
    procedure LayoutChanged; override;
    procedure ColWidthsChanged; override;
    procedure RowHeightsChanged; override;
    procedure DoTitleClick(ACol: LongInt; AField: TField); override;
    procedure RedirectGetBtnParams(Sender: TObject; Field: TField; AFont: TFont;
      var Background: TColor; var SortMarker: TSortMarker; IsDown: Boolean);
    procedure TopLeftChanged; override;
  public
    FilterList: TFilterList;
    ConfigFileName: string;
    ConfigOptions: set of TConfigOptions;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    function Sort(FieldsToSort: TSortFields): Boolean;
    property SortedFields: TSortFields read FSortedFields;
    procedure ExportToFile(FileName: string; Format: TExportFormat; Titles: Boolean;
      Delimiter: Char);
    procedure SaveGridConfig(IniFileName: string; SaveLayout: Boolean = True;
      SaveSort: Boolean = True; SaveFilter: Boolean = True);
    procedure RestoreGridConfig(IniFileName: string; RestLayout: Boolean = True;
      RestSort: Boolean = True; RestFilter: Boolean = True);
    procedure SaveGridPosition;
    procedure RestoreGridPosition;
    function Search(ValueToSearch: Variant; const SearchFields: TStringList;
      var ResultCol, ResultRecord: Integer; Focus: Boolean): Boolean;
    function SearchNext(var ResultCol, ResultRecord: Integer; Focus: Boolean): Boolean;
    procedure MenuSetFilter(Sender: TObject);
    procedure MenuExportGrid(Sender: TObject);
    procedure MenuPrintGrid(Sender: TObject);
    procedure CloneLayout(CloneDBGrid: TDBGrid); //1.3
    function FillWithSelectedRecords(DestMemDS: TRxMemoryData; Append: Boolean): Boolean; //1.3
      { To be used only by UltimDBFooter }
    procedure RegisterFooter(Footer: TWinControl);
    procedure UnregisterFooter(Footer: TWinControl);
  published
      { NoFlickering: remove the flickering effect but slow down a lot the display}
    property NoFlickering: Boolean read FNoFlickering write SetNoFlickering default False;
      { Controls: list of controls used to edit data }
    property Controls: TUltimControls read FControls write SetControls;
      { OnMouse...: events fired by mouse use }
    property OnMouseDown read FOnMouseDown write FOnMouseDown;
    property OnMouseUp read FOnMouseUp write FOnMouseUp;
    property OnMouseMove read FOnMouseMove write FOnMouseMove;
      { AutoWidth: change automatically the columns width
        - awNone: no change
        - awDefault: each column is resized to its default width
        - awWidestValue: each column is resized according to its longest data
        - awWidestValueTitle: same as awWidestValue but the titles are also taken into account
        - awProportional: columns take all the visible part of the grid
        - awUniform: same as awProportional but all columns have the same width }
    property AutoWidth: TAutoWidth read FAutoWidth write SetAutoWidth default awNone;
      { AutoWidthOnResize: the columns width is recalculated after every resizing }
    property AutoWidthOnResize: Boolean read FAutoWidthOnResize write SetAutoWidthOnResize
      default False;
      { OnAutoWidthChange: event fired if the value of AutoWidth changes }
    property OnAutoWidthChange: TAutoWidthEvent read FOnAutoWidthChange write FOnAutoWidthChange;
      { DisplayBoolean: display a checkbox instead of True/False }
    property DisplayBoolean: Boolean read FDisplayBoolean write SetDisplayBoolean default True;
      { DisplayImages: display the picture instead of the tags (GRAPHIC), (BLOB) or a glyph }
    property DisplayImages: Boolean read FDisplayImages write SetDisplayImages default True;
      { DisplayMemo: display the memo content instead of the tag (MEMO) or a glyph }
    property DisplayMemo: Boolean read FDisplayMemo write SetDisplayMemo default True;
      { FullSizeMemo: display the full memo content if the screen is wide enough }
    property FullSizeMemo: Boolean read FFullSizeMemo write SetFullSizeMemo default False;
      { UseRowColors: color rows alternatively with colors RowColor1 and 2 }
    property UseRowColors: Boolean read FUseRowColors write SetUseRowColors default False;
    property RowColor1: TColor index 0 read FRowColors[0] write SetRowColor default clRowColor1;
    property RowColor2: TColor index 1 read FRowColors[1] write SetRowColor default clRowColor2;
      { RowSizingAllowed: allow the resizing of rows }
    property RowSizingAllowed: Boolean read FRowSizingAllowed write SetRowSizingAllowed
      default False;
      { DefaultRowHeight: default height of grid rows }
    property DefaultRowHeight: Integer read GetDefaultRowHeight write SetDefaultRowHeight;
      { CellHints: display the full content of a cell in a hint box when mouse is over }
    property CellHints: Boolean read FCellHints write SetCellHints default False;
      { ShowTextEllipsis: add '...' to the end of truncated texts }
    property ShowTextEllipsis: Boolean read FShowTextEllipsis write SetShowTextEllipsis
      default True;
      { MultiLines: display many lines per cell }
    property MultiLines: Boolean read FMultiLines write SetMultiLines default False;
      { SortOnTitleClick: sort a column when its title is clicked }
    property SortOnTitleClick: Boolean read FSortOnTitleClick write SetSortOnTitleClick
      default False;
      { EnterAsTab: the Return key acts as a Tab key }
    property EnterAsTab: Boolean read FEnterAsTab write FEnterAsTab default False;
      { OptionsMenu: add a context menu allowing to modify the grid options }
    property OptionsMenu: Boolean read FOptionsMenu write SetOptionsMenu default False;
      { OptionsMenuItems: item list of options menu }
    property OptionsMenuItems: TOptionsMenuItems read FOptionsMenuItems write FOptionsMenuItems
      default DefaultMenuItems;
      { OnMenuPrintGrid: event fired by a call to the function MenuPrintGrid }
    property OnMenuPrintGrid: TNotifyEvent read FOnMenuPrintGrid write FOnMenuPrintGrid;
      { ReportTitle: title of the report to print }
    property ReportTitle: string read FReportTitle write FReportTitle;
      { StretchDraw: the picture size is adjusted to fit the cell size }
    property StretchDraw: Boolean read FStretchDraw write SetStretchDraw default True; //1.3
  end;

implementation

uses UltimDBFooter;

{------------------------------------------- CONTROLS --------------------------------------------}

procedure TUltimControl.Assign(Source: TPersistent);
begin
  if Source is TUltimControl then
  begin
    ControlName := TUltimControl(Source).ControlName;
    FieldName := TUltimControl(Source).FieldName;
  end
  else
    inherited Assign(Source);
end;

constructor TUltimControls.Create(UltimGrid: TUltimDBGrid);
begin
  inherited Create(TUltimControl);
  FUltimGrid := UltimGrid;
end;

function TUltimControls.GetOwner: TPersistent;
begin
  Result := FUltimGrid;
end;

function TUltimControls.Add: TUltimControl;
begin
  Result := TUltimControl(inherited Add);
end;

function TUltimControls.GetItem(Index: Integer): TUltimControl;
begin
  Result := TUltimControl(inherited GetItem(Index));
end;

procedure TUltimControls.SetItem(Index: Integer; Value: TUltimControl);
begin
  inherited SetItem(Index, Value);
end;

function TUltimControls.ControlByField(FieldName: string): TUltimControl;
var
  Ctrl_Idx: Integer;
begin
  Result := nil;
  for Ctrl_Idx := 0 to Count - 1 do
    if SameText(Items[Ctrl_Idx].FieldName, FieldName) then
    begin
      Result := Items[Ctrl_Idx];
      Break;
    end;
end;

{-------------------------------------------- PUBLIC ---------------------------------------------}

constructor TUltimDBGrid.Create(AOwner: TComponent);
begin
  inherited;
  FBackBuffer := TBitmap.Create;
  FNoFlickering := False;
  FControls := TUltimControls.Create(Self);
  FCurrentControl := nil;
  FOldControlWndProc := nil;
  FCustomEditor := False;
  FBooleanEditor := False;
  FAutoWidth := awNone;
  FAutoWidthOnResize := False;
  FDisplayBoolean := True;
  FDisplayImages := True;
  FDisplayMemo := True;
  FFullSizeMemo := False;
  FUseRowColors := False;
  FRowColors[0] := clRowColor1;
  FRowColors[1] := clRowColor2;
  FRowSizingAllowed := False;
  FLockedHeight := 0;
  FTitleRowHeight := 0;
  FNewDefRowHeight := 0;
  FLockedWidth := False;
  FHintWnd := THintWindow.Create(Self);
  FHintWnd.Color := clInfoBk;
  FHintWnd.Font.Color := clInfoText;
  FCellHints := False;
  FShowTextEllipsis := True;
  FMultiLines := False;
  FSortOnTitleClick := False;
  FSortedFields := nil;
  FilterList := nil;
  FOldGetBtnParams := nil;
  FEnterAsTab := False;
  FGridPopUpMenu := TPopUpMenu.Create(Self);
  FOptionsMenu := False;
  FOptionsMenuItems := DefaultMenuItems;
  ConfigFileName := ExtractFilePath(ParamStr(0)) + DefaultIniName;
  ConfigOptions := [cfgLayout, cfgSortSettings, cfgFilters];
  FValueToSearch := Null;
  FSearchFields := nil;
  FOnMenuPrintGrid := nil;
  FReportTitle := '';
  FStretchDraw := True; //1.3
  FFooter := nil;
end;

destructor TUltimDBGrid.Destroy;
begin
  HideCurrentControl;
  FGridPopUpMenu.Free;
  FHintWnd.Free;
  FControls.Free;
  FBackBuffer.Free;
  inherited;
end;

procedure TUltimDBGrid.Invalidate;
begin
  if (FCurrentControl <> nil) and (DataLink <> nil) and (DataLink.Active) and
    not (DataLink.DataSet.State in [dsEdit, dsInsert]) then
    HideCurrentControl;
   // In some cases, [goRowSizing] is lost and have to be set again
  if FRowSizingAllowed and not (goRowSizing in TDrawGrid(Self).Options) then
    TDrawGrid(Self).Options := TDrawGrid(Self).Options + [goRowSizing];
  inherited;
end;

function TUltimDBGrid.Sort(FieldsToSort: TSortFields): Boolean;
var
  DSet: TDataSet;
  FTS, MaxFTS: Integer;
  SortString: string;
  SortField: TField;
  {$IFDEF BDE}
  IndexDefBDE: TIndexDefs;
  I: Integer;
  {$ENDIF}
begin
  Result := False;
  if (DataLink <> nil) and DataLink.Active then
  begin
    DSet := DataLink.DataSet;
    if DSet.State in [dsEdit, dsInsert] then
      // we have to leave the insert/edit mode before sorting
      if (dgCancelOnExit in Options) and (DSet.State = dsInsert) and not DSet.Modified then
        DSet.Cancel
      else
        try
          DSet.Post;
        except
          on E: Exception do
          begin
            Application.MessageBox(PChar(E.Message + '.'), lsError, MB_ICONSTOP);
            Exit;
          end;
        end;
    // Sorting of records
    Screen.Cursor := crHourGlass;
    DSet.DisableControls;
    try
      SortString := '';
      MaxFTS := Length(FieldsToSort) - 1;
      for FTS := 0 to MaxFTS do
      begin
        FieldsToSort[FTS].Name := Trim(FieldsToSort[FTS].Name);
        SortField := DSet.FieldByName(FieldsToSort[FTS].Name);
        if (SortField is TBlobField) or (SortField is TBytesField) or
          ((SortField.FieldKind <> fkData) and (SortField.FieldKind <> fkInternalCalc)) then
        begin
          Application.MessageBox(PChar(Format(lsNoBlobSort, [FieldsToSort[FTS].Name])),
            lsError, MB_ICONWARNING);
          Break;
        end
        else
        {$IFDEF ADO}
        if DSet is TCustomADODataSet then
        begin // ADO Table or Query
          if SortString <> '' then
            SortString := SortString + ',';
          if Pos(' ', FieldsToSort[FTS].Name) = 0 then
            SortString := SortString + FieldsToSort[FTS].Name
          else
            SortString := SortString + '[' + FieldsToSort[FTS].Name + ']';
          if FieldsToSort[FTS].Order = soSortDESC then
            SortString := SortString + ' DESC';
          if FTS = MaxFTS then
          begin
            TCustomADODataSet(DSet).Sort := SortString;
            FSortedFields := FieldsToSort;
            Result := True;
          end;
        end
        else
        {$ENDIF}
        {$IFDEF BDE}
        if DSet is TTable then
        begin // BDE Table
          if SortString <> '' then
            SortString := SortString + ';';
          SortString := SortString + FieldsToSort[FTS].Name;
          FieldsToSort[FTS].Order := soSortASC;
          if FTS = MaxFTS then
          begin
            IndexDefBDE := TTable(DSet).IndexDefs;
            IndexDefBDE.Update;
            for I := 0 to IndexDefBDE.Count - 1 do
              if SameText(SortString, IndexDefBDE.Items[I].Fields) then
              begin
                TTable(DSet).IndexFieldNames := IndexDefBDE.Items[I].Fields;
                FSortedFields := FieldsToSort;
                Result := True;
              end;
            if not Result then
            begin
              if MaxFTS = 0 then
                Application.MessageBox(PChar(Format(lsNoBDEIndex1, [SortString])), lsError,
                  MB_ICONWARNING)
              else
                Application.MessageBox(PChar(Format(lsNoBDEIndex2, [SortString])), lsError,
                  MB_ICONWARNING)
            end;
          end;
        end
        else
        {$ENDIF}
        if DSet is TRxMemoryData then
        begin // Memory Table
          if SortString <> '' then
            SortString := SortString + ';';
          SortString := SortString + FieldsToSort[FTS].Name;
          if FieldsToSort[FTS].Order <> FieldsToSort[0].Order then
            FieldsToSort[FTS].Order := FieldsToSort[0].Order;
          if FTS = MaxFTS then
          begin
            TRxMemoryData(DSet).SortOnFields(SortString, True, not FieldsToSort[0].Order);
            TRxMemoryData(DSet).First;
            FSortedFields := FieldsToSort;
            Result := True;
          end;
        end
        else
        begin // DataSet unknown
          Application.MessageBox(lsDSNotSupported, lsError, MB_ICONWARNING);
          Break;
        end;
      end;
    finally
      DSet.EnableControls;
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TUltimDBGrid.ExportToFile(FileName: string; Format: TExportFormat; Titles: Boolean;
  Delimiter: Char);

  function HTMLConvert(Chaine: string): string;
  begin
      { Special characters replacement }
    Result := StringReplace(Chaine, '&', '&amp;', [rfReplaceAll]);
    Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
    Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
    Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  end;

const
  SylkMarkX = '‡ X ‡';
  SylkMarkY = '‡ Y ‡';
var
  GridContent: TStringList;
  Delim,
    FieldText,
    GridLine: string;
  I, Cols, Rows: Integer;
begin
  if (DataLink <> nil) and DataLink.Active then
  begin
    GridContent := TStringList.Create;
    Screen.Cursor := crHourGlass;
    try
      DataLink.DataSet.DisableControls;
      try
            { Initializations }
        Cols := 0;
        Rows := 0;
        if Format = efHTML then
        begin
          GridContent.Add('<HTML><HEAD><TITLE>' + ExtractFileName(FileName) + ' ' +
            DateTimeToStr(Now) + '</TITLE></HEAD>');
          GridContent.Add('<BODY TEXT="Black" BGCOLOR="White"><TABLE BORDER="1">');
          Delim := '</TD><TD>';
        end
        else
        if Format = efSylk then
        begin
          GridContent.Add('ID;PUltimDBGrid');
          GridContent.Add('B;X' + SylkMarkX + ';Y' + SylkMarkY);
          Delim := #13#10;
        end
        else
          Delim := Delimiter;

        // Get the titles of visible columns
        if Titles then
        begin
          if Format = efHTML then
            GridLine := '<TR BGCOLOR="#CCCCCC"><TD>'
          else
            GridLine := '';
          for I := 0 to Columns.Count - 1 do
            if Columns[I].Visible then
            begin
              if Format = efHTML then
              begin
                if Columns[I].Title.Caption = '' then
                  GridLine := GridLine + '&nbsp;'
                else
                  GridLine := GridLine + HTMLConvert(Columns[I].Title.Caption);
              end
              else
              if Format = efSylk then
              begin
                Rows := 1;
                Inc(Cols);
                GridLine := GridLine + 'F;W' + IntToStr(Cols) + ' ' + IntToStr(Cols) +
                  ' ' + IntToStr(Columns[I].Field.DisplayWidth) + #13#10 +
                  'C;X' + IntToStr(Cols) + ';Y1;K"' + Columns[I].Title.Caption + '"';
              end
              else
                GridLine := GridLine + Columns[I].Title.Caption;
              if I < Columns.Count - 1 then
                GridLine := GridLine + Delim;
            end;
          if Format = efHTML then
          begin
            GridLine := GridLine + '</TD></TR>';
                  // Remove the last empty column (occurs when last column is invisible)
            GridLine := StringReplace(GridLine, '<TD></TD>', '', [rfIgnoreCase]);
          end;
          GridContent.Add(GridLine);
        end;

        // Get the content of the visible columns
        DataLink.DataSet.First;
        while not DataLink.DataSet.EOF do
        begin
          if Format = efHTML then
            GridLine := '<TR><TD>'
          else
          begin
            GridLine := '';
            if Format = efSylk then
            begin
              Inc(Rows);
              Cols := 0;
            end;
          end;
          for I := 0 to Columns.Count - 1 do
          begin
            FieldText := Columns[I].Field.DisplayText;
            if Columns[I].Visible then
            begin
              if Format = efHTML then
              begin
                if Columns[I].Field.DataType = ftMemo then
                  FieldText := Columns[I].Field.AsString;
                if FieldText = '' then
                  GridLine := GridLine + '&nbsp;'
                else
                  GridLine := GridLine + HTMLConvert(FieldText);
              end
              else
              if Format = efSylk then
              begin
                Inc(Cols);
                GridLine := GridLine + 'C;X' + IntToStr(Cols) + ';Y' + IntToStr(Rows) +
                  ';K"' + FieldText + '"';
              end
              else
                GridLine := GridLine + FieldText;
              if I < Columns.Count - 1 then
                GridLine := GridLine + Delim;
            end;
          end;
          if Format = efHTML then
          begin
            GridLine := GridLine + '</TD></TR>';
                  // Remove the last empty column (occurs when last column is invisible)
            GridLine := StringReplace(GridLine, '<TD></TD>', '', [rfIgnoreCase]);
          end;
          GridContent.Add(GridLine);
          DataLink.DataSet.Next;
        end;
        DataLink.DataSet.First;
        if Format = efHTML then
          GridContent.Add('</TABLE></BODY></HTML>')
        else
        if Format = efSylk then
        begin
               // Update the boundaries definition
          GridContent.Strings[1] := StringReplace(GridContent.Strings[1], SylkMarkX,
            IntToStr(Cols), []);
          GridContent.Strings[1] := StringReplace(GridContent.Strings[1], SylkMarkY,
            IntToStr(Rows), []);
          GridContent.Add('E');
        end;

        // Save it to a file
        GridContent.SaveToFile(FileName);
      finally
        DataLink.DataSet.EnableControls;
      end;
    finally
      Screen.Cursor := crDefault;
      GridContent.Free;
    end;
  end;
end;

procedure TUltimDBGrid.SaveGridConfig;
var
  IniFile: TIniFile;
  ID: string;

  procedure WriteLayout(Key, Value: string);
  begin
    IniWriteString(IniFile, 'Layout', ID + Key, Value);
  end;

  procedure WriteLayoutBool(Key: string; Value: Boolean);
  begin
    if Value then
      IniWriteString(IniFile, 'Layout', ID + Key, 'Yes')
    else
      IniWriteString(IniFile, 'Layout', ID + Key, 'No');
  end;

  procedure WriteSort(Key, Value: string);
  begin
    IniWriteString(IniFile, 'Sort', ID + Key, Value);
  end;

  procedure WriteFilter(Key, Value: string);
  begin
    IniWriteString(IniFile, 'Filter', ID + Key, Value);
  end;

var
  I: Integer;
begin
  DeleteFile(IniFileName);
  IniFile := TIniFile.Create(IniFileName);
  Enabled := False;
  Screen.Cursor := crHourGlass;
  try
    if SaveLayout then
    begin
      // Saving the properties of the grid
      ID := 'Grid.';
      WriteLayout('Color', IntToStr(Color));
      WriteLayout('FixedColor', IntToStr(FixedColor));
      WriteLayout('FixedCols', IntToStr(FixedCols));
      WriteLayoutBool('UseRowColors', FUseRowColors);
      WriteLayout('RowColor1', IntToStr(FRowColors[0]));
      WriteLayout('RowColor2', IntToStr(FRowColors[1]));
      WriteLayout('FontName', Font.Name);
      WriteLayout('FontSize', IntToStr(Font.Size));
      WriteLayout('FontColor', IntToStr(Font.Color));
      WriteLayoutBool('FontBold', fsBold in Font.Style); //1.3
      WriteLayoutBool('FontItalic', fsItalic in Font.Style); //1.3
      WriteLayoutBool('FontUnderline', fsUnderline in Font.Style); //1.3
      WriteLayout('TitleFontName', TitleFont.Name);
      WriteLayout('TitleFontSize', IntToStr(TitleFont.Size));
      WriteLayout('TitleFontColor', IntToStr(TitleFont.Color));
      WriteLayoutBool('TitleFontBold', fsBold in TitleFont.Style); //1.3
      WriteLayoutBool('TitleFontItalic', fsItalic in TitleFont.Style); //1.3
      WriteLayoutBool('TitleFontUnderline', fsUnderline in TitleFont.Style); //1.3
      WriteLayoutBool('ParentColor', ParentColor);
      WriteLayoutBool('ParentFont', ParentFont);
      WriteLayout('DefaultRowHeight', IntToStr(DefaultRowHeight));
      WriteLayoutBool('dgIndicator', dgIndicator in Options);
      WriteLayoutBool('dgTitles', dgTitles in Options);
      WriteLayoutBool('dgColLines', dgColLines in Options);
      WriteLayoutBool('dgRowLines', dgRowLines in Options);
      WriteLayoutBool('ShowGlyphs', ShowGlyphs);
      WriteLayoutBool('DisplayBoolean', FDisplayBoolean);
      WriteLayoutBool('DisplayImages', FDisplayImages);
      WriteLayoutBool('DisplayMemo', FDisplayMemo);
      WriteLayoutBool('FullSizeMemo', FFullSizeMemo);
      WriteLayoutBool('ShowTextEllipsis', FShowTextEllipsis);
      WriteLayoutBool('MultiLines', FMultiLines);
      WriteLayoutBool('StretchDraw', FStretchDraw); //1.3
      with Columns do
      begin
        // Save the number of columnts
        WriteLayout('ColCount', IntToStr(Count));

        // Save the properties of columns
        for I := 0 to Count - 1 do
        begin
          ID := 'Col[' + IntToStr(I) + '].';
          WriteLayout('FieldName', Items[I].FieldName);
          if cvAlignment in Items[I].AssignedValues then //1.3
            WriteLayout('Alignment', IntToStr(Ord(Items[I].Alignment)));
          if cvColor in Items[I].AssignedValues then //1.3
            WriteLayout('Color', IntToStr(Items[I].Color));
          if cvFont in Items[I].AssignedValues then //1.3
            with Items[I].Font do
            begin
              WriteLayout('FontName', Name);
              WriteLayout('FontSize', IntToStr(Size));
              WriteLayout('FontColor', IntToStr(Color));
              WriteLayoutBool('FontBold', fsBold in Style);
              WriteLayoutBool('FontItalic', fsItalic in Style);
              WriteLayoutBool('FontUnderline', fsUnderline in Style);
            end;
          if cvTitleCaption in Items[I].AssignedValues then //1.3
            WriteLayout('Title', Items[I].Title.Caption);
          if cvTitleAlignment in Items[I].AssignedValues then //1.3
            WriteLayout('TitleAlignment', IntToStr(Ord(Items[I].Title.Alignment)));
          if cvTitleColor in Items[I].AssignedValues then //1.3
            WriteLayout('TitleColor', IntToStr(Items[I].Title.Color));
          if cvTitleFont in Items[I].AssignedValues then //1.3
            with Items[I].Title.Font do
            begin
              WriteLayout('TitleFontName', Name);
              WriteLayout('TitleFontSize', IntToStr(Size));
              WriteLayout('TitleFontColor', IntToStr(Color));
              WriteLayoutBool('TitleFontBold', fsBold in Style);
              WriteLayoutBool('TitleFontItalic', fsItalic in Style);
              WriteLayoutBool('TitleFontUnderline', fsUnderline in Style);
            end;
          if cvWidth in Items[I].AssignedValues then //1.3
            WriteLayout('Width', IntToStr(Items[I].Width));
          if cvReadOnly in Items[I].AssignedValues then //1.3
            WriteLayoutBool('ReadOnly', Items[I].ReadOnly);
          WriteLayoutBool('Visible', Items[I].Visible);
        end;
      end;

      // Save the type of automatic sizing
      IniWriteString(IniFile, 'Layout', 'Grid.AutoWidth', IntToStr(Ord(FAutoWidth)));
    end;
    if SaveSort then
    begin
      // Save sorting criterias
      ID := 'Sort.';
      WriteSort('Count', IntToStr(Length(FSortedFields)));
      for I := 0 to Length(FSortedFields) - 1 do
      begin
        ID := 'SF[' + IntToStr(I) + '].';
        WriteSort('Name', FSortedFields[I].Name);
        if FSortedFields[I].Order then
          WriteSort('Order', 'A')
        else
          WriteSort('Order', 'D');
      end;
    end;
    if SaveFilter then
    begin
      // Save the current filter and the list of filters
      ID := 'Filter.';
      if DataLink.DataSet <> nil then
        WriteFilter('Current', DataLink.DataSet.Filter);
      WriteFilter('Count', IntToStr(Length(FilterList)));
      for I := 0 to Length(FilterList) - 1 do
      begin
        ID := 'FL[' + IntToStr(I) + '].';
        WriteFilter('Name', FilterList[I].Name);
        WriteFilter('Filter', FilterList[I].Filter);
      end;
    end;
  finally
    Screen.Cursor := crDefault;
    Enabled := True;
    IniFile.Free;
  end;
end;

procedure TUltimDBGrid.RestoreGridConfig;
var
  IniFile: TIniFile;
  ID: string;
  ReOpen: Boolean;
  I, NbCol, NbItems: Integer;
  ColStr: string; //1.3

  function ReadLayoutStr(Key, Default: string): string;
  begin
    Result := IniReadString(IniFile, 'Layout', ID + Key, Default);
  end;

  function ReadLayoutInt(Key: string; Default: Int64): Int64;
  begin
    Result := IniReadInteger(IniFile, 'Layout', ID + Key, Default);
  end;

  function ReadSortStr(Key, Default: string): string;
  begin
    Result := IniReadString(IniFile, 'Sort', ID + Key, Default);
  end;

  function ReadFilterStr(Key, Default: string): string;
  begin
    Result := IniReadString(IniFile, 'Filter', ID + Key, Default);
  end;

begin
  IniFile := TIniFile.Create(IniFileName);
  Enabled := False;
  Screen.Cursor := crHourGlass;
  try
    // closing the dataset
    if (DataLink.DataSet <> nil) and DataLink.Active then
    begin
      DataLink.DataSet.Close;
      ReOpen := True;
    end
    else
      ReOpen := False;
    if RestLayout and IniFile.SectionExists('Layout') then
    begin
      // Delete existing columngs
      SetAutoWidthOnResize(False);
      SetAutoWidth(awNone);
      Columns.Clear;

      // Reloading the properties of the grid
      ID := 'Grid.';
      Color := TColor(ReadLayoutInt('Color', clWindow));
      FixedColor := TColor(ReadLayoutInt('FixedColor', clBtnFace));
      FixedCols := ReadLayoutInt('FixedCols', 0);
      FUseRowColors := (ReadLayoutStr('UseRowColors', 'No') = 'Yes');
      FRowColors[0] := ReadLayoutInt('RowColor1', clRowColor1);
      FRowColors[1] := ReadLayoutInt('RowColor2', clRowColor2);
      Font.Name := ReadLayoutStr('FontName', 'MS Sans Serif');
      Font.Size := ReadLayoutInt('FontSize', 8);
      Font.Color := ReadLayoutInt('FontColor', clWindowText);
      Font.Style := []; //1.3
      if ReadLayoutStr('FontBold', 'No') = 'Yes' then //1.3
        Font.Style := Font.Style + [fsBold];
      if ReadLayoutStr('FontItalic', 'No') = 'Yes' then //1.3
        Font.Style := Font.Style + [fsItalic];
      if ReadLayoutStr('FontUnderline', 'No') = 'Yes' then //1.3
        Font.Style := Font.Style + [fsUnderline];
      TitleFont.Name := ReadLayoutStr('TitleFontName', 'MS Sans Serif');
      TitleFont.Size := ReadLayoutInt('TitleFontSize', 8);
      TitleFont.Color := ReadLayoutInt('TitleFontColor', clWindowText);
      TitleFont.Style := []; //1.3
      if ReadLayoutStr('TitleFontBold', 'No') = 'Yes' then //1.3
        TitleFont.Style := TitleFont.Style + [fsBold];
      if ReadLayoutStr('TitleFontItalic', 'No') = 'Yes' then //1.3
        TitleFont.Style := TitleFont.Style + [fsItalic];
      if ReadLayoutStr('TitleFontUnderline', 'No') = 'Yes' then //1.3
        TitleFont.Style := TitleFont.Style + [fsUnderline];
      ParentColor := (ReadLayoutStr('ParentColor', 'No') = 'Yes');
      ParentFont := (ReadLayoutStr('ParentFont', 'Yes') = 'Yes');
      DefaultRowHeight := ReadLayoutInt('DefaultRowHeight', 17);
      if ReadLayoutStr('dgIndicator', 'Yes') = 'Yes' then
        Options := Options + [dgIndicator]
      else
        Options := Options - [dgIndicator];
      if ReadLayoutStr('dgTitles', 'Yes') = 'Yes' then
        Options := Options + [dgTitles]
      else
        Options := Options - [dgTitles];
      if ReadLayoutStr('dgColLines', 'Yes') = 'Yes' then
        Options := Options + [dgColLines]
      else
        Options := Options - [dgColLines];
      if ReadLayoutStr('dgRowLines', 'Yes') = 'Yes' then
        Options := Options + [dgRowLines]
      else
        Options := Options - [dgRowLines];
      ShowGlyphs := (ReadLayoutStr('ShowGlyphs', 'Yes') = 'Yes');
      FDisplayBoolean := (ReadLayoutStr('DisplayBoolean', 'Yes') = 'Yes');
      FDisplayImages := (ReadLayoutStr('DisplayImages', 'Yes') = 'Yes');
      FDisplayMemo := (ReadLayoutStr('DisplayMemo', 'Yes') = 'Yes');
      FFullSizeMemo := (ReadLayoutStr('FullSizeMemo', 'No') = 'Yes');
      FShowTextEllipsis := (ReadLayoutStr('ShowTextEllipsis', 'Yes') = 'Yes');
      FMultiLines := (ReadLayoutStr('MultiLines', 'No') = 'Yes');
      FStretchDraw := (ReadLayoutStr('StretchDraw', 'Yes') = 'Yes'); //1.3
      
      // Read the number of columns to be created
      NbCol := ReadLayoutInt('ColCount', 0);
      if NbCol > 0 then
      begin
        // Rebuilding the columns
        with Columns do
        begin
          for I := 0 to NbCol - 1 do
          begin
            Columns.Add;
            ID := 'Col[' + IntToStr(I) + '].';
            Items[I].FieldName := ReadLayoutStr('FieldName', '');
            ColStr := ReadLayoutStr('Alignment', ''); //1.3
            if ColStr <> '' then //1.3
              Items[I].Alignment := TAlignment(StrToIntDef(ColStr, Ord(taLeftJustify)));
            ColStr := ReadLayoutStr('Color', ''); //1.3
            if ColStr <> '' then //1.3
              Items[I].Color := TColor(StrToIntDef(ColStr, clWindow));
            ColStr := ReadLayoutStr('FontName', ''); //1.3
            if ColStr <> '' then //1.3
              with Items[I].Font do
              begin
                Name := ColStr;
                Size := ReadLayoutInt('FontSize', 8);
                Color := TColor(ReadLayoutInt('FontColor', clWindowText));
                Style := [];
                if ReadLayoutStr('FontBold', 'No') = 'Yes' then
                  Style := Style + [fsBold];
                if ReadLayoutStr('FontItalic', 'No') = 'Yes' then
                  Style := Style + [fsItalic];
                if ReadLayoutStr('FontUnderline', 'No') = 'Yes' then
                  Style := Style + [fsUnderline];
              end;
            ColStr := ReadLayoutStr('Title', ''); //1.3
            if ColStr <> '' then //1.3
              Items[I].Title.Caption := ColStr;
            ColStr := ReadLayoutStr('TitleAlignment', ''); //1.3
            if ColStr <> '' then //1.3
              Items[I].Title.Alignment := TAlignment(StrToIntDef(ColStr, Ord(taLeftJustify)));
            ColStr := ReadLayoutStr('TitleColor', ''); //1.3
            if ColStr <> '' then //1.3
              Items[I].Title.Color := TColor(StrToIntDef(ColStr, clBtnFace));
            ColStr := ReadLayoutStr('TitleFontName', ''); //1.3
            if ColStr <> '' then //1.3
              with Items[I].Title.Font do
              begin
                Name := ColStr;
                Size := ReadLayoutInt('TitleFontSize', 8);
                Color := TColor(ReadLayoutInt('TitleFontColor', clWindowText));
                Style := [];
                if ReadLayoutStr('TitleFontBold', 'No') = 'Yes' then
                  Style := Style + [fsBold];
                if ReadLayoutStr('TitleFontItalic', 'No') = 'Yes' then
                  Style := Style + [fsItalic];
                if ReadLayoutStr('TitleFontUnderline', 'No') = 'Yes' then
                  Style := Style + [fsUnderline];
              end;
            ColStr := ReadLayoutStr('Width', ''); //1.3
            if ColStr <> '' then //1.3
              Items[I].Width := StrToIntDef(ColStr, 64);
            ColStr := ReadLayoutStr('ReadOnly', ''); //1.3
            if ColStr <> '' then //1.3
              Items[I].ReadOnly := (ColStr = 'Yes');
            Items[I].Visible := (ReadLayoutStr('Visible', 'Yes') = 'Yes');
          end;
        end;
      end;
      // Reloading the type of autosizing
      SetAutoWidth(TAutoWidth(IniReadInteger(IniFile, 'Layout', 'Grid.AutoWidth', Ord(awNone))));
    end;

    // Reopen the dataset
    if ReOpen then
      DataLink.DataSet.Open;

    // Reload the sorting criterias
    if RestSort and IniFile.SectionExists('Sort') then
    begin
      FSortedFields := nil;
      NbItems := IniReadInteger(IniFile, 'Sort', 'Sort.Count', 0);
      if NbItems > 0 then
      begin
        SetLength(FSortedFields, NbItems);
        for I := 0 to NbItems - 1 do
        begin
          ID := 'SF[' + IntToStr(I) + '].';
          FSortedFields[I].Name := ReadSortStr('Name', '');
          if ReadSortStr('Order', 'A') = 'A' then
            FSortedFields[I].Order := soSortASC
          else
            FSortedFields[I].Order := soSortDESC;
        end;
        Sort(FSortedFields);
      end;
    end;

    // Reloading the filter to be applied and the list of filters
    if RestFilter and IniFile.SectionExists('Filter') then
    begin
      if DataLink.DataSet <> nil then
      begin
        DataLink.DataSet.Filter := IniReadString(IniFile, 'Filter', 'Filter.Current', '');
        DataLink.DataSet.Filtered := (DataLink.DataSet.Filter <> '');
      end;
      FilterList := nil;
      NbItems := IniReadInteger(IniFile, 'Filter', 'Filter.Count', 0);
      if NbItems > 0 then
      begin
        SetLength(FilterList, NbItems);
        for I := 0 to NbItems - 1 do
        begin
          ID := 'FL[' + IntToStr(I) + '].';
          FilterList[I].Name := ReadFilterStr('Name', '');
          FilterList[I].Filter := ReadFilterStr('Filter', '');
        end;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
    Enabled := True;
    IniFile.Free;
  end;
end;

procedure TUltimDBGrid.SaveGridPosition;
begin
  FSavedBookmark := DataLink.DataSet.Bookmark;
  FSavedRowPos := DataLink.ActiveRecord;
end;

procedure TUltimDBGrid.RestoreGridPosition;
var
  Bookmark: TBookmark;
begin
   // Reset the current row (this also moves the record pointer) then goto the original record
  {$IFDEF ADO}
  if DataLink.DataSet is TCustomADODataSet then
  begin
    DataLink.ActiveRecord := FSavedRowPos;
    BookMark := Pointer(FSavedBookmark);
    if DataLink.DataSet.BookmarkValid(Bookmark) then
      TCustomADODataSet(DataLink.DataSet).Recordset.Bookmark := POleVariant(Bookmark)^;
  end
  else
  {$ENDIF}
  {$IFDEF BDE}
  if DataLink.DataSet is TBDEDataSet then
  begin
    DataLink.ActiveRecord := FSavedRowPos;
    BookMark := Pointer(FSavedBookmark);
    if DataLink.DataSet.BookmarkValid(Bookmark) then
      Check(DbiSetToBookmark(TBDEDataSet(DataLink.DataSet).Handle, Bookmark));
  end
  else
  {$ENDIF}
  begin
    Application.MessageBox(lsDSNotSupportedByFooter, lsError, MB_ICONSTOP);
    Exit;
  end;
   // Force a reread of the record buffer with the current settings
  try
    DataLink.DataSet.Resync([rmExact]);
  except
  end;
end;

function TUltimDBGrid.PrivateSearch(var ResultCol, ResultRecord: Integer; Next: Boolean): Boolean;
const
  GUpcaseLUT: Pointer = nil;
var
  GUpcaseTable: array [0..255] of Char;
  I, ColNo, Start: Integer;

  { This is a tuned version of FastPosNoCase/FastMemPosNC from FastStrings of Peter Morris }
  function FindText(const AFindString, ASourceString: string): Boolean;
  // (rom) changed to var
  var
    FindLen: Integer;
    SourceLen: Integer;

    function FastMemPosNC(const aSource, aFind): Pointer;
    asm
         push ESI
         push EDI
         push EBX
         mov  ESI, aFind
         mov  EDI, aSource
         mov  ECX, SourceLen
         mov  Result, 0
         cmp  ECX, FindLen
         jl   @TheEnd
         cmp  FindLen, 1
         jl   @TheEnd
         sub  ECX, FindLen
         inc  ECX
         mov  EDX, GUpcaseLUT
         xor  EBX, EBX
         jmp  @FindFirst
      @FindNext:
         inc  EDI
         dec  ECX
         jz   @NotFound
      @FindFirst:
         mov  Bl, [ESI]
         mov  AL, [EDX+EBX]
      @ScaSB:
         mov  Bl, [EDI]
         cmp  Al, [EDX+EBX]
         jz   @CompareStrings
         inc  EDI
         dec  ECX
         jnz  @ScaSB
         jmp  @NotFound
      @CompareStrings:
         push ECX
         mov  ECX, FindLen
      @CompareNext:
         dec  ECX
         jz   @FullMatch
         mov  Bl, [ESI+ECX]
         mov  Al, [EDX+EBX]
         mov  Bl, [EDI+ECX]
         cmp  Al, [EDX+EBX]
         jz   @KeepChecking
         POP  ECX
         jmp  @FindNext
      @KeepChecking:
         Jmp  @CompareNext
      @FullMatch:
         pop  ECX
         mov  Result, EDI
         jmp  @TheEnd
      @NotFound:
         mov  Result, 0
      @TheEnd:
         pop  EBX
         pop  EDI
         pop  ESI
    end;

  begin
    FindLen := Length(AFindString);
    SourceLen := Length(ASourceString);
    Result := (Integer(FastMemPosNC(ASourceString[1], AFindString[1])) > 0);
  end;

begin
  Result := False;
  if (DataLink <> nil) and DataLink.Active then
  begin
    DataLink.DataSet.DisableControls;
    Screen.Cursor := crHourGlass;
    try
      // Creation of the table of Upper Case characters
      // (obones) Not sure this is working with non European character sets...
      for I := 0 to 255 do
        GUpcaseTable[I] := Chr(I);
      CharUpperBuff(@GUpcaseTable[0], 256);
      GUpcaseLUT := @GUpcaseTable[0];

      // Set position in dataset
      SaveGridPosition;
      if Next then
      begin
        Start := Col;
        if not (dgIndicator in Options) then
          Inc(Start);
      end
      else
      begin
        Start := 0;
        DataLink.DataSet.First;
      end;

      // Iterating over the dataset
      while not DataLink.DataSet.EOF do
      begin
        for ColNo := Start to Columns.Count - 1 do
          for I := 0 to FSearchFields.Count - 1 do
          begin
            // Is the field in the list ?
            if SameText(FSearchFields[I], Columns[ColNo].FieldName) and
              (Columns[ColNo].Field.Value <> Null) then
            begin
              // Lookup for the text in the field
              if FindText(string(FValueToSearch), Columns[ColNo].Field.AsString) then
              begin
                ResultCol := ColNo;
                if dgIndicator in Options then
                  Inc(ResultCol);
                ResultRecord := DataLink.ActiveRecord;
                Result := True;
                Exit;
              end;
            end;
          end;
        Start := 0;
        DataLink.DataSet.Next;
      end;
    finally
      Screen.Cursor := crDefault;
      DataLink.DataSet.EnableControls;
    end;
  end;
end;

function TUltimDBGrid.Search(ValueToSearch: Variant; const SearchFields: TStringList;
  var ResultCol, ResultRecord: Integer; Focus: Boolean): Boolean;
begin
  Result := False;
  if (SearchFields <> nil) and (ValueToSearch <> Null) and (ValueToSearch <> '') then
  begin
    FValueToSearch := ValueToSearch;
    FSearchFields := SearchFields;
    Result := PrivateSearch(ResultCol, ResultRecord, False);
    if Result and Focus then
    begin
      Self.Col := ResultCol;
      if Visible then
        SetFocus;
    end
    else
      RestoreGridPosition;
  end;
end;

function TUltimDBGrid.SearchNext(var ResultCol, ResultRecord: Integer; Focus: Boolean): Boolean;
begin
  Result := False;
  if (FSearchFields <> nil) and (FValueToSearch <> Null) and (FValueToSearch <> '') then
  begin
    Result := PrivateSearch(ResultCol, ResultRecord, True);
    if Result and Focus then
    begin
      Self.Col := ResultCol;
      if Visible then
        SetFocus;
    end
    else
      RestoreGridPosition;
  end;
end;

procedure TUltimDBGrid.CloneLayout; //1.3
begin
  // Copy the layout of the grid
  if CloneDBGrid <> nil then
  begin
    CloneDBGrid.Columns.Assign(Self.Columns);
    CloneDBGrid.Color := Self.Color;
    CloneDBGrid.FixedColor := Self.FixedColor;
    CloneDBGrid.Options := Self.Options;
    CloneDBGrid.Font.Assign(Self.Font);
    CloneDBGrid.TitleFont.Assign(Self.TitleFont);
    CloneDBGrid.ParentFont := Self.ParentFont;
    if CloneDBGrid is TUltimDBGrid then
    begin
      TUltimDBGrid(CloneDBGrid).CellHints := Self.FCellHints;
      TUltimDBGrid(CloneDBGrid).DefaultRowHeight := Self.DefaultRowHeight;
      TUltimDBGrid(CloneDBGrid).DisplayMemo := Self.FDisplayMemo;
      TUltimDBGrid(CloneDBGrid).DisplayBoolean := Self.FDisplayBoolean;
      TUltimDBGrid(CloneDBGrid).DisplayImages := Self.FDisplayImages;
      TUltimDBGrid(CloneDBGrid).FixedCols := Self.FixedCols;
      TUltimDBGrid(CloneDBGrid).FullSizeMemo := Self.FFullSizeMemo;
      TUltimDBGrid(CloneDBGrid).MultiLines := Self.FMultiLines;
      TUltimDBGrid(CloneDBGrid).MultiSelect := Self.MultiSelect;
      TUltimDBGrid(CloneDBGrid).RowSizingAllowed := Self.FRowSizingAllowed;
      TUltimDBGrid(CloneDBGrid).ShowGlyphs := Self.ShowGlyphs;
      TUltimDBGrid(CloneDBGrid).ShowTextEllipsis := Self.FShowTextEllipsis;
      TUltimDBGrid(CloneDBGrid).StretchDraw := Self.FStretchDraw;
    end;
  end;
end;

function TUltimDBGrid.FillWithSelectedRecords; //1.3
var
  DSet: TDataSet;
  I: Integer;
  Fld: TField;
begin
  { At least 1 row selected ? }
  if SelectedRows.Count = 0 then
  begin
    Application.MessageBox(lsNoSelection, lsError, MB_ICONWARNING);
    Result := False;
    Exit;
  end;
  { Data set ready to be used ? }
  if (DataLink = nil) or (not DataLink.Active) or (DataLink.DataSet.State <> dsBrowse) then
  begin
    Application.MessageBox(lsNoSource, lsError, MB_ICONSTOP);
    Result := False;
    Exit;
  end;
  Screen.Cursor := crHourGlass;
  DSet := DataLink.DataSet;
  DSet.DisableControls;
  try
    try
      { Create or append fields ? }
      if Append then
      begin
        if not DestMemDS.Active then
        begin
          Application.MessageBox(lsMemDSClosed, lsError, MB_ICONSTOP);
          Result := False;
          Exit;
        end;
      end
      else
      begin
        if DestMemDS.Active then
          DestMemDS.Close;
        DestMemDS.CopyStructure(DSet);
        DestMemDS.Open;
      end;
      { Records copy }
      SaveGridPosition;
      DSet.First;
      while not DSet.Eof do
      begin
        if SelectedRows.CurrentRowSelected then
        begin
          DestMemDS.Append;
          for I := 0 to DSet.FieldCount - 1 do
          begin
            Fld := DestMemDS.FindField(DSet.Fields[I].FieldName);
            if Fld <> nil then
              Fld.Value := DSet.Fields[I].Value;
          end;
          DestMemDS.Post;
        end;
        DSet.Next;
      end;
      RestoreGridPosition;
    except
      on E: Exception do
      begin
        Application.MessageBox(PChar(E.Message), lsError, MB_ICONSTOP);
        Result := False;
        Exit;
      end;
    end;
  finally
    DSet.EnableControls;
    Screen.Cursor := crDefault;
  end;
  Result := True;
end;

procedure TUltimDBGrid.Loaded;
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
  // Redirection of the OnGetBtnParams event
  // (obones) There should be a cleaner way to do this
  if not (csDesigning in ComponentState) then
  begin
    FOldGetBtnParams := OnGetBtnParams;
    OnGetBtnParams := RedirectGetBtnParams;
  end;
end;

function TUltimDBGrid.CanEditField: Boolean;
var
  F: TField;
  Control: TUltimControl;

  function IsActiveControl: Boolean;
  var
    H: HWND;
    ParentForm: TCustomForm;
  begin
    Result := False;
    ParentForm := GetParentForm(Self);
    if Assigned(ParentForm) then
    begin
      if ParentForm.ActiveControl = Self then
        Result := True;
    end
    else
    begin
      H := GetFocus;
      while IsWindow(H) and not Result do
        if H = WindowHandle then
          Result := True
        else
          H := GetParent(H);
    end;
  end;

begin
  FCustomEditor := False;
  FBooleanEditor := False;

  Result := (dgEditing in Options) and not (csDesigning in ComponentState) and
    HandleAllocated and (LayoutLock = 0) and ((dgAlwaysShowEditor in Options) or IsActiveControl);

  if not Result then
    Exit;

  Result := (DataLink <> nil) and DataLink.Active and (SelectedIndex >= 0) and
    (SelectedIndex < Columns.Count); //1.1

  if not Result then
    Exit;

  F := SelectedField; //1.1
  Result := (F <> nil);

  if not Result then
    Exit;

  Control := FControls.ControlByField(F.FieldName);
  Result := (Control <> nil);
  if not Result then
    Result := not (F.DataType in [ftUnknown, ftBytes, ftVarBytes, ftBlob,
      ftMemo, ftFmtMemo, ftGraphic, ftTypedBinary, ftParadoxOle, ftDBaseOle,
        ftCursor, ftReference, ftDataSet, ftOraClob, ftOraBlob]);
  Result := Result and F.CanModify and not Columns[SelectedIndex].ReadOnly; //1.1

  if not Result then
    Exit;

  if Assigned(OnShowEditor) then
  begin
    OnShowEditor(Self, F, Result);
    if not Result then
      Exit;
  end;

  // Any customized edit controls to use ?
  if Control <> nil then
  begin
    PlaceControl(TWinControl(Owner.FindComponent(Control.ControlName)), Col, Row);
    FCustomEditor := True;
    Result := False;
  end
  else
  if (F.DataType = ftBoolean) and FDisplayBoolean then
  begin
    FBooleanEditor := True;
    Result := False;
  end;
end;

function TUltimDBGrid.CanEditShow: Boolean;
begin
  // To bypass TRxDBGrid.CanEditShow, there's no inheritance
  Result := EditorMode and CanEditField;
end;

procedure TUltimDBGrid.PlaceControl(Control: TWinControl; ACol, ARow: Integer);
var
  R: TRect;
  FrmParent: TCustomForm;
  OldOptions: TDBGridOptions;
begin
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
  Control.BringToFront;
  Control.Show;
  Control.BoundsRect := R;

  FrmParent := GetParentForm(Self);
  if dgCancelOnExit in Options then
  begin
    OldOptions := Options;
    Options := Options - [dgCancelOnExit];
    if Self.Visible and Control.Visible and Self.Parent.Visible and
      FrmParent.Visible then
      Control.SetFocus;
    Options := Options + [dgCancelOnExit];
    Options := OldOptions;
  end
  else
  if Self.Visible and Control.Visible and Self.Parent.Visible and
    FrmParent.Visible then
    Control.SetFocus;
end;

procedure TUltimDBGrid.GetCellProps(Field: TField; AFont: TFont;
  var Background: TColor; Highlight: Boolean);

  function IsFixedCol: Boolean;
  var
    FC: Integer;
  begin
    Result := False;
    for FC := 0 to FixedCols - 1 do
      if Columns[FC].FieldName = Field.FieldName then
      begin
        Result := True;
        Break;
      end;
  end;

begin
  if IsFixedCol then
    Background := FixedColor
  else
  if FUseRowColors and not Highlight then
    Background := FRowColors[DataLink.ActiveRecord mod 2];
  inherited GetCellProps(Field, AFont, Background, Highlight);
end;

procedure TUltimDBGrid.DrawColumnCell(const Rect: TRect; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
var
  TmpRect: TRect;
  CellSelected: Boolean;
  Image: TBitmap;
  BlobStream: TStringStream; //1.3
  Index: Integer;
  BackBufTmpRect: TRect;
  //MemStream: TMemoryStream; //1.3
  DrawOptions: Integer;

  function BackBufferRect: TRect;
  begin
    Result := Classes.Rect(0, 0, FBackBuffer.Width, FBackBuffer.Height);
  end;

  function BackBufferTmpRect: TRect;
  var
    XOffset, YOffset: Integer;
  begin
    XOffset := TmpRect.Left - Rect.Left;
    YOffset := TmpRect.Top - Rect.Top;
    Result := Classes.Rect(XOffset, YOffset, XOffset + TmpRect.Right - TmpRect.Left,
      YOffset + TmpRect.Bottom - TmpRect.Top);
  end;

  procedure CenterRect(ObjWidth, ObjHeight: Integer);
  begin
    TmpRect.Left := TmpRect.Left + ((TmpRect.Right - TmpRect.Left - ObjWidth) shr 1);
    if TmpRect.Left < Rect.Left then
      TmpRect.Left := Rect.Left;
    TmpRect.Right := TmpRect.Left + ObjWidth;
    TmpRect.Top := TmpRect.Top + ((TmpRect.Bottom - TmpRect.Top - ObjHeight) shr 1);
    if TmpRect.Top < Rect.Top then
      TmpRect.Top := Rect.Top;
    TmpRect.Bottom := TmpRect.Top + ObjHeight;
  end;

  procedure FillCellBkgrd;
  var
    NewBackgrnd: TColor;
  begin
    NewBackgrnd := Self.Canvas.Brush.Color;
    GetCellProps(Column.Field, Self.Canvas.Font, NewBackgrnd, CellSelected);
    Self.Canvas.Brush.Color := NewBackgrnd;
    if FNoFlickering then
    begin
      FBackBuffer.Height := Rect.Bottom - Rect.Top;
      FBackBuffer.Width := Rect.Right - Rect.Left;
      FBackBuffer.Canvas.Brush.Color := NewBackgrnd;
      FBackBuffer.Canvas.FillRect(BackBufferRect);
    end
    else
      Self.Canvas.FillRect(Rect);
  end;

  procedure DrawImage;
  var
    RatioH, RatioW: Single;
  begin
    Self.Canvas.Lock;
    try
      FillCellBkgrd;

      TmpRect := Rect;
      Inc(TmpRect.Left);
      Dec(TmpRect.Right, 2);
      Inc(TmpRect.Top);
      Dec(TmpRect.Bottom, 2);
      if FStretchDraw then
      begin //1.3
        RatioH := (TmpRect.Bottom - TmpRect.Top + 1) / Image.Height;
        RatioW := (TmpRect.Right - TmpRect.Left + 1) / Image.Width;
        if RatioW < RatioH then
          CenterRect(Round(Image.Width * RatioW), Round(Image.Height * RatioW))
        else
          CenterRect(Round(Image.Width * RatioH), Round(Image.Height * RatioH));
      end
      else
      begin //1.3
        if Image.Width > Succ(TmpRect.Right - TmpRect.Left) then
          Image.Width := Succ(TmpRect.Right - TmpRect.Left);
        if Image.Height > Succ(TmpRect.Bottom - TmpRect.Top) then
          Image.Height := Succ(TmpRect.Bottom - TmpRect.Top);
        CenterRect(Image.Width, Image.Height)
      end;
      if FNoFlickering then
      begin
        if FStretchDraw then //1.3
          FBackBuffer.Canvas.StretchDraw(BackBufferTmpRect, Image)
        else
          FBackBuffer.Canvas.Draw(BackBufferTmpRect.Left, BackBufferTmpRect.Top, Image);
        Self.Canvas.Draw(Rect.Left, Rect.Top, FBackBuffer);
      end
      else
      begin
        if FStretchDraw then //1.3
          Self.Canvas.StretchDraw(TmpRect, Image)
        else
          Self.Canvas.Draw(TmpRect.Left, TmpRect.Top, Image);
      end;

      if (gdFocused in State) and not (csDesigning in ComponentState) and
        not (dgRowSelect in Options) then
        Self.Canvas.DrawFocusRect(Rect);
    finally
      Self.Canvas.Unlock;
    end;
  end;

  {$IFDEF IMG_GIF}
  function LoadGIFImage: Boolean; //1.3
  var
    GIFPic: TGIFImage;
  begin
    GIFPic := TGIFImage.Create;
    try
      try
        GIFPic.LoadFromStream(BlobStream);
        GIFPic.DecodeAllFrames;
        Image.Assign(GIFPic);
      except
        on Exception do
        begin // Bad format or corrupted data
          Result := False;
          Exit;
        end;
      end;
    finally
      GIFPic.Free;
    end;
    Result := True;
  end;
  {$ENDIF}

  {$IFDEF IMG_JPEG}
  function LoadJPEGImage: Boolean; //1.3
  var
    JpegPic: TJPEGImage;
  begin
    JpegPic := TJPEGImage.Create;
    try
      JpegPic.Performance := jpBestSpeed;
      try
        JpegPic.LoadFromStream(BlobStream);
        Image.Assign(JpegPic);
      except
        on Exception do
        begin // Bad format or corrupted data
          Result := False;
          Exit;
        end;
      end;
    finally
      JpegPic.Free;
    end;
    Result := True;
  end;
  {$ENDIF}

  {$IFDEF IMG_META}
  function LoadMetafile: Boolean; //1.3
  var
    MF: TMetafile;
  begin
    MF := TMetafile.Create;
    try
      try
        MF.LoadFromStream(BlobStream);
        Image.Width := MF.Width;
        Image.Height := MF.Height;
        Image.Canvas.Draw(0, 0, MF);
      except
        on Exception do
        begin // Bad format or corrupted data
          Result := False;
          Exit;
        end;
      end;
    finally
      MF.Free;
    end;
    Result := True;
  end;
  {$ENDIF}

  {$IFDEF IMG_ICON}
  function LoadIcon: Boolean; //1.3
  var
    Icon: TIcon;
  begin
    Icon := TIcon.Create;
    try
      try
        Icon.LoadFromStream(BlobStream);
        Image.Width := Icon.Width;
        Image.Height := Icon.Height;
        Image.Transparent := True;
        Image.Canvas.Draw(0, 0, Icon);
      except
        on Exception do
        begin // Bad format or corrupted data
          Result := False;
          Exit;
        end;
      end;
    finally
      Icon.Free;
    end;
    Result := True;
  end;
  {$ENDIF}

  procedure Identify_And_Load_Image; //1.3
  begin
    BlobStream := TStringStream.Create('');
    try
      (Column.Field as TBlobField).SaveToStream(BlobStream);
      {$IFDEF IMG_GIF}
      // GIF format?
      if Copy(BlobStream.DataString, 1, 3) = 'GIF' then
      begin
        BlobStream.Position := 0;
        if LoadGIFImage then
          Exit;
      end;
      {$ENDIF}
      {$IFDEF IMG_JPEG}
      // JPEG format?
      if (Copy(BlobStream.DataString, 1, 2) = 'ÿØ') and
        (Copy(BlobStream.DataString, Pred(BlobStream.Size), 2) = 'ÿÙ') then
      begin
        BlobStream.Position := 0;
        if LoadJPEGImage then
          Exit;
      end;
      {$ENDIF}
      {$IFDEF IMG_ICON}
      // Icon format?
      if (Copy(BlobStream.DataString, 1, 4) = #0#0#1#0) and
        (Copy(BlobStream.DataString, 10, 1) = #0) then
      begin
        BlobStream.Position := 0;
        if LoadIcon then
          Exit;
      end;
      {$ENDIF}
      {$IFDEF IMG_META}
      // Metafile format?
      if (Copy(BlobStream.DataString, 1, 4) = '×ÍÆš') or
        (Copy(BlobStream.DataString, 41, 4) = ' EMF') then
      begin
        BlobStream.Position := 0;
        if LoadMetafile then
          Exit;
      end;
      {$ENDIF}
      // OLE Paint BMP format?
      if (Copy(BlobStream.DataString, 34, 13) = 'Paint.Picture') and
        (Copy(BlobStream.DataString, 79, 2) = 'BM') then
      begin
        BlobStream.Seek(78, soFromBeginning);
        Image.LoadFromStream(BlobStream);
        Exit;
      end;
    finally
      BlobStream.Free;
    end;
  end;

begin
  FBooleanEditor := False;
  if not (DataLink.Active and DefaultDrawing and HandleAllocated) then
  begin
    inherited DrawColumnCell(Rect, DataCol, Column, State);
    Exit;
  end;
  CellSelected := ((gdSelected in State) and ((dgAlwaysShowSelection in Options) or Focused));
  if MultiSelect then
    CellSelected := CellSelected or SelectedRows.Find(DataLink.DataSet.Bookmark, Index);
  if (Column.Field.DataType = ftMemo) or (Column.Field.DataType = ftFmtMemo) then
  begin
    if FDisplayMemo and (Column.Field.Value <> Null) then
    begin // The memo content is displayed
      Self.Canvas.Lock;
      try
        FillCellBkgrd;

        TmpRect := Rect;
        Inc(TmpRect.Top, 2);
        Inc(TmpRect.Left, 2);
        if FMultiLines then
          DrawOptions := DT_LEFT or DT_WORDBREAK
        else
          DrawOptions := DT_LEFT;
        if FNoFlickering then
        begin
          FBackBuffer.Canvas.Font.Assign(Self.Canvas.Font); //1.1
          BackBufTmpRect := BackBufferTmpRect;
          DrawText(FBackBuffer.Canvas.Handle, PChar(Column.Field.AsString),
            Length(Column.Field.AsString), BackBufTmpRect, DT_NOPREFIX or DrawOptions);
          Self.Canvas.Draw(Rect.Left, Rect.Top, FBackBuffer);
        end
        else
          DrawText(Self.Canvas.Handle, PChar(Column.Field.AsString),
            Length(Column.Field.AsString), TmpRect, DT_NOPREFIX or DrawOptions);

        if (gdFocused in State) and not (csDesigning in ComponentState) and
          not (dgRowSelect in Options) then
          Self.Canvas.DrawFocusRect(Rect);
      finally
        Self.Canvas.Unlock;
      end;
    end
    else
      inherited DrawColumnCell(Rect, DataCol, Column, State);
  end
  else
  if Column.Field.DataType = ftBoolean then
  begin
    if FDisplayBoolean then
    begin // A checkbox is displayed
      Self.Canvas.Lock;
      try
        FillCellBkgrd;

        TmpRect := Rect;
        CenterRect(CheckBoxSize, CheckBoxSize);
        if FNoFlickering then
        begin
          BackBufTmpRect := BackBufferTmpRect;
          if Column.Field.AsBoolean = True then
            DrawFrameControl(FBackBuffer.Canvas.Handle, BackBufTmpRect, DFC_BUTTON,
              DFCS_CHECKED or DFCS_FLAT)
          else
            DrawFrameControl(FBackBuffer.Canvas.Handle, BackBufTmpRect, DFC_BUTTON,
              DFCS_BUTTONCHECK or DFCS_FLAT);
          Self.Canvas.Draw(Rect.Left, Rect.Top, FBackBuffer);
        end
        else
        if Column.Field.AsBoolean = True then
          DrawFrameControl(Self.Canvas.Handle, TmpRect, DFC_BUTTON, DFCS_CHECKED or DFCS_FLAT)
        else
          DrawFrameControl(Self.Canvas.Handle, TmpRect, DFC_BUTTON,
            DFCS_BUTTONCHECK or DFCS_FLAT);

        if (gdFocused in State) and not (csDesigning in ComponentState) and
          not (dgRowSelect in Options) then
          Self.Canvas.DrawFocusRect(Rect);
      finally
        Self.Canvas.Unlock;
      end;
    end
    else
      inherited DrawColumnCell(Rect, DataCol, Column, State);
  end
  else
  if Column.Field is TBlobField then
  begin
    if FDisplayImages then
    begin // The picture is displayed
      Image := TBitmap.Create;
      try
        try
          Image.Assign(Column.Field as TBlobField);
        except
          Identify_And_Load_Image; //1.3
        end;
        if Image.Empty then
          inherited DrawColumnCell(Rect, DataCol, Column, State)
        else
          DrawImage;
      finally
        Image.Free;
      end;
    end
    else
      inherited DrawColumnCell(Rect, DataCol, Column, State);
  end
  else
  if FShowTextEllipsis or FMultiLines or FNoFlickering then
  begin // Customized display of text (ellipsis, multilines, without flickering)
    Self.Canvas.Lock;
    try
      FillCellBkgrd;

      TmpRect := Rect;
      Inc(TmpRect.Top, 2);
      case Column.Alignment of
        taLeftJustify:
          begin
            Inc(TmpRect.Left, 2);
            DrawOptions := DT_LEFT;
          end;
        taRightJustify:
          begin
            Dec(TmpRect.Right, 3);
            DrawOptions := DT_RIGHT;
          end;
      else
        DrawOptions := DT_CENTER;
      end;
      if FShowTextEllipsis then
        DrawOptions := DrawOptions or DT_END_ELLIPSIS;
      if FMultiLines then
        DrawOptions := DrawOptions or DT_WORDBREAK;
      if FNoFlickering then
      begin
        FBackBuffer.Canvas.Font.Assign(Self.Canvas.Font); //1.1
        BackBufTmpRect := BackBufferTmpRect;
        DrawText(FBackBuffer.Canvas.Handle, PChar(Column.Field.DisplayText), //1.1
          Length(Column.Field.DisplayText), BackBufTmpRect, DT_NOPREFIX or DrawOptions); //1.1
        Self.Canvas.Draw(Rect.Left, Rect.Top, FBackBuffer);
      end
      else
        DrawText(Self.Canvas.Handle, PChar(Column.Field.DisplayText), //1.1
          Length(Column.Field.DisplayText), TmpRect, DT_NOPREFIX or DrawOptions); //1.1

      if (gdFocused in State) and not (csDesigning in ComponentState) and
        not (dgRowSelect in Options) then
        Self.Canvas.DrawFocusRect(Rect);
    finally
      Self.Canvas.Unlock;
    end;
  end
  else
    inherited DrawColumnCell(Rect, DataCol, Column, State);
end;

procedure TUltimDBGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  OldPenColor: TColor;
begin
  inherited DrawCell(ACol, ARow, ARect, AState);
  if FOptionsMenu and (ACol = 0) and (ARow = 0) and (dgIndicator in Options) and
    (dgTitles in Options) then
  begin
    Canvas.Lock;
    try
      OldPenColor := Canvas.Pen.Color;
      Canvas.Pen.Color := clBlack;
      Canvas.Brush.Color := clMenu;
      Canvas.Rectangle(ARect.Left + 2, ARect.Top + 3, ARect.Right - 2, ARect.Top + 14);
      Canvas.Pen.Color := OldPenColor; // needed to display correctly the column moving bar
    finally
      Canvas.Unlock;
    end;
  end;
end;

procedure TUltimDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  CellX, CellY: Integer;
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
  inherited MouseDown(Button, Shift, X, Y);
  { Affichage du menu des options ? }
  if FOptionsMenu and (DataLink <> nil) and DataLink.Active and (dgIndicator in Options) and
    (dgTitles in Options) then
  begin
    MouseToCell(X, Y, CellX, CellY);
    if (CellX = 0) and (CellY = 0) then
      ShowPopUpMenu(X, Y);
  end;
end;

procedure TUltimDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TUltimDBGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
  inherited MouseMove(Shift, X, Y);
  if FCellHints then
    PopUpHint(X, Y);
end;

procedure TUltimDBGrid.CellClick(Column: TColumn);
begin
  inherited CellClick(Column);
  if FBooleanEditor then
    ChangeBoolean(Column.Field);
end;

procedure TUltimDBGrid.DblClick;
begin
  inherited DblClick;
  if FDisplayBoolean and (SelectedField is TBooleanField) then
    // Boolean editor?
    CanEditField;
end;

procedure TUltimDBGrid.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  if FCellHints and Assigned(FHintWnd) then
    FHintWnd.ReleaseHandle;
end;

procedure TUltimDBGrid.Scroll(Distance: Integer);
begin
  if FUseRowColors then
    Invalidate;
  inherited Scroll(Distance);
  if FCellHints and Assigned(FHintWnd) then
    FHintWnd.ReleaseHandle;
end;

procedure TUltimDBGrid.WMSize(var Message: TWMSize);
begin
  inherited;
  if not FLockedWidth then
  begin
    if (FAutoWidth <> awNone) and FAutoWidthOnResize then
      SetAutoWidth(FAutoWidth);

    if FCurrentControl <> nil then
      if FCurrentControl.Visible then
        PlaceControl(FCurrentControl, Col, Row);
  end;
end;

procedure TUltimDBGrid.LinkActive(Value: Boolean);
begin
  inherited LinkActive(Value);
  FSortedFields := nil;
  RowHeightsChanged;
  SendFooterMsg(FTR_ACTIVE);
end;

procedure TUltimDBGrid.LayoutChanged;
begin
  inherited LayoutChanged;
  if FLockedHeight = 0 then
  begin
    FTitleRowHeight := 0;
    SetDefaultRowHeight(FNewDefRowHeight);
  end;
  SendSizeMessage;
end;

procedure TUltimDBGrid.ColWidthsChanged;
begin
  inherited ColWidthsChanged;
  SendSizeMessage;
  SendFooterMsg(FTR_COLWIDTHS);
end;

procedure TUltimDBGrid.RowHeightsChanged;
var
  RowNo: Integer;
begin
  inherited RowHeightsChanged;
  for RowNo := 1 to VisibleRowCount + 1 do
    if RowHeights[RowNo] <> DefaultRowHeight then
    begin
      DefaultRowHeight := RowHeights[RowNo];
      Break;
    end;
  SendSizeMessage;
end;

procedure TUltimDBGrid.DoTitleClick(ACol: LongInt; AField: TField);
var
  Found: Boolean;
  I: Integer;
  ClickedField: TSortFields;
begin
  if FSortOnTitleClick and (AField <> nil) then
  begin
    Found := False;
    for I := 0 to Length(FSortedFields) - 1 do
      if SameText(AField.FieldName, FSortedFields[I].Name) then
      begin
        Found := True;
        FSortedFields[I].Order := not FSortedFields[I].Order;
        Sort(FSortedFields);
        Break;
      end;
    if not Found then
    begin
      SetLength(ClickedField, 1);
      ClickedField[0].Name := AField.FieldName;
      ClickedField[0].Order := soSortASC;
      Sort(ClickedField);
    end;
  end;
  inherited DoTitleClick(ACol, AField);
end;

procedure TUltimDBGrid.RedirectGetBtnParams(Sender: TObject; Field: TField; AFont: TFont;
  var Background: TColor; var SortMarker: TSortMarker; IsDown: Boolean);
var
  SortOrder: Boolean;

  function IsSortedField: Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to Length(FSortedFields) - 1 do
      if SameText(Field.FieldName, FSortedFields[I].Name) then
      begin
        SortOrder := FSortedFields[I].Order;
        Result := True;
        Break;
      end;
  end;

begin
  if (Field <> nil) and IsSortedField then
  begin
    if SortOrder = soSortUP then
      SortMarker := smUP
    else
      SortMarker := smDOWN;
  end
  else
    SortMarker := smNONE;
  if Assigned(FOldGetBtnParams) then
    FOldGetBtnParams(Sender, Field, AFont, Background, SortMarker, IsDown);
end;

procedure TUltimDBGrid.TopLeftChanged;
begin
  inherited TopLeftChanged;
  SendFooterMsg(FTR_SCROLL);
end;

procedure TUltimDBGrid.SendSizeMessage;
var
  Msg: TMessage;
begin
  if HandleAllocated and (FNewDefRowHeight <> 0) then
  begin
    // Awful hack to get inherited grid to call private UpdateRowCount
    Msg.Msg := WM_SIZE;
    Msg.WParam := SIZE_RESTORED;
    Msg.lParamLo := Width;
    Msg.lParamHi := Height;
    Perform(Msg.Msg, Msg.wParam, Msg.lParam);
  end;
end;

procedure TUltimDBGrid.ChangeBoolean(Field: TField);
begin
  DataLink.Edit;
  if Field.Value = Null then
    Field.Value := True
  else
    Field.Value := not Field.Value;
  DrawCell(Col, Row, CellRect(Col, Row), [gdSelected, gdFocused]);
end;

procedure TUltimDBGrid.WMChar(var Msg: TWMChar);
var
  Tab: Word;
begin
  if FEnterAsTab and (Msg.CharCode = VK_RETURN) then
  begin
    Tab := VK_TAB;
    KeyDown(Tab, []);
    Exit;
  end;
  if Char(Msg.CharCode) in [^H, #32..#255] then
  begin
    if CanEditField then
      inherited
    else
    if FBooleanEditor then
      ChangeBoolean(Fields[SelectedIndex]);
  end
  else
    inherited;
end;

procedure TUltimDBGrid.SetNoFlickering(Value: Boolean);
begin
  if FNoFlickering <> Value then
  begin
    FNoFlickering := Value;
    Invalidate;
  end;
end;

procedure TUltimDBGrid.SetControls(Value: TUltimControls);
begin
  FControls.Assign(Value);
end;

procedure TUltimDBGrid.HideCurrentControl;
begin
  if FCurrentControl <> nil then
  begin
    FCurrentControl.WindowProc := FOldControlWndProc;
    FCurrentControl.Hide;
    FCurrentControl := nil;
  end;
  FOldControlWndProc := nil;
end;

procedure TUltimDBGrid.ControlWndProc(var Message: TMessage);
begin
  FOldControlWndProc(Message);
  if Message.Msg = WM_KEYUP then
    // If you test KeyDown, the Escape key does not cancel your changes
    case TWMKey(Message).CharCode of
      VK_ESCAPE:
        if not (dgAlwaysShowEditor in Options) then
        begin
          HideCurrentControl;
          if Self.Visible then
            Self.SetFocus;
        end;
    end;
  if Message.Msg = CM_EXIT then
    HideCurrentControl;
end;

function TUltimDBGrid.MaxColIndex: Integer;
begin
  Result := ColCount - 1;
  if dgIndicator in Options then
    Dec(Result);
end;

procedure TUltimDBGrid.SetAutoWidth(Value: TAutoWidth);
var
  ColNo, MaxColNo, ColumnCount: Integer;
  NewWidth, MaxWidth: Integer;
  Total, Available, OldAvail: Integer;
begin
  FAutoWidth := Value;
  if Assigned(FOnAutoWidthChange) then
    FOnAutoWidthChange(Self, FAutoWidth);
  if FAutoWidth <> awNone then
  begin
    if (FAutoWidth <> awWidestValue) and (FAutoWidth <> awWidestValueTitle) then
      SetFullSizeMemo(False);
    if DataLink.Active then
    begin
      FLockedWidth := True;
      Screen.Cursor := crHourGlass;
      BeginUpdate;
      try
        MaxColNo := MaxColIndex;

        if (FAutoWidth = awWidestValue) or (FAutoWidth = awWidestValueTitle) then
        begin // Widest value / Wv title
          for ColNo := 0 to MaxColNo do
            if Columns[ColNo].Visible then
            begin
              if FAutoWidth = awWidestValue then
                MaxWidth := 0
              else
              begin
                Canvas.Font.Assign(Columns[ColNo].Title.Font); //1.1
                MaxWidth := Canvas.TextWidth(Columns[ColNo].Title.Caption) + 2;
              end;
              Canvas.Font.Assign(Columns[ColNo].Font); //1.1
              DataLink.DataSet.DisableControls;
              try
                DataLink.DataSet.Last;
                while not DataLink.DataSet.BOF do
                begin
                  if Fields[ColNo].Value <> Null then
                  begin
                    if FFullSizeMemo = False then
                      NewWidth := Canvas.TextWidth(Fields[ColNo].DisplayText)
                    else
                    begin
                      if (Fields[ColNo].DataType = ftMemo) or
                        (Fields[ColNo].DataType = ftFmtMemo) then
                        NewWidth := Canvas.TextWidth(Fields[ColNo].Value)
                      else
                        NewWidth := Canvas.TextWidth(Fields[ColNo].DisplayText);
                    end;
                    if NewWidth > MaxWidth then
                      MaxWidth := NewWidth;
                  end;
                  DataLink.DataSet.Prior;
                end;
              finally
                DataLink.DataSet.EnableControls;
              end;
              if MaxWidth = 0 then
                Columns[ColNo].Width := Columns[ColNo].DefaultWidth
              else
              begin
                Inc(MaxWidth, 5);
                if MaxWidth > Self.ClientWidth then
                  Columns[ColNo].Width := Self.ClientWidth
                else
                  Columns[ColNo].Width := MaxWidth;
              end;
            end;
        end
        else
        if FAutoWidth = awDefault then
        begin // Default
          for ColNo := 0 to MaxColNo do
            if Columns[ColNo].Visible then
              Columns[ColNo].Width := Columns[ColNo].DefaultWidth;
        end
        else
        begin // Proportional & uniform
          ColumnCount := 0;
          Available := Self.ClientWidth;
          Total := 0;
          for ColNo := 0 to MaxColNo do
            if Columns[ColNo].Visible then
            begin
              Inc(ColumnCount);
              if dgColLines in Options then
                Dec(Available, GridLineWidth);
              Inc(Total, Columns[ColNo].Width);
            end;
          if dgIndicator in Options then
          begin
            Dec(Available, IndicatorWidth);
            if dgColLines in Options then
              Dec(Available, GridLineWidth);
          end;
          OldAvail := Available;
          for ColNo := 0 to MaxColNo do
            if Columns[ColNo].Visible then
            begin
              if FAutoWidth = awProportional then
              begin // Proportional
                NewWidth := Round((Columns[ColNo].Width / Total) * OldAvail);
                Dec(Available, NewWidth);
                Dec(ColumnCount);
                if ColumnCount = 0 then
                  Columns[ColNo].Width := NewWidth + Available
                else
                  Columns[ColNo].Width := NewWidth;
              end
              else
              begin // Uniform
                NewWidth := Available div ColumnCount;
                if ColNo < (Available mod ColumnCount) then
                  Columns[ColNo].Width := NewWidth + 1
                else
                  Columns[ColNo].Width := NewWidth;
              end;
            end;
        end;
      finally
        EndUpdate;
        Screen.Cursor := crDefault;
        FLockedWidth := False;
      end;
    end;
    if not ((csLoading in ComponentState) or FAutoWidthOnResize) then //1.1
      SetAutoWidth(awNone);
  end;
end;

procedure TUltimDBGrid.SetAutoWidthOnResize(Value: Boolean);
begin
  if FAutoWidthOnResize <> Value then
  begin
    FAutoWidthOnResize := Value;
    if FAutoWidthOnResize then
      SetAutoWidth(FAutoWidth);
  end;
end;

procedure TUltimDBGrid.SetDisplayBoolean(Value: Boolean);
begin
  if FDisplayBoolean <> Value then
  begin
    FDisplayBoolean := Value;
    Invalidate;
  end;
end;

procedure TUltimDBGrid.SetDisplayImages(Value: Boolean);
begin
  if FDisplayImages <> Value then
  begin
    FDisplayImages := Value;
    Invalidate;
  end;
end;

procedure TUltimDBGrid.SetDisplayMemo(Value: Boolean);
begin
  if FDisplayMemo <> Value then
  begin
    FDisplayMemo := Value;
    if FDisplayMemo = False then
      SetFullSizeMemo(False);
    Invalidate;
  end;
end;

procedure TUltimDBGrid.SetFullSizeMemo(Value: Boolean);
var
  ColNo: Integer;
  NewWidth, MaxWidth: Integer;
begin
  if FFullSizeMemo <> Value then
  begin
    FFullSizeMemo := Value;
    if FFullSizeMemo = True then
    begin
      SetDisplayMemo(True);
      if not (FAutoWidth in [awNone, awWidestValue, awWidestValueTitle]) then
        SetAutoWidth(awNone);
    end;
    if DataLink.Active then
    begin
      FLockedWidth := True;
      BeginUpdate;
      try
        for ColNo := 0 to MaxColIndex do
          if ((Fields[ColNo].DataType = ftMemo) or (Fields[ColNo].DataType = ftFmtMemo)) and
            Columns[ColNo].Visible then
          begin
            Canvas.Font.Assign(Columns[ColNo].Font); //1.1

            if FFullSizeMemo = False then
              MaxWidth := Canvas.TextWidth(Fields[ColNo].DisplayText)
            else
            begin
              MaxWidth := 0;
              DataLink.DataSet.DisableControls;
              try
                DataLink.DataSet.Last;
                while not DataLink.DataSet.BOF do
                begin
                  if Fields[ColNo].Value <> Null then
                  begin
                    NewWidth := Canvas.TextWidth(Fields[ColNo].Value);
                    if NewWidth > MaxWidth then
                      MaxWidth := NewWidth;
                  end;
                  DataLink.DataSet.Prior;
                end;
              finally
                DataLink.DataSet.EnableControls;
              end;
            end;

            if MaxWidth = 0 then
              Columns[ColNo].Width := Columns[ColNo].DefaultWidth
            else
            begin
              Inc(MaxWidth, 5);
              if MaxWidth > Self.ClientWidth then
                Columns[ColNo].Width := Self.ClientWidth
              else
                Columns[ColNo].Width := MaxWidth;
            end;
          end;
      finally
        EndUpdate;
        FLockedWidth := False;
      end;
    end;
  end;
end;

procedure TUltimDBGrid.SetUseRowColors(Value: Boolean);
begin
  if FUseRowColors <> Value then
  begin
    FUseRowColors := Value;
    Invalidate;
  end;
end;

procedure TUltimDBGrid.SetRowColor(Index: Integer; Value: TColor);
begin
  if FRowColors[Index] <> Value then
  begin
    FRowColors[Index] := Value;
    if FUseRowColors then
      Invalidate;
  end;
end;

procedure TUltimDBGrid.SetRowSizingAllowed(Value: Boolean);
begin
  if FRowSizingAllowed <> Value then
  begin
    FRowSizingAllowed := Value;
    if FRowSizingAllowed then
      TDrawGrid(Self).Options := TDrawGrid(Self).Options + [goRowSizing]
    else
      TDrawGrid(Self).Options := TDrawGrid(Self).Options - [goRowSizing];
  end;
end;

procedure TUltimDBGrid.LockHeight;
begin
  Inc(FLockedHeight);
end;

procedure TUltimDBGrid.UnLockHeight;
begin
  Dec(FLockedHeight);
end;

procedure TUltimDBGrid.CalcTitleHeight;
var
  ColNo, NewHeight: Integer;
begin
  if dgTitles in Options then
  begin
    if FTitleRowHeight = 0 then
    begin
      if (Columns.Count > 0) and HandleAllocated then
      begin
        for ColNo := 0 to MaxColIndex do
          if Columns[ColNo].Visible then
          begin
            Canvas.Font.Assign(Columns[ColNo].Title.Font); //1.1
            NewHeight := Canvas.TextHeight('Â_');
            if NewHeight > FTitleRowHeight then
              FTitleRowHeight := NewHeight;
          end;
      end;

      if FTitleRowHeight = 0 then
        FTitleRowHeight := DefaultRowHeight
      else
        Inc(FTitleRowHeight, 4);
    end;
    RowHeights[0] := FTitleRowHeight;
  end;
end;

function TUltimDBGrid.GetDefaultRowHeight: Integer;
begin
  Result := inherited DefaultRowHeight;
end;

procedure TUltimDBGrid.SetDefaultRowHeight(Value: Integer);
begin
  if (Value <> FNewDefRowHeight) or (Value <> inherited DefaultRowHeight) then
  begin
    LockHeight;
    try
      if Value = 0 then
        FNewDefRowHeight := inherited DefaultRowHeight
      else
      begin
        FNewDefRowHeight := Value;
        BeginUpdate;
        try
          inherited DefaultRowHeight := Value;
          CalcTitleHeight;
        finally
          EndUpdate;
        end;
      end;
    finally
      UnLockHeight;
    end;
  end;
end;

procedure TUltimDBGrid.PopUpHint(ClientX, ClientY: Integer);
var
  CellX, CellY: Integer;
  Field: TField;
  OldActive: Integer;
  HintMsg: string;
  HintRect: TRect;
  Pt: TPoint;
begin
  if Assigned(FHintWnd) and HandleAllocated and DataLink.Active then
  begin
    MouseToCell(ClientX, ClientY, CellX, CellY);
    if dgIndicator in Options then
      Dec(CellX);
    if dgTitles in Options then
      Dec(CellY);
    if (CellX >= 0) and (CellY >= 0) then
    begin
      Field := Columns[CellX].Field;
      if (Field.DataType = ftMemo) or (Field.DataType = ftFmtMemo) or
        not (Field is TBlobField) then
      begin
        OldActive := DataLink.ActiveRecord;
        try
          DataLink.ActiveRecord := CellY;
          if (Field.DataType = ftMemo) or (Field.DataType = ftFmtMemo) then //1.1
            HintMsg := Field.AsString
          else //1.1
            HintMsg := Field.DisplayText;
               {End;}//1.1
        finally
          DataLink.ActiveRecord := OldActive;
        end;
        if HintMsg <> '' then
        begin
          Canvas.Font.Assign(Columns[CellX].Font); //1.1
          if (Canvas.TextWidth(HintMsg) + 2) > Columns[CellX].Width then
          begin
            HintRect := FHintWnd.CalcHintRect(Screen.Width shr 1, HintMsg, nil);
            Pt.X := ClientX;
            Pt.Y := ClientY;
            Pt := ClientToScreen(Pt);
            OffsetRect(HintRect, Pt.X, Pt.Y + 16);
            FHintWnd.ActivateHint(HintRect, HintMsg);
          end
          else
            FHintWnd.ReleaseHandle;
        end
        else
          FHintWnd.ReleaseHandle;
      end
      else
        FHintWnd.ReleaseHandle;
    end
    else
      FHintWnd.ReleaseHandle;
  end;
end;

procedure TUltimDBGrid.SetCellHints(Value: Boolean);
begin
  if FCellHints <> Value then
  begin
    FCellHints := Value;
    if (not FCellHints) and Assigned(FHintWnd) then
      FHintWnd.ReleaseHandle;
  end;
end;

procedure TUltimDBGrid.SetShowTextEllipsis(Value: Boolean);
begin
  if FShowTextEllipsis <> Value then
  begin
    FShowTextEllipsis := Value;
    Invalidate;
  end;
end;

procedure TUltimDBGrid.SetMultiLines(Value: Boolean);
begin
  if FMultiLines <> Value then
  begin
    FMultiLines := Value;
    Invalidate;
  end;
end;

procedure TUltimDBGrid.SetStretchDraw(Value: Boolean); //1.3
begin
  if FStretchDraw <> Value then
  begin
    FStretchDraw := Value;
    Invalidate;
  end;
end;

procedure TUltimDBGrid.SetSortOnTitleClick(Value: Boolean);
begin
  if FSortOnTitleClick <> Value then
  begin
    FSortOnTitleClick := Value;
    if FSortOnTitleClick then
      TitleButtons := True;
    Invalidate;
  end;
end;

procedure TUltimDBGrid.ShowPopUpMenu(X, Y: Integer);
const
  NbFixedCols = 7;
var
  Itm: Integer;
  NewItem, NewSubItem: TMenuItem;
  Pt: TPoint;
begin
  if not (csDesigning in ComponentState) then
  begin
    with FGridPopUpMenu do
    begin
      // Do we create the menu ?
      if Items.Count = 0 then
      begin
        PopupComponent := Self;

        NewItem := TMenuItem.Create(FGridPopUpMenu);
        NewItem.Caption := lsMenus[0];
        for Itm := 0 to 4 do
        begin
          NewSubItem := TMenuItem.Create(FGridPopUpMenu);
          NewSubItem.Caption := lsMenusAW[Itm];
          NewItem.Add(NewSubItem);
        end;
        NewItem.Items[0].OnClick := MenuAWDefault;
        NewItem.Items[1].OnClick := MenuAWProportional;
        NewItem.Items[2].OnClick := MenuAWUniform;
        NewItem.Items[3].OnClick := MenuAWWidestValue;
        NewItem.Items[4].OnClick := MenuAWWidestValueTitle;
        Items.Add(NewItem);

        for Itm := 1 to 5 do
        begin
          NewItem := TMenuItem.Create(FGridPopUpMenu);
          NewItem.Caption := lsMenus[Itm];
          Items.Add(NewItem);
        end;
        Items[1].OnClick := MenuAWOnResize;
        Items[2].OnClick := MenuAllowColResizing;
        Items[3].OnClick := MenuAllowRowResizing;

        NewItem := TMenuItem.Create(FGridPopUpMenu);
        NewItem.Caption := lsMenus[6];
        for Itm := 0 to NbFixedCols do
        begin
          NewSubItem := TMenuItem.Create(FGridPopUpMenu);
          if Itm < 2 then
            NewSubItem.Caption := IntToStr(Itm) + lsMenusFC[0]
          else
            NewSubItem.Caption := IntToStr(Itm) + lsMenusFC[1];
          NewSubItem.Tag := Itm;
          NewSubItem.OnClick := MenuSetFixedCols;
          NewItem.Add(NewSubItem);
        end;
        Items.Add(NewItem);

        for Itm := 7 to 37 do
        begin
          NewItem := TMenuItem.Create(FGridPopUpMenu);
          NewItem.Caption := lsMenus[Itm];
          Items.Add(NewItem);
        end;
        Items[7].OnClick := MenuShowHideColumns;
        Items[8].OnClick := MenuSortColumns;
        Items[11].OnClick := MenuSetFilter;
        Items[12].OnClick := MenuActivateFilter;
        Items[13].OnClick := MenuAddFilter;
        Items[16].OnClick := MenuDisplayBoolean;
        Items[17].OnClick := MenuDisplayImages;
        Items[18].OnClick := MenuDisplayMemo;
        Items[19].OnClick := MenuFullSizeMemo;
        Items[20].OnClick := MenuUseRowColors;
        Items[21].OnClick := MenuSetCellHints;
        Items[22].OnClick := MenuShowTextEllipsis;
        Items[23].OnClick := MenuSetMultiLines;
        Items[26].OnClick := MenuSetEnterAsTab;
        Items[29].OnClick := MenuExportGrid;
        Items[30].OnClick := MenuPrintGrid;
        Items[33].OnClick := MenuSetReadOnly;
        Items[36].OnClick := MenuConfig;
        Items[36].Tag := tagSAVE;
        Items[37].OnClick := MenuConfig;
        Items[37].Tag := tagRESTORE;
      end;

      // Update the values
      Items[0].Items[0].Checked := (FAutoWidth = awDefault);
      Items[0].Items[1].Checked := (FAutoWidth = awProportional);
      Items[0].Items[2].Checked := (FAutoWidth = awUniform);
      Items[0].Items[3].Checked := (FAutoWidth = awWidestValue);
      Items[0].Items[4].Checked := (FAutoWidth = awWidestValueTitle);
      Items[0].Visible := mnuAutoWidth in FOptionsMenuItems;
      Items[1].Checked := FAutoWidthOnResize;
      Items[1].Visible := mnuAutoWidthOnResize in FOptionsMenuItems;
      Items[2].Checked := (dgColumnResize in Options);
      Items[2].Visible := mnuAllowColResizing in FOptionsMenuItems;
      Items[3].Checked := FRowSizingAllowed;
      Items[3].Visible := mnuAllowRowResizing in FOptionsMenuItems;
      for Itm := 0 to NbFixedCols do
        Items[6].Items[Itm].Checked := (FixedCols = Itm);
      Items[6].Visible := mnuSetFixedColumns in FOptionsMenuItems;
      Items[7].Visible := mnuShowHideColumns in FOptionsMenuItems;
      Items[8].Visible := mnuSortColumns in FOptionsMenuItems;
      Items[11].Visible := mnuSetFilter in FOptionsMenuItems;
      if DataLink.DataSet.Filter = '' then
        Items[12].Visible := False
      else
      begin
        if Length(DataLink.DataSet.Filter) > 40 then
          Items[12].Caption := lsMenus[12] + ': ' +
            Copy(DataLink.DataSet.Filter, 1, 37) + '...'
        else
          Items[12].Caption := lsMenus[12] + ': ' + DataLink.DataSet.Filter;
        Items[12].Checked := DataLink.DataSet.Filtered;
        Items[12].Visible := mnuUseFilter in FOptionsMenuItems;
      end;
      Items[13].Visible := Items[12].Visible and (mnuAddCurrentFilter in FOptionsMenuItems);
      Items[16].Checked := FDisplayBoolean;
      Items[16].Visible := mnuDisplayBooleans in FOptionsMenuItems;
      Items[17].Checked := FDisplayImages;
      Items[17].Visible := mnuDisplayImages in FOptionsMenuItems;
      Items[18].Checked := FDisplayMemo;
      Items[18].Visible := mnuDisplayMemo in FOptionsMenuItems;
      Items[19].Checked := FFullSizeMemo;
      Items[19].Visible := mnuFullSizeMemo in FOptionsMenuItems;
      Items[20].Checked := FUseRowColors;
      Items[20].Visible := mnuUseRowColors in FOptionsMenuItems;
      Items[21].Checked := FCellHints;
      Items[21].Visible := mnuShowCellHints in FOptionsMenuItems;
      Items[22].Checked := FShowTextEllipsis;
      Items[22].Visible := mnuShowTextEllipsis in FOptionsMenuItems;
      Items[23].Checked := FMultiLines;
      Items[23].Visible := mnuMultiLinesCell in FOptionsMenuItems;
      Items[26].Checked := FEnterAsTab;
      Items[26].Visible := mnuEnterKeyAsTab in FOptionsMenuItems;
      Items[29].Visible := mnuExportGrid in FOptionsMenuItems;
      Items[30].Visible := mnuPrintGrid in FOptionsMenuItems;
      Items[33].Checked := Self.ReadOnly;
      Items[33].Visible := mnuReadOnly in FOptionsMenuItems;
      Items[36].Visible := mnuSaveConfiguration in FOptionsMenuItems;
      Items[37].Visible := mnuRestoreConfiguration in FOptionsMenuItems;

      // Display the popup menu
      Pt.X := X;
      Pt.Y := Y;
      Pt := ClientToScreen(Pt);
      Popup(Pt.X, Pt.Y);
    end;
  end;
end;

procedure TUltimDBGrid.SetOptionsMenu(Value: Boolean);
begin
  if FOptionsMenu <> Value then
  begin
    FOptionsMenu := Value;
    if FOptionsMenu then
      Options := Options + [dgIndicator, dgTitles];
    InvalidateTitles;
  end;
end;

procedure TUltimDBGrid.MenuAWDefault;
begin
  if TMenuItem(Sender).Checked then
    SetAutoWidth(awNone)
  else
    SetAutoWidth(awDefault);
end;

procedure TUltimDBGrid.MenuAWProportional;
begin
  if TMenuItem(Sender).Checked then
    SetAutoWidth(awNone)
  else
    SetAutoWidth(awProportional);
end;

procedure TUltimDBGrid.MenuAWUniform;
begin
  if TMenuItem(Sender).Checked then
    SetAutoWidth(awNone)
  else
    SetAutoWidth(awUniform);
end;

procedure TUltimDBGrid.MenuAWWidestValue;
begin
  if TMenuItem(Sender).Checked then
    SetAutoWidth(awNone)
  else
    SetAutoWidth(awWidestValue);
end;

procedure TUltimDBGrid.MenuAWWidestValueTitle;
begin
  if TMenuItem(Sender).Checked then
    SetAutoWidth(awNone)
  else
    SetAutoWidth(awWidestValueTitle);
end;

procedure TUltimDBGrid.MenuAWOnResize;
begin
  SetAutoWidthOnResize(not TMenuItem(Sender).Checked);
end;

procedure TUltimDBGrid.MenuAllowColResizing;
begin
  if TMenuItem(Sender).Checked then
    Options := Options - [dgColumnResize]
  else
    Options := Options + [dgColumnResize];
end;

procedure TUltimDBGrid.MenuAllowRowResizing;
begin
  SetRowSizingAllowed(not TMenuItem(Sender).Checked);
end;

procedure TUltimDBGrid.MenuSetFixedCols;
begin
  FixedCols := TMenuItem(Sender).Tag;
end;

procedure TUltimDBGrid.MenuShowHideColumns;
var
  FrmSelect: TFUltimDBSelect;
  I: Integer;
begin
  FrmSelect := TFUltimDBSelect.Create(Self);
  with FrmSelect do
  try
    Icon := TForm(GetParentForm(Self)).Icon;
    Color := GetParentForm(Self).Color;
    Caption := StringReplace(TMenuItem(Sender).Caption, '&', '', []);
    Caption := StringReplace(Caption, '...', '', []);
    B_Cancel.Caption := lsBtnCancel;
    for I := 0 to Columns.Count - 1 do
    begin
      UltimCLB.Items.Add(Columns[I].Title.Caption);
      UltimCLB.Checked[I] := Columns[I].Visible;
    end;
    if ShowModal = mrOk then
    begin
      for I := 0 to Columns.Count - 1 do
        Columns[I].Visible := UltimCLB.Checked[I];
    end;
  finally
    FrmSelect.Free;
  end;
end;

procedure TUltimDBGrid.MenuSortColumns;
var
  FrmSort: TFUltimDBSort;
  FTS, Sep: Integer;
  FieldsToSort: TSortFields;
  DSet: TDataSet;
  I, DC: Integer;
  Key: string;
  {$IFDEF ADO}
  SynNo: Integer;
  Syntagme,
    Order: string;
  {$ENDIF}
  {$IFDEF BDE}
  TableBDE: TTable;
  IndexDefBDE: TIndexDefs;
  {$ENDIF}
  ShowForm: Boolean;

  procedure Fill_FTS_And_Sort;
  var
    SF: Integer;
  begin
    with FrmSort do
    begin
      SetLength(FieldsToSort, SortFieldCount);
      FTS := 0;
      for SF := 1 to 5 do
        if SortFields[SF] <> '' then
        begin
          Sep := Pos('|', SortFields[SF]);
          if Sep > 0 then
            FieldsToSort[FTS].Name := Copy(SortFields[SF], Sep + 2, Length(SortFields[SF]))
          else
            FieldsToSort[FTS].Name := SortFields[SF];
          FieldsToSort[FTS].Order := not Descending[SF];
          Inc(FTS);
        end;
      Sort(FieldsToSort);
    end;
  end;

begin
  FrmSort := TFUltimDBSort.Create(Self);
  with FrmSort do
  try
    Icon := TForm(GetParentForm(Self)).Icon;
    Color := GetParentForm(Self).Color;
    Caption := StringReplace(TMenuItem(Sender).Caption, '&', '', []);
    CB_Desc1.Caption := lsDescSort;
    CB_Desc2.Caption := lsDescSort;
    CB_Desc3.Caption := lsDescSort;
    CB_Desc4.Caption := lsDescSort;
    CB_Desc5.Caption := lsDescSort;
    B_Cancel.Caption := lsBtnCancel;
    DSet := DataLink.DataSet;
    {$IFDEF ADO}
    if DSet is TCustomADODataSet then
    begin // ADO Table or Query
      DC := 0;
      for I := 0 to Columns.Count - 1 do
        if not ((Columns[I].Field is TBlobField) or (Columns[I].Field is TBytesField)) then
        begin
          if Columns[I].Title.Caption = Columns[I].FieldName then
            LB_Fields.Items.Add(Columns[I].FieldName)
          else
            LB_Fields.Items.Add('"' + Columns[I].Title.Caption + '" | ' +
              Columns[I].FieldName);
          Inc(DC);
        end;
      DisplayControls(DC);
      SynNo := 1;
      repeat
        // Splitting and analysis of sorting criterias
        Syntagme := Trim(ExtractWord(SynNo, TCustomADODataSet(DSet).Sort, [',']));
        if Syntagme <> '' then
        begin
          if Syntagme[1] = '[' then
          begin
            Key := ExtractWord(1, Syntagme, ['[', ']']);
            Order := Trim(Copy(Syntagme, Length(Key) + 4, Length(Syntagme)));
          end
          else
          begin
            Key := ExtractWord(1, Syntagme, [' ']);
            Order := Trim(Copy(Syntagme, Length(Key) + 2, Length(Syntagme)));
          end;
          for I := 0 to LB_Fields.Items.Count - 1 do
          begin // On recherche dans la liste l'item correspondant au critère de tri
            Sep := Pos('|', LB_Fields.Items[I]);
            if Sep = 0 then
            begin
              if SameText(Key, LB_Fields.Items[I]) then
                Key := LB_Fields.Items[I];
            end
            else
            if SameText(Key, Copy(LB_Fields.Items[I], Sep + 2,
              Length(LB_Fields.Items[I]))) then
              Key := LB_Fields.Items[I];
          end;
          SetValues(SynNo, Key, (Order = 'DESC'));
          Inc(SynNo);
        end;
      until Syntagme = '';
      ShowForm := True;
    end
    else
    {$ENDIF}
    {$IFDEF BDE}
    if DSet is TTable then
    begin // BDE Table
      DisplayControls(1);
      CB_Desc1.Visible := False;
      TableBDE := TTable(DSet);
      IndexDefBDE := TableBDE.IndexDefs;
      IndexDefBDE.Update;
      for I := 0 to IndexDefBDE.Count - 1 do
      begin
        if IndexDefBDE.Items[I].Name = '' then
        begin
          LB_Fields.Items.Add(IndexDefBDE.Items[I].Fields);
          if TableBDE.IndexFieldNames = IndexDefBDE.Items[I].Fields then
            SortField1.Text := LB_Fields.Items[I];
        end
        else
        begin
          if SameText(IndexDefBDE.Items[I].Name, IndexDefBDE.Items[I].Fields) then
            LB_Fields.Items.Add(IndexDefBDE.Items[I].Fields)
          else
            LB_Fields.Items.Add('"' + IndexDefBDE.Items[I].Name + '" | ' +
              IndexDefBDE.Items[I].Fields);
          if (TableBDE.IndexName = IndexDefBDE.Items[I].Name) or
            (TableBDE.IndexFieldNames = IndexDefBDE.Items[I].Fields) then
            SortField1.Text := LB_Fields.Items[I];
        end;
      end;
      ShowForm := True;
    end
    else
    {$ENDIF}
    if DSet is TRxMemoryData then
    begin // Memory Table
      DC := 0;
      for I := 0 to Columns.Count - 1 do
        if not ((Columns[I].Field is TBlobField) or (Columns[I].Field is TBytesField)) then
        begin
          if Columns[I].Title.Caption = Columns[I].FieldName then
            LB_Fields.Items.Add(Columns[I].FieldName)
          else
            LB_Fields.Items.Add('"' + Columns[I].Title.Caption + '" | ' +
              Columns[I].FieldName);
          Inc(DC);
        end;
      DisplayControls(DC);
      CB_Desc2.Visible := False;
      CB_Desc3.Visible := False;
      CB_Desc4.Visible := False;
      CB_Desc5.Visible := False;
      DC := 0;
      for FTS := 0 to Length(FSortedFields) - 1 do
      begin
        Key := FSortedFields[FTS].Name;
        for I := 0 to LB_Fields.Items.Count - 1 do
        begin // We look in the list for the item matching the sorting criteria
          Sep := Pos('|', LB_Fields.Items[I]);
          if Sep = 0 then
          begin
            if SameText(Key, LB_Fields.Items[I]) then
              Key := LB_Fields.Items[I];
          end
          else
          if SameText(Key, Copy(LB_Fields.Items[I], Sep + 2,
            Length(LB_Fields.Items[I]))) then
            Key := LB_Fields.Items[I];
        end;
        Inc(DC);
        SetValues(DC, Key, not FSortedFields[0].Order);
      end;
      ShowForm := True;
    end
    else
    begin // DataSet unknown
      Application.MessageBox(lsDSNotSupported, lsError, MB_ICONWARNING);
      ShowForm := False;
    end;
    if ShowForm = True then
      if ShowModal = mrOk then
      begin
        {$IFDEF ADO}
        if DSet is TCustomADODataSet then
        begin // ADO Table or Query
          if SortFieldCount = 0 then
          begin
            FSortedFields := nil;
            TCustomADODataSet(DSet).Sort := '';
          end
          else
            Fill_FTS_And_Sort;
        end
        else
        {$ENDIF}
        {$IFDEF BDE}
        if DSet is TTable then
        begin // BDE Table
          if SortFieldCount = 0 then
          begin
            FSortedFields := nil;
            TTable(DSet).IndexName := '';
            TTable(DSet).IndexFieldNames := '';
          end
          else
          begin
            Sep := Pos('|', SortFields[1]);
            if Sep > 0 then
              Key := Copy(SortFields[1], Sep + 2, Length(SortFields[1]))
            else
              Key := SortFields[1];
            I := WordCount(Key, [';']);
            SetLength(FieldsToSort, I);
            for FTS := 1 to I do
              FieldsToSort[Pred(FTS)].Name := ExtractWord(FTS, Key, [';']);
            Sort(FieldsToSort);
          end;
        end
        else
        {$ENDIF}
        if DSet is TRxMemoryData then
        begin // Memory Table
          if SortFieldCount = 0 then
          begin
            FSortedFields := nil;
            TRxMemoryData(DSet).SortOnFields('');
            TRxMemoryData(DSet).First;
          end
          else
            Fill_FTS_And_Sort;
        end;
      end;
  finally
    FrmSort.Free;
  end;
end;

procedure TUltimDBGrid.MenuSetFilter;
var
  FrmFilter: TFUltimDBFilter;
  I, Sep: Integer;
  FilterField, ListField: string;
  Found: Boolean;
begin
  FrmFilter := TFUltimDBFilter.Create(Self);
  with FrmFilter do
  try
    Icon := TForm(GetParentForm(Self)).Icon;
    Color := GetParentForm(Self).Color;
    Caption := StringReplace(lsMenus[11], '&', '', []);
    LblField.Caption := lsField;
    LblFilter.Caption := lsFilterCond;
    LblORExpr.Caption := lsORCompleteExpr;
    LblORPreset.Caption := lsORPreset;
    B_Cancel.Caption := lsBtnCancel;

    // Filling the list of fields
    for I := 0 to Columns.Count - 1 do
      if not ((Columns[I].Field is TBlobField) or (Columns[I].Field is TBytesField)) then
        if Columns[I].Title.Caption = Columns[I].FieldName then
          CB_Field.Items.Add(Columns[I].FieldName)
        else
          CB_Field.Items.Add('"' + Columns[I].Title.Caption + '" | ' + Columns[I].FieldName);

    // Filling the list of predefined filters
    for I := 0 to Length(FilterList) - 1 do
      CB_Presets.Items.Add('"' + Trim(FilterList[I].Name) + '" | ' +
        Trim(FilterList[I].Filter));

    // Filling the values of the form
    if DataLink.DataSet.Filter <> '' then
    begin
      Filter := Trim(DataLink.DataSet.Filter);
      if Filter[1] = '[' then
      begin
        FilterField := ExtractWord(1, Filter, ['[', ']']);
        Filter := Trim(Copy(Filter, Length(FilterField) + 3, Length(Filter)));
      end
      else
      begin
        FilterField := ExtractWord(1, Filter, [' ']);
        Filter := Trim(Copy(Filter, Length(FilterField) + 1, Length(Filter)));
      end;
      Found := False;
      for I := 0 to CB_Field.Items.Count - 1 do
      begin
        Sep := Pos('|', CB_Field.Items[I]);
        if Sep > 0 then
          ListField := Copy(CB_Field.Items[I], Sep + 2, Length(CB_Field.Items[I]))
        else
          ListField := CB_Field.Items[I];
        if SameText(FilterField, ListField) then
        begin
          Found := True;
          CB_Field.ItemIndex := I;
          FilterCond.Text := Filter;
          Break;
        end;
      end;
      if not Found then
        ExprCompl.Text := Trim(DataLink.DataSet.Filter);
    end;

    // Editing/Selecting the filter
    if ShowModal = mrOk then
    begin
      DataLink.DataSet.Filter := Filter;
      DataLink.DataSet.Filtered := True;
    end;
  finally
    FrmFilter.Free;
  end;
end;

procedure TUltimDBGrid.MenuActivateFilter;
begin
  DataLink.DataSet.Filtered := not TMenuItem(Sender).Checked;
end;

procedure TUltimDBGrid.MenuAddFilter;
var
  Title, FilterName: string;
begin
  if (DataLink.DataSet <> nil) and (DataLink.DataSet.Filter <> '') then
  begin
    Title := StringReplace(TMenuItem(Sender).Caption, '&', '', []);
    Title := StringReplace(Title, '...', '', []);
    repeat
      if not InputQuery(Title, lsAskFilterName, FilterName) then
        Exit;
      FilterName := Trim(FilterName);
    until FilterName <> '';
    SetLength(FilterList, Length(FilterList) + 1);
    FilterList[Length(FilterList) - 1].Name := FilterName;
    FilterList[Length(FilterList) - 1].Filter := DataLink.DataSet.Filter;
  end;
end;

procedure TUltimDBGrid.MenuDisplayBoolean;
begin
  SetDisplayBoolean(not TMenuItem(Sender).Checked);
end;

procedure TUltimDBGrid.MenuDisplayImages;
begin
  SetDisplayImages(not TMenuItem(Sender).Checked);
end;

procedure TUltimDBGrid.MenuDisplayMemo;
begin
  SetDisplayMemo(not TMenuItem(Sender).Checked);
end;

procedure TUltimDBGrid.MenuFullSizeMemo;
begin
  SetFullSizeMemo(not TMenuItem(Sender).Checked);
end;

procedure TUltimDBGrid.MenuUseRowColors;
begin
  SetUseRowColors(not TMenuItem(Sender).Checked);
end;

procedure TUltimDBGrid.MenuSetCellHints;
begin
  SetCellHints(not TMenuItem(Sender).Checked);
end;

procedure TUltimDBGrid.MenuShowTextEllipsis;
begin
  SetShowTextEllipsis(not TMenuItem(Sender).Checked);
end;

procedure TUltimDBGrid.MenuSetMultiLines;
begin
  SetMultiLines(not TMenuItem(Sender).Checked);
end;

procedure TUltimDBGrid.MenuSetEnterAsTab;
begin
  FEnterAsTab := not TMenuItem(Sender).Checked;
end;

procedure TUltimDBGrid.MenuExportGrid;
var
  FrmExport: TFUltimDBExport;
begin
  FrmExport := TFUltimDBExport.Create(Self);
  with FrmExport do
  try
    Icon := TForm(GetParentForm(Self)).Icon;
    Color := GetParentForm(Self).Color;
    Caption := StringReplace(lsMenus[29], '&', '', []);
    Caption := StringReplace(Caption, '...', '', []);
    ExportFileName.FileName := ExtractFilePath(ParamStr(0)) + DefaultExportName;
    ExportFileName.DialogTitle := Caption;
    CB_Titles.Caption := lsWithTitles;
    LblDelim.Caption := lsDelimiter;
    E_Delim.Text := DefaultDelimiter;
    B_Cancel.Caption := lsBtnCancel;
    if ShowModal = mrOk then
    begin
      if RB_FormatHTML.Checked then
        ExportToFile(ExportFileName.FileName, efHTML, CB_Titles.Checked, DefaultDelimiter)
      else
      if RB_FormatSYLK.Checked then
        ExportToFile(ExportFileName.FileName, efSYLK, CB_Titles.Checked, DefaultDelimiter)
      else
        ExportToFile(ExportFileName.FileName, efText, CB_Titles.Checked, E_Delim.Text[1]);
    end;
  finally
    FrmExport.Free;
  end;
end;

procedure TUltimDBGrid.MenuPrintGrid;
begin
  if Assigned(FOnMenuPrintGrid) then
    FOnMenuPrintGrid(Self)
  {$IFDEF QREPORT}
  else
  begin
    SaveGridPosition;
    GetParentForm(Self).Enabled := False;
    try
      PreviewReport(Self, FReportTitle, RowHeights[0], DefaultRowHeight);
    finally
      GetParentForm(Self).Enabled := True;
      RestoreGridPosition;
    end;
  end;
  {$ENDIF};
end;

procedure TUltimDBGrid.MenuSetReadOnly;
begin
  ReadOnly := not TMenuItem(Sender).Checked;
  if ReadOnly and (DataLink <> nil) and DataLink.Active and
    (DataLink.DataSet.State in [dsEdit, dsInsert]) then
    DataLink.DataSet.Cancel;
end;

procedure TUltimDBGrid.MenuConfig;
var
  FrmConfig: TFUltimDBConfig;
begin
  FrmConfig := TFUltimDBConfig.Create(Self);
  with FrmConfig do
  try
    Icon := TForm(GetParentForm(Self)).Icon;
    Color := GetParentForm(Self).Color;
    Caption := StringReplace(TMenuItem(Sender).Caption, '&', '', []);
    Caption := StringReplace(Caption, '...', '', []);
    SaveForm := (TMenuItem(Sender).Tag = tagSAVE);
    INIFileName.FileName := ConfigFileName;
    INIFileName.DefaultExt := 'INI';
    INIFileName.Filter := lsINIFilter + '|' + lsDefaultFilter;
    INIFileName.DialogTitle := Caption;
    INIFileName.DirectInput := SaveForm;
    CB_Layout.Caption := lsCfgLayout;
    CB_Layout.Checked := (cfgLayout in ConfigOptions);
    CB_SortSettings.Caption := lsCfgSortSettings;
    CB_SortSettings.Checked := (cfgSortSettings in ConfigOptions);
    CB_Filters.Caption := lsCfgFilters;
    CB_Filters.Checked := (cfgFilters in ConfigOptions);
    B_Cancel.Caption := lsBtnCancel;
    if ShowModal = mrOk then
    begin
      // Saving the user's choices
      ConfigFileName := INIFileName.FileName;
      if CB_Layout.Checked then
        ConfigOptions := ConfigOptions + [cfgLayout]
      else
        ConfigOptions := ConfigOptions - [cfgLayout];
      if CB_SortSettings.Checked then
        ConfigOptions := ConfigOptions + [cfgSortSettings]
      else
        ConfigOptions := ConfigOptions - [cfgSortSettings];
      if CB_Filters.Checked then
        ConfigOptions := ConfigOptions + [cfgFilters]
      else
        ConfigOptions := ConfigOptions - [cfgFilters];

      // Save/Load
      if SaveForm then
        SaveGridConfig(ConfigFileName, CB_Layout.Checked, CB_SortSettings.Checked,
          CB_Filters.Checked)
      else
        RestoreGridConfig(ConfigFileName, CB_Layout.Checked, CB_SortSettings.Checked,
          CB_Filters.Checked);
    end;
  finally
    FrmConfig.Free;
  end;
end;

procedure TUltimDBGrid.RegisterFooter(Footer: TWinControl);
begin
  if Footer is TUltimDBFooter then
  begin
    FFooter := TUltimDBFooter(Footer);
    if (DataSource <> nil) and (DataSource.DataSet <> nil) and DataSource.DataSet.Active then
      TUltimDBFooter(FFooter).FooterMessage(FTR_ACTIVE);
  end
  else
    FFooter := nil;
end;

procedure TUltimDBGrid.UnregisterFooter(Footer: TWinControl);
begin
  FFooter := nil;
end;

procedure TUltimDBGrid.SendFooterMsg(Msg: Integer);
begin
  if FFooter <> nil then
    TUltimDBFooter(FFooter).FooterMessage(Msg);
end;

end.
