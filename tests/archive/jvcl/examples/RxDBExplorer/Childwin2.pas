{*******************************************************}
{                                                       }
{     Delphi VCL Extensions (RX) demo program           }
{                                                       }
{     Copyright (c) 1996 AO ROSNO                       }
{     Copyright (c) 1997, 1998 Master-Bank              }
{                                                       }
{*******************************************************}

{$I JVCL.INC}

{$IFNDEF COMPILER3_UP}
  { use Visual Query Builder in Delphi 2.x & C++Builder 1.x only }
  {$DEFINE USE_VQB}
{$ENDIF}

{.$DEFINE USE_QR2}  { use QuickReport 2.x }
{$IFDEF COMPILER3_UP}
  {$IFNDEF CBUILDER}
    {$DEFINE USE_QR2}
  {$ENDIF}
{$ENDIF}

{$IFDEF COMPILER4_UP}
  {$UNDEF USE_QR2}
{$ENDIF}

unit ChildWin2;

interface

uses WinTypes, WinProcs, Messages, Classes, Graphics, Forms, Controls, DB,
  JvDBLists, Tabs, ExtCtrls, JvSplit, DBTables, Grids, DBGrids, JvDBCtrl,
  JvQuery, StdCtrls, Buttons, JvPlacemnt, JvDBIndex, JvDBSecur, Menus, Dialogs,
  JvDBPrgrss, JvPicclip, ComCtrls, JvxAnimate, JvxCtrls
  {$IFDEF USE_QR2}, QuickRpt, QRPrntr, QRExtra, QRPrev, Printers,
  QRCtrls, JvComponent {$ENDIF USE_QR2};

type
  TTransOperation = (teStart, teCommit, teRollback);
  TTransSession = (tsTables, tsQuery);

  TMDIChild = class(TForm)
    TableList: TJvDatabaseItems ;
    DataSource1: TDataSource;
    TablesGrid: TJvDBGrid ;
    rxSplitter1: TJvxSplitter ;
    Panel1: TPanel;
    Notebook1: TNotebook;
    FieldList1: TJvTableItems ;
    DataSource2: TDataSource;
    Table1: TTable;
    rxDBGrid2: TJvDBGrid ;
    Panel2: TPanel;
    SQLMemo: TMemo;
    Panel3: TPanel;
    RunSQL: TJvSpeedButton ;
    Panel4: TPanel;
    Label1: TLabel;
    Panel5: TPanel;
    rxDBGrid3: TJvDBGrid ;
    Query1: TJvQuery ;
    TableListTABNAME: TStringField;
    TableListEXTENSION: TStringField;
    TableListTYPE: TStringField;
    FieldList1TYPE: TWordField;
    FieldList1SUBTYPE: TWordField;
    FieldList1UNITS1: TWordField;
    FieldList1UNITS2: TWordField;
    FieldList1LENGTH: TWordField;
    FieldList1TypeName: TStringField;
    FieldList1SubTypeName: TStringField;
    TableListPict: TBooleanField;
    FieldList1NAME: TStringField;
    FormStorage: TJvFormStorage ;
    rxSplitter2: TJvxSplitter ;
    Panel6: TPanel;
    Panel7: TPanel;
    DBIndexCombo1: TJvDBIndexCombo ;
    Label2: TLabel;
    PopupTablesMenu: TPopupMenu;
    FilterItem: TMenuItem;
    N1: TMenuItem;
    CloseItem: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    AbortQueryMenu: TPopupMenu;
    CancelItem: TMenuItem;
    TableListVIEW: TBooleanField;
    PriorSQL: TJvSpeedButton ;
    NextSQL: TJvSpeedButton ;
    PopupSQLMenu: TPopupMenu;
    Undo1: TMenuItem;
    N2: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    N3: TMenuItem;
    SelectAll1: TMenuItem;
    N4: TMenuItem;
    Saveas1: TMenuItem;
    Load1: TMenuItem;
    PriorSQLItem: TMenuItem;
    NextSQLItem: TMenuItem;
    Runquery1: TMenuItem;
    RefIntList: TJvTableItems ;
    RefIntListNAME: TStringField;
    RefIntListOTHERTABLE: TStringField;
    FieldList1Required: TBooleanField;
    CloseTableItem: TMenuItem;
    DBQueryProgress: TJvDBProgress ;
    RefIntListTYPE: TIntegerField;
    TableListNAME: TStringField;
    QuerySession: TSession;
    QueryDB: TDatabase;
    IndexList1: TJvTableItems ;
    IndexList1NAME: TStringField;
    IndexList1TAGNAME: TStringField;
    IndexList1UNIQUE: TBooleanField;
    TableListSYNONYM: TBooleanField;
    DbImages: TJvPicClip ;
    TableListDELETED: TBooleanField;
    Panel9: TPanel;
    TabSet1: TTabSet;
    QueryAnimation: TJvAnimatedImage ;
    Querybuilder1: TMenuItem;
    N5: TMenuItem;
    ShowDeletedItem: TMenuItem;
    OpenTableItem: TMenuItem;
    RefIntListFIELDCOUNT: TWordField;
    IndexList1FORMAT: TStringField;
    IndexList1PRIMARY: TBooleanField;
    FieldList1FIELDNUM: TWordField;
    QueryParamItem: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
    procedure TabSet1Change(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure TableListCalcFields(DataSet: TDataset);
    procedure RunSQLClick(Sender: TObject);
    procedure FieldListCalcFields(DataSet: TDataset);
    procedure TablesGridDrawDataCell(Sender: TObject; const Rect: TRect;
      Field: TField; State: TGridDrawState);
    procedure OpenTableClick(Sender: TObject);
    procedure TablesGridKeyPress(Sender: TObject; var Key: Char);
    procedure GridDblClick(Sender: TObject);
    procedure AfterPost(DataSet: TDataset);
    procedure CloseItemClick(Sender: TObject);
    procedure FilterItemClick(Sender: TObject);
    procedure PopupSQLMenuClick(Sender: TObject);
    procedure PopupSQLMenuPopup(Sender: TObject);
    procedure SQLMemoChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CancelQueryClick(Sender: TObject);
    procedure AfterOpen(DataSet: TDataset);
    procedure NavigateSQLClick(Sender: TObject);
    procedure FormStorageRestorePlacement(Sender: TObject);
    procedure FormStorageSavePlacement(Sender: TObject);
    procedure DataSource2StateChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CloseTableItemClick(Sender: TObject);
    procedure QueryAborting(DataSet: TDataSet; var AbortQuery: Boolean);
    procedure DBQueryProgressTrace(Sender: TObject; Flag: TTraceFlag;
      const Msg: string);
    procedure GridCheckButton(Sender: TObject; ACol: Longint;
      Field: TField; var Enabled: Boolean);
    procedure GridTitleBtnClick(Sender: TObject; ACol: Longint;
      Field: TField);
    procedure GridGetBtnParams(Sender: TObject; Field: TField;
      AFont: TFont; var Background: TColor; var SortMarker: TSortMarker;
      IsDown: Boolean);
    procedure ShowDeletedItemClick(Sender: TObject);
    procedure PopupTablesMenuPopup(Sender: TObject);
    procedure TabAfterClose(DataSet: TDataSet);
    procedure GridGetCellParams(Sender: TObject; Field: TField;
      AFont: TFont; var Background: TColor; Highlight: Boolean);
    procedure TableChange(Sender: TObject; Field: TField);
    procedure FormActivate(Sender: TObject);
    procedure TabBeforeDelete(DataSet: TDataSet);
    procedure RefIntListTYPEGetText(Sender: TField; var Text: string;
      DisplayText: Boolean);
    procedure DBQryProgress(Sender: TObject; var Abort: Boolean);
    procedure BeforeClose(DataSet: TDataSet);
  private
    { Private declarations }
    FSQLHistoryIndex: Integer;
    FSQLHistory: TStrings;
    FQueryRunning: Boolean;
    FQueryStartTime: Longint;
    FAbortQuery: Boolean;
    FDeletedList: TStrings;
    FShowDeleted: Boolean;
    FCurDeleted: Boolean;
    FTryOpenTable: Boolean; { for TUTIL32.DLL }
{$IFDEF USE_QR2}
    procedure PreviewReport(Sender: TObject);
{$ENDIF}
    function GetDatabaseName: string;
    function GetActiveDataSource: TDataSource;
    procedure SetDatabaseName(const Value: string);
    procedure CloseCurrent;
    procedure InternalOpenCurrent(const TabName: string);
    procedure UpdateFieldFormats(DataSet: TDataSet);
    procedure UpdateSQLHistory;
    procedure EnableSQLHistoryItems;
    procedure ExecSQL;
    procedure StartWatch;
    procedure StopWatch;
    procedure QueryThreadDone(Sender: TObject);
    procedure RunQueryBuilder;
  public
    { Public declarations }
    procedure CloseDatabase;
    procedure SetTrace(Value: Boolean);
    function CheckStandard: Boolean;
    procedure UpdateSystemTables;
    procedure UpdateDataFieldFormats;
    procedure UpdateThreadOptions;
    procedure SetToCurrentTable;
    procedure PackCurrentTable;
    procedure CheckAndRepairParadoxTable(AllTables: Boolean);
    procedure ExportCurrentTable;
    procedure PrintCurrentTable;
    procedure ImportToCurrentTable;
    procedure ReindexTable;
    function CurrentTable: TTable;
    procedure MarkAsDeleted(const TabName: string);
    function SessionDB(ASession: TTransSession): TDatabase;
    function TransOperEnabled(ASession: TTransSession;
      Operation: TTransOperation): Boolean;
    procedure StartTransaction(ASession: TTransSession);
    procedure Commit(ASession: TTransSession);
    procedure Rollback(ASession: TTransSession);
    procedure RefreshData;
    property DatabaseName: string read GetDatabaseName write SetDatabaseName;
    property DataSource: TDataSource read GetActiveDataSource;
  end;

implementation

{$B-}
{$R *.DFM}

uses SysUtils, Clipbrd, DBConsts, TUtil, JvVCLUtils, JvObjStr, Options, JvStrUtils,
  {$IFDEF USE_VQB} Qbe, {$ENDIF} Bde, SqlMon, JvFileUtil, JvAppUtils, EditStr,
  EditPict, ViewBlob, JvDBUtils, JvBdeUtils, Main, FiltDlg, DestTab, SrcTab,
  JvQbnddlg, BdeInfo;

const
  SQuerySuccess = 'Query successfully executed.';
  STimeElapsed = 'Time elapsed:';
  SNoRows = 'No rows selected.';
  SDatabase = 'Database: %s';
  SCommitted = 'Changes successfully committed to a database.';
  SSqlDatabase = 'Cannot perform this operation on a SQL database';
  SCheckComplete = 'Verification complete.';
  STabCreated = 'Table %s successfully created.';
  SQueryRunning = 'You cannot close database while query is running.';
  SUndeleteConfirm = 'Undelete current record?';
  SCommitConfirm = 'You have uncommitted changes in %s session. Commit changes to a database?';
  SMainSession = 'main';
  SQuerySession = 'query';
  SQueryHint = '%s: query running...|';
  SQueryAborting = '%s: query aborting...|';
{$IFDEF USE_VQB}
  SVqbNotLoaded = 'Could not load Visual Query Builder. Make sure that all required libraries are available';
{$ENDIF}
{$IFDEF USE_QR2}
  SPreview = 'Preview report';
  SClosePreview = 'You must close preview window before closing database.';
{$ENDIF}

{$WARNINGS OFF}

{ TQueryThread }

type
  TQueryThread = class(TThread)
  private
    FQuery: TJvQuery ;
    FExcept: Exception;
    procedure DoExcept;
  protected
    procedure Execute; override;
  public
    constructor Create(Query: TJvQuery );
  end;

constructor TQueryThread.Create(Query: TJvQuery );
begin
  inherited Create(False);
  FQuery := Query;
  FreeOnTerminate := True;
end;

procedure TQueryThread.DoExcept;
begin
  if not (FExcept is EAbort) then
    if Assigned(Application.OnException) then
      Application.OnException(FQuery, FExcept)
    else Application.ShowException(FExcept);
end;

procedure TQueryThread.Execute;
begin
  try
    FQuery.OpenOrExec(True);
  except
    on E: Exception do begin
      FExcept := E;
      Synchronize(DoExcept);
    end;
  end;
end;

{$IFDEF USE_QR2}

type
  TQRDataSetBuilder = class(TQRBuilder)
  private
    FDataSet : TDataSet;
  protected
    procedure SetActive(Value: Boolean); override;
    procedure BuildList;
  public
    property DataSet: TDataSet read FDataSet write FDataSet;
  end;

procedure TQRDataSetBuilder.SetActive(Value: Boolean);
begin
  if Value <> Active then begin
    if Value and Assigned(FDataSet) then begin
      inherited SetActive(True);
      BuildList;
    end
    else inherited SetActive(False);
  end;
end;

procedure TQRDataSetBuilder.BuildList;
var
  I: Integer;
  AField: TField;
  AData: TQRDBText;
  ALabel: TQRLabel;
  AHeight: Integer;
  HadDetail: Boolean;
  HadColHead: Boolean;

  procedure AddField(AField: TField);
  begin
    ALabel := TQRLabel(Report.Bands.ColumnHeaderBand.AddPrintable(TQRLabel));
    AHeight := ALabel.Height;
    ALabel.AutoSize := True;
    ALabel.Font.Style := [fsBold];
    ALabel.Caption := MakeStr('X', AField.DisplayWidth);
    ALabel.AutoSize := False;
    ALabel.Caption := AField.DisplayLabel;
    ALabel.Frame.DrawBottom := True;
    AData := TQRDBText(Report.Bands.DetailBand.AddPrintable(TQRDBText));
    AData.AutoSize := False;
    AData.DataSet := DataSet;
    AData.DataField := AField.FieldName;
    AData.Left := ALabel.Left;
    AData.Width := ALabel.Width;
    AData.Alignment := AField.Alignment;
    if (AData.Left + AData.Width > Report.Bands.DetailBand.Width) and
      (Orientation = poPortrait) then Orientation := poLandscape;
    if AData.Left + AData.Width > Report.Bands.DetailBand.Width then begin
      ALabel.Free;
      AData.Free;
    end;
  end;

begin
  HadDetail := Report.Bands.HasDetail;
  HadColHead := Report.Bands.HasColumnHeader;
  if not HadColHead then Report.Bands.HasColumnHeader := True;
  if not HadDetail then Report.Bands.HasDetail := True;
  AHeight := Round(Report.Bands.DetailBand.Height / 1.5);
  Report.DataSet := Self.DataSet;
  if DataSet <> nil then begin
    for I := 0 to DataSet.FieldCount - 1 do begin
      AField := DataSet.Fields[I];
      if AField.Visible and not (AField.DataType in
        [ftUnknown, ftBytes, ftVarBytes, ftBlob, ftMemo, ftGraphic,
        ftFmtMemo, ftParadoxOle, ftDBaseOle, ftTypedBinary]) then
        AddField(AField);
    end;
  end;
  if not HadDetail then
    Report.Bands.DetailBand.Height := Round(AHeight * 1.5);
  if not HadColHead then
    Report.Bands.ColumnHeaderBand.Height := Round(AHeight * 1.5);
  RenameObjects;
end;

{$ENDIF USE_QR2}

{ TMDIChild }

function TMDIChild.GetDatabaseName: string;
begin
  Result := TableList.DatabaseName;
end;

procedure TMDIChild.SetDatabaseName(const Value: string);
begin
  if Self.DatabaseName <> Value then begin
    TableList.Close;
    try
      TableList.DatabaseName := Value;
      TableList.SystemItems := SystemTables;
      Table1.DatabaseName := Value;
      Query1.DatabaseName := Value;
      FieldList1.DatabaseName := Value;
      IndexList1.DatabaseName := Value;
      RefIntList.DatabaseName := Value;
      TableList.Open;
      if Value <> '' then Caption := Format(SDatabase, [Value]);
    except
      Close;
      raise;
    end;
  end;
end;

procedure TMDIChild.RefreshData;
begin
  TableList.Close;
  try
    TableList.Open;
  except
    Close;
    raise;
  end;
end;

function TMDIChild.GetActiveDataSource: TDataSource;
begin
  Result := DataSource2;
end;

procedure TMDIChild.UpdateDataFieldFormats;
begin
  UpdateFieldFormats(Table1);
  UpdateFieldFormats(Query1);
  rxDBGrid2.Refresh;
  rxDBGrid3.Refresh;
end;

procedure TMDIChild.UpdateThreadOptions;
begin
  if QueryInThreads then begin
    if Query1.SessionName <> QuerySession.SessionName then begin
      Query1.Close;
      Query1.SessionName := QuerySession.SessionName;
      Query1.DatabaseName := QueryDB.DatabaseName;
    end;
  end
  else begin
    if Query1.SessionName = QuerySession.SessionName then begin
      Query1.Close;
      Query1.SessionName := ''; { default session }
      Query1.DatabaseName := Table1.DatabaseName;
    end;
  end;
end;

procedure TMDIChild.UpdateFieldFormats(DataSet: TDataSet);
var
  I: Integer;
begin
  for I := 0 to DataSet.FieldCount - 1 do begin
    case DataSet.Fields[I].DataType of
      ftFloat, ftCurrency, ftBCD:
        begin
          TNumericField(DataSet.Fields[I]).DisplayFormat := defFloatFormat;
          TNumericField(DataSet.Fields[I]).EditFormat := '#.##';
        end;
      ftDate: TDateTimeField(DataSet.Fields[I]).DisplayFormat := defDateFormat;
      ftTime: TDateTimeField(DataSet.Fields[I]).DisplayFormat := defTimeFormat;
      ftDateTime: TDateTimeField(DataSet.Fields[I]).DisplayFormat := defDateTimeFormat;
    end;
  end;
end;

procedure TMDIChild.UpdateSystemTables;
begin
  TableList.SystemItems := SystemTables;
end;

procedure TMDIChild.MarkAsDeleted(const TabName: string);
begin { mark current table as deleted }
  if TabName <> '' then begin
    if FDeletedList.IndexOf(TabName) < 0 then FDeletedList.Add(TabName);
    if TableList.Active then begin
      TableList.UpdateCursorPos;
      TableList.Resync([rmExact]);
    end;
  end;
end;

function TMDIChild.CurrentTable: TTable;
var
  Val: string;
begin
  if not TableList.Active then begin
    Result := nil;
    Exit;
  end;
  Val := TableListTABNAME.AsString;
  if Table1.Active then begin
    if Table1.TableName <> Val then SetToCurrentTable;
  end
  else begin
    Table1.TableName := Val;
  end;
  Result := Table1;
end;

function TMDIChild.CheckStandard: Boolean;
begin
  Result := False;
  if TableList.Database <> nil then
    Result := not TableList.Database.IsSQLBased;
end;

function TMDIChild.SessionDB(ASession: TTransSession): TDatabase;
begin
  case ASession of
    tsTables: Result := TableList.Database;
    tsQuery: Result := QueryDB;
  end;
end;

function TMDIChild.TransOperEnabled(ASession: TTransSession;
  Operation: TTransOperation): Boolean;
var
  InTransNow: Boolean;
  Db: TDatabase;
begin
  Result := False;
  Db := SessionDB(ASession);
  if Db <> nil then begin
    InTransNow := TransActive(Db);
    { Reading Database.InTransaction property causes change of current BDE session }
    case Operation of
      teStart: Result := not InTransNow;
      teCommit: Result := InTransNow;
      teRollback: Result := InTransNow;
    end;
  end;
end;

procedure TMDIChild.StartTransaction(ASession: TTransSession);
begin
  if TransOperEnabled(ASession, teStart) then
    with SessionDB(ASession) do begin
      if not IsSQLBased then TransIsolation := tiDirtyRead;
      StartTransaction;
    end;
  TDBExplorerMainForm(Application.MainForm).UpdateMenus;
end;

procedure TMDIChild.Commit(ASession: TTransSession);
begin
  if TransOperEnabled(ASession, teCommit) then
  try
    SessionDB(ASession).Commit;
    MessageDlg(SCommitted, mtInformation, [mbOk], 0);
  finally
    TDBExplorerMainForm(Application.MainForm).UpdateMenus;
  end;
end;

procedure TMDIChild.Rollback(ASession: TTransSession);
begin
  if TransOperEnabled(ASession, teRollback) then
  try
    SessionDB(ASession).Rollback;
  finally
    TDBExplorerMainForm(Application.MainForm).UpdateMenus;
  end;
end;

procedure TMDIChild.CheckAndRepairParadoxTable(AllTables: Boolean);
var
  KeepActive: Boolean;
  FullName: string;
begin
  if (not CheckStandard) or (not TableList.Active) then
    DatabaseError(SSqlDatabase);
  KeepActive := Table1.Active;
  if (not KeepActive) and (not FTryOpenTable) then begin
    Table1.DisableControls;
    try
      try
        SetToCurrentTable;
      except
        { ignore exceptions }
      end;
      CloseCurrent;
    finally
      Table1.EnableControls;
    end;
  end;
  CloseCurrent;
  if not FQueryRunning then Query1.Close;
  try
    if AllTables then begin
      CheckTables(DatabaseName, crConfirmRepair);
      MessageDlg(SCheckComplete, mtInformation, [mbOk], 0);
    end
    else begin
      FullName := DatabaseName;
      if not IsDirectory(FullName) then FullName := GetAliasPath(FullName);
      FullName := NormalDir(FullName) + TableListTABNAME.AsString;
      CheckTable(FullName, crConfirmRepair);
    end;
  finally
    if KeepActive then SetToCurrentTable;
  end;
end;

{$IFDEF USE_QR2}
function FindPreview(AOwner: TComponent): TForm;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do begin
    if (Screen.Forms[I] is TQRStandardPreview) and
      (Screen.Forms[I].Tag = Longint(Pointer(AOwner))) then
    begin
      Result := Screen.Forms[I];
      Exit;
    end;
  end;
end;

procedure TMDIChild.PreviewReport(Sender: TObject);
begin
  if not (Sender is TQRPrinter) then Exit;
  with TQRStandardPreview.CreatePreview(Application, TQRPrinter(Sender)) do
  begin
    Caption := SPreview;
    Show;
  end;
end;
{$ENDIF USE_QR2}

procedure TMDIChild.PrintCurrentTable;
{$IFDEF USE_QR2}
var
  F: TForm;
{$ENDIF USE_QR2}
begin
{$IFDEF USE_QR2}
  if (DataSource.DataSet <> nil) then begin
    if DataSource.DataSet.Active then DataSource.DataSet.CheckBrowseMode
    else _DBError(SDataSetClosed);
    F := FindPreview(Self);
    if F <> nil then F.Close;
    with TQRDataSetBuilder.Create(Self) do
    try
      DataSet := DataSource.DataSet;
      Active := True;
      Title := 'Report';
      Report.OnPreview := PreviewReport;
      Report.Preview;
    finally
      Free;
    end;
  end;
{$ELSE}
  NotImplemented;
{$ENDIF USE_QR2}
end;

procedure TMDIChild.ExportCurrentTable;
var
  DestName: string;
  TabType: TTableType;
  RecCount: Longint;
  DestTable: TTable;
begin
  if (DataSource.DataSet <> nil) then begin
    if DataSource.DataSet.Active then DataSource.DataSet.CheckBrowseMode;
    if (DataSource.DataSet is TTable) then begin
      DestName := ExtractFileName(TTable(DataSource.DataSet).TableName);
      if not CheckStandard then begin
        if Pos('.', DestName) > 0 then
          DestName := Copy(DestName, Pos('.', DestName) + 1, MaxInt);
        if DestName = '' then DestName := '$table';
      end;
    end
    else begin
      if not DataSource.DataSet.Active then _DBError(SDataSetClosed);
      DestName := 'Query';
    end;
  end;
  TabType := ttDefault;
  RecCount := 0;
  if not GetDestTable(DestName, TabType, RecCount) then Exit;
  Update;
  DestTable := TTable.Create(Self);
  try
    DestTable.TableName := DestName;
    ExportDataSet(DataSource.DataSet as TBDEDataSet, DestTable, TabType,
      ASCIICharSet, ASCIIDelimited, RecCount);
    MessageDlg(Format(STabCreated, [DestTable.TableName]),
      mtInformation, [mbOk], 0);
  finally
    DestTable.Free;
  end;
end;

procedure TMDIChild.ImportToCurrentTable;
var
  DestTable: TTable;
  SrcName: string;
  MaxRecCnt: Longint;
  BatchMode: TBatchMode;
  Mappings: TStrings;
  SrcTable: TTable;
begin
  DestTable := CurrentTable;
  if DestTable <> nil then begin
    Mappings := TStringList.Create;
    DestTable.DisableControls;
    try
      if GetImportParams(DestTable, SrcName, MaxRecCnt, Mappings,
        BatchMode) then
      begin
        SrcTable := TTable.Create(Self);
        try
          SrcTable.TableName := SrcName;
          ImportDataSet(SrcTable, DestTable, MaxRecCnt, Mappings, BatchMode);
        finally
          SrcTable.Free;
        end;
      end;
    finally
      Mappings.Free;
      DestTable.EnableControls;
    end;
  end;
end;

procedure TMDIChild.InternalOpenCurrent(const TabName: string);
var
  I: Integer;
begin
  FieldList1.TableName := TabName;
  IndexList1.TableName := TabName;
  RefIntList.TableName := TabName;
  try
    if not Table1.Active then Table1.TableName := TabName;
    FTryOpenTable := True;
    try
      Table1.Open;
    except
      on E: EDBEngineError do begin
        if E.Errors[0].ErrorCode = DBIERR_NOSUCHTABLE then
          MarkAsDeleted(TabName);
        raise;
      end;
      else raise;
    end;
    I := FDeletedList.IndexOf(TabName);
    if I >= 0 then begin
      FDeletedList.Delete(I);
      TableList.UpdateCursorPos;
      TableList.Resync([rmExact]);
    end;
    FieldList1.Open;
    IndexList1.Open;
    if DataSource2.DataSet = RefIntList then RefIntList.Open;
  except
    CloseCurrent;
    raise;
  end;
end;

procedure TMDIChild.ReindexTable;
var
  Val: string;
begin
  if DataSource.DataSet = nil then Exit;
  StartWait;
  DataSource.DataSet.DisableControls;
  try
    CloseCurrent;
    if TableList.Active then begin
      Val := TableListTABNAME.AsString;
      if Table1.TableName <> Val then Table1.TableName := Val;
      if Val <> '' then
      try
        JvBdeUtils.ReindexTable(Table1);
      finally
        InternalOpenCurrent(Val);
      end;
    end;
  finally
    DataSource.DataSet.EnableControls;
    StopWait;
  end;
end;

procedure TMDIChild.PackCurrentTable;
var
  Val: string;
begin
  StartWait;
  DataSource.DataSet.DisableControls;
  try
    CloseCurrent;
    if TableList.Active then begin
      Val := TableListTABNAME.AsString;
      if Table1.TableName <> Val then Table1.TableName := Val;
      if Val <> '' then begin
        Table1.Open;
        try
          PackTable(Table1);
        except
          Application.HandleException(Self);
        end;
        InternalOpenCurrent(Val);
      end;
    end;
  finally
    DataSource.DataSet.EnableControls;
    StopWait;
  end;
end;

procedure TMDIChild.CloseCurrent;
begin
  Table1.Close;
  FieldList1.Close;
  IndexList1.Close;
  RefIntList.Close;
end;

procedure TMDIChild.SetToCurrentTable;
var
  Val: string;
begin
  if DataSource.DataSet <> nil then
    DataSource.DataSet.DisableControls;
  StartWait;
  try
    CloseCurrent;
    if TableList.Active then begin
      Val := TableListTABNAME.AsString;
      if Table1.TableName <> Val then Table1.TableName := Val;
      if Val <> '' then InternalOpenCurrent(Val);
    end;
  finally
    StopWait;
    if DataSource.DataSet <> nil then
      DataSource.DataSet.EnableControls;
  end;
end;

procedure TMDIChild.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TMDIChild.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  if AutoActivate then SetToCurrentTable;
end;

procedure TMDIChild.TabSet1Change(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
var
  KeepPage: Integer;
  KeepDS: TDataSet;
begin
  KeepPage := Notebook1.PageIndex;
  KeepDS := DataSource2.DataSet;
  try
    case NewTab of
      0: begin
           Notebook1.PageIndex := 0;
           DataSource2.DataSet := Table1;
         end;
      1: begin
           Notebook1.PageIndex := 0;
           DataSource2.DataSet := FieldList1;
         end;
      2: begin
           Notebook1.PageIndex := 0;
           DataSource2.DataSet := IndexList1;
         end;
      3: begin
           Notebook1.PageIndex := 0;
           if (RefIntList.TableName = Table1.TableName) and Table1.Active then
           begin
             StartWait;
             try
               RefIntList.Open;
             finally
               StopWait;
             end;
           end;
           DataSource2.DataSet := RefIntList;
         end;
      4: begin
           Notebook1.PageIndex := 1;
           if not FQueryRunning then DataSource2.DataSet := Query1
           else DataSource2.DataSet := nil;
         end;
    end;
  except
    AllowChange := False;
    Notebook1.PageIndex := KeepPage;
    DataSource2.DataSet := KeepDS;
    raise;
  end;
end;

procedure TMDIChild.TableListCalcFields(DataSet: TDataset);
begin
  TableListTABNAME.AsString := TableList.ItemName;
  TableListDELETED.AsBoolean :=
    FDeletedList.IndexOf(TableListTABNAME.AsString) >= 0;
end;

procedure TMDIChild.RefIntListTYPEGetText(Sender: TField; var Text: string;
  DisplayText: Boolean);
begin
  case RINTType(Sender.AsInteger) of
    rintMASTER: Text := 'Master';
    rintDEPENDENT: Text := 'Dependent';
    else Text := '';
  end;
end;

procedure TMDIChild.StartWatch;
begin
  if FQueryRunning then SysUtils.Abort;
  FQueryStartTime := GetTickCount;
end;

procedure TMDIChild.StopWatch;
var
  H, M, S, MS: Longint;
begin
  if (Query1.OpenStatus in [qsExecuted, qsOpened]) and
    (FQueryStartTime > 0) then
  begin
    MS := GetTickCount - FQueryStartTime;
    S := MS div 1000;
    MS := MS - (1000 * S);
    M := S div 60;
    S := S - (M * 60);
    H := M div 60;
    M := M - (H * 60);
    FQueryStartTime := 0;
    Application.Restore;
    Application.BringToFront;
    if (M > 0) or (H > 0) then
      MessageDlg(Format('%s %s %d:%d:%d.', [SQuerySuccess, STimeElapsed,
        H, M, S]), mtInformation, [mbOk], 0)
    else
      MessageDlg(Format('%s %s %d:%d:%d.%.3d.', [SQuerySuccess, STimeElapsed,
        H, M, S, MS]), mtInformation, [mbOk], 0);
  end;
end;

procedure TMDIChild.ExecSQL;
begin
  StartWatch;
  StartWait;
  try
    if QueryInThreads then begin
      RunQuery1.Enabled := False;
      RunSQL.Enabled := False;
      FAbortQuery := False;
      FQueryRunning := True;
      with TQueryThread.Create(Query1) do OnTerminate := QueryThreadDone;
      DataSource2.DataSet := nil;
      CancelItem.Enabled := False;
      QueryAnimation.GlyphNum := 0;
      QueryAnimation.Hint := Format(SQueryHint, [DatabaseName]);
      QueryAnimation.Visible := True;
      QueryAnimation.Active := True;
      AbortQueryMenu.AutoPopup := AsyncQrySupported(QueryDB);
    end
    else Query1.OpenOrExec(True);
  finally
    StopWait;
  end;
  if not QueryInThreads then begin
    Application.ProcessMessages;
    if ShowExecTime then StopWatch
    else if (Query1.OpenStatus = qsExecuted) then begin
      MessageDlg(SQuerySuccess, mtInformation, [mbOk], 0);
    end
    else if (Query1.OpenStatus = qsOpened) and IsDataSetEmpty(Query1) then
    begin
      MessageDlg(SNoRows, mtInformation, [mbOk], 0);
    end;
  end;
end;

procedure TMDIChild.QueryThreadDone(Sender: TObject);
begin
  FQueryRunning := False;
  QueryAnimation.Active := False;
  QueryAnimation.Visible := False;
  FAbortQuery := False;
  CancelItem.Enabled := False;
  SQLMemoChange(nil);
  if DataSource2.DataSet = nil then DataSource2.DataSet := Query1;
  if Query1.OpenStatus in [qsExecuted, qsOpened] then MessageBeep(0);
  if ShowExecTime then
    StopWatch
  else if (Query1.OpenStatus = qsExecuted) or
    ((Query1.OpenStatus = qsOpened) and ((Notebook1.PageIndex <> 1) or
    (Application.MainForm.ActiveMDIChild <> Self))) then
  begin
    Application.ProcessMessages;
    MessageDlg(SQuerySuccess,  mtInformation, [mbOk], 0);
  end;
  FQueryStartTime := 0;
end;

procedure TMDIChild.RunQueryBuilder;
begin
{$IFDEF USE_VQB}
  if not VQBLoadAttempted and not VQBLoaded then begin
    StartWait;
    try
      InitVQB;
    finally
      StopWait;
    end;
  end;
  if VQBLoaded then begin
    ExecBuilder(Query1);
    SQLMemo.Lines := Query1.SQL;
    SQLMemo.Modified := True;
    UpdateSQLHistory;
  end
  else DatabaseError(SVqbNotLoaded);
{$ELSE}
  NotImplemented;
{$ENDIF}
end;

procedure TMDIChild.QueryAborting(DataSet: TDataSet; var AbortQuery: Boolean);
begin
  if (DataSet = Query1) and EnableQueryAbort then begin
    CancelItem.Enabled := not FAbortQuery;
    AbortQuery := FAbortQuery;
  end;
end;

procedure TMDIChild.DBQryProgress(Sender: TObject; var Abort: Boolean);
begin
  if FQueryRunning and EnableQueryAbort then begin
    CancelItem.Enabled := not FAbortQuery;
    Abort := FAbortQuery;
  end;
end;

procedure TMDIChild.CancelQueryClick(Sender: TObject); { for 32-bit only }
begin
  if FQueryRunning then begin
    FAbortQuery := True;
    QueryAnimation.Hint := Format(SQueryAborting, [DatabaseName]);
  end;
  CancelItem.Enabled := False;
end;

procedure TMDIChild.RunSQLClick(Sender: TObject);
begin
  if FQueryRunning then Exit;
  Query1.Close;
  if Query1.SQL.Text <> SQLMemo.Lines.Text + #13#10 then begin
    Query1.SQL.Text := SQLMemo.Lines.Text + #13#10;
  end;
  if SQLMemo.Lines.Count = 0 then Exit;
  Query1.RequestLive := LiveQueries;
  {Query1.Params.Clear;} {!!!???}
  Query1.Macros.Clear;
  Query1.Unprepare;
  UpdateSQLHistory;
  ExecSQL;
end;

procedure TMDIChild.UpdateSQLHistory;
begin
  if (SQLMemo.Modified) and (SQLMemo.Lines.Count > 0) then begin
    while FSQLHistory.Count >= SQLHistoryCapacity do
      if FSQLHistory.Count > 0 then FSQLHistory.Delete(0);
    if (SQLHistoryCapacity > 0) then begin
      FSQLHistoryIndex := FSQLHistory.AddObject('',
        TStringList.Create);
      TStrings(FSQLHistory.Objects[FSQLHistoryIndex]).Assign(SQLMemo.Lines);
      SQLMemo.Modified := False;
    end;
  end;
  EnableSQLHistoryItems;
end;

procedure TMDIChild.EnableSQLHistoryItems;
begin
  PriorSQL.Enabled := ((FSQLHistoryIndex > 0) or (FSQLHistoryIndex = -1)) and
    (FSQLHistory.Count > 0);
  PriorSQLItem.Enabled := PriorSQL.Enabled;
  NextSQL.Enabled := (FSQLHistoryIndex <> -1);
  NextSQLItem.Enabled := NextSQL.Enabled;
end;

procedure TMDIChild.FieldListCalcFields(DataSet: TDataset);
var
  F: TField;
begin
  FieldList1TypeName.AsString := FieldTypeName(FieldList1TYPE.AsInteger);
  FieldList1SubTypeName.AsString := FieldSubtypeName(FieldList1SUBTYPE.AsInteger);
  F := Table1.FindField(FieldList1NAME.AsString);
  if F <> nil then FieldList1Required.AsBoolean := (F.Tag = 2) or F.Required;
end;

procedure TMDIChild.TablesGridDrawDataCell(Sender: TObject;
  const Rect: TRect; Field: TField; State: TGridDrawState);
var
  I: Integer;
begin
  if Field.FieldName = 'Pict' then begin
    if TableListVIEW.AsBoolean then I := 1 else I := 0;
    if TableListDELETED.AsBoolean then I := 4;
    DbImages.DrawCenter(TablesGrid.Canvas, Rect, I);
  end;
end;

procedure TMDIChild.OpenTableClick(Sender: TObject);
begin
  SetToCurrentTable;
end;

procedure TMDIChild.TablesGridKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Char(VK_RETURN)) then SetToCurrentTable;
end;

procedure TMDIChild.GridDblClick(Sender: TObject);
var
  F: TField;
begin
  if GetActiveDataSource.State in [dsBrowse, dsEdit, dsInsert] then begin
    F := (Sender as TJvDBGrid ).SelectedField;
    if F = nil then Exit;
    if (F.DataType in [ftMemo]) then
      StrListEdit(GetActiveDataSource.DataSet, F.FieldName)
    else if (F.DataType in [ftGraphic]) then
      PictureEdit(GetActiveDataSource.DataSet, F.FieldName)
    else if (F.DataType in [ftBlob..ftTypedBinary]) then
      BlobView(GetActiveDataSource.DataSet, F.FieldName);
    (Sender as TJvDBGrid ).Update;
  end;
end;

procedure TMDIChild.AfterPost(DataSet: TDataset);
begin
  try
    DataSet.Refresh;
  except
  end;
end;

procedure TMDIChild.CloseItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMDIChild.FilterItemClick(Sender: TObject);
var
  TabMask: string;
  P: TPoint;
begin
  TabMask := TableList.FileMask;
  P.X := TablesGrid.Left + 25;
  P.Y := TablesGrid.Top + 25;
  P := ClientToScreen(P);
  if ShowFilterDialog(TabMask, P.X, P.Y) then
    TableList.FileMask := TabMask;
end;

procedure TMDIChild.PopupSQLMenuClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    1: if SQLMemo.Perform(EM_CANUNDO, 0, 0) <> 0 then
         SQLMemo.Perform(EM_UNDO, 0, 0);
    2: SQLMemo.CutToClipboard;
    3: SQLMemo.CopyToClipboard;
    4: SQLMemo.PasteFromClipboard;
    5: SQLMemo.SelectAll;
    6: if SaveDialog1.Execute then begin
         SaveDialog1.InitialDir := ExtractFilePath(SaveDialog1.FileName);
         SQLMemo.Lines.SaveToFile(SaveDialog1.FileName);
       end;
    7: if OpenDialog1.Execute then begin
         OpenDialog1.InitialDir := ExtractFilePath(OpenDialog1.FileName);
         SQLMemo.Lines.LoadFromFile(OpenDialog1.FileName);
         SQLMemo.Modified := True;
         UpdateSQLHistory;
       end;
    8: RunSQLClick(Sender);
    9: NavigateSQLClick(PriorSQL);
   10: NavigateSQLClick(NextSQL);
   11: RunQueryBuilder;
   12: if not FQueryRunning and (SQLMemo.Lines.Count > 0) then begin
         { parameters }
         if Query1.SQL.Text <> SQLMemo.Lines.Text + #13#10 then begin
           Query1.Close;
           Query1.SQL.Text := SQLMemo.Lines.Text + #13#10;
         end;
         EditQueryParams(Query1, Query1.Params, 0);
       end;
  end;
end;

procedure TMDIChild.PopupSQLMenuPopup(Sender: TObject);
var
  EnableCopy: Boolean;
begin
  EnableCopy := SQLMemo.SelLength <> 0;
  Undo1.Enabled := (SQLMemo.Perform(EM_CANUNDO, 0, 0) <> 0);
  Cut1.Enabled := EnableCopy;
  Copy1.Enabled := EnableCopy;
  Paste1.Enabled := Clipboard.HasFormat(CF_TEXT);
  SelectAll1.Enabled := SQLMemo.Lines.Count > 0;
  Saveas1.Enabled := SQLMemo.Lines.Count > 0;
  Runquery1.Enabled := (SQLMemo.Lines.Count > 0) and not FQueryRunning;
  QueryParamItem.Enabled := Runquery1.Enabled;
  EnableSQLHistoryItems;
end;

procedure TMDIChild.SQLMemoChange(Sender: TObject);
begin
  RunSQL.Enabled := (SQLMemo.Lines.Count > 0) and not FQueryRunning;
  Runquery1.Enabled := (SQLMemo.Lines.Count > 0) and not FQueryRunning;
  QueryParamItem.Enabled := (SQLMemo.Lines.Count > 0) and not FQueryRunning;
end;

procedure TMDIChild.SetTrace(Value: Boolean);
begin
  DBQueryProgress.TraceFlags := SQLTraceFlags;
  DBQueryProgress.Trace := Value;
end;

procedure TMDIChild.CloseDatabase;
var
  TempDatabase: TDatabase;
begin
  CloseCurrent;
  Query1.Close;
  TableList.Close;
  TempDatabase := Session.FindDatabase(DatabaseName);
  if TempDatabase <> nil then
    TempDatabase.Session.CloseDatabase(TempDatabase);
end;

procedure TMDIChild.FormCreate(Sender: TObject);
begin
  FSQLHistoryIndex := -1;
  FSQLHistory := TJvObjectStrings .Create;
  FDeletedList := TStringList.Create;
  TStringList(FDeletedList).Sorted := True;
  Notebook1.PageIndex := 0;
  FTryOpenTable := False;
  FQueryRunning := False;
  EnableSQLHistoryItems;
  QueryAnimation.Parent := DBExplorerMainForm.StatusLine;
{$IFNDEF COMPILER4_UP}
  Query1.OnServerYield := QueryAborting;
{$ENDIF}
end;

procedure TMDIChild.FormDestroy(Sender: TObject);
begin
  CloseDatabase;
  FSQLHistory.Free;
  FSQLHistory := nil;
  FDeletedList.Free;
  FDeletedList := nil;
end;

procedure TMDIChild.AfterOpen(DataSet: TDataset);
var
  I: Integer;
begin
  UpdateFieldFormats(DataSet);
  for I := 0 to DataSet.FieldCount - 1 do
    if DataSet.Fields[I].Required then begin
      DataSet.Fields[I].Required := False;
      DataSet.Fields[I].Tag := 2;
    end;
end;

procedure TMDIChild.NavigateSQLClick(Sender: TObject);
var
  NewSQL: Boolean;
begin
  if (FSQLHistory = nil) or (FSQLHistory.Count = 0) then Exit;
  NewSQL := False;
  if Sender = PriorSQL then begin
    if FSQLHistoryIndex > 0 then Dec(FSQLHistoryIndex)
    else if FSQLHistoryIndex = -1 then begin
      UpdateSQLHistory;
      FSQLHistoryIndex := FSQLHistory.Count - 1;
    end;
  end
  else if Sender = NextSQL then begin
    if FSQLHistoryIndex = -1 then UpdateSQLHistory;
    if FSQLHistoryIndex < FSQLHistory.Count - 1 then
      Inc(FSQLHistoryIndex)
    else begin
      NewSQL := True;
    end;
  end;
  if NewSQL then begin
    FSQLHistoryIndex := -1;
    SQLMemo.Clear;
    SQLMemo.Modified := False;
  end
  else begin
    SQLMemo.Lines.Assign(TStrings(FSQLHistory.Objects[FSQLHistoryIndex]));
    SQLMemo.Modified := False;
  end;
  EnableSQLHistoryItems;
end;

procedure TMDIChild.FormStorageRestorePlacement(Sender: TObject);
begin
  RestoreFields(FieldList1, FormStorage.IniFile, False);
  RestoreFields(IndexList1, FormStorage.IniFile, False);
  RestoreFields(RefIntList, FormStorage.IniFile, False);
end;

procedure TMDIChild.FormStorageSavePlacement(Sender: TObject);
begin
  SaveFields(FieldList1, FormStorage.IniFile);
  SaveFields(IndexList1, FormStorage.IniFile);
  SaveFields(RefIntList, FormStorage.IniFile);
end;

procedure TMDIChild.DataSource2StateChange(Sender: TObject);
var
  CanEdit: Boolean;
begin
  CanEdit := (DataSource2.DataSet <> nil) and DataSource2.DataSet.CanModify;
  with rxDBGrid2 do begin
    ReadOnly := not CanEdit;
  end;
  with rxDBGrid3 do begin
    ReadOnly := not CanEdit;
  end;
end;

procedure TMDIChild.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
{$IFDEF USE_QR2}
var
  F: TForm;
{$ENDIF}
begin
  if FQueryRunning then MessageDlg(SQueryRunning, mtWarning, [mbOk], 0);
  CanClose := not FQueryRunning;
  if CanClose then begin
    TDBExplorerMainForm(Application.MainForm).ClosedDatabases.Add(DatabaseName, 0);
    if TransOperEnabled(tsTables, teCommit) then begin
      case MessageDlg(Format(SCommitConfirm, [SMainSession]), mtWarning,
        mbYesNoCancel, 0) of
        mrYes: Commit(tsTables);
        mrNo: Rollback(tsTables);
        mrCancel: CanClose := False;
      end;
    end;
    if CanClose and TransOperEnabled(tsQuery, teCommit) then begin
      case MessageDlg(Format(SCommitConfirm, [SQuerySession]), mtWarning,
        mbYesNoCancel, 0) of
        mrYes: Commit(tsQuery);
        mrNo: Rollback(tsQuery);
        mrCancel: CanClose := False;
      end;
    end;
{$IFDEF USE_QR2}
    if CanClose then begin
      F := FindPreview(Self);
      if F <> nil then begin
        MessageDlg(SClosePreview, mtWarning, [mbOk], 0);
        CanClose := False;
        F.BringToFront;
        {F.Close;}
      end;
    end;
{$ENDIF}
  end;
end;

procedure TMDIChild.CloseTableItemClick(Sender: TObject);
begin
  CloseCurrent;
end;

procedure TMDIChild.DBQueryProgressTrace(Sender: TObject; Flag: TTraceFlag;
  const Msg: string);
begin
  BufAddLine(Msg);
end;

procedure TMDIChild.GridCheckButton(Sender: TObject; ACol: Longint;
  Field: TField; var Enabled: Boolean);
begin
  Enabled := (TJvDBGrid (Sender).DataSource.DataSet is TTable) and
    (Field <> nil) and not (Field is TBlobField) and
    (TTable(TJvDBGrid (Sender).DataSource.DataSet).IndexDefs.Count > 0);
end;

procedure TMDIChild.GridTitleBtnClick(Sender: TObject; ACol: Longint;
  Field: TField);
begin
  if TJvDBGrid (Sender).DataSource.DataSet is TTable then
  try
    TTable(TJvDBGrid (Sender).DataSource.DataSet).IndexFieldNames :=
      Field.FieldName;
  except
    TTable(TJvDBGrid (Sender).DataSource.DataSet).IndexFieldNames := '';
  end;
end;

procedure TMDIChild.GridGetBtnParams(Sender: TObject; Field: TField;
  AFont: TFont; var Background: TColor; var SortMarker: TSortMarker;
  IsDown: Boolean);
begin
  if (TJvDBGrid (Sender).DataSource.DataSet is TTable) and (Field <> nil) and
    (Field.IsIndexField) then
  begin
    SortMarker := smDown;
  end;
end;

procedure TMDIChild.ShowDeletedItemClick(Sender: TObject);
var
  Tab: TTable;
begin
  Tab := CurrentTable;
  if (Tab <> nil) and Tab.Active then
    DataSetShowDeleted(Tab, not FShowDeleted);
  FShowDeleted := not FShowDeleted;
end;

procedure TMDIChild.PopupTablesMenuPopup(Sender: TObject);
var
  IsCurrent: Boolean;
begin
  CloseTableItem.Enabled := Table1.Active;
  OpenTableItem.Enabled := not Table1.Active;
  IsCurrent := TableList.Active and Table1.Active and
    (TableListTABNAME.AsString = Table1.TableName);
  ShowDeletedItem.Enabled := IsCurrent;
  ShowDeletedItem.Checked := ShowDeletedItem.Enabled and FShowDeleted;
end;

procedure TMDIChild.TabAfterClose(DataSet: TDataSet);
begin
  FShowDeleted := False;
  with Table1 do begin
    IndexFieldNames := '';
    IndexName := '';
    IndexFiles.Clear;
    FieldDefs.Clear;
  end;
end;

procedure TMDIChild.GridGetCellParams(Sender: TObject; Field: TField;
  AFont: TFont; var Background: TColor; Highlight: Boolean);
begin
  if FShowDeleted and not Highlight and CurrentRecordDeleted(Table1) then
    AFont.Color := clGrayText;
end;

procedure TMDIChild.TableChange(Sender: TObject; Field: TField);
begin
  FCurDeleted := FShowDeleted and CurrentRecordDeleted(Table1);
  TDBExplorerMainForm(Application.MainForm).DBNavigator.ConfirmDelete :=
    not FCurDeleted;
  if FCurDeleted then
    rxDBGrid2.Options := rxDBGrid2.Options - [dgConfirmDelete]
  else
    rxDBGrid2.Options := rxDBGrid2.Options + [dgConfirmDelete];
end;

procedure TMDIChild.FormActivate(Sender: TObject);
begin
  TableChange(Sender, nil);
end;

procedure TMDIChild.TabBeforeDelete(DataSet: TDataSet);
begin
  if FShowDeleted and not (dgConfirmDelete in rxDBGrid2.Options) and
    CurrentRecordDeleted(Table1) then
  begin
    if MessageDlg(SUndeleteConfirm, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      Table1.GetCurrentRecord(nil);
      Check(DbiUndeleteRecord(Table1.Handle));
      Table1.Refresh;
    end;
    SysUtils.Abort;
  end;
end;

procedure TMDIChild.BeforeClose(DataSet: TDataSet);
{$IFDEF USE_QR2}
var
  F: TForm;
{$ENDIF USE_QR2}
begin
{$IFDEF USE_QR2}
  F := FindPreview(Self);
  if F <> nil then F.Close;
{$ENDIF}
end;

end.
