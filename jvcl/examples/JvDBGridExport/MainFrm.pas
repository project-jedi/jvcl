unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, Db, Grids, DBGrids, ComCtrls,
  JvComponent, JvDBGridExport, JvCsvData,
  JvBaseDlg, JvProgressDialog, JvDBGrid, JvExDBGrids;

type
  TfrmMain = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Export1: TMenuItem;
    SaveDialog1: TSaveDialog;
    DataSource1: TDataSource;
    DBGrid1: TJvDBGrid;
    JvProgressDialog1: TJvProgressDialog;
    Options1: TMenuItem;
    mnuOpenFile: TMenuItem;
    N1: TMenuItem;
    Getdata1: TMenuItem;
    Exit1: TMenuItem;
    N2: TMenuItem;
    Cleartable1: TMenuItem;
    StatusBar1: TStatusBar;
    procedure Export1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mnuOpenFileClick(Sender: TObject);
    procedure Getdata1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Cleartable1Click(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
    procedure StatusBar1Resize(Sender: TObject);
    procedure SaveDialog1TypeChange(Sender: TObject);
    procedure DBGrid1TitleClick(Column: TColumn);
    procedure DBGrid1GetBtnParams(Sender: TObject; Field: TField;
      AFont: TFont; var Background: TColor; var SortMarker: TSortMarker;
      IsDown: Boolean);
  private
    Data: TJvCsvDataSet;
    Ascending:boolean;
    procedure DoExportProgress(Sender: TObject; Min, Max, Position: Cardinal; const AText: string;
      var AContinue: Boolean);
    procedure SetupData;
    procedure SaveDoc(AExportClass: TJvCustomDBGridExportClass;
      const Filename: string);
  public
  end;

var
  frmMain: TfrmMain;

implementation

uses
  ShellAPI, ShlObj, CommDlg, Dlgs,
  JvTypes, JvJVCLUtils, JvJCLUtils;

{$R *.dfm}

procedure TfrmMain.SaveDoc(AExportClass: TJvCustomDBGridExportClass; const Filename: string);
var
  AExporter: TJvCustomDBGridExport;
begin
  AExporter := AExportClass.Create(self);
  try
    AExporter.Grid := DBGrid1;
    if AExporter is TJvDBGridCSVExport then
      TJvDBGridCSVExport(AExporter).ExportSeparator := esComma; // this to be compatible with JvCsvData
    AExporter.Filename := Filename;
    AExporter.OnProgress := DoExportProgress;
    JvProgressDialog1.Caption := AExporter.Caption;
    JvProgressDialog1.Show;
    AExporter.ExportGrid;
  finally
    AExporter.Free;
  end;
end;

procedure TfrmMain.Export1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    case SaveDialog1.FilterIndex of
      1: SaveDoc(TJvDBGridWordExport, SaveDialog1.Filename);
      2: SaveDoc(TJvDBGridExcelExport, SaveDialog1.Filename);
      3: SaveDoc(TJvDBGridHTMLExport, SaveDialog1.Filename);
      4: SaveDoc(TJvDBGridCSVExport, SaveDialog1.Filename);
      5: SaveDoc(TJvDBGridXMLExport, SaveDialog1.Filename);
    end;
    // Open doc in default app
    if mnuOpenFile.Checked then
      ShellExecute(Handle, 'open', PChar(SaveDialog1.Filename), nil, nil, SW_SHOWNORMAL);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Data := TJvCsvDataSet.Create(self);
  Data.CaseInsensitive := true;
  SetupData;
end;

procedure TfrmMain.SetupData;
begin
  Data.CsvFieldDef := 'Filename:$255,Size:%,Attributes:$64,Type:$255';
//  Data.FieldDefs.Add('Filename', ftString, 255, false);
//  Data.FieldDefs.Add('Size', ftInteger, 0, false);
//  Data.FieldDefs.Add('Attributes', ftString, 64, false);
//  Data.FieldDefs.Add('Type', ftString, 255, false);
  Data.Filename := ExtractFilePath(Application.Exename) + 'TestData.csv';
  Data.Active := true;
  Data.Sort('Filename,Type,Attributes,Size', true);
  DataSource1.Dataset := Data;
end;

procedure TfrmMain.DoExportProgress(Sender: TObject; Min, Max,
  Position: Cardinal; const AText: string; var AContinue: Boolean);
begin
  JvProgressDialog1.Min := Min;
  JvProgressDialog1.Max := Max;
  JvProgressDialog1.Position := Position;
  JvProgressDialog1.Caption := AText;
  if Max > 0 then
    JvProgressDialog1.Text := Format('Exporting (%d%% finished)', [round(Position / Max * 100)]);
  AContinue := not JvProgressDialog1.Cancelled;
  if not AContinue or (Position >= Max) then
    JvProgressDialog1.Hide;
end;

procedure TfrmMain.mnuOpenFileClick(Sender: TObject);
begin
  mnuOpenFile.Checked := not mnuOpenFile.Checked;
end;

procedure TfrmMain.Getdata1Click(Sender: TObject);
var
  S: string;
  F: TSearchRec;

  function AttrToStr(Attr: integer): string;
  begin
    Result := '';
    if Attr and FILE_ATTRIBUTE_ARCHIVE = FILE_ATTRIBUTE_ARCHIVE then
      Result := Result + 'A';
    if Attr and FILE_ATTRIBUTE_COMPRESSED = FILE_ATTRIBUTE_COMPRESSED then
      Result := Result + 'C';
    if Attr and FILE_ATTRIBUTE_HIDDEN = FILE_ATTRIBUTE_HIDDEN then
      Result := Result + 'H';
    if Attr and FILE_ATTRIBUTE_READONLY = FILE_ATTRIBUTE_READONLY then
      Result := Result + 'R';
    if Attr and FILE_ATTRIBUTE_SYSTEM = FILE_ATTRIBUTE_SYSTEM then
      Result := Result + 'S';
    if Attr and FILE_ATTRIBUTE_TEMPORARY = FILE_ATTRIBUTE_TEMPORARY then
      Result := Result + 'T';
  end;

  procedure ParseFile(const Filename: string; Size: Cardinal; Attr: integer);
  var
    psfi: TSHFileInfo;
  begin
    FillChar(psfi, sizeof(psfi), 0);
    if SHGetFileInfo(PChar(Filename), 0, psfi, sizeof(psfi), SHGFI_TYPENAME) <> 0 then
    begin
      Data.Append;
      Data.Fields[0].AsString := Filename;
      Data.Fields[1].AsInteger := Size;
      Data.Fields[2].AsString := AttrToStr(Attr);
      Data.Fields[3].AsString := psfi.szTypeName;
      Data.Post;
    end;
  end;
begin
  Data.DisableControls;
  try
    S := GetCurrentDir;
    if BrowseForFolderNative(Handle, 'Select folder to read data from', S) then
    begin
      Screen.Cursor := crHourGlass;
      try
        SetCurrentDir(S);
        S := IncludeTrailingPathDelimiter(S);
        if FindFirst(S + '*.*', faAnyfile and not faDirectory, F) = 0 then
        begin
          repeat
            if (F.Attr and FILE_ATTRIBUTE_DIRECTORY = 0)
              {and not Data.Locate('Filename', VarArrayOf([S + F.Name]), [loCaseInsensitive])}then
              ParseFile(S + F.Name, F.Size, F.Attr);
          until FindNext(F) <> 0;
          FindClose(F);
        end;
        Data.Sort('Filename,Type,Attributes,Size', true);
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  finally
    Data.EnableControls;
  end;
end;

procedure TfrmMain.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.Cleartable1Click(Sender: TObject);
begin
  Data.DisableControls;
  try
    Data.EmptyTable;
  finally
    Data.EnableControls;
  end;
end;

procedure TfrmMain.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  StatusBar1.Panels[0].Text := MinimizeFileName(' ' + Data.Filename, StatusBar1.Canvas, StatusBar1.Panels[0].Width);
  if Data.RecNo >= 0 then
    StatusBar1.Panels[1].Text := Format('  %d of %d', [Data.RecNo + 1, Data.RecordCount])
  else
    StatusBar1.Panels[1].Text := '  Inserting...';
end;

procedure TfrmMain.StatusBar1Resize(Sender: TObject);
begin
  StatusBar1.Panels[0].Width := ClientWidth - 100;
  DataSource1DataChange(nil, nil);
end;

procedure TfrmMain.SaveDialog1TypeChange(Sender: TObject);
var
  S: string;
begin
  S := ExtractFilename(SaveDialog1.Filename);
  if S <> '' then
  begin
    case SaveDialog1.FilterIndex of
      1: S := ChangeFileExt(S, '.doc');
      2: S := ChangeFileExt(S, '.xls');
      3: S := ChangeFileExt(S, '.htm');
      4: S := ChangeFileExt(S, '.csv');
      5: S := ChangeFileExt(S, '.xml');
    end;
    SendMessage(Windows.GetParent(SaveDialog1.Handle), CDM_SETCONTROLTEXT,
      edt1, Integer(PChar(S)));
  end;
end;

procedure TfrmMain.DBGrid1TitleClick(Column: TColumn);
begin
  if DBGrid1.SortedField = Column.FieldName then
    Ascending := not Ascending
  else
    Ascending := false;
  Data.Sort(Column.FieldName, Ascending);
  DBGrid1.SortedField := Column.FieldName;
end;

procedure TfrmMain.DBGrid1GetBtnParams(Sender: TObject; Field: TField;
  AFont: TFont; var Background: TColor; var SortMarker: TSortMarker;
  IsDown: Boolean);
const
  Direction: array[boolean] of TSortmarker = (smDown, smUp);
begin
  if Field.FieldName = DBGrid1.SortedField then
    SortMarker := Direction[Ascending]
  else
    SortMarker := smNone;
end;

end.

