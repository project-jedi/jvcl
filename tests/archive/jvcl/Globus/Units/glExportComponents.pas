unit glExportComponents;
{$I glDEF.INC}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, db, dbtables;

type
  TglExportCaptions = (fecDisplayLabels, fecFieldNames, fecNone);
  TGetCaptionEvent = procedure (Sender: TObject; var Caption: string) of object;
  TExportRecordEvent = procedure (Sender: TObject; var AllowExport: boolean) of object;
  TExportFieldEvent = procedure (Sender: TObject; const Field: TField; var FieldValue: string) of object;
  TGetLineFontEvent = procedure (Sender: TObject; LineNo: integer; const Value: string; Font: TFont) of object;

  glExportException = class(Exception)
  end;

  TglCommonExport = class(TComponent)
  private
    FSaveToFileName: string;
    FDataSet: TDataSet;
    FOnExportField: TExportFieldEvent;
    FOnExportRecord: TExportRecordEvent;
    FOnGetCaption: TGetCaptionEvent;
    FCaptions: TglExportCaptions;
    FTransliterateRusToEng: boolean;
    FMaxFieldSize: integer;
    procedure SetCaptions(const Value: TglExportCaptions);
    procedure SetDataSet(const Value: TDataSet);
    procedure SetOnExportField(const Value: TExportFieldEvent);
    procedure SetOnExportRecord(const Value: TExportRecordEvent);
    procedure SetOnGetCaption(const Value: TGetCaptionEvent);
    procedure SetSaveToFileName(const Value: string);
    procedure SetMaxFieldSize(const Value: integer);
    procedure SetTransliterateRusToEng(const Value: boolean);
    { Private declarations }
  protected
    function GetFieldValue(const Field: TField): string;
  public
    procedure Execute; virtual;
  protected
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property Captions: TglExportCaptions read FCaptions write SetCaptions;
    property SaveToFileName: string read FSaveToFileName write SetSaveToFileName;
    {$IFDEF GLVER_D5}
    property TransliterateRusToEng: boolean read FTransliterateRusToEng write SetTransliterateRusToEng;
    {$ENDIF}
    property MaxFieldSize: integer read FMaxFieldSize write SetMaxFieldSize;

    property OnGetCaption: TGetCaptionEvent read FOnGetCaption write SetOnGetCaption;
    property OnExportRecord: TExportRecordEvent read FOnExportRecord write SetOnExportRecord;
    property OnExportField: TExportFieldEvent read FOnExportField write SetOnExportField;
  end;

  TglExportExcel = class(TglCommonExport)
  private
    FHeader: TStrings;
    FFooter: TStrings;
    FBackgroundPicture: TFileName;
    FAutoColumnFit: boolean;
    FExcelVisible: boolean;
    FCloseExcel: boolean;
    FOnGetFooterLineFont: TGetLineFontEvent;
    FOnGetHeaderLineFont: TGetLineFontEvent;
    FSubHeader: TStrings;
    FSubHeaderFont: TFont;
    FHeaderFont: TFont;
    FFooterFont: TFont;
    FOnGetSubHeaderLineFont: TGetLineFontEvent;
    procedure SetHeader(const Value: TStrings);
    procedure SetFooter(const Value: TStrings);
    procedure SetBackgroundPicture(const Value: TFileName);
    procedure SetAutoColumnFit(const Value: boolean);
    procedure SetExcelVisible(const Value: boolean);
    procedure SetCloseExcel(const Value: boolean);
    procedure SetOnGetFooterLineFont(const Value: TGetLineFontEvent);
    procedure SetOnGetHeaderLineFont(const Value: TGetLineFontEvent);
    procedure SetSubHeader(const Value: TStrings);
    procedure SetFooterFont(const Value: TFont);
    procedure SetHeaderFont(const Value: TFont);
    procedure SetSubHeaderFont(const Value: TFont);
    procedure SetOnGetSubHeaderLineFont(const Value: TGetLineFontEvent);
    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Execute; override;
  published
    property DataSet;
    property Captions;
    property SaveToFileName;
    {$IFDEF GLVER_D5}
    property TransliterateRusToEng;
    {$ENDIF}
    property MaxFieldSize;
    property OnGetCaption;
    property OnExportRecord;
    property OnExportField;

    property Header: TStrings read FHeader write SetHeader;
    property SubHeader: TStrings read FSubHeader write SetSubHeader;
    property Footer: TStrings read FFooter write SetFooter;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property SubHeaderFont: TFont read FSubHeaderFont write SetSubHeaderFont;
    property FooterFont: TFont read FFooterFont write SetFooterFont;
    property AutoColumnFit: boolean read FAutoColumnFit write SetAutoColumnFit default true;
    property BackgroundPicture: TFileName read FBackgroundPicture write SetBackgroundPicture;
    property ExcelVisible: boolean read FExcelVisible write SetExcelVisible;
    property CloseExcel: boolean read FCloseExcel write SetCloseExcel;

    property OnGetHeaderLineFont: TGetLineFontEvent read FOnGetHeaderLineFont write SetOnGetHeaderLineFont;
    property OnGetSubHeaderLineFont: TGetLineFontEvent read FOnGetSubHeaderLineFont write SetOnGetSubHeaderLineFont;
    property OnGetFooterLineFont: TGetLineFontEvent read FOnGetFooterLineFont write SetOnGetFooterLineFont;
  end;

  TglExportDBETable = class(TglCommonExport)
  private
    FTableType: TTableType;
    procedure SetTableType(const Value: TTableType);
    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
//    destructor Destroy; override;

    procedure Execute; override;
  published
    property DataSet;
    property Captions;
    property SaveToFileName;
    {$IFDEF GLVER_D5}
    property TransliterateRusToEng;
    {$ENDIF}
    property MaxFieldSize;
    property OnGetCaption;
    property OnExportRecord;
    property OnExportField;

    property TableType: TTableType read FTableType write SetTableType default ttDBase;
  end;

  TglExportHTML = class(TglCommonExport)
  private
    FFooter: TStrings;
    FHeader: TStrings;
    FStyles: TStrings;
    procedure SetFooter(const Value: TStrings);
    procedure SetHeader(const Value: TStrings);
    procedure SetStyles(const Value: TStrings);
    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

//    procedure Execute; override;
  published
    property DataSet;
    property Captions;
    property SaveToFileName;
    {$IFDEF GLVER_D5}
    property TransliterateRusToEng;
    {$ENDIF}
    property MaxFieldSize;
    property OnGetCaption;
    property OnExportRecord;
    property OnExportField;

    property Header: TStrings read FHeader write SetHeader;
    property Footer: TStrings read FFooter write SetFooter;
    property Styles: TStrings read FStyles write SetStyles;
  end;

  TglExportXML = class(TglCommonExport)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
//    constructor Create(AOwner: TComponent); override;
//    destructor Destroy; override;

//    procedure Execute; override;
  published
    property DataSet;
    property Captions;
    property SaveToFileName;
    {$IFDEF GLVER_D5}
    property TransliterateRusToEng;
    {$ENDIF}
    property MaxFieldSize;
    property OnGetCaption;
    property OnExportRecord;
    property OnExportField;

  end;

procedure Register;

implementation
uses ComObj, FileCtrl, glUtils, glFUtils;

procedure Register;
begin
  RegisterComponents('Gl ExportImport', [TglExportExcel, TglExportDBETable{, TglExportHTML, TglExportXML}]);
end;

{ TglCommonExport }

procedure TglCommonExport.Execute;
begin
  if not Assigned(DataSet) then raise glExportException.Create('DataSet is unassigned');
  DataSet.Active := true;
  if SaveToFileName <> '' then
    ForceDirectories(ExtractFilePath(SaveToFileName));
end;

procedure TglCommonExport.SetCaptions(const Value: TglExportCaptions);
begin
  FCaptions := Value;
end;

procedure TglCommonExport.SetDataSet(const Value: TDataSet);
begin
  FDataSet := Value;
end;

procedure TglCommonExport.SetMaxFieldSize(const Value: integer);
begin
  FMaxFieldSize := Value;
end;

procedure TglCommonExport.SetOnExportField(const Value: TExportFieldEvent);
begin
  FOnExportField := Value;
end;

procedure TglCommonExport.SetOnExportRecord(
  const Value: TExportRecordEvent);
begin
  FOnExportRecord := Value;
end;

procedure TglCommonExport.SetOnGetCaption(const Value: TGetCaptionEvent);
begin
  FOnGetCaption := Value;
end;

procedure TglCommonExport.SetSaveToFileName(const Value: string);
begin
  FSaveToFileName := trim(Value);
end;

procedure TglCommonExport.SetTransliterateRusToEng(const Value: boolean);
begin
  FTransliterateRusToEng := Value;
end;

function TglCommonExport.GetFieldValue(const Field: TField): string;
begin
  Result := Field.AsString;
  if Assigned(OnExportField) then OnExportField(self, Field, Result);

  {$IFDEF GLVER_D5}
  if FTransliterateRusToEng then Result := Transliterate(Result, true);
  {$ENDIF}

  if (FMaxFieldSize > 0)and(Field.DataType in [ftString, ftMemo, ftFmtMemo]) then
  begin
    if length(Result) > FMaxFieldSize then
      Result := copy(Result, 1, FMaxFieldSize) + '...';
  end;
end;

{ TglExportExcel }

constructor TglExportExcel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFooter := TStringList.Create;
  FHeader := TStringList.Create;
  FSubHeader := TStringList.Create;
  FHeaderFont := TFont.Create;
  FSubHeaderFont := TFont.Create;
  FFooterFont := TFont.Create;
  //...defaults
  FHeaderFont.Size := 12;
  FHeaderFont.Style := [fsBold];
  FSubHeaderFont.Size := 10;
  FAutoColumnFit := true;
end;

destructor TglExportExcel.Destroy;
begin
  FFooter.Free;
  FHeader.Free;
  FSubHeader.Free;
  FHeaderFont.Free;
  FSubHeaderFont.Free;
  FFooterFont.Free;
  inherited Destroy;
end;

procedure TglExportExcel.Execute;
var
  XL: variant;
  Sheet: variant;
  AllowExportRecord: boolean;
  i, j, RecNo, ColNo, OldRecNo: integer;
  CellFont: TFont;
  procedure InsertStrings(Strings: TStrings; Font: TFont; GetLineFontEvent: TGetLineFontEvent);
  var i: integer;
  begin
    for i:=0 to Strings.Count-1 do
    begin
      Sheet.Cells[RecNo, ColNo] := Strings[i];
      CellFont.Assign(Font);
      if Assigned(FOnGetHeaderLineFont) then OnGetHeaderLineFont(self, i, Strings[i], CellFont);

      Sheet.Cells[RecNo, ColNo].Font.Size := CellFont.Size;
      Sheet.Cells[RecNo, ColNo].Font.Color := CellFont.Color;      
      if fsBold in CellFont.Style then
        Sheet.Cells[RecNo, ColNo].Font.Bold := true;
      if fsItalic in CellFont.Style then
        Sheet.Cells[RecNo, ColNo].Font.Italic := true;
      inc(RecNo);
    end;
  end;

begin
  inherited Execute;

  try
    XL := GetActiveOleObject('Excel.Application');
  except
    XL := CreateOleObject('Excel.Application');
  end;

  XL.Visible := FExcelVisible;
  XL.WorkBooks.Add;
  XL.WorkBooks[XL.WorkBooks.Count].WorkSheets[1].Name := 'Report';
  Sheet := XL.WorkBooks[XL.WorkBooks.Count].WorkSheets['Report'];
  if (BackgroundPicture<>'')and FileExists(BackgroundPicture) then
    Sheet.SetBackgroundPicture(FileName:=BackgroundPicture);

  CellFont := TFont.Create;
  try
    RecNo := 1; ColNo := 1;

    inc(RecNo, Header.Count + SubHeader.Count);

    if FCaptions <> fecNone then
    for i:=0 to DataSet.FieldCount-1 do
    begin
      case FCaptions of
        fecDisplayLabels:
         if DataSet.Fields[i].DisplayLabel <> '' then Sheet.Cells[RecNo, ColNo+i] := DataSet.Fields[i].DisplayLabel
                                                 else Sheet.Cells[RecNo, ColNo+i] := DataSet.Fields[i].FieldName;
        fecFieldNames:
          Sheet.Cells[RecNo, ColNo+i] := DataSet.Fields[i].FieldName;
      end;
      Sheet.Cells[RecNo, ColNo+i].Font.Bold := true;
      Sheet.Cells[RecNo, ColNo+i].Font.Size := 10;
    end;

    inc(RecNo);
    DataSet.First;
    while not DataSet.EOF do
    begin
      AllowExportRecord := true;
      if Assigned(OnExportRecord) then OnExportRecord(self, AllowExportRecord);
      if AllowExportRecord then
      begin
        for i:=0 to DataSet.FieldCount-1 do
          if not (DataSet.Fields[i].DataType in [ftBlob, ftGraphic, ftParadoxOle, ftDBaseOle, ftTypedBinary{$IFDEF GLVER_D5}, ftReference, ftDataSet, ftOraBlob, ftOraClob, ftInterface, ftIDispatch{$ENDIF}]) then
            Sheet.Cells[RecNo, ColNo+i] := GetFieldValue(DataSet.Fields[i]);

        inc(RecNo);
      end;
      DataSet.Next;
    end;

    if FAutoColumnFit then
    for i:=0 to DataSet.FieldCount-1 do
      Sheet.Columns[i+1].EntireColumn.AutoFit;

    OldRecNo := RecNo;
    RecNo := 1;
    InsertStrings(Header, HeaderFont, FOnGetHeaderLineFont);
    InsertStrings(SubHeader, SubHeaderFont, FOnGetSubHeaderLineFont);
    RecNo := OldRecNo + 1;
    InsertStrings(Footer, SubHeaderFont, FOnGetSubHeaderLineFont);


    if ExtractFileExt(FSaveToFileName) = '' then FSaveToFileName := DelFileExt(FSaveToFileName) + '.xls';
    if FileExists(FSaveToFileName) then DeleteFileEx(FSaveToFileName);

    if FSaveToFileName <> '' then XL.WorkBooks[XL.WorkBooks.Count].SaveAs(FSaveToFileName);

    if CloseExcel then XL.Quit;

  finally
    CellFont.Free;
  end;
end;

procedure TglExportExcel.SetAutoColumnFit(const Value: boolean);
begin
  FAutoColumnFit := Value;
end;

procedure TglExportExcel.SetBackgroundPicture(const Value: TFileName);
begin
  FBackgroundPicture := Value;
end;

procedure TglExportExcel.SetCloseExcel(const Value: boolean);
begin
  FCloseExcel := Value;
end;

procedure TglExportExcel.SetExcelVisible(const Value: boolean);
begin
  FExcelVisible := Value;
end;

procedure TglExportExcel.SetFooter(const Value: TStrings);
begin
  FFooter.Assign(Value);
end;

procedure TglExportExcel.SetFooterFont(const Value: TFont);
begin
  FFooterFont.Assign(Value);
end;

procedure TglExportExcel.SetHeader(const Value: TStrings);
begin
  FHeader.Assign(Value);
end;

procedure TglExportExcel.SetHeaderFont(const Value: TFont);
begin
  FHeaderFont.Assign(Value);
end;

procedure TglExportExcel.SetOnGetFooterLineFont(const Value: TGetLineFontEvent);
begin
  FOnGetFooterLineFont := Value;
end;

procedure TglExportExcel.SetOnGetHeaderLineFont(const Value: TGetLineFontEvent);
begin
  FOnGetHeaderLineFont := Value;
end;

procedure TglExportExcel.SetOnGetSubHeaderLineFont(const Value: TGetLineFontEvent);
begin
  FOnGetSubHeaderLineFont := Value;
end;

procedure TglExportExcel.SetSubHeader(const Value: TStrings);
begin
  FSubHeader.Assign(Value);
end;

procedure TglExportExcel.SetSubHeaderFont(const Value: TFont);
begin
  FSubHeaderFont.Assign(Value);
end;

{ TglExportHTML }

constructor TglExportHTML.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFooter := TStringList.Create;
  FHeader := TStringList.Create;
  FStyles := TStringList.Create;
end;

destructor TglExportHTML.Destroy;
begin
  FFooter.Free;
  FHeader.Free;
  FStyles.Free;
  inherited Destroy;
end;

procedure TglExportHTML.SetFooter(const Value: TStrings);
begin
  FFooter.Assign(Value);
end;

procedure TglExportHTML.SetHeader(const Value: TStrings);
begin
  FHeader.Assign(Value);
end;

procedure TglExportHTML.SetStyles(const Value: TStrings);
begin
  FStyles.Assign(Value);
end;

{ TglExportDBETable }

constructor TglExportDBETable.Create(AOwner: TComponent);
begin
  inherited;
  //...defailts
  TableType := ttDBase;
end;

procedure TglExportDBETable.Execute;
var
  i: integer;
  Table: TTable;
  AllowExportRecord: boolean;
  FieldType: TFieldType;
const
  {$IFDEF GLVER_D4}
  aTableTypeExt: array[TTableType] of string = ('', 'db', 'dbf', 'dbf', 'txt');
  {$ELSE}
  aTableTypeExt: array[TTableType] of string = ('', 'db', 'dbf', 'txt');
  {$ENDIF}
begin
  inherited;

  if SaveToFileName = '' then raise glExportException.Create('SaveToFileName property is empty');

  Table := TTable.Create(nil);

  Table.TableType := TableType;
  Table.TableName := SaveToFileName;
//  if ExtractFileExt(Table.TableName) = '' then Table.TableName := DelFileExt() + aTableTypeExt[TableType];

  FieldType := DataSet.Fields[i].DataType;
  if FieldType = ftAutoInc then FieldType := ftInteger;

  for i:=0 to DataSet.FieldCount-1 do
    Table.FieldDefs.Add(DataSet.Fields[i].Name, FieldType, DataSet.Fields[i].Size, DataSet.Fields[i].Required);

  Table.CreateTable;
  Table.Open;

  try
    DataSet.First;
    while not DataSet.EOF do
    begin
      AllowExportRecord := true;
      if Assigned(OnExportRecord) then OnExportRecord(self, AllowExportRecord);
      if AllowExportRecord then
      begin
        Table.Append;
        for i:=0 to DataSet.FieldCount-1 do
          if DataSet.Fields[i].DataType in [ftString, ftMemo] then Table.Fields[i].Value := GetFieldValue(DataSet.Fields[i])
                                                              else Table.Fields[i].Value := DataSet.Fields[i].Value;
        Table.Post;
    end;
      DataSet.Next;
    end;
    Table.Close;
  except
    Table.Free;
  end;


end;

procedure TglExportDBETable.SetTableType(const Value: TTableType);
begin
  if Value = ttDefault then FTableType := ttDBase
                       else FTableType := Value;
end;

end.
