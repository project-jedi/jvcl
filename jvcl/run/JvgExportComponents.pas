{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgExportComponents.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc}

unit JvgExportComponents;

interface

uses
  Windows, Messages, SysUtils, Classes, JvComponent, Graphics,
  Controls, Forms, Dialogs, DB;

type
  TJvExportCaptions = (fecDisplayLabels, fecFieldNames, fecNone);
  TJvExportGetValue = procedure(Sender: TObject; const Field: TField; var Caption: string) of object;
// RDB Added TDataset to Signature
  TJvExportRecordEvent = procedure(Sender: TObject; const Dataset: TDataSet;
    var AllowExport: boolean) of object;
// RDB Added TField to Signature
  TJvExportFieldEvent = procedure(Sender: TObject; const Field: TField; var
    FieldValue: string) of object;

  TJvExportProgressEvent = procedure (Sender:TObject; Min, Max, Position:integer; const Msg:string) of object;

  TJvGetLineFontEvent = procedure(Sender: TObject; LineNo: integer; const Value:
    string; Font: TFont) of object;

  EJvgExportException = class(Exception);

  TJvgCommonExport = class(TJvComponent)
  private
    FSaveToFileName: string;
    FDataSet: TDataSet;
    FOnExportField: TJvExportFieldEvent;
    FOnExportRecord: TJvExportRecordEvent;
    FOnGetCaption: TJvExportGetValue;
    FCaptions: TJvExportCaptions;
    FTransliterateRusToEng: boolean;
    FMaxFieldSize: integer;
    FOnGetTableName: TJvExportGetValue;
    FOnProgress: TJvExportProgressEvent;
    procedure SetCaptions(const Value: TJvExportCaptions);
    procedure SetDataSet(const Value: TDataSet);
    procedure SetSaveToFileName(const Value: string);
    procedure SetMaxFieldSize(const Value: integer);
    procedure SetTransliterateRusToEng(const Value: boolean);
  protected
    function GetFieldValue(const Field: TField): string;
    procedure DoGetTableName(var ATableName:string);virtual;
    procedure DoProgress(Min, Max, Position:integer; const Msg:string);virtual;
  public
    procedure Execute; virtual;
  protected
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property Captions: TJvExportCaptions read FCaptions write SetCaptions;
    property SaveToFileName: string read FSaveToFileName write
      SetSaveToFileName;
    property TransliterateRusToEng: boolean read FTransliterateRusToEng write
      SetTransliterateRusToEng;
    property MaxFieldSize: integer read FMaxFieldSize write SetMaxFieldSize;

    property OnGetCaption: TJvExportGetValue read FOnGetCaption write FOnGetCaption;
    property OnExportRecord: TJvExportRecordEvent read FOnExportRecord write FOnExportRecord;
    property OnExportField: TJvExportFieldEvent read FOnExportField write FOnExportField;
    property OnProgress:TJvExportProgressEvent read FOnProgress write FOnProgress;
    property OnGetTableName:TJvExportGetValue read FOnGetTableName write FOnGetTableName;
  end;

  TJvgExportExcel = class(TJvgCommonExport)
  private
    FHeader: TStringList;
    FFooter: TStringList;
    FBackgroundPicture: TFileName;
    FAutoColumnFit: boolean;
    FExcelVisible: boolean;
    FCloseExcel: boolean;
    FOnGetFooterLineFont: TJvGetLineFontEvent;
    FOnGetHeaderLineFont: TJvGetLineFontEvent;
    FSubHeader: TStringList;
    FSubHeaderFont: TFont;
    FHeaderFont: TFont;
    FFooterFont: TFont;
    FForceTextFormat:boolean;
    FOnGetSubHeaderLineFont: TJvGetLineFontEvent;
    function GetHeader: TStrings;
    function GetFooter: TStrings;
    function GetSubHeader: TStrings;
    procedure SetHeader(const Value: TStrings);
    procedure SetFooter(const Value: TStrings);
    procedure SetBackgroundPicture(const Value: TFileName);
    procedure SetAutoColumnFit(const Value: boolean);
    procedure SetExcelVisible(const Value: boolean);
    procedure SetCloseExcel(const Value: boolean);
    procedure SetSubHeader(const Value: TStrings);
    procedure SetFooterFont(const Value: TFont);
    procedure SetHeaderFont(const Value: TFont);
    procedure SetSubHeaderFont(const Value: TFont);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; override;
  published
    property DataSet;
    property Captions;
    property SaveToFileName;
    property TransliterateRusToEng;
    property MaxFieldSize;
    property Header: TStrings read GetHeader write SetHeader;
    property SubHeader: TStrings read GetSubHeader write SetSubHeader;
    property Footer: TStrings read GetFooter write SetFooter;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property SubHeaderFont: TFont read FSubHeaderFont write SetSubHeaderFont;
    property FooterFont: TFont read FFooterFont write SetFooterFont;
    property AutoColumnFit: boolean read FAutoColumnFit write SetAutoColumnFit
      default true;
    property BackgroundPicture: TFileName read FBackgroundPicture write
      SetBackgroundPicture;
    property ExcelVisible: boolean read FExcelVisible write SetExcelVisible;
    property ForceTextFormat: boolean read FForceTextFormat write FForceTextFormat default false;
    property CloseExcel: boolean read FCloseExcel write SetCloseExcel;

    property OnGetHeaderLineFont: TJvGetLineFontEvent read FOnGetHeaderLineFont write FOnGetHeaderLineFont;
    property OnGetSubHeaderLineFont: TJvGetLineFontEvent read FOnGetSubHeaderLineFont write FOnGetSubHeaderLineFont;
    property OnGetFooterLineFont: TJvGetLineFontEvent read FOnGetFooterLineFont write FOnGetFooterLineFont;
    property OnGetCaption;
    property OnExportRecord;
    property OnExportField;
  end;

  TJvCreateDataset = procedure (Sender:TObject; var Dataset:TDataset) of object;
  TJvSaveDataset = procedure (Sender:TObject; Dataset:TDataset) of object;
  TJvgExportDataset = class(TJvgCommonExport)
  private
    FOnCreateDest: TJvCreateDataset;
    FOnSaveDest: TJvCreateDataset;
  public
    procedure Execute; override;
  published
    property DataSet;
    property Captions;
    property MaxFieldSize;
    property OnGetCaption;
    property OnExportRecord;
    property OnExportField;
    property OnCreateDest:TJvCreateDataset read FOnCreateDest write FOnCreateDest;
    property OnSaveDest:TJvCreateDataset read FOnSaveDest write FOnSaveDest;
  end;

  TJvgExportHTML = class(TJvgCommonExport)
  private
    FFooter: TStringList;
    FHeader: TStringList;
    FStyles: TStringList;
    function GetFooter: TStrings;
    function GetHeader: TStrings;
    function GetStyles: TStrings;
    procedure SetFooter(const Value: TStrings);
    procedure SetHeader(const Value: TStrings);
    procedure SetStyles(const Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //    procedure Execute; override;
  published
    property DataSet;
    property Captions;
    property SaveToFileName;
    property TransliterateRusToEng;
    property MaxFieldSize;
    property OnGetCaption;
    property OnExportRecord;
    property OnExportField;
    property Header: TStrings read GetHeader write SetHeader;
    property Footer: TStrings read GetFooter write SetFooter;
    property Styles: TStrings read GetStyles write SetStyles;
  end;

  TJvgExportXML = class(TJvgCommonExport)
  public
    procedure Execute; override;
  published
    property DataSet;
    property Captions;
    property SaveToFileName;
    property TransliterateRusToEng;
    property MaxFieldSize;
    property OnGetCaption;
    property OnExportRecord;
    property OnExportField;
    property OnProgress;
  end;

implementation

uses
  ComObj, FileCtrl,
  {$IFDEF USEJVCL}
  JvResources,
  {$ENDIF USEJVCL}
  JvConsts, JvSimpleXML,
  JvgUtils, JvgFileUtils;

{$IFNDEF USEJVCL}
resourcestring
  RsEDataSetIsUnassigned = 'DataSet is unassigned';
  RsESaveToFileNamePropertyIsEmpty = 'SaveToFileName property is empty';
{$ENDIF USEJVCL}

{$IFDEF COMPILER5}
function BoolToStr(Value: Boolean; AsString: Boolean = False): string;
const
  BoolStr: array[Boolean, Boolean] of string = (('0', 'False'), ('-1', 'True'));
begin
  Result := BoolStr[Value, AsString];
end;
{$ENDIF COMPILER5}

  { TJvgCommonExport }

procedure TJvgCommonExport.Execute;
begin
  if not Assigned(DataSet) then
    raise EJvgExportException.Create(RsEDataSetIsUnassigned);
  DataSet.Active := true;
  if SaveToFileName <> '' then
    ForceDirectories(ExtractFilePath(SaveToFileName));
end;

procedure TJvgCommonExport.SetCaptions(const Value: TJvExportCaptions);
begin
  FCaptions := Value;
end;

procedure TJvgCommonExport.SetDataSet(const Value: TDataSet);
begin
  FDataSet := Value;
end;

procedure TJvgCommonExport.SetMaxFieldSize(const Value: integer);
begin
  FMaxFieldSize := Value;
end;

procedure TJvgCommonExport.SetSaveToFileName(const Value: string);
begin
  FSaveToFileName := trim(Value);
end;

procedure TJvgCommonExport.SetTransliterateRusToEng(const Value: boolean);
begin
  FTransliterateRusToEng := Value;
end;

procedure TJvgCommonExport.DoProgress(Min, Max, Position: integer; const Msg: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Min, Max, Position, Msg);
end;

function TJvgCommonExport.GetFieldValue(const Field: TField): string;
begin
  Result := Field.AsString;
  if Assigned(OnExportField) then
    OnExportField(Self, Field, Result);

  if FTransliterateRusToEng then
    Result := Transliterate(Result, true);

  if (FMaxFieldSize > 0) and (Field.DataType in [ftString, ftMemo, ftFmtMemo])
    then
  begin
    if length(Result) > FMaxFieldSize then
      Result := copy(Result, 1, FMaxFieldSize) + '...';
  end;
end;

procedure TJvgCommonExport.DoGetTableName(var ATableName: string);
begin
  if Assigned(FOnGetTableName) then
    FOnGetTableName(Self, nil, ATableName);
end;

{ TJvgExportExcel }

constructor TJvgExportExcel.Create(AOwner: TComponent);
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

destructor TJvgExportExcel.Destroy;
begin
  FFooter.Free;
  FHeader.Free;
  FSubHeader.Free;
  FHeaderFont.Free;
  FSubHeaderFont.Free;
  FFooterFont.Free;
  inherited Destroy;
end;

procedure TJvgExportExcel.Execute;
var
  XL: variant;
  Sheet: variant;
  AllowExportRecord: boolean;
  i, RecCount, RecNo, ColNo, OldRecNo: integer;
  CellFont: TFont;

  procedure InsertStrings(Strings: TStrings; Font: TFont; GetLineFontEvent:
    TJvGetLineFontEvent);
  var
    i: integer;
  begin
    for i := 0 to Strings.Count - 1 do
    begin
      Sheet.Cells[RecNo, ColNo] := Strings[i];
      CellFont.Assign(Font);
      if Assigned(FOnGetHeaderLineFont) then
        OnGetHeaderLineFont(Self, i, Strings[i], CellFont);

      Sheet.Cells[RecNo, ColNo].Font.Size := CellFont.Size;
      Sheet.Cells[RecNo, ColNo].Font.Color := CellFont.Color;
      if fsBold in CellFont.Style then
        Sheet.Cells[RecNo, ColNo].Font.Bold := true;
      if fsItalic in CellFont.Style then
        Sheet.Cells[RecNo, ColNo].Font.Italic := true;
      Inc(RecNo);
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
  if (BackgroundPicture <> '') and FileExists(BackgroundPicture) then
    Sheet.SetBackgroundPicture(FileName := BackgroundPicture);

  CellFont := TFont.Create;
  try
    RecNo := 1;
    ColNo := 1;

    Inc(RecNo, Header.Count + SubHeader.Count);

    if FCaptions <> fecNone then
      for i := 0 to DataSet.FieldCount - 1 do
      begin
        case FCaptions of
          fecDisplayLabels:
            if DataSet.Fields[i].DisplayLabel <> '' then
              Sheet.Cells[RecNo, ColNo + i] :=
                DataSet.Fields[i].DisplayLabel
            else
              Sheet.Cells[RecNo, ColNo + i] :=
                DataSet.Fields[i].FieldName;
          fecFieldNames:
            Sheet.Cells[RecNo, ColNo + i] := DataSet.Fields[i].FieldName;
        end;
        Sheet.Cells[RecNo, ColNo + i].Font.Bold := true;
        Sheet.Cells[RecNo, ColNo + i].Font.Size := 10;
      end;

    Inc(RecNo);
    DataSet.First;
    RecCount := Dataset.RecordCount;
    while not DataSet.EOF do
    begin
      AllowExportRecord := true;
      if Assigned(OnExportRecord) then
        OnExportRecord(Self, Dataset, AllowExportRecord);
      if AllowExportRecord then
      begin
        for i := 0 to DataSet.FieldCount - 1 do
          if not (DataSet.Fields[i].DataType in [ftBlob, ftGraphic,
            ftParadoxOle, ftDBaseOle, ftTypedBinary,
              ftReference, ftDataSet, ftOraBlob, ftOraClob, ftInterface,
              ftIDispatch]) then
          begin
            if ForceTextFormat then
              Sheet.Cells.NumberFormat := '@';
            Sheet.Cells[RecNo, ColNo + i] := GetFieldValue(DataSet.Fields[i]);
          end;
      end;
      DoProgress(0, RecCount, RecNo, '');
      Inc(RecNo);
      DataSet.Next;
    end;

    if FAutoColumnFit then
      for i := 0 to DataSet.FieldCount - 1 do
        Sheet.Columns[i + 1].EntireColumn.AutoFit;

    OldRecNo := RecNo;
    RecNo := 1;
    InsertStrings(Header, HeaderFont, FOnGetHeaderLineFont);
    InsertStrings(SubHeader, SubHeaderFont, FOnGetSubHeaderLineFont);
    RecNo := OldRecNo + 1;
    InsertStrings(Footer, SubHeaderFont, FOnGetSubHeaderLineFont);

    if ExtractFileExt(FSaveToFileName) = '' then
      FSaveToFileName := ChangeFileExt(FSaveToFileName, '.xls');
    DeleteFileEx(FSaveToFileName);

    if FSaveToFileName <> '' then
      XL.WorkBooks[XL.WorkBooks.Count].SaveAs(FSaveToFileName);

    if CloseExcel then
      XL.Quit;

  finally
    CellFont.Free;
  end;
end;

procedure TJvgExportExcel.SetAutoColumnFit(const Value: boolean);
begin
  FAutoColumnFit := Value;
end;

procedure TJvgExportExcel.SetBackgroundPicture(const Value: TFileName);
begin
  FBackgroundPicture := Value;
end;

procedure TJvgExportExcel.SetCloseExcel(const Value: boolean);
begin
  FCloseExcel := Value;
end;

procedure TJvgExportExcel.SetExcelVisible(const Value: boolean);
begin
  FExcelVisible := Value;
end;

function TJvgExportExcel.GetFooter: TStrings;
begin
  Result := FFooter;
end;

procedure TJvgExportExcel.SetFooter(const Value: TStrings);
begin
  FFooter.Assign(Value);
end;

procedure TJvgExportExcel.SetFooterFont(const Value: TFont);
begin
  FFooterFont.Assign(Value);
end;

function TJvgExportExcel.GetHeader: TStrings;
begin
  Result := FHeader;
end;

procedure TJvgExportExcel.SetHeader(const Value: TStrings);
begin
  FHeader.Assign(Value);
end;

procedure TJvgExportExcel.SetHeaderFont(const Value: TFont);
begin
  FHeaderFont.Assign(Value);
end;

function TJvgExportExcel.GetSubHeader: TStrings;
begin
  Result := FSubHeader;
end;

procedure TJvgExportExcel.SetSubHeader(const Value: TStrings);
begin
  FSubHeader.Assign(Value);
end;

procedure TJvgExportExcel.SetSubHeaderFont(const Value: TFont);
begin
  FSubHeaderFont.Assign(Value);
end;

{ TJvgExportHTML }

constructor TJvgExportHTML.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFooter := TStringList.Create;
  FHeader := TStringList.Create;
  FStyles := TStringList.Create;
end;

destructor TJvgExportHTML.Destroy;
begin
  FFooter.Free;
  FHeader.Free;
  FStyles.Free;
  inherited Destroy;
end;

function TJvgExportHTML.GetFooter: TStrings;
begin
  Result := FFooter;
end;

function TJvgExportHTML.GetHeader: TStrings;
begin
  Result := FHeader;
end;

function TJvgExportHTML.GetStyles: TStrings;
begin
  Result := FStyles;
end;

procedure TJvgExportHTML.SetFooter(const Value: TStrings);
begin
  FFooter.Assign(Value);
end;

procedure TJvgExportHTML.SetHeader(const Value: TStrings);
begin
  FHeader.Assign(Value);
end;

procedure TJvgExportHTML.SetStyles(const Value: TStrings);
begin
  FStyles.Assign(Value);
end;

{ TJvgExportDataset }

procedure TJvgExportDataset.Execute;
var
  i, RecNo, RecCount: integer;
  Dest: TDataset;
  AllowExportRecord: boolean;
  FieldType: TFieldType;
begin
  inherited;

  Dest := nil;
  if Assigned(FOnCreateDest) then
    FOnCreateDest(Self, Dest);
  if Dest = nil then Exit;
  Dest.Close;
  for i := 0 to DataSet.FieldCount - 1 do
  begin
    FieldType := DataSet.Fields[i].DataType;
    if FieldType = ftAutoInc then
      FieldType := ftInteger;
    Dest.FieldDefs.Add(DataSet.Fields[i].Name, FieldType,
      DataSet.Fields[i].Size, DataSet.Fields[i].Required);
  end;

  Dest.Open;
  try
    DataSet.First;
    RecCount := DataSet.RecordCount;
    RecNo    := 0;
    while not DataSet.EOF do
    begin
      AllowExportRecord := true;
      if Assigned(OnExportRecord) then
        OnExportRecord(Self, DataSet, AllowExportRecord);
      if AllowExportRecord then
      begin
        Dest.Append;
        for i := 0 to DataSet.FieldCount - 1 do
          if DataSet.Fields[i].DataType in [ftString, ftMemo] then
            Dest.Fields[i].Value := GetFieldValue(DataSet.Fields[i])
          else
            Dest.Fields[i].Value := DataSet.Fields[i].Value;
        Dest.Post;
      end;
      DoProgress(0, RecCount, RecNo, '');
      Inc(RecNo);
      DataSet.Next;
    end;
    DoProgress(0, RecCount, RecCount, '');
    if Assigned(FOnSaveDest) then
      FOnSaveDest(Self, Dest);
  finally
    Dest.Close;
    FreeAndNil(Dest);
  end;
end;


{ TJvgExportXML }
(*
procedure TJvgExportXML.Execute;
var
  RecNo, RecCount: Integer;
  XML: TJvSimpleXML;
  Header: TJvSimpleXMLElemClassic;
  Table: TJvSimpleXMLElemClassic;
  Field: TJvSimpleXMLElemClassic;
  Records: TJvSimpleXMLElemClassic;
  XMLRecord: TJvSimpleXMLElemClassic;
  AllowExportRecord: boolean;
  AName, FieldValue: string;
  i: integer;

  function CreateNode(Name: string; Base: TJvSimpleXMLElemClassic):
      TJvSimpleXMLElemClassic;
  begin
    result := TJvSimpleXMLElemClassic.Create(XML.Root);
    Base.Items.add(result);
    result.Name := Name;
  end;

begin
  XML := TJvSimpleXML.Create(Self);
  XML.Root.Name := 'Database';
  XML.IndentString := '  ';

  Header := CreateNode('Header', XML.Root);
  Table := CreateNode('Table', Header);
  AName := Dataset.Name;
  DoGetTableName(AName);
  Table.Properties.Add('Name', AName);
  DataSet.Open;
  RecNo := 0;
  RecCount := DataSet.RecordCount;
  {$IFDEF DEBUG}
  dbg.LogInteger('FieldCount', DataSet.FieldCount);
  {$ENDIF DEBUG}
  for i := 0 to DataSet.FieldCount - 1 do
  begin
    Field := CreateNode('Field', Table);
    Field.Properties.Add('Name', DataSet.Fields[i].DisplayName);
    Field.Properties.Add('Size', DataSet.Fields[i].Size);
    Field.Properties.Add('DataType', Ord(DataSet.Fields[i].DataType));
    Field.Properties.Add('Blob', BoolToStr(DataSet.Fields[i].IsBlob));
    Field.Properties.Add('Required', BoolToStr(DataSet.Fields[i].Required));
    {$IFDEF DEBUG}
    dbg.LogObject('Properties', Field.Properties);
    {$ENDIF DEBUG}
  end;
  Records := CreateNode('Records', XML.Root);
  XMLRecord := CreateNode('Record', Records);
  DataSet.First;
  RecNo := 0;
  while not DataSet.EOF do
  begin
    Inc(RecNo);
    XMLRecord := CreateNode('Record', Records);
    XMLRecord.Properties.Add('Nr', RecNo);
    AllowExportRecord := true;
    if Assigned(OnExportRecord) then
      OnExportRecord(Self, Dataset, AllowExportRecord);
    if AllowExportRecord then
    begin
      for i := 0 to DataSet.FieldCount - 1 do
      begin
        if not (DataSet.Fields[i].DataType in [ftBlob, ftGraphic,
          ftParadoxOle, ftDBaseOle, ftTypedBinary,
            ftReference, ftDataSet, ftOraBlob, ftOraClob, ftInterface,
            ftIDispatch]) then
        begin
          Field := CreateNode('RecordField', XMLRecord);
          Field.Properties.Add('Name', DataSet.Fields[i].DisplayName);
          FieldValue := DataSet.Fields[i].AsString;
          if Assigned(OnExportField) then
            OnExportField(Self, DataSet.Fields[i], FieldValue);
        end;
        Field.Value := FieldValue;
      end;
    end;
    DoProgress(0, RecCount, RecNo, '');
    Inc(RecNo);
    DataSet.Next;
  end;
  DoProgress(0, RecCount, RecCount, '');
  XML.SaveToFile(Self.FSaveToFileName);
end;
*)

{ TJvgExportXML }

procedure TJvgExportXML.Execute;
var
  RecNo, RecCount: Integer;
  XML: TJvSimpleXML;
  Header: TJvSimpleXMLElemClassic;
  Table: TJvSimpleXMLElemClassic;
  Field: TJvSimpleXMLElemClassic;
  Records: TJvSimpleXMLElemClassic;
  XMLRecord: TJvSimpleXMLElemClassic;
  AllowExportRecord: boolean;
  Fieldvalue: string;
  i: integer;

  function CreateNode(Name: string; Base: TJvSimpleXMLElemClassic):
      TJvSimpleXMLElemClassic;
  begin
    result := TJvSimpleXMLElemClassic.Create(XML.Root);
    Base.Items.add(result);
    result.Name := Name;
  end; // CreateNode

  procedure AddFieldName( Field : TJvSimpleXMLElemClassic);
  Var
     Caption : String;
  begin
    Caption := '';
    if Self.FCaptions = fecDisplayLabels then
      Caption := DataSet.Fields[i].DisplayName;
    if Self.FCaptions = fecFieldNames then
      Caption := DataSet.Fields[i].FullName;
    if Self.FCaptions = fecNone then (* NIX *);

    if Assigned(FOnGetCaption) then
       FOnGetCaption(Self, DataSet.Fields[i], Caption );
      Field.Properties.Add('Name', Caption);
  end;    // AddFieldName

begin
  XML := TJvSimpleXML.Create(Self);
  XML.Root.Name := 'Database';
  XML.IndentString := '  ';

  Header := CreateNode('Header', XML.Root);
  Table := CreateNode('Table', Header);
  DataSet.Open;
  RecCount := DataSet.RecordCount;
  for i := 0 to DataSet.FieldCount - 1 do
  begin
    Field := CreateNode('Field', Table);

    AddFieldName(Field);
    Field.Properties.Add('Size', DataSet.Fields[i].Size);
    Field.Properties.Add('DataType', FieldTypeNames[DataSet.Fields[i].DataType]);
    Field.Properties.Add('Blob', BoolToStr(DataSet.Fields[i].IsBlob, True));
    Field.Properties.Add('Required', BoolToStr(DataSet.Fields[i].Required, True));
  end;
  Records := CreateNode('Records', XML.Root);
  DataSet.First;
  RecNo := 0;
  while not DataSet.EOF do
  begin
    Inc(RecNo);
    XMLRecord := CreateNode('Record', Records);
    XMLRecord.Properties.Add('Nr', RecNo);
    AllowExportRecord := true;
    if Assigned(OnExportRecord) then
      OnExportRecord(Self, Dataset, AllowExportRecord);
    if AllowExportRecord then
    begin
      for i := 0 to DataSet.FieldCount - 1 do
      begin
        if not (DataSet.Fields[i].DataType in [ftBlob, ftGraphic,
          ftParadoxOle, ftDBaseOle, ftTypedBinary,
            ftReference, ftDataSet, ftOraBlob, ftOraClob, ftInterface,
            ftIDispatch]) then
        begin
          Field := CreateNode('RecordField', XMLRecord);
          AddFieldName(Field);
          FieldValue := DataSet.Fields[i].AsString;
          if Assigned(OnExportfield) then
            OnExportField(Self, DataSet.Fields[i], Fieldvalue);
          Field.Value := Fieldvalue;
        end;
      end;
    end;
    DoProgress(0, RecCount, RecNo, '');
    DataSet.Next;
  end;
  DoProgress(0, RecCount, RecCount, '');

  XML.SaveToFile(Self.FSaveToFileName);
end; // TJvgExportXML.Execute

end.

