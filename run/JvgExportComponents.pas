{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgExportComponents.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgExportComponents;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, DB,
  JvComponentBase;

type
  TJvExportCaptions = (fecDisplayLabels, fecFieldNames, fecNone);
  TJvExportGetValue = procedure(Sender: TObject; const Field: TField; var Caption: string) of object;
  // RDB Added TDataSet to Signature
  TJvExportRecordEvent = procedure(Sender: TObject; const DataSet: TDataSet;
    var AllowExport: Boolean) of object;
  // RDB Added TField to Signature
  TJvExportFieldEvent = procedure(Sender: TObject; const Field: TField; var
    FieldValue: string) of object;

  TJvExportProgressEvent = procedure(Sender: TObject; Min, Max, Position: Integer;
    const Msg: string) of object;

  TJvGetLineFontEvent = procedure(Sender: TObject; LineNo: Integer;
    const Value: string; Font: TFont) of object;

  EJvgExportException = class(Exception);

  TJvgExportOptions = set of (jeoOutputInvisibleColumns, jeoOutputFormattedStrings);

  TJvgCommonExport = class(TJvComponent)
  private
    FOptions: TJvgExportOptions;
    FSaveToFileName: string;
    FDataSet: TDataSet;
    FOnExportField: TJvExportFieldEvent;
    FOnExportRecord: TJvExportRecordEvent;
    FOnGetCaption: TJvExportGetValue;
    FCaptions: TJvExportCaptions;
    FTransliterateRusToEng: Boolean;
    FMaxFieldSize: Integer;
    FOnGetTableName: TJvExportGetValue;
    FOnProgress: TJvExportProgressEvent;
    procedure SetCaptions(const Value: TJvExportCaptions);
    procedure SetDataSet(const Value: TDataSet);
    procedure SetSaveToFileName(const Value: string);
    procedure SetMaxFieldSize(const Value: Integer);
    procedure SetTransliterateRusToEng(const Value: Boolean);
  protected
    function GetFieldValue(const Field: TField): string;
    procedure DoGetTableName(var ATableName: string); virtual;
    procedure DoProgress(Min, Max, Position: Integer; const Msg: string); virtual;
  public
    procedure Execute; virtual;
  protected
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property Captions: TJvExportCaptions read FCaptions write SetCaptions;
    property SaveToFileName: string read FSaveToFileName write SetSaveToFileName;
    property TransliterateRusToEng: Boolean read FTransliterateRusToEng write SetTransliterateRusToEng;
    property MaxFieldSize: Integer read FMaxFieldSize write SetMaxFieldSize;
    property Options: TJvgExportOptions read FOptions write FOptions default [];

    property OnGetCaption: TJvExportGetValue read FOnGetCaption write FOnGetCaption;
    property OnExportRecord: TJvExportRecordEvent read FOnExportRecord write FOnExportRecord;
    property OnExportField: TJvExportFieldEvent read FOnExportField write FOnExportField;
    property OnProgress: TJvExportProgressEvent read FOnProgress write FOnProgress;
    property OnGetTableName: TJvExportGetValue read FOnGetTableName write FOnGetTableName;
  end;

  TJvgExportExcel = class(TJvgCommonExport)
  private
    FHeader: TStringList;
    FFooter: TStringList;
    FBackgroundPicture: TFileName;
    FAutoColumnFit: Boolean;
    FExcelVisible: Boolean;
    FCloseExcel: Boolean;
    FOnGetFooterLineFont: TJvGetLineFontEvent;
    FOnGetHeaderLineFont: TJvGetLineFontEvent;
    FSubHeader: TStringList;
    FSubHeaderFont: TFont;
    FHeaderFont: TFont;
    FFooterFont: TFont;
    FForceTextFormat: Boolean;
    FOnGetSubHeaderLineFont: TJvGetLineFontEvent;
    function GetHeader: TStrings;
    function GetFooter: TStrings;
    function GetSubHeader: TStrings;
    procedure SetHeader(const Value: TStrings);
    procedure SetFooter(const Value: TStrings);
    procedure SetBackgroundPicture(const Value: TFileName);
    procedure SetAutoColumnFit(const Value: Boolean);
    procedure SetExcelVisible(const Value: Boolean);
    procedure SetCloseExcel(const Value: Boolean);
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
    property AutoColumnFit: Boolean read FAutoColumnFit write SetAutoColumnFit default True;
    property BackgroundPicture: TFileName read FBackgroundPicture write
      SetBackgroundPicture;
    property ExcelVisible: Boolean read FExcelVisible write SetExcelVisible;
    property ForceTextFormat: Boolean read FForceTextFormat write FForceTextFormat default False;
    property CloseExcel: Boolean read FCloseExcel write SetCloseExcel;

    property Options;

    property OnGetHeaderLineFont: TJvGetLineFontEvent read FOnGetHeaderLineFont write FOnGetHeaderLineFont;
    property OnGetSubHeaderLineFont: TJvGetLineFontEvent read FOnGetSubHeaderLineFont write FOnGetSubHeaderLineFont;
    property OnGetFooterLineFont: TJvGetLineFontEvent read FOnGetFooterLineFont write FOnGetFooterLineFont;
    property OnGetCaption;
    property OnExportRecord;
    property OnExportField;
  end;

  TJvCreateDataset = procedure(Sender: TObject; var DataSet: TDataSet) of object;

  TJvgExportDataset = class(TJvgCommonExport)
  private
    FOnCreateDest: TJvCreateDataset;
    FOnSaveDest: TJvCreateDataset;
  public
    procedure Execute; override;
  published
    property DataSet;
    property Captions;
    property Options;
    property MaxFieldSize;
    property OnGetCaption;
    property OnExportRecord;
    property OnExportField;
    property OnCreateDest: TJvCreateDataset read FOnCreateDest write FOnCreateDest;
    property OnSaveDest: TJvCreateDataset read FOnSaveDest write FOnSaveDest;
  end;

  TJvgExportXML = class(TJvgCommonExport)
  public
    procedure Execute; override;
  published
    property DataSet;
    property Captions;
    property SaveToFileName;
    property Options;
    property TransliterateRusToEng;
    property MaxFieldSize;
    property OnGetCaption;
    property OnExportRecord;
    property OnExportField;
    property OnProgress;
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
  ComObj, FileCtrl,
  JvResources,
  JvConsts, JvSimpleXml,
  JvgUtils;

function DeleteFileEx(const FileName: string): Boolean;
const
  cSuffix = '_del_';
begin
  if FileExists(FileName) then
  begin
    Result := RenameFile(FileName, FileName + cSuffix);
    if Result then
      Result := DeleteFile(FileName + cSuffix);
  end
  else
    Result := False;
end;

//=== { TJvgCommonExport } ===================================================

procedure TJvgCommonExport.Execute;
begin
  if not Assigned(DataSet) then
    raise EJvgExportException.CreateRes(@RsEDataSetIsUnassigned);
  DataSet.Active := True;
  if SaveToFileName <> '' then
    {$IFDEF RTL220_UP}SysUtils.{$ENDIF RTL220_UP}ForceDirectories(ExtractFilePath(SaveToFileName));
end;

procedure TJvgCommonExport.SetCaptions(const Value: TJvExportCaptions);
begin
  FCaptions := Value;
end;

procedure TJvgCommonExport.SetDataSet(const Value: TDataSet);
begin
  FDataSet := Value;
end;

procedure TJvgCommonExport.SetMaxFieldSize(const Value: Integer);
begin
  FMaxFieldSize := Value;
end;

procedure TJvgCommonExport.SetSaveToFileName(const Value: string);
begin
  FSaveToFileName := Trim(Value);
end;

procedure TJvgCommonExport.SetTransliterateRusToEng(const Value: Boolean);
begin
  FTransliterateRusToEng := Value;
end;

procedure TJvgCommonExport.DoProgress(Min, Max, Position: Integer; const Msg: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Min, Max, Position, Msg);
end;

function TJvgCommonExport.GetFieldValue(const Field: TField): string;
begin
  if jeoOutputFormattedStrings in Options then
    Result := Field.DisplayText
  else
    Result := Field.AsString;
  if Assigned(FOnExportField) then
    FOnExportField(Self, Field, Result);

  if FTransliterateRusToEng then
    Result := Transliterate(Result, True);

  if (FMaxFieldSize > 0) and (Field.DataType in [ftString, ftMemo, ftFmtMemo]) then
    if Length(Result) > FMaxFieldSize then
      Result := Copy(Result, 1, FMaxFieldSize) + '...';
end;

procedure TJvgCommonExport.DoGetTableName(var ATableName: string);
begin
  if Assigned(FOnGetTableName) then
    FOnGetTableName(Self, nil, ATableName);
end;

//=== { TJvgExportExcel } ====================================================

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
  FAutoColumnFit := True;
  FOptions := [];
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
  XL: Variant;
  Sheet: Variant;
  AllowExportRecord: Boolean;
  I, RecCount, RecNo, ColNo, OldRecNo: Integer;
  CellFont: TFont;

  procedure InsertStrings(Strings: TStrings; Font: TFont; GetLineFontEvent:
    TJvGetLineFontEvent);
  var
    I: Integer;
  begin
    for I := 0 to Strings.Count - 1 do
    begin
      Sheet.Cells[RecNo, ColNo] := Strings[I];
      CellFont.Assign(Font);
      if Assigned(GetLineFontEvent) then
        GetLineFontEvent(Self, I, Strings[I], CellFont);

      Sheet.Cells[RecNo, ColNo].Font.Size := CellFont.Size;
      Sheet.Cells[RecNo, ColNo].Font.Color := CellFont.Color;
      if fsBold in CellFont.Style then
        Sheet.Cells[RecNo, ColNo].Font.Bold := True;
      if fsItalic in CellFont.Style then
        Sheet.Cells[RecNo, ColNo].Font.Italic := True;
      Inc(RecNo);
    end;
  end;

begin
  inherited Execute;

  try
    XL := GetActiveOleObject('Excel.Application');
  except
    try
      XL := CreateOleObject('Excel.Application');
    except
      raise EJvgExportException.CreateRes(@RsEExcelNotAvailable);
    end;
  end;

  XL.Visible := FExcelVisible;
  XL.WorkBooks.Add;
  XL.WorkBooks[XL.WorkBooks.Count].WorkSheets[1].Name := 'Report';
  Sheet := XL.WorkBooks[XL.WorkBooks.Count].WorkSheets['Report'];
  if (BackgroundPicture <> '') and FileExists(BackgroundPicture) then
    // (rom) This is correct Delphi. See "positional parameters" in the Delphi help.
    Sheet.SetBackgroundPicture(FileName := BackgroundPicture);

  CellFont := TFont.Create;
  try
    RecNo := 1;
    ColNo := 1;

    Inc(RecNo, Header.Count + SubHeader.Count);

    // HEADERS
    if FCaptions <> fecNone then
      for I := 0 to DataSet.FieldCount - 1 do
      begin
        if not DataSet.Fields[I].Visible and not (jeoOutputInvisibleColumns in Options) then
        begin
          Dec(ColNo);
          Continue;
        end;

        case FCaptions of
          fecDisplayLabels:
            if DataSet.Fields[I].DisplayLabel <> '' then
              Sheet.Cells[RecNo, ColNo + I] := DataSet.Fields[I].DisplayLabel
            else
              Sheet.Cells[RecNo, ColNo + I] := DataSet.Fields[I].FieldName;
          fecFieldNames:
            Sheet.Cells[RecNo, ColNo + I] := DataSet.Fields[I].FieldName;
        end;
        Sheet.Cells[RecNo, ColNo + I].Font.Bold := True;
        Sheet.Cells[RecNo, ColNo + I].Font.Size := 10;
      end;

    Inc(RecNo);
    DataSet.First;
    RecCount := DataSet.RecordCount;
    while not DataSet.Eof do
    begin
      ColNo := 1;
      AllowExportRecord := True;
      if Assigned(FOnExportRecord) then
        FOnExportRecord(Self, DataSet, AllowExportRecord);

      if AllowExportRecord then
      begin
        // DATA
        for I := 0 to DataSet.FieldCount - 1 do
        begin
          if not DataSet.Fields[I].Visible and not (jeoOutputInvisibleColumns in Options) then
          begin
            Dec(ColNo);
            Continue;
          end;

          if not (DataSet.Fields[I].DataType in [ftBlob, ftGraphic,
            ftParadoxOle, ftDBaseOle, ftTypedBinary,
              ftReference, ftDataSet, ftOraBlob, ftOraClob, ftInterface,
              ftIDispatch]) then
          begin
            if ForceTextFormat then
              Sheet.Cells.NumberFormat := '@';
            Sheet.Cells[RecNo, ColNo + I] := GetFieldValue(DataSet.Fields[I]);
          end;
        end;
      end;
      DoProgress(0, RecCount, RecNo, '');
      Inc(RecNo);
      DataSet.Next;
    end;

    if FAutoColumnFit then
      for I := 0 to DataSet.FieldCount - 1 + ColNo - 1 do
        Sheet.Columns[I + 1].EntireColumn.AutoFit;

    ColNo := 1;
    OldRecNo := RecNo;
    RecNo := 1;
    InsertStrings(Header, HeaderFont, FOnGetHeaderLineFont);
    InsertStrings(SubHeader, SubHeaderFont, FOnGetSubHeaderLineFont);
    RecNo := OldRecNo + 1;
    InsertStrings(Footer, SubHeaderFont, FOnGetSubHeaderLineFont);

    if Length(FSaveToFileName) > 0 then
    begin
      if ExtractFileExt(FSaveToFileName) = '' then
        FSaveToFileName := ChangeFileExt(FSaveToFileName, '.xls');
      DeleteFileEx(FSaveToFileName);

      if FSaveToFileName <> '' then
        XL.WorkBooks[XL.WorkBooks.Count].SaveAs(FSaveToFileName);
    end;
  finally
    try
      if CloseExcel then
        XL.Quit;
    except;
    end;
    CellFont.Free;
  end;
end;

procedure TJvgExportExcel.SetAutoColumnFit(const Value: Boolean);
begin
  FAutoColumnFit := Value;
end;

procedure TJvgExportExcel.SetBackgroundPicture(const Value: TFileName);
begin
  FBackgroundPicture := Value;
end;

procedure TJvgExportExcel.SetCloseExcel(const Value: Boolean);
begin
  FCloseExcel := Value;
end;

procedure TJvgExportExcel.SetExcelVisible(const Value: Boolean);
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

//=== { TJvgExportDataset } ==================================================

procedure TJvgExportDataset.Execute;
var
  I, RecNo, RecCount: Integer;
  Dest: TDataSet;
  AllowExportRecord: Boolean;
  FieldType: TFieldType;
begin
  inherited Execute;

  Dest := nil;
  if Assigned(FOnCreateDest) then
    FOnCreateDest(Self, Dest);
  if Dest = nil then
    Exit;
  Dest.Close;
  for I := 0 to DataSet.FieldCount - 1 do
  begin
    FieldType := DataSet.Fields[I].DataType;
    if FieldType = ftAutoInc then
      FieldType := ftInteger;
    if not DataSet.DefaultFields then
      Dest.FieldDefs.Add(DataSet.Fields[I].Name, FieldType,
        DataSet.Fields[I].Size, DataSet.Fields[I].Required);
  end;

  Dest.Open;
  try
    DataSet.First;
    RecCount := DataSet.RecordCount;
    RecNo := 0;
    while not DataSet.Eof do
    begin
      AllowExportRecord := True;
      if Assigned(FOnExportRecord) then
        FOnExportRecord(Self, DataSet, AllowExportRecord);
      if AllowExportRecord then
      begin
        Dest.Append;
        for I := 0 to DataSet.FieldCount - 1 do
          if DataSet.Fields[I].DataType in [ftString, ftMemo] then
            Dest.Fields[I].Value := GetFieldValue(DataSet.Fields[I])
          else
            Dest.Fields[I].Value := DataSet.Fields[I].Value;
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
    if Dest <> nil then
      Dest.Close;
    FreeAndNil(Dest);
  end;
end;

procedure TJvgExportXML.Execute;
var
  RecNo, RecCount: Integer;
  XML: TJvSimpleXML;
  Header: TJvSimpleXMLElemClassic;
  Table: TJvSimpleXMLElemClassic;
  Field: TJvSimpleXMLElemClassic;
  Records: TJvSimpleXMLElemClassic;
  XMLRecord: TJvSimpleXMLElemClassic;
  AllowExportRecord: Boolean;
  FieldValue: string;
  I: Integer;

  function CreateNode(const Name: string; Base: TJvSimpleXMLElemClassic):
    TJvSimpleXMLElemClassic;
  begin
    Result := TJvSimpleXMLElemClassic.Create;
    Base.Items.Add(Result);
    Result.Name := Name;
  end;

  procedure AddFieldName(Field: TJvSimpleXMLElemClassic);
  var
    Caption: string;
  begin
    Caption := '';
    if Self.FCaptions = fecDisplayLabels then
      Caption := DataSet.Fields[I].DisplayName;
    if Self.FCaptions = fecFieldNames then
      Caption := DataSet.Fields[I].FullName;
    if Self.FCaptions = fecNone then
      (* empty *);

    if Assigned(FOnGetCaption) then
       FOnGetCaption(Self, DataSet.Fields[I], Caption);
      Field.Properties.Add('Name', Caption);
  end;

begin
  XML := TJvSimpleXML.Create(Self);
  XML.Root.Name := 'Database';
  XML.IndentString := '  ';

  Header := CreateNode('Header', XML.Root);
  Table := CreateNode('Table', Header);
  DataSet.Open;
  RecCount := DataSet.RecordCount;
  for I := 0 to DataSet.FieldCount - 1 do
  begin
    Field := CreateNode('Field', Table);

    AddFieldName(Field);
    Field.Properties.Add('Size', DataSet.Fields[I].Size);
    Field.Properties.Add('DataType', FieldTypeNames[DataSet.Fields[I].DataType]);
    Field.Properties.Add('Blob', BoolToStr(DataSet.Fields[I].IsBlob, True));
    Field.Properties.Add('Required', BoolToStr(DataSet.Fields[I].Required, True));
  end;
  Records := CreateNode('Records', XML.Root);
  DataSet.First;
  RecNo := 0;
  while not DataSet.Eof do
  begin
    Inc(RecNo);
    XMLRecord := CreateNode('Record', Records);
    XMLRecord.Properties.Add('Nr', RecNo);
    AllowExportRecord := True;
    if Assigned(FOnExportRecord) then
      FOnExportRecord(Self, DataSet, AllowExportRecord);
    if AllowExportRecord then
      for I := 0 to DataSet.FieldCount - 1 do
        if not (DataSet.Fields[I].DataType in [ftBlob, ftGraphic,
          ftParadoxOle, ftDBaseOle, ftTypedBinary,
            ftReference, ftDataSet, ftOraBlob, ftOraClob, ftInterface,
            ftIDispatch]) then
        begin
          Field := CreateNode('RecordField', XMLRecord);
          AddFieldName(Field);
          FieldValue := DataSet.Fields[I].AsString;
          if Assigned(FOnExportField) then
            FOnExportField(Self, DataSet.Fields[I], FieldValue);
          Field.Value := FieldValue;
        end;
    DoProgress(0, RecCount, RecNo, '');
    DataSet.Next;
  end;
  DoProgress(0, RecCount, RecCount, '');

  XML.SaveToFile(Self.FSaveToFileName);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
