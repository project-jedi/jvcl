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
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

UNIT JvgExportComponents;

INTERFACE

USES
   Windows,
   Messages,
   SysUtils,
   Classes,
   JVComponent,
   Graphics,
   Controls,
   Forms,
   Dialogs,
   db,
   dbtables;

TYPE
   TglExportCaptions = (fecDisplayLabels, fecFieldNames, fecNone);
   TGetCaptionEvent = PROCEDURE(Sender: TObject; VAR Caption: STRING) OF OBJECT;
   TExportRecordEvent = PROCEDURE(Sender: TObject; VAR AllowExport: boolean) OF
      OBJECT;
   TExportFieldEvent = PROCEDURE(Sender: TObject; CONST Field: TField; VAR
      FieldValue: STRING) OF OBJECT;
   TGetLineFontEvent = PROCEDURE(Sender: TObject; LineNo: integer; CONST Value:
      STRING; Font: TFont) OF OBJECT;

   EJvgExportException = CLASS(Exception)
   END;

   TJvgCommonExport = CLASS(TJvComponent)
   PRIVATE
      FSaveToFileName: STRING;
      FDataSet: TDataSet;
      FOnExportField: TExportFieldEvent;
      FOnExportRecord: TExportRecordEvent;
      FOnGetCaption: TGetCaptionEvent;
      FCaptions: TglExportCaptions;
      FTransliterateRusToEng: boolean;
      FMaxFieldSize: integer;
      PROCEDURE SetCaptions(CONST Value: TglExportCaptions);
      PROCEDURE SetDataSet(CONST Value: TDataSet);
      PROCEDURE SetOnExportField(CONST Value: TExportFieldEvent);
      PROCEDURE SetOnExportRecord(CONST Value: TExportRecordEvent);
      PROCEDURE SetOnGetCaption(CONST Value: TGetCaptionEvent);
      PROCEDURE SetSaveToFileName(CONST Value: STRING);
      PROCEDURE SetMaxFieldSize(CONST Value: integer);
      PROCEDURE SetTransliterateRusToEng(CONST Value: boolean);
      { Private declarations }
   PROTECTED
      FUNCTION GetFieldValue(CONST Field: TField): STRING;
   PUBLIC
      PROCEDURE Execute; VIRTUAL;
   PROTECTED
      PROPERTY DataSet: TDataSet READ FDataSet WRITE SetDataSet;
      PROPERTY Captions: TglExportCaptions READ FCaptions WRITE SetCaptions;
      PROPERTY SaveToFileName: STRING READ FSaveToFileName WRITE
         SetSaveToFileName;
      {$IFDEF COMPILER5_UP}
      PROPERTY TransliterateRusToEng: boolean READ FTransliterateRusToEng WRITE
         SetTransliterateRusToEng;
      {$ENDIF}
      PROPERTY MaxFieldSize: integer READ FMaxFieldSize WRITE SetMaxFieldSize;

      PROPERTY OnGetCaption: TGetCaptionEvent READ FOnGetCaption WRITE
         SetOnGetCaption;
      PROPERTY OnExportRecord: TExportRecordEvent READ FOnExportRecord WRITE
         SetOnExportRecord;
      PROPERTY OnExportField: TExportFieldEvent READ FOnExportField WRITE
         SetOnExportField;
   END;

   TJvgExportExcel = CLASS(TJvgCommonExport)
   PRIVATE
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
      PROCEDURE SetHeader(CONST Value: TStrings);
      PROCEDURE SetFooter(CONST Value: TStrings);
      PROCEDURE SetBackgroundPicture(CONST Value: TFileName);
      PROCEDURE SetAutoColumnFit(CONST Value: boolean);
      PROCEDURE SetExcelVisible(CONST Value: boolean);
      PROCEDURE SetCloseExcel(CONST Value: boolean);
      PROCEDURE SetOnGetFooterLineFont(CONST Value: TGetLineFontEvent);
      PROCEDURE SetOnGetHeaderLineFont(CONST Value: TGetLineFontEvent);
      PROCEDURE SetSubHeader(CONST Value: TStrings);
      PROCEDURE SetFooterFont(CONST Value: TFont);
      PROCEDURE SetHeaderFont(CONST Value: TFont);
      PROCEDURE SetSubHeaderFont(CONST Value: TFont);
      PROCEDURE SetOnGetSubHeaderLineFont(CONST Value: TGetLineFontEvent);
      { Private declarations }
   PROTECTED
      { Protected declarations }
   PUBLIC
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;

      PROCEDURE Execute; OVERRIDE;
   PUBLISHED
      PROPERTY DataSet;
      PROPERTY Captions;
      PROPERTY SaveToFileName;
      {$IFDEF COMPILER5_UP}
      PROPERTY TransliterateRusToEng;
      {$ENDIF}
      PROPERTY MaxFieldSize;
      PROPERTY OnGetCaption;
      PROPERTY OnExportRecord;
      PROPERTY OnExportField;

      PROPERTY Header: TStrings READ FHeader WRITE SetHeader;
      PROPERTY SubHeader: TStrings READ FSubHeader WRITE SetSubHeader;
      PROPERTY Footer: TStrings READ FFooter WRITE SetFooter;
      PROPERTY HeaderFont: TFont READ FHeaderFont WRITE SetHeaderFont;
      PROPERTY SubHeaderFont: TFont READ FSubHeaderFont WRITE SetSubHeaderFont;
      PROPERTY FooterFont: TFont READ FFooterFont WRITE SetFooterFont;
      PROPERTY AutoColumnFit: boolean READ FAutoColumnFit WRITE SetAutoColumnFit
         DEFAULT true;
      PROPERTY BackgroundPicture: TFileName READ FBackgroundPicture WRITE
         SetBackgroundPicture;
      PROPERTY ExcelVisible: boolean READ FExcelVisible WRITE SetExcelVisible;
      PROPERTY CloseExcel: boolean READ FCloseExcel WRITE SetCloseExcel;

      PROPERTY OnGetHeaderLineFont: TGetLineFontEvent READ FOnGetHeaderLineFont
         WRITE SetOnGetHeaderLineFont;
      PROPERTY OnGetSubHeaderLineFont: TGetLineFontEvent READ
         FOnGetSubHeaderLineFont WRITE SetOnGetSubHeaderLineFont;
      PROPERTY OnGetFooterLineFont: TGetLineFontEvent READ FOnGetFooterLineFont
         WRITE SetOnGetFooterLineFont;
   END;

   TJvgExportDBETable = CLASS(TJvgCommonExport)
   PRIVATE
      FTableType: TTableType;
      PROCEDURE SetTableType(CONST Value: TTableType);
      { Private declarations }
   PROTECTED
      { Protected declarations }
   PUBLIC
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      //    destructor Destroy; override;

      PROCEDURE Execute; OVERRIDE;
   PUBLISHED
      PROPERTY DataSet;
      PROPERTY Captions;
      PROPERTY SaveToFileName;
      {$IFDEF COMPILER5_UP}
      PROPERTY TransliterateRusToEng;
      {$ENDIF}
      PROPERTY MaxFieldSize;
      PROPERTY OnGetCaption;
      PROPERTY OnExportRecord;
      PROPERTY OnExportField;

      PROPERTY TableType: TTableType READ FTableType WRITE SetTableType DEFAULT
         ttDBase;
   END;

   TJvgExportHTML = CLASS(TJvgCommonExport)
   PRIVATE
      FFooter: TStrings;
      FHeader: TStrings;
      FStyles: TStrings;
      PROCEDURE SetFooter(CONST Value: TStrings);
      PROCEDURE SetHeader(CONST Value: TStrings);
      PROCEDURE SetStyles(CONST Value: TStrings);
      { Private declarations }
   PROTECTED
      { Protected declarations }
   PUBLIC
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;

      //    procedure Execute; override;
   PUBLISHED
      PROPERTY DataSet;
      PROPERTY Captions;
      PROPERTY SaveToFileName;
      {$IFDEF COMPILER5_UP}
      PROPERTY TransliterateRusToEng;
      {$ENDIF}
      PROPERTY MaxFieldSize;
      PROPERTY OnGetCaption;
      PROPERTY OnExportRecord;
      PROPERTY OnExportField;

      PROPERTY Header: TStrings READ FHeader WRITE SetHeader;
      PROPERTY Footer: TStrings READ FFooter WRITE SetFooter;
      PROPERTY Styles: TStrings READ FStyles WRITE SetStyles;
   END;

   TJvgExportXML = CLASS(TJvgCommonExport)
   PRIVATE
      { Private declarations }
   PROTECTED
      { Protected declarations }
   PUBLIC
      //    constructor Create(AOwner: TComponent); override;
      //    destructor Destroy; override;

      //    procedure Execute; override;
   PUBLISHED
      PROPERTY DataSet;
      PROPERTY Captions;
      PROPERTY SaveToFileName;
      {$IFDEF COMPILER5_UP}
      PROPERTY TransliterateRusToEng;
      {$ENDIF}
      PROPERTY MaxFieldSize;
      PROPERTY OnGetCaption;
      PROPERTY OnExportRecord;
      PROPERTY OnExportField;

   END;

IMPLEMENTATION
USES ComObj,
   FileCtrl,
   JvgUtils,
   JvgFileUtils;

{ TJvgCommonExport }

PROCEDURE TJvgCommonExport.Execute;
BEGIN
   IF NOT Assigned(DataSet) THEN
      RAISE EJvgExportException.Create('DataSet is unassigned');
   DataSet.Active := true;
   IF SaveToFileName <> '' THEN
      ForceDirectories(ExtractFilePath(SaveToFileName));
END;

PROCEDURE TJvgCommonExport.SetCaptions(CONST Value: TglExportCaptions);
BEGIN
   FCaptions := Value;
END;

PROCEDURE TJvgCommonExport.SetDataSet(CONST Value: TDataSet);
BEGIN
   FDataSet := Value;
END;

PROCEDURE TJvgCommonExport.SetMaxFieldSize(CONST Value: integer);
BEGIN
   FMaxFieldSize := Value;
END;

PROCEDURE TJvgCommonExport.SetOnExportField(CONST Value: TExportFieldEvent);
BEGIN
   FOnExportField := Value;
END;

PROCEDURE TJvgCommonExport.SetOnExportRecord(
   CONST Value: TExportRecordEvent);
BEGIN
   FOnExportRecord := Value;
END;

PROCEDURE TJvgCommonExport.SetOnGetCaption(CONST Value: TGetCaptionEvent);
BEGIN
   FOnGetCaption := Value;
END;

PROCEDURE TJvgCommonExport.SetSaveToFileName(CONST Value: STRING);
BEGIN
   FSaveToFileName := trim(Value);
END;

PROCEDURE TJvgCommonExport.SetTransliterateRusToEng(CONST Value: boolean);
BEGIN
   FTransliterateRusToEng := Value;
END;

FUNCTION TJvgCommonExport.GetFieldValue(CONST Field: TField): STRING;
BEGIN
   Result := Field.AsString;
   IF Assigned(OnExportField) THEN
      OnExportField(self, Field, Result);

   {$IFDEF COMPILER5_UP}
   IF FTransliterateRusToEng THEN
      Result := Transliterate(Result, true);
   {$ENDIF}

   IF (FMaxFieldSize > 0) AND (Field.DataType IN [ftString, ftMemo, ftFmtMemo])
      THEN
   BEGIN
      IF length(Result) > FMaxFieldSize THEN
         Result := copy(Result, 1, FMaxFieldSize) + '...';
   END;
END;

{ TJvgExportExcel }

CONSTRUCTOR TJvgExportExcel.Create(AOwner: TComponent);
BEGIN
   INHERITED Create(AOwner);
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
END;

DESTRUCTOR TJvgExportExcel.Destroy;
BEGIN
   FFooter.Free;
   FHeader.Free;
   FSubHeader.Free;
   FHeaderFont.Free;
   FSubHeaderFont.Free;
   FFooterFont.Free;
   INHERITED Destroy;
END;

PROCEDURE TJvgExportExcel.Execute;
VAR
   XL                         : variant;
   Sheet                      : variant;
   AllowExportRecord          : boolean;
   i, j, RecNo, ColNo, OldRecNo: integer;
   CellFont                   : TFont;

   PROCEDURE InsertStrings(Strings: TStrings; Font: TFont; GetLineFontEvent:
      TGetLineFontEvent);
   VAR
      i                       : integer;
   BEGIN
      FOR i := 0 TO Strings.Count - 1 DO
      BEGIN
         Sheet.Cells[RecNo, ColNo] := Strings[i];
         CellFont.Assign(Font);
         IF Assigned(FOnGetHeaderLineFont) THEN
            OnGetHeaderLineFont(self, i, Strings[i], CellFont);

         Sheet.Cells[RecNo, ColNo].Font.Size := CellFont.Size;
         Sheet.Cells[RecNo, ColNo].Font.Color := CellFont.Color;
         IF fsBold IN CellFont.Style THEN
            Sheet.Cells[RecNo, ColNo].Font.Bold := true;
         IF fsItalic IN CellFont.Style THEN
            Sheet.Cells[RecNo, ColNo].Font.Italic := true;
         inc(RecNo);
      END;
   END;

BEGIN
   INHERITED Execute;

   TRY
      XL := GetActiveOleObject('Excel.Application');
   EXCEPT
      XL := CreateOleObject('Excel.Application');
   END;

   XL.Visible := FExcelVisible;
   XL.WorkBooks.Add;
   XL.WorkBooks[XL.WorkBooks.Count].WorkSheets[1].Name := 'Report';
   Sheet := XL.WorkBooks[XL.WorkBooks.Count].WorkSheets['Report'];
   IF (BackgroundPicture <> '') AND FileExists(BackgroundPicture) THEN
      Sheet.SetBackgroundPicture(FileName := BackgroundPicture);

   CellFont := TFont.Create;
   TRY
      RecNo := 1;
      ColNo := 1;

      inc(RecNo, Header.Count + SubHeader.Count);

      IF FCaptions <> fecNone THEN
         FOR i := 0 TO DataSet.FieldCount - 1 DO
         BEGIN
            CASE FCaptions OF
               fecDisplayLabels:
                  IF DataSet.Fields[i].DisplayLabel <> '' THEN
                     Sheet.Cells[RecNo, ColNo + i] :=
                        DataSet.Fields[i].DisplayLabel
                  ELSE
                     Sheet.Cells[RecNo, ColNo + i] :=
                        DataSet.Fields[i].FieldName;
               fecFieldNames:
                  Sheet.Cells[RecNo, ColNo + i] := DataSet.Fields[i].FieldName;
            END;
            Sheet.Cells[RecNo, ColNo + i].Font.Bold := true;
            Sheet.Cells[RecNo, ColNo + i].Font.Size := 10;
         END;

      inc(RecNo);
      DataSet.First;
      WHILE NOT DataSet.EOF DO
      BEGIN
         AllowExportRecord := true;
         IF Assigned(OnExportRecord) THEN
            OnExportRecord(self, AllowExportRecord);
         IF AllowExportRecord THEN
         BEGIN
            FOR i := 0 TO DataSet.FieldCount - 1 DO
               IF NOT (DataSet.Fields[i].DataType IN [ftBlob, ftGraphic,
                  ftParadoxOle, ftDBaseOle, ftTypedBinary{$IFDEF COMPILER5_UP},
                  ftReference, ftDataSet, ftOraBlob, ftOraClob, ftInterface,
                  ftIDispatch{$ENDIF}]) THEN
                  Sheet.Cells[RecNo, ColNo + i] :=
                     GetFieldValue(DataSet.Fields[i]);

            inc(RecNo);
         END;
         DataSet.Next;
      END;

      IF FAutoColumnFit THEN
         FOR i := 0 TO DataSet.FieldCount - 1 DO
            Sheet.Columns[i + 1].EntireColumn.AutoFit;

      OldRecNo := RecNo;
      RecNo := 1;
      InsertStrings(Header, HeaderFont, FOnGetHeaderLineFont);
      InsertStrings(SubHeader, SubHeaderFont, FOnGetSubHeaderLineFont);
      RecNo := OldRecNo + 1;
      InsertStrings(Footer, SubHeaderFont, FOnGetSubHeaderLineFont);

      IF ExtractFileExt(FSaveToFileName) = '' THEN
         FSaveToFileName := DelFileExt(FSaveToFileName) + '.xls';
      IF FileExists(FSaveToFileName) THEN
         DeleteFileEx(FSaveToFileName);

      IF FSaveToFileName <> '' THEN
         XL.WorkBooks[XL.WorkBooks.Count].SaveAs(FSaveToFileName);

      IF CloseExcel THEN
         XL.Quit;

   FINALLY
      CellFont.Free;
   END;
END;

PROCEDURE TJvgExportExcel.SetAutoColumnFit(CONST Value: boolean);
BEGIN
   FAutoColumnFit := Value;
END;

PROCEDURE TJvgExportExcel.SetBackgroundPicture(CONST Value: TFileName);
BEGIN
   FBackgroundPicture := Value;
END;

PROCEDURE TJvgExportExcel.SetCloseExcel(CONST Value: boolean);
BEGIN
   FCloseExcel := Value;
END;

PROCEDURE TJvgExportExcel.SetExcelVisible(CONST Value: boolean);
BEGIN
   FExcelVisible := Value;
END;

PROCEDURE TJvgExportExcel.SetFooter(CONST Value: TStrings);
BEGIN
   FFooter.Assign(Value);
END;

PROCEDURE TJvgExportExcel.SetFooterFont(CONST Value: TFont);
BEGIN
   FFooterFont.Assign(Value);
END;

PROCEDURE TJvgExportExcel.SetHeader(CONST Value: TStrings);
BEGIN
   FHeader.Assign(Value);
END;

PROCEDURE TJvgExportExcel.SetHeaderFont(CONST Value: TFont);
BEGIN
   FHeaderFont.Assign(Value);
END;

PROCEDURE TJvgExportExcel.SetOnGetFooterLineFont(CONST Value:
   TGetLineFontEvent);
BEGIN
   FOnGetFooterLineFont := Value;
END;

PROCEDURE TJvgExportExcel.SetOnGetHeaderLineFont(CONST Value:
   TGetLineFontEvent);
BEGIN
   FOnGetHeaderLineFont := Value;
END;

PROCEDURE TJvgExportExcel.SetOnGetSubHeaderLineFont(CONST Value:
   TGetLineFontEvent);
BEGIN
   FOnGetSubHeaderLineFont := Value;
END;

PROCEDURE TJvgExportExcel.SetSubHeader(CONST Value: TStrings);
BEGIN
   FSubHeader.Assign(Value);
END;

PROCEDURE TJvgExportExcel.SetSubHeaderFont(CONST Value: TFont);
BEGIN
   FSubHeaderFont.Assign(Value);
END;

{ TJvgExportHTML }

CONSTRUCTOR TJvgExportHTML.Create(AOwner: TComponent);
BEGIN
   INHERITED Create(AOwner);
   FFooter := TStringList.Create;
   FHeader := TStringList.Create;
   FStyles := TStringList.Create;
END;

DESTRUCTOR TJvgExportHTML.Destroy;
BEGIN
   FFooter.Free;
   FHeader.Free;
   FStyles.Free;
   INHERITED Destroy;
END;

PROCEDURE TJvgExportHTML.SetFooter(CONST Value: TStrings);
BEGIN
   FFooter.Assign(Value);
END;

PROCEDURE TJvgExportHTML.SetHeader(CONST Value: TStrings);
BEGIN
   FHeader.Assign(Value);
END;

PROCEDURE TJvgExportHTML.SetStyles(CONST Value: TStrings);
BEGIN
   FStyles.Assign(Value);
END;

{ TJvgExportDBETable }

CONSTRUCTOR TJvgExportDBETable.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   //...defailts
   TableType := ttDBase;
END;

PROCEDURE TJvgExportDBETable.Execute;
VAR
   i                          : integer;
   Table                      : TTable;
   AllowExportRecord          : boolean;
   FieldType                  : TFieldType;
CONST
   {$IFDEF COMPILER4_UP}
   aTableTypeExt                 : ARRAY[TTableType] OF STRING = ('', 'db', 'dbf',
      'dbf', 'txt');
   {$ELSE}
   aTableTypeExt                 : ARRAY[TTableType] OF STRING = ('', 'db', 'dbf',
      'txt');
   {$ENDIF}
BEGIN
   INHERITED;

   IF SaveToFileName = '' THEN
      RAISE EJvgExportException.Create('SaveToFileName property is empty');

   Table := TTable.Create(NIL);

   Table.TableType := TableType;
   Table.TableName := SaveToFileName;
   //  if ExtractFileExt(Table.TableName) = '' then Table.TableName := DelFileExt() + aTableTypeExt[TableType];

   FieldType := DataSet.Fields[i].DataType;
   IF FieldType = ftAutoInc THEN
      FieldType := ftInteger;

   FOR i := 0 TO DataSet.FieldCount - 1 DO
      Table.FieldDefs.Add(DataSet.Fields[i].Name, FieldType,
         DataSet.Fields[i].Size, DataSet.Fields[i].Required);

   Table.CreateTable;
   Table.Open;

   TRY
      DataSet.First;
      WHILE NOT DataSet.EOF DO
      BEGIN
         AllowExportRecord := true;
         IF Assigned(OnExportRecord) THEN
            OnExportRecord(self, AllowExportRecord);
         IF AllowExportRecord THEN
         BEGIN
            Table.Append;
            FOR i := 0 TO DataSet.FieldCount - 1 DO
               IF DataSet.Fields[i].DataType IN [ftString, ftMemo] THEN
                  Table.Fields[i].Value := GetFieldValue(DataSet.Fields[i])
               ELSE
                  Table.Fields[i].Value := DataSet.Fields[i].Value;
            Table.Post;
         END;
         DataSet.Next;
      END;
      Table.Close;
   EXCEPT
      Table.Free;
   END;

END;

PROCEDURE TJvgExportDBETable.SetTableType(CONST Value: TTableType);
BEGIN
   IF Value = ttDefault THEN
      FTableType := ttDBase
   ELSE
      FTableType := Value;
END;

END.

