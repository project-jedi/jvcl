{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgCrossTable.PAS, released on 2003-01-15.

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

{
  Компонент позволяет печатать так называемые Cross Tables, разбивая крупные
  таблицы на несколько листов как по ширине, так и по высоте.

  Для построения таблицы необходимо указать исходный набор данных(DataSet)
  и три поля: ColumnFieldName, RowFieldName и ValueFieldName, первые два
  из которых указывают на поля столбцов и строк соответственно, а последнее
  используется для заполнения Cross Table.

  Размеры ячеек в сантиметрах таблицы задаются свойствами
  CaptColWidthInSantim, CaptRowHeightInSantim, ColWidthInSantim и RowHeightInSantim.

  Отступы при печати страниц определяются параметрами свойства IndentsInSantim.

  Цвета и шрифты заголовков, ячеек и итоговых значений таблицы настраиваются
  через свойства Colors и Fonts.

  Заголовок отчета и параметры его выравнивания задаются свойствами Title и
  TitleAlignment соответственно. Выводом заголовока на каждой странице можно управлять
  в обработчике события OnPrintTableElement.

  Компоненту можно передавать DataSet с установленным свойством Filter;
--------------
  Свойство Optons:TPCTOptions;

  TPCTOptions = set of ( fcoIntermediateColResults, fcoIntermediateRowResults,
      fcoColResults, fcoRowResults,
     fcoIntermediateColCaptions, fcoIntermediateRowCaptions,
     fcoIntermediateLeftIndent, fcoIntermediateTopIndent,
     fcoIntermediateRightIndent, fcoIntermediateBottomIndent,
     fcoShowPageNumbers, fcoVertColCaptionsFont );

   fcoIntermediateColResults		- вывод промежуточных итогов по столбцам;
   fcoIntermediateRowResults		- вывод промежуточных итогов по строкам;
   fcoColResults			- вывод итогов по столбцам;
   fcoRowResults			- вывод итогов по строкам;
   fcoIntermediateColCaptions		- отображение заголовков столбцов на каждой странице или только на первой;
   fcoIntermediateRowCaptions		- отображение заголовков строк на каждой странице;
   fcoIntermediateLeftIndent		- использовать левый отступ на каждой странице;
   fcoIntermediateTopIndent		- соотв-но;
   fcoIntermediateRightIndent		- соотв-но;
   fcoIntermediateBottomIndent		- соотв-но;
   fcoShowPageNumbers		- отображение номера страницы в соответствии с разбиением;
   fcoVertColCaptionsFont		- вывод заголовков столбцов вертикальным шрифтом;

--------------

___СОБЫТИЯ___

  OnPrintQuery	- информирует о необходимом кол-ве страниц для вывода таблицы;
                  позволяет отменить печать;

  OnPrintNewPage	- информирует о начале печати очередной страницы; позволяет отменить печать;

  OnPrintTableElement	- информирует о печати каждого элемента таблицы ( заголовка, очередной ячейки );
                                       позволяет изменить значение ячейки, задать индивидуальный шрифт и цвет фона,
                                       установить параметры выравнивания текста, отменить печать;
                                       Параметр TableElement: TPCTableElement = ( teTitle, teCell, teColCapt, teRowCapt,  TeColIRes, teRowIRes, teColRes,
                                        teRowRes ); указывает на тип выводимого элемента.

  OnCalcResult	- если данное событие назначено, то расчет итогов осуществляется Вами. Событие передает  значение текущей ячейки и
                                       значения итогов для данного столбца и строки. Если событие OnCalcResult не назначено, расчет выполняется
                                       компонентом,   причем предполагается, что значение ячейки можно преобразовать в значение single и итоги
                                       рассчитываются как суммы соответствующих столбцов и строк.

  OnDuplicateCellValue  - событие инициируется, если какой-либо паре значений колонки и строки соответствует более одного значения.

___МЕТОДЫ___

    procedure Print; - без слов.
    procedure PreviewTo( Canvas: TCanvas; PageWidth, PageHeight: integer );  - вывод таблицы на произвольный холст(Canvas) с указанием
                                                                               размеров условной таблицы в точках.

}
UNIT JvgCrossTable;

INTERFACE
USES
   Windows,
   Messages,
   Classes,
   Controls,
   Graphics,
   buttons,
   Dialogs,
   JvComponent,
   JvgTypes,
   JvgCommClasses,
   JvgUtils,
   StdCtrls,
   ExtCtrls,
   SysUtils,
   Forms,
   JvgBevel,
   DB,
   DBCtrls,
   Menus,
   DBTables,
   Printers;

CONST
   MAX_COLS                   = 1024;
   MAX_ROWS                   = 1024;
TYPE
   TglPrintingStatus = (fpsContinue, fpsResume, fpsAbort);

   TPrintQueryEvent = PROCEDURE(Sender: TObject;
      ColPageCount, RowPageCount: cardinal;
      VAR CanPrint: Boolean) OF OBJECT;

   TPrintNewPageEvent = PROCEDURE(Sender: TObject;
      ColPageNo, RowPageNo: cardinal;
      VAR PrintingStatus: TglPrintingStatus) OF OBJECT;

   TDrawCellEvent = PROCEDURE(Sender: TObject;
      ColNo, RowNo: cardinal;
      Value: STRING;
      VAR CanPrint: Boolean) OF OBJECT;

   TCalcResultEvent = PROCEDURE(Sender: TObject;
      ColNo, RowNo: cardinal;
      CellValue: STRING;
      IntermediateColResult,
      IntermediateRowResult,
      ColResult,
      RowResult: single) OF OBJECT;

   TDuplicateCellValueEvent = PROCEDURE(Sender: TObject;
      ColNo, RowNo: cardinal;
      Value: STRING;
      VAR UseDuplicateValue: Boolean) OF OBJECT;

   TPCTOptions = SET OF (fcoIntermediateColResults, fcoIntermediateRowResults,
      fcoColResults, fcoRowResults,
      fcoIntermediateColCaptions, fcoIntermediateRowCaptions,
      fcoIntermediateLeftIndent, fcoIntermediateTopIndent,
      fcoIntermediateRightIndent, fcoIntermediateBottomIndent,
      fcoShowPageNumbers, fcoVertColCaptionsFont);

   TPCTableElement = (teTitle, teCell, teColCapt, teRowCapt, teColIRes,
      teRowIRes, teColRes, teRowRes);

   TPrintTableElement = PROCEDURE(Sender: TObject;
      VAR Text: STRING;
      ColNo, RowNo: integer;
      TableElement: TPCTableElement;
      VAR Font: TFont;
      VAR Color: TColor;
      VAR AlignFlags: Word;
      VAR CanPrint: Boolean) OF OBJECT;

   TJvgPrintCrossTableColors = CLASS(TPersistent)
   PRIVATE
      FCaptions, FCells, FResults, FIntermediateResults: TColor;
   PUBLISHED
      PROPERTY Captions: TColor READ FCaptions WRITE FCaptions;
      PROPERTY Cells: TColor READ FCells WRITE FCells;
      PROPERTY Results: TColor READ FResults WRITE FResults;
      PROPERTY IntermediateResults: TColor READ FIntermediateResults WRITE
         FIntermediateResults;
   END;

   TJvgPrintCrossTableFonts = CLASS(TPersistent)
   PRIVATE
      FColCaptions, FRowCaptions, FCells, FResults, FIntermediateResults,
         FTitles: TFont;
      PROCEDURE SetColCaptions(Value: TFont);
      PROCEDURE SetRowCaptions(Value: TFont);
      PROCEDURE SetCells(Value: TFont);
      PROCEDURE SetResults(Value: TFont);
      PROCEDURE SetIntermediateResults(Value: TFont);
      PROCEDURE SetTitles(Value: TFont);
   PUBLIC
      CONSTRUCTOR Create;
      DESTRUCTOR Destroy; OVERRIDE;
   PUBLISHED
      PROPERTY Titles: TFont READ FTitles WRITE SetTitles;
      PROPERTY ColCaptions: TFont READ FColCaptions WRITE SetColCaptions;
      PROPERTY RowCaptions: TFont READ FRowCaptions WRITE SetRowCaptions;
      PROPERTY Cells: TFont READ FCells WRITE SetCells;
      PROPERTY Results: TFont READ FResults WRITE SetResults;
      PROPERTY IntermediateResults: TFont READ FIntermediateResults WRITE
         SetIntermediateResults;
   END;

   TJvgPrintCrossTableIndents = CLASS(TPersistent)
   PRIVATE
      FLeft, FTop,
         FRight, FBottom: single;
   PUBLIC
      //    constructor Create;
      //    destructor Destroy; override;
   PUBLISHED
      PROPERTY _Left: single READ FLeft WRITE FLeft;
      PROPERTY _Top: single READ FTop WRITE FTop;
      PROPERTY _Right: single READ FRight WRITE FRight;
      PROPERTY _Bottom: single READ FBottom WRITE FBottom;
   END;

   TJvgPrintCrossTable = CLASS(TJvComponent)
   PRIVATE
      FDataSet: TDataSet;
      FColumnFieldName: STRING;
      FRowFieldName: STRING;
      FValueFieldName: STRING;
      FVerticalGrid: boolean;
      FHorizontalGrid: boolean;
      FOptions: TPCTOptions;
      FPageWidth: integer;
      FPageHeight: integer;
      FColWidthInSantim: single;
      FRowHeightInSantim: single;
      FIndentsInSantim: TJvgPrintCrossTableIndents;
      FCaptColWidthInSantim: single;
      FCaptRowHeightInSantim: single;
      FFonts: TJvgPrintCrossTableFonts;
      FColors: TJvgPrintCrossTableColors;
      FTitle: STRING;
      FTitleAlignment: TAlignment;

      FOnPrintQuery: TPrintQueryEvent;
      FOnPrintNewPage: TPrintNewPageEvent;
      FOnPrintTableElement: TPrintTableElement;
      FOnCalcResult: TCalcResultEvent;
      FOnDuplicateCellValue: TDuplicateCellValueEvent;

      Font_: TFont;
      Color_: TColor;
      ColsSum: ARRAY[0..MAX_COLS] OF single;
      RowsSum: ARRAY[0..MAX_ROWS] OF single;
      FinalColsSum: ARRAY[0..MAX_COLS] OF single;
      FinalRowsSum: ARRAY[0..MAX_ROWS] OF single;
      Columnslist, RowsList: TStringList;
      ColsOnPage, RowsOnPage, TotalCols, TotalRows: integer;
      ColsOnPage1, RowsOnPage1: integer;
      ColsOnPageX, RowsOnPageX: integer;
      RowPageCount, ColPageCount: integer;
      //    LOGPIXELSX_, LOGPIXELSY_: integer;
      CaptColWidth, CaptRowHeight: integer;
      LeftIndent, TopIndent, RightIndent, BottomIndent: integer;
      ColWidth, RowHeight: integer;
      ColsOnCurrPage, RowsOnCurrPage: integer;

      PROCEDURE PrintTable(Canvas: TCanvas);
      PROCEDURE CalcResults(Str: STRING; ColNo, RowNo: integer);

      PROCEDURE SetColumnFieldName(Value: STRING);
      PROCEDURE SetRowFieldName(Value: STRING);
      PROCEDURE SetValueFieldName(Value: STRING);
      PROCEDURE SetDataSet(Value: TDataSet);
      PROCEDURE SetOptions(Value: TPCTOptions);

      PROCEDURE Notification(AComponent: TComponent; Operation: TOperation);
         OVERRIDE;
      PROCEDURE Loaded; OVERRIDE;
      PROCEDURE DrawGrid(Canvas: TCanvas; ColPageNo, RowPageNo, ColsOnThisPage,
         RowsOnThisPage: integer);
      PROCEDURE DrawCell(Canvas: TCanvas; ColPageNo, RowPageNo, ColNo, RowNo:
         integer; Str: STRING; Element: TPCTableElement);
      PROCEDURE DrawTitle(Canvas: TCanvas; RowPageNo: integer);
      FUNCTION CalcColNo(ColPageNo: integer): integer;
      FUNCTION CalcRowNo(RowPageNo: integer): integer;
   PUBLIC

      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
      PROCEDURE Print;
      PROCEDURE PreviewTo(Canvas: TCanvas; PageWidth, PageHeight: integer);
   PUBLISHED
      PROPERTY DataSet: TDataSet READ FDataSet WRITE SetDataSet;
      PROPERTY ColumnFieldName: STRING READ FColumnFieldName WRITE
         SetColumnFieldName;
      PROPERTY RowFieldName: STRING READ FRowFieldName WRITE SetRowFieldName;
      PROPERTY ValueFieldName: STRING READ FValueFieldName WRITE
         SetValueFieldName;
      PROPERTY Options: TPCTOptions READ FOptions WRITE SetOptions;
      PROPERTY PageWidth: integer READ FPageWidth WRITE FPageWidth;
      PROPERTY PageHeight: integer READ FPageHeight WRITE FPageHeight;
      PROPERTY ColWidthInSantim: single READ FColWidthInSantim WRITE
         FColWidthInSantim;
      PROPERTY RowHeightInSantim: single READ FRowHeightInSantim WRITE
         FRowHeightInSantim;
      PROPERTY IndentsInSantim: TJvgPrintCrossTableIndents READ FIndentsInSantim
         WRITE FIndentsInSantim;
      PROPERTY CaptColWidthInSantim: single READ FCaptColWidthInSantim WRITE
         FCaptColWidthInSantim;
      PROPERTY CaptRowHeightInSantim: single READ FCaptRowHeightInSantim WRITE
         FCaptRowHeightInSantim;
      PROPERTY Fonts: TJvgPrintCrossTableFonts READ FFonts WRITE FFonts;
      PROPERTY Colors: TJvgPrintCrossTableColors READ FColors WRITE FColors;

      PROPERTY OnPrintQuery: TPrintQueryEvent READ FOnPrintQuery WRITE
         FOnPrintQuery;
      PROPERTY OnPrintNewPage: TPrintNewPageEvent READ FOnPrintNewPage WRITE
         FOnPrintNewPage;
      PROPERTY OnPrintTableElement: TPrintTableElement READ FOnPrintTableElement
         WRITE FOnPrintTableElement;
      PROPERTY OnCalcResult: TCalcResultEvent READ FOnCalcResult WRITE
         FOnCalcResult;
      PROPERTY OnDuplicateCellValue: TDuplicateCellValueEvent READ
         FOnDuplicateCellValue WRITE FOnDuplicateCellValue;
      PROPERTY Title: STRING READ FTitle WRITE FTitle;
      PROPERTY TitleAlignment: TAlignment READ FTitleAlignment WRITE
         FTitleAlignment;
   END;

PROCEDURE Register;

IMPLEMENTATION
{~~~~~~~~~~~~~~~~~~~~~~~~~}

PROCEDURE Register;
BEGIN
END;
{~~~~~~~~~~~~~~~~~~~~~~~~~}

CONSTRUCTOR TJvgPrintCrossTableFonts.Create;
BEGIN
   FTitles := TFont.Create;
   FColCaptions := TFont.Create;
   FRowCaptions := TFont.Create;
   FCells := TFont.Create;
   FResults := TFont.Create;
   FIntermediateResults := TFont.Create;
END;

DESTRUCTOR TJvgPrintCrossTableFonts.Destroy;
BEGIN
   FTitles.Free;
   FColCaptions.Free;
   FRowCaptions.Free;
   FCells.Free;
   FResults.Free;
   FIntermediateResults.Free;
   INHERITED;
END;

PROCEDURE TJvgPrintCrossTableFonts.SetTitles(Value: TFont);
BEGIN
   FTitles.Assign(Value);
END;

PROCEDURE TJvgPrintCrossTableFonts.SetColCaptions(Value: TFont);
BEGIN
   FColCaptions.Assign(Value);
END;

PROCEDURE TJvgPrintCrossTableFonts.SetRowCaptions(Value: TFont);
BEGIN
   FRowCaptions.Assign(Value);
END;

PROCEDURE TJvgPrintCrossTableFonts.SetCells(Value: TFont);
BEGIN
   FCells.Assign(Value);
END;

PROCEDURE TJvgPrintCrossTableFonts.SetResults(Value: TFont);
BEGIN
   FResults.Assign(Value);
END;

PROCEDURE TJvgPrintCrossTableFonts.SetIntermediateResults(Value: TFont);
BEGIN
   FIntermediateResults.Assign(Value);
END;

CONSTRUCTOR TJvgPrintCrossTable.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   ColumnsList := TStringList.Create;
   RowsList := TStringList.Create;
   Colors := TJvgPrintCrossTableColors.Create;
   Fonts := TJvgPrintCrossTableFonts.Create;
   Font_ := TFont.Create;
   //  Fonts.Cells.Name := 'Arial';
   {$IFDEF FR_RUS}
   WITH Fonts DO
   BEGIN
      Titles.CharSet := RUSSIAN_CHARSET;
      ColCaptions.CharSet := RUSSIAN_CHARSET;
      RowCaptions.CharSet := RUSSIAN_CHARSET;
      Cells.CharSet := RUSSIAN_CHARSET;
      Results.CharSet := RUSSIAN_CHARSET;
      IntermediateResults.CharSet := RUSSIAN_CHARSET;
   END;
   {$ENDIF}
   FIndentsInSantim := TJvgPrintCrossTableIndents.Create;

   WITH FIndentsInSantim DO
   BEGIN
      _Left := 1;
      _Top := 1;
      _Right := 1;
      _Bottom := 1;
   END;

   ColWidthInSantim := 0.9;
   RowHeightInSantim := 0.5;

   FVerticalGrid := true;
   FHorizontalGrid := true;

   Colors.Captions := $00FFF2D2;
   Colors.Cells := clWhite;
   Colors.Results := $00C5DEC5;
   Colors.IntermediateResults := $00ABCCF1;

   Options := [fcoIntermediateColResults, fcoIntermediateRowResults,
      fcoColResults, fcoRowResults,
      fcoShowPageNumbers];
END;

DESTRUCTOR TJvgPrintCrossTable.Destroy;
BEGIN
   INHERITED;
   ColumnsList.Free;
   RowsList.Free;
   Fonts.Free;
   Colors.Free;
   FIndentsInSantim.Free;
   Font_.Free;
END;

PROCEDURE TJvgPrintCrossTable.Loaded;
BEGIN
   INHERITED;
   IF fcoVertColCaptionsFont IN Options THEN
   BEGIN
      FFonts.ColCaptions.Handle := CreateRotatedFont(Fonts.ColCaptions, 900);
   END
   ELSE
      FFonts.ColCaptions.Handle := CreateRotatedFont(Fonts.ColCaptions, 0);
END;

PROCEDURE TJvgPrintCrossTable.Notification(AComponent: TComponent; Operation:
   TOperation);
BEGIN
   INHERITED;
   IF (Operation = opRemove) AND (AComponent = DataSet) THEN
      DataSet := NIL;
END;

PROCEDURE TJvgPrintCrossTable.DrawGrid(Canvas: TCanvas; ColPageNo, RowPageNo,
   ColsOnThisPage, RowsOnThisPage: integer);
VAR
   Col, Row                   : integer;
BEGIN
   exit;
   IF ColPageNo = ColPageCount - 1 THEN
      inc(ColsOnThisPage);
   IF RowPageNo = RowPageCount - 1 THEN
      inc(RowsOnThisPage);
   IF FVerticalGrid THEN
      FOR Col := 0 TO ColsOnThisPage + 1 DO
         WITH Canvas DO
         BEGIN
            MoveToEx(Handle, CaptColWidth + Col * ColWidth + LeftIndent,
               TopIndent, NIL);
            Windows.LineTo(Handle, CaptColWidth + Col * ColWidth + LeftIndent,
               TopIndent + (RowsOnThisPage + 2) * RowHeight);
         END;
   IF FHorizontalGrid THEN
      FOR Row := 0 TO RowsOnThisPage + 1 DO
         WITH Canvas DO
         BEGIN
            MoveToEx(Handle, LeftIndent, CaptRowHeight + Row * RowHeight +
               TopIndent, NIL);
            Windows.LineTo(Handle, LeftIndent + (ColsOnThisPage + 2) * ColWidth,
               CaptRowHeight + Row * RowHeight + TopIndent);
         END;
END;

FUNCTION TJvgPrintCrossTable.CalcColNo(ColPageNo: integer): integer;
BEGIN
   IF ColPageNo > 0 THEN
      Result := ColsOnPage1
   ELSE
      Result := 0;
   IF ColPageNo > 1 THEN
      inc(Result, ColsOnPageX * (ColPageNo - 1));
END;

FUNCTION TJvgPrintCrossTable.CalcRowNo(RowPageNo: integer): integer;
BEGIN
   IF RowPageNo > 0 THEN
      Result := RowsOnPage1
   ELSE
      Result := 0;
   IF RowPageNo > 1 THEN
      inc(Result, RowsOnPageX * (RowPageNo - 1));
END;

PROCEDURE TJvgPrintCrossTable.DrawCell(Canvas: TCanvas; ColPageNo, RowPageNo,
   ColNo, RowNo: integer; Str: STRING; Element: TPCTableElement);
VAR
   R, R_                      : TRect;
   i, j                       : integer;
   AlignFlags                 : Word;
   fCanPrint                  : boolean;
CONST
   SingleLine                    : ARRAY[boolean] OF integer = (DT_WORDBREAK,
      DT_SINGLELINE);
BEGIN

   WITH Canvas DO
   BEGIN

      IF (ColNo = -1) OR (RowNo = -1) THEN //...Draw Caption
      BEGIN
         i := max(0, ColNo);
         j := max(0, RowNo);
         IF (RowNo = -1) THEN
         BEGIN
            R.Left := i * ColWidth;
            R.Top := 0;
            IF (ColPageNo = 0) OR (fcoIntermediateLeftIndent IN Options) THEN
               inc(R.Left, LeftIndent);
            IF (RowPageNo = 0) OR (fcoIntermediateTopIndent IN Options) THEN
               inc(R.Top, TopIndent);
            IF (ColPageNo = 0) OR (fcoIntermediateRowCaptions IN Options) THEN
               inc(R.Left, CaptColWidth);
            R.Right := R.Left + ColWidth;
            R.Bottom := R.Top + CaptRowHeight;
         END
         ELSE
         BEGIN
            R.Left := 0;
            R.Top := j * RowHeight;
            IF (ColPageNo = 0) OR (fcoIntermediateLeftIndent IN Options) THEN
               inc(R.Left, LeftIndent);
            IF (RowPageNo = 0) OR (fcoIntermediateTopIndent IN Options) THEN
               inc(R.Top, TopIndent);
            IF (RowPageNo = 0) OR (fcoIntermediateColCaptions IN Options) THEN
               inc(R.Top, CaptRowHeight);
            R.Right := R.Left + CaptColWidth;
            R.Bottom := R.Top + RowHeight;
         END;

      END
      ELSE                              //...Draw Cell
      BEGIN
         i := CalcColNo(ColPageNo);
         j := CalcRowNo(RowPageNo);

         R.Left := (ColNo - i + 1) * ColWidth - ColWidth;
         R.Top := (RowNo - j + 1) * RowHeight - RowHeight;

         IF (ColPageNo = 0) OR (fcoIntermediateLeftIndent IN Options) THEN
            inc(R.Left, LeftIndent);
         IF (RowPageNo = 0) OR (fcoIntermediateTopIndent IN Options) THEN
            inc(R.Top, TopIndent);

         IF (RowPageNo = 0) OR (fcoIntermediateColCaptions IN Options) THEN
            inc(R.Top, CaptRowHeight);
         IF (ColPageNo = 0) OR (fcoIntermediateRowCaptions IN Options) THEN
            inc(R.Left, CaptColWidth);

         R.Right := R.Left + ColWidth;
         R.Bottom := R.Top + RowHeight;
         Inc(R.Bottom);
         Inc(R.Right);
      END;

      InflateRect(R, -2, -2);

      WITH Fonts, Brush DO
         CASE Element OF
            teCell:
               BEGIN
                  Font.Assign(Cells);
                  Color := Colors.Cells;
               END;
            teColCapt:
               BEGIN
                  Font.Assign(ColCaptions);
                  Color := Colors.Captions;
               END;
            teRowCapt:
               BEGIN
                  Font.Assign(RowCaptions);
                  Color := Colors.Captions;
               END;
            teColIRes,
               teRowIRes:
               BEGIN
                  Font.Assign(IntermediateResults);
                  Color := Colors.IntermediateResults;
               END;
            teColRes,
               teRowRes:
               BEGIN
                  Font.Assign(Results);
                  Color := Colors.Results;
               END;
         END;

      AlignFlags := SingleLine[(ColNo <> -1) AND (RowNo <> -1)] OR DT_CENTER OR
         DT_VCENTER;

      fCanPrint := true;
      IF Assigned(FOnPrintTableElement) THEN
      BEGIN
         Color_ := Brush.Color;
         Font_.Assign(Font);
         FOnPrintTableElement(self, Str, i + 1, j + 1, Element, Font_, Color_,
            AlignFlags, fCanPrint);
         Font.Assign(Font_);
      END;

      IF NOT fCanPrint THEN
         exit;

      Canvas.FillRect(R);
      Brush.Color := 0;
      InflateRect(R, 1, 1);
      Canvas.FrameRect(R);
      SetBkMode(Handle, TRANSPARENT);

      IF (fcoVertColCaptionsFont IN Options) AND (RowNo = -1) THEN
         ExtTextOut(Handle, R.Left + 5, R.Bottom - 2, ETO_CLIPPED, @r,
            PChar(Str), Length(Str), NIL)
      ELSE
      BEGIN
         R_ := R;
         DrawText(Handle, PChar(Str), -1, R_, DT_CENTER OR DT_WORDBREAK OR
            DT_CALCRECT);
         R.Top := R.Top + max(0, (R.Bottom - R_.Bottom) DIV 2);
         DrawText(Handle, PChar(Str), -1, R, DT_CENTER OR DT_WORDBREAK);
      END;

   END;
END;

PROCEDURE TJvgPrintCrossTable.DrawTitle(Canvas: TCanvas; RowPageNo: integer);
VAR
   fCanPrint                  : boolean;
   Str                        : STRING;
   AlignFlags                 : Word;
   R                          : TRect;
CONST
   Alignments                    : ARRAY[TAlignment] OF Word = (DT_LEFT, DT_RIGHT,
      DT_CENTER);
BEGIN
   WITH Canvas DO
   BEGIN
      Font.Assign(Fonts.Titles);
      IF TopIndent < TextHeight('ky') THEN
         exit;
      IF NOT ((RowPageNo = 0) OR (fcoIntermediateTopIndent IN Options)) THEN
         exit;

      fCanPrint := true;
      AlignFlags := (DT_SINGLELINE OR DT_EXPANDTABS) OR
         Alignments[FTitleAlignment];
      Str := FTitle;
      IF Assigned(FOnPrintTableElement) THEN
      BEGIN
         Color_ := Brush.Color;
         Font_.Assign(Font);
         FOnPrintTableElement(self, Str, -1, -1, teTitle, Font_, Color_,
            AlignFlags, fCanPrint);
         Font.Assign(Font_);
      END;
      IF NOT fCanPrint THEN
         exit;
      R := Rect(LeftIndent, 10, PageWidth - RightIndent, PageHeight -
         BottomIndent);
      SetBkMode(Handle, TRANSPARENT);
      DrawText(Handle, PChar(Str), -1, R, AlignFlags);

   END;
END;

PROCEDURE TJvgPrintCrossTable.Print;
BEGIN
   PrintTable(NIL);
END;

PROCEDURE TJvgPrintCrossTable.PreviewTo(Canvas: TCanvas; PageWidth, PageHeight:
   integer);
BEGIN
   Self.PageWidth := PageWidth;
   Self.PageHeight := PageHeight;
   PrintTable(Canvas);
END;

PROCEDURE TJvgPrintCrossTable.PrintTable(Canvas: TCanvas);
VAR
   i, j                       : integer;
   fPrint, fCanPrint, fUseDuplicateValue: boolean;
   ClientSize                 : TSize;
   PrintingStatus             : TglPrintingStatus;
   str                        : STRING;
   TargetCanvas               : TCanvas;
   ColumnField, RowField, ValueField: TField;
   ColPageNo, RowPageNo, ColNo, RowNo: integer;
   ClientR, CaptR, DataR, R   : TRect;
   FilledRowNo                : ARRAY[0..MAX_ROWS] OF boolean;
   OldFilter                  : STRING;
   OldFiltered                : boolean;
BEGIN
   IF NOT Assigned(FDataSet) THEN
      exit;
   FillChar(FinalColsSum, SizeOf(FinalColsSum), 0);
   FillChar(FinalRowsSum, SizeOf(FinalRowsSum), 0);
   WITH IndentsInSantim DO
   TRY
      WITH FDataSet DO
      BEGIN

         OldFilter := Filter;
         OldFiltered := Filtered;

         Filtered := false;
         ColumnField := FieldByName(ColumnFieldName);
         RowField := FieldByName(RowFieldName);
         ValueField := FieldByName(ValueFieldName);
      END;

      fPrint := NOT Assigned(Canvas);
      IF fPrint THEN
      BEGIN
         TargetCanvas := Printer.Canvas;
         PageWidth := Printer.PageWidth;
         PageHeight := Printer.PageHeight;
      END
      ELSE
      BEGIN
         TargetCanvas := Canvas;
      END;
      IF fPrint THEN
         Printer.BeginDoc;
      WITH TargetCanvas DO
      BEGIN
         //    LOGPIXELSX_ := GetDeviceCaps(Canvas.Handle,LOGPIXELSX);
         //    LOGPIXELSY_ := GetDeviceCaps(Canvas.Handle,LOGPIXELSY);

         ColWidth := SantimsToPixels(Handle, ColWidthInSantim, true);
         RowHeight := SantimsToPixels(Handle, RowHeightInSantim, false);
         LeftIndent := SantimsToPixels(Handle, _Left, true);
         TopIndent := SantimsToPixels(Handle, _Top, false);
         RightIndent := SantimsToPixels(Handle, _Right, true);
         BottomIndent := SantimsToPixels(Handle, _Bottom, false);
         CaptColWidth := SantimsToPixels(Handle, CaptColWidthInSantim, true);
         CaptRowHeight := SantimsToPixels(Handle, CaptRowHeightInSantim, false);

      END;
      //  CaptR := Rect( LeftIndent, TopIndent, PageWidth-RightIndent, PageHeight-BottomIndent );
      //  DataR := CaptR; InflateRect( DataR, -ColWidth, -RowHeight );
        //---------
      WITH FDataSet DO
      BEGIN
         ColumnsList.Clear;
         RowsList.Clear;

         First;
         WHILE NOT EOF DO
         BEGIN
            ColumnsList.Add(ColumnField.AsString);
            RowsList.Add(RowField.AsString);
            Next;
         END;
      END;
      ColumnsList.Sort;
      RowsList.Sort;
      ColumnsList.Sorted := true;
      RowsList.Sorted := true;
      FOR i := ColumnsList.Count - 1 DOWNTO 1 DO
         IF ColumnsList[i - 1] = ColumnsList[i] THEN
            ColumnsList.Delete(i);

      FOR i := RowsList.Count - 1 DOWNTO 1 DO
         IF RowsList[i - 1] = RowsList[i] THEN
            RowsList.Delete(i);

      TotalCols := ColumnsList.Count + 1; //...+1 - final results
      TotalRows := RowsList.Count + 1;

      ClientSize.cx := PageWidth - LeftIndent - RightIndent;
      ClientSize.cy := PageHeight - TopIndent - BottomIndent;

      ColsOnPage1 := (ClientSize.cx - CaptColWidth) DIV ColWidth -
         integer(fcoIntermediateColResults IN Options);
      RowsOnPage1 := (ClientSize.cy - CaptRowHeight) DIV RowHeight -
         integer(fcoIntermediateRowResults IN Options);

      ClientSize.cx := PageWidth;
      ClientSize.cy := PageHeight;

      IF (fcoIntermediateColCaptions IN Options) THEN
         dec(ClientSize.cy, CaptRowHeight);
      IF (fcoIntermediateRowCaptions IN Options) THEN
         dec(ClientSize.cx, CaptColWidth);
      IF (fcoIntermediateLeftIndent IN Options) THEN
         dec(ClientSize.cx, LeftIndent);
      IF (fcoIntermediateTopIndent IN Options) THEN
         dec(ClientSize.cy, TopIndent);
      IF (fcoIntermediateRightIndent IN Options) THEN
         dec(ClientSize.cx, RightIndent);
      IF (fcoIntermediateBottomIndent IN Options) THEN
         dec(ClientSize.cy, BottomIndent);

      ColsOnPageX := (ClientSize.cx) DIV ColWidth -
         integer(fcoIntermediateColResults IN Options);
      RowsOnPageX := (ClientSize.cy) DIV RowHeight -
         integer(fcoIntermediateRowResults IN Options);

      RowPageCount := max(trunc(TotalRows / (RowsOnPageX + 1)), 1);
      ColPageCount := max(trunc(TotalCols / (ColsOnPageX + 1)), 1);

      //...EVENT OnPrintQuery
      fCanPrint := true;
      IF Assigned(FOnPrintQuery) THEN
         FOnPrintQuery(self, ColPageCount, RowPageCount, fCanPrint);
      IF NOT fCanPrint THEN
      BEGIN
         IF fPrint THEN
            Printer.Abort;
         exit;
      END;
      //...
      TargetCanvas.Font := Fonts.Cells;
      TargetCanvas.Brush.Color := 0;
      TargetCanvas.Font.Color := 0;
      SetBkMode(TargetCanvas.Handle, TRANSPARENT);

      FOR RowPageNo := 0 TO RowPageCount - 1 DO
      BEGIN
         FOR ColPageNo := 0 TO ColPageCount - 1 DO
            WITH TargetCanvas DO
            BEGIN

               IF ColPageNo = 0 THEN
                  ColsOnPage := ColsOnPage1
               ELSE
                  ColsOnPage := ColsOnPageX;
               IF RowPageNo = 0 THEN
                  RowsOnPage := RowsOnPage1
               ELSE
                  RowsOnPage := RowsOnPageX;

               FillChar(ColsSum, SizeOf(ColsSum), 0);
               FillChar(RowsSum, SizeOf(RowsSum), 0);

               ColsOnCurrPage := min(ColsOnPage, TotalCols - ColPageNo *
                  ColsOnPage);
               RowsOnCurrPage := min(RowsOnPage, TotalRows - RowPageNo *
                  RowsOnPage);

               IF ColPageNo = ColPageCount - 1 THEN
                  dec(ColsOnCurrPage);
               IF RowPageNo = RowPageCount - 1 THEN
                  dec(RowsOnCurrPage);
               //...EVENT OnPrintNewPage
               PrintingStatus := fpsContinue;
               IF Assigned(OnPrintNewPage) THEN
                  OnPrintNewPage(self, ColPageNo, RowPageNo, PrintingStatus);
               IF PrintingStatus = fpsAbort THEN
               BEGIN
                  IF fPrint THEN
                     Printer.Abort;
                  exit;
               END;
               IF PrintingStatus = fpsResume THEN
               BEGIN
                  IF fPrint THEN
                     Printer.EndDoc;
                  exit;
               END;
               //...
               ClientR := Rect(0, 0, PageWidth, PageHeight);
               Brush.Color := clWhite;
               FillRect(ClientR);
               Brush.Color := 0;
               //      FrameRect(ClientR);
               DrawTitle(TargetCanvas, RowPageNo);
               CaptR := Rect(LeftIndent, TopIndent, LeftIndent + (ColsOnCurrPage
                  + 2) * (ColWidth) + CaptColWidth,
                  TopIndent + (RowsOnCurrPage + 2) * (RowHeight) +
                     CaptRowHeight);
               DataR := CaptR;
               InflateRect(DataR, -CaptColWidth, -CaptRowHeight);

               Brush.Color := clWhite;
               FillRect(CaptR);
               Brush.Color := 0;
               //	  Brush.Color := $0080FFFF; if not fPrint then FillRect(DataR); Brush.Color := 0;
               //	  FrameRect(CaptR);
               SetBkMode(TargetCanvas.Handle, TRANSPARENT);
               IF fcoShowPageNumbers IN Options THEN
                  TextOut(10, 10, '[ ' + IntToStr(RowPageNo + 1) + ' / ' +
                     IntToStr(ColPageNo + 1) + ' ]');

               DrawGrid(TargetCanvas, ColPageNo, RowPageNo, ColsOnCurrPage,
                  RowsOnCurrPage);
               SetBkMode(TargetCanvas.Handle, TRANSPARENT);
               IF (RowPageNo = 0) OR (fcoIntermediateColCaptions IN Options)
                  THEN
                  FOR i := 0 TO ColsOnCurrPage - 1 DO  //...captions_________________________
                  TRY
                     Str := ColumnsList[CalcColNo(ColPageNo) + i];
                     DrawCell(TargetCanvas, ColPageNo, RowPageNo, i, -1, Str,
                        teColCapt);
                  EXCEPT break;
                  END;

               IF (ColPageNo = 0) OR (fcoIntermediateRowCaptions IN Options)
                  THEN
                  FOR i := 0 TO RowsOnCurrPage - 1 DO  //...captions_________________________
                  TRY
                     Str := RowsList[CalcRowNo(RowPageNo) + i];
                     DrawCell(TargetCanvas, ColPageNo, RowPageNo, -1, i, Str,
                        teRowCapt);
                  EXCEPT break;
                  END;

               i := CalcColNo(ColPageNo);
               WITH FDataSet DO
                  FOR ColNo := i TO i + ColsOnCurrPage - 1 DO
                  BEGIN

                     IF ColNo >= ColumnsList.Count THEN
                        break;
                     IF ColumnsList[ColNo] = '' THEN
                        continue;
                     Filtered := false;
                     Filter := '[' + ColumnFieldName + ']=''' +
                        ColumnsList[ColNo] + '''' + OldFilter;
                     Filtered := true;
                     First;

                     FillChar(FilledRowNo, sizeof(FilledRowNo), 0);
                     FOR i := 0 TO RecordCount - 1 DO
                     BEGIN

                        IF NOT RowsList.Find(RowField.AsString, RowNo) THEN
                        BEGIN
                           Next;
                           continue;
                        END;

                        IF (RowNo < CalcRowNo(RowPageNo)) OR (RowNo >=
                           CalcRowNo(RowPageNo) + RowsOnCurrPage) THEN
                        BEGIN
                           Next;
                           continue;
                        END;
                        //	  if not((RowNo >= RowPageNo*(RowsOnCurrPage))and(RowNo <= (RowPageNo+1)*(RowsOnPage)-1))then
                        //	  begin Next; continue; end;

                        fUseDuplicateValue := false;
                        IF FilledRowNo[RowNo] THEN //...duplicate
                        BEGIN
                           IF Assigned(OnDuplicateCellValue) THEN
                              OnDuplicateCellValue(self, ColNo, RowNo,
                                 valueField.AsString, fUseDuplicateValue);

                        END;

                        IF (NOT FilledRowNo[RowNo]) OR fUseDuplicateValue THEN
                        BEGIN
                           FilledRowNo[RowNo] := true;
                           Str := valueField.AsString;
                           DrawCell(TargetCanvas, ColPageNo, RowPageNo,
                              ColNo, RowNo, Str, teCell);
                           CalcResults(Str, ColNo, RowNo);

                        END;

                        Next;
                     END;
                  END;
               //...sums
               i := CalcColNo(ColPageNo);
               j := CalcRowNo(RowPageNo);
               IF fcoIntermediateColResults IN Options THEN
                  FOR ColNo := i TO i + ColsOnCurrPage - 1 DO
                  BEGIN
                     Str := FloatToStr(ColsSum[ColNo]);
                     DrawCell(TargetCanvas, ColPageNo, RowPageNo, ColNo, j +
                        RowsOnCurrPage, Str, teColIRes);
                  END;
               IF fcoIntermediateRowResults IN Options THEN
                  FOR RowNo := j TO j + RowsOnCurrPage - 1 DO
                  BEGIN
                     Str := FloatToStr(RowsSum[RowNo]);
                     DrawCell(TargetCanvas, ColPageNo, RowPageNo, i +
                        ColsOnCurrPage, RowNo, Str, teRowIRes);
                  END;
               //...sums

               IF RowPageNo = RowPageCount - 1 THEN
                  FOR ColNo := i TO i + ColsOnCurrPage - 1 DO
                  BEGIN
                     Str := FloatToStr(FinalColsSum[ColNo]);
                     DrawCell(TargetCanvas, ColPageNo, RowPageNo, ColNo, j +
                        RowsOnCurrPage + integer(fcoIntermediateRowResults IN
                        Options), Str, teColRes);
                  END;
               IF ColPageNo = ColPageCount - 1 THEN
                  FOR RowNo := j TO j + RowsOnCurrPage - 1 DO
                  BEGIN
                     Str := FloatToStr(FinalRowsSum[RowNo]);
                     DrawCell(TargetCanvas, ColPageNo, RowPageNo, i +
                        ColsOnCurrPage + integer(fcoIntermediateColResults IN
                        Options), RowNo, Str, teRowRes);
                  END;
               //...
               IF fPrint AND
                  ((ColPageNo <> ColPageCount - 1) OR
                  (RowPageNo <> RowPageCount - 1)) THEN
                  Printer.NewPage;
            END;
      END;
   FINALLY
      DataSet.Filter := OldFilter;
      DataSet.Filtered := OldFiltered;
   END;
   //...
   IF fPrint THEN
      Printer.EndDoc;

END;

{procedure TJvgPrintCrossTable.SetDataSource(Value: TDataSource);
begin
  FDataSource := Value;
end;}

PROCEDURE TJvgPrintCrossTable.CalcResults(Str: STRING; ColNo, RowNo: integer);
BEGIN
   //...if event is assigned then user should calculates results himself
   IF Assigned(OnCalcResult) THEN
   BEGIN
      OnCalcResult(self,
         ColNo, RowNo,
         Str,                           {CellValue}
         ColsSum[ColNo],                {IntermediateColResult}
         RowsSum[RowNo],                {IntermediateRowResult}
         FinalColsSum[ColNo],           {ColResult}
         FinalRowsSum[RowNo] {RowResult});
   END
   ELSE
   BEGIN
      TRY
         ColsSum[ColNo] := ColsSum[ColNo] + StrToFloat(Str);
      EXCEPT
      END;
      TRY
         FinalColsSum[ColNo] := FinalColsSum[ColNo] + StrToFloat(Str);
      EXCEPT
      END;
      TRY
         RowsSum[RowNo] := RowsSum[RowNo] + StrToFloat(Str);
      EXCEPT
      END;
      TRY
         FinalRowsSum[RowNo] := FinalRowsSum[RowNo] + StrToFloat(Str);
      EXCEPT
      END;
   END;
END;

PROCEDURE TJvgPrintCrossTable.SetDataSet(Value: TDataSet);
BEGIN
   FDataSet := Value;
END;

PROCEDURE TJvgPrintCrossTable.SetColumnFieldName(Value: STRING);
BEGIN
   FColumnFieldName := Value;
END;

PROCEDURE TJvgPrintCrossTable.SetRowFieldName(Value: STRING);
BEGIN
   FRowFieldName := Value;
END;

PROCEDURE TJvgPrintCrossTable.SetValueFieldName(Value: STRING);
BEGIN
   FValueFieldName := Value;
END;

PROCEDURE TJvgPrintCrossTable.SetOptions(Value: TPCTOptions);
BEGIN
   FOptions := Value;
END;

END.

