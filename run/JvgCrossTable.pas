{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgCrossTable.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Burov Dmitry, translation of russian text.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

//  Компонент позволяет печатать так называемые Cross Tables, разбивая крупные
//  таблицы на несколько листов как по ширине, так и по высоте.
//
//  Для построения таблицы необходимо указать исходный набор данных(DataSet)
//  и три поля: ColumnFieldName, RowFieldName и ValueFieldName, первые два
//  из которых указывают на поля столбцов и строк соответственно, а последнее
//  используется для заполнения Cross Table.
//
//  Размеры ячеек в сантиметрах таблицы задаются свойствами
//  CaptColWidthInSantim, CaptRowHeightInSantim, ColWidthInSantim и RowHeightInSantim.
//
//  Отступы при печати страниц определяются параметрами свойства IndentsInSantim.
//
//  Цвета и шрифты заголовков, ячеек и итоговых значений таблицы настраиваются
//  через свойства Colors и Fonts.
//
//  Заголовок отчета и параметры его выравнивания задаются свойствами Title и
//  TitleAlignment соответственно. Выводом заголовока на каждой странице можно управлять
//  в обработчике события OnPrintTableElement.
//
//  Компоненту можно передавать DataSet с установленным свойством Filter;
//--------------
//  Свойство Optons: TPCTOptions;
//
//  TPCTOptions = set of ( fcoIntermediateColResults, fcoIntermediateRowResults,
//      fcoColResults, fcoRowResults,
//     fcoIntermediateColCaptions, fcoIntermediateRowCaptions,
//     fcoIntermediateLeftIndent, fcoIntermediateTopIndent,
//     fcoIntermediateRightIndent, fcoIntermediateBottomIndent,
//     fcoShowPageNumbers, fcoVertColCaptionsFont );
//
//   fcoIntermediateColResults   - вывод промежуточных итогов по столбцам;
//   fcoIntermediateRowResults   - вывод промежуточных итогов по строкам;
//   fcoColResults               - вывод итогов по столбцам;
//   fcoRowResults               - вывод итогов по строкам;
//   fcoIntermediateColCaptions  - отображение заголовков столбцов на каждой странице или только на первой;
//   fcoIntermediateRowCaptions  - отображение заголовков строк на каждой странице;
//   fcoIntermediateLeftIndent   - использовать левый отступ на каждой странице;
//   fcoIntermediateTopIndent    - соотв-но;
//   fcoIntermediateRightIndent  - соотв-но;
//   fcoIntermediateBottomIndent - соотв-но;
//   fcoShowPageNumbers          - отображение номера страницы в соответствии с разбиением;
//   fcoVertColCaptionsFont      - вывод заголовков столбцов вертикальным шрифтом;
//
//--------------
//
//___СОБЫТИЯ___
//
//  OnPrintQuery        - информирует о необходимом кол-ве страниц для вывода таблицы;
//                        позволяет отменить печать;
//
//  OnPrintNewPage      - информирует о начале печати очередной страницы; позволяет отменить печать;
//
//  OnPrintTableElement - информирует о печати каждого элемента таблицы ( заголовка, очередной ячейки );
//                                       позволяет изменить значение ячейки, задать индивидуальный шрифт и цвет фона,
//                                       установить параметры выравнивания текста, отменить печать;
//                                       Параметр TableElement: TPCTableElement = ( teTitle, teCell, teColCapt, teRowCapt,  TeColIRes, teRowIRes, teColRes,
//                                        teRowRes ); указывает на тип выводимого элемента.
//
//  OnCalcResult - если данное событие назначено, то расчет итогов осуществляется Вами. Событие передает  значение текущей ячейки и
//                                       значения итогов для данного столбца и строки. Если событие OnCalcResult не назначено, расчет выполняется
//                                       компонентом,   причем предполагается, что значение ячейки можно преобразовать в значение Single и итоги
//                                       рассчитываются как суммы соответствующих столбцов и строк.
//
//  OnDuplicateCellValue  - событие инициируется, если какой-либо паре значений колонки и строки соответствует более одного значения.
//
//___МЕТОДЫ___
//
//    procedure Print; - без слов.
//    procedure PreviewTo( Canvas: TCanvas; PageWidth, PageHeight: Integer );  - вывод таблицы на произвольный холст(Canvas) с указанием
//                                                                               размеров условной таблицы в точках.
{ [Translation]

  Component allows printing so-called Cross-Tables, splitting large tables
  into several sheets in both height and width.

  Source DataSet, and 3 fields (ColumnFieldName, RowFieldName and ValueFieldName)
  need to be specified to build the table. The table is filled with value of
  ValueFieldName in rows and columns determined by RowFieldName and ColumnFieldName.

  The properties CaptColWidthInSantim, CaptRowHeightInSantim, ColWidthInSantim
  and RowHeightInSantim specify sizes of table in cm.

  Property IndentsInSantim determines indents (margins?) when printing pages.

  Colors and fonts of titles(headers), cells, and aggregates of the table are
  specified with Colors and Fonts properties.

  Report's header and the adjustment of the latter are specified with
  Title and TitleAdjustment properties.

  One can control printing of header on each page by writing an
  OnPrintTableElement event handler.

  DataSet with assigned .Filter can be passed to the component.

  Properties
  ----------

  Options: TPCTOptions;

  TPCTOptions = set of ( fcoIntermediateColResults, fcoIntermediateRowResults,
    fcoColResults, fcoRowResults, fcoIntermediateColCaptions, fcoIntermediateRowCaptions,
    fcoIntermediateLeftIndent, fcoIntermediateTopIndent, fcoIntermediateRightIndent,
    fcoIntermediateBottomIndent, fcoShowPageNumbers, fcoVertColCaptionsFont );

  fcoIntermediateColResults   - Showing intermediate results (summaries, totals, agregates) by columns
  fcoIntermediateRowResults   - Showing intermediate results by rows
  fcoColResults               - Showing results (summaries, totals, agregates) by columns
  fcoRowResults               - Showing results by rows
  fcoIntermediateColCaptions  - showing Column Headers(Titles) on each page or the 1st only.
  fcoIntermediateRowCaptions  - showing Row Headers on each page.
  fcoIntermediateLeftIndent   - Use left indent(margin) on each page.
  fcoIntermediateTopIndent    - Use top indent(margin) on each page.
  fcoIntermediateRightIndent  - Use right indent(margin) on each page.
  fcoIntermediateBottomIndent - Use bottom indent(margin) on each page.
  fcoShowPageNumbers          - Showing pages numbers according to splitting ( of the whole report to pages)
  fcoVertColCaptionsFont      - Showing column headers by vertical font (text? alignment?)

  Events
  ------

  OnPrintQuery         - Tells (informs of) required pages number(count) for
                         printing the table. Allows to cancel printing
  OnPrintNewPage       - Notifies about every new page starting printing (and
                         allows to cancel printing)
  OnPrintTableElement  - Notifies about every new table element (Caption(header,
                         title), each cell) (and allows to cancel printing).
                         Allows to change cell's value, or assign specific color
                         and font to it, to set(customise) parameters of text
                         alignment, to cancel the printing.
                         Parameter TableElement: TPCTableElement = (teTitle, teCell,
                         teColCapt, teRowCapt,  TeColIRes, teRowIRes, teRowRes)
                         specifies type of element being printed.
  OnCalcResult         - If the event is assigned, it is You to process calculation
                         of totals(agregates). Event gives You values of current
                         cell and totals for column and row. If event is not
                         assigned, the component proceeds it, assuming cell value
                         can be casted to Single, and calculating totals as sums
                         of all cells in the row/column
  OnDuplicateCellValue - Event is fired if some pair [Column & Row] value matches
                         another pairs value.

  Methods
  -------

  procedure Print;
    No comment needed.
  procedure PreviewTo( Canvas: TCanvas; PageWidth, PageHeight: Integer );
    Rendering the table to the given canvas, specifying size of conditional(virtual,
    conventional) table in pixels.
}

{$I jvcl.inc}

unit JvgCrossTable;

interface

uses
  Windows, Messages, Classes, Controls, Graphics, Buttons, Dialogs,
  StdCtrls, ExtCtrls, SysUtils, Forms, DB, DBCtrls, Menus, DBTables, Printers,
  JvComponent, JvgTypes, JvgCommClasses, JvgUtils, JvgBevel;

const
  JvDefaultCaptionsColor = TColor($00FFF2D2);
  JvDefaultResultsColor = TColor($00C5DEC5);
  JvDefaultIntermediateResultsColor = TColor($00ABCCF1);

type
  TglPrintingStatus = (fpsContinue, fpsResume, fpsAbort);

  TPrintQueryEvent = procedure(Sender: TObject;
    ColPageCount, RowPageCount: cardinal;
    var CanPrint: Boolean) of object;

  TPrintNewPageEvent = procedure(Sender: TObject;
    ColPageNo, RowPageNo: cardinal;
    var PrintingStatus: TglPrintingStatus) of object;

  TDrawCellEvent = procedure(Sender: TObject;
    ColNo, RowNo: cardinal;
    Value: string;
    var CanPrint: Boolean) of object;

  TCalcResultEvent = procedure(Sender: TObject;
    ColNo, RowNo: cardinal;
    CellValue: string;
    IntermediateColResult,
    IntermediateRowResult,
    ColResult,
    RowResult: Single) of object;

  TDuplicateCellValueEvent = procedure(Sender: TObject;
    ColNo, RowNo: cardinal;
    Value: string;
    var UseDuplicateValue: Boolean) of object;

  TPCTOptions = set of (fcoIntermediateColResults, fcoIntermediateRowResults,
    fcoColResults, fcoRowResults,
    fcoIntermediateColCaptions, fcoIntermediateRowCaptions,
    fcoIntermediateLeftIndent, fcoIntermediateTopIndent,
    fcoIntermediateRightIndent, fcoIntermediateBottomIndent,
    fcoShowPageNumbers, fcoVertColCaptionsFont);

  TPCTableElement = (teTitle, teCell, teColCapt, teRowCapt, teColIRes,
    teRowIRes, teColRes, teRowRes);

  TPrintTableElement = procedure(Sender: TObject;
    var Text: string;
    ColNo, RowNo: Integer;
    TableElement: TPCTableElement;
    var Font: TFont;
    var Color: TColor;
    var AlignFlags: Word;
    var CanPrint: Boolean) of object;

  TJvgPrintCrossTableColors = class(TPersistent)
  private
    FCaptions, FCells, FResults, FIntermediateResults: TColor;
  published
    property Captions: TColor read FCaptions write FCaptions;
    property Cells: TColor read FCells write FCells;
    property Results: TColor read FResults write FResults;
    property IntermediateResults: TColor read FIntermediateResults write
      FIntermediateResults;
  end;

  TJvgPrintCrossTableFonts = class(TPersistent)
  private
    FColCaptions, FRowCaptions, FCells, FResults, FIntermediateResults,
      FTitles: TFont;
    procedure SetColCaptions(Value: TFont);
    procedure SetRowCaptions(Value: TFont);
    procedure SetCells(Value: TFont);
    procedure SetResults(Value: TFont);
    procedure SetIntermediateResults(Value: TFont);
    procedure SetTitles(Value: TFont);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Titles: TFont read FTitles write SetTitles;
    property ColCaptions: TFont read FColCaptions write SetColCaptions;
    property RowCaptions: TFont read FRowCaptions write SetRowCaptions;
    property Cells: TFont read FCells write SetCells;
    property Results: TFont read FResults write SetResults;
    property IntermediateResults: TFont read FIntermediateResults write
      SetIntermediateResults;
  end;

  TJvgPrintCrossTableIndents = class(TPersistent)
  private
    FLeft, FTop,
      FRight, FBottom: Single;
  public
    //    constructor Create;
    //    destructor Destroy; override;
  published
    property _Left: Single read FLeft write FLeft;
    property _Top: Single read FTop write FTop;
    property _Right: Single read FRight write FRight;
    property _Bottom: Single read FBottom write FBottom;
  end;

  TJvgPrintCrossTable = class(TJvComponent)
  private
    FDataSet: TDataSet;
    FColumnFieldName: string;
    FRowFieldName: string;
    FValueFieldName: string;
    FVerticalGrid: Boolean;
    FHorizontalGrid: Boolean;
    FOptions: TPCTOptions;
    FPageWidth: Integer;
    FPageHeight: Integer;
    FColWidthInSantim: Single;
    FRowHeightInSantim: Single;
    FIndentsInSantim: TJvgPrintCrossTableIndents;
    FCaptColWidthInSantim: Single;
    FCaptRowHeightInSantim: Single;
    FFonts: TJvgPrintCrossTableFonts;
    FColors: TJvgPrintCrossTableColors;
    FTitle: string;
    FTitleAlignment: TAlignment;

    FOnPrintQuery: TPrintQueryEvent;
    FOnPrintNewPage: TPrintNewPageEvent;
    FOnPrintTableElement: TPrintTableElement;
    FOnCalcResult: TCalcResultEvent;
    FOnDuplicateCellValue: TDuplicateCellValueEvent;

    Font_: TFont;
    Color_: TColor;
    ColsSum: array of Single;
    RowsSum: array of Single;
    FinalColsSum: array of Single;
    FinalRowsSum: array of Single;
    ColumnsList: TStringList;
    RowsList: TStringList;
    ColsOnPage: Integer;
    RowsOnPage: Integer;
    TotalCols: Integer;
    TotalRows: Integer;
    ColsOnPage1: Integer;
    RowsOnPage1: Integer;
    ColsOnPageX: Integer;
    RowsOnPageX: Integer;
    RowPageCount: Integer;
    ColPageCount: Integer;
    //    LOGPIXELSX_, LOGPIXELSY_: Integer;
    CaptColWidth: Integer;
    CaptRowHeight: Integer;
    LeftIndent: Integer;
    TopIndent: Integer;
    RightIndent: Integer;
    BottomIndent: Integer;
    ColWidth: Integer;
    RowHeight: Integer;
    ColsOnCurrPage: Integer;
    RowsOnCurrPage: Integer;

    procedure PrintTable(Canvas: TCanvas);
    procedure CalcResults(Str: string; ColNo, RowNo: Integer);

    procedure SetColumnFieldName(Value: string);
    procedure SetRowFieldName(Value: string);
    procedure SetValueFieldName(Value: string);
    procedure SetDataSet(Value: TDataSet);
    procedure SetOptions(Value: TPCTOptions);

    procedure DrawGrid(Canvas: TCanvas; ColPageNo, RowPageNo, ColsOnThisPage,
      RowsOnThisPage: Integer);
    procedure DrawCell(Canvas: TCanvas; ColPageNo, RowPageNo, ColNo, RowNo:
      Integer; Str: string; Element: TPCTableElement);
    procedure DrawTitle(Canvas: TCanvas; RowPageNo: Integer);
    function CalcColNo(ColPageNo: Integer): Integer;
    function CalcRowNo(RowPageNo: Integer): Integer;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Print;
    procedure PreviewTo(Canvas: TCanvas; PageWidth, PageHeight: Integer);
  published
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property ColumnFieldName: string read FColumnFieldName write
      SetColumnFieldName;
    property RowFieldName: string read FRowFieldName write SetRowFieldName;
    property ValueFieldName: string read FValueFieldName write
      SetValueFieldName;
    property Options: TPCTOptions read FOptions write SetOptions;
    property PageWidth: Integer read FPageWidth write FPageWidth;
    property PageHeight: Integer read FPageHeight write FPageHeight;
    property ColWidthInSantim: Single read FColWidthInSantim write
      FColWidthInSantim;
    property RowHeightInSantim: Single read FRowHeightInSantim write
      FRowHeightInSantim;
    property IndentsInSantim: TJvgPrintCrossTableIndents read FIndentsInSantim
      write FIndentsInSantim;
    property CaptColWidthInSantim: Single read FCaptColWidthInSantim write
      FCaptColWidthInSantim;
    property CaptRowHeightInSantim: Single read FCaptRowHeightInSantim write
      FCaptRowHeightInSantim;
    property Fonts: TJvgPrintCrossTableFonts read FFonts write FFonts;
    property Colors: TJvgPrintCrossTableColors read FColors write FColors;

    property OnPrintQuery: TPrintQueryEvent read FOnPrintQuery write
      FOnPrintQuery;
    property OnPrintNewPage: TPrintNewPageEvent read FOnPrintNewPage write
      FOnPrintNewPage;
    property OnPrintTableElement: TPrintTableElement read FOnPrintTableElement
      write FOnPrintTableElement;
    property OnCalcResult: TCalcResultEvent read FOnCalcResult write
      FOnCalcResult;
    property OnDuplicateCellValue: TDuplicateCellValueEvent read
      FOnDuplicateCellValue write FOnDuplicateCellValue;
    property Title: string read FTitle write FTitle;
    property TitleAlignment: TAlignment read FTitleAlignment write
      FTitleAlignment;
  end;

implementation

uses
  Math;

const
  MAX_COLS = 1024;
  MAX_ROWS = 1024;

constructor TJvgPrintCrossTableFonts.Create;
begin
  inherited Create;
  FTitles := TFont.Create;
  FColCaptions := TFont.Create;
  FRowCaptions := TFont.Create;
  FCells := TFont.Create;
  FResults := TFont.Create;
  FIntermediateResults := TFont.Create;
end;

destructor TJvgPrintCrossTableFonts.Destroy;
begin
  FTitles.Free;
  FColCaptions.Free;
  FRowCaptions.Free;
  FCells.Free;
  FResults.Free;
  FIntermediateResults.Free;
  inherited Destroy;
end;

procedure TJvgPrintCrossTableFonts.SetTitles(Value: TFont);
begin
  FTitles.Assign(Value);
end;

procedure TJvgPrintCrossTableFonts.SetColCaptions(Value: TFont);
begin
  FColCaptions.Assign(Value);
end;

procedure TJvgPrintCrossTableFonts.SetRowCaptions(Value: TFont);
begin
  FRowCaptions.Assign(Value);
end;

procedure TJvgPrintCrossTableFonts.SetCells(Value: TFont);
begin
  FCells.Assign(Value);
end;

procedure TJvgPrintCrossTableFonts.SetResults(Value: TFont);
begin
  FResults.Assign(Value);
end;

procedure TJvgPrintCrossTableFonts.SetIntermediateResults(Value: TFont);
begin
  FIntermediateResults.Assign(Value);
end;

constructor TJvgPrintCrossTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetLength(ColsSum, MAX_COLS+1);
  SetLength(RowsSum, MAX_ROWS+1);
  SetLength(FinalColsSum, MAX_COLS+1);
  SetLength(FinalRowsSum, MAX_ROWS+1);
  ColumnsList := TStringList.Create;
  RowsList := TStringList.Create;
  Colors := TJvgPrintCrossTableColors.Create;
  Fonts := TJvgPrintCrossTableFonts.Create;
  Font_ := TFont.Create;
  //  Fonts.Cells.Name := 'Arial';
  {$IFDEF FR_RUS}
  with Fonts do
  begin
    Titles.CharSet := RUSSIAN_CHARSET;
    ColCaptions.CharSet := RUSSIAN_CHARSET;
    RowCaptions.CharSet := RUSSIAN_CHARSET;
    Cells.CharSet := RUSSIAN_CHARSET;
    Results.CharSet := RUSSIAN_CHARSET;
    IntermediateResults.CharSet := RUSSIAN_CHARSET;
  end;
  {$ENDIF FR_RUS}
  FIndentsInSantim := TJvgPrintCrossTableIndents.Create;

  with FIndentsInSantim do
  begin
    _Left := 1;
    _Top := 1;
    _Right := 1;
    _Bottom := 1;
  end;

  ColWidthInSantim := 0.9;
  RowHeightInSantim := 0.5;

  FVerticalGrid := True;
  FHorizontalGrid := True;

  Colors.Captions := JvDefaultCaptionsColor;
  Colors.Cells := clWhite;
  Colors.Results := JvDefaultResultsColor;
  Colors.IntermediateResults := JvDefaultIntermediateResultsColor;

  Options := [fcoIntermediateColResults, fcoIntermediateRowResults,
    fcoColResults, fcoRowResults,
    fcoShowPageNumbers];
end;

destructor TJvgPrintCrossTable.Destroy;
begin
  ColumnsList.Free;
  RowsList.Free;
  Fonts.Free;
  Colors.Free;
  FIndentsInSantim.Free;
  Font_.Free;
  inherited Destroy;
end;

procedure TJvgPrintCrossTable.Loaded;
begin
  inherited Loaded;
  if fcoVertColCaptionsFont in Options then
  begin
    FFonts.ColCaptions.Handle := CreateRotatedFont(Fonts.ColCaptions, 900);
  end
  else
    FFonts.ColCaptions.Handle := CreateRotatedFont(Fonts.ColCaptions, 0);
end;

procedure TJvgPrintCrossTable.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = DataSet) then
    DataSet := nil;
end;

procedure TJvgPrintCrossTable.DrawGrid(Canvas: TCanvas; ColPageNo, RowPageNo,
  ColsOnThisPage, RowsOnThisPage: Integer);
var
  Col, Row: Integer;
begin
  exit;
  if ColPageNo = ColPageCount - 1 then
    Inc(ColsOnThisPage);
  if RowPageNo = RowPageCount - 1 then
    Inc(RowsOnThisPage);
  if FVerticalGrid then
    for Col := 0 to ColsOnThisPage + 1 do
      with Canvas do
      begin
        MoveToEx(Handle, CaptColWidth + Col * ColWidth + LeftIndent,
          TopIndent, nil);
        Windows.LineTo(Handle, CaptColWidth + Col * ColWidth + LeftIndent,
          TopIndent + (RowsOnThisPage + 2) * RowHeight);
      end;
  if FHorizontalGrid then
    for Row := 0 to RowsOnThisPage + 1 do
      with Canvas do
      begin
        MoveToEx(Handle, LeftIndent, CaptRowHeight + Row * RowHeight +
          TopIndent, nil);
        Windows.LineTo(Handle, LeftIndent + (ColsOnThisPage + 2) * ColWidth,
          CaptRowHeight + Row * RowHeight + TopIndent);
      end;
end;

function TJvgPrintCrossTable.CalcColNo(ColPageNo: Integer): Integer;
begin
  if ColPageNo > 0 then
    Result := ColsOnPage1
  else
    Result := 0;
  if ColPageNo > 1 then
    Inc(Result, ColsOnPageX * (ColPageNo - 1));
end;

function TJvgPrintCrossTable.CalcRowNo(RowPageNo: Integer): Integer;
begin
  if RowPageNo > 0 then
    Result := RowsOnPage1
  else
    Result := 0;
  if RowPageNo > 1 then
    Inc(Result, RowsOnPageX * (RowPageNo - 1));
end;

procedure TJvgPrintCrossTable.DrawCell(Canvas: TCanvas; ColPageNo, RowPageNo,
  ColNo, RowNo: Integer; Str: string; Element: TPCTableElement);
var
  R, R_: TRect;
  I, J: Integer;
  AlignFlags: Word;
  fCanPrint: Boolean;
const
  SingleLine: array[Boolean] of Integer = (DT_WORDBREAK,
    DT_SINGLELINE);
begin

  with Canvas do
  begin

    if (ColNo = -1) or (RowNo = -1) then //...Draw Caption
    begin
      I := max(0, ColNo);
      J := max(0, RowNo);
      if (RowNo = -1) then
      begin
        R.Left := I * ColWidth;
        R.Top := 0;
        if (ColPageNo = 0) or (fcoIntermediateLeftIndent in Options) then
          Inc(R.Left, LeftIndent);
        if (RowPageNo = 0) or (fcoIntermediateTopIndent in Options) then
          Inc(R.Top, TopIndent);
        if (ColPageNo = 0) or (fcoIntermediateRowCaptions in Options) then
          Inc(R.Left, CaptColWidth);
        R.Right := R.Left + ColWidth;
        R.Bottom := R.Top + CaptRowHeight;
      end
      else
      begin
        R.Left := 0;
        R.Top := J * RowHeight;
        if (ColPageNo = 0) or (fcoIntermediateLeftIndent in Options) then
          Inc(R.Left, LeftIndent);
        if (RowPageNo = 0) or (fcoIntermediateTopIndent in Options) then
          Inc(R.Top, TopIndent);
        if (RowPageNo = 0) or (fcoIntermediateColCaptions in Options) then
          Inc(R.Top, CaptRowHeight);
        R.Right := R.Left + CaptColWidth;
        R.Bottom := R.Top + RowHeight;
      end;

    end
    else //...Draw Cell
    begin
      I := CalcColNo(ColPageNo);
      J := CalcRowNo(RowPageNo);

      R.Left := (ColNo - I + 1) * ColWidth - ColWidth;
      R.Top := (RowNo - J + 1) * RowHeight - RowHeight;

      if (ColPageNo = 0) or (fcoIntermediateLeftIndent in Options) then
        Inc(R.Left, LeftIndent);
      if (RowPageNo = 0) or (fcoIntermediateTopIndent in Options) then
        Inc(R.Top, TopIndent);

      if (RowPageNo = 0) or (fcoIntermediateColCaptions in Options) then
        Inc(R.Top, CaptRowHeight);
      if (ColPageNo = 0) or (fcoIntermediateRowCaptions in Options) then
        Inc(R.Left, CaptColWidth);

      R.Right := R.Left + ColWidth;
      R.Bottom := R.Top + RowHeight;
      Inc(R.Bottom);
      Inc(R.Right);
    end;

    InflateRect(R, -2, -2);

    with Fonts, Brush do
      case Element of
        teCell:
          begin
            Font.Assign(Cells);
            Color := Colors.Cells;
          end;
        teColCapt:
          begin
            Font.Assign(ColCaptions);
            Color := Colors.Captions;
          end;
        teRowCapt:
          begin
            Font.Assign(RowCaptions);
            Color := Colors.Captions;
          end;
        teColIRes,
          teRowIRes:
          begin
            Font.Assign(IntermediateResults);
            Color := Colors.IntermediateResults;
          end;
        teColRes,
          teRowRes:
          begin
            Font.Assign(Results);
            Color := Colors.Results;
          end;
      end;

    AlignFlags := SingleLine[(ColNo <> -1) and (RowNo <> -1)] or DT_CENTER or
      DT_VCENTER;

    fCanPrint := True;
    if Assigned(FOnPrintTableElement) then
    begin
      Color_ := Brush.Color;
      Font_.Assign(Font);
      FOnPrintTableElement(Self, Str, I + 1, J + 1, Element, Font_, Color_,
        AlignFlags, fCanPrint);
      Font.Assign(Font_);
    end;

    if not fCanPrint then
      exit;

    Canvas.FillRect(R);
    Brush.Color := 0;
    InflateRect(R, 1, 1);
    Canvas.FrameRect(R);
    SetBkMode(Handle, TRANSPARENT);

    if (fcoVertColCaptionsFont in Options) and (RowNo = -1) then
      ExtTextOut(Handle, R.Left + 5, R.Bottom - 2, ETO_CLIPPED, @R,
        PChar(Str), Length(Str), nil)
    else
    begin
      R_ := R;
      DrawText(Handle, PChar(Str), -1, R_, DT_CENTER or DT_WORDBREAK or
        DT_CALCRECT);
      R.Top := R.Top + max(0, (R.Bottom - R_.Bottom) div 2);
      DrawText(Handle, PChar(Str), -1, R, DT_CENTER or DT_WORDBREAK);
    end;

  end;
end;

procedure TJvgPrintCrossTable.DrawTitle(Canvas: TCanvas; RowPageNo: Integer);
var
  fCanPrint: Boolean;
  Str: string;
  AlignFlags: Word;
  R: TRect;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT,
    DT_CENTER);
begin
  with Canvas do
  begin
    Font.Assign(Fonts.Titles);
    if TopIndent < TextHeight('ky') then
      exit;
    if not ((RowPageNo = 0) or (fcoIntermediateTopIndent in Options)) then
      exit;

    fCanPrint := True;
    AlignFlags := (DT_SINGLELINE or DT_EXPANDTABS) or
      Alignments[FTitleAlignment];
    Str := FTitle;
    if Assigned(FOnPrintTableElement) then
    begin
      Color_ := Brush.Color;
      Font_.Assign(Font);
      FOnPrintTableElement(Self, Str, -1, -1, teTitle, Font_, Color_,
        AlignFlags, fCanPrint);
      Font.Assign(Font_);
    end;
    if not fCanPrint then
      exit;
    R := Rect(LeftIndent, 10, PageWidth - RightIndent, PageHeight -
      BottomIndent);
    SetBkMode(Handle, TRANSPARENT);
    DrawText(Handle, PChar(Str), -1, R, AlignFlags);

  end;
end;

procedure TJvgPrintCrossTable.Print;
begin
  PrintTable(nil);
end;

procedure TJvgPrintCrossTable.PreviewTo(Canvas: TCanvas; PageWidth, PageHeight:
  Integer);
begin
  Self.PageWidth := PageWidth;
  Self.PageHeight := PageHeight;
  PrintTable(Canvas);
end;

procedure TJvgPrintCrossTable.PrintTable(Canvas: TCanvas);
var
  I, J: Integer;
  fPrint, fCanPrint, fUseDuplicateValue: Boolean;
  ClientSize: TSize;
  PrintingStatus: TglPrintingStatus;
  Str: string;
  TargetCanvas: TCanvas;
  ColumnField, RowField, ValueField: TField;
  ColPageNo, RowPageNo, ColNo, RowNo: Integer;
  ClientR, CaptR, DataR: TRect;
  FilledRowNo: array[0..MAX_ROWS] of Boolean;
  OldFilter: string;
  OldFiltered: Boolean;
begin
  if not Assigned(FDataSet) then
    exit;
  FillChar(FinalColsSum, SizeOf(FinalColsSum), 0);
  FillChar(FinalRowsSum, SizeOf(FinalRowsSum), 0);
  OldFiltered := False;
  with IndentsInSantim do
  try
    with FDataSet do
    begin

      OldFilter := Filter;
      OldFiltered := Filtered;

      Filtered := False;
      ColumnField := FieldByName(ColumnFieldName);
      RowField := FieldByName(RowFieldName);
      ValueField := FieldByName(ValueFieldName);
    end;

    fPrint := not Assigned(Canvas);
    if fPrint then
    begin
      TargetCanvas := Printer.Canvas;
      PageWidth := Printer.PageWidth;
      PageHeight := Printer.PageHeight;
    end
    else
    begin
      TargetCanvas := Canvas;
    end;
    if fPrint then
      Printer.BeginDoc;
    with TargetCanvas do
    begin
      //    LOGPIXELSX_ := GetDeviceCaps(Canvas.Handle,LOGPIXELSX);
      //    LOGPIXELSY_ := GetDeviceCaps(Canvas.Handle,LOGPIXELSY);

      ColWidth := CentimetersToPixels(Handle, ColWidthInSantim, True);
      RowHeight := CentimetersToPixels(Handle, RowHeightInSantim, False);
      LeftIndent := CentimetersToPixels(Handle, _Left, True);
      TopIndent := CentimetersToPixels(Handle, _Top, False);
      RightIndent := CentimetersToPixels(Handle, _Right, True);
      BottomIndent := CentimetersToPixels(Handle, _Bottom, False);
      CaptColWidth := CentimetersToPixels(Handle, CaptColWidthInSantim, True);
      CaptRowHeight := CentimetersToPixels(Handle, CaptRowHeightInSantim, False);

    end;
    //  CaptR := Rect( LeftIndent, TopIndent, PageWidth-RightIndent, PageHeight-BottomIndent );
    //  DataR := CaptR; InflateRect( DataR, -ColWidth, -RowHeight );
      //---------
    with FDataSet do
    begin
      ColumnsList.Clear;
      RowsList.Clear;

      First;
      while not EOF do
      begin
        ColumnsList.Add(ColumnField.AsString);
        RowsList.Add(RowField.AsString);
        Next;
      end;
    end;
    ColumnsList.Sort;
    RowsList.Sort;
    ColumnsList.Sorted := True;
    RowsList.Sorted := True;
    for I := ColumnsList.Count - 1 downto 1 do
      if ColumnsList[I - 1] = ColumnsList[I] then
        ColumnsList.Delete(I);

    for I := RowsList.Count - 1 downto 1 do
      if RowsList[I - 1] = RowsList[I] then
        RowsList.Delete(I);

    TotalCols := ColumnsList.Count + 1; //...+1 - final results
    TotalRows := RowsList.Count + 1;

    ClientSize.cx := PageWidth - LeftIndent - RightIndent;
    ClientSize.cy := PageHeight - TopIndent - BottomIndent;

    ColsOnPage1 := (ClientSize.cx - CaptColWidth) div ColWidth -
      Integer(fcoIntermediateColResults in Options);
    RowsOnPage1 := (ClientSize.cy - CaptRowHeight) div RowHeight -
      Integer(fcoIntermediateRowResults in Options);

    ClientSize.cx := PageWidth;
    ClientSize.cy := PageHeight;

    if (fcoIntermediateColCaptions in Options) then
      Dec(ClientSize.cy, CaptRowHeight);
    if (fcoIntermediateRowCaptions in Options) then
      Dec(ClientSize.cx, CaptColWidth);
    if (fcoIntermediateLeftIndent in Options) then
      Dec(ClientSize.cx, LeftIndent);
    if (fcoIntermediateTopIndent in Options) then
      Dec(ClientSize.cy, TopIndent);
    if (fcoIntermediateRightIndent in Options) then
      Dec(ClientSize.cx, RightIndent);
    if (fcoIntermediateBottomIndent in Options) then
      Dec(ClientSize.cy, BottomIndent);

    ColsOnPageX := (ClientSize.cx) div ColWidth -
      Integer(fcoIntermediateColResults in Options);
    RowsOnPageX := (ClientSize.cy) div RowHeight -
      Integer(fcoIntermediateRowResults in Options);

    RowPageCount := max(trunc(TotalRows / (RowsOnPageX + 1)), 1);
    ColPageCount := max(trunc(TotalCols / (ColsOnPageX + 1)), 1);

    //...EVENT OnPrintQuery
    fCanPrint := True;
    if Assigned(FOnPrintQuery) then
      FOnPrintQuery(Self, ColPageCount, RowPageCount, fCanPrint);
    if not fCanPrint then
    begin
      if fPrint then
        Printer.Abort;
      exit;
    end;
    //...
    TargetCanvas.Font := Fonts.Cells;
    TargetCanvas.Brush.Color := 0;
    TargetCanvas.Font.Color := 0;
    SetBkMode(TargetCanvas.Handle, TRANSPARENT);

    for RowPageNo := 0 to RowPageCount - 1 do
    begin
      for ColPageNo := 0 to ColPageCount - 1 do
        with TargetCanvas do
        begin

          if ColPageNo = 0 then
            ColsOnPage := ColsOnPage1
          else
            ColsOnPage := ColsOnPageX;
          if RowPageNo = 0 then
            RowsOnPage := RowsOnPage1
          else
            RowsOnPage := RowsOnPageX;

          FillChar(ColsSum, SizeOf(ColsSum), 0);
          FillChar(RowsSum, SizeOf(RowsSum), 0);

          ColsOnCurrPage := min(ColsOnPage, TotalCols - ColPageNo *
            ColsOnPage);
          RowsOnCurrPage := min(RowsOnPage, TotalRows - RowPageNo *
            RowsOnPage);

          if ColPageNo = ColPageCount - 1 then
            Dec(ColsOnCurrPage);
          if RowPageNo = RowPageCount - 1 then
            Dec(RowsOnCurrPage);
          //...EVENT OnPrintNewPage
          PrintingStatus := fpsContinue;
          if Assigned(OnPrintNewPage) then
            OnPrintNewPage(Self, ColPageNo, RowPageNo, PrintingStatus);
          if PrintingStatus = fpsAbort then
          begin
            if fPrint then
              Printer.Abort;
            exit;
          end;
          if PrintingStatus = fpsResume then
          begin
            if fPrint then
              Printer.EndDoc;
            exit;
          end;
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
          // Brush.Color := $0080FFFF; if not fPrint then FillRect(DataR); Brush.Color := 0;
          // FrameRect(CaptR);
          SetBkMode(TargetCanvas.Handle, TRANSPARENT);
          if fcoShowPageNumbers in Options then
            TextOut(10, 10, '[ ' + IntToStr(RowPageNo + 1) + ' / ' +
              IntToStr(ColPageNo + 1) + ' ]');

          DrawGrid(TargetCanvas, ColPageNo, RowPageNo, ColsOnCurrPage,
            RowsOnCurrPage);
          SetBkMode(TargetCanvas.Handle, TRANSPARENT);
          if (RowPageNo = 0) or (fcoIntermediateColCaptions in Options) then
            for I := 0 to ColsOnCurrPage - 1 do //...captions_________________________
            try
              Str := ColumnsList[CalcColNo(ColPageNo) + I];
              DrawCell(TargetCanvas, ColPageNo, RowPageNo, I, -1, Str,
                teColCapt);
            except
              Break;
            end;

          if (ColPageNo = 0) or (fcoIntermediateRowCaptions in Options) then
            for I := 0 to RowsOnCurrPage - 1 do //...captions_________________________
            try
              Str := RowsList[CalcRowNo(RowPageNo) + I];
              DrawCell(TargetCanvas, ColPageNo, RowPageNo, -1, I, Str,
                teRowCapt);
            except
              Break;
            end;

          I := CalcColNo(ColPageNo);
          with FDataSet do
            for ColNo := I to I + ColsOnCurrPage - 1 do
            begin

              if ColNo >= ColumnsList.Count then
                Break;
              if ColumnsList[ColNo] = '' then
                Continue;
              Filtered := False;
              Filter := '[' + ColumnFieldName + ']=''' +
                ColumnsList[ColNo] + '''' + OldFilter;
              Filtered := True;
              First;

              FillChar(FilledRowNo, SizeOf(FilledRowNo), 0);
              for I := 0 to RecordCount - 1 do
              begin

                if not RowsList.Find(RowField.AsString, RowNo) then
                begin
                  Next;
                  Continue;
                end;

                if (RowNo < CalcRowNo(RowPageNo)) or (RowNo >=
                  CalcRowNo(RowPageNo) + RowsOnCurrPage) then
                begin
                  Next;
                  Continue;
                end;
                // if not((RowNo >= RowPageNo*(RowsOnCurrPage))and(RowNo <= (RowPageNo+1)*(RowsOnPage)-1))then
                // begin Next; Continue; end;

                fUseDuplicateValue := False;
                if FilledRowNo[RowNo] then //...duplicate
                begin
                  if Assigned(OnDuplicateCellValue) then
                    OnDuplicateCellValue(Self, ColNo, RowNo,
                      ValueField.AsString, fUseDuplicateValue);
                end;

                if (not FilledRowNo[RowNo]) or fUseDuplicateValue then
                begin
                  FilledRowNo[RowNo] := True;
                  Str := ValueField.AsString;
                  DrawCell(TargetCanvas, ColPageNo, RowPageNo,
                    ColNo, RowNo, Str, teCell);
                  CalcResults(Str, ColNo, RowNo);
                end;

                Next;
              end;
            end;
          //...sums
          I := CalcColNo(ColPageNo);
          J := CalcRowNo(RowPageNo);
          if fcoIntermediateColResults in Options then
            for ColNo := I to I + ColsOnCurrPage - 1 do
            begin
              Str := FloatToStr(ColsSum[ColNo]);
              DrawCell(TargetCanvas, ColPageNo, RowPageNo, ColNo, J +
                RowsOnCurrPage, Str, teColIRes);
            end;
          if fcoIntermediateRowResults in Options then
            for RowNo := J to J + RowsOnCurrPage - 1 do
            begin
              Str := FloatToStr(RowsSum[RowNo]);
              DrawCell(TargetCanvas, ColPageNo, RowPageNo, I +
                ColsOnCurrPage, RowNo, Str, teRowIRes);
            end;
          //...sums

          if RowPageNo = RowPageCount - 1 then
            for ColNo := I to I + ColsOnCurrPage - 1 do
            begin
              Str := FloatToStr(FinalColsSum[ColNo]);
              DrawCell(TargetCanvas, ColPageNo, RowPageNo, ColNo, J +
                RowsOnCurrPage + Integer(fcoIntermediateRowResults in
                Options), Str, teColRes);
            end;
          if ColPageNo = ColPageCount - 1 then
            for RowNo := J to J + RowsOnCurrPage - 1 do
            begin
              Str := FloatToStr(FinalRowsSum[RowNo]);
              DrawCell(TargetCanvas, ColPageNo, RowPageNo, I +
                ColsOnCurrPage + Integer(fcoIntermediateColResults in
                Options), RowNo, Str, teRowRes);
            end;
          //...
          if fPrint and
            ((ColPageNo <> ColPageCount - 1) or
            (RowPageNo <> RowPageCount - 1)) then
            Printer.NewPage;
        end;
    end;
  finally
    DataSet.Filter := OldFilter;
    DataSet.Filtered := OldFiltered;
  end;
  //...
  if fPrint then
    Printer.EndDoc;
end;

{procedure TJvgPrintCrossTable.SetDataSource(Value: TDataSource);
begin
  FDataSource := Value;
end;}

procedure TJvgPrintCrossTable.CalcResults(Str: string; ColNo, RowNo: Integer);
begin
  //...if event is assigned then user should calculates results himself
  if Assigned(OnCalcResult) then
  begin
    OnCalcResult(Self,
      ColNo, RowNo,
      Str, {CellValue}
      ColsSum[ColNo], {IntermediateColResult}
      RowsSum[RowNo], {IntermediateRowResult}
      FinalColsSum[ColNo], {ColResult}
      FinalRowsSum[RowNo] {RowResult});
  end
  else
  begin
    try
      ColsSum[ColNo] := ColsSum[ColNo] + StrToFloat(Str);
    except
    end;
    try
      FinalColsSum[ColNo] := FinalColsSum[ColNo] + StrToFloat(Str);
    except
    end;
    try
      RowsSum[RowNo] := RowsSum[RowNo] + StrToFloat(Str);
    except
    end;
    try
      FinalRowsSum[RowNo] := FinalRowsSum[RowNo] + StrToFloat(Str);
    except
    end;
  end;
end;

procedure TJvgPrintCrossTable.SetDataSet(Value: TDataSet);
begin
  FDataSet := Value;
end;

procedure TJvgPrintCrossTable.SetColumnFieldName(Value: string);
begin
  FColumnFieldName := Value;
end;

procedure TJvgPrintCrossTable.SetRowFieldName(Value: string);
begin
  FRowFieldName := Value;
end;

procedure TJvgPrintCrossTable.SetValueFieldName(Value: string);
begin
  FValueFieldName := Value;
end;

procedure TJvgPrintCrossTable.SetOptions(Value: TPCTOptions);
begin
  FOptions := Value;
end;

end.

