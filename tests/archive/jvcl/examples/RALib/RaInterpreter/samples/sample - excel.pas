Var ExcelWindow, NewWorkBooks, WorkBook, Cell, Range, Border, WorkSheet,
    Diagram, MyDiagram : Variant;
    Line, Column, LineCount, ColumnCount : Integer;
begin
ExcelWindow := CreateOleObject('Excel.Application');
// сделаем окно Excel видимым и активным
ExcelWindow.Visible := 1;
// устанавливаем имя окна Excel
ExcelWindow.Caption := 'Отчет';
// создадим новую рабочую книгу
NewWorkBooks := ExcelWindow.Workbooks;
WorkBook := NewWorkBooks.Add;
LineCount := 10;
ColumnCount := 5;
// проставим названия строк
For Line := 1 to LineCount do
begin
   Cell := ExcelWindow.Cells(Line + 1, 1);
   Cell.Value := 'Строка ' + Line;
end;
// проставим названия столбцов
for column := 1 to ColumnCount do
begin
   Cell := ExcelWindow.Cells(1, Column + 1);
   Cell.Value := 'Столбец ' + Column;
end;
// заполним ячейки таблицы значениями
for Line := 1 to LineCount do
   for Column := 1 to ColumnCount do
   begin
      Cell := ExcelWindow.Cells(Line + 1, Column + 1);
      Cell.Value := Line + Column;
   end;

// выделим область в таблице и присвоим ее переменной языка
Range := ExcelWindow.Range(ExcelWindow.Cells(1, 1),
                          ExcelWindow.Cells(LineCount + 1, ColumnCount + 1));
// зададим имя выделенной области
Range.Name := 'ОбластьДанных';
// определим рамку выделенной области и присвоим ее переменной языка
Border := Range.Borders;
// установим стили для рамки выделенной области
Border.LineStyle := 1;
Border.ColorIndex := 3;
// построим диаграмму 
WorkSheet := WorkBook.Worksheets(1);
Diagram := WorkSheet.ChartObjects;
Diagram := Diagram.Add(5, 5 + Range.Top + Range.Height,
                          Range.Width, Range.Height);
MyDiagram := Diagram.Chart;
MyDiagram.ChartWizard('ОбластьДанных ', -4102, 6, 1, 1, 1, 1, 'Отчет');
end;
