Var ExcelWindow, NewWorkBooks, WorkBook, Cell, Range, Border, WorkSheet,
    Diagram, MyDiagram : Variant;
    Line, Column, LineCount, ColumnCount : Integer;
begin
ExcelWindow := CreateOleObject('Excel.Application');
// Let's make the Excel window visible and active [translated]
ExcelWindow.Visible := 1;
// We set a name of the Excel window [translated]
// [translated]:
ExcelWindow.Caption := 'Report';
// Let us create the new working book [translated]
NewWorkBooks := ExcelWindow.Workbooks;
WorkBook := NewWorkBooks.Add;
LineCount := 10;
ColumnCount := 5;
// Let's put down names of lines [translated]
For Line := 1 to LineCount do
begin
   Cell := ExcelWindow.Cells(Line + 1, 1);
   // [translated]:
   Cell.Value := 'Line ' + Line;
end;
// Let's put down names of columns [translated]
for column := 1 to ColumnCount do
begin
   Cell := ExcelWindow.Cells(1, Column + 1);
   // [translated]:
   Cell.Value := 'Column ' + Column;
end;
// let us fill the cells of table with values [translated]
for Line := 1 to LineCount do
   for Column := 1 to ColumnCount do
   begin
      Cell := ExcelWindow.Cells(Line + 1, Column + 1);
      Cell.Value := Line + Column;
   end;

// Isolate range in the table and set to its variable of the language [translated]
Range := ExcelWindow.Range(ExcelWindow.Cells(1, 1),
                          ExcelWindow.Cells(LineCount + 1, ColumnCount + 1));
// let us assign the name of the chosen region [translated]
// [translated]:
Range.Name := 'Oblast Dannyh';
// Let's define a framework of the selected area and we shall assign its variable of the language [translated]
Border := Range.Borders;
// Let's set styles for a framework of the selected area [translated]
Border.LineStyle := 1;
Border.ColorIndex := 3;
// Let's construct the diagram [translated]
WorkSheet := WorkBook.Worksheets(1);
Diagram := WorkSheet.ChartObjects;
Diagram := Diagram.Add(5, 5 + Range.Top + Range.Height,
                          Range.Width, Range.Height);
MyDiagram := Diagram.Chart;
// [translated]:
MyDiagram.ChartWizard('Oblast Dannyh ', -4102, 6, 1, 1, 1, 1, 'Report');
end;
