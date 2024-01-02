var
  Excel : Variant;
  Workbook : Variant;
  Worksheet : Variant;
  Cells : Variant;
  Item : Variant;
  X, Y : Integer;
begin
  Excel := CreateOleObject('Excel.Application');
  try
    // Make a new workbook in Excel
    Workbook := Excel.Workbooks.Add;
    // Create a new worksheet
    Worksheet := Workbook.Worksheets.Add;
    Worksheet.Activate;
    // Fill in some cells with "Hello!"
    Item := Worksheet.Range('A1:B8');
    Item.Value := 'Hello!';
    // Fill in a cell with "Goodbye"
    Cells := Excel.Cells;
    Item := Cells.Item(2,1);
    Item.Value := 'Goodbye';
    // Save the workbook
    Workbook.SaveAs('C:\Temp\Test.xlsx');
  finally
    // Always make sure to tell Excel to quit
    // or it will stay in memory
    Excel.Quit;
  end;
end;
