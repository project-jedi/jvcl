{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgExport.PAS, released on 2003-01-15.

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

unit JvgExport;

interface
uses Windows, Messages, Graphics, ExtCtrls, SysUtils, Classes, Controls, Forms,
  JvgTypes, QuickRpt, QRExport, DB;

type
  TOnExportProgress = procedure(Progress: integer) of object;

procedure ExportToExcel(QuickRep: TCustomQuickRep);
procedure ExportDataSetToExcel(DataSet: TDataSet; OnExportProgress: TOnExportProgress);

implementation
uses ComObj, JvgUtils;
//_________________________________________________________________\\

procedure ExportToExcel(QuickRep: TCustomQuickRep);
const
  TEMP_FILE = 'c:\temp_REPORT.txt';
type
  PPChar = ^Char;
  TParr = ^Tarr;
  Tarr = array[0..0] of char;
var
  arr: Tarr;
  Parr: TParr;
  XL: variant;
  Sheet: variant;
  i, j, RecNo: integer;
  sl, sl2: TStringList;
  AExportFilter: TQRCommaSeparatedFilter;
  ms: TMemoryStream;

  function DeleteEOLs(str: string): string;
  var
    i: integer;
  begin
    for i := 1 to Length(str) do
      if (str[i] = #13) then str[i] := ' ';
    Result := str;
  end;
begin

  try
    XL := GetActiveOleObject('Excel.Application');
  except
    XL := CreateOleObject('Excel.Application');
  end;

  AExportFilter := TQRCommaSeparatedFilter.Create(TEMP_FILE);
  try
    QuickRep.ExportToFilter(AExportFilter);
  finally
    AExportFilter.Free;
  end;

  XL.Visible := true;
  XL.WorkBooks.Add;
  XL.WorkBooks[XL.WorkBooks.Count].WorkSheets[1].Name := 'Report';
  Sheet := XL.WorkBooks[XL.WorkBooks.Count].WorkSheets['Report'];

  sl := TStringList.Create;
  sl2 := TStringList.Create;
  try
    //  Sheet.SetBackgroundPicture(FileName:=ExtractFilePath(ParamStr(0))+'data\bg.JPG');
    //  Sheet.Cells[1, 1] := 'Biblio'; Sheet.Cells[1, 1].Font.Color := $FFFFFF;
    //  Sheet.Cells[2, 1] := 'Globus'; Sheet.Cells[2, 1].Font.Color := $FFFFFF;
    //  Sheet.Columns[1].ColumnWidth := 11;

    RecNo := 1;
    Sheet.Cells[RecNo, 1] := 'Отчет создан ' + DateToStr(date);
    Sheet.Cells[RecNo, 1].Font.Italic := True;
    inc(RecNo);
    Sheet.Cells[RecNo, 1] := 'Пользователь ' + UserName;
    Sheet.Cells[RecNo, 1].Font.Italic := True;

    inc(RecNo, 2);
    Sheet.Cells[RecNo, 1] := '';
    Sheet.Cells[RecNo, 1].Font.Bold := true;
    Sheet.Cells[RecNo, 1].Font.Size := 14;

    inc(RecNo, 2);

    ms := TMemoryStream.Create;
    ms.LoadFromFile(TEMP_FILE);
    Parr := ms.Memory;
    {$R-}
    for i := 0 to ms.Size - 1 do
      if Parr[i] = chr(0) then Parr[i] := ',';

    sl.LoadFromStream(ms);
    ms.Free;
    for i := 0 to sl.Count - 1 do
    begin
      sl2.CommaText := sl[i];
      for j := 0 to sl2.Count - 1 do
        Sheet.Cells[RecNo, 1 + j] := sl2[j];

      inc(RecNo);
    end;

  finally
    sl.Free;
    sl2.Free;
    if FileExists(TEMP_FILE) then
      DeleteFile(TEMP_FILE);
  end;
end;
//------------------------------------------------------------------------------

procedure ExportDataSetToExcel(DataSet: TDataSet; OnExportProgress: TOnExportProgress);
var
  XL: variant;
  Sheet: variant;
  i, j, RecNo, ColIndex: integer;
begin
  try
    XL := GetActiveOleObject('Excel.Application');
  except
    XL := CreateOleObject('Excel.Application');
  end;

  XL.Visible := true;
  XL.WorkBooks.Add;
  XL.WorkBooks[XL.WorkBooks.Count].WorkSheets[1].Name := 'Report';
  Sheet := XL.WorkBooks[XL.WorkBooks.Count].WorkSheets['Report'];
  //  Sheet.SetBackgroundPicture(FileName:=ExtractFilePath(ParamStr(0))+'bg.JPG');

  //  Sheet.Cells[1, 1] := 'Biblio'; Sheet.Cells[1, 1].Font.Bold := True; Sheet.Cells[1, 1].Font.Color := clWhite;
  //  Sheet.Cells[2, 1] := 'Globus'; Sheet.Cells[2, 1].Font.Bold := True; Sheet.Cells[2, 1].Font.Color := clWhite;

  RecNo := 1;
  Sheet.Cells[RecNo, 2] := 'Документ создан: ' + DateToStr(date) + '   ' + TimeToStr(time);
  Sheet.Cells[RecNo, 2].Font.Italic := True;
  inc(RecNo);
  Sheet.Cells[RecNo, 2] := 'Пользователь: ' + ComputerName + ' / ' + UserName;
  Sheet.Cells[RecNo, 2].Font.Italic := True;
  inc(RecNo);
  Sheet.Cells[RecNo, 2] := 'Программа: ' + ExtractFileName(ParamStr(0));
  Sheet.Cells[RecNo, 2].Font.Italic := True;

  // Шапка
  inc(RecNo, 3);
  ColIndex := 0;
  for i := 0 to DataSet.FieldCount - 1 do
  begin
    if not DataSet.Fields[i].Visible then continue;

    if DataSet.Fields[i].DisplayLabel <> '' then
      Sheet.Cells[RecNo, 2 + ColIndex] := DataSet.Fields[i].DisplayLabel
    else
      Sheet.Cells[RecNo, 2 + ColIndex] := DataSet.Fields[i].FieldName;
    Sheet.Cells[RecNo, 2 + ColIndex].Font.Bold := true;
    Sheet.Cells[RecNo, 2 + ColIndex].Font.Size := 10;
    inc(ColIndex);
  end;

  // Данные пошли
  DataSet.First;
  inc(RecNo, 3);
  while not DataSet.EOF do
  begin
    ColIndex := 0;
    for i := 0 to DataSet.FieldCount - 1 do
    begin
      if not DataSet.Fields[i].Visible then continue;
      Sheet.Cells[RecNo, 2 + ColIndex] := DataSet.Fields[i].AsString;
      inc(ColIndex);
    end;
    DataSet.Next;
    if Assigned(OnExportProgress) then OnExportProgress(trunc((DataSet.RecNo / DataSet.RecordCount) * 100));
    inc(RecNo);
  end;
end;

end.
