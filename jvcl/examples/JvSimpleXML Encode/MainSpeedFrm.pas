{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit MainSpeedFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, JvStringGrid, JvExGrids;

type
  TfrmSpeedTest = class(TForm)
    Label1: TLabel;
    memFiles: TMemo;
    btnEncode: TButton;
    btnDecode: TButton;
    Label2: TLabel;
    sgResults: TJvStringGrid;
    btnClear: TButton;
    procedure btnEncodeClick(Sender: TObject);
    procedure btnDecodeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure sgResultsCaptionClick(Sender: TJvStringGrid; AColumn,
      ARow: integer);
    procedure btnClearClick(Sender: TObject);
  private
    FDescending: boolean;
    procedure Test(Decode: boolean);
    procedure AddInfo(const FileName: string; Decode: boolean;
      FileSize: Int64; MSecs: Cardinal; AddRow: boolean);
  protected
    { Private declarations }
    procedure WMDropFiles(var Message: TWMDropFiles); message WM_DROPFILES;
  public
    { Public declarations }

  end;

var
  frmSpeedTest: TfrmSpeedTest;

implementation
uses
  JvSimpleXML, JvJVCLUtils, ShellAPI;

{$R *.dfm}

function StringFromFile(const FileName: string; var FileSize: Int64): string;
begin
  with TFileStream.Create(FileName, fmOpenRead) do
  try
    FileSize := Size;
    SetLength(Result, FileSize);
    if Size > 0 then
      Read(Result[1], FileSize);
  finally
    Free;
  end;
end;

procedure TfrmSpeedTest.Test(Decode: boolean);
var
  i: integer;
  FStartTime: Cardinal;
  FStringLength: Int64;
  S: string;
begin
  WaitCursor;
  for i := 0 to memFiles.Lines.Count - 1 do
    if FileExists(memFiles.Lines[i]) then
    begin
      S := StringFromFile(memFiles.Lines[i], FStringLength);
      FStartTime := GetTickCount;
      if Decode then
        SimpleXMLDecode(S, false)
      else
        SimpleXMLEncode(S);
      AddInfo(memFiles.Lines[i], Decode, FStringLength, GetTickCount - FStartTime, sgResults.Cells[0,1] <> '');
    end;
end;

procedure TfrmSpeedTest.AddInfo(const FileName: string; Decode: boolean;
  FileSize: Int64; MSecs: Cardinal; AddRow: boolean);
const
  cDecoded: array[boolean] of PChar = ('Encoded', 'Decoded');
var
  i: integer;
  Speed, KBSpeed:Extended;
begin
  if MSecs = 0 then
  begin
    Speed := FileSize;
    KBSpeed := FileSize / 1024;
  end
  else
  begin
    Speed := FileSize / MSecs;
    KBSpeed := (FileSize / 1024) / (MSecs / 1000);
  end;
  if AddRow then
    sgResults.RowCount := sgResults.RowCount + 1;
  i := sgResults.RowCount - 1;
  sgResults.Cells[0, i] := ExtractFileName(FileName);
  sgResults.Cells[1, i] := IntToStr(FileSize);
  sgResults.Cells[2, i] := cDecoded[Decode];
  sgResults.Cells[3, i] := IntToStr(MSecs);
  sgResults.Cells[4, i] := IntToStr(round(Speed));
  sgResults.Cells[5, i] := Format('%.*f', [1 + Ord(KBSpeed < 1), KBSpeed]);
end;

procedure TfrmSpeedTest.btnEncodeClick(Sender: TObject);
begin
  Test(false);
end;

procedure TfrmSpeedTest.btnDecodeClick(Sender: TObject);
begin
  Test(true);
end;

procedure TfrmSpeedTest.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(Handle, true);
  sgResults.Cells[0, 0] := 'Filename';
  sgResults.Cells[1, 0] := 'Size (bytes)';
  sgResults.Cells[2, 0] := 'Action';
  sgResults.Cells[3, 0] := 'Time (msecs)';
  sgResults.Cells[4, 0] := 'byte/msec';
  sgResults.Cells[5, 0] := 'kB/sec';
end;

procedure TfrmSpeedTest.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(Handle, false);
end;

procedure TfrmSpeedTest.WMDropFiles(var Message: TWMDropFiles);
var
  Count, i: integer;
  buf: array[0..MAX_PATH] of char;
begin
  WaitCursor;
  Count := DragQueryFile(Message.Drop, $FFFFFFFF, nil, 0);
  try
    for i := 0 to Count - 1 do
    begin
      DragQueryFile(Message.Drop, i, buf, sizeof(buf));
      if FileExists(buf) then
        memFiles.Lines.Add(buf);
    end;
  finally
    DragFinish(Message.Drop);
  end;
end;

procedure TfrmSpeedTest.FormResize(Sender: TObject);
begin
  sgResults.DefaultColWidth := (sgResults.ClientWidth - sgResults.ColCount + 1) div sgResults.ColCount;
end;

procedure TfrmSpeedTest.sgResultsCaptionClick(Sender: TJvStringGrid;
  AColumn, ARow: integer);
const
  SortType:array[0..5] of TJvSortType = (stClassic, stNumeric, stClassic, stNumeric, stNumeric, stNumeric);
begin
  sgResults.SortGrid(AColumn, FDescending, false, SortType[AColumn]);
  FDescending := not FDescending;
end;

procedure TfrmSpeedTest.btnClearClick(Sender: TObject);
begin
  sgResults.RowCount := 2;
  sgResults.Rows[1].Clear;
end;

end.

