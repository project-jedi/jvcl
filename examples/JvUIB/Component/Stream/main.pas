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

unit main;

interface

uses
{$IFDEF LINUX}
  libc, QForms, QStdCtrls, QControls, QGraphics, QDialogs, QExtCtrls, QGrids,
{$ELSE}
  Windows, Graphics, Controls, Forms, Messages, Dialogs, StdCtrls, Grids,
{$ENDIF}
  SysUtils, Classes, JvUIB, JvUIBLib, JvComponent;

type
  TMainForm = class(TForm)
    DataBase: TJvUIBDataBase;
    Transaction: TJvUIBTransaction;
    Query: TJvUIBQuery;
    Button1: TButton;
    StringGrid: TStringGrid;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.Button1Click(Sender: TObject);
var
  MemStream: TMemoryStream;
  FieldResult: TSQLResult;
  i, j: integer;
begin
  Query.Open;
  Query.FetchAll;
  MemStream := TMemoryStream.Create;
  FieldResult := TSQLResult.Create;
  Query.Fields.SaveToStream(MemStream);
  Query.Close(etmCommit);

  MemStream.Seek(0, soFromBeginning);
  FieldResult.LoadFromStream(MemStream);
  MemStream.Free;

  StringGrid.ColCount := FieldResult.FieldCount  + 1;
  StringGrid.RowCount := FieldResult.RecordCount + 1;

  for i := 1 to FieldResult.FieldCount do
    StringGrid.Cells[i, 0] := FieldResult.AliasName[i-1];

  for i := 1 to FieldResult.RecordCount do
  begin
    FieldResult.GetRecord(i-1);
    StringGrid.Cells[0, i] := Inttostr(i);
    for j := 1 to FieldResult.FieldCount do
      StringGrid.Cells[j, i] := FieldResult.AsString[j-1];
  end;

  FieldResult.Free;


end;

end.
