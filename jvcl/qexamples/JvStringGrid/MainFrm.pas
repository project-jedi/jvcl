{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

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

unit MainFrm;

interface
uses
  QWindows, QMessages, SysUtils, Classes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QComCtrls, QExtCtrls, QGrids, JvQStringGrid, JvQExGrids,
  QComCtrlsEx;

type
  TForm1 = class(TForm)
    JvSg: TJvStringGrid;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    edRow: TEdit;
    udRows: TUpDown;
    edCol: TEdit;
    udCols: TUpDown;
    RowInsert: TButton;
    RowDelete: TButton;
    RowHide: TButton;
    RowShow: TButton;
    ColInsert: TButton;
    ColDelete: TButton;
    ColHide: TButton;
    ColShow: TButton;
    Label3: TLabel;
    Clear: TButton;
    btnAddRow: TButton;
    btnDelRow: TButton;
    btnAddCol: TButton;
    btnDelCol: TButton;
    reData: TMemo;
    procedure RowInsertClick(Sender: TObject);
    procedure ClearClick(Sender: TObject);
    procedure RowDeleteClick(Sender: TObject);
    procedure RowHideClick(Sender: TObject);
    procedure RowShowClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ColInsertClick(Sender: TObject);
    procedure ColDeleteClick(Sender: TObject);
    procedure ColHideClick(Sender: TObject);
    procedure ColShowClick(Sender: TObject);
    procedure btnAddRowClick(Sender: TObject);
    procedure btnDelRowClick(Sender: TObject);
    procedure btnAddColClick(Sender: TObject);
    procedure btnDelColClick(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateTrackers;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

procedure TForm1.UpdateTrackers;
// var i:integer;
begin
  udRows.Min := -1; // JvSG.FixedRows;
  udCols.Min := -1; // JvSG.FixedCols;
  udRows.Max := JvSG.RowCount;
  udCols.Max := JvSG.ColCount;
{
  JvSG.Cells[0,0] := '';

  if JvSG.FixedCols > 0 then
    for i := 1 to JvSG.RowCount - 1 do
      JvSG.Cells[0,i] := Format('%.3d',[i]);
  if JvSG.FixedRows > 0 then
    for i := 1 to JvSG.ColCount - 1 do
      JvSG.Cells[i, 0] := Format('%.3d',[i]);
}
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  UpdateTrackers;
end;

procedure TForm1.RowInsertClick(Sender: TObject);
begin
  JvSg.InsertRow(udRows.Position).Assign(reData.Lines);
  UpdateTrackers;
end;

procedure TForm1.RowDeleteClick(Sender: TObject);
begin
  JvSg.RemoveRow(udRows.Position);
  UpdateTrackers;
end;

procedure TForm1.RowHideClick(Sender: TObject);
begin
  JvSg.HideRow(udRows.Position);
  UpdateTrackers;
end;

procedure TForm1.RowShowClick(Sender: TObject);
begin
  JvSg.ShowRow(udRows.Position,0);
  UpdateTrackers;
end;

procedure TForm1.ColInsertClick(Sender: TObject);
begin
  JvSg.InsertCol(udCols.Position).Assign(reData.Lines);
  UpdateTrackers;
end;

procedure TForm1.ColDeleteClick(Sender: TObject);
begin
  JvSg.RemoveCol(udCols.Position);
  UpdateTrackers;
end;

procedure TForm1.ColHideClick(Sender: TObject);
begin
  JvSg.HideCol(udCols.Position);
  UpdateTrackers;
end;

procedure TForm1.ColShowClick(Sender: TObject);
begin
  JvSg.ShowCol(udCols.Position,0);
  UpdateTrackers;
end;

procedure TForm1.ClearClick(Sender: TObject);
begin
  JvSG.Clear;
  UpdateTrackers;
end;

procedure TForm1.btnAddRowClick(Sender: TObject);
begin
  JvSg.InsertRow(JvSg.RowCount+1);
end;

procedure TForm1.btnDelRowClick(Sender: TObject);
begin
  JvSg.RemoveRow(JvSg.RowCount-1);
end;

procedure TForm1.btnAddColClick(Sender: TObject);
begin
  JvSg.InsertCol(JvSg.ColCount+1);
end;

procedure TForm1.btnDelColClick(Sender: TObject);
begin
  JvSg.RemoveCol(JvSg.ColCount-1);
end;

end.
