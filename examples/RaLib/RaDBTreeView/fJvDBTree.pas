{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: RAFDAlignPalette.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s): 

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : resource identifers,
              common global objects,
              utilities

Known Issues:
-----------------------------------------------------------------------------}


{$I jvcl.inc}

unit fJvDBTree;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, JvDBTreeView, Db, DBTables, StdCtrls, Grids, DBGrids,
  JvBDEUtils, DBCtrls, JvHint, JvComponent, JvFormPlacement, JvBDESQLScript,
  JvExComCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Table1: TTable;
    RADBTreeView1: TJvDBTreeView;
    DataSource1: TDataSource;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    DBGrid1: TDBGrid;
    RASQLScript1: TJvBDESQLScript;
    Database1: TDatabase;
    DBText1: TDBText;
    RegAuto1: TJvFormStorage;
    Label3: TLabel;
    procedure Table1NewRecord(DataSet: TDataSet);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    RAHint1: TJvHint;
    procedure GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation


{$R *.DFM}

procedure TForm1.Table1NewRecord(DataSet: TDataSet);
begin
  Table1['Uni'] := JvBDEUtils.GetQueryResult(Table1.DatabaseName,
     'select max(Uni) from "Tree.dbf"') + 1;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RAHint1 := TJvHint.Create(Self);
  TDrawGrid(DBGrid1).OnMouseMove := GridMouseMove;
  try
    Table1.DeleteTable;
  except
  end;
  RASQLScript1.Execute;
  Table1.Open;
  {$IFNDEF VER120}
   Label3.Free;
  {$ENDIF}
  //RADBTreeView1.Mirror := True;
end;

procedure TForm1.GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  G : TGridCoord;
  R : TRect;
begin
//  G := TDrawGrid(DBGrid1).MouseCoord(X, Y);
  TDrawGrid(DBGrid1).MouseToCell(X, Y, G.X, G.Y);

  R := TDrawGrid(DBGrid1).CellRect(G.X, G.Y);
  OffsetRect(R, DBGrid1.ClientOrigin.X, DBGrid1.ClientOrigin.Y);
  RAHint1.ActivateHint(R, Format('X: %d, Y: %d', [G.X, G.Y]));
end;

end.
