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

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, JvRegAuto, Grids, DBGrids, Db, DBTables;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    ComboBox1: TComboBox;
    GroupBox1: TGroupBox;
    RegAuto1: TJvFormStorage;
    Label2: TLabel;
    Table1: TTable;
    DataSource1: TDataSource;
    Button4: TButton;
    GroupBox2: TGroupBox;
    CheckBox2: TCheckBox;
    Button2: TButton;
    DBGrid1: TDBGrid;
    pnlStatus: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GroupBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure GroupBox1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure GroupBox1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ComboBox1DropDown(Sender: TObject);
    procedure Table1ActiveChanged(DataSet: TDataSet);
    procedure CheckBox2Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure MyClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

function MessageBox(hWnd: integer; lpText, lpCaption: PChar; uType: integer): Integer;
  external 'user32.dll' name 'MessageBoxA';

procedure TForm1.Button1Click(Sender: TObject);
begin
  MessageBox(0, 'Hello World !', 'Information', 0);
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin
  Label1.Caption := 'Line Count: ' + IntToStr(Memo1.Lines.Count);
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  Label1.Caption := TEdit(Sender).Text;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := Name;
end;

procedure TForm1.GroupBox1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  Label2.Caption := 'X = ' + IntToStr(X) + ', Y = ' + IntToStr(Y);
end;

procedure TForm1.GroupBox1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := Source = Label2;
end;

procedure TForm1.GroupBox1DragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  Label2.Left := X;
  Label2.Top := Y;
end;

procedure TForm1.ComboBox1DropDown(Sender: TObject);
begin
  ComboBox1.Items.Add('This');
  ComboBox1.Items.Add('items');
  ComboBox1.Items.Add('are');
  ComboBox1.Items.Add('added');
  ComboBox1.Items.Add('in');
  ComboBox1.Items.Add('run-time');
end;

procedure TForm1.Table1ActiveChanged(DataSet: TDataSet);
begin
  if Table1.Active then
    pnlStatus.Caption := 'Open'
  else
    pnlStatus.Caption := 'Close'
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
 // if TCheckBox(Sender).Checked then
  if Sender.Checked then
    Table1.Open
  else
    Table1.Close
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Table1.AppendRecord([1000, 'Hello']);
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  B1: TButton;
begin
  B1 := TButton.Create(Self);
  B1.Parent := Self;
  B1.Left := TButton(Sender).Left + TButton(Sender).Width + 20;
  B1.Top := TButton(Sender).Top;
  B1.Caption := 'MyButton';
  B1.OnClick := MyClick;
end;

procedure TForm1.MyClick(Sender: TObject);
begin
  MessageBox(0, 'MyButton Clicked', 'Information', 0);
end;

end.
