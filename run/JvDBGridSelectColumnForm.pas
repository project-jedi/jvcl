{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBGridSelectColumnForm.PAS, released on 2004-01-15.

The Initial Developers of the Original Code is Lionel Reynaud
Copyright (c) 2004 Lionel Reynaud
All Rights Reserved.

Contributor(s):

Last Modified: 2004-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

unit JvDBGridSelectColumnForm;
{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, CheckLst, ExtCtrls, DB, DBGrids, JvDBGrid;

resourcestring
  RES_JvDBGridSelectTitle = 'Select columns';
  RES_JvDBGridSelectWarning = 'At least one column must be visible!';

type
  TfrmSelectColumn = class(TForm)
    Panel1: TPanel;
    clbList: TCheckListBox;
    ButtonOK: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure clbListClickCheck(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    FDataSource: TDataSource;
    FJvDBGrid: TJvDBGrid;
    FSelectColumn: TSelectColumn;
    FColumnUpdate: boolean;
    FCanHide: boolean;
    procedure ResizeForm;
    function GetColumn(aField: TField): TColumn;
    { Déclarations privées }
  public
    property DataSource: TDataSource read FDataSource write FDataSource;
    property Grid: TJvDBGrid read FJvDBGrid write FJvDBGrid;
    property SelectColumn: TSelectColumn read FSelectColumn write FSelectColumn;
  end;

implementation

{$R *.dfm}

procedure TfrmSelectColumn.FormCreate(Sender: TObject);
begin
  FColumnUpdate := true;
  FCanHide := true;
  Caption := RES_JvDBGridSelectTitle;
end;

procedure TfrmSelectColumn.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  i: Integer;
  lColumn: TColumn;
begin
  if (ModalResult = mrOk) and FColumnUpdate and FCanHide then
  begin
    for i := 0 to clbList.Items.Count - 1 do
    begin
      if FSelectColumn = scDataBase then
      begin
        DataSource.DataSet.Fields[i].Visible := clbList.Checked[i];
        if Assigned(clbList.Items.Objects[i]) then
          TColumn(clbList.Items.Objects[i]).Visible := clbList.Checked[i]
        else
          if DataSource.DataSet.Fields[i].Visible then
          begin
            lColumn := FJvMyDBGrid.Columns.Add;
            // On essaie de l'insérer à sa place
            if i < FJvMyDBGrid.Columns.Count then lColumn.Index := i;
            lColumn.Field := DataSource.DataSet.Fields[i];
          end;
      end
      else
        FJvMyDBGrid.Columns.Items[i].Visible := clbList.Checked[i];
    end;
  end;
end;

procedure TfrmSelectColumn.FormDestroy(Sender: TObject);
begin
  clbList.Items.Clear;
end;

procedure TfrmSelectColumn.FormActivate(Sender: TObject);
var
  i, j: Integer;
  lColumn: TColumn;
begin
  if Assigned(FJvMyDBGrid) then
  begin
    with FJvMyDBGrid do
    try
      if not (Assigned(DataSource) and
        Assigned(DataSource.DataSet)) then Exit;

      if FSelectColumn = scDataBase then
        with DataSource.DataSet do
          for i := 0 to FieldCount - 1 do
          begin
            lColumn := GetColumn(Fields[i]);
            j := clbList.Items.AddObject(Fields[i].DisplayLabel, lColumn);
            if Assigned(lColumn) then
              clbList.Checked[j] := Fields[i].Visible and lColumn.Visible
            else clbList.Checked[j] := false;
          end
      else
        with FJvMyDBGrid.Columns do
          for i := 0 to Count - 1 do
          begin
            j := clbList.Items.AddObject(Items[i].Title.Caption, Items[i]);
            clbList.Checked[j] := Items[i].Visible;
          end
    finally
    end;

    if clbList.Items.Count > 0 then
      clbList.ItemIndex := 0;
  end;
  ResizeForm;
end;

function TfrmSelectColumn.GetColumn(aField: TField): TColumn;
var
  i: integer;
begin
  result := nil;
  with FJvMyDBGrid.Columns do
    for i := 0 to Count - 1 do
    begin
      if Items[i].FieldName = aField.FieldName then
      begin
        Result := Items[i];
        Break;
      end;
    end;
end;

procedure TfrmSelectColumn.clbListClickCheck(Sender: TObject);
var
  i: integer;
begin
  FCanHide := clbList.Items.Count = 0;
  if not FCanHide then
    for i := 0 to clbList.Items.Count - 1 do
    begin
      if clbList.Checked[i] then
      begin
        FCanHide := True;
        Break;
      end;
    end;
  if not FCanHide then
  begin
    MessageDlg(RES_JvDBGridSelectWarning, mtWarning, [mbOk], 0);
    if clbList.ItemIndex >= 0 then
    begin
      clbList.Checked[clbList.ItemIndex] := true;
      FCanHide := true;
    end;
  end;

end;

procedure TfrmSelectColumn.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
  begin
    ModalResult := mrCancel;
    FColumnUpdate := False;
  end;

end;

procedure TfrmSelectColumn.ResizeForm;
var MinHeight:integer;
begin
  MinHeight := clbList.ItemHeight * clbList.Items.Count;
  if MinHeight >= 400 then
    ClientHeight := 400
  else
    while clbList.ClientHeight < MinHeight do
      ClientHeight := ClientHeight + clbList.ItemHeight;
end;

end.

