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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBGridSelectColumnForm;

{$I jvcl.inc}

interface

uses
  Windows, Classes, Controls, Forms, StdCtrls, Dialogs, CheckLst, ExtCtrls,
  DB, DBGrids, JvDBGrid;

type
  TfrmSelectColumn = class(TForm)
    Panel1: TPanel;
    clbList: TCheckListBox;
    cbWithFieldName: TCheckBox;
    ButtonOK: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure cbClick(Sender: TObject);
    procedure clbListClickCheck(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    FDataSource: TDataSource;
    FJvDBGrid: TJvDBGrid;
    FSelectColumn: TSelectColumn;
    FColumnUpdate: Boolean;
    FCanHide: Boolean;
    FNoSelectionWarning: string;
    procedure ResizeForm;
    function GetColumn(AField: TField): TColumn;
  public
    property DataSource: TDataSource read FDataSource write FDataSource;
    property Grid: TJvDBGrid read FJvDBGrid write FJvDBGrid;
    property SelectColumn: TSelectColumn read FSelectColumn write FSelectColumn;
  published
    // make this published so localization tools have a chance to pick it up
    property NoSelectionWarning: string read FNoSelectionWarning write FNoSelectionWarning;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvConsts, Sysutils;

{$IFDEF VCL}
{$R *.dfm}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$R *.xfm}
{$ENDIF VisualCLX}

procedure TfrmSelectColumn.FormCreate(Sender: TObject);
begin
  FColumnUpdate := True;
  FCanHide := True;
  // (p3) don't use resourcestring here since this property is normally set from the JvDBGrid
  // and using resourcestrings might give problems with localization synchronizing
  NoSelectionWarning := 'At least one column must be visible!';
end;

type
  TJvDBGridAccessProtected = class(TJvDBGrid);

procedure TfrmSelectColumn.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  I, J: Integer;
begin
  if (ModalResult = mrOk) and FColumnUpdate and FCanHide and Assigned(FJvDBGrid) then
  begin
    TJvDBGridAccessProtected(FJvDBGrid).BeginLayout;
    try
      for I := 0 to clbList.Items.Count - 1 do
      begin
        J := Integer(clbList.Items.Objects[I]);
        if (J >= 0) and (J < FJvDBGrid.Columns.Count) then
          FJvDBGrid.Columns[J].Visible := clbList.Checked[I];
      end;
    finally
      TJvDBGridAccessProtected(FJvDBGrid).EndLayout;
    end;
  end;
end;

procedure TfrmSelectColumn.FormDestroy(Sender: TObject);
begin
  clbList.Items.Clear;
end;

procedure TfrmSelectColumn.FormActivate(Sender: TObject);
var
  I, J: Integer;
  ColumnTitle: string;
  lColumn: TColumn;
begin
  if Assigned(FJvDBGrid) then
    with FJvDBGrid do
    begin
      clbList.Items.Clear;
      cbWithFieldName.Hide;
      if (FSelectColumn = scDatabase) and Assigned(DataSource) and Assigned(DataSource.Dataset) then
      begin
        with DataSource.Dataset do
          for I := 0 to FieldCount - 1 do
          begin
            lColumn := GetColumn(Fields[I]);
            if Assigned(lColumn) then
            begin
              ColumnTitle := lColumn.Title.Caption;
              if not AnsiSameText(ColumnTitle, Fields[I].FieldName) then
              begin
                if not cbWithFieldName.Visible then
                  cbWithFieldName.Show;
                if cbWithFieldName.State = cbChecked then
                  ColumnTitle := ColumnTitle + ' [' + Fields[I].FieldName + ']';
              end;
              J := clbList.Items.AddObject(ColumnTitle, TObject(lColumn.Index));
              clbList.Checked[J] := lColumn.Visible and Fields[I].Visible;
            end;
          end;
      end
      else
      begin
        for I := 0 to Columns.Count - 1 do
        begin
          ColumnTitle := FJvDBGrid.Columns[I].Title.Caption;
          if not AnsiSameText(ColumnTitle, FJvDBGrid.Columns[I].FieldName) then
          begin
            if not cbWithFieldName.Visible then
              cbWithFieldName.Show;
            if cbWithFieldName.State = cbChecked then
              ColumnTitle := ColumnTitle + ' [' + FJvDBGrid.Columns[I].FieldName + ']';
          end;
          J := clbList.Items.AddObject(ColumnTitle, TObject(I));
          clbList.Checked[J] := FJvDBGrid.Columns[I].Visible;
        end;
      end;
      if clbList.Items.Count > 0 then
        clbList.ItemIndex := 0;
    end;
  ResizeForm;
end;

procedure TfrmSelectColumn.cbClick(Sender: TObject);
begin
  FormActivate(Self);
end;

function TfrmSelectColumn.GetColumn(AField: TField): TColumn;
var
  I: Integer;
begin
  Result := nil;
  with FJvDBGrid.Columns do
    for I := 0 to Count - 1 do
    begin
      if Items[I].FieldName = AField.FieldName then
      begin
        Result := Items[I];
        Break;
      end;
    end;
end;

procedure TfrmSelectColumn.clbListClickCheck(Sender: TObject);
var
  I: Integer;
begin
  FCanHide := clbList.Items.Count = 0;
  if not FCanHide then
    for I := 0 to clbList.Items.Count - 1 do
    begin
      if clbList.Checked[I] then
      begin
        FCanHide := True;
        Break;
      end;
    end;
  if not FCanHide then
  begin
    MessageDlg(NoSelectionWarning, mtWarning, [mbOk], 0);
    if clbList.ItemIndex >= 0 then
    begin
      clbList.Checked[clbList.ItemIndex] := True;
      FCanHide := True;
    end;
  end;

end;

procedure TfrmSelectColumn.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Esc then
  begin
    ModalResult := mrCancel;
    FColumnUpdate := False;
  end;
end;

procedure TfrmSelectColumn.ResizeForm;
var
  MinHeight: Integer;
begin
  MinHeight := clbList.ItemHeight * clbList.Items.Count;
  if MinHeight >= 400 then
    ClientHeight := 400
  else
    while clbList.ClientHeight < MinHeight do
      ClientHeight := ClientHeight + clbList.ItemHeight;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

