{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBGridProp.PAS, released on 2004-07-09.

The Initial Developer of the Original Code is Frédéric Leneuf-Magaud.
Portions created by Frédéric Leneuf-Magaud are Copyright (C) 2004 Frédéric Leneuf-Magaud.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBGridControlsEditorForm;

{$I jvcl.inc}

interface

uses
  Classes, Controls, Forms,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  StdCtrls, Buttons, Graphics,
  JvDBGrid, TypInfo;

type
  TfrmJvDBGridControlsEditor = class(TForm)
    GroupBoxFields: TGroupBox;
    lbFields: TListBox;
    GroupBoxSelected: TGroupBox;
    lbSelected: TListBox;
    sbAdd: TSpeedButton;
    sbDelete: TSpeedButton;
    LabelControl: TLabel;
    cbControl: TComboBox;
    LabelFillCell: TLabel;
    cbFillCell: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure lbSelectedClick(Sender: TObject);
    procedure cbControlClick(Sender: TObject);
    procedure cbFillCellClick(Sender: TObject);
    procedure sbAddClick(Sender: TObject);
    procedure sbDeleteClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure SetControl(Name: string);
  public
    JvDBGridControls: TJvDBGridControls;
    Grid: TJvDBGrid;
    procedure Initialize;
  end;

var
  frmJvDBGridControlsEditor: TfrmJvDBGridControlsEditor;

implementation

uses
  SysUtils, Dialogs, JvDsgnConsts;

{$R *.dfm}

procedure TfrmJvDBGridControlsEditor.FormCreate(Sender: TObject);
begin
  JvDBGridControls := TJvDBGridControls.Create(nil);
end;

procedure TfrmJvDBGridControlsEditor.Initialize;
var
  I: Integer;
begin
  for I := 0 to Grid.Columns.Count - 1 do
    lbFields.Items.Add(Grid.Columns.Items[I].FieldName);
  for I := 0 to JvDBGridControls.Count - 1 do
    lbSelected.Items.Add(JvDBGridControls.Items[I].FieldName);
  for I := 0 to Grid.Owner.ComponentCount - 1 do
    if Grid.Owner.Components[I] is TWinControl then
      if IsPublishedProp(Grid.Owner.Components[I], 'DataField') then
        cbControl.Items.Add(Grid.Owner.Components[I].Name);
  lbSelectedClick(lbSelected);
end;

procedure TfrmJvDBGridControlsEditor.SetControl(Name: string);
var
  I: Integer;
begin
  for I := 0 to cbControl.Items.Count - 1 do
    if CompareText(Name, cbControl.Items[I]) = 0 then
    begin
      cbControl.ItemIndex := I;
      Exit;
    end;
  cbControl.ItemIndex := -1;
end;

procedure TfrmJvDBGridControlsEditor.lbSelectedClick(Sender: TObject);
begin
  if lbSelected.ItemIndex >= 0 then
  begin
    cbControl.Enabled := True;
    cbControl.Color := clWindow;
    SetControl(JvDBGridControls.Items[lbSelected.ItemIndex].ControlName);
    cbFillCell.Enabled := True;
    cbFillCell.Color := clWindow;
    cbFillCell.ItemIndex := Ord(JvDBGridControls.Items[lbSelected.ItemIndex].FitCell);
  end
  else
  begin
    cbControl.Enabled := False;
    cbControl.Color := clBtnFace;
    cbFillCell.Enabled := False;
    cbFillCell.Color := clBtnFace;
  end;
end;

procedure TfrmJvDBGridControlsEditor.cbControlClick(Sender: TObject);
begin
  if lbSelected.ItemIndex >= 0 then
    JvDBGridControls.Items[lbSelected.ItemIndex].ControlName := cbControl.Text;
end;

procedure TfrmJvDBGridControlsEditor.cbFillCellClick(Sender: TObject);
begin
  if lbSelected.ItemIndex >= 0 then
    JvDBGridControls.Items[lbSelected.ItemIndex].FitCell := TJvDBGridControlSize(cbFillCell.ItemIndex);
end;

procedure TfrmJvDBGridControlsEditor.sbAddClick(Sender: TObject);
begin
  if lbFields.ItemIndex >= 0 then
  begin
    if lbSelected.Items.IndexOf(lbFields.Items[lbFields.ItemIndex]) < 0 then
    begin
      lbSelected.Items.Add(lbFields.Items[lbFields.ItemIndex]);
      with JvDBGridControls.Add do
      begin
        FieldName := lbFields.Items[lbFields.ItemIndex];
        FitCell := fcCellSize;
      end;
    end
    else
      MessageDlg(Format(RsJvDBGridAlreadyAdded,
        [lbFields.Items[lbFields.ItemIndex]]), mtWarning, [mbOK], 0);
  end;
end;

procedure TfrmJvDBGridControlsEditor.sbDeleteClick(Sender: TObject);
begin
  if lbSelected.ItemIndex >= 0 then
  begin
    JvDBGridControls.Items[lbSelected.ItemIndex].Free;
    lbSelected.Items.Delete(lbSelected.ItemIndex);
  end;
end;

procedure TfrmJvDBGridControlsEditor.FormDestroy(Sender: TObject);
begin
  JvDBGridControls.Free;
end;

end.
