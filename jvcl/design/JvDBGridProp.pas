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

unit JvDBGridProp;

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
  TJvDBGridControlsEditor = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  TfmGridProp = class(TForm)
    GroupBoxFields: TGroupBox;
    lbFields: TListBox;
    GroupBoxSelected: TGroupBox;
    lbSelected: TListBox;
    sbAdd: TSpeedButton;
    sbDelete: TSpeedButton;
    LabelControl: TLabel;
    cbControl: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
    cbxFillCell: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure lbSelectedClick(Sender: TObject);
    procedure cbControlClick(Sender: TObject);
    procedure sbAddClick(Sender: TObject);
    procedure sbDeleteClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbxFillCellClick(Sender: TObject);
  private
    procedure SetControl(Name: string);
  public
    JvDBGridControls: TJvDBGridControls;
    Grid: TJvDBGrid;
    procedure Initialize;
  end;

var
  fmGridProp: TfmGridProp;

implementation

uses
  SysUtils, Dialogs,
  JvDsgnConsts, JvTypes;

{$R *.dfm}

procedure TJvDBGridControlsEditor.Edit;
var
  Dlg: TfmGridProp;
  CloseDataset: Boolean;
begin
  CloseDataset := False;
  if TJvDBGrid(GetComponent(0)).DataSource = nil then
    raise EJVCLException.CreateRes(@RsEJvDBGridDataSourceNeeded);
  if TJvDBGrid(GetComponent(0)).DataSource.DataSet = nil then
    raise EJVCLException.CreateRes(@RsEJvDBGridDataSetNeeded);
  if not TJvDBGrid(GetComponent(0)).DataSource.DataSet.Active then
  begin
    TJvDBGrid(GetComponent(0)).DataSource.DataSet.Open;
    CloseDataset := True;
  end;

  Dlg := TfmGridProp.Create(Application);
  try
    Dlg.JvDBGridControls.Assign(TJvDBGridControls(GetOrdValue));
    Dlg.Grid := TJvDBGrid(GetComponent(0));
    Dlg.Initialize;
    Dlg.ShowModal;
    if Dlg.ModalResult = mrOk then
    begin
      TJvDBGridControls(GetOrdValue).Assign(Dlg.JvDBGridControls);
      Modified;
    end;
  finally
    Dlg.Free;
    if CloseDataset then
      TJvDBGrid(GetComponent(0)).DataSource.DataSet.Close;
  end;
end;

function TJvDBGridControlsEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TJvDBGridControlsEditor.GetValue: string;
begin
  Result := '(' + GetPropType^.Name + ')';
end;

procedure TfmGridProp.FormCreate(Sender: TObject);
begin
  JvDBGridControls := TJvDBGridControls.Create(nil);
end;

procedure TfmGridProp.Initialize;
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

procedure TfmGridProp.SetControl(Name: string);
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

procedure TfmGridProp.lbSelectedClick(Sender: TObject);
begin
  if lbSelected.ItemIndex >= 0 then
  begin
    cbControl.Enabled := True;
    cbControl.Color := clWindow;
    SetControl(JvDBGridControls.Items[lbSelected.ItemIndex].ControlName);
    cbxFillCell.Checked := JvDBGridControls.Items[lbSelected.ItemIndex].FitCell;
  end
  else
  begin
    cbControl.Enabled := False;
    cbControl.Color := clBtnFace;
  end;
end;

procedure TfmGridProp.cbControlClick(Sender: TObject);
begin
  if lbSelected.ItemIndex >= 0 then
    JvDBGridControls.Items[lbSelected.ItemIndex].ControlName := cbControl.Text;
end;

procedure TfmGridProp.cbxFillCellClick(Sender: TObject);
begin
  if lbSelected.ItemIndex >= 0 then
    JvDBGridControls.Items[lbSelected.ItemIndex].FitCell := cbxFillCell.Checked;
end;

procedure TfmGridProp.sbAddClick(Sender: TObject);
begin
  if lbFields.ItemIndex >= 0 then
  begin
    if lbSelected.Items.IndexOf(lbFields.Items[lbFields.ItemIndex]) < 0 then
    begin
      lbSelected.Items.Add(lbFields.Items[lbFields.ItemIndex]);
      with JvDBGridControls.Add do
      begin
        FieldName := lbFields.Items[lbFields.ItemIndex];
        FitCell := True;
      end;
    end
    else
      MessageDlg(Format(RsJvDBGridAlreadyAdded,
        [lbFields.Items[lbFields.ItemIndex]]), mtWarning, [mbOK], 0);
  end;
end;

procedure TfmGridProp.sbDeleteClick(Sender: TObject);
begin
  if lbSelected.ItemIndex >= 0 then
  begin
    JvDBGridControls.Items[lbSelected.ItemIndex].Free;
    lbSelected.Items.Delete(lbSelected.ItemIndex);
  end;
end;

procedure TfmGridProp.FormDestroy(Sender: TObject);
begin
  JvDBGridControls.Free;
end;

end.
