Unit JvDBGridProp;

Interface

{$I jvcl.inc}

Uses
   Classes, Controls, Forms,
   {$IFDEF COMPILER6_UP}
   DesignIntf, DesignEditors,
   {$ELSE}
   DsgnIntf,
   {$ENDIF COMPILER6_UP}
   StdCtrls, Buttons, Graphics, JvDBGrid;

Type 
   TJvDBGridControlsEditor = Class(TPropertyEditor)
   Public
      Procedure Edit; Override;
      Function GetAttributes: TPropertyAttributes; Override;
      Function GetValue: String; Override;
   End;

   TfmGridProp = Class(TForm)
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
      Procedure FormCreate(Sender: TObject);
      Procedure lbSelectedClick(Sender: TObject);
      Procedure cbControlClick(Sender: TObject);
      Procedure sbAddClick(Sender: TObject);
      Procedure sbDeleteClick(Sender: TObject);
      Procedure FormDestroy(Sender: TObject);
   Private
      Procedure SetControl(Name: String);
   Public
      JvDBGridControls: TJvDBGridControls;
      Grid: TJvDBGrid;
      Procedure Initialize;
   End;

Var
   fmGridProp: TfmGridProp;

Implementation

Uses SysUtils, Dialogs;

ResourceString
  RsJvDBGridDataSourceNeeded = 'Datasource property must be set before selecting controls.';
  RsJvDBGridDataSetNeeded = 'A dataset must be linked to the grid datasource before selecting controls.';
  RsJvDBGridAlreadyAdded = 'The field "%s" has already been added.';

{$R *.DFM}

Procedure TJvDBGridControlsEditor.Edit;
Var
   Dlg: TfmGridProp;
   CloseDataset: Boolean;
Begin
   CloseDataset := False;
   If TJvDBGrid(GetComponent(0)).DataSource = Nil Then
      Raise Exception.Create(RsJvDBGridDataSourceNeeded);
   If TJvDBGrid(GetComponent(0)).DataSource.DataSet = Nil Then
      Raise Exception.Create(RsJvDBGridDataSetNeeded);
   If Not TJvDBGrid(GetComponent(0)).DataSource.DataSet.Active Then
   Begin
      TJvDBGrid(GetComponent(0)).DataSource.DataSet.Open;
      CloseDataset := True;
   End;

   Dlg := TfmGridProp.Create(Application);
   Try
      Dlg.JvDBGridControls.Assign(TJvDBGridControls(GetOrdValue));
      Dlg.Grid := TJvDBGrid(GetComponent(0));
      Dlg.Initialize;
      Dlg.ShowModal;
      If Dlg.ModalResult = mrOk Then 
      Begin
         TJvDBGridControls(GetOrdValue).Assign(Dlg.JvDBGridControls);
         Modified;
      End;
   Finally
      Dlg.Free;
      If CloseDataset Then
         TJvDBGrid(GetComponent(0)).DataSource.DataSet.Close;
   End;
End;

Function TJvDBGridControlsEditor.GetAttributes: TPropertyAttributes;
Begin
   Result := [paDialog, paReadOnly];
End;

Function TJvDBGridControlsEditor.GetValue: String;
Begin
   Result := '(' + GetPropType^.Name +')';
End;

Procedure TfmGridProp.FormCreate(Sender: TObject);
Begin
   JvDBGridControls := TJvDBGridControls.Create(Nil);
End;

Procedure TfmGridProp.Initialize;
Var 
   i: Integer;
Begin
   For i := 0 To Grid.Columns.Count - 1 Do
      lbFields.Items.Add(Grid.Columns.Items[i].FieldName);
   For i := 0 To JvDBGridControls.Count - 1 Do
      lbSelected.Items.Add(JvDBGridControls.Items[i].FieldName);
   For i := 0 To Grid.Owner.ComponentCount - 1 Do
      If Grid.Owner.Components[i] Is TWinControl Then
         cbControl.Items.Add(Grid.Owner.Components[i].Name);
   lbSelectedClick(lbSelected);      
End;

Procedure TfmGridProp.SetControl(Name: String);
Var 
   i: Integer;
Begin
   For i := 0 To cbControl.Items.Count - 1 Do
      If CompareText(Name, cbControl.Items[i]) = 0 Then
      Begin
         cbControl.ItemIndex := i;
         Exit;
      End;
   cbControl.ItemIndex := -1;
End;

Procedure TfmGridProp.lbSelectedClick(Sender: TObject);
Begin
   If lbSelected.ItemIndex >= 0 Then
   Begin
      cbControl.Enabled := True; 
      cbControl.Color := clWindow;
      SetControl(JvDBGridControls.Items[lbSelected.ItemIndex].ControlName);
   End
   Else
   Begin
      cbControl.Enabled := False;
      cbControl.Color := clBtnface;
   End;
End;

Procedure TfmGridProp.cbControlClick(Sender: TObject);
Begin
   If lbSelected.ItemIndex >= 0 Then
      JvDBGridControls.Items[lbSelected.ItemIndex].ControlName := cbControl.Text;
End;

Procedure TfmGridProp.sbAddClick(Sender: TObject);
Begin
   If (lbFields.ItemIndex >= 0) Then
   Begin
      If (lbSelected.Items.Indexof(lbFields.Items[lbFields.ItemIndex]) < 0) Then
      Begin
         lbSelected.Items.Add(lbFields.Items[lbFields.ItemIndex]);
         With JvDBGridControls.Add Do
            FieldName := lbFields.Items[lbFields.ItemIndex];
      End
      Else
         MessageDlg(Format(RsJvDBGridAlreadyAdded, [lbFields.Items[lbFields.ItemIndex]]),
                    mtWarning, [mbOK], 0);
   End;
End;

Procedure TfmGridProp.sbDeleteClick(Sender: TObject);
Begin
   If lbSelected.ItemIndex >= 0 Then
   Begin
      JvDBGridControls.Items[lbSelected.ItemIndex].Free;
      lbSelected.Items.Delete(lbSelected.ItemIndex);
   End;
End;

Procedure TfmGridProp.FormDestroy(Sender: TObject);
Begin
   JvDBGridControls.Free;
End;

End.