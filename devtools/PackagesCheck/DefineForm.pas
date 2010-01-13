{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: DefineForm.pas, released on 2006-02-20.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2006 Florent Ouchet.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit DefineForm;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, JvExComCtrls, JvComCtrls, ActnList,
  TargetInfo, JvDialogs;

type
  TDefForm = class(TForm)
    LabelSymbols: TLabel;
    MemoSymbols: TMemo;
    LabelTarget: TLabel;
    ComboBoxTargets: TComboBox;
    LabelIncludeDirs: TLabel;
    MemoIncludeDirs: TMemo;
    LabelUnits: TLabel;
    JvTreeViewUnits: TJvTreeView;
    ButtonLoadPackage: TButton;
    ButtonAddPackage: TButton;
    ButtonAddUnit: TButton;
    ButtonDelete: TButton;
    ButtonClose: TButton;
    ActionList: TActionList;
    ActionAddPackage: TAction;
    ActionAddUnit: TAction;
    ActionDelete: TAction;
    ButtonAddTarget: TButton;
    ButtonDeleteTarget: TButton;
    ButtonRenameTarget: TButton;
    ActionLoadPackage: TAction;
    ActionRenameTarget: TAction;
    ActionAddTarget: TAction;
    ActionDeleteTarget: TAction;
    JvOpenDialogBpl: TJvOpenDialog;
    ButtonRename: TButton;
    ActionRename: TAction;
    procedure ActionAddPackageUpdate(Sender: TObject);
    procedure ActionAddUnitUpdate(Sender: TObject);
    procedure ActionAddPackageExecute(Sender: TObject);
    procedure ActionAddUnitExecute(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ComboBoxTargetsChange(Sender: TObject);
    procedure ActionDeleteUpdate(Sender: TObject);
    procedure ActionLoadPackageUpdate(Sender: TObject);
    procedure ActionRenameTargetExecute(Sender: TObject);
    procedure ActionAddTargetUpdate(Sender: TObject);
    procedure ActionDeleteTargetUpdate(Sender: TObject);
    procedure ActionDeleteTargetExecute(Sender: TObject);
    procedure ActionAddTargetExecute(Sender: TObject);
    procedure ActionRenameTargetUpdate(Sender: TObject);
    procedure ActionLoadPackageExecute(Sender: TObject);
    procedure ActionRenameUpdate(Sender: TObject);
    procedure ActionRenameExecute(Sender: TObject);
  private
    FTargetsInfo: TTargetsInfo;
    FCurrentTarget: TTargetInfo;
    procedure LoadTarget(ATargetInfo: TTargetInfo);
    procedure SaveTarget(ATargetInfo: TTargetInfo);
  public
    function Execute(ATargetsInfo: TTargetsInfo): Boolean;
  end;

implementation


uses
  JclPeImage,
  JclFileUtils;
  
{$R *.dfm}

{ TDefForm }

procedure TDefForm.ActionAddPackageExecute(Sender: TObject);
var
  PackageName: string;
begin
  PackageName := InputBox(Application.Title, 'Package name', '');
  if PackageName <> '' then
    JvTreeViewUnits.Items.AddChild(nil, PackageName);
end;

procedure TDefForm.ActionAddPackageUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := JvTreeViewUnits.Enabled;
end;

procedure TDefForm.ActionAddTargetExecute(Sender: TObject);
var
  TargetName: string;
begin
  TargetName := InputBox(Application.Title, 'Target name', '');

  if TargetName <> '' then
  begin
    SaveTarget(FCurrentTarget);
    ComboBoxTargets.ItemIndex := ComboBoxTargets.Items.Add(TargetName);
    FCurrentTarget := FTargetsInfo.AddInfo(TargetName);
    LoadTarget(FCurrentTarget);
  end;
end;

procedure TDefForm.ActionAddTargetUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := True;
end;

procedure TDefForm.ActionAddUnitExecute(Sender: TObject);
var
  UnitName: string;
  CurrentNode: TTreeNode;
begin
  CurrentNode := JvTreeViewUnits.Selected;
  while CurrentNode.Parent <> nil do
    CurrentNode := CurrentNode.Parent;
  UnitName := InputBox(Application.Title, 'Unit name', '');
  if UnitName <> '' then
    JvTreeViewUnits.Items.AddChild(CurrentNode, UnitName);
end;

procedure TDefForm.ActionAddUnitUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := JvTreeViewUnits.Enabled
    and Assigned(JvTreeViewUnits.Selected);
end;

procedure TDefForm.ActionDeleteExecute(Sender: TObject);
begin
  JvTreeViewUnits.Selected.Free;
end;

procedure TDefForm.ActionDeleteTargetExecute(Sender: TObject);
begin
  FTargetsInfo.DeleteInfo(ComboBoxTargets.ItemIndex);
  ComboBoxTargets.Items.Delete(ComboBoxTargets.ItemIndex);
  ComboBoxTargets.ItemIndex := -1;
  ComboBoxTargets.Text := '';
  FCurrentTarget := nil;
  LoadTarget(nil);
end;

procedure TDefForm.ActionDeleteTargetUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ComboBoxTargets.ItemIndex >= 0;
end;

procedure TDefForm.ActionDeleteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := JvTreeViewUnits.Enabled
    and Assigned(JvTreeViewUnits.Selected);
end;

procedure TDefForm.ActionLoadPackageExecute(Sender: TObject);
var
  PeImage: TJclPeImage;
  ExportList: TJclPeExportFuncList;
  IndexPackage, IndexFile, IndexExport, EndOfUnitPos: Integer;
  ExportName, UnitName, FileName, PackageName: string;
  TargetPackage: TTargetPackage;
begin
  SaveTarget(FCurrentTarget);
  if JvOpenDialogBpl.Execute then
  begin
    for IndexFile := 0 to JvOpenDialogBpl.Files.Count - 1 do
    begin
      FileName := JvOpenDialogBpl.Files.Strings[IndexFile];
      PackageName := PathExtractFileNameNoExt(FileName);

      IndexPackage := FCurrentTarget.IndexOf(PackageName);
      if IndexPackage >= 0 then
        TargetPackage := FCurrentTarget.Packages[IndexPackage]
      else
        TargetPackage := FCurrentTarget.AddPackage(PackageName);

      TargetPackage.Clear;
      PeImage := TJclPeImage.Create;
      try
         PeImage.FileName := FileName;
        ExportList := PeImage.ExportList;
        for IndexExport := 0 to ExportList.FunctionCount - 1 do
        begin
          ExportName := ExportList.Items[IndexExport].Name;
          if (Length(ExportName) > 0) and (ExportName[1] = '@') then
          begin
            UnitName := Copy(ExportName, 2, Length(ExportName) - 1);
            EndOfUnitPos := Pos('@', UnitName);
            if EndOfUnitPos > 0 then
            begin
              SetLength(UnitName, EndOfUnitPos - 1);
              if Pos('$', UnitName) = 0 then
                TargetPackage.AddUnit(UnitName);
            end;
          end;
        end;
      finally
        PeImage.Free;
      end;
    end;
  end;
  LoadTarget(FCurrentTarget);
end;

procedure TDefForm.ActionLoadPackageUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := JvTreeViewUnits.Enabled;
end;

procedure TDefForm.ActionRenameExecute(Sender: TObject);
var
  SelectedNode: TTreeNode;
begin
  SelectedNode := JvTreeViewUnits.Selected;
  SelectedNode.Text := InputBox(Application.Title, 'New name', SelectedNode.Text);
end;

procedure TDefForm.ActionRenameTargetExecute(Sender: TObject);
var
  TargetName: string;
begin
  TargetName := ComboBoxTargets.Text;
  TargetName := InputBox(Application.Title, 'New target name', TargetName);

  FTargetsInfo.Names[ComboBoxTargets.ItemIndex] := TargetName;
  ComboBoxTargets.Items.Strings[ComboBoxTargets.ItemIndex] := TargetName;
end;

procedure TDefForm.ActionRenameTargetUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ComboBoxTargets.ItemIndex >= 0;
end;

procedure TDefForm.ActionRenameUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := JvTreeViewUnits.Enabled
    and Assigned(JvTreeViewUnits.Selected);
end;

procedure TDefForm.ComboBoxTargetsChange(Sender: TObject);
begin
  SaveTarget(FCurrentTarget);
  if ComboBoxTargets.ItemIndex >= 0 then
    FCurrentTarget := FTargetsInfo.Infos[ComboBoxTargets.ItemIndex]
  else
    FCurrentTarget := nil;
  LoadTarget(FCurrentTarget);
end;

function TDefForm.Execute(ATargetsInfo: TTargetsInfo): Boolean;
var
  IndexTarget: Integer;
begin
  FTargetsInfo := ATargetsInfo;

  ComboBoxTargets.Items.Clear;
  for IndexTarget := 0 to ATargetsInfo.TargetCount - 1 do
    ComboBoxTargets.Items.Add(ATargetsInfo.Names[IndexTarget]);

  if ATargetsInfo.TargetCount > 0 then
  begin
    ComboBoxTargets.ItemIndex := 0;
    FCurrentTarget := ATargetsInfo.Infos[0];
  end
  else
    FCurrentTarget := nil;
  LoadTarget(FCurrentTarget);

  Result := ShowModal = mrOk;

  SaveTarget(FCurrentTarget);
end;

procedure TDefForm.LoadTarget(ATargetInfo: TTargetInfo);
var
  IndexPackage, IndexUnit: Integer;
  PackageTreeNode: TTreeNode;
  TargetPackage: TTargetPackage;
begin
  if Assigned(ATargetInfo) then
  begin
    MemoSymbols.Enabled := True;
    MemoSymbols.Color := clWindow;
    MemoSymbols.Lines.Assign(ATargetInfo.Defines);

    MemoIncludeDirs.Enabled := True;
    MemoIncludeDirs.Color := clWindow;
    MemoIncludeDirs.Lines.Assign(ATargetInfo.IncludeDirs);

    JvTreeViewUnits.Enabled := True;
    JvTreeViewUnits.Color := clWindow;
    JvTreeViewUnits.Items.Clear;
    for IndexPackage := 0 to ATargetInfo.PackageCount - 1 do
    begin
      PackageTreeNode := JvTreeViewUnits.Items.AddChild(nil, ATargetInfo.Names[IndexPackage]);
      TargetPackage := ATargetInfo.Packages[IndexPackage];
      for IndexUnit := 0 to TargetPackage.UnitCount - 1 do
        JvTreeViewUnits.Items.AddChild(PackageTreeNode, TargetPackage.Units[IndexUnit]);
    end;
  end
  else
  begin
    MemoSymbols.Enabled := False;
    MemoSymbols.Color := clBtnFace;
    MemoSymbols.Lines.Clear;

    MemoIncludeDirs.Enabled := False;
    MemoIncludeDirs.Color := clBtnFace;
    MemoIncludeDirs.Lines.Clear;

    JvTreeViewUnits.Enabled := False;
    JvTreeViewUnits.Color := clBtnFace;
    JvTreeViewUnits.Items.Clear;
  end;
end;

procedure TDefForm.SaveTarget(ATargetInfo: TTargetInfo);
var
  IndexUnit: Integer;
  PackageTreeNode: TTreeNode;
  TargetPackage: TTargetPackage;
begin
  if Assigned(ATargetInfo) then
  begin
    ATargetInfo.Clear;
    
    ATargetInfo.Defines.Assign(MemoSymbols.Lines);

    ATargetInfo.IncludeDirs.Assign(MemoIncludeDirs.Lines);

    PackageTreeNode := JvTreeViewUnits.Items.GetFirstNode;
    while Assigned(PackageTreeNode) do
    begin
      TargetPackage := ATargetInfo.AddPackage(PackageTreeNode.Text);
      for IndexUnit := 0 to PackageTreeNode.Count - 1 do
        TargetPackage.AddUnit(PackageTreeNode.Item[IndexUnit].Text);
        
      PackageTreeNode := PackageTreeNode.getNextSibling;
    end;
  end;
end;

end.
