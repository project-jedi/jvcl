{ -----------------------------------------------------------------------------
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Original Code is: JvJclUnitVersioningBrowser.pas, released on 2009-03-09.

  The Initial Developers of the Original Code is: Jens Fudickar
  All Rights Reserved.

  Contributor(s):
  Jens Fudickar

  You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
  located at http://jvcl.delphi-jedi.org

  Known Issues:

  ----------------------------------------------------------------------------- }
// $Id$

unit JvJclUnitVersioningBrowser;

{$I jvcl.inc}
{$I crossplatform.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes, ComCtrls, Controls, Forms, JvDynControlEngine;

type
  TJvJclUnitVersioningBrowser = class(TPersistent)
  private
    RCSFilePanel, RevisionPanel, DatePanel, ExtraPanel, PathPanel, LabelControl,
      RCSFileEdit, RevisionEdit, DateEdit, ExtraEdit, PathEdit: TWinControl;
    { Private-Deklarationen }
    UnitVersionForm: TCustomForm;
    procedure CloseButtonOnClick(Sender: TObject);
    {$IFDEF UNITVERSIONING}
    procedure ExportButtonOnClick(Sender: TObject);
    {$ENDIF UNITVERSIONING}
    procedure TreeViewOnChange(Sender: TObject; Node: TTreeNode);
  public
    procedure ShowUnitVersioning(const aDynControlEngine: tJvDynControlEngine);
  end;

procedure ShowUnitVersioning(const aDynControlEngine: tJvDynControlEngine = nil);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Rev$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  StdCtrls, SysUtils, JclStrings, JvDynControlEngineIntf, ExtCtrls,
  JclFileUtils, Dialogs;

procedure ShowUnitVersioning(const aDynControlEngine: tJvDynControlEngine = nil);
var
  JvJclUnitVersioningBrowser: TJvJclUnitVersioningBrowser;
begin
  JvJclUnitVersioningBrowser := TJvJclUnitVersioningBrowser.Create;
  try
    JvJclUnitVersioningBrowser.ShowUnitVersioning(aDynControlEngine);
  finally
    FreeAndNil(JvJclUnitVersioningBrowser);
  end;
end;

procedure TJvJclUnitVersioningBrowser.CloseButtonOnClick(Sender: TObject);
begin
  if Assigned(UnitVersionForm) then
    UnitVersionForm.ModalResult := mrOk;
end;

{$IFDEF UNITVERSIONING}
procedure TJvJclUnitVersioningBrowser.ExportButtonOnClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.DefaultExt := 'lst';
    SaveDialog.Filter := '*.lst|Versionlist (*.lst)';
    SaveDialog.Options := [ofHideReadOnly, ofPathMustExist, ofNoReadOnlyReturn, ofEnableSizing];
    if SaveDialog.Execute then
      ExportUnitVersioningToFile(SaveDialog.FileName);
  finally
    SaveDialog.Free;
  end;
end;
{$ENDIF UNITVERSIONING}

procedure TJvJclUnitVersioningBrowser.ShowUnitVersioning(const aDynControlEngine: tJvDynControlEngine);

  function FindMasterNode(iNodes: TTreeNodes; iNode: TTreeNode; const iPath: string): TTreeNode;
  var
    Part1, Part2: string;
    I: Integer;
  begin
    Result := nil;
    Part2 := iPath;
    Part1 := '';
    while (Part1 = '') and (Part2 <> '') do
    begin
      Part1 := trim(StrBefore(PathDelim, Part2));
      Part2 := trim(StrAfter(PathDelim, Part2));
    end;
    if Part1 <> '' then
    begin
      for I := 0 to iNode.Count - 1 do
      begin
        if iNode.Item[I].Text = Part1 then
        begin
          Result := FindMasterNode(iNodes, iNode.Item[I], Part2);
          break;
        end;
      end;
      if Result = nil then
      begin
        Result := iNodes.AddChild(iNode, Part1);
        Result := FindMasterNode(iNodes, Result, Part2);
      end;
    end
    else
      Result := iNode;
  end;

var
  DynEngine: tJvDynControlEngine;
  MainPanel, ButtonPanel: TWinControl;
  TopBox, BottomBox: TWinControl;
  Button: TButton;
  TreeView: TWinControl;
  IJvReadOnly: IJvDynControlReadOnly;
  IJvTreeView: IJvDynControlTreeView;
  MainNode: TTreeNode;
  Nodes: TTreeNodes;
  {$IFDEF UNITVERSIONING}
  I: Integer;
  Item: TUnitVersion;
  {$ENDIF UNITVERSIONING}
begin
  if Assigned(aDynControlEngine) then
    DynEngine := aDynControlEngine
  else
    DynEngine := DefaultDynControlEngine;
  UnitVersionForm := DynEngine.CreateForm('Unit Versioning', '');
  try
    if UnitVersionForm is TForm then
      {$IFDEF COMPILER7_UP}
      TForm(UnitVersionForm).Position := poOwnerFormCenter;
      {$ELSE}
      TForm(UnitVersionForm).Position := poScreenCenter;
      {$ENDIF COMPILER7_UP};  
    UnitVersionForm.Width := 500;
    UnitVersionForm.Height := 500;
    ButtonPanel := DynEngine.CreatePanelControl(UnitVersionForm, UnitVersionForm, 'ButtonPanel', '', alBottom);
    MainPanel := DynEngine.CreatePanelControl(UnitVersionForm, UnitVersionForm, 'MainPanel', '', alClient);
    if MainPanel is TPanel then
      TPanel(MainPanel).borderWidth := 3;
    Button := DynEngine.CreateButton(UnitVersionForm, ButtonPanel, 'CloseBtn', 'Close', '', CloseButtonOnClick, True, True);
    Button.Left := Round((UnitVersionForm.Width - Button.Width) / 2);
    ButtonPanel.Height := Button.Height + 6;
    Button.Top := Round((ButtonPanel.Height - Button.Height) / 2);
    {$IFDEF UNITVERSIONING}
    Button := DynEngine.CreateButton(UnitVersionForm, ButtonPanel, 'ExportBtn', 'Export', '', ExportButtonOnClick, True, True);
    Button.Left := 10;
    Button.Top := Round((ButtonPanel.Height - Button.Height) / 2);
    {$ENDIF UNITVERSIONING}
    BottomBox := DynEngine.CreateGroupBoxControl(UnitVersionForm, MainPanel, 'BottomBox', 'Details');
    BottomBox.Align := alBottom;
    TopBox := DynEngine.CreateGroupBoxControl(UnitVersionForm, MainPanel, 'TopBox', 'Unit Versions');
    TopBox.Align := alClient;
    TreeView := DynEngine.CreateTreeViewControl(UnitVersionForm, TopBox, 'TreeView');
    TreeView.Align := alClient;
    RCSFilePanel := DynEngine.CreatePanelControl(UnitVersionForm, BottomBox, 'RCSFilePanel', '', alTop);
    RCSFilePanel.Align := alTop;
    RCSFileEdit := DynEngine.CreateEditControl(UnitVersionForm, RCSFilePanel, 'RCSFileEdit');
    RCSFileEdit.Width := 380;
    if Supports(RCSFileEdit, IJvDynControlReadOnly, IJvReadOnly) then
      IJvReadOnly.ControlSetReadOnly(True);
    LabelControl := DynEngine.CreateLabelControlPanel(UnitVersionForm, RCSFilePanel, 'RCSFileLabel', 'RCS File',
      RCSFileEdit, False, 80);
    RCSFilePanel.Height := RCSFileEdit.Height + 1;
    RevisionPanel := DynEngine.CreatePanelControl(UnitVersionForm, BottomBox, 'RevisionPanel', '', alTop);
    RevisionPanel.Align := alTop;
    RevisionEdit := DynEngine.CreateEditControl(UnitVersionForm, RevisionPanel, 'RevisionEdit');
    RevisionEdit.Width := 380;
    LabelControl := DynEngine.CreateLabelControlPanel(UnitVersionForm, RevisionPanel, 'RevisionLabel', 'Revision',
      RevisionEdit, False, 80);
    if Supports(RevisionEdit, IJvDynControlReadOnly, IJvReadOnly) then
      IJvReadOnly.ControlSetReadOnly(True);
    RevisionPanel.Height := RevisionEdit.Height + 1;
    DatePanel := DynEngine.CreatePanelControl(UnitVersionForm, BottomBox, 'DatePanel', '', alTop);
    DatePanel.Align := alTop;
    DateEdit := DynEngine.CreateEditControl(UnitVersionForm, DatePanel, 'DateEdit');
    DateEdit.Width := 380;
    LabelControl := DynEngine.CreateLabelControlPanel(UnitVersionForm, DatePanel, 'DateLabel', 'Date', DateEdit, False,
      80);
    if Supports(DateEdit, IJvDynControlReadOnly, IJvReadOnly) then
      IJvReadOnly.ControlSetReadOnly(True);
    DatePanel.Height := DateEdit.Height + 1;
    PathPanel := DynEngine.CreatePanelControl(UnitVersionForm, BottomBox, 'PathPanel', '', alTop);
    PathPanel.Align := alTop;
    PathEdit := DynEngine.CreateEditControl(UnitVersionForm, PathPanel, 'PathEdit');
    PathEdit.Width := 380;
    LabelControl := DynEngine.CreateLabelControlPanel(UnitVersionForm, PathPanel, 'PathLabel', 'Path', PathEdit, False, 80);
    if Supports(PathEdit, IJvDynControlReadOnly, IJvReadOnly) then
      IJvReadOnly.ControlSetReadOnly(True);
    PathPanel.Height := PathEdit.Height + 1;
    ExtraPanel := DynEngine.CreatePanelControl(UnitVersionForm, BottomBox, 'ExtraPanel', '', alTop);
    ExtraPanel.Align := alTop;
    ExtraEdit := DynEngine.CreateMemoControl(UnitVersionForm, ExtraPanel, 'ExtraEdit');
    // if Supports(ExtraEdit, IJvDynControlReadOnly, IJvReadOnly) then
    // IJvReadOnly.ControlSetReadOnly(True);
    ExtraEdit.Width := 400;
    LabelControl := DynEngine.CreateLabelControlPanel(UnitVersionForm, ExtraPanel, 'ExtraLabel', 'Extra', ExtraEdit, True, 80);
    LabelControl.Width := 80 + PathEdit.Width;
    ExtraPanel.Height := LabelControl.Height;

    BottomBox.Height := DatePanel.Height * 4 + 10 + ExtraPanel.Height;

    if Supports(TreeView, IJvDynControlReadOnly, IJvReadOnly) then
      IJvReadOnly.ControlSetReadOnly(True);

    if Supports(TreeView, IJvDynControlTreeView, IJvTreeView) then
    begin
      Nodes := IJvTreeView.ControlGetItems;
      Nodes.Clear;
      IJvTreeView.ControlSetOnChange(TreeViewOnChange);
      MainNode := Nodes.AddChild(nil, ExtractFileName(ParamStr(0)) + ' ' + VersionFixedFileInfoString(ParamStr(0)));
      {$IFDEF UNITVERSIONING}
      for I := 0 to GetUnitVersioning.Count - 1 do
      begin
        Item := GetUnitVersioning.Items[I];
        Nodes.AddChildObject(FindMasterNode(Nodes, MainNode, Item.LogPath),
          StrRestOf(Item.RCSfile, StrLastPos('/', Item.RCSfile) + 1) + ' - ' + Item.Revision, Item);
      end;
      {$ENDIF UNITVERSIONING}
      IJvTreeView.ControlSetSortType(stText);
      if TreeView is TTreeView then
        TTreeView(TreeView).FullExpand;
      MainNode.Selected := True;
    end;
    TreeViewOnChange(nil, nil);
    UnitVersionForm.ShowModal;
  finally
    UnitVersionForm.Release; // keep the form created till all used interfaces are cleared
  end;
end;

procedure TJvJclUnitVersioningBrowser.TreeViewOnChange(Sender: TObject; Node: TTreeNode);
{$IFDEF UNITVERSIONING}
var
  IJvData: IJvDynControlData;
{$ENDIF UNITVERSIONING}
begin
  {$IFDEF UNITVERSIONING}
  if Assigned(Node) and Assigned(Node.Data) and
    (TObject(Node.Data) is TUnitVersion) then
  begin
    if Supports(RCSFileEdit, IJvDynControlData, IJvData) then
    begin
      IJvData.ControlValue := TUnitVersion(Node.Data).RCSfile;
      RCSFilePanel.Visible := True;
    end
    else
      RCSFilePanel.Visible := False;
    if Supports(RevisionEdit, IJvDynControlData, IJvData) then
    begin
      IJvData.ControlValue := TUnitVersion(Node.Data).Revision;
      RevisionPanel.Visible := True;
    end
    else
      RevisionPanel.Visible := False;
    if Supports(DateEdit, IJvDynControlData, IJvData) then
    begin
      IJvData.ControlValue := TUnitVersion(Node.Data).Date;
      DatePanel.Visible := True;
    end
    else
      DatePanel.Visible := False;
    if Supports(ExtraEdit, IJvDynControlData, IJvData) and
      (TUnitVersion(Node.Data).Extra <> '') then
    begin
      IJvData.ControlValue := TUnitVersion(Node.Data).Extra;
      ExtraPanel.Visible := True;
    end
    else
      ExtraPanel.Visible := False;
    if Supports(PathEdit, IJvDynControlData, IJvData) then
    begin
      IJvData.ControlValue := TUnitVersion(Node.Data).LogPath;
      PathPanel.Visible := True;
    end
    else
      PathPanel.Visible := False;
  end
  else
  {$ENDIF UNITVERSIONING}
  begin
    RCSFilePanel.Visible := False;
    RevisionPanel.Visible := False;
    DatePanel.Visible := False;
    ExtraPanel.Visible := False;
    PathPanel.Visible := False;
  end;
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
end.