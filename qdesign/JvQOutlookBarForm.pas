{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvOLBEditor.PAS, released on 2002-12-10.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 att users dott sourceforge dott net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQOutlookBarForm;

interface

uses
  SysUtils, Classes,

  Windows,
  
  QControls, QForms, QToolWin,
  QMenus, QActnList, QComCtrls, QImgList,
  DesignEditors, DesignIntf, DesignMenus, ClxDesignWindows,
  
  JvQOutlookBar, QTypes, QExtCtrls;

type
  
  
  TFrmOLBEditor = class(TClxDesignWindow)
  
    tbTop: TToolBar;
    btnNew: TToolButton;
    btnDel: TToolButton;
    ToolButton3: TToolButton;
    btnUp: TToolButton;
    btnDown: TToolButton;
    il16: TImageList;
    popNew: TPopupMenu;
    tvItems: TTreeView;
    StatusBar1: TStatusBar;
    alActions: TActionList;
    acNewPage: TAction;
    acNewButton: TAction;
    acDelete: TAction;
    acUp: TAction;
    acDown: TAction;
    NewPage1: TMenuItem;
    NewButton1: TMenuItem;
    acUpdate: TAction;
    acShowTextLabels: TAction;
    popToolbar: TPopupMenu;
    extLabels1: TMenuItem;
    popForm: TPopupMenu;
    NewPage2: TMenuItem;
    NewButton2: TMenuItem;
    Delete1: TMenuItem;
    MoveUp1: TMenuItem;
    MoveDown1: TMenuItem;
    N2: TMenuItem;
    acToolBar: TAction;
    oolBar1: TMenuItem;
    N3: TMenuItem;
    procedure alActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure acNewPageExecute(Sender: TObject);
    procedure acNewButtonExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tvItemsChange(Sender: TObject; Node: TTreeNode);
    procedure tvItemsCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure tvItemsKeyPress(Sender: TObject; var Key: Char);
    procedure acUpdateExecute(Sender: TObject);
    procedure acUpExecute(Sender: TObject);
    procedure acDownExecute(Sender: TObject);
    procedure acShowTextLabelsExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure acToolBarExecute(Sender: TObject);
  private
    FOutlookBar: TJvCustomOutlookBar;
    function GetRegPath: string;
    procedure UpdateList;
    procedure UpdateItems;
    procedure SetOutlookBar(const Value: TJvCustomOutlookBar);
    class function GetButtonName(OLBar: TJvCustomOutlookBar): string;
    class function GetPageName(OLBar: TJvCustomOutlookBar): string;
    procedure DeleteItem(Item: TPersistent);
    procedure SwitchItems(Node1, Node2: TTreeNode);
  protected
    procedure StoreSettings;
    procedure LoadSettings;
    procedure SelectItem(Node: TTreeNode);
  public
    property OutlookBar: TJvCustomOutlookBar read FOutlookBar write SetOutlookBar;
    procedure Activated; override;
    function GetEditState: TEditState; override;
    
    procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); override;
    procedure DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean); override;
    procedure ItemsModified(const Designer: IDesigner); override;
    
    function UniqueName(Component: TComponent): string; override;
    
    
  end;

implementation

uses
  Registry,
  
  
  QDialogs,
  
  JvQConsts, JvQDsgnConsts;



{$R *.xfm}


const
  cJvOutlookBar = 'TJvOutlookBar';
  cWidth = 'Width';
  cHeight = 'Height';
  cTextLabels = 'TextLabels';
  cToolBar = 'ToolBar';

type
  THackOutlookBar = class(TJvCustomOutlookBar);

//=== TFrmOLBEditor ==========================================================

function GetFullPathName(C: TComponent): string;
begin
  Result := C.Name;
  while C.Owner <> nil do
  begin
    C := C.Owner;
    Result := C.Name + '.' + Result;
  end;
end;

procedure TFrmOLBEditor.Activated;
begin
  UpdateItems;
  if OutlookBar <> nil then
    Caption := Format(RsFmtCaption, [GetFullPathName(OutlookBar)])
  else
    Caption := RsOutlookBarCaption;
end;

function TFrmOLBEditor.GetEditState: TEditState;
begin
  Result := [];
end;



procedure TFrmOLBEditor.DesignerClosed(const Designer: IDesigner;
  AGoingDormant: Boolean);
begin
  if Designer = Self.Designer then
    Close;
end;

procedure TFrmOLBEditor.ItemDeleted(const ADesigner: IDesigner;
  Item: TPersistent);
begin
  if Item = FOutlookBar then
  begin
    OutlookBar := nil;
    Close;
  end
  else
  if Item is TTreeNode then
    DeleteItem(Item);
end;

procedure TFrmOLBEditor.ItemsModified(const Designer: IDesigner);
begin
  if not (csDestroying in ComponentState) then
    UpdateItems;
end;


function TFrmOLBEditor.UniqueName(Component: TComponent): string;
begin
  Result := Designer.UniqueName(Component.ClassName);
end;




procedure TFrmOLBEditor.SelectItem(Node: TTreeNode);
begin
  if (Node <> nil) and (Node.Data <> nil) then
  begin
    if TObject(Node.Data) is TJvOutlookBarPage then
      THackOutlookBar(OutlookBar).ActivePageIndex := TJvOutlookBarPage(Node.Data).Index
    else
    if TObject(Node.Data) is TJvOutlookBarButton then
    begin
      THackOutlookBar(OutlookBar).ActivePageIndex := TJvOutlookBarPage(Node.Parent.Data).Index;
      Node.Parent.Expand(False);
    end;
    Designer.SelectComponent(TPersistent(Node.Data));
    Designer.Modified;
  end;
end;

procedure TFrmOLBEditor.SetOutlookBar(const Value: TJvCustomOutlookBar);
begin
  if FOutlookBar <> Value then
  begin
    FOutlookBar := Value;
    UpdateList;
  end;
end;

procedure TFrmOLBEditor.DeleteItem(Item: TPersistent);
var
  N, N2: TTreeNode;

  function FindNextNode(const Node: TTreeNode): TTreeNode;
  begin
    if Node = nil then
    begin
      Result := nil;
      Exit;
    end;
    Result := Node.GetNextSibling;
    if Result = nil then
      Result := Node.GetPrevSibling;
    if Result = nil then
      Result := Node.Parent;
    if Result = nil then
      Result := tvItems.Items.GetFirstNode;
    if Result = Node then
      Result := nil;
  end;

begin
  N2 := tvItems.Selected;
  N := tvItems.Items.GetFirstNode;
  try
    while Assigned(N) do
    begin
      if N = Item then
      begin
        N2 := FindNextNode(N);
        N.Data := nil;
        N.Delete;
        Exit;
      end;
      N := N.GetNext;
      N2 := N;
    end;
  finally
    tvItems.Selected := N2;
  end;
end;

procedure TFrmOLBEditor.UpdateList;
var
  I, J: Integer;
  N, N2: TTreeNode;
  HAllocated: Boolean;
begin
  N2 := nil;
  HAllocated := tvItems.HandleAllocated;
  if HAllocated then
    tvItems.Items.BeginUpdate;
  try
    tvItems.Items.Clear;
    if OutlookBar <> nil then
    begin
      for I := 0 to THackOutlookBar(OutlookBar).Pages.Count - 1 do
      begin
        N := tvItems.Items.AddObject(nil, THackOutlookBar(OutlookBar).Pages[I].Caption,
          THackOutlookBar(OutlookBar).Pages[I]);
        if THackOutlookBar(OutlookBar).Pages[I] = THackOutlookBar(OutlookBar).ActivePage then
          N2 := N;
        for J := 0 to THackOutlookBar(OutlookBar).Pages[I].Buttons.Count - 1 do
          tvItems.Items.AddChildObject(N,
            THackOutlookBar(OutlookBar).Pages[I].Buttons[J].Caption,
            THackOutlookBar(OutlookBar).Pages[I].Buttons[J]);
        N.Expand(True);
      end;
    end;
  finally
    if HAllocated then
      tvItems.Items.EndUpdate;
  end;
  tvItems.FullExpand;
  SelectItem(N2);
end;

procedure TFrmOLBEditor.alActionsUpdate(Action: TBasicAction;
  var Handled: Boolean);
var
 Sel: Boolean;
begin
  Sel := tvItems.Selected <> nil;
  acNewButton.Enabled := Sel;
  acDelete.Enabled := Sel;
  acUp.Enabled := Sel and (tvItems.Selected.GetPrevSibling <> nil);
  acDown.Enabled := Sel and (tvItems.Selected.GetNextSibling <> nil);
  acUpdate.Enabled := Screen.ActiveForm = Self;
end;

procedure TFrmOLBEditor.acNewPageExecute(Sender: TObject);
var
  P: TJvOutlookBarPage;
begin
  P := THackOutlookBar(OutlookBar).Pages.Add;
  P.Caption := GetPageName(OutlookBar);
  tvItems.Selected := tvItems.Items.AddObject(nil, P.Caption, P);
end;

procedure TFrmOLBEditor.acNewButtonExecute(Sender: TObject);
var
  B: TJvOutlookBarButton;
  P: tJvOutlookBarPage;
  N: TTreeNode;
begin
  N := tvItems.Selected;
  if N.Parent <> nil then
    N := N.Parent;
  P := TJvOutlookBarPage(N.Data);
  B := P.Buttons.Add;
  B.Caption := getButtonName(OutlookBar);
  tvItems.Selected := tvItems.Items.AddChildObject(N, B.Caption, B);
end;

procedure TFrmOLBEditor.acDeleteExecute(Sender: TObject);
var
  pr: TPersistent;
  P: TJvOutlookBarPage;
  N: TTreeNode;
begin
  tvItems.Items.BeginUpdate;
  try
    N := tvItems.Selected;
    pr := TPersistent(N.Data);
    if pr is TJvOutlookBarPage then
      THackOutlookBar(OutlookBar).Pages.Delete(TJvOutlookBarPage(pr).Index)
    else
    if pr is TJvOutlookBarButton then
    begin
      P := TJvOutlookBarPage(N.Parent.Data);
      P.Buttons.Delete(TJvOutlookBarButton(pr).Index);
    end;
    DeleteItem(N);
  finally
    tvItems.Items.EndUpdate;
  end;
end;

procedure TFrmOLBEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

class function TFrmOLBEditor.GetButtonName(OLBar: TJvCustomOutlookBar): string;
const
  cPrefix = 'JvOutlookBarButton';
  cTemplate = cPrefix + '%d';
var
  K: Integer;
  Tmp: string;

  function IsUnique(const S: string): Boolean;
  var
    I, J: Integer;
  begin
    Result := False;
    for I := 0 to THackOutlookBar(OLBar).Pages.Count - 1 do
      for J := 0 to THackOutlookBar(OLBar).Pages[I].Buttons.Count - 1 do
        if AnsiSameText(THackOutlookBar(OLBar).Pages[I].Buttons[J].Caption, S) then
          Exit;
    Result := True;
  end;

begin
  Result := cPrefix;
  if OLBar <> nil then
    for K := 1 to MaxInt - 1 do
    begin
      Tmp := Format(cTemplate, [K]);
      if IsUnique(Tmp) then
      begin
        Result := Tmp;
        Exit;
      end;
    end;
end;

class function TFrmOLBEditor.GetPageName(OLBar: TJvCustomOutlookBar): string;
const
  cPrefix = 'JvOutlookBarPage';
  cTemplate = cPrefix + '%d';
var
  K: Integer;
  Tmp: string;

  function IsUnique(const S: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to THackOutlookBar(OLBar).Pages.Count - 1 do
      if AnsiSameText(THackOutlookBar(OLBar).Pages[I].Caption, S) then
        Exit;
    Result := True;
  end;

begin
  Result := cPrefix;
  if OLBar <> nil then
    for K := 1 to MaxInt - 1 do
    begin
      Tmp := Format(cTemplate, [K]);
      if IsUnique(Tmp) then
      begin
        Result := Tmp;
        Exit;
      end;
    end;
end;

procedure TFrmOLBEditor.UpdateItems;
var
  N, N2: TTreeNode;
begin
  tvItems.Items.BeginUpdate;
  N2 := nil;
  try
    N := tvItems.Items.GetFirstNode;
    while Assigned(N) do
    begin
      if TObject(N.Data) is TJvOutlookBarPage then
      begin
        N.Text := TJvOutlookBarPage(N.Data).Caption;
        if TJvOutlookBarPage(N.Data) = OutlookBar.ActivePage then
          N2 := N;
      end
      else
      if TObject(N.Data) is TJvOutlookBarButton then
      begin
        N.Text := TJvOutlookBarButton(N.Data).Caption;
        if N.Selected then
          N2 := N;
      end;
      N := N.GetNext;
    end;
  finally
    tvItems.Items.EndUpdate;
  end;
  tvItems.FullExpand;
  if Screen.ActiveForm = Self then
    tvItems.Selected := N2;
end;

procedure TFrmOLBEditor.tvItemsChange(Sender: TObject; Node: TTreeNode);
begin
  SelectItem(Node);
end;

procedure TFrmOLBEditor.tvItemsCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  AllowCollapse := False;
end;

procedure TFrmOLBEditor.tvItemsKeyPress(Sender: TObject; var Key: Char);
begin
  if tvItems.Selected <> nil then
  begin
    SelectItem(tvItems.Selected);
    ActivateInspector(Key);
    Key := #0;
  end;
end;

procedure TFrmOLBEditor.acUpdateExecute(Sender: TObject);
begin
  UpdateList;
end;

procedure TFrmOLBEditor.acUpExecute(Sender: TObject);
var
  N: TTreeNode;
begin
  N := tvItems.Selected;
  SwitchItems(tvItems.Selected, N.getPrevSibling);
  N.MoveTo(N.getPrevSibling, naInsert);
  N.Expand(True);
  tvItems.Selected := N;
end;

procedure TFrmOLBEditor.acDownExecute(Sender: TObject);
var
  N: TTreeNode;
begin
  SwitchItems(tvItems.Selected, tvItems.Selected.getNextSibling);
  N := tvItems.Selected.getNextSibling;
  N.MoveTo(tvItems.Selected, naInsert);
  N.Expand(True);
end;

procedure TFrmOLBEditor.acShowTextLabelsExecute(Sender: TObject);
begin
  acShowTextLabels.Checked := not acShowTextLabels.Checked;
  tbTop.ShowCaptions := acShowTextLabels.Checked;
  tbTop.ButtonHeight := 16;
  tbTop.ButtonWidth := 16;
  //  ClientWidth := Max(ClientWidth, btnDown.Left + btnDown.Width + 4);
end;

procedure TFrmOLBEditor.StoreSettings;
var
  R: TRegIniFile;
begin
  R := TRegIniFile.Create;
  try
    R.RootKey := HKEY_CURRENT_USER;
    R.OpenKey(GetRegPath, True);
    // Width, Height, TextLabels
    R.WriteInteger(cJvOutlookBar, cWidth, Width);
    R.WriteInteger(cJvOutlookBar, cHeight, Height);
    R.WriteBool(cJvOutlookBar, cTextLabels, acShowTextLabels.Checked);
    R.WriteBool(cJvOutlookBar, cToolBar, acToolBar.Checked);
  finally
    R.Free;
  end;
end;

procedure TFrmOLBEditor.LoadSettings;
var
  R: TRegIniFile;
begin
  R := TRegIniFile.Create;
  try
    R.RootKey := HKEY_CURRENT_USER;
    R.OpenKey(GetRegPath, True);
    // Width, Height, TextLabels
    Width := R.ReadInteger(cJvOutlookBar, cWidth, Width);
    Height := R.ReadInteger(cJvOutlookBar, cHeight, Height);
    acToolBar.Checked := not R.ReadBool(cJvOutlookBar, cToolBar, True);
    acToolBar.Execute;
    acShowTextLabels.Checked := not R.ReadBool(cJvOutlookBar, cTextLabels, False);
    acShowTextLabels.Execute; // toggles
  finally
    R.Free;
  end;
end;

function TFrmOLBEditor.GetRegPath: string;
const
  cRegKey = '\JVCL\OutlookBar Editor';
begin
  
  Result := Designer.GetBaseRegKey + cRegKey;
  
end;

procedure TFrmOLBEditor.SwitchItems(Node1, Node2: TTreeNode);
var
  I: Integer;
begin
  if TObject(Node1.Data) is TJvOutlookbarButton then
  begin
    I := TJvOutlookbarButton(Node1.Data).Index;
    TJvOutlookbarButton(Node1.Data).Index := TJvOutlookbarButton(Node2.Data).Index;
    TJvOutlookbarButton(Node2.Data).Index := I;
  end
  else
  if TObject(Node1.Data) is TJvOutlookbarPage then
  begin
    I := TJvOutlookbarPage(Node1.Data).Index;
    TJvOutlookbarPage(Node1.Data).Index := TJvOutlookbarPage(Node2.Data).Index;
    TJvOutlookbarPage(Node2.Data).Index := I;
  end;
end;

procedure TFrmOLBEditor.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  StoreSettings;
end;

procedure TFrmOLBEditor.FormShow(Sender: TObject);
begin
  LoadSettings;
end;

procedure TFrmOLBEditor.acToolBarExecute(Sender: TObject);
begin
  acToolBar.Checked := not acToolBar.Checked;
  tbTop.Visible := acToolBar.Checked;
end;

end.

