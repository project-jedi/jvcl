{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvOLBEditor.PAS, released on 2002-12-10.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-12-06

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvOLBEditor;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, ToolWin,
  Menus, ActnList, ComCtrls, ImgList,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf, DesignMenus, DesignWindows,
  {$ELSE}
  DsgnIntf, DsgnWnds,
  {$ENDIF}
  JvDsgnEditors, JvOLBar;

type
  TFrmOLBEditor = class(TDesignWindow)
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
    property OutlookBar: TJvCustomOutlookBar read FOutlookBar write SetOutlookBar;
  public
    procedure Activated; override;
    function GetEditState: TEditState; override;
    {$IFDEF COMPILER6_UP}
    procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); override;
    procedure DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean); override;
    procedure ItemsModified(const Designer: IDesigner); override;
    {$ELSE}
    procedure ComponentDeleted(Component: IPersistent); override;
    function UniqueName(Component: TComponent): string; override;
    procedure FormClosed(AForm: TCustomForm); override;
    procedure FormModified; override;
    {$ENDIF}
  end;

  TJvOutlookBarActivePageEditor = class(TIntegerProperty)
  private
    function GetOL: TJvCustomOutlookBar;
  protected
    property OL: TJvCustomOutlookBar read GetOL;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TJvOutlookBarComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TJvOutlookBarPagesPropertyEditor = class(TPropertyEditor)
  private
    function GetOutlookBar: TJvCustomOutlookBar;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  TJvOutlookBarButtonImageIndexProperty = class(TJvDefaultImageIndexProperty)
  protected
    function GetPage: TJvOutlookBarPage;
    function GetBar: TJvCustomOutlookBar;
    function ImageList: TCustomImageList; override;
  end;

procedure Register;

implementation

uses
  Registry, Dialogs;

{$R *.dfm}

const
  cJvOutlookBar = 'TJvOutlookBar';

type
  THackOL = class(TJvCustomOutlookBar);

resourcestring
  SFmtCaption = 'Editing %s';
  SCaption = 'OutlookBar Editor';
  SOLEditor = 'OutlookBar Editor...';

procedure Register;
begin
  RegisterComponentEditor(TJvCustomOutlookBar, TJvOutlookBarComponentEditor);
  RegisterPropertyEditor(TypeInfo(Integer),
    TJvCustomOutlookBar, 'ActivePageIndex', TJvOutlookBarActivePageEditor);
  RegisterPropertyEditor(TypeInfo(TJvOutlookBarPages),
    TJvCustomOutlookBar, '', TJvOutlookBarPagesPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TJvOutlookBarButtons),
    TJvOutlookBarPage, '', TJvOutlookBarPagesPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer),
    TJvOutlookBarButton, 'ImageIndex', TJvOutlookBarButtonImageIndexProperty);
end;

procedure ShowEditor(Designer: IDesigner; OutlookBar: TJvCustomOutlookBar);
var
  I: Integer;
  AEditor: TfrmOLBEditor;
begin
  AEditor := nil;
  for I := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[I] is TFrmOLBEditor then
    begin
      if TFrmOLBEditor(Screen.Forms[I]).OutlookBar = OutlookBar then
      begin
        AEditor := TFrmOLBEditor(Screen.Forms[I]);
        Break;
      end;
    end;
  end;
  // Show the editor
  if Assigned(AEditor) then
  begin
    AEditor.Show;
    if AEditor.WindowState = wsMinimized then
      AEditor.WindowState := wsNormal;
  end
  else
  begin
    AEditor := TFrmOLBEditor.Create(Application);
    try
      {$IFDEF COMPILER6_UP}
      AEditor.Designer := Designer;
      {$ELSE}
      AEditor.Designer := Designer as IFormDesigner;
      {$ENDIF}
      AEditor.OutlookBar := OutlookBar;
      AEditor.Show;
    except
      AEditor.Free;
      raise;
    end;
  end;
end;

//=== TJvOutlookBarPagesPropertyEditor =======================================

procedure TJvOutlookBarPagesPropertyEditor.Edit;
begin
  ShowEditor(Designer, GetOutlookBar);
end;

function TJvOutlookBarPagesPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

type
  THackPages = class(TJvOutlookBarPages);
  THackButtons = class(TjvOutlookBarButtons);

function TJvOutlookBarPagesPropertyEditor.GetOutlookBar: TJvCustomOutlookBar;
begin
  if GetComponent(0) is TJvCustomOutlookBar then
    Result := TJvCustomOutlookBar(GetComponent(0))
  else
    if GetComponent(0) is TJvOutlookBarPage then
    Result := THackOL(THackPages(TJvOutlookBarPage(GetComponent(0)).Collection).GetOwner)
  else
    Result := nil;
end;

function TJvOutlookBarPagesPropertyEditor.GetValue: string;
begin
  Result := Format('(%s)', [GetPropType^.Name]);
end;

//=== TJvOutlookBarComponentEditor ===========================================

procedure TJvOutlookBarComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      ShowEditor(Designer, Component as TJvCustomOutlookBar);
  else
    inherited ExecuteVerb(Index);
  end;
end;

function TJvOutlookBarComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := SOLEditor;
  else
    Result := inherited GetVerb(Index);
  end;
end;

function TJvOutlookBarComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

//=== TJvOutlookBarActivePageEditor ==========================================

procedure TJvOutlookBarActivePageEditor.Edit;
begin
  inherited Edit;
end;

function TJvOutlookBarActivePageEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paRevertable];
end;

function TJvOutlookBarActivePageEditor.GetOL: TJvCustomOutlookBar;
begin
  if GetComponent(0) is TJvCustomOutlookBar then
    Result := TJvCustomOutlookBar(GetComponent(0))
  else
    Result := nil;
end;

function TJvOutlookBarActivePageEditor.GetValue: string;
var
  I: Integer;
begin
  I := GetOrdValue;
  if I < 0 then
    Result := ''
  else
    if I < THackOL(OL).Pages.Count then
    Result := THackOL(OL).Pages[I].Caption
  else
    Result := inherited GetValue;
end;

procedure TJvOutlookBarActivePageEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to THackOL(OL).Pages.Count - 1 do
    Proc(THackOL(OL).Pages[I].Caption);
end;

procedure TJvOutlookBarActivePageEditor.SetValue(const Value: string);
var
  I: Integer;
begin
  I := StrToIntDef(Value, -1);
  if I < 0 then
  begin
    for I := 0 to THackOL(OL).Pages.Count - 1 do
      if AnsiSameText(THackOL(OL).Pages[I].Caption, Value) then
      begin
        SetOrdValue(I);
        Modified;
        Exit;
      end;
  end
  else
    inherited SetValue(Value);
end;

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
    Caption := Format(SFmtCaption, [GetFullPathName(OutlookBar)])
  else
    Caption := SCaption;
end;

function TFrmOLBEditor.GetEditState: TEditState;
begin
  Result := [];
end;

{$IFDEF COMPILER6_UP}

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

{$ELSE}

procedure TFrmOLBEditor.ComponentDeleted(Component: IPersistent);
var
  P: TPersistent;
begin
  P := ExtractPersistent(Component);
  if P = OutlookBar then
  begin
    OutlookBar := nil;
    Close;
  end;
end;

procedure TFrmOLBEditor.FormClosed(AForm: TCustomForm);
begin
  Assert(Designer <> nil, 'Designer is nil in FormClosed');
  if AForm = Designer.Form then
  begin
    Designer := nil;
    Close;
  end;
end;

procedure TFrmOLBEditor.FormModified;
begin
  if not (csDestroying in ComponentState) then
    UpdateItems;
end;

function TFrmOLBEditor.UniqueName(Component: TComponent): string;
begin
  Result := Designer.UniqueName(Component.ClassName);
end;
{$ENDIF}

procedure TFrmOLBEditor.SelectItem(Node: TTreeNode);
begin
  if (Node <> nil) and (Node.Data <> nil) then
  begin
    if TObject(Node.Data) is TJvOutlookBarPage then
      THackOL(OutlookBar).ActivePageIndex := TJvOutlookBarPage(Node.Data).Index
    else
      if TObject(Node.Data) is TJvOutlookBarButton then
    begin
      THackOL(OutlookBar).ActivePageIndex := TJvOutlookBarPage(Node.Parent.Data).Index;
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
      if (N = Item) then
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
      for I := 0 to THackOL(OutlookBar).Pages.Count - 1 do
      begin
        N := tvItems.Items.AddObject(nil, THackOL(OutlookBar).Pages[I].Caption,
          THackOL(OutlookBar).Pages[I]);
        if THackOL(OutlookBar).Pages[I] = THackOL(OutlookBar).ActivePage then
          N2 := N;
        for J := 0 to THackOL(OutlookBar).Pages[I].Buttons.Count - 1 do
          tvItems.Items.AddChildObject(N,
            THackOL(OutlookBar).Pages[I].Buttons[J].Caption,
            THackOL(OutlookBar).Pages[I].Buttons[J]);
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
begin
  acNewButton.Enabled := tvItems.Selected <> nil;
  acDelete.Enabled := tvItems.Selected <> nil;
  acUp.Enabled := (tvItems.Selected <> nil) and (tvItems.Selected.getPrevSibling <> nil);
  acDown.Enabled := (tvItems.Selected <> nil) and (tvItems.Selected.getNextSibling <> nil);
  acUpdate.Enabled := Screen.ActiveForm = self;
end;

procedure TFrmOLBEditor.acNewPageExecute(Sender: TObject);
var
  P: TJvOutlookBarPage;
begin
  P := THackOL(OutlookBar).Pages.Add;
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
      THackOL(OutlookBar).Pages.Delete(TJvOutlookBarPage(pr).Index)
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
  cTemplate = 'JvOutlookBarButton%d';
var
  K: Integer;
  Tmp: string;

  function IsUnique(const S: string): Boolean;
  var
    I, J: Integer;
  begin
    Result := False;
    for I := 0 to THackOL(OLBar).Pages.Count - 1 do
      for J := 0 to THackOL(OLBar).Pages[I].Buttons.Count - 1 do
        if AnsiSameText(THackOL(OLBar).Pages[I].Buttons[J].Caption, S) then
          Exit;
    Result := True;
  end;

begin
  Result := 'JvOutlookBarButton';
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
  cTemplate = 'JvOutlookBarPage%d';
var
  K: Integer;
  Tmp: string;

  function IsUnique(const S: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to THackOL(OLBar).Pages.Count - 1 do
      if AnsiSameText(THackOL(OLBar).Pages[I].Caption, S) then
        Exit;
    Result := True;
  end;

begin
  Result := 'JvOutlookPage';
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
  if Screen.ActiveForm = self then
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
    // Width,Height,TextLabels
    R.WriteInteger(cJvOutlookBar, 'Width', Width);
    R.WriteInteger(cJvOutlookBar, 'Height', Height);
    R.WriteBool(cJvOutlookBar, 'TextLabels', acShowTextLabels.Checked);
    R.WriteBool(cJvOutlookBar, 'ToolBar', acToolBar.Checked);
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
    // Width,Height,TextLabels
    Width := R.ReadInteger(cJvOutlookBar, 'Width', Width);
    Height := R.ReadInteger(cJvOutlookBar, 'Height', Height);
    acToolBar.Checked := not R.ReadBool(cJvOutlookBar, 'ToolBar', True);
    acToolBar.Execute;
    acShowTextLabels.Checked := not R.ReadBool(cJvOutlookBar, 'TextLabels', False);
    acShowTextLabels.Execute; // toggles
  finally
    R.Free;
  end;
end;

function TFrmOLBEditor.GetRegPath: string;
const
  cRegKey = '\Property Editors\JVCL\OutlookBar Editor';
begin
  {$IFDEF COMPILER6_UP}
  Result := Designer.GetBaseRegKey + cRegKey;
  {$ENDIF}
  {$IFDEF DELPHI5}
  Result := '\Software\Borland\Delphi\5.0\' + cRegKey;
  {$ENDIF}
  {$IFDEF BCB5}
  Result := '\Software\Borland\C++Builder\5.0\' + cRegKey;
  {$ENDIF}
  // previous versions (4.0-1.0) not supported
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

//=== TJvOutlookBarButtonImageIndexProperty ==================================

function TJvOutlookBarButtonImageIndexProperty.GetBar: TJvCustomOutlookBar;
begin
  Result := THackPages(GetPage.Collection).GetOwner as TJvCustomOutlookBar;
end;

function TJvOutlookBarButtonImageIndexProperty.GetPage: TJvOutlookBarPage;
begin
  Result := TJvOutlookBarPage(THackButtons((GetComponent(0) as TJvOutlookBarButton).Collection).GetOwner);
end;

function TJvOutlookBarButtonImageIndexProperty.ImageList: TCustomImageList;
begin
  if GetPage.ButtonSize = olbsLarge then
    Result := THackOL(GetBar).LargeImages
  else
    Result := THackOL(GetBar).SmallImages;
end;

end.

