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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ActnList, ComCtrls, ImgList, ToolWin, JvOLBar,
{$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf, DesignMenus, DesignWindows,
{$ELSE}
  DsgnIntf, DsgnWnds,
{$ENDIF}
  JvDsgnEditors;

type
  TfrmOLBEditor = class(TDesignWindow)
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
    { Private declarations }
    FOutlookBar: TJvCustomOutlookBar;
    function GetRegPath: string;
    procedure UpdateList;
    procedure UpdateItems;
    procedure SetOutlookBar(const Value: TJvCustomOutlookBar);
    class function GetButtonName(OLBar: TJvCustomOutlookBar): string;
    class function GetPageName(OLBar: TJvCustomOutlookBar): string;
    procedure DeleteItem(Item: TPersistent);
  protected
    procedure StoreSettings;
    procedure LoadSettings;
    procedure SelectItem(Node: TTreeNode);
    property OutlookBar: TJvCustomOutlookBar read FOutlookBar write SetOutlookBar;
  public
    { Public declarations }
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
  Registry;

{$R *.dfm}

type
  THackOL = class(TJvCustomOutlookBar);

resourcestring
  SFmtCaption = 'Editing %s';
  SCaption = 'OutlookBar Editor';

  SOLEditor = 'OutlookBar Editor...';

procedure Register;
begin
  RegisterComponentEditor(TJvCustomOutlookBar, TJvOutlookBarComponentEditor);
  RegisterPropertyEditor(typeinfo(integer),
    TJvCustomOutlookBar,
    'ActivePageIndex', TJvOutlookBarActivePageEditor);
  RegisterPropertyEditor(
    typeinfo(TJvOutlookBarPages), TJvCustomOutlookBar,
    '', TJvOutlookBarPagesPropertyEditor);
  RegisterPropertyEditor(
    typeinfo(TJvOutlookBarButtons), TJvOutlookBarPage,
    '', TJvOutlookBarPagesPropertyEditor);
  RegisterPropertyEditor(typeinfo(integer), TJvOutlookBarButton,
    'ImageIndex',TJvOutlookBarButtonImageIndexProperty);
end;

procedure ShowEditor(Designer: IDesigner; OutlookBar: TJvCustomOutlookBar);
var
  i: Integer;
  AEditor: TfrmOLBEditor;
begin
  AEditor := nil;
  for i := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[i] is TfrmOLBEditor then
    begin
      if TfrmOLBEditor(Screen.Forms[i]).OutlookBar = OutlookBar then
      begin
        AEditor := TfrmOLBEditor(Screen.Forms[i]);
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
    AEditor := TfrmOLBEditor.Create(Application);
    try
{$IFDEF COMPILER6_UP}
      AEditor.Designer := Designer;
{$ELSE}
      AEditor.Designer := IFormDesigner(Designer);
{$ENDIF}
      AEditor.OutlookBar := OutlookBar;
      AEditor.Show;
    except
      if Assigned(AEditor) then
        AEditor.Free;
      raise;
    end;
  end;
end;

{ TJvOutlookBarPagesPropertyEditor }

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
  else if GetComponent(0) is TJvOutlookBarPage then
    Result := THackOL(THackPages(TJvOutlookBarPage(GetComponent(0)).Collection).GetOwner)
  else
    Result := nil;
end;

function TJvOutlookBarPagesPropertyEditor.GetValue: string;
begin
  Result := Format('(%s)', [GetPropType^.Name]);
end;

{ TJvOutlookBarComponentEditor }

procedure TJvOutlookBarComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ShowEditor(Designer, Component as TJvCustomOutlookBar);
  else
    inherited ExecuteVerb(Index);
  end;
end;

function TJvOutlookBarComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := SOLEditor;
  else
    Result := inherited GetVerb(Index);
  end;
end;

function TJvOutlookBarComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TJvOutlookBarActivePageEditor }

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
var i: integer;
begin
  i := GetOrdValue;
  if i < 0 then
    Result := ''
  else if i < THackOL(OL).Pages.Count then
    Result := THackOL(OL).Pages[i].Caption
  else
    Result := inherited GetValue;
end;

procedure TJvOutlookBarActivePageEditor.GetValues(Proc: TGetStrProc);
var i: integer;
begin
  for i := 0 to THackOL(OL).Pages.Count - 1 do
    Proc(THackOL(OL).Pages[i].Caption);
end;

procedure TJvOutlookBarActivePageEditor.SetValue(const Value: string);
var i: integer;
begin
  i := StrToIntDef(Value, -1);
  if i < 0 then
  begin
    for i := 0 to THackOL(OL).Pages.Count - 1 do
      if AnsiSameText(THackOL(OL).Pages[i].Caption, Value) then
      begin
        SetOrdValue(i);
        Modified;
        Exit;
      end;
  end
  else
    inherited SetValue(Value);
end;

{ TfrmOLBEditor }

function GetFullPathName(C: TComponent): string;
begin
  Result := C.Name;
  while C.Owner <> nil do
  begin
    C := C.Owner;
    Result := C.Name + '.' + Result;
  end;
end;

procedure TfrmOLBEditor.Activated;
begin
  UpdateItems;
  if OutlookBar <> nil then
    Caption := Format(SFmtCaption, [GetFullPathName(OutlookBar)])
  else
    Caption := SCaption;
end;

function TfrmOLBEditor.GetEditState: TEditState;
begin
  Result := [];
end;

{$IFDEF COMPILER6_UP}

procedure TfrmOLBEditor.DesignerClosed(const Designer: IDesigner;
  AGoingDormant: Boolean);
begin
  if Designer = Self.Designer then
    Close;
end;

procedure TfrmOLBEditor.ItemDeleted(const ADesigner: IDesigner;
  Item: TPersistent);
begin
  if Item = FOutlookBar then
  begin
    OutlookBar := nil;
    Close;
  end
  else
    DeleteItem(Item);
end;

procedure TfrmOLBEditor.ItemsModified(const Designer: IDesigner);
begin
  if not (csDestroying in ComponentState) then
    UpdateItems;
end;

{$ELSE}

procedure TfrmOLBEditor.ComponentDeleted(Component: IPersistent);
begin
  if ExtractPersistent(Component) = OutlookBar then
  begin
    OutlookBar := nil;
    Close;
  end
  else
    DeleteItem(ExtractPersistent(Component));
end;

procedure TfrmOLBEditor.FormClosed(AForm: TCustomForm);
begin
  if AForm = Designer.Form then
    Close;
end;

procedure TfrmOLBEditor.FormModified;
begin
  if not (csDestroying in ComponentState) then
    UpdateItems;
end;

function TfrmOLBEditor.UniqueName(Component: TComponent): string;
begin
  Result := Designer.UniqueName(Component.ClassName);
end;
{$ENDIF}

procedure TfrmOLBEditor.SelectItem(Node: TTreeNode);
begin
  if (Node <> nil) and (Node.Data <> nil) then
  begin
    if TObject(Node.Data) is TJvOutlookBarPage then
      THackOL(OutlookBar).ActivePageIndex := TJvOutlookBarPage(Node.Data).Index
    else if TObject(Node.Data) is TJvOutlookBarButton then
    begin
      THackOL(OutlookBar).ActivePageIndex := TJvOutlookBarPage(Node.Parent.Data).Index;
      Node.Parent.Expand(false);
    end;
    Designer.SelectComponent(TPersistent(Node.Data));
    Designer.Modified;
  end;
end;

procedure TfrmOLBEditor.SetOutlookBar(const Value: TJvCustomOutlookBar);
begin
  if FOutlookBar <> Value then
  begin
    FOutlookBar := Value;
    UpdateList;
  end;
end;

procedure TfrmOLBEditor.DeleteItem(Item: TPersistent);
var N, N2: TTreeNode;
  function FindNextNode(const Node: TTreeNode): TTreeNode;
  begin
    if Node.GetNextSibling <> nil then
      Result := Node.GetNextSibling
    else if Node.GetPrevSibling <> nil then
      Result := Node.GetPrevSibling
    else if Node.GetPrevVisible <> nil then
      Result := Node.GetPrevVisible
    else if Node.GetNextVisible <> nil then
      Result := Node.GetNextVisible
    else if Node.GetPrev <> nil then
      Result := Node.GetPrev
    else if Node.GetNext <> nil then
      Result := Node.GetNext
    else if Node.Parent <> nil then
      Result := Node.Parent
    else
      Result := tvItems.Items.GetFirstNode;
  end;
begin
  N2 := tvItems.Selected;
  N := tvItems.Items.GetFirstNode;
  while Assigned(N) do
  begin
    if N.Data = Item then
    begin
      N2 := FindNextNode(N);
      N.Data := nil;
      N.Delete;
      Break;
    end;
    N := N.GetNext;
  end;
  if tvItems.CanFocus then
    tvItems.Selected := N2;
end;

procedure TfrmOLBEditor.UpdateList;
var i, j: integer; n, n2: TTreeNode; HAllocated: boolean;
begin
  n2 := nil;
  HAllocated := tvItems.HandleAllocated;
  if HAllocated then
    tvItems.Items.BeginUpdate;
  try
    tvItems.Items.Clear;
    if OutlookBar <> nil then
    begin
      for i := 0 to THackOL(OutlookBar).Pages.Count - 1 do
      begin
        n := tvItems.Items.AddObject(nil, THackOL(OutlookBar).Pages[i].Caption,
          THackOL(OutlookBar).Pages[i]);
        if THackOL(OutlookBar).Pages[i] = THackOL(OutlookBar).ActivePage then
          n2 := n;
        for j := 0 to THackOL(OutlookBar).Pages[i].Buttons.Count - 1 do
          tvItems.Items.AddChildObject(n,
            THackOL(OutlookBar).Pages[i].Buttons[j].Caption,
            THackOL(OutlookBar).Pages[i].Buttons[j]);
        n.Expand(true);
      end;
    end;
  finally
    if HAllocated then
      tvItems.Items.EndUpdate;
  end;
  tvItems.FullExpand;
  SelectItem(n2);
end;

procedure TfrmOLBEditor.alActionsUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  acNewButton.Enabled := tvItems.Selected <> nil;
  acDelete.Enabled := tvItems.Selected <> nil;
  acUp.Enabled := (tvItems.Selected <> nil) and (tvItems.Selected.getPrevSibling <> nil);
  acDown.Enabled := (tvItems.Selected <> nil) and (tvItems.Selected.getNextSibling <> nil);
  acUpdate.Enabled := Screen.ActiveForm = self;
end;

procedure TfrmOLBEditor.acNewPageExecute(Sender: TObject);
var p: TJvOutlookBarPage;
begin
  p := THackOL(OutlookBar).Pages.Add;
  p.Caption := GetPageName(OutlookBar);
  tvItems.Selected := tvItems.Items.AddObject(nil, p.Caption, p);
end;

procedure TfrmOLBEditor.acNewButtonExecute(Sender: TObject);
var b: TJvOutlookBarButton; p: tJvOutlookBarPage; n: TTreeNode;
begin
  n := tvItems.Selected;
  if n.Parent <> nil then
    n := n.Parent;
  p := TJvOutlookBarPage(n.Data);
  b := p.Buttons.Add;
  b.Caption := getButtonName(OutlookBar);
  tvItems.Selected := tvItems.Items.AddChildObject(n, b.Caption, b);
end;

procedure TfrmOLBEditor.acDeleteExecute(Sender: TObject);
var
  pr: TPersistent;
  p: TJvOutlookBarPage;
  n: TTreeNode;
begin
  tvItems.Items.BeginUpdate;
  try
    n := tvItems.Selected;
    pr := TPersistent(n.Data);
    if pr is TJvOutlookBarPage then
      THackOL(OutlookBar).Pages.Delete(TJvOutlookBarPage(pr).Index)
    else if pr is TJvOutlookBarButton then
    begin
      p := TJvOutlookBarPage(n.Parent.Data);
      p.Buttons.Delete(TJvOutlookBarButton(pr).Index);
    end;
    DeleteItem(n);
  finally
    tvItems.Items.EndUpdate;
  end;
end;

procedure TfrmOLBEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

class function TfrmOLBEditor.GetButtonName(OLBar: TJvCustomOutlookBar): string;
const
  cTemplate = 'JvOutlookBarButton%d';
var k: integer; tmp: string;
  function IsUnique(const S: string): boolean;
  var i, j: integer;
  begin
    Result := false;
    for i := 0 to THackOL(OLBar).Pages.Count - 1 do
      for j := 0 to THackOL(OLBar).Pages[i].Buttons.Count - 1 do
        if AnsiSameText(THackOL(OLBar).Pages[i].Buttons[j].Caption, S) then
          Exit;
    Result := true;
  end;
begin
  Result := 'JvOutlookBarButton';
  if OLBar <> nil then
    for k := 1 to MaxInt - 1 do
    begin
      tmp := Format(cTemplate, [k]);
      if IsUnique(tmp) then
      begin
        Result := tmp;
        Exit;
      end;
    end;
end;

class function TfrmOLBEditor.GetPageName(OLBar: TJvCustomOutlookBar): string;
const
  cTemplate = 'JvOutlookBarPage%d';
var k: integer; tmp: string;
  function IsUnique(const S: string): boolean;
  var i: integer;
  begin
    Result := false;
    for i := 0 to THackOL(OLBar).Pages.Count - 1 do
      if AnsiSameText(THackOL(OLBar).Pages[i].Caption, S) then
        Exit;
    Result := true;
  end;
begin
  Result := 'JvOutlookPage';
  if OLBar <> nil then
    for k := 1 to MaxInt - 1 do
    begin
      tmp := Format(cTemplate, [k]);
      if IsUnique(tmp) then
      begin
        Result := tmp;
        Exit;
      end;
    end;
end;

procedure TfrmOLBEditor.UpdateItems;
var N, N2: TTreeNode;
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
      else if TObject(N.Data) is TJvOutlookBarButton then
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

procedure TfrmOLBEditor.tvItemsChange(Sender: TObject; Node: TTreeNode);
begin
  SelectItem(Node);
end;

procedure TfrmOLBEditor.tvItemsCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  AllowCollapse := false;
end;

procedure TfrmOLBEditor.tvItemsKeyPress(Sender: TObject; var Key: Char);
begin
  if tvItems.Selected <> nil then
  begin
    SelectItem(tvItems.Selected);
    ActivateInspector(Key);
    Key := #0;
  end;
end;

procedure TfrmOLBEditor.acUpdateExecute(Sender: TObject);
begin
  UpdateList;
end;

procedure TfrmOLBEditor.acUpExecute(Sender: TObject);
var N: TTreeNode;
begin
  N := tvItems.Selected;
  N.MoveTo(N.getPrevSibling, naInsert);
  N.Expand(true);
  tvItems.Selected := N;
end;

procedure TfrmOLBEditor.acDownExecute(Sender: TObject);
var N: TTreeNode;
begin
  N := tvItems.Selected.getNextSibling;
  N.MoveTo(tvItems.Selected, naInsert);
  N.Expand(true);
end;

function Max(Val1, Val2: integer): integer;
begin
  Result := Val1;
  if Val2 > Val1 then
    Result := Val2;
end;

procedure TfrmOLBEditor.acShowTextLabelsExecute(Sender: TObject);
begin
  acShowTextLabels.Checked := not acShowTextLabels.Checked;
  tbTop.ShowCaptions := acShowTextLabels.Checked;
  tbTop.ButtonHeight := 16;
  tbTop.ButtonWidth := 16;
//  ClientWidth := Max(ClientWidth, btnDown.Left + btnDown.Width + 4);
end;

procedure TfrmOLBEditor.StoreSettings;
var R: TRegIniFile;
begin
  R := TRegIniFile.Create;
  try
    R.RootKey := HKEY_CURRENT_USER;
    R.OpenKey(GetRegPath, true);
    // Width,Height,TextLabels
    R.WriteInteger('TJvOutlookBar', 'Width', Width);
    R.WriteInteger('TJvOutlookBar', 'Height', Height);
    R.WriteBool('TJvOutlookBar', 'TextLabels', acShowTextLabels.Checked);
    R.WriteBool('TJvOutlookBar', 'ToolBar', acToolBar.Checked);
  finally
    R.Free;
  end;
end;

procedure TfrmOLBEditor.LoadSettings;
var R: TRegIniFile;
begin
  R := TRegIniFile.Create;
  try
    R.RootKey := HKEY_CURRENT_USER;
    R.OpenKey(GetRegPath, true);
    // Width,Height,TextLabels
    Width := R.ReadInteger('TJvOutlookBar', 'Width', Width);
    Height := R.ReadInteger('TJvOutlookBar', 'Height', Height);
    acToolBar.Checked := not R.ReadBool('TJvOutlookBar', 'ToolBar', true);
    acToolBar.Execute;
    acShowTextLabels.Checked := not R.ReadBool('TJvOutlookBar', 'TextLabels', false);
    acShowTextLabels.Execute; // toggles
  finally
    R.Free;
  end;
end;

function TfrmOLBEditor.GetRegPath: string;
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

{ TJvOutlookBarButtonImageIndexProperty }

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

procedure TfrmOLBEditor.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  StoreSettings;
end;

procedure TfrmOLBEditor.FormShow(Sender: TObject);
begin
  LoadSettings;
end;

procedure TfrmOLBEditor.acToolBarExecute(Sender: TObject);
begin
  acToolBar.Checked := not acToolBar.Checked;
  tbTop.Visible := acToolBar.Checked;
end;

end.

