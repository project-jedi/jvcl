{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDirPathComboBox.PAS, released on 2003-01-25.

The Initial Developer of the Original Code is Michael Beck [mbeck@bigfoot.com]
Portions created by Michael Beck are Copyright (C) 2003 Michael Beck
All Rights Reserved.

Contributor(s):

Last Modified:  2003-01-25

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:

TO DO: Select subdirectory Node base on text entered into ComboBox
-----------------------------------------------------------------------------}

{$I JVCL.INC}
{$I WINDOWSONLY.INC}

unit JvDirPathComboBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ComCtrls,
  JvCaptionPanel, JvToolEdit, JvComCtrls, ImgList;

type
  TJvTreeViewPopupWindow = class(TJvPopupWindow)
  private
    FTreeView: TJvTreeView;
    FTreeImageList: TCustomImageList;
    procedure SetTreeImageList(const Value: TCustomImageList);
    procedure TVClickButton(Sender: TObject; Button: TJvCapBtnStyle);
    procedure OnTVExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure OnTVChange(Sender: TObject; Node: TTreeNode); virtual;
    procedure OnTVCollapsed(Sender: TObject; Node: TTreeNode);
    procedure OnTVKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnTVSelectionChange(Sender: TObject);
    function FindChildNode(const AName: string;
      CurrentNode: TTreeNode): TTreeNode;
  protected
    function GetValue: Variant; override;
    procedure SetValue(const Value: Variant); override;
    property TreeImageList: TCustomImageList read FTreeImageList write SetTreeImageList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  end;

  TJvTreeViewCombo = class(TJvComboEdit)
  private
    procedure WMMove(var Msg: TWMMove); message WM_MOVE;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;

  protected
    procedure ButtonClick; override;
    procedure Loaded; override;
    procedure HidePanel;
    procedure ShowPanel;
    function GetDirNode(const Path: string): TTreeNode;
  published
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PopulateTree; virtual;
    procedure FillTree(n: TTreeNode); virtual;

  end; { TJvTreeViewCombo }

  TJvDirPathCombo = class(TJvTreeViewCombo)
  protected
    procedure Loaded; override;
  public
    procedure PopulateTree; override;
    procedure FillTree(n: TTreeNode); override;
  end;

procedure Register;

implementation
uses
  Forms, Graphics, JclStrings;

{$R ..\resources\drives.res}

procedure Register;
begin
  RegisterComponents('MB', [TJvTreeViewCombo, TJvDirPathCombo]);
end;

function GetDirStr(Node: TTreeNode): string;
var
  s: string;
  nx: TtreeNode;
begin
  Result := '';
  if (Node = nil) or Node.Deleting then
    Exit;
  s := Node.Text;
  nx := Node;
  while nx.Parent <> nil do
  begin
    nx := nx.Parent;
    s := nx.Text + '\' + s;
  end;
  if (Length(S) > 0) and (AnsiLastChar(S) = ':') then
    Result := S + '\'
  else
    Result := S;
end;

destructor TJvTreeViewCombo.Destroy;
begin
//  FPanel.Free;
  inherited Destroy;
end; { Destroy }

constructor TJvTreeViewCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPopup := TJvTreeViewPopupWindow.Create(self);
//  FPanel.Buttons := FPanel.Buttons + [capClose, capMax, capMin];
//  FPanel.CaptionColor := clBtnFace;
//  FPanel.CaptionPosition := dpBottom;
//  FPanel.OutlookLook := True;
//  FPanel.CaptionColor := clBtnShadow;
  FPopup.Height := 90;
end; { Create }

procedure TJvTreeViewCombo.ButtonClick;
begin
  if FPopup.Visible = False then
    ShowPanel
  else
    HidePanel;
  inherited;
end;

procedure TJvTreeViewCombo.Loaded;
begin
  inherited Loaded;
  FPopup.Width := Width;
  FPopup.Visible := False;
  ButtonWidth := 17;

  Glyph.LoadFromResourceName(HInstance, 'COMBODOWNARROW');
end;

procedure TJvTreeViewCombo.FillTree(n: TTreeNode);
begin

end;

procedure TJvTreeViewCombo.PopulateTree;
begin

end;


procedure TJvTreeViewCombo.HidePanel;
begin
  if FPopup.Visible then
  begin
    ReleaseCapture;
    FPopup.Visible := false;
    if CanFocus then
      SetFocus;
  end;
end;

procedure TJvTreeViewCombo.ShowPanel;
var n:TTreeNode;
begin
  if not FPopup.Visible then
  with TJvTreeViewPopupWindow(FPopup) do
  begin
    FTreeView.OnExpanding := nil;
    FTreeView.OnKeyUp := nil;
    FTreeView.OnCollapsed := nil;
    FTreeView.OnChange := nil;
    FTreeView.OnSelectionChange := nil;
    n := GetDirNode(Text);
    if n <> nil then
      FTreeView.Selected := n
    else if FTreeView.Selected <> nil then
      Text := GetDirStr(FTreeView.Selected)
    else
    begin
      FTreeView.Selected := FTreeView.Items.GetFirstNode;
      Text := GetDirStr(FTreeView.Selected);
    end;
    FPopup.Visible := true;
    if FTreeView.Selected <> nil then
      FTreeView.Selected.MakeVisible;
    if FTreeView.CanFocus then
      FTreeView.SetFocus;
    FTreeView.OnExpanding := OnTVExpanding;
    FTreeView.OnKeyUp := OnTVKeyUp;
    FTreeView.OnCollapsed := OnTVCollapsed;
    FTreeView.OnChange := OnTVChange;
    FTreeView.OnSelectionChange := OnTVSelectionChange;
  end;
end;

//-------------- TJvDirPathCombo --------------

procedure TJvDirPathCombo.Loaded;
begin
  inherited Loaded;
  ButtonWidth := 21;
  FTreeView.Images := FTreeImageList;
  FTreeImageList.ResInstLoad(HInstance, rtBitmap, 'DRIVES', clFuchsia);
  Glyph.LoadFromResourceName(HInstance, 'OPENDRIVE');
  Text := 'C:\';
end;

procedure TJvDirPathCombo.FillTree(n: TTreeNode);
var
  path: string;
  Search: TSearchrec;
  nc: TTreeNode;
begin
  if n = nil then
    Exit;
  Path := IncludeTrailingPathDelimiter(GetDirStr(n));
  if FindFirst(Path + '*.*', faDirectory, Search) = 0 then
    repeat
      if ((Search.Attr and faDirectory) = faDirectory) and (Search.Name <> '.') and (Search.Name <> '..') then
      begin
        nc := FTreeview.Items.AddChild(n, Search.Name);
        nc.ImageIndex := 0;
        nc.SelectedIndex := 0;
        // nc.overlayindex := 1;
        FTreeview.Items.AddChild(nc, 'y');
      end;
    until FindNext(Search) <> 0;
  FindClose(Search);
end;

procedure TJvDirPathCombo.PopulateTree;
var
  nc: TTreeNode;
  i, DriveType: integer;
  s: string;
begin
  FTreeview.Items.Clear;
  for i := 0 to 25 do
  begin
    s := Chr(i + 65) + ':\';
    DriveType := GetDriveType(PChar(s));
    if DriveType <> 0 then
      case DriveType of
        DRIVE_REMOVABLE:
          begin
            nc := FTreeview.Items.AddChild(nil, Chr(i + 65) + ':');
            nc.ImageIndex := 4;
            nc.SelectedIndex := 4;
            FTreeview.Items.AddChild(nc, 'y');
          end;
        DRIVE_FIXED:
          begin
            nc := FTreeview.Items.AddChild(nil, Chr(i + 65) + ':');
            nc.ImageIndex := 1;
            nc.SelectedIndex := 1;
            FTreeview.Items.AddChild(nc, 'y');
          end;
        DRIVE_CDROM:
          begin
            nc := FTreeview.Items.AddChild(nil, Chr(i + 65) + ':');
            nc.ImageIndex := 2;
            nc.SelectedIndex := 2;
            FTreeview.Items.AddChild(nc, 'y');
          end;
        DRIVE_RAMDISK:
          begin
            nc := FTreeview.Items.AddChild(nil, Chr(i + 65) + ':');
            nc.ImageIndex := 1;
            nc.SelectedIndex := 1;
            FTreeview.Items.AddChild(nc, 'y');
          end;
        DRIVE_REMOTE:
          begin
            nc := FTreeview.Items.AddChild(nil, Chr(i + 65) + ':');
            nc.ImageIndex := 3;
            nc.SelectedIndex := 3;
            FTreeview.Items.AddChild(nc, 'y');
          end;
      end;
  end;
end;

function TJvTreeViewCombo.FindChildNode(const AName: string; CurrentNode: TTreeNode): TTreeNode;
begin
end;

function TJvTreeViewCombo.GetDirNode(const Path: string): TTreeNode;
var
  List: TStringlist;
  i: integer;
  dummy: boolean;
begin
  List := TStringlist.Create;
  FTreeView.Items.BeginUpdate;
  try
    StrTokenToStrings(Path, '\', List);
    Result := nil;
    for i := 0 to List.Count - 1 do
    begin
      if Result <> nil then
        OnTVExpanding(self, Result, dummy);
      Result := FindChildNode(List[i], Result);
    end;
  finally
    List.Free;
    FTreeView.Items.EndUpdate;
  end;
end;

{ TJvTreeViewPopupWindow }

constructor TJvTreeViewPopupWindow.Create(AOwner: TComponent);
begin
  inherited;
  FTreeImageList := TCustomImageList.Create(nil);
  FTreeView := TJvTreeView.Create(nil);
  FTreeView.Parent := self;

  FTreeView.Align := alClient;
  FTreeView.BorderStyle := bsNone;

  FTreeView.OnExpanding := OnTVExpanding;
  FTreeView.OnKeyUp := OnTVKeyUp;
  FTreeView.OnCollapsed := OnTVCollapsed;
  FTreeView.OnChange := OnTVChange;
  FTreeView.OnSelectionChange := OnTVSelectionChange;

  if not (csDesigning in ComponentState) then
    PopulateTree;
end;

destructor TJvTreeViewPopupWindow.Destroy;
begin
  FTreeView.Free;
  FTreeImageList.Destroy;
  inherited;
end;

function TJvTreeViewPopupWindow.FindChildNode(const AName: string;
  CurrentNode: TTreeNode): TTreeNode;
begin
  if CurrentNode = nil then
    Result := FTreeView.Items.GetFirstNode
  else
    Result := CurrentNode.getFirstChild;
  while Assigned(Result) and not AnsiSameText(Result.Text, AName) do
    Result := Result.getNextSibling;
end;

function TJvTreeViewPopupWindow.GetValue: Variant;
begin
  Result := '';
end;

procedure TJvTreeViewPopupWindow.OnTVChange(Sender: TObject;
  Node: TTreeNode);
begin
  Text := GetDirStr(Node);
end;

procedure TJvTreeViewPopupWindow.OnTVCollapsed(Sender: TObject;
  Node: TTreeNode);
begin
  if Node.Parent = nil then
    Exit;
  Node.SelectedIndex := 0;
  Node.ImageIndex := 0;
end;

procedure TJvTreeViewPopupWindow.OnTVExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
begin
  FTreeView.Items.BeginUpdate;
  Node.DeleteChildren;
  FillTree(Node);
  FTreeView.Items.EndUpdate;
  if Node.Parent = nil then
    Exit;
  Node.SelectedIndex := 5;
  Node.ImageIndex := 5;
end;

procedure TJvTreeViewPopupWindow.OnTVKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) or (Key = VK_ESCAPE) then
  begin
    HidePanel;
    Key := 0;
  end;
end;

procedure TJvTreeViewPopupWindow.OnTVSelectionChange(Sender: TObject);
begin
  Text := FTreeView.Selected.Text;
end;

procedure TJvTreeViewPopupWindow.SetTreeImageList(
  const Value: TCustomImageList);
begin
  FTreeImageList.Assign(Value);
end;

procedure TJvTreeViewPopupWindow.SetValue(const Value: Variant);
begin

end;


procedure TJvTreeViewPopupWindow.TVClickButton(Sender: TObject;
  Button: TJvCapBtnStyle);
begin

end;

end.

