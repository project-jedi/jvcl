{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRegistryViewer.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvRegistryViewer;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ComCtrls, ExtCtrls, Registry, Menus,
  JvTypes, JVCLVer;

type
  TJvRegistryViewer = class(TWinControl)
  private
    FTreeView1: TTreeView;
    FSplitter1: TSplitter;
    FListView1: TListView;
    FRegistry: TRegistry;
    FChangeKey: TOnChangeKey;
    FAboutJVCL: TJVCLAboutInfo;

    function GetTreeView1Width: Integer;
    procedure SetTreeView1Width(Value: Integer);
    function GetTreeView1Cursor: TCursor;
    procedure SetTreeView1Cursor(Value: TCursor);
    function GetTreeView1Hint: string;
    procedure SetTreeView1Hint(Value: string);
    function GetListView1Width: Integer;
    procedure SetListView1Width(Value: Integer);
    function GetListView1Hint: string;
    procedure SetListView1Hint(Value: string);
    procedure AddChild(ParentNode: TTreeNode; Path: string; Root: HKEY);
    procedure Expand(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure Collapse(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
    procedure Changing(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    function GetPopupList: TPopupMenu;
    function GetPopupTree: TPopupMenu;
    procedure SetPopupList(const Value: TPopupMenu);
    procedure SetPopupTree(const Value: TPopupMenu);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property TreeViewWidth: Integer read GetTreeView1Width write SetTreeView1Width;
    property TreeViewCursor: TCursor read GetTreeView1Cursor write SetTreeView1Cursor;
    property TreeViewHint: string read GetTreeView1Hint write SetTreeView1Hint;
    property ListViewWidth: Integer read GetListView1Width write SetListView1Width;
    property ListViewHint: string read GetListView1Hint write SetListView1Hint;
    property BorderWidth;
    property Align;
    property OnChangeKey: TOnChangeKey read FChangeKey write FChangeKey;
    property PopupTreeView: TPopupMenu read GetPopupTree write SetPopupTree;
    property PopupListView: TPopupMenu read GetPopupList write SetPopupList;
  end;

implementation

resourcestring
  RC_HKey1 = 'HKEY_CLASSES_ROOT';
  RC_HKey2 = 'HKEY_CURRENT_USER';
  RC_HKey3 = 'HKEY_LOCAL_MACHINE';
  RC_HKey4 = 'HKEY_USERS';
  RC_HKey5 = 'HKEY_PERFORMANCE_DATA';
  RC_HKey6 = 'HKEY_CURRENT_CONFIG';
  RC_HKey7 = 'HKEY_DYN_DATA';

  RC_DefaultCaption = '<Default>';
  RC_Unknown = 'Unknown';
  RC_String = 'String';
  RC_ExpandString = 'ExpandString';
  RC_Integer = 'Integer';
  RC_Binary = 'Binary';

  {***************************************************}

constructor TJvRegistryViewer.Create(AOwner: TComponent);
var
  t: TListColumn;
begin
  inherited Create(AOwner);

  Width := 576;
  Height := 358;
  FTreeView1 := TTreeView.Create(Self);
  FTreeView1.Parent := Self;
  FSplitter1 := TSplitter.Create(Self);
  FSplitter1.Parent := Self;
  FListView1 := TListView.Create(Self);
  FListView1.Parent := Self;

  FTreeView1.Left := 0;
  FTreeView1.Top := 0;
  FTreeView1.Width := 201;
  FTreeView1.Height := 331;
  FTreeView1.Align := alLeft;

  FSplitter1.Left := 201;
  FSplitter1.Top := 0;
  FSplitter1.Width := 4;
  FSplitter1.Height := 331;
  FSplitter1.Align := alLeft;

  FListView1.Left := 205;
  FListView1.Top := 0;
  FListView1.Width := 363;
  FListView1.Height := 331;
  FListView1.Align := AlClient;
  FListView1.ViewStyle := vsReport;
  t := FListView1.Columns.Add;
  t.Caption := 'Name';
  t.Width := 200;
  t := FListView1.Columns.Add;
  t.Caption := 'Type';
  t.Width := 50;
  t := FListView1.Columns.Add;
  t.Caption := 'Data';
  t.Width := 100;

  FRegistry := TRegistry.Create;
  FTreeView1.OnExpanding := Expand;
  FTreeView1.OnCollapsing := Collapse;
  FTreeView1.ReadOnly := True;
  FTreeView1.OnChanging := Changing;
  FListView1.ReadOnly := True;
  FListView1.HotTrack := True;
  FListView1.FlatScrollBars := True;
  FListView1.RowSelect := True;
  FListView1.FullDrag := False;
  FListView1.ColumnClick := False;
end;

{***************************************************}

destructor TJvRegistryViewer.Destroy;
begin
  FTreeView1.Free;
  FSplitter1.Free;
  FListView1.Free;
  FRegistry.Free;
  inherited Destroy;
end;

{***************************************************}

function TJvRegistryViewer.GetTreeView1Width: Integer;
begin
  Result := FTreeView1.Width;
end;

{***************************************************}

procedure TJvRegistryViewer.SetTreeView1Width(Value: Integer);
begin
  FTreeView1.Width := Value;
end;

{***************************************************}

function TJvRegistryViewer.GetTreeView1Cursor: TCursor;
begin
  Result := FTreeView1.Cursor;
end;

{***************************************************}

procedure TJvRegistryViewer.SetTreeView1Cursor(Value: TCursor);
begin
  FTreeView1.Cursor := Value;
end;

{***************************************************}

function TJvRegistryViewer.GetTreeView1Hint: string;
begin
  Result := FTreeView1.Hint;
end;

{***************************************************}

procedure TJvRegistryViewer.SetTreeView1Hint(Value: string);
begin
  FTreeView1.Hint := Value;
end;

{***************************************************}

function TJvRegistryViewer.GetListView1Width: Integer;
begin
  Result := FListView1.Width;
end;

{***************************************************}

procedure TJvRegistryViewer.SetListView1Width(Value: Integer);
begin
  FListView1.Width := Value;
end;

{***************************************************}

function TJvRegistryViewer.GetListView1Hint: string;
begin
  Result := FListView1.Hint;
end;

{***************************************************}

procedure TJvRegistryViewer.SetListView1Hint(Value: string);
begin
  FListView1.Hint := Value;
end;

{***************************************************}

procedure TJvRegistryViewer.AddChild(ParentNode: TTreeNode; Path: string; Root: HKEY);
var
  tl: TTreeNode;
  ts: TStringList;
  i: Integer;
begin
  FRegistry.RootKey := Root;
  FRegistry.OpenKey(Path, False);
  ts := TStringList.Create;
  FRegistry.GetKeyNames(ts);
  for i := 0 to ts.count - 1 do
  begin
    tl := FTreeView1.Items.AddChild(ParentNode, ts[i]);
    AddChild(tl, Path + ts[i] + '\', Root);
  end;
  ts.Free;
end;

{***************************************************}

procedure TJvRegistryViewer.Loaded;
var
  tl: TTreeNode;
begin
  FRegistry.RootKey := HKEY_CLASSES_ROOT;
  FRegistry.OpenKey('\', False);
  tl := FTreeView1.Items.AddChildFirst(nil, RC_HKey1);
  if FRegistry.HasSubKeys then
    tl.HasChildren := True;

  FRegistry.RootKey := HKEY_CURRENT_USER;
  FRegistry.OpenKey('\', False);
  tl := FTreeView1.Items.AddChildFirst(nil, RC_HKey2);
  if FRegistry.HasSubKeys then
    tl.HasChildren := True;

  FRegistry.RootKey := HKEY_LOCAL_MACHINE;
  FRegistry.OpenKey('\', False);
  tl := FTreeView1.Items.AddChildFirst(nil, RC_HKey3);
  if FRegistry.HasSubKeys then
    tl.HasChildren := True;

  FRegistry.RootKey := HKEY_USERS;
  FRegistry.OpenKey('\', False);
  tl := FTreeView1.Items.AddChildFirst(nil, RC_HKey4);
  if FRegistry.HasSubKeys then
    tl.HasChildren := True;

  FRegistry.RootKey := HKEY_PERFORMANCE_DATA;
  FRegistry.OpenKey('\', False);
  tl := FTreeView1.Items.AddChildFirst(nil, RC_HKey5);
  if FRegistry.HasSubKeys then
    tl.HasChildren := True;

  FRegistry.RootKey := HKEY_CURRENT_CONFIG;
  FRegistry.OpenKey('\', False);
  tl := FTreeView1.Items.AddChildFirst(nil, RC_HKey6);
  if FRegistry.HasSubKeys then
    tl.HasChildren := True;

  FRegistry.RootKey := HKEY_DYN_DATA;
  FRegistry.OpenKey('\', False);
  tl := FTreeView1.Items.AddChildFirst(nil, RC_HKey7);
  if FRegistry.HasSubKeys then
    tl.HasChildren := True;
end;

{***************************************************}

procedure TJvRegistryViewer.Expand(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  Path: string;
  rootk: HKEY;
  t, r: TTreeNode;
  ts: TStringList;
  i: Integer;
begin
  Path := '\';
  t := Node;
  r := t;
  while t <> nil do
  begin
    if t.Parent <> nil then
      Path := '\' + t.Text + Path;
    r := t;
    t := t.Parent;
  end;

  if r.Text = RC_HKey2 then
    rootk := HKEY_CURRENT_USER
  else if r.Text = RC_HKey1 then
    rootk := HKEY_CLASSES_ROOT
  else if r.Text = RC_HKey3 then
    rootk := HKEY_LOCAL_MACHINE
  else if r.Text = RC_HKey4 then
    rootk := HKEY_USERS
  else if r.Text = RC_HKey5 then
    rootk := HKEY_PERFORMANCE_DATA
  else if r.Text = RC_HKey7 then
    rootk := HKEY_DYN_DATA
  else
    rootk := HKEY_CURRENT_CONFIG;

  FRegistry.RootKey := rootk;

  FRegistry.OpenKey(Path, False);
  ts := TStringList.Create;
  FRegistry.GetKeyNames(ts);
  FRegistry.CloseKey;

  ts.Sort;

  for i := 0 to ts.Count - 1 do
  begin
    t := FTreeView1.Items.AddChild(Node, ts[i]);
    t.Text := ts[i];
    FRegistry.OpenKey(Path + ts[i] + '\', False);
    t.HasChildren := FRegistry.HasSubKeys;
    FRegistry.CloseKey;
  end;

  AllowExpansion := True;

  ts.Free;
end;

{***************************************************}

procedure TJvRegistryViewer.Collapse(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  AllowCollapse := True;
  Node.DeleteChildren;
  Node.HasChildren := True;
end;

{***************************************************}

procedure TJvRegistryViewer.Changing(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
var
  Path, st: string;
  rootk: HKEY;
  t, r: TTreeNode;
  tl: TListItem;
  ts: TStringList;
  i, j, k: Integer;
  p: array[0..255] of Byte;
begin
  FListView1.Items.Clear;

  Path := '\';
  t := Node;
  r := t;
  while t <> nil do
  begin
    if t.Parent <> nil then
      Path := '\' + t.Text + Path;
    r := t;
    t := t.Parent;
  end;
  if r.Text = RC_HKey2 then
    rootk := HKEY_CURRENT_USER
  else if r.Text = RC_HKey1 then
    rootk := HKEY_CLASSES_ROOT
  else if r.Text = RC_HKey3 then
    rootk := HKEY_LOCAL_MACHINE
  else if r.Text = RC_HKey4 then
    rootk := HKEY_USERS
  else if r.Text = RC_HKey5 then
    rootk := HKEY_PERFORMANCE_DATA
  else if r.Text = RC_HKey7 then
    rootk := HKEY_DYN_DATA
  else
    rootk := HKEY_CURRENT_CONFIG;

  FRegistry.RootKey := rootk;

  FRegistry.OpenKey(Path, False);
  ts := TStringList.Create;
  FRegistry.GetValueNames(ts);

  ts.Sort;

  for i := 0 to ts.Count - 1 do
  begin
    tl := FListView1.Items.Add;
    if ts[i] = '' then
      tl.Caption := RC_DefaultCaption
    else
      tl.Caption := ts[i];
    case FRegistry.GetDataType(ts[i]) of
      rdUnknown:
        tl.SubItems.Add(RC_Unknown);
      rdstring:
        begin
          tl.SubItems.Add(RC_string);
          tl.SubItems.Add(FRegistry.ReadString(ts[i]));
        end;
      rdExpandstring:
        begin
          tl.SubItems.Add(RC_Expandstring);
          tl.SubItems.Add(FRegistry.ReadString(ts[i]));
        end;
      rdInteger:
        begin
          tl.SubItems.Add(RC_Integer);
          tl.SubItems.Add(IntToStr(FRegistry.ReadInteger(ts[i])));
        end;
      rdBinary:
        begin
          tl.SubItems.Add(RC_Binary);
          j := FRegistry.ReadBinaryData(ts[i], p, 256);
          st := '';
          if j <> -1 then
            for k := 0 to j - 1 do
              st := st + IntToHex(p[k], 2) + ' ';
          tl.SubItems.Add(st);
        end;
    else
      tl.SubItems.Add(RC_Unknown);
    end;
  end;

  FRegistry.CloseKey;
  AllowChange := True;

  if Assigned(FChangeKey) then
    FChangeKey(Self, rootk, Path);

  ts.Free;
end;

{***************************************************}

function TJvRegistryViewer.GetPopupList: TPopupMenu;
begin
  Result := FListView1.PopupMenu;
end;

{***************************************************}

function TJvRegistryViewer.GetPopupTree: TPopupMenu;
begin
  Result := FTreeView1.PopupMenu;
end;

{***************************************************}

procedure TJvRegistryViewer.SetPopupList(const Value: TPopupMenu);
begin
  FListView1.PopupMenu := Value;
end;

{***************************************************}

procedure TJvRegistryViewer.SetPopupTree(const Value: TPopupMenu);
begin
  FTreeView1.PopupMenu := Value;
end;

end.
