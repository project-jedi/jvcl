{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRegistryTreeView.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  A treeview that displays the keys from the registry

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvRegistryTreeView;

{$I jvcl.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms,
  ComCtrls, Registry, ImgList,
  JvComponent, JvTypes;

type

  TJvRegistryTreeView = class(TJvCustomTreeView)
  private
    FRegistryKeys: TJvRegKeys;
    FInternalImages: TImageList;
    FListView: TListView;
    FRootCaption: string;
    FDefaultCaption: string;
    FDefaultNoValue: string;
    FReg: TRegistry;
    procedure SetDefaultCaption(Value: string);
    procedure SetDefaultNoValue(Value: string);
    procedure SetRootCaption(Value: string);
    procedure SetRegistryKeys(Value: TJvRegKeys);
    procedure BuildTree;
    function FillListView(Node: TTreeNode): Boolean;
    procedure SetDefaultImages;
    function GetCurrentPath: string;
    function GetShortPath: string;
    function GetCurrentKey: HKEY;
    function GetShowHint: Boolean;
    procedure SetShowHint(Value: Boolean);
    procedure OpenRegistry(Node: TTreeNode);
    procedure CloseRegistry;
    function FindChildNode(ParentNode: TTreeNode;
      const Name: string): TTreeNode;
  protected
    procedure RefreshSubTrees(ANode: TTreeNode; Key, OldKey: string; Level: Integer); virtual;
    function CanCollapse(Node: TTreeNode): Boolean; override;
    function CanExpand(Node: TTreeNode): Boolean; override;
    procedure Change(Node: TTreeNode); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function SaveKey(const Filename: string): Boolean;
    function LoadKey(const Filename: string): Boolean;
    procedure RefreshNode(Node: TTreeNode);
    function AddKey(ParentNode: TTreeNode; const KeyName: string): TTreeNode;
    function AddStringValue(ParentNode: TTreeNode; const Name, Value: string): TTreeNode;
    function AddBinaryValue(ParentNode: TTreeNode; const Name: string; var Buf; BufSize: Integer): TTreeNode;
    function AddDWORDValue(ParentNode: TTreeNode; const Name: string; Value: DWORD): TTreeNode;
    property CurrentPath: string read GetCurrentPath;
    property ShortPath: string read GetShortPath;
    property CurrentKey: HKEY read GetCurrentKey;
    property Items stored False;
  published
    property Align;
    property Color;
    property BorderStyle;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property ShowButtons;
    property ShowHint: Boolean read GetShowHint write SetShowHint;
    property ShowLines;
    property ShowRoot;
    property ReadOnly default True;
    property RightClickSelect;
    property Indent;
    property HideSelection;
    property RegistryKeys: TJvRegKeys read FRegistryKeys write SetRegistryKeys default
      [hkCurrentUser, hkLocalMachine];
    property ListView: TListView read FListView write FListView;
    property RootCaption: string read FRootCaption write SetRootCaption;
    property DefaultCaption: string read FDefaultCaption write SetDefaultCaption;
    property DefaultNoValueCaption: string read FDefaultNoValue write SetDefaultNoValue;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;
    property OnDragDrop;
    property OnStartDock;
    property OnEndDock;
    property OnDockDrop;
    property OnEditing;
    property OnEdited;
    property OnExpanding;
    property OnExpanded;
    property OnCollapsing;
    property OnCollapsed;
    property OnChanging;
    property OnChange;
    property OnCompare;
    //PRY 2002.06.04
    {$IFDEF COMPILER6_UP}
    property OnAddition;
    {$ENDIF COMPILER6_UP}
    // PRY END
    property OnDeletion;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
  end;

implementation

uses
  JvResources;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvRegistryTreeView.res}
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
{$R ../Resources/JvRegistryTreeView.res}
{$ENDIF UNIX}

const
  imMyPC = 0;
  imClosed = 1;
  imOpen = 2;
  imText = 3;
  imBin = 4;

type
  TRegistryAccessProtected = class(TRegistry);

function SetRootKey(Reg: TRegistry; Node: TTreeNode): Boolean;
var
  TmpNode: TTreeNode;
begin
  Result := False;
  if Node <> nil then
  begin
    TmpNode := Node;
    while TmpNode <> nil do
    begin
      if Longint(TmpNode.Data) < 0 then
      begin
        Reg.RootKey := Longint(TmpNode.Data);
        Result := True;
        Exit;
      end;
      TmpNode := TmpNode.Parent;
    end;
  end;
end;

function FixupPath(Key: string): string;
begin
  if Key = '' then
    Result := '\'
  else
  if AnsiLastChar(Key) <> '\' then
    Result := Key + '\'
  else
    Result := Key;
  if Length(Result) > 1 then
    if (Result[1] = '\') and (Result[2] = '\') then
      Result := Copy(Result, 2, Length(Result));
end;

function GetFullPath(ANode: TTreeNode): string;
var
  TmpNode: TTreeNode;
begin
  Result := '';
  if ANode = nil then
    Exit;
  TmpNode := ANode;
  while TmpNode <> nil do
  begin
    Result := TmpNode.Text + '\' + Result;
    TmpNode := TmpNode.Parent;
  end;
  if (Result <> '') and (AnsiLastChar(Result) = '\') then
    SetLength(Result, Length(Result) - 1);
end;

function GetKeyPath(ANode: TTreeNode): string;
var
  TmpNode: TTreeNode;
begin
  Result := '';
  if ANode = nil then
    Exit;
  TmpNode := ANode;
  while (TmpNode.Parent <> nil) and (TmpNode.Parent.Parent <> nil) do
  begin
    Result := TmpNode.Text + '\' + Result;
    TmpNode := TmpNode.Parent;
  end;
  if (Length(Result) > 0) and (Result[1] <> '\') then
    Result := '\' + Result;
end;

{
function GetPreviousKey(Key: string): string;
var
  I: Integer;
begin
  Result := Key;
  if (Result = '') or (Result = '\') then Exit;
  for I := Length(Result) - 1 downto 1 do
    if Result[I] = '\' then
    begin
      Result := Copy(Result,1,I - 1);
      Exit;
    end;
end;

function StripChars(Str: string; Ch: Char): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Str) do
  begin
    if Str[I] = Ch then Continue;
    AppendStr(Result,str[I]);
  end;
end;
}

function BufToStr(Buffer: array of Byte; BufSize: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to BufSize - 1 do
    Result := Result + ' ' + IntToHex(Buffer[I], 2);
end;

function TJvRegistryTreeView.GetCurrentPath: string;
begin
  Result := GetFullPath(Selected);
end;

function TJvRegistryTreeView.GetShortPath: string;
begin
  Result := GetKeyPath(Selected);
end;

function TJvRegistryTreeView.GetCurrentKey: HKEY;
begin
  OpenRegistry(Selected);
  Result := TRegistryAccessProtected(FReg).GetKey(ShortPath);
  CloseRegistry;
end;

function TJvRegistryTreeView.GetShowHint: Boolean;
begin
  Result := inherited ShowHint;
end;

procedure TJvRegistryTreeView.SetShowHint(Value: Boolean);
begin
  if inherited ShowHint <> Value then
  begin
    inherited ShowHint := Value;
    Items.Clear; // AV's in ComCtrl32.dll without this
    RecreateWnd;
  end;
end;

procedure TJvRegistryTreeView.SetDefaultImages;
begin
  if not Assigned(FInternalImages) then
    FInternalImages := TImageList.CreateSize(16, 16);
  if FInternalImages.Count = 0 then
  begin
    FInternalImages.GetInstRes(hInstance, rtBitmap, 'REGTV_MYCOMPUTER', 16, [], clFuchsia);
    FInternalImages.GetInstRes(hInstance, rtBitmap, 'REGTV_CLOSEDFOLDER', 16, [], clFuchsia);
    FInternalImages.GetInstRes(hInstance, rtBitmap, 'REGTV_OPENFOLDER', 16, [], clFuchsia);
    FInternalImages.GetInstRes(hInstance, rtBitmap, 'REGTV_TEXTIMAGE', 16, [], clFuchsia);
    FInternalImages.GetInstRes(hInstance, rtBitmap, 'REGTV_BINIMAGE', 16, [], clFuchsia);
  end;
  Images := FInternalImages;
end;

procedure TJvRegistryTreeView.RefreshSubTrees(ANode: TTreeNode; Key, OldKey: string; Level: Integer);
var
  AStrings: TStringList;
  I: Integer;
  NewNode: TTreeNode;
  AKey: string;
begin
  AKey := FixupPath(OldKey);
  if FReg.OpenKeyReadOnly(Key) and FReg.HasSubKeys then
  begin
    ANode.HasChildren := True;
    Dec(Level);
    if Level = 1 then
    begin
      AStrings := TStringList.Create;
      try
        FReg.GetKeyNames(AStrings);
        for I := 0 to AStrings.Count - 1 do
        begin
          NewNode := Items.AddChild(ANode, AStrings[I]);
          NewNode.ImageIndex := imClosed;
          NewNode.SelectedIndex := imOpen;
          RefreshSubTrees(NewNode, AStrings[I], AKey + Key, Level);
        end;
      finally
        AStrings.Free;
      end;
    end;
  end;
  FReg.OpenKeyReadOnly(AKey);
end;

function RegDataTypeToString(Value: TRegDataType): string;
begin
  case Value of
    rdString:
      Result := 'REG_SZ';
    rdExpandString:
      Result := 'REG_EXPAND_SZ';
    rdInteger:
      Result := 'REG_DWORD';
    rdBinary:
      Result := 'REG_BINARY';
  else
    Result := 'REG_NONE';
  end;
end;

function TJvRegistryTreeView.FillListView(Node: TTreeNode): Boolean;
var
  Strings: TStringList;
  I: Integer;
  TmpItem: TListItem;
  Buffer: array [0..4095] of Byte;
  S: string;
  DefaultSet: Boolean;
begin
  Result := False;
  if not Assigned(FListView) then
    Exit;
  OpenRegistry(Node);
  FListView.Items.BeginUpdate;
  try
    FListView.Items.Clear;
    if FListView.SmallImages = nil then
      FListView.SmallImages := Images;
    if (Node = nil) or (Node = Items.GetFirstNode) then
      Exit;
    { set current root }
    DefaultSet := False;
    if FReg.OpenKeyReadOnly(GetKeyPath(Node)) then
    begin
      Strings := TStringList.Create;
      FReg.GetValueNames(Strings);

      for I := 0 to Strings.Count - 1 do
      begin
        { set default item }
        if (Strings[I] = '') and not DefaultSet then
        begin
          TmpItem := FListView.Items.Insert(0);
          TmpItem.Caption := FDefaultCaption;
          DefaultSet := True;
        end
        else
        begin
          TmpItem := FListView.Items.Add;
          TmpItem.Caption := Strings[I];
        end;

        case FReg.GetDataType(Strings[I]) of
          rdUnknown:
            begin
              TmpItem.ImageIndex := imText;
              TmpItem.SubItems.Add(RsUnknownCaption);
            end;
          rdString, rdExpandString:
            begin
              S := FReg.ReadString(Strings[I]);
              if (S = '') and AnsiSameText(TmpItem.Caption, FDefaultCaption) then
                S := FDefaultNoValue
              else
                S := Format('"%s"', [S]);
              TmpItem.ImageIndex := imText;
              TmpItem.SubItems.Add(S);
            end;
          rdInteger:
            begin
              TmpItem.ImageIndex := imBin;
              TmpItem.SubItems.Add(Format('0x%.8x (%d)', [FReg.ReadInteger(Strings[I]),
                FReg.ReadInteger(Strings[I])]));
            end;
          rdBinary:
            begin
              TmpItem.ImageIndex := imBin;
              FReg.ReadBinaryData(Strings[I], Buffer, SizeOf(Buffer));
              TmpItem.SubItems.Add(BufToStr(Buffer, FReg.GetDataSize(Strings[I])));
            end;
        end;
        TmpItem.SubItems.Add(RegDataTypeToString(FReg.GetDataType(Strings[I])));
      end;
      Result := True;
    end;
    { set default item }
    if (Node.Parent <> nil) and not DefaultSet then
    begin
      TmpItem := FListView.Items.Insert(0);
      TmpItem.ImageIndex := imText;
      TmpItem.Caption := FDefaultCaption;
      TmpItem.SubItems.Add(FDefaultNoValue);
      TmpItem.SubItems.Add('REG_SZ');
    end;
  finally
    FListView.Items.EndUpdate;
    CloseRegistry;
  end;
end;

procedure TJvRegistryTreeView.SetDefaultCaption(Value: string);
begin
  FDefaultCaption := Value;
  FillListView(Selected);
end;

procedure TJvRegistryTreeView.SetDefaultNoValue(Value: string);
begin
  FDefaultNoValue := Value;
  FillListView(Selected);
end;

procedure TJvRegistryTreeView.SetRootCaption(Value: string);
begin
  FRootCaption := Value;
  BuildTree;
end;

procedure TJvRegistryTreeView.SetRegistryKeys(Value: TJvRegKeys);
begin
  if FRegistryKeys <> Value then
    FRegistryKeys := Value;
  BuildTree;
end;

procedure TJvRegistryTreeView.BuildTree;
var
  NewNode, ANode: TTreeNode;
begin
  OpenRegistry(nil);
  Items.BeginUpdate;
  try
    Items.Clear;
    ANode := Items.Add(nil, FRootCaption);
    ANode.ImageIndex := imMyPC;
    ANode.SelectedIndex := imMyPC;
    if hkClassesRoot in FRegistryKeys then
    begin
      FReg.RootKey := HKEY_CLASSES_ROOT;
      NewNode := Items.AddChild(ANode, 'HKEY_CLASSES_ROOT');
      NewNode.ImageIndex := imClosed;
      NewNode.SelectedIndex := imOpen;
      NewNode.Data := Pointer(FReg.RootKey);
      if not (csDesigning in ComponentState) then
        RefreshSubTrees(NewNode, '\', '', 1);
    end;

    if hkCurrentUser in FRegistryKeys then
    begin
      FReg.RootKey := HKEY_CURRENT_USER;
      NewNode := Items.AddChild(ANode, 'HKEY_CURRENT_USER');
      NewNode.ImageIndex := imClosed;
      NewNode.SelectedIndex := imOpen;
      NewNode.Data := Pointer(FReg.RootKey);
      if not (csDesigning in ComponentState) then
        RefreshSubTrees(NewNode, '\', '', 1);
    end;

    if hkLocalMachine in FRegistryKeys then
    begin
      FReg.RootKey := HKEY_LOCAL_MACHINE;
      NewNode := Items.AddChild(ANode, 'HKEY_LOCAL_MACHINE');
      NewNode.ImageIndex := imClosed;
      NewNode.SelectedIndex := imOpen;
      NewNode.Data := Pointer(FReg.RootKey);
      if not (csDesigning in ComponentState) then
        RefreshSubTrees(NewNode, '\', '', 1);
    end;

    if hkUsers in FRegistryKeys then
    begin
      FReg.RootKey := HKEY_USERS;
      NewNode := Items.AddChild(ANode, 'HKEY_USERS');
      NewNode.ImageIndex := imClosed;
      NewNode.SelectedIndex := imOpen;
      NewNode.Data := Pointer(FReg.RootKey);
      if not (csDesigning in ComponentState) then
        RefreshSubTrees(NewNode, '\', '', 1);
    end;

    if hkPerformanceData in FRegistryKeys then
    begin
      FReg.RootKey := HKEY_PERFORMANCE_DATA;
      NewNode := Items.AddChild(ANode, 'HKEY_PERFORMANCE_DATA');
      NewNode.ImageIndex := imClosed;
      NewNode.SelectedIndex := imOpen;
      NewNode.Data := Pointer(FReg.RootKey);
      if not (csDesigning in ComponentState) then
        RefreshSubTrees(NewNode, '\', '', 1);
    end;

    if hkCurrentConfig in FRegistryKeys then
    begin
      FReg.RootKey := HKEY_CURRENT_CONFIG;
      NewNode := Items.AddChild(ANode, 'HKEY_CURRENT_CONFIG');
      NewNode.ImageIndex := imClosed;
      NewNode.SelectedIndex := imOpen;
      NewNode.Data := Pointer(FReg.RootKey);
      if not (csDesigning in ComponentState) then
        RefreshSubTrees(NewNode, '\', '', 1);
    end;
    if hkDynData in FRegistryKeys then
    begin
      FReg.RootKey := HKEY_DYN_DATA;
      NewNode := Items.AddChild(ANode, 'HKEY_DYN_DATA');
      NewNode.ImageIndex := imClosed;
      NewNode.SelectedIndex := imOpen;
      NewNode.Data := Pointer(FReg.RootKey);
      if not (csDesigning in ComponentState) then
        RefreshSubTrees(NewNode, '\', '', 1);
    end;
    ANode.Expand(False);
    ANode.Selected := True;
  finally
    CloseRegistry;
    Items.EndUpdate;
  end;
end;

function TJvRegistryTreeView.CanCollapse(Node: TTreeNode): Boolean;
begin
  Result := inherited CanCollapse(Node);
  {  if Result then
      Node.ImageIndex := imClosed;}
end;

function TJvRegistryTreeView.CanExpand(Node: TTreeNode): Boolean;
begin
  Result := inherited CanExpand(Node);
  if not Result or (Node.Parent = nil) then
    Exit;
  OpenRegistry(Node);
  try
    //  Node.ImageIndex := imOpen;
    //  Node.DeleteChildren;
    SetRootKey(FReg, Node);
    if not (csDesigning in ComponentState) and (Node.Count = 0) then
      RefreshSubTrees(Node, FixupPath(GetKeyPath(Node)), '', 2);
  finally
    CloseRegistry;
  end;
end;

procedure TJvRegistryTreeView.Change(Node: TTreeNode);
begin
  FillListView(Node);
  inherited Change(Node);
end;

procedure TJvRegistryTreeView.CreateParams(var Params: TCreateParams);
const
  TVS_NOTOOLTIPS = $0080;
begin
  inherited CreateParams(Params);
  if not ShowHint then
    Params.Style := Params.Style or TVS_NOTOOLTIPS;
end;

procedure TJvRegistryTreeView.CreateWnd;
begin
  inherited CreateWnd;
  SetDefaultImages;
  BuildTree;
end;

procedure TJvRegistryTreeView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FListView) and (Operation = opRemove) then
  begin
    if FListView.SmallImages = FInternalImages then
      FListView.SmallImages := nil;
    FListView := nil;
  end;
  if (AComponent = Images) and (Operation = opRemove) then
    SetDefaultImages;
end;

constructor TJvRegistryTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRegistryKeys := [hkCurrentUser, hkLocalMachine];
  FRootCaption := RsMyComputer;
  FDefaultCaption := RsDefaultCaption;
  FDefaultNoValue := RsDefaultNoValue;
  SetDefaultImages;
end;

destructor TJvRegistryTreeView.Destroy;
begin
  if Assigned(FListView) and (FListView.SmallImages = FInternalImages) then
    FListView.SmallImages := nil;
  if Assigned(FInternalImages) then
    FInternalImages.Free;
  inherited Destroy;
end;

procedure TJvRegistryTreeView.RefreshNode(Node: TTreeNode);
var
  b: Boolean;
begin
  Items.BeginUpdate;
  try
    b := False;
    if Node <> nil then
      b := Node.Expanded;
    OpenRegistry(Node);
    try
      if Node <> nil then
        Node.DeleteChildren;
      if (Node = nil) or (Node = Items.GetFirstNode) then
        BuildTree
      else
      begin
        SetRootKey(FReg, Node);
        RefreshSubTrees(Node, FixupPath(GetKeyPath(Node)), '', 2);
      end;
    finally
      if Node <> nil then
        Node.Expanded := b;
      CloseRegistry;
    end;
  finally
    Items.EndUpdate;
  end;
end;

function TJvRegistryTreeView.FindChildNode(ParentNode: TTreeNode; const Name: string): TTreeNode;
var
  N: TTreeNode;
begin
  Result := nil;
  if ParentNode = nil then
    Exit;
  N := ParentNode.getFirstChild;
  while Assigned(N) do
  begin
    if AnsiSameText(N.Text, Name) then
    begin
      Result := N;
      Exit;
    end;
    N := N.getNextSibling;
  end;
end;

function TJvRegistryTreeView.AddBinaryValue(ParentNode: TTreeNode;
  const Name: string; var Buf; BufSize: Integer): TTreeNode;
begin
  Result := nil;
  if ParentNode = nil then
    Exit;
  OpenRegistry(ParentNode);
  FReg.WriteBinaryData(FixupPath(GetKeyPath(ParentNode)) + Name,
    Buf, BufSize);
  CloseRegistry;
  RefreshNode(ParentNode);
  Result := FindChildNode(ParentNode, Name);
end;

function TJvRegistryTreeView.AddDWORDValue(ParentNode: TTreeNode;
  const Name: string; Value: DWORD): TTreeNode;
begin
  Result := nil;
  if ParentNode = nil then
    Exit;
  OpenRegistry(ParentNode);
  FReg.WriteInteger(FixupPath(GetKeyPath(ParentNode)) + Name, Value);
  CloseRegistry;
  RefreshNode(ParentNode);
  Result := FindChildNode(ParentNode, Name);
end;

function TJvRegistryTreeView.AddKey(ParentNode: TTreeNode;
  const KeyName: string): TTreeNode;
begin
  Result := nil;
  if ParentNode = nil then
    Exit;
  OpenRegistry(ParentNode);
  FReg.OpenKey(FixupPath(GetKeyPath(ParentNode)) + KeyName, True);
  CloseRegistry;
  RefreshNode(ParentNode);
  Result := FindChildNode(ParentNode, KeyName);
end;

function TJvRegistryTreeView.AddStringValue(ParentNode: TTreeNode;
  const Name, Value: string): TTreeNode;
begin
  Result := nil;
  if ParentNode = nil then
    Exit;
  OpenRegistry(ParentNode);
  FReg.WriteString(FixupPath(GetKeyPath(ParentNode)) + Name, Value);
  CloseRegistry;
  RefreshNode(ParentNode);
  Result := FindChildNode(ParentNode, Name);
end;

procedure TJvRegistryTreeView.CloseRegistry;
begin
  FReg.Free;
  FReg := nil;
end;

procedure TJvRegistryTreeView.OpenRegistry(Node: TTreeNode);
begin
  if FReg = nil then
    FReg := TRegistry.Create;
  SetRootKey(FReg, Node);
end;

function TJvRegistryTreeView.LoadKey(const Filename: string): Boolean;
begin
  OpenRegistry(Selected);
  Result := FReg.LoadKey(ShortPath, ChangeFileExt(Filename, ''));
  CloseRegistry;
end;

function TJvRegistryTreeView.SaveKey(const Filename: string): Boolean;
begin
  OpenRegistry(Selected);
  Result := FReg.SaveKey(ShortPath, Filename);
  CloseRegistry;
end;

end.

