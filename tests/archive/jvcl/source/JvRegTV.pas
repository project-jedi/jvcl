{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRegTV.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{A treeview that displays the keys from the registry }
unit JvRegTV;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Registry, ImgList, JVCLVer;

type
  TJvRegistryKey = (hkClassesRoot, hkCurrentUser, hkLocalMachine, hkUsers, hkPerformanceData, hkCurrentConfig, hkDynData);
  TJvRegistryKeys = set of TJvRegistryKey;

  TJvRegistryTreeView = class(TCustomTreeView)
  private
    { Private declarations }
    FAboutJVCL: TJVCLAboutInfo;
    FRegistryKeys: TJvRegistryKeys;
    FInternalImages: TImageList;
    FListView: TListView;
    FRootCaption: string;
    FDefaultCaption: string;
    FDefaultNoValue: string;
    FReg: TRegistry;
    procedure SetDefaultCaption(Value: string);
    procedure SetDefaultNoValue(Value: string);
    procedure SetRootCaption(Value: string);
    procedure SetRegistryKeys(Value: TJvRegistryKeys);
    procedure BuildTree;
    function FillListView(Node: TTreeNode): boolean;
    procedure SetDefaultImages;
    function GetCurrentPath: string;
    function GetShortPath: string;
    function GetCurrentKey: HKEY;
    function GetShowHint: boolean;
    procedure SetShowHint(Value: boolean);
    procedure OpenRegistry(Node: TTreeNode);
    procedure CloseRegistry;
    function FindChildNode(ParentNode: TTreeNode;
      const Name: string): TTreeNode;
  protected
    { Protected declarations }
    procedure RefreshSubTrees(aNode: TTreeNode; Key, OldKey: string; Level: integer); virtual;
    function CanCollapse(Node: TTreeNode): Boolean; override;
    function CanExpand(Node: TTreeNode): Boolean; override;
    procedure Change(Node: TTreeNode); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function SaveKey(const Filename: string): boolean;
    function LoadKey(const Filename: string): boolean;
    procedure RefreshNode(Node: TTreeNode);
    function AddKey(ParentNode: TTreeNode; const KeyName: string): TTreeNode;
    function AddStringValue(ParentNode: TTreeNode; const Name, Value: string): TTreeNode;
    function AddBinaryValue(ParentNode: TTreeNode; const Name: string; var Buf; BufSize: integer): TTreeNode;
    function AddDWORDValue(ParentNode: TTreeNode; const Name: string; Value: DWORD): TTreeNode;
    property CurrentPath: string read GetCurrentPath;
    property ShortPath: string read GetShortPath;
    property CurrentKey: HKEY read GetCurrentKey;
    property Items stored false;
  published
    { Published declarations }
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;	
    property Align;
    property Color;
    property BorderStyle;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property ShowButtons;
    property ShowHint: boolean read GetShowHint write SetShowHint;
    property ShowLines;
    property ShowRoot;
    property ReadOnly default true;
    property RightClickSelect;
    property Indent;
    property HideSelection;
    property RegistryKeys: TJvRegistryKeys read FRegistryKeys write SetRegistryKeys default [hkCurrentUser, hkLocalMachine];
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

resourcestring
  SDefaultCaption = '(Default)';
  SMyComputer = 'My Computer';
  SDefaultNoValue = '(value not set)';
  SUnknown = '(Unknown)';

implementation
{$R JvRegTree.res }

const
  imMyPC = 0;
  imClosed = 1;
  imOpen = 2;
  imText = 3;
  imBin = 4;
type
  THackRegistry = class(TRegistry);

  { utility }

function SetRootKey(Reg: TRegistry; Node: TTreeNode): boolean;
var tmpNode: TTreeNode;
begin
  Result := false;
  if (Node <> nil) then
  begin
    tmpNode := Node;
    while (tmpNode <> nil) do
    begin
      if longint(tmpNode.Data) < 0 then
      begin
        Reg.RootKey := longint(tmpNode.Data);
        Result := true;
        Exit;
      end;
      tmpNode := tmpNode.Parent;
    end;
  end;
end;

function FixupPath(Key: string): string;
begin
  if Key = '' then
    Result := '\'
  else if AnsiLastChar(Key) <> '\' then
    Result := Key + '\'
  else
    Result := Key;
  if Length(Result) > 1 then
    if (Result[1] = '\') and (Result[2] = '\') then
      Result := Copy(Result, 2, Length(Result));
end;

function GetFullPath(aNode: TTreeNode): string;
var TmpNode: TTreeNode;
begin
  Result := '';
  if (aNode = nil) then
    Exit;
  TmpNode := aNode;
  while (TmpNode <> nil) do
  begin
    Result := TmpNode.Text + '\' + Result;
    TmpNode := TmpNode.Parent;
  end;
  if (Length(Result) > 0) and (AnsiLastChar(Result) = '\') then
    SetLength(Result, Length(Result) - 1);
end;

function GetKeyPath(aNode: TTreeNode): string;
var TmpNode: TTreeNode;
begin
  Result := '';
  if aNode = nil then
    Exit;
  TmpNode := aNode;
  while (TmpNode.Parent <> nil) and (TmpNode.Parent.Parent <> nil) do
  begin
    Result := TmpNode.Text + '\' + Result;
    TmpNode := TmpNode.Parent;
  end;
  if (Length(Result) > 0) and (Result[1] <> '\') then
    Result := '\' + Result;
end;

{
function GetPreviousKey(Key:string):string;
var i:integer;
begin
  Result := Key;
  if (Result = '') or (Result = '\') then Exit;
  for i := Length(Result) - 1 downto 1 do
    if Result[i] = '\' then
    begin
      Result := Copy(Result,1,i - 1);
      Exit;
    end;
end;

function StripChars(Str:string;Ch:Char):string;
var i:integer;
begin
  Result := '';
  for i := 1 to Length(Str) do
  begin
    if Str[i] = Ch then Continue;
    AppendStr(Result,str[i]);
  end;
end;
}

function BufToStr(Buffer: array of byte; BufSize: integer): string;
var i: integer;
begin
  Result := '';
  for i := 0 to BufSize - 1 do
    Result := Result + ' ' + IntToHex(Buffer[i], 2);
end;

{ private }

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
  Result := THackRegistry(FReg).GetKey(ShortPath);
  CloseRegistry;
end;

function TJvRegistryTreeView.GetShowHint: boolean;
begin
  Result := inherited ShowHint;
end;

procedure TJvRegistryTreeView.SetShowHint(Value: boolean);
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

procedure TJvRegistryTreeView.RefreshSubTrees(aNode: TTreeNode; Key, OldKey: string; Level: integer);
var aStrings: TStringlist; i: integer; NewNode: TTreeNode; aKey: string;
begin
  aKey := FixupPath(OldKey);
  if FReg.OpenKeyReadOnly(Key) and FReg.HasSubKeys then
  begin
    aNode.HasChildren := true;
    Dec(Level);
    if Level = 1 then
    begin
      aStrings := TStringlist.Create;
      try
        FReg.GetKeyNames(aStrings);
        for i := 0 to aStrings.Count - 1 do
        begin
          NewNode := Items.AddChild(aNode, aStrings[i]);
          NewNode.ImageIndex := imClosed;
          NewNode.SelectedIndex := imOpen;
          RefreshSubTrees(NewNode, aStrings[i], aKey + Key, Level);
        end;
      finally
        aStrings.Free;
      end;
    end;
  end;
  FReg.OpenKeyReadOnly(aKey);
end;

function RegDataTypeToString(Value: TRegDataType): string;
begin
  case Value of
    rdString: Result := 'REG_SZ';
    rdExpandString: Result := 'REG_EXPAND_SZ';
    rdInteger: Result := 'REG_DWORD';
    rdBinary: Result := 'REG_BINARY';
  else
    Result := 'REG_NONE';
  end;
end;

function TJvRegistryTreeView.FillListView(Node: TTreeNode): boolean;
var aStrings: TStrings; i: integer; TmpItem: TListItem;
  Buffer: array[0..4095] of byte; S: string; DefaultSet: boolean;
begin
  Result := false;
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
    DefaultSet := false;
    if FReg.OpenKeyReadOnly(GetKeyPath(Node)) then
    begin
      aStrings := TStringList.Create;
      FReg.GetValueNames(aStrings);

      for i := 0 to aStrings.Count - 1 do
      begin
        { set default item }
        if (aStrings[i] = '') and not DefaultSet then
        begin
          TmpItem := FListView.Items.Insert(0);
          TmpItem.Caption := FDefaultCaption;
          DefaultSet := true;
        end
        else
        begin
          TmpItem := FListView.Items.Add;
          TmpItem.Caption := aStrings[i];
        end;

        case FReg.GetDataType(aStrings[i]) of
          rdUnknown:
            begin
              TmpItem.ImageIndex := imText;
              TmpItem.SubItems.Add(SUnknown);
            end;
          rdString, rdExpandString:
            begin
              S := FReg.ReadString(aStrings[i]);
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
              TmpItem.SubItems.Add(Format('0x%.8x (%d)', [FReg.ReadInteger(aStrings[i]), FReg.ReadInteger(aStrings[i])]));
            end;
          rdBinary:
            begin
              TmpItem.ImageIndex := imBin;
              FReg.ReadBinaryData(aStrings[i], Buffer, 4095);
              TmpItem.SubItems.Add(BufToStr(Buffer, FReg.GetDataSize(aStrings[i])));
            end;
        end; // case
        TmpItem.SubItems.Add(RegDataTypeToString(FReg.GetDataType(aStrings[i])));
      end;
      Result := true;
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

procedure TJvRegistryTreeView.SetRegistryKeys(Value: TJvRegistryKeys);
begin
  if FRegistryKeys <> Value then
    FRegistryKeys := Value;
  BuildTree;
end;

procedure TJvRegistryTreeView.BuildTree;
var NewNode, aNode: TTreeNode;
begin
  OpenRegistry(nil);
  Items.BeginUpdate;
  try
    Items.Clear;
    aNode := Items.Add(nil, FRootCaption);
    aNode.ImageIndex := imMyPC;
    aNode.SelectedIndex := imMyPC;
    if hkClassesRoot in FRegistryKeys then
    begin
      FReg.RootKey := HKEY_CLASSES_ROOT;
      NewNode := Items.AddChild(aNode, 'HKEY_CLASSES_ROOT');
      NewNode.ImageIndex := imClosed;
      NewNode.SelectedIndex := imOpen;
      NewNode.Data := Pointer(FReg.RootKey);
      if not (csDesigning in ComponentState) then
        RefreshSubTrees(NewNode, '\', '', 1);
    end;

    if hkCurrentUser in FRegistryKeys then
    begin
      FReg.Rootkey := HKEY_CURRENT_USER;
      NewNode := Items.AddChild(aNode, 'HKEY_CURRENT_USER');
      NewNode.ImageIndex := imClosed;
      NewNode.SelectedIndex := imOpen;
      NewNode.Data := Pointer(FReg.RootKey);
      if not (csDesigning in ComponentState) then
        RefreshSubTrees(NewNode, '\', '', 1);
    end;

    if hkLocalMachine in FRegistryKeys then
    begin
      FReg.Rootkey := HKEY_LOCAL_MACHINE;
      NewNode := Items.AddChild(aNode, 'HKEY_LOCAL_MACHINE');
      NewNode.ImageIndex := imClosed;
      NewNode.SelectedIndex := imOpen;
      NewNode.Data := Pointer(FReg.RootKey);
      if not (csDesigning in ComponentState) then
        RefreshSubTrees(NewNode, '\', '', 1);
    end;

    if hkUsers in FRegistryKeys then
    begin
      FReg.Rootkey := HKEY_USERS;
      NewNode := Items.AddChild(aNode, 'HKEY_USERS');
      NewNode.ImageIndex := imClosed;
      NewNode.SelectedIndex := imOpen;
      NewNode.Data := Pointer(FReg.RootKey);
      if not (csDesigning in ComponentState) then
        RefreshSubTrees(NewNode, '\', '', 1);
    end;

    if hkPerformanceData in FRegistryKeys then
    begin
      FReg.Rootkey := HKEY_PERFORMANCE_DATA;
      NewNode := Items.AddChild(aNode, 'HKEY_PERFORMANCE_DATA');
      NewNode.ImageIndex := imClosed;
      NewNode.SelectedIndex := imOpen;
      NewNode.Data := Pointer(FReg.RootKey);
      if not (csDesigning in ComponentState) then
        RefreshSubTrees(NewNode, '\', '', 1);
    end;

    if hkCurrentConfig in FRegistryKeys then
    begin
      FReg.Rootkey := HKEY_CURRENT_CONFIG;
      NewNode := Items.AddChild(aNode, 'HKEY_CURRENT_CONFIG');
      NewNode.ImageIndex := imClosed;
      NewNode.SelectedIndex := imOpen;
      NewNode.Data := Pointer(FReg.RootKey);
      if not (csDesigning in ComponentState) then
        RefreshSubTrees(NewNode, '\', '', 1);
    end;
    if hkDynData in FRegistryKeys then
    begin
      FReg.Rootkey := HKEY_DYN_DATA;
      NewNode := Items.AddChild(aNode, 'HKEY_DYN_DATA');
      NewNode.ImageIndex := imClosed;
      NewNode.SelectedIndex := imOpen;
      NewNode.Data := Pointer(FReg.RootKey);
      if not (csDesigning in ComponentState) then
        RefreshSubTrees(NewNode, '\', '', 1);
    end;
    aNode.Expand(false);
    aNode.Selected := true;
  finally
    CloseRegistry;
    Items.EndUpdate;
  end;
end;
{ protected }

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

{ public }

constructor TJvRegistryTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRegistryKeys := [hkCurrentUser, hkLocalMachine];
  FRootCaption := SMyComputer;
  FDefaultCaption := SDefaultCaption;
  FDefaultNoValue := SDefaultNoValue;
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
var b: boolean;
begin
  Items.BeginUpdate;
  try
    b := false;
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
var N: TTreeNode;
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
  const Name: string; var Buf; BufSize: integer): TTreeNode;
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
  FReg.OpenKey(FixupPath(GetKeyPath(ParentNode)) + KeyName, true);
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

function TJvRegistryTreeView.LoadKey(const Filename: string): boolean;
begin
  OpenRegistry(Selected);
  Result := FReg.LoadKey(ShortPath, ChangeFileExt(Filename, ''));
  CloseRegistry;
end;

function TJvRegistryTreeView.SaveKey(const Filename: string): boolean;
begin
  OpenRegistry(Selected);
  Result := FReg.SaveKey(ShortPath, Filename);
  CloseRegistry;
end;

end.

