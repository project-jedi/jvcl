{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMRUList.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvMRUManager;

interface

uses
  SysUtils, Classes, Menus, IniFiles, Registry,
  JvAppStore, JvComponent, JvFormPlacement;

type
  TJvRecentStrings = class;

  TGetItemEvent = procedure(Sender: TObject; var Caption: string;
    var ShortCut: TShortCut; UserData: Longint) of object;
  TReadItemEvent = procedure(Sender: TObject; AppStorage: TJvCustomAppStore;
    const Path: string; Index: Integer; var RecentName: string;
    var UserData: Longint) of object;
  TWriteItemEvent = procedure(Sender: TObject; AppStorage: TJvCustomAppStore;
    const Path: string; Index: Integer; const RecentName: string;
    UserData: Longint) of object;
  TClickMenuEvent = procedure(Sender: TObject; const RecentName,
    Caption: string; UserData: Longint) of object;
  TGetItemInfoEvent = procedure(Sender: TObject; Item: TMenuItem) of object;

  TAccelDelimiter = (adTab, adSpace);
  TRecentMode = (rmInsert, rmAppend);

  TJvMRUManager = class(TJvComponent)
  private
    FList: TStrings;
    FItems: TList;
    FIniLink: TJvIniLink;
    FSeparateSize: Word;
    FAutoEnable: Boolean;
    FAutoUpdate: Boolean;
    FShowAccelChar: Boolean;
    FRemoveOnSelect: Boolean;
    FStartAccel: Cardinal;
    FAccelDelimiter: TAccelDelimiter;
    FRecentMenu: TMenuItem;
    FOnChange: TNotifyEvent;
    FOnGetItem: TGetItemEvent;
    FOnClick: TClickMenuEvent;
    FOnReadItem: TReadItemEvent;
    FOnWriteItem: TWriteItemEvent;
    FOnAfterUpdate: TNotifyEvent;
    FOnBeforeUpdate: TNotifyEvent;
    FOnItemInfo: TGetItemInfoEvent;
    FDuplicates: TDuplicates;
    procedure ListChanged(Sender: TObject);
    procedure ClearRecentMenu;
    procedure SetRecentMenu(Value: TMenuItem);
    procedure SetSeparateSize(Value: Word);
    function GetStorage: TJvFormPlacement;
    procedure SetStorage(Value: TJvFormPlacement);
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    function GetMode: TRecentMode;
    procedure SetMode(Value: TRecentMode);
    procedure SetStartAccel(Value: Cardinal);
    procedure SetShowAccelChar(Value: Boolean);
    procedure SetAccelDelimiter(Value: TAccelDelimiter);
    procedure SetAutoEnable(Value: Boolean);
    procedure AddMenuItem(Item: TMenuItem);
    procedure MenuItemClick(Sender: TObject);
    procedure IniSave(Sender: TObject);
    procedure IniLoad(Sender: TObject);
    procedure InternalLoad(const Section: string);
    procedure InternalSave(const Section: string);
    procedure SetDuplicates(const Value: TDuplicates);
    procedure DoDuplicateFixUp;
  protected
    procedure Change; dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoReadItem(AppStorage: TJvCustomAppStore; const Path: string; Index: Integer;
      var RecentName: string; var UserData: Longint); dynamic;
    procedure DoWriteItem(AppStorage: TJvCustomAppStore; const Path: string; Index: Integer;
      const RecentName: string; UserData: Longint); dynamic;
    procedure GetItemData(var Caption: string; var ShortCut: TShortCut;
      UserData: Longint); dynamic;
    procedure GetItemInfo(Item: TMenuItem); dynamic;
    procedure DoClick(const RecentName, Caption: string; UserData: Longint); dynamic;
    procedure DoBeforeUpdate; virtual;
    procedure DoAfterUpdate; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(const RecentName: string; UserData: Longint);
    procedure Clear;
    procedure Remove(const RecentName: string);
    procedure UpdateRecentMenu;
    procedure RemoveInvalid;
    procedure LoadFromAppStore(const AppStorage: TJvCustomAppStore; const Path: string);
    procedure SaveToAppStore(const AppStorage: TJvCustomAppStore; const Path: string);
    procedure Load;
    procedure Save;
    property Strings: TStrings read FList;
  published
    // Duplicates works just as for TStrings, but the list doesn't need to be sorted
    property Duplicates: TDuplicates read FDuplicates write SetDuplicates;
    property AccelDelimiter: TAccelDelimiter read FAccelDelimiter write SetAccelDelimiter default adTab;
    property AutoEnable: Boolean read FAutoEnable write SetAutoEnable default True;
    property AutoUpdate: Boolean read FAutoUpdate write FAutoUpdate default True;
    property Capacity: Integer read GetCapacity write SetCapacity default 10;
    property Mode: TRecentMode read GetMode write SetMode default rmInsert;
    property RemoveOnSelect: Boolean read FRemoveOnSelect write FRemoveOnSelect default False;
    property IniStorage: TJvFormPlacement read GetStorage write SetStorage;
    property SeparateSize: Word read FSeparateSize write SetSeparateSize default 0;
    property RecentMenu: TMenuItem read FRecentMenu write SetRecentMenu;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default True;
    property StartAccel: Cardinal read FStartAccel write SetStartAccel default 1;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick: TClickMenuEvent read FOnClick write FOnClick;
    // OnGetItemData is called just before the menu item is created
    property OnGetItemData: TGetItemEvent read FOnGetItem write FOnGetItem;
    property OnReadItem: TReadItemEvent read FOnReadItem write FOnReadItem;
    property OnWriteItem: TWriteItemEvent read FOnWriteItem write FOnWriteItem;
    // called just before the menu items are updated
    property OnBeforeUpdate: TNotifyEvent read FOnBeforeUpdate write FOnBeforeUpdate;
    // called just after the menu items have been updated
    property OnAfterUpdate: TNotifyEvent read FOnAfterUpdate write FOnAfterUpdate;
    // called just before the newly created menu item is added to the RecentMenu submenu
    // this makes it easier to set any additional properties of the menu item not
    // handled by OnGetItemData.
    property OnGetItemInfo: TGetItemInfoEvent read FOnItemInfo write FOnItemInfo;
  end;

  TJvRecentStrings = class(TStringList)
  private
    FMaxSize: Integer;
    FMode: TRecentMode;
    procedure SetMaxSize(Value: Integer);
  public
    constructor Create;
    function Add(const S: string): Integer; override;
    procedure AddStrings(Strings: TStrings); override;
    procedure DeleteExceed;
    procedure Remove(const S: string);
    property MaxSize: Integer read FMaxSize write SetMaxSize;
    property Mode: TRecentMode read FMode write FMode;
  end;


implementation

uses
  Controls, Math,
  JvConsts, JvJVCLUtils, JvResources;

const
  siRecentItem = 'Item_%d';
  siRecentData = 'User_%d';

//=== TJvMRUManager ==========================================================

constructor TJvMRUManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList := TJvRecentStrings.Create;
  FItems := TList.Create;
  TJvRecentStrings(FList).OnChange := ListChanged;
  FIniLink := TJvIniLink.Create;
  FIniLink.OnSave := IniSave;
  FIniLink.OnLoad := IniLoad;
  FAutoUpdate := True;
  FAutoEnable := True;
  FShowAccelChar := True;
  FStartAccel := 1;
end;

destructor TJvMRUManager.Destroy;
begin
  ClearRecentMenu;
  FIniLink.Free;
  TJvRecentStrings(FList).OnChange := nil;
  FList.Free;
  FItems.Free;
  FItems := nil;
  inherited Destroy;
end;

procedure TJvMRUManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = RecentMenu) and (Operation = opRemove) then
    RecentMenu := nil;
end;

procedure TJvMRUManager.GetItemData(var Caption: string; var ShortCut: TShortCut;
  UserData: Longint);
begin
  if Assigned(FOnGetItem) then
    FOnGetItem(Self, Caption, ShortCut, UserData);
end;

procedure TJvMRUManager.DoClick(const RecentName, Caption: string; UserData: Longint);
begin
  if Assigned(FOnClick) then
    FOnClick(Self, RecentName, Caption, UserData);
end;

procedure TJvMRUManager.MenuItemClick(Sender: TObject);
var
  I: Integer;
begin
  if Sender is TMenuItem then
  begin
    I := TMenuItem(Sender).Tag;
    if (I >= 0) and (I < FList.Count) then
    try
      DoClick(FList[I], TMenuItem(Sender).Caption, Longint(FList.Objects[I]));
    finally
      if RemoveOnSelect then
        Remove(FList[I]);
    end;
  end;
end;

function TJvMRUManager.GetCapacity: Integer;
begin
  Result := TJvRecentStrings(FList).MaxSize;
end;

procedure TJvMRUManager.SetCapacity(Value: Integer);
begin
  TJvRecentStrings(FList).MaxSize := Value;
end;

function TJvMRUManager.GetMode: TRecentMode;
begin
  Result := TJvRecentStrings(FList).Mode;
end;

procedure TJvMRUManager.SetMode(Value: TRecentMode);
begin
  TJvRecentStrings(FList).Mode := Value;
end;

function TJvMRUManager.GetStorage: TJvFormPlacement;
begin
  Result := FIniLink.Storage;
end;

procedure TJvMRUManager.SetStorage(Value: TJvFormPlacement);
begin
  FIniLink.Storage := Value;
end;

procedure TJvMRUManager.SetAutoEnable(Value: Boolean);
begin
  if FAutoEnable <> Value then
  begin
    FAutoEnable := Value;
    if Assigned(FRecentMenu) and FAutoEnable then
      FRecentMenu.Enabled := FRecentMenu.Count > 0;
  end;
end;

procedure TJvMRUManager.SetStartAccel(Value: Cardinal);
begin
  if FStartAccel <> Value then
  begin
    FStartAccel := Value;
    if FAutoUpdate then
      UpdateRecentMenu;
  end;
end;

procedure TJvMRUManager.SetAccelDelimiter(Value: TAccelDelimiter);
begin
  if FAccelDelimiter <> Value then
  begin
    FAccelDelimiter := Value;
    if FAutoUpdate and ShowAccelChar then
      UpdateRecentMenu;
  end;
end;

procedure TJvMRUManager.SetShowAccelChar(Value: Boolean);
begin
  if FShowAccelChar <> Value then
  begin
    FShowAccelChar := Value;
    if FAutoUpdate then
      UpdateRecentMenu;
  end;
end;

procedure TJvMRUManager.Add(const RecentName: string; UserData: Longint);
var
  I: Integer;
begin
  if not (Duplicates = dupAccept) and (FList.IndexOf(RecentName) > -1) then
  begin
    if Duplicates = dupError then
      raise Exception.Create(sDuplicatesNotAllowedInMRUList);
  end
  else
  begin
    I := TJvRecentStrings(FList).Add(RecentName);
    FList.Objects[I] := TObject(UserData);
  end;
end;

procedure TJvMRUManager.Clear;
begin
  FList.Clear;
end;

procedure TJvMRUManager.Remove(const RecentName: string);
begin
  TJvRecentStrings(FList).Remove(RecentName);
end;

procedure TJvMRUManager.AddMenuItem(Item: TMenuItem);
begin
  if Assigned(Item) then
  begin
    FRecentMenu.Add(Item);
    FItems.Add(Item);
  end;
end;

procedure TJvMRUManager.DoDuplicateFixUp;
var
  I, J: Integer;
  Tmp: Boolean;
begin
  if Duplicates = dupAccept then
    Exit;
  Tmp := AutoUpdate;
  try
    AutoUpdate := False;
    I := FList.Count - 1;
    while I >= 0 do
    begin
      // we don't raise an error here even if Duplicates is dupError
      J := FList.IndexOf(FList[I]);
      while (J > -1) and (J <> I) do
      begin
        FList.Delete(J);
        Dec(I);
        J := FList.IndexOf(FList[I]);
      end;
      Dec(I);
    end;
  finally
    AutoUpdate := Tmp;
  end;
end;

procedure TJvMRUManager.UpdateRecentMenu;
const
  AccelDelimChars: array [TAccelDelimiter] of Char = (#9, ' ');
var
  I: Integer;
  L: Cardinal;
  S: string;
  C: string[2];
  ShortCut: TShortCut;
  Item: TMenuItem;
begin
  ClearRecentMenu;
  DoDuplicateFixUp;
  DoBeforeUpdate;
  if Assigned(FRecentMenu) then
  begin
    if (FList.Count > 0) and (FRecentMenu.Count > 0) then
      AddMenuItem(NewLine);
    for I := 0 to FList.Count - 1 do
    begin
      if (FSeparateSize > 0) and (I > 0) and (I mod FSeparateSize = 0) then
        AddMenuItem(NewLine);
      S := FList[I];
      ShortCut := scNone;
      GetItemData(S, ShortCut, Longint(FList.Objects[I]));
      Item := NewItem(GetShortHint(S), ShortCut, False, True,
        MenuItemClick, 0, '');
      Item.Hint := GetLongHint(S);
      if FShowAccelChar then
      begin
        L := Cardinal(I) + FStartAccel;
        if L < 10 then
          C := '&' + Char(Ord('0') + L)
        else
        if L <= (Ord('Z') + 10) then
          C := '&' + Char(L + Ord('A') - 10)
        else
          C := ' ';
        Item.Caption := C + AccelDelimChars[FAccelDelimiter] + Item.Caption;
      end;
      Item.Tag := I;
      GetItemInfo(Item);
      AddMenuItem(Item);
    end;
    DoAfterUpdate;
    if AutoEnable then
      FRecentMenu.Enabled := FRecentMenu.Count > 0;
  end;
end;

procedure TJvMRUManager.ClearRecentMenu;
var
  Item: TMenuItem;
begin
  while FItems.Count > 0 do
  begin
    Item := TMenuItem(FItems[0]);
    if Assigned(FRecentMenu) and (FRecentMenu.IndexOf(Item) >= 0) then
      Item.Free;
    FItems.Remove(Item);
  end;
  if Assigned(FRecentMenu) and AutoEnable then
    FRecentMenu.Enabled := FRecentMenu.Count > 0;
end;

procedure TJvMRUManager.SetRecentMenu(Value: TMenuItem);
begin
  ClearRecentMenu;
  FRecentMenu := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
  UpdateRecentMenu;
end;

procedure TJvMRUManager.SetSeparateSize(Value: Word);
begin
  if FSeparateSize <> Value then
  begin
    FSeparateSize := Value;
    if FAutoUpdate then
      UpdateRecentMenu;
  end;
end;

procedure TJvMRUManager.ListChanged(Sender: TObject);
begin
  Change;
  if FAutoUpdate then
    UpdateRecentMenu;
end;

procedure TJvMRUManager.IniSave(Sender: TObject);
begin
  if (Name <> '') and (IniStorage.IsActive) then
    InternalSave(GetDefaultSection(Self));
end;

procedure TJvMRUManager.IniLoad(Sender: TObject);
begin
  if (Name <> '') and (IniStorage.IsActive) then
    InternalLoad(GetDefaultSection(Self));
end;

procedure TJvMRUManager.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvMRUManager.DoReadItem(AppStorage: TJvCustomAppStore; const Path: string;
  Index: Integer; var RecentName: string; var UserData: Longint);
begin
  if Assigned(FOnReadItem) then
    FOnReadItem(Self, AppStorage, Path, Index, RecentName, UserData)
  else
  begin
    RecentName := AppStorage.ReadString(AppStorage.ConcatPaths(
      [Path, Format(siRecentItem, [Index])]), RecentName);
    UserData := AppStorage.ReadInteger(AppStorage.ConcatPaths(
      [Path, Format(siRecentData, [Index])]), UserData);
  end;
end;

procedure TJvMRUManager.DoWriteItem(AppStorage: TJvCustomAppStore; const Path: string;
  Index: Integer; const RecentName: string; UserData: Longint);
begin
  if Assigned(FOnWriteItem) then
    FOnWriteItem(Self, AppStorage, Path, Index, RecentName, UserData)
  else
  begin
    AppStorage.WriteString(AppStorage.ConcatPaths(
      [Path, Format(siRecentItem, [Index])]), RecentName);
    if UserData = 0 then
      AppStorage.DeleteValue(AppStorage.ConcatPaths(
      [Path, Format(siRecentData, [Index])]))
    else
      AppStorage.WriteInteger(AppStorage.ConcatPaths(
      [Path, Format(siRecentData, [Index])]), UserData);
  end;
end;

procedure TJvMRUManager.InternalLoad(const Section: string);
begin
  if IniStorage.IsActive then
    with IniStorage do
      LoadFromAppStore(AppStorage, AppStorage.ConcatPaths([AppStoragePath, Section]));
end;

procedure TJvMRUManager.InternalSave(const Section: string);
begin
  if IniStorage.IsActive then
    with IniStorage do
      SaveToAppStore(AppStorage, AppStorage.ConcatPaths([AppStoragePath, Section]));
end;

procedure TJvMRUManager.LoadFromAppStore(const AppStorage: TJvCustomAppStore; const Path: string);
var
  I: Integer;
  S: string;
  UserData: Longint;
  AMode: TRecentMode;
begin
  AMode := Mode;
  FList.BeginUpdate;
  try
    FList.Clear;
    Mode := rmInsert;
    for I := TJvRecentStrings(FList).MaxSize - 1 downto 0 do
    begin
      S := '';
      UserData := 0;
      DoReadItem(AppStorage, Path, I, S, UserData);
      if S <> '' then
        Add(S, UserData);
    end;
  finally
    Mode := AMode;
    FList.EndUpdate;
  end;
end;

procedure TJvMRUManager.SaveToAppStore(const AppStorage: TJvCustomAppStore; const Path: string);
var
  I: Integer;
begin
  AppStorage.DeleteSubTree(Path);
  for I := 0 to FList.Count - 1 do
    DoWriteItem(AppStorage, Path, I, FList[I], Longint(FList.Objects[I]));
end;

procedure TJvMRUManager.Load;
begin
  IniLoad(nil);
end;

procedure TJvMRUManager.Save;
begin
  IniSave(nil);
end;

procedure TJvMRUManager.DoAfterUpdate;
begin
  if Assigned(FOnAfterUpdate) then
    FOnAfterUpdate(Self);
end;

procedure TJvMRUManager.DoBeforeUpdate;
begin
  if Assigned(FOnBeforeUpdate) then
    FOnBeforeUpdate(Self);
end;

procedure TJvMRUManager.RemoveInvalid;
var
  I: Integer;
begin
  for I := FList.Count - 1 downto 0 do
    if not FileExists(FList[I]) then
      FList.Delete(I);
end;

procedure TJvMRUManager.GetItemInfo(Item: TMenuItem);
begin
  if Assigned(FOnItemInfo) then
    FOnItemInfo(Self, Item);
end;

procedure TJvMRUManager.SetDuplicates(const Value: TDuplicates);
begin
  if FDuplicates <> Value then
  begin
    FDuplicates := Value;
    if FAutoUpdate then
      UpdateRecentMenu;
  end;
end;

//=== TJvRecentStrings =======================================================

constructor TJvRecentStrings.Create;
begin
  inherited Create;
  FMaxSize := 10;
  FMode := rmInsert;
end;

procedure TJvRecentStrings.SetMaxSize(Value: Integer);
begin
  if FMaxSize <> Value then
  begin
    FMaxSize := Max(1, Value);
    DeleteExceed;
  end;
end;

procedure TJvRecentStrings.DeleteExceed;
var
  I: Integer;
begin
  BeginUpdate;
  try
    if FMode = rmInsert then
      for I := Count - 1 downto FMaxSize do
        Delete(I)
    else
    begin { rmAppend }
      while Count > FMaxSize do
        Delete(0);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TJvRecentStrings.Remove(const S: string);
var
  I: Integer;
begin
  I := IndexOf(S);
  if I >= 0 then
    Delete(I);
end;

function TJvRecentStrings.Add(const S: string): Integer;
begin
  Result := IndexOf(S);
  if Result >= 0 then
  begin
    if FMode = rmInsert then
      Move(Result, 0)
    else { rmAppend }
      Move(Result, Count - 1);
  end
  else
  begin
    BeginUpdate;
    try
      if FMode = rmInsert then
        Insert(0, S)
      else { rmAppend }
        Insert(Count, S);
      DeleteExceed;
    finally
      EndUpdate;
    end;
  end;
  if FMode = rmInsert then
    Result := 0
  else { rmAppend }
    Result := Count - 1;
end;

procedure TJvRecentStrings.AddStrings(Strings: TStrings);
var
  I: Integer;
begin
  BeginUpdate;
  try
    if FMode = rmInsert then
    begin
      for I := Min(Strings.Count, FMaxSize) - 1 downto 0 do
        AddObject(Strings[I], Strings.Objects[I]);
    end
    else { rmAppend }
      for I := 0 to Min(Strings.Count, FMaxSize) - 1 do
        AddObject(Strings[I], Strings.Objects[I]);
    DeleteExceed;
  finally
    EndUpdate;
  end;
end;

end.

