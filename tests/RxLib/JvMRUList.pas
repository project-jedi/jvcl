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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}


unit JvMRUList;

interface

uses SysUtils, Classes, Menus, IniFiles {$IFDEF WIN32}, Registry {$ENDIF},
  JvPlacemnt{, JvComponent};

type
  TJvRecentStrings = class;

{ TJvMRUManager }

  TGetItemEvent = procedure (Sender: TObject; var Caption: string;
    var ShortCut: TShortCut; UserData: Longint) of object;
  TReadItemEvent = procedure (Sender: TObject; IniFile: TObject;
    const Section: string; Index: Integer; var RecentName: string;
    var UserData: Longint) of object;
  TWriteItemEvent = procedure (Sender: TObject; IniFile: TObject;
    const Section: string; Index: Integer; const RecentName: string;
    UserData: Longint) of object;
  TClickMenuEvent = procedure (Sender: TObject; const RecentName,
    Caption: string; UserData: Longint) of object;

  TAccelDelimiter = (adTab, adSpace);
  TRecentMode = (rmInsert, rmAppend);

  TJvMRUManager = class(TComponent)
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
    procedure InternalLoad(Ini: TObject; const Section: string);
    procedure InternalSave(Ini: TObject; const Section: string);
  protected
    procedure Change; dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoReadItem(Ini: TObject; const Section: string;
      Index: Integer; var RecentName: string; var UserData: Longint); dynamic;
    procedure DoWriteItem(Ini: TObject; const Section: string; Index: Integer;
      const RecentName: string; UserData: Longint); dynamic;
    procedure GetItemData(var Caption: string; var ShortCut: TShortCut;
      UserData: Longint); dynamic;
    procedure DoClick(const RecentName, Caption: string; UserData: Longint); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(const RecentName: string; UserData: Longint);
    procedure Clear;
    procedure Remove(const RecentName: string);
    procedure UpdateRecentMenu;
{$IFDEF WIN32}
    procedure LoadFromRegistry(Ini: TRegIniFile; const Section: string);
    procedure SaveToRegistry(Ini: TRegIniFile; const Section: string);
{$ENDIF WIN32}
    procedure LoadFromIni(Ini: TIniFile; const Section: string);
    procedure SaveToIni(Ini: TIniFile; const Section: string);
    property Strings: TStrings read FList;
  published
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
    property OnGetItemData: TGetItemEvent read FOnGetItem write FOnGetItem;
    property OnReadItem: TReadItemEvent read FOnReadItem write FOnReadItem;
    property OnWriteItem: TWriteItemEvent read FOnWriteItem write FOnWriteItem;
  end;

{ TJvRecentStrings }

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
    procedure Remove(const S: String);
    property MaxSize: Integer read FMaxSize write SetMaxSize;
    property Mode: TRecentMode read FMode write FMode;
  end;

implementation

uses Controls, JvMaxMin, JvAppUtils;

const
  siRecentItem = 'Item_%d';
  siRecentData = 'User_%d';

{ TJvMRUManager }

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
  if Assigned(FOnGetItem) then FOnGetItem(Self, Caption, ShortCut, UserData);
end;

procedure TJvMRUManager.DoClick(const RecentName, Caption: string; UserData: Longint);
begin
  if Assigned(FOnClick) then FOnClick(Self, RecentName, Caption, UserData);
end;

procedure TJvMRUManager.MenuItemClick(Sender: TObject);
var
  I: Integer;
begin
  if Sender is TMenuItem then begin
    I := TMenuItem(Sender).Tag;
    if (I >= 0) and (I < FList.Count) then
      try
        DoClick(FList[I], TMenuItem(Sender).Caption, Longint(FList.Objects[I]));
      finally
        if RemoveOnSelect then Remove(FList[I]);
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
  if FAutoEnable <> Value then begin
    FAutoEnable := Value;
    if Assigned(FRecentMenu) and FAutoEnable then
      FRecentMenu.Enabled := FRecentMenu.Count > 0;
  end;
end;

procedure TJvMRUManager.SetStartAccel(Value: Cardinal);
begin
  if FStartAccel <> Value then begin
    FStartAccel := Value;
    if FAutoUpdate then UpdateRecentMenu;
  end;
end;

procedure TJvMRUManager.SetAccelDelimiter(Value: TAccelDelimiter);
begin
  if FAccelDelimiter <> Value then begin
    FAccelDelimiter := Value;
    if FAutoUpdate and ShowAccelChar then UpdateRecentMenu;
  end;
end;

procedure TJvMRUManager.SetShowAccelChar(Value: Boolean);
begin
  if FShowAccelChar <> Value then begin
    FShowAccelChar := Value;
    if FAutoUpdate then UpdateRecentMenu;
  end;
end;

procedure TJvMRUManager.Add(const RecentName: string; UserData: Longint);
begin
  FList.AddObject(RecentName, TObject(UserData));
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
  if Assigned(Item) then begin
    FRecentMenu.Add(Item);
    FItems.Add(Item);
  end;
end;

procedure TJvMRUManager.UpdateRecentMenu;
const
  AccelDelimChars: array[TAccelDelimiter] of Char = (#9, ' ');
var
  I: Integer;
  L: Cardinal;
  S: string;
  C: string[2];
  ShortCut: TShortCut;
  Item: TMenuItem;
begin
  ClearRecentMenu;
  if Assigned(FRecentMenu) then begin
    if (FList.Count > 0) and (FRecentMenu.Count > 0) then
      AddMenuItem(NewLine);
    for I := 0 to FList.Count - 1 do begin
      if (FSeparateSize > 0) and (I > 0) and (I mod FSeparateSize = 0) then
        AddMenuItem(NewLine);
      S := FList[I];
      ShortCut := scNone;
      GetItemData(S, ShortCut, Longint(FList.Objects[I]));
      Item := NewItem(GetShortHint(S), ShortCut, False, True,
        MenuItemClick, 0, '');
      Item.Hint := GetLongHint(S);
      if FShowAccelChar then begin
        L := Cardinal(I) + FStartAccel;
        if L < 10 then
          C := '&' + Char(Ord('0') + L)
        else if L <= (Ord('Z') + 10) then
          C := '&' + Char(L + Ord('A') - 10)
        else
          C := ' ';
        Item.Caption := C + AccelDelimChars[FAccelDelimiter] + Item.Caption;
      end;
      Item.Tag := I;
      AddMenuItem(Item);
    end;
    if AutoEnable then FRecentMenu.Enabled := FRecentMenu.Count > 0;
  end;
end;

procedure TJvMRUManager.ClearRecentMenu;
var
  Item: TMenuItem;
begin
  while FItems.Count > 0 do begin
    Item := TMenuItem(FItems.Last);
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
{$IFDEF WIN32}
  if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
  UpdateRecentMenu;
end;

procedure TJvMRUManager.SetSeparateSize(Value: Word);
begin
  if FSeparateSize <> Value then begin
    FSeparateSize := Value;
    if FAutoUpdate then UpdateRecentMenu;
  end;
end;

procedure TJvMRUManager.ListChanged(Sender: TObject);
begin
  Change;
  if FAutoUpdate then UpdateRecentMenu;
end;

procedure TJvMRUManager.IniSave(Sender: TObject);
begin
  if (Name <> '') and (FIniLink.IniObject <> nil) then
    InternalSave(FIniLink.IniObject, FIniLink.RootSection +
      GetDefaultSection(Self));
end;

procedure TJvMRUManager.IniLoad(Sender: TObject);
begin
  if (Name <> '') and (FIniLink.IniObject <> nil) then
    InternalLoad(FIniLink.IniObject, FIniLink.RootSection +
      GetDefaultSection(Self));
end;

procedure TJvMRUManager.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TJvMRUManager.DoReadItem(Ini: TObject; const Section: string;
  Index: Integer; var RecentName: string; var UserData: Longint);
begin
  if Assigned(FOnReadItem) then
    FOnReadItem(Self, Ini, Section, Index, RecentName, UserData)
  else begin
    RecentName := IniReadString(Ini, Section, Format(siRecentItem, [Index]), RecentName);
    UserData := IniReadInteger(Ini, Section, Format(siRecentData, [Index]), UserData);
  end;
end;

procedure TJvMRUManager.DoWriteItem(Ini: TObject; const Section: string;
  Index: Integer; const RecentName: string; UserData: Longint);
begin
  if Assigned(FOnWriteItem) then
    FOnWriteItem(Self, Ini, Section, Index, RecentName, UserData)
  else begin
    IniWriteString(Ini, Section, Format(siRecentItem, [Index]), RecentName);
    if UserData = 0 then
      IniDeleteKey(Ini, Section, Format(siRecentData, [Index]))
    else
      IniWriteInteger(Ini, Section, Format(siRecentData, [Index]), UserData);
  end;
end;

procedure TJvMRUManager.InternalLoad(Ini: TObject; const Section: string);
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
    for I := TJvRecentStrings(FList).MaxSize - 1 downto 0 do begin
      S := '';
      UserData := 0;
      DoReadItem(Ini, Section, I, S, UserData);
      if S <> '' then Add(S, UserData);
    end;
  finally
    Mode := AMode;
    FList.EndUpdate;
  end;
end;

procedure TJvMRUManager.InternalSave(Ini: TObject; const Section: string);
var
  I: Integer;
begin
  IniEraseSection(Ini, Section);
  for I := 0 to FList.Count - 1 do
    DoWriteItem(Ini, Section, I, FList[I], Longint(FList.Objects[I]));
end;

{$IFDEF WIN32}
procedure TJvMRUManager.LoadFromRegistry(Ini: TRegIniFile; const Section: string);
begin
  InternalLoad(Ini, Section);
end;

procedure TJvMRUManager.SaveToRegistry(Ini: TRegIniFile; const Section: string);
begin
  InternalSave(Ini, Section);
end;
{$ENDIF WIN32}

procedure TJvMRUManager.LoadFromIni(Ini: TIniFile; const Section: string);
begin
  InternalLoad(Ini, Section);
end;

procedure TJvMRUManager.SaveToIni(Ini: TIniFile; const Section: string);
begin
  InternalSave(Ini, Section);
end;

{ TJvRecentStrings }

constructor TJvRecentStrings.Create;
begin
  inherited Create;
  FMaxSize := 10;
  FMode := rmInsert;
end;

procedure TJvRecentStrings.SetMaxSize(Value: Integer);
begin
  if FMaxSize <> Value then begin
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
    if FMode = rmInsert then begin
      for I := Count - 1 downto FMaxSize do Delete(I);
    end
    else begin { rmAppend }
      while Count > FMaxSize do Delete(0);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TJvRecentStrings.Remove(const S: String);
var
  I: Integer;
begin
  I := IndexOf(S);
  if I >= 0 then Delete(I);
end;

function TJvRecentStrings.Add(const S: String): Integer;
begin
  Result := IndexOf(S);
  if Result >= 0 then begin
    if FMode = rmInsert then Move(Result, 0)
    else { rmAppend } Move(Result, Count - 1);
  end
  else begin
    BeginUpdate;
    try
      if FMode = rmInsert then Insert(0, S)
      else { rmAppend } Insert(Count, S);
      DeleteExceed;
    finally
      EndUpdate;
    end;
  end;
  if FMode = rmInsert then Result := 0
  else { rmAppend } Result := Count - 1;
end;

procedure TJvRecentStrings.AddStrings(Strings: TStrings);
var
  I: Integer;
begin
  BeginUpdate;
  try
    if FMode = rmInsert then begin
      for I := Min(Strings.Count, FMaxSize) - 1 downto 0 do
        AddObject(Strings[I], Strings.Objects[I]);
    end
    else begin { rmAppend }
      for I := 0 to Min(Strings.Count, FMaxSize) - 1 do
        AddObject(Strings[I], Strings.Objects[I]);
    end;
    DeleteExceed;
  finally
    EndUpdate;
  end;
end;

end.
