{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvUninstallControls.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc}

unit JvQUninstallControls;

interface

uses
  Windows, SysUtils, Classes, Types, QGraphics, QControls, QStdCtrls,
  JvQComponent, JvQTypes;

type
  TJvUCBDisplayMode = (hkCurrentUser, hkLocalMachine); // subset of TJvRegKey
  TJvUCBDisplayModes = set of TJvUCBDisplayMode;

  TJvUninstallComboBox = class(TCustomComboBox)
  private
    FDisplayMode: TJvUCBDisplayModes;
    FShowAll: Boolean;
    FShowEmptyValues: Boolean;
    function GetItems: TStrings;
    function GetDisplayName: string;
    function GetSection: string;
    function GetUninstallString: string;
    procedure SetShowAll(const Value: Boolean);
    procedure SetShowEmptyValues(const Value: Boolean);
    procedure SetDisplayMode(const Value: TJvUCBDisplayModes);
    procedure Rebuild;
    function GetProperties: TStrings;
    function GetHKey: HKEY;
    function GetHKeyName: string;
    procedure SetSorted(const Value: boolean);
  protected  
    procedure CreateWidget; override; 
  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    procedure Clear; 
    procedure RefreshItem;
    property Items: TStrings read GetItems;
    property Section: string read GetSection;
    property HKey: HKEY read GetHKey;
    property HKeyName: string read GetHKeyName;
    property UninstallString: string read GetUninstallString;
    property DisplayName: string read GetDisplayName;
    property Properties: TStrings read GetProperties;
  published
    property DisplayMode: TJvUCBDisplayModes read FDisplayMode write SetDisplayMode default [hkCurrentUser, hkLocalMachine];
    property ShowAll: Boolean read FShowAll write SetShowAll default False;
    property ShowEmptyValues: Boolean read FShowEmptyValues write SetShowEmptyValues default False;
    property Color; 
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted write SetSorted;
    property Style;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;
    property Anchors;
    property Constraints;
  end;

  TJvUninstallListBox = class(TCustomListBox)
  private
    FShowAll: Boolean;
    FDisplayMode: TJvUCBDisplayModes;
    FShowEmptyValues: Boolean;
    function GetItems: TStrings;
    function GetDisplayName: string;
    function GetSection: string;
    function GetUninstallString: string;
    procedure SetShowAll(const Value: Boolean);
    procedure SetShowEmptyValues(const Value: Boolean);
    procedure SetDisplayMode(const Value: TJvUCBDisplayModes);
    function GetProperties: TStrings;
    function GetHKey: HKEY;
    function GetHKeyName: string;
    procedure SetSorted(const Value: boolean);
  protected  
    procedure CreateWidget; override; 
  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    procedure Clear; 
    procedure RefreshItem;
    procedure Rebuild;
    property Items: TStrings read GetItems;
    property Section: string read GetSection;
    property UninstallString: string read GetUninstallString;
    property DisplayName: string read GetDisplayName;
    property Properties: TStrings read GetProperties;
    property HKey: HKEY read GetHKey;
    property HKeyName: string read GetHKeyName;
  published
    property Align;
    property DisplayMode: TJvUCBDisplayModes read FDisplayMode write SetDisplayMode default [hkCurrentUser, hkLocalMachine];
    property ShowAll: Boolean read FShowAll write SetShowAll default False;
    property ShowEmptyValues: Boolean read FShowEmptyValues write SetShowEmptyValues default False;
    property Color; 
    property Enabled;
    property Font;
    property ItemHeight;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted write SetSorted;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;
    property Anchors;
    property Constraints;
  end;

implementation

uses
  Math, Registry;

const
  cUninstallPath = 'Software\Microsoft\Windows\CurrentVersion\Uninstall';
  cUninstallString = 'UninstallString';
  cDisplayName = 'DisplayName';

  FKey: array[TJvUCBDisplayMode] of DWORD = (HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE);

type
  TJvUninstallInfo = class(TObject)
  private
    FSection: string;
    FHKey: HKEY;
    FProperties: TStringList;
    FHKEYName: string;
    function GetProperties: TStrings;
//    procedure SetProperties(const Value: TStrings); make Delphi 5 compiler happy // andreas
  public
    destructor Destroy; override;
    property HKey: HKEY read FHKey write FHKey;
    property HKEYName: string read FHKEYName write FHKEYName;
    property Section: string read FSection write FSection;
    property Properties: TStrings read GetProperties {write SetProperties // make Delphi 5 compiler happy // andreas};
  end;

  TSafeRegIniFile = class(TRegIniFile)
  public
    function ReadString(const Section, Ident, Default: string): string;
  end;

//=== TJvUninstallInfo ====================================================

destructor TJvUninstallInfo.Destroy;
begin
  FProperties.Free;
  inherited Destroy;
end;

function TJvUninstallInfo.GetProperties: TStrings;
begin
  if FProperties = nil then
    FProperties := TStringList.Create;
  Result := FProperties;
end;

{ make Delphi 5 compiler happy // andreas
procedure TJvUninstallInfo.SetProperties(const Value: TStrings);
begin
  if FProperties = nil then
    FProperties := TStringlist.Create;
  FProperties.Assign(Value);
end;}

//=== TSafeRegIniFile ====================================================

function TSafeRegIniFile.ReadString(const Section, Ident, Default: string): string;
var
  Key, OldKey: HKEY;
  Len: Integer;
  RegData: TRegDataType;
  Buffer: array[0..4095] of byte;
  function BufToStr(Buffer: array of byte; BufSize: Integer): string;
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to Min(sizeof(Buffer), BufSize) - 1 do
      Result := Result + ' ' + IntToHex(Buffer[I], 2);
  end;

  function ExpandEnvVar(const S: string): string;
  begin
    SetLength(Result, ExpandEnvironmentStrings(PChar(S), nil, 0));
    ExpandEnvironmentStrings(PChar(S), PChar(Result), Length(Result));
  end;
begin
  Key := GetKey(Section);
  if Key <> 0 then
  try
    OldKey := CurrentKey;
    SetCurrentKey(Key);
    try
      if ValueExists(Ident) then
      begin
        RegData := GetDataType(Ident);
        case RegData of
          rdString, rdExpandString:
            begin
              Len := GetDataSize(Ident);
              if Len > 0 then
              begin
                SetString(Result, nil, Len);
                GetData(Ident, PChar(Result), Len, RegData);
                SetLength(Result, StrLen(PChar(Result)));
                if RegData = rdExpandString then
                  Result := ExpandEnvVar(Result);
              end
              else
                Result := '';
            end;
          rdInteger:
            begin
              GetData(Ident, @Len, sizeof(Len), RegData);
              Result := IntToStr(Len);
            end;
          rdBinary:
            begin
              Len := GetDataSize(Ident);
              if Len > 0 then
              begin
                GetData(Ident, @Buffer, sizeof(Buffer), RegData);
                Result := BufToStr(Buffer, Min(Len, sizeof(buffer)));
              end
              else
                Result := '';
            end;
        end;
      end
      else
        Result := Default;
    finally
      SetCurrentKey(OldKey);
    end;
  finally
    RegCloseKey(Key);
  end
  else
    Result := Default;
end;

procedure GetUninstallApps(DisplayModes: TJvUCBDisplayModes; Strings: TStrings; ShowAll, ShowEmptyValues: Boolean);
var
  I: Integer;
  FFolders, FItems: TStringList;
  Tmp: string;
  Reg: TSafeRegIniFile;
  Dm: TJvUCBDisplayMode;
  UI: TJvUninstallInfo;

  function BufToStr(Buffer: array of byte; BufSize: Integer): string;
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to BufSize - 1 do
      Result := Result + ' ' + IntToHex(Buffer[I], 2);
  end;

  function ExpandEnvVar(const S: string): string;
  begin
    SetLength(Result, ExpandEnvironmentStrings(PChar(S), nil, 0));
    ExpandEnvironmentStrings(PChar(S), PChar(Result), Length(Result));
  end;

  procedure ReadProperties(Reg: TSafeRegIniFile; const Section: string; ShowEmptyValues: boolean; Items, Props: TStrings);
  var
    I: Integer;
    Tmp: string;
  begin
    Reg.OpenKeyReadOnly(Section);
    for I := 0 to Items.Count - 1 do
    begin
      Tmp := Reg.ReadString('', Items[i], '');
      if (Tmp <> '') or ShowEmptyValues then
        Props.Add(Format('%s=%s', [Items[I], Tmp]));
    end;
    Reg.CloseKey;
  end;

  function DMToStr(DM: TJvUCBDisplayMode): string;
  begin
    case DM of
      hkCurrentUser:
        Result := 'HKEY_CURRENT_USER';
      hkLocalMachine:
        Result := 'HKEY_LOCAL_MACHINE';
    end;
  end;
begin
  FFolders := TStringList.Create;
  FItems := TStringList.Create;
  Strings.BeginUpdate;
  Reg := TSafeRegIniFile.Create('');
//  FFolders.Sorted := True;
  with Reg do
  try
    for Dm := Low(FKey) to High(FKey) do
      if Dm in DisplayModes then
      begin
        RootKey := FKey[Dm];
        if OpenKeyReadOnly(cUninstallPath) then
        begin
          ReadSections(FFolders);
          for I := FFolders.Count - 1 downto 0 do
          begin
            Tmp := ReadString(FFolders[I], cDisplayName, '');
            if (Tmp = '') and not ShowAll then
              FFolders.Delete(I)
            else
            begin
              UI := TJvUninstallInfo.Create;
              if Tmp = '' then
                Tmp := FFolders[I];
              UI.HKey := RootKey;
              UI.HKeyName := DMToStr(Dm);
              UI.Section := cUninstallPath + FFolders[I];
              ReadSection(FFolders[I], FItems);
              ReadProperties(Reg, FFolders[I], ShowEmptyValues, FItems, UI.Properties);
              Strings.AddObject(Tmp, UI);
              OpenKeyReadOnly(cUninstallPath);
            end;
          end;
        end;
      end;
  finally
    Free;
    FFolders.Free;
    FItems.Free;
    Strings.EndUpdate;
  end;
end;

//=== TJvUninstallComboBox ===================================================

constructor TJvUninstallComboBox.Create(AComponent: TComponent);
begin
  inherited Create(AComponent);
  Style := csDropDownList;
  FDisplayMode := [hkCurrentUser, hkLocalMachine];
end;

destructor TJvUninstallComboBox.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJvUninstallComboBox.Rebuild;
var
  I: Integer;
begin
  if ([csLoading, csDestroying] * ComponentState) <> [] then
    Exit;
  I := ItemIndex;
  Clear;
  GetUninstallApps(DisplayMode, Items, ShowAll, ShowEmptyValues);
  ItemIndex := I;
  if (Items.Count > 0) and (ItemIndex < 0) then
    ItemIndex := 0;
end;

function TJvUninstallComboBox.GetDisplayName: string;
begin
  if ItemIndex > -1 then
    Result := TJvUninstallInfo(Items.Objects[ItemIndex]).Properties.Values[cDisplayName]
  else
    Result := '';
end;

function TJvUninstallComboBox.GetItems: TStrings;
begin
  Result := inherited Items;
end;

function TJvUninstallComboBox.GetSection: string;
begin
  if ItemIndex > -1 then
    Result := TJvUninstallInfo(Items.Objects[ItemIndex]).Section
  else
    Result := '';
end;

function TJvUninstallComboBox.GetUninstallString: string;
begin
  if ItemIndex > -1 then
    Result := TJvUninstallInfo(Items.Objects[ItemIndex]).Properties.Values[cUninstallString]
  else
    Result := '';
end;

procedure TJvUninstallComboBox.SetDisplayMode(const Value: TJvUCBDisplayModes);
begin
  if FDisplayMode <> Value then
  begin
    FDisplayMode := Value;
    Rebuild;
  end;
end;

procedure TJvUninstallComboBox.SetShowAll(const Value: Boolean);
begin
  if FShowAll <> Value then
  begin
    FShowAll := Value;
    Rebuild;
  end;
end;

procedure TJvUninstallComboBox.SetShowEmptyValues(const Value: Boolean);
begin
  if FShowEmptyValues <> Value then
  begin
    FShowEmptyValues := Value;
    Rebuild;
  end;
end;

procedure TJvUninstallComboBox.Clear;
var
  I: Integer;
begin
  if Parent = nil then
    Exit;
  for I := 0 to Items.Count - 1 do
    Items.Objects[I].Free;
  inherited Clear;
end;




procedure TJvUninstallComboBox.CreateWidget;
begin
  inherited CreateWidget;
  if ItemIndex < 0 then
    Rebuild;
end;


function TJvUninstallComboBox.GetProperties: TStrings;
begin
  Result := nil;
  if ItemIndex > -1 then
    Result := TJvUninstallInfo(Items.Objects[ItemIndex]).Properties;
end;

function TJvUninstallComboBox.GetHKey: HKEY;
begin
  Result := 0;
  if ItemIndex > -1 then
    Result := TJvUninstallInfo(Items.Objects[ItemIndex]).HKey;
end;

function TJvUninstallComboBox.GetHKeyName: string;
begin
  Result := '';
  if ItemIndex > -1 then
    Result := TJvUninstallInfo(Items.Objects[ItemIndex]).HKeyName;
end;

procedure TJvUninstallComboBox.SetSorted(const Value: boolean);
var
  S: string;
begin
  if Value <> inherited Sorted then
  begin
    if ItemIndex > -1 then
      S := Items[ItemIndex]
    else
      S := '';
    inherited Sorted := Value;
    if not Value then
      Rebuild;
    if S <> '' then
      ItemIndex := Items.IndexOf(S);
  end;
end;

//=== TJvUninstallListBox ====================================================

constructor TJvUninstallListBox.Create(AComponent: TComponent);
begin
  inherited Create(AComponent);
  FDisplayMode := [hkCurrentUser, hkLocalMachine];
end;

destructor TJvUninstallListBox.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJvUninstallListBox.GetDisplayName: string;
begin
  if ItemIndex > -1 then
    Result := TJvUninstallInfo(Items.Objects[ItemIndex]).Properties.Values[cDisplayName]
  else
    Result := '';
end;

function TJvUninstallListBox.GetUninstallString: string;
begin
  if ItemIndex > -1 then
    Result := TJvUninstallInfo(Items.Objects[ItemIndex]).Properties.Values[cUninstallString]
  else
    Result := '';
end;

function TJvUninstallListBox.GetItems: TStrings;
begin
  Result := inherited Items;
end;

function TJvUninstallListBox.GetSection: string;
begin
  if ItemIndex > -1 then
    Result := TJvUninstallInfo(Items.Objects[ItemIndex]).Section
  else
    Result := '';
end;

procedure TJvUninstallListBox.SetDisplayMode(const Value: TJvUCBDisplayModes);
begin
  if FDisplayMode <> Value then
  begin
    FDisplayMode := Value;
    Rebuild;
  end;
end;

procedure TJvUninstallListBox.SetShowAll(const Value: Boolean);
begin
  if FShowAll <> Value then
  begin
    FShowAll := Value;
    Rebuild;
  end;
end;

procedure TJvUninstallListBox.SetShowEmptyValues(const Value: Boolean);
begin
  if FShowEmptyValues <> Value then
  begin
    FShowEmptyValues := Value;
    Rebuild;
  end;
end;

procedure TJvUninstallListBox.Clear;
var
  I: Integer;
begin
  if Parent = nil then
    Exit;
  for I := 0 to Items.Count - 1 do
    Items.Objects[I].Free;
  Items.Clear;
  inherited Clear;
end;

procedure TJvUninstallListBox.Rebuild;
var
  I: Integer;
begin
  if ([csLoading, csDestroying] * ComponentState) <> [] then
    Exit;
  I := ItemIndex;
  Clear;
  GetUninstallApps(DisplayMode, Items, ShowAll, ShowEmptyValues);
  ItemIndex := I;
  if (Items.Count > 0) and (ItemIndex < 0) then
    ItemIndex := 0;
end;




procedure TJvUninstallListBox.CreateWidget;
begin
  inherited CreateWidget;
  if ItemIndex < 0 then
    Rebuild;
end;


function TJvUninstallListBox.GetProperties: TStrings;
begin
  Result := nil;
  if ItemIndex > -1 then
    Result := TJvUninstallInfo(Items.Objects[ItemIndex]).Properties;
end;

function TJvUninstallListBox.GetHKey: HKEY;
begin
  Result := 0;
  if ItemIndex > -1 then
    Result := TJvUninstallInfo(Items.Objects[ItemIndex]).HKey;
end;

function TJvUninstallListBox.GetHKeyName: string;
begin
  Result := '';
  if ItemIndex > -1 then
    Result := TJvUninstallInfo(Items.Objects[ItemIndex]).HKeyName;
end;

procedure TJvUninstallListBox.SetSorted(const Value: boolean);
var
  S: string;
begin
  if Value <> inherited Sorted then
  begin
    if ItemIndex > -1 then
      S := Items[ItemIndex]
    else
      S := '';
    inherited Sorted := Value;
    if not Value then
      Rebuild;
    if S <> '' then
      ItemIndex := Items.IndexOf(S);
  end;
end;

procedure TJvUninstallComboBox.RefreshItem;
begin
  Click;
  Change;
end;

procedure TJvUninstallListBox.RefreshItem;
begin
  Click;
end;

end.

