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

unit JvUninstallControls;

interface

uses
  Windows, SysUtils, Classes,
  {$IFDEF VCL}
  Graphics, Controls, StdCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QStdCtrls,
  {$ENDIF VisualCLX}
  JvComponent, JvTypes;

type
  TJvUCBDisplayMode = (hkCurrentUser, hkLocalMachine); // subset of TJvRegKey
  TJvUCBDisplayModes = set of TJvUCBDisplayMode;

  TJvUninstallComboBox = class(TCustomComboBox)
  private
    FDisplayMode: TJvUCBDisplayModes;
    FShowAll: Boolean;
    function GetItems: TStrings;
    function GetDisplayName: string;
    function GetSection: string;
    function GetUninstallString: string;
    procedure SetShowAll(const Value: Boolean);
    procedure SetDisplayMode(const Value: TJvUCBDisplayModes);
    procedure Rebuild;
    function GetProperties: TStrings;
    function GetHKey: HKEY;
  protected
    {$IFDEF VCL}
    procedure CreateHandle; override;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure CreateWidget; override;
    {$ENDIF VisualCLX}
  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    procedure Clear; {$IFDEF VCL} {$IFDEF COMPILER6_UP} override; {$ENDIF} {$ENDIF}
    property Items: TStrings read GetItems;
    property Section: string read GetSection;
    property HKey: HKEY read GetHKey;
    property UninstallString: string read GetUninstallString;
    property DisplayName: string read GetDisplayName;
    property Properties: TStrings read GetProperties;
  published
    property ShowAll: Boolean read FShowAll write SetShowAll;
    property DisplayMode: TJvUCBDisplayModes read FDisplayMode write SetDisplayMode
      default [hkCurrentUser, hkLocalMachine];
    property Color;
    {$IFDEF VCL}
    property DragCursor;
    property DragMode;
    property ImeMode;
    property ImeName;
    property BiDiMode;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    property DragKind;
    {$ENDIF VCL}
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
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
    function GetItems: TStrings;
    function GetDisplayName: string;
    function GetSection: string;
    function GetUninstallString: string;
    procedure SetShowAll(const Value: Boolean);
    procedure SetDisplayMode(const Value: TJvUCBDisplayModes);
    function GetProperties: TStrings;
    function GetHKey: HKEY;
  protected
    {$IFDEF VCL}
    procedure CreateHandle; override;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure CreateWidget; override;
    {$ENDIF VisualCLX}
  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    procedure Clear; {$IFDEF VCL} {$IFDEF COMPILER6_UP} override; {$ENDIF} {$ENDIF}
    procedure Rebuild;
    property Items: TStrings read GetItems;
    property Section: string read GetSection;
    property UninstallString: string read GetUninstallString;
    property DisplayName: string read GetDisplayName;
    property Properties: TStrings read GetProperties;
    property HKey: HKEY read GetHKey;
  published
    property Align;
    property ShowAll: Boolean read FShowAll write SetShowAll;
    property DisplayMode: TJvUCBDisplayModes read FDisplayMode write SetDisplayMode
      default [hkCurrentUser, hkLocalMachine];
    property Color;
    {$IFDEF VCL}
    property DragCursor;
    property DragMode;
    property ImeMode;
    property ImeName;
    property BiDiMode;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    {$ENDIF VCL}
    property Enabled;
    property Font;
    property ItemHeight;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
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

  FKey: array [TJvUCBDisplayMode] of DWORD =
    (HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE);

type
  TUninstallInfo = class(TObject)
  private
    FSection: string;
    FHKey: HKEY;
    FProperties: TStringList;
    function GetProperties: TStrings;
//    procedure SetProperties(const Value: TStrings); make Delphi 5 compiler happy // andreas
  public
    destructor Destroy; override;
    property HKey: HKEY read FHKey write FHKey;
    property Section: string read FSection write FSection;
    property Properties: TStrings read GetProperties {write SetProperties // make Delphi 5 compiler happy // andreas};
  end;

procedure GetUninstallApps(DisplayModes: TJvUCBDisplayModes; Strings: TStrings; ShowAll: Boolean);
var
  I: Integer;
  FFolders, FItems: TStringList;
  Tmp: string;
  Reg: TRegIniFile;
  Dm: TJvUCBDisplayMode;
  UI: TUninstallInfo;

  function BufToStr(Buffer: array of byte; BufSize: Integer): string;
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to Min(sizeof(Buffer),BufSize) - 1 do
      Result := Result + ' ' + IntToHex(Buffer[I], 2);
  end;

  function ExpandEnvVar(const S: string): string;
  begin
    SetLength(Result, ExpandEnvironmentStrings(PChar(S), nil, 0));
    ExpandEnvironmentStrings(PChar(S), PChar(Result), Length(Result));
  end;

  procedure MakeProps(Reg: TRegIniFile; const Section: string; Items, Props: TStrings);
  var
    I: Integer;
    BufTmp: string;
    BufSize:Cardinal;
    Buffer: array [0..4095] of Byte;
    DValue: Cardinal;
  begin
    Reg.OpenKeyReadOnly(Section);
    for I := 0 to Items.Count - 1 do
    begin
      case Reg.GetDataType(Items[I]) of
        rdString:
          BufTmp := Reg.ReadString('', Items[I], '');
        rdExpandString:
          BufTmp := ExpandEnvVar(Reg.ReadString('', Items[I], ''));
        rdInteger:
          begin
            BufSize := SizeOf(DValue);
            RegQueryValueEx(Reg.CurrentKey, PChar(Items[I]), nil, nil, PByte(@DValue), @BufSize);
            BufTmp := IntToStr(DValue);
          end;
        rdBinary:
          begin
            BufSize := Reg.GetDataSize(Items[I]);
            if BufSize > 0 then
              try
                Reg.ReadBinaryData(Items[I], Buffer, sizeof(Buffer));
                BufTmp := BufToStr(Buffer,Reg.GetDataSize(Items[I]));
              except
                BufTmp := '';
              end;
          end;
      end;
      if BufTmp <> '' then
        Props.Add(Format('%s=%s', [Items[I], BufTmp]));
    end;
    Reg.CloseKey;
  end;

begin
  FFolders := TStringList.Create;
  FItems := TStringList.Create;
  Reg := TRegIniFile.Create('');
//  FFolders.Sorted := True;
  Strings.BeginUpdate;
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
              UI := TUninstallInfo.Create;
              if Tmp = '' then
                Tmp := FFolders[I];
              UI.HKey := RootKey;
              UI.Section := cUninstallPath + FFolders[I];
              ReadSection(FFolders[I], FItems);
              MakeProps(Reg, FFolders[I], FItems, UI.Properties);
              Strings.AddObject(Tmp, UI);
              OpenKeyReadOnly(cUninstallPath);
            end;
          end;
        end;
      end;
  finally
    Free;
    FFolders.Free;
    FITems.Free;
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
  GetUninstallApps(DisplayMode, Items, ShowAll);
  ItemIndex := I;
  if (Items.Count > 0) and (ItemIndex < 0) then
    ItemIndex := 0;
end;

function TJvUninstallComboBox.GetDisplayName: string;
begin
  if ItemIndex > -1 then
    Result := TUninstallInfo(Items.Objects[ItemIndex]).Properties.Values[cDisplayName]
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
    Result := TUninstallInfo(Items.Objects[ItemIndex]).Section
  else
    Result := '';
end;

function TJvUninstallComboBox.GetUninstallString: string;
begin
  if ItemIndex > -1 then
    Result := TUninstallInfo(Items.Objects[ItemIndex]).Properties.Values[cUninstallString]
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

{$IFDEF VCL}
procedure TJvUninstallComboBox.CreateHandle;
begin
  inherited CreateHandle;
  if ItemIndex < 0 then
    Rebuild;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
procedure TJvUninstallComboBox.CreateWidget;
begin
  inherited CreateWidget;
  if ItemIndex < 0 then
    Rebuild;
end;
{$ENDIF VisualCLX}

function TJvUninstallComboBox.GetProperties: TStrings;
begin
  Result := nil;
  if ItemIndex > -1 then
    Result := TUninstallInfo(Items.Objects[ItemIndex]).Properties;
end;

function TJvUninstallComboBox.GetHKey: HKEY;
begin
  Result := 0;
  if ItemIndex > -1 then
    Result := TUninstallInfo(Items.Objects[ItemIndex]).HKey;
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
    Result := TUninstallInfo(Items.Objects[ItemIndex]).Properties.Values[cDisplayName]
  else
    Result := '';
end;

function TJvUninstallListBox.GetUninstallString: string;
begin
  if ItemIndex > -1 then
    Result := TUninstallInfo(Items.Objects[ItemIndex]).Properties.Values[cUninstallString]
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
    Result := TUninstallInfo(Items.Objects[ItemIndex]).Section
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
  GetUninstallApps(DisplayMode, Items, ShowAll);
  ItemIndex := I;
  if (Items.Count > 0) and (ItemIndex < 0) then
    ItemIndex := 0;
end;


{$IFDEF VCL}
procedure TJvUninstallListBox.CreateHandle;
begin
  inherited CreateHandle;
  if ItemIndex < 0 then
    Rebuild;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
procedure TJvUninstallListBox.CreateWidget;
begin
  inherited CreateWidget;
  if ItemIndex < 0 then
    Rebuild;
end;
{$ENDIF VisualCLX}

function TJvUninstallListBox.GetProperties: TStrings;
begin
  Result := nil;
  if ItemIndex > -1 then
    Result := TUninstallInfo(Items.Objects[ItemIndex]).Properties;
end;

function TJvUninstallListBox.GetHKey: HKEY;
begin
  Result := 0;
  if ItemIndex > -1 then
    Result := TUninstallInfo(Items.Objects[ItemIndex]).HKey;
end;

//=== TUninstallInfo ====================================================

destructor TUninstallInfo.Destroy;
begin
  FProperties.Free;
  inherited Destroy;
end;

function TUninstallInfo.GetProperties: TStrings;
begin
  if FProperties = nil then
    FProperties := TStringList.Create;
  Result := FProperties;
end;

{ make Delphi 5 compiler happy // andreas
procedure TUninstallInfo.SetProperties(const Value: TStrings);
begin
  if FProperties = nil then
    FProperties := TStringlist.Create;
  FProperties.Assign(Value);
end;}

end.

