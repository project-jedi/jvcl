{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvUCB.PAS, released on 2002-05-26.

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

unit JvUCB;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls,
  StdCtrls,
  JvComponent, JvTypes;

type
  TJvUCBDisplayMode = (hkCurrentUser, hkLocalMachine);  // subset of TJvRegKey
  TJvUCBDisplayModes = set of TJvUCBDisplayMode;

  TJvUninstallComboBox = class(TCustomComboBox)
  private
    FFolders: TStringList;
    FDisplayMode: TJvUCBDisplayModes;
    FShowAll: Boolean;
    procedure GetUninstallApps;
    function GetItems: TStrings;
    function GetDisplayName: string;
    function GetSection: string;
    function GetUninstallString: string;
    procedure SetShowAll(const Value: Boolean);
    procedure SetDisplayMode(const Value: TJvUCBDisplayModes);
  protected
    procedure CreateWnd; override;
  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    property Items: TStrings read GetItems;
    property Section: string read GetSection;
    property UninstallString: string read GetUninstallString;
    property DisplayName: string read GetDisplayName;
  published
    property ShowAll: Boolean read FShowAll write SetShowAll;
    property DisplayMode: TJvUCBDisplayModes read FDisplayMode write SetDisplayMode
      default [hkCurrentUser, hkLocalMachine];
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
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
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
  end;

  TJvUninstallListBox = class(TCustomListBox)
  private
    FFolders: TStringList;
    FShowAll: Boolean;
    FDisplayMode: TJvUCBDisplayModes;
    procedure GetUninstallApps;
    function GetItems: TStrings;
    function GetDisplayName: string;
    function GetSection: string;
    function GetUninstallString: string;
    procedure SetShowAll(const Value: Boolean);
    procedure SetDisplayMode(const Value: TJvUCBDisplayModes);
  protected
    procedure CreateWnd; override;
  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    property Items: TStrings read GetItems;
    property Section: string read GetSection;
    property UninstallString: string read GetUninstallString;
    property DisplayName: string read GetDisplayName;
  published
    property Align;
    property ShowAll: Boolean read FShowAll write SetShowAll;
    property DisplayMode: TJvUCBDisplayModes read FDisplayMode write SetDisplayMode
      default [hkCurrentUser, hkLocalMachine];
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
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
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
  end;

implementation

uses
  Registry;

const
  cUninstallPath = 'Software\Microsoft\Windows\CurrentVersion\Uninstall';
  FKey: array [TJvUCBDisplayMode] of DWORD =
    (HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE);

function UninstallStr(Reg: TRegIniFile; Section: string): string;
begin
  if Reg.OpenKey('\' + cUninstallPath, False) then
    Result := Reg.ReadString(Section, 'UninstallString', '')
  else
    Result := '';
end;

function DispName(Reg: TRegIniFile; Section: string): string;
begin
  if Reg.OpenKey('\' + cUninstallPath, False) then
    Result := Reg.ReadString(Section, 'DisplayName', '')
  else
    Result := '';
end;

//=== TJvUninstallComboBox ===================================================

constructor TJvUninstallComboBox.Create(AComponent: TComponent);
begin
  inherited Create(AComponent);
  FFolders := TStringList.Create;
  Style := csDropDownList;
  FDisplayMode := [hkCurrentUser, hkLocalMachine];
end;

destructor TJvUninstallComboBox.Destroy;
begin
  FFolders.Free;
  inherited Destroy;
end;

procedure TJvUninstallComboBox.CreateWnd;
begin
  inherited CreateWnd;
  Sorted := True;
  GetUninstallApps;
end;

function TJvUninstallComboBox.GetDisplayName: string;
begin
  Result := '';
  if (ItemIndex < 0) then
    Exit;
  Result := Items[ItemIndex];
end;

function TJvUninstallComboBox.GetItems: TStrings;
begin
  Result := inherited Items;
end;

function TJvUninstallComboBox.GetSection: string;
begin
  Result := '';
  if ItemIndex < 0 then
    Exit;
  Result := cUninstallPath + '\' + FFolders[Integer(Items.Objects[ItemIndex])];
end;

procedure TJvUninstallComboBox.GetUninstallApps;
var
  I, J: Integer;
  S, FTmpFolders: TStringList;
  Tmp: string;
  Reg: TReginifile;
  Dm: TJvUCBDisplayMode;
begin
  S := TStringList.Create;
  FTmpFolders := TStringList.Create;
  FFolders.Clear;
  Reg := TRegIniFile.Create('');
  Items.Clear;
  with Reg do
  try
    for Dm := Low(FKey) to High(FKey) do
      if Dm in FDisplayMode then
      begin
        J := Items.Count;
        RootKey := FKey[Dm];
        if OpenKey(cUninstallPath, False) then
        begin
          ReadSections(FFolders);
          FTmpFolders.AddStrings(FFolders);
          for I := FFolders.Count - 1 downto 0 do
            if (DispName(Reg, FFolders[I]) = '') and not FShowAll then
              FFolders.Delete(I);
          for I := 0 to FFolders.Count - 1 do
          begin
            Tmp := DispName(Reg, FFolders[I]);
            if (Tmp = '') and FShowAll then
              Tmp := FFolders[I];
            S.AddObject(Tmp, TObject(I + J));
          end;
        end;
        // peter3: this shouldn't be called
//        else
//          raise EJVCLException.CreateFmt('Unable to open key %s', [cUninstallPath]);
        Items.AddStrings(S);
        S.Clear;
      end;
  finally
    Free;
    S.Free;
    FFolders.Assign(FTmpFolders);
    FTmpFolders.Free;
  end;
  if Items.Count > 0 then
    ItemIndex := 0;
end;

function TJvUninstallComboBox.GetUninstallString: string;
var
  Reg: TReginifile;
  Dm: TJvUCBDisplayMode;
begin
  Result := '';
  if ItemIndex < 0 then
    Exit;
  Reg := TRegIniFile.Create('');
  with Reg do
  try
    for Dm := Low(FKey) to High(FKey) do
      if Dm in FDisplayMode then
      begin
        RootKey := FKey[Dm];
        Result := FFolders[Integer(Items.Objects[ItemIndex])];
        Result := UninstallStr(Reg, Result);
        if Result <> '' then
          Break;
      end;
  finally
    Free;
  end;
end;

procedure TJvUninstallComboBox.SetDisplayMode(
  const Value: TJvUCBDisplayModes);
begin
  if FDisplayMode <> Value then
  begin
    FDisplayMode := Value;
    RecreateWnd;
  end;
end;

procedure TJvUninstallComboBox.SetShowAll(const Value: Boolean);
begin
  if FShowAll <> Value then
  begin
    FShowAll := Value;
    RecreateWnd;
  end;
end;

//=== TJvUninstallListBox ====================================================

constructor TJvUninstallListBox.Create(AComponent: TComponent);
begin
  inherited Create(AComponent);
  FFolders := TStringList.Create;
  FDisplayMode := [hkCurrentUser, hkLocalMachine];
end;

destructor TJvUninstallListBox.Destroy;
begin
  FFolders.Free;
  inherited Destroy;
end;

procedure TJvUninstallListBox.CreateWnd;
begin
  // (rom) strange place to call it
  inherited CreateWnd;
  Sorted := True;
  GetUninstallApps;
end;

function TJvUninstallListBox.GetDisplayName: string;
begin
  Result := '';
  if ItemIndex < 0 then
    Exit;
  Result := Items[ItemIndex];
end;

function TJvUninstallListBox.GetItems: TStrings;
begin
  Result := inherited Items;
end;

function TJvUninstallListBox.GetSection: string;
begin
  Result := '';
  if ItemIndex < 0 then
    Exit;
  Result := cUninstallPath + '\' + FFolders[Integer(Items.Objects[ItemIndex])];
end;

procedure TJvUninstallListBox.GetUninstallApps;
var
  I, J: Integer;
  S, FTmpFolders: TStringList;
  Tmp: string;
  Reg: TReginifile;
  Dm: TJvUCBDisplayMode;
begin
  S := TStringList.Create;
  FTmpFolders := TStringList.Create;
  FFolders.Clear;
  Reg := TRegIniFile.Create('');
  Items.Clear;
  with Reg do
  try
    for Dm := Low(FKey) to High(FKey) do
      if Dm in FDisplayMode then
      begin
        FFolders.Clear;
        RootKey := FKey[Dm];
        J := Items.Count;
        if OpenKey(cUninstallPath, False) then
        begin
          ReadSections(FFolders);
          FTmpFolders.AddStrings(FFolders);
          for I := FFolders.Count - 1 downto 0 do
            if (DispName(Reg, FFolders[I]) = '') and not FShowAll then
              FFolders.Delete(I);
          for I := 0 to FFolders.Count - 1 do
          begin
            Tmp := DispName(Reg, FFolders[I]);
            if (Tmp = '') and FShowAll then
              Tmp := FFolders[I];
            S.AddObject(Tmp, TObject(I + J));
          end;
          Items.AddStrings(S);
          S.Clear;
        end;
        // peter3: this shouldn't be called
//        else
//          raise EJVCLException.CreateFmt('Unable to open key %s', [cUninstallPath]);
      end;
  finally
    FFolders.Assign(FTmpFolders);
    FTmpFolders.Free;
    Free;
    S.Free;
  end;
end;

function TJvUninstallListBox.GetUninstallString: string;
var
  Reg: TReginifile;
  Dm: TJvUCBDisplayMode;
begin
  Result := '';
  if ItemIndex < 0 then
    Exit;
  Reg := TRegIniFile.Create('');
  with Reg do
  try
    for Dm := Low(FKey) to High(FKey) do
      if Dm in FDisplayMode then
      begin
        RootKey := FKey[Dm];
        Result := FFolders[Integer(Items.Objects[ItemIndex])];
        Result := UninstallStr(Reg, Result);
        if Result <> '' then
          Break;
      end;
  finally
    Free;
  end;
end;

procedure TJvUninstallListBox.SetDisplayMode(const Value: TJvUCBDisplayModes);
begin
  if FDisplayMode <> Value then
  begin
    FDisplayMode := Value;
    RecreateWnd;
  end;
end;

procedure TJvUninstallListBox.SetShowAll(const Value: Boolean);
begin
  if FShowAll <> Value then
  begin
    FShowAll := Value;
    RecreateWnd;
  end;
end;

end.

