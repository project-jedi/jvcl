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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvUCB;


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvComponent, JvCtrls;

type
  TJvUCBDisplayMode = (dmCurrentUser, dmLocalMachine);
  TJvUCBDisplayModes = set of TJvUCBDisplayMode;

  TJvUninstallComboBox = class(TJvCustomComboBox)
  private
    FFolders: TStringList;
    FDisplayMode: TJvUCBDisplayModes;
    FShowAll: boolean;
    procedure GetUninstallApps;
    function GetItems: TStrings;
    function GetDisplayName: string;
    function GetSection: string;
    function GetUninstallString: string;
    procedure SetShowAll(const Value: boolean);
    procedure SetDisplayMode(const Value: TJvUCBDisplayModes);
    { Private declarations }
  protected
    { Protected declarations }
    procedure CreateWnd; override;
  public
    { Public declarations }
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    property Items: TStrings read GetItems;
    property Section: string read GetSection;
    property UninstallString: string read GetUninstallString;
    property DisplayName: string read GetDisplayName;
  published
    { Published declarations }

    property ShowAll: boolean read FShowAll write SetShowAll;
    property DisplayMode: TJvUCBDisplayModes read FDisplayMode write SetDisplayMode default [dmCurrentUser, dmLocalMachine];

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

  TJvUninstallListBox = class(TJvCustomListBox)
  private
    FFolders: TStringList;
    FShowAll: boolean;
    FDisplayMode: TJvUCBDisplayModes;
    procedure GetUninstallApps;
    function GetItems: TStrings;
    function GetDisplayName: string;
    function GetSection: string;
    function GetUninstallString: string;
    procedure SetShowAll(const Value: boolean);
    procedure SetDisplayMode(const Value: TJvUCBDisplayModes);
    { Private declarations }
  protected
    { Protected declarations }
    procedure CreateWnd; override;
  public
    { Public declarations }
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    property Items: TStrings read GetItems;
    property Section: string read GetSection;
    property UninstallString: string read GetUninstallString;
    property DisplayName: string read GetDisplayName;
  published
    { Published declarations }
    property Align;
    property ShowAll: boolean read FShowAll write SetShowAll;
    property DisplayMode: TJvUCBDisplayModes read FDisplayMode write SetDisplayMode default [dmCurrentUser, dmLocalMachine];
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
  Registry, ShellAPI;

const
  cUninstallPath = 'Software\Microsoft\Windows\CurrentVersion\Uninstall';
  FKey: array[TJvUCBDisplayMode] of DWORD = (HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE);

function UninstallStr(Reg: TRegIniFile; Section: string): string;
begin
  if reg.OpenKey('\' + cUninstallPath, false) then
    Result := reg.ReadString(Section, 'UninstallString', '')
  else
    Result := '';
end;

function DispName(Reg: TRegIniFile; Section: string): string;
begin
  if reg.OpenKey('\' + cUninstallPath, false) then
    Result := reg.ReadString(Section, 'DisplayName', '')
  else
    Result := '';
end;

{ TJvUninstallComboBox }

constructor TJvUninstallComboBox.Create(AComponent: TComponent);
begin
  inherited Create(AComponent);
  FFolders := TStringlist.Create;
  Style := csDropDownList;
  FDisplayMode := [dmCurrentUser, dmLocalMachine];
end;

procedure TJvUninstallComboBox.CreateWnd;
begin
  inherited CreateWnd;
  Sorted := true;
  GetUninstallApps;
end;

destructor TJvUninstallComboBox.Destroy;
begin
  FFolders.Free;
  inherited Destroy;
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
  Result := cUninstallPath + '\' + FFolders[integer(Items.Objects[ItemIndex])];
end;

procedure TJvUninstallComboBox.GetUninstallApps;
var i, j: integer; S, FTmpFolders: TStringlist; tmp: string; reg: TReginifile;
  dm: TJvUCBDisplayMode;
begin
  S := TStringlist.Create;
  FTmpFolders := TStringlist.Create;
  FFolders.Clear;
  reg := TRegIniFile.Create('');
  Items.Clear;
  with reg do
  try
    for dm := Low(FKey) to High(FKey) do
      if dm in FDisplayMode then
      begin
        j := Items.Count;
        RootKey := FKey[dm];
        if OpenKey(cUninstallPath, false) then
        begin
          ReadSections(FFolders);
          FTmpFolders.AddStrings(FFolders);
          for i := FFolders.Count - 1 downto 0 do
            if (DispName(reg, FFolders[i]) = '') and not FShowAll then
              FFolders.Delete(i);
          for i := 0 to FFolders.Count - 1 do
          begin
            tmp := DispName(reg, FFolders[i]);
            if (tmp = '') and FShowAll then
              tmp := FFolders[i];
            S.AddObject(tmp, TObject(i + j));
          end;
        end;
        // peter3: this shouldn't be called
//        else
//          raise Exception.CreateFmt('Unable to open key %s', [cUninstallPath]);
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
var reg: TReginifile; dm: TJvUCBDisplayMode;
begin
  Result := '';
  if ItemIndex < 0 then
    Exit;
  reg := TRegIniFile.Create('');
  with reg do
  try
    for dm := Low(FKey) to High(FKey) do
      if dm in FDisplayMode then
      begin
        RootKey := FKey[dm];
        Result := FFolders[integer(Items.Objects[ItemIndex])];
        Result := UninstallStr(reg, Result);
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

procedure TJvUninstallComboBox.SetShowAll(const Value: boolean);
begin
  if FShowAll <> Value then
  begin
    FShowAll := Value;
    RecreateWnd;
  end;
end;

{ TJvUninstallListBox }

constructor TJvUninstallListBox.Create(AComponent: TComponent);
begin
  inherited Create(AComponent);
  FFolders := TStringlist.Create;
  FDisplayMode := [dmCurrentUser, dmLocalMachine];
end;

procedure TJvUninstallListBox.CreateWnd;
begin
  inherited CreateWnd;
  Sorted := true;
  GetUninstallApps;
end;

destructor TJvUninstallListBox.Destroy;
begin
  FFolders.Free;
  inherited Destroy;
end;

function TJvUninstallListBox.GetDisplayName: string;
begin
  Result := '';
  if (ItemIndex < 0) then
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
  Result := cUninstallPath + '\' + FFolders[integer(Items.Objects[ItemIndex])];
end;

procedure TJvUninstallListBox.GetUninstallApps;
var i, j: integer; S, FTmpFolders: TStringlist; tmp: string; reg: TReginifile; dm: TJvUCBDisplayMode;
begin
  S := TStringlist.Create;
  FTmpFolders := TStringlist.Create;
  FFolders.Clear;
  reg := TRegIniFile.Create('');
  Items.Clear;
  with reg do
  try
    for dm := Low(FKey) to High(FKey) do
      if dm in FDisplayMode then
      begin
        FFolders.Clear;
        RootKey := FKey[dm];
        j := Items.Count;
        if OpenKey(cUninstallPath, false) then
        begin
          ReadSections(FFolders);
          FTmpFolders.AddStrings(FFolders);
          for i := FFolders.Count - 1 downto 0 do
            if (DispName(reg, FFolders[i]) = '') and not FShowAll then
              FFolders.Delete(i);
          for i := 0 to FFolders.Count - 1 do
          begin
            tmp := DispName(reg, FFolders[i]);
            if (tmp = '') and FShowAll then
              tmp := FFolders[i];
            S.AddObject(tmp, TObject(i + j));
          end;
          Items.AddStrings(S);
          S.Clear;
        end;
        // peter3: this shouldn't be called
//        else
//          raise Exception.CreateFmt('Unable to open key %s', [cUninstallPath]);
      end;
  finally
    FFolders.Assign(FTmpFolders);
    FTmpFolders.Free;
    Free;
    S.Free;
  end;
end;

function TJvUninstallListBox.GetUninstallString: string;
var reg: TReginifile; dm: TJvUCBDisplayMode;
begin
  Result := '';
  if ItemIndex < 0 then
    Exit;
  reg := TRegIniFile.Create('');
  with reg do
  try
    for dm := Low(FKey) to High(FKey) do
      if dm in FDisplayMode then
      begin
        RootKey := FKey[dm];
        Result := FFolders[integer(Items.Objects[ItemIndex])];
        Result := UninstallStr(reg, Result);
        if Result <> '' then
          Break;
      end;
  finally
    Free;
  end;
end;

procedure TJvUninstallListBox.SetDisplayMode(
  const Value: TJvUCBDisplayModes);
begin
  if FDisplayMode <> Value then
  begin
    FDisplayMode := Value;
    RecreateWnd;
  end;
end;

procedure TJvUninstallListBox.SetShowAll(const Value: boolean);
begin
  if FShowAll <> Value then
  begin
    FShowAll := Value;
    RecreateWnd;
  end;
end;

end.

