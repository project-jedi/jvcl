{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPerfMon95.PAS, released Dec 26, 1999.

The Initial Developer of the Original Code is Petr Vones (petr dott v att mujmail dott cz)
Portions created by Petr Vones are Copyright (C) 1999 Petr Vones.
Portions created by Microsoft are Copyright (C) 1998, 1999 Microsoft Corp.
All Rights Reserved.

Contributor(s): ______________________________________.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvPerfMon95;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  SysUtils, Registry, Classes, Contnrs,
  {$IFDEF VCL}
  Windows, Forms,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Windows, QForms,
  {$ENDIF VisualCLX}
  JclBase, JvComponent;

type
  EJvPerfStatException = class(EJclError);

  TJvPerfStat95 = class;

  PJvPerfStatCategory = ^TJvPerfStatCategory;
  TJvPerfStatCategory = record
    Category: string; // MBCS
    Name: string; // MBCS
  end;

  TJvPerfStatActiveItem = class(TObject)
  private
    FCategoryIndex: Integer;
    FDescription: string;
    FDifferentiate: Boolean;
    FKey: string; // MBCS
    FLastPerfData: Longword;
    FLastTime: Longword;
    FName: string;
    FOwner: TJvPerfStat95;
    FStarted: Boolean;
    function GetCategory: TJvPerfStatCategory;
    function GetPerfData: Longword;
    function GetPerfDataStr: string;
    function GetKey: string;
  protected
    function InternalStartStop(Start: Boolean): Boolean;
  public
    constructor Create(AOwner: TJvPerfStat95; const AKey: string;
      ACategoryIndex: Integer);
    procedure Start(NoCheckState: Boolean = False);
    procedure Stop(NoCheckState: Boolean = False);
    property Category: TJvPerfStatCategory read GetCategory;
    property Description: string read FDescription;
    property Differentiate: Boolean read FDifferentiate;
    property Key: string read GetKey;
    property Name: string read FName;
    property PerfData: Longword read GetPerfData;
    property PerfDataStr: string read GetPerfDataStr;
    property Started: Boolean read FStarted;
  end;

  TJvPerfStatItem = class(TCollectionItem)
  private
    FActiveItem: TJvPerfStatActiveItem;
    FPerfStatKey: string;
    procedure SetPerfStatKey(const Value: string);
    function GetActiveItem: TJvPerfStatActiveItem;
    function GetExist: Boolean;
  protected
    function GetDisplayName: string; override;
  public
    property ActiveItem: TJvPerfStatActiveItem read GetActiveItem;
    property Exist: Boolean read GetExist;
  published
    property PerfStatKey: string read FPerfStatKey write SetPerfStatKey;
  end;

  TJvPerfStatItems = class(TCollection)
  private
    FOwner: TJvPerfStat95;
    function GetItem(Index: Integer): TJvPerfStatItem;
    procedure SetItem(Index: Integer; const Value: TJvPerfStatItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TJvPerfStat95);
    function Add: TJvPerfStatItem;
    property Items[Index: Integer]: TJvPerfStatItem read GetItem write SetItem; default;
  end;

  TJvPerfStat95 = class(TJvComponent)
  private
    FCategories: array of TJvPerfStatCategory;
    FActiveObjectsList: TObjectList;
    FItems: TJvPerfStatItems;
    FWarnIfWrongOS: Boolean;
    function GetActiveObjects(Index: Integer): TJvPerfStatActiveItem;
    function GetActiveObjectCount: Integer;
    function GetCategories(Index: Integer): TJvPerfStatCategory;
    function GetCategoryCount: Integer;
    function GetKeys(const Name: string): TJvPerfStatActiveItem;
    procedure SetItems(const Value: TJvPerfStatItems);
  protected
    Reg: TRegistry;
    procedure Loaded; override;
    procedure ReadActiveObjects;
    function ReadMBStringValue(const Name: string): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StopAll;
    property ActiveObjects[Index: Integer]: TJvPerfStatActiveItem read GetActiveObjects; default;
    property ActiveObjectCount: Integer read GetActiveObjectCount;
    property Categories[Index: Integer]: TJvPerfStatCategory read GetCategories;
    property CategoryCount: Integer read GetCategoryCount;
    property Keys[const Name: string]: TJvPerfStatActiveItem read GetKeys;
  published
    property Items: TJvPerfStatItems read FItems write SetItems;
    property WarnIfWrongOS: Boolean read FWarnIfWrongOS write FWarnIfWrongOS default True;
  end;

function JvGetPerfStatItems(List: TStrings): Boolean;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF VCL}
  Consts,
  {$ENDIF VCL}
  {$IFDEF HAS_UNIT_RTLCONSTS}
  RTLConsts,
  {$ENDIF HAS_UNIT_RTLCONSTS}
  JclSysInfo,
  JvJCLUtils, JvJVCLUtils, JvResources;

const
  PerfEnumKey = 'System\CurrentControlSet\Control\PerfStats\Enum';
  StartDataKey = 'PerfStats\StartStat';
  StopDataKey = 'PerfStats\StopStat';
  StatDataKey = 'PerfStats\StatData';

var
  WrongOSWarningShown: Boolean = False;

function MultiByteStringToString(const S: string): string;
var
  W: array [0..MAX_PATH] of WideChar;
begin
  OSCheck(MultiByteToWideChar(CP_OEMCP, 0, PChar(S), -1, W, MAX_PATH) <> 0);
  Result := W;
end;

function StringToMultiByteString(const S: string): string;
var
  W: WideString;
  C: array [0..MAX_PATH] of AnsiChar;
begin
  W := S;
  OSCheck(WideCharToMultiByte(CP_OEMCP, 0, PWideChar(W), -1, C, MAX_PATH, nil, nil) <> 0);
  Result := C;
end;

function JvGetPerfStatItems(List: TStrings): Boolean;
var
  List1, List2: TStringList;
  Reg: TRegistry;
  I1, I2: Integer;
begin
  Result := True;
  Reg := TRegistry.Create;
  List1 := TStringList.Create;
  List2 := TStringList.Create;
  List.BeginUpdate;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly(PerfEnumKey) then
    begin
      Reg.GetKeyNames(List1);
      Reg.CloseKey;
      List1.Sort;
      for I1 := 0 to List1.Count - 1 do
        if Reg.OpenKeyReadOnly(PerfEnumKey + '\' + List1[I1]) then
        begin
          Reg.GetKeyNames(List2);
          Reg.CloseKey;
          for I2 := 0 to List2.Count - 1 do
            List.Add(MultiByteStringToString(Format('%s\%s', [List1[I1], List2[I2]])));
        end;
    end
    else
      Result := False;
  finally
    Reg.Free;
    List2.Free;
    List1.Free;
    List.EndUpdate;
  end;
end;

procedure ShowWrongOSWarning;
begin
  if WrongOSWarningShown or (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) then
    Exit;
  with Application do
    {$IFDEF VCL}
    MessageBox(PChar(RsWrongOS), PChar(Title), MB_ICONWARNING);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    MessageBox(RsWrongOS, Title, [smbOK], smsWarning);
    {$ENDIF VisualCLX}
  WrongOSWarningShown := True;
end;

//=== { TJvPerfStatActiveItem } ==============================================

constructor TJvPerfStatActiveItem.Create(AOwner: TJvPerfStat95;
  const AKey: string; ACategoryIndex: Integer);
begin
  inherited Create;
  FOwner := AOwner;
  FKey := AKey;
  FCategoryIndex := ACategoryIndex;
  with FOwner do
    if Reg.OpenKeyReadOnly(PerfEnumKey + '\' + FKey) then
    try
      FDifferentiate := (ReadMBStringValue('Differentiate') = 'TRUE');
      FDescription := ReadMBStringValue('Description');
      FName := ReadMBStringValue('Name');
    finally
      Reg.CloseKey;
    end;
end;

function TJvPerfStatActiveItem.GetCategory: TJvPerfStatCategory;
begin
  Result := FOwner.GetCategories(FCategoryIndex);
end;

function TJvPerfStatActiveItem.GetKey: string;
begin
  Result := MultiByteStringToString(FKey);
end;

function TJvPerfStatActiveItem.GetPerfData: Longword;
var
  Size: Integer;
  Value: Longword;
  CurrentTickCount: DWORD;
begin
  with FOwner.Reg do
  begin
    RootKey := HKEY_DYN_DATA;
    if OpenKeyReadOnly(StatDataKey) then
    begin
      Size := GetDataSize(FKey);
      if Size = SizeOf(Value) then
        ReadBinaryData(FKey, Value, Size);
      CloseKey;
    end;
  end;
  if FDifferentiate then
  begin
    CurrentTickCount := GetTickCount;
    if (FLastTime = 0) or (CurrentTickCount = FLastTime) then
      Result := 0
    else
      Result := Trunc((Value - FLastPerfData) * 1000 / (CurrentTickCount - FLastTime));
    FLastTime := CurrentTickCount;
    FLastPerfData := Value;
  end
  else
    Result := Value;
end;

function TJvPerfStatActiveItem.GetPerfDataStr: string;
var
  E: Extended;
begin
  E := GetPerfData;
  Result := Format('%.n', [E]);
end;

function TJvPerfStatActiveItem.InternalStartStop(Start: Boolean): Boolean;
const
  StartStopKeys: array [Boolean] of string = (StopDataKey, StartDataKey);
var
  Size, Dummy: Integer;
begin
  Result := False;
  with FOwner.Reg do
  begin
    RootKey := HKEY_DYN_DATA;
    if OpenKeyReadOnly(StartStopKeys[Start]) then
    begin
      Size := GetDataSize(FKey);
      if Size = SizeOf(Dummy) then
      begin
        ReadBinaryData(FKey, Dummy, Size);
        Result := True;
      end;
      CloseKey;
    end;
  end;
end;

procedure TJvPerfStatActiveItem.Start(NoCheckState: Boolean);
begin
  if not NoCheckState and FStarted then
    Exit;
  if not InternalStartStop(True) then
    raise EJvPerfStatException.CreateResFmt(@RsECantStart, [Key]);
  FStarted := True;
end;

procedure TJvPerfStatActiveItem.Stop(NoCheckState: Boolean);
begin
  if not NoCheckState and not FStarted then
    Exit;
  if not InternalStartStop(False) then
    raise EJvPerfStatException.CreateResFmt(@RsECantStop, [Key]);
  FStarted := False;
end;

//=== { TJvPerfStatItem } ====================================================

function TJvPerfStatItem.GetActiveItem: TJvPerfStatActiveItem;
begin
  Result := FActiveItem;
  if Result = nil then
    raise EJvPerfStatException.CreateResFmt(@RsEKeyNotExist, [FPerfStatKey]);
  Result.Start;
end;

function TJvPerfStatItem.GetDisplayName: string;
begin
  Result := FPerfStatKey;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TJvPerfStatItem.GetExist: Boolean;
begin
  Result := (FActiveItem <> nil);
end;

procedure TJvPerfStatItem.SetPerfStatKey(const Value: string);
begin
  if FPerfStatKey <> Value then
  begin
    FPerfStatKey := Value;
    Changed(False);
  end;
end;

//=== { TJvPerfStatItems } ===================================================

constructor TJvPerfStatItems.Create(AOwner: TJvPerfStat95);
begin
  inherited Create(TJvPerfStatItem);
  FOwner := AOwner;
end;

function TJvPerfStatItems.Add: TJvPerfStatItem;
begin
  Result := TJvPerfStatItem(inherited Add);
end;

function TJvPerfStatItems.GetItem(Index: Integer): TJvPerfStatItem;
begin
  Result := TJvPerfStatItem(inherited GetItem(Index));
end;

function TJvPerfStatItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TJvPerfStatItems.SetItem(Index: Integer; const Value: TJvPerfStatItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TJvPerfStatItems.Update(Item: TCollectionItem);
var
  I: Integer;

  procedure BindItem(Item: TCollectionItem);
  begin
    with TJvPerfStatItem(Item) do
      FActiveItem := FOwner.Keys[PerfStatKey];
  end;

begin
  if csDesigning in FOwner.ComponentState then
    Exit;
  if Item = nil then
    for I := 0 to Count - 1 do
      BindItem(Items[I])
  else
    BindItem(Item);
end;

//=== { TJvPerfStat95 } ======================================================

constructor TJvPerfStat95.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TJvPerfStatItems.Create(Self);
  FWarnIfWrongOS := True;
  if not (csDesigning in ComponentState) then
  begin
    FActiveObjectsList := TObjectList.Create(True);
    Reg := TRegistry.Create;
    ReadActiveObjects;
  end;
end;

destructor TJvPerfStat95.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    StopAll;
    FreeAndNil(FActiveObjectsList);
    FreeAndNil(Reg);
  end;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TJvPerfStat95.GetActiveObjectCount: Integer;
begin
  Result := FActiveObjectsList.Count;
end;

function TJvPerfStat95.GetActiveObjects(Index: Integer): TJvPerfStatActiveItem;
begin
  Result := TJvPerfStatActiveItem(FActiveObjectsList.Items[Index]);
end;

function TJvPerfStat95.GetCategories(Index: Integer): TJvPerfStatCategory;
begin
  if (Index < 0) or (Index > GetCategoryCount - 1) then
    raise EJvPerfStatException.CreateResFmt(@SListIndexError, [Index]);
  Result := FCategories[Index];
  with Result do
  begin
    Category := MultiByteStringToString(Category);
    Name := MultiByteStringToString(Name);
  end;
end;

function TJvPerfStat95.GetCategoryCount: Integer;
begin
  Result := Length(FCategories);
end;

function TJvPerfStat95.GetKeys(const Name: string): TJvPerfStatActiveItem;
var
  I: Integer;
  FindKey: string;
begin
  FindKey := StringToMultiByteString(Name);
  Result := nil;
  for I := 0 to FActiveObjectsList.Count - 1 do
    if ActiveObjects[I].FKey = FindKey then
    begin
      Result := ActiveObjects[I];
      Break;
    end;
end;

procedure TJvPerfStat95.Loaded;
begin
  inherited Loaded;
  if FWarnIfWrongOS and not (csDesigning in ComponentState) then
    ShowWrongOSWarning;
end;

procedure TJvPerfStat95.ReadActiveObjects;
var
  List1, List2: TStringList;
  I1, I2: Integer;
begin
  List1 := TStringList.Create;
  List2 := TStringList.Create;
  try
    FActiveObjectsList.Clear;
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly(PerfEnumKey) then
    begin
      Reg.GetKeyNames(List1);
      Reg.CloseKey;
      List1.Sort;
      SetLength(FCategories, List1.Count);
      for I1 := 0 to List1.Count - 1 do
        if Reg.OpenKeyReadOnly(PerfEnumKey + '\' + List1[I1]) then
        begin
          Reg.GetKeyNames(List2);
          FCategories[I1].Category := List1[I1];
          FCategories[I1].Name := Reg.ReadString('Name');
          Reg.CloseKey;
          for I2 := 0 to List2.Count - 1 do
            FActiveObjectsList.Add(TJvPerfStatActiveItem.Create(Self,
              Format('%s\%s', [List1[I1], List2[I2]]), I1));
        end;
    end
    else
      raise EJvPerfStatException.CreateRes(@RsECantOpenPerfKey);
  finally
    List2.Free;
    List1.Free;
  end;
end;

function TJvPerfStat95.ReadMBStringValue(const Name: string): string;
begin
  Result := MultiByteStringToString(Reg.ReadString(Name));
end;

procedure TJvPerfStat95.SetItems(const Value: TJvPerfStatItems);
begin
  FItems.Assign(Value);
end;

procedure TJvPerfStat95.StopAll;
var
  I: Integer;
begin
  for I := 0 to FActiveObjectsList.Count - 1 do
    ActiveObjects[I].Stop;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

