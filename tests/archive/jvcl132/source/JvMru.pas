{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMru.PAS, released on 2001-02-28.

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

unit JvMru;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, JvHMru, JvComponent;

type
  TJvHKey = (hkClassesRoot, hkCurrentUser, hkLocalMachine, hkUsers, hkCurrentConfig);
{$EXTERNALSYM TJvHKey}
  TJvDataType = (dtstring, dtBinary);
{$EXTERNALSYM TJvDataType}
  TOnEnumData = procedure(Sender: TObject; Data: Pointer; Size: Integer; Index: Integer) of object;
  TOnEnumText = procedure(Sender: TObject; Value: string; Index: Integer) of object;

  TJvMruList = class(TJvComponent)
  private
    FMax: Integer;
    FSubKey: string;
    FKey: TJvHKey;
    FList: THandle;
    FType: TJvDataType;
    FOnEnumData: TOnEnumData;
    FOnEnumText: TOnEnumText;
    procedure SetKey(const Value: TJvHKey);
    procedure SetMax(const Value: Integer);
    procedure SetSubKey(const Value: string);
    procedure SetType(const Value: TJvDataType);
  protected
    procedure ReCreateList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property RootKey: TJvHKey read FKey write SetKey default hkCurrentUser;
    property SubKey: string read FSubKey write SetSubKey;
    property MaxItems: Integer read FMax write SetMax default 10;
    property DataType: TJvDataType read FType write SetType default dtstring;
    property OnEnumText: TOnEnumText read FOnEnumText write FOnEnumText;
    property OnEnumData: TOnEnumData read FOnEnumData write FOnEnumData;
    function AddString(Value: string): Boolean;
    function AddData(Value: Pointer; Size: Integer): Boolean;
    function GetItemsCount: Integer;
    function EnumItems: Boolean;
    function GetMostRecentItem: Boolean;
    function GetItem(Index: Integer = 0): Boolean;
    function FindString(Value: string): Integer;
    function FindData(Value: Pointer; Size: Integer): Integer;
    function DeleteString(Index: Integer = 0): Boolean;
  end;

  EMruException = class(Exception);

implementation

resourcestring
  RC_ErrorCreatingMRU = 'Unable to create the MRU';

  {**************************************************}

function TJvMruList.AddData(Value: Pointer; Size: Integer): Boolean;
begin
  if FList <> 0 then
    Result := AddMruData(FList, Value, Size) <> -1
  else
    Result := False;
end;

{**************************************************}

function TJvMruList.AddString(Value: string): Boolean;
begin
  if FList <> 0 then
    Result := AddMruString(FList, PChar(Value)) <> -1
  else
    Result := False;
end;

{**************************************************}

constructor TJvMruList.Create(AOwner: TComponent);
begin
  inherited;
  FList := 0;
  FMax := 10;
  FType := dtstring;
  FKey := hkCurrentUser;

  ReCreateList;
end;

{**************************************************}

function TJvMruList.DeleteString(Index: Integer): Boolean;
begin
  if FList <> 0 then
    Result := DelMruString(FList, Index)
  else
    Result := False;
end;

{**************************************************}

destructor TJvMruList.Destroy;
begin
  if FList <> 0 then
    FreeMruList(FList);
  inherited;
end;

{**************************************************}

function TJvMruList.EnumItems: Boolean;
var
  st, st2: string;
  i: Integer;
  p: Pointer;
  index: Integer;
begin
  Result := False;
  if FList = 0 then
    Exit;

  if FType = dtstring then
  begin
    SetLength(st, 256);
    i := 0;
    Index := 0;
    while i <> -1 do
    begin
      i := EnumMruList(FList, Index, PChar(st), 256);
      if i <> -1 then
      begin
        Result := True;
        if Assigned(FOnEnumText) then
        begin
          st2 := st;
          SetLength(st2, i);
          FOnEnumText(Self, st2, Index);
        end;
      end;
      Inc(Index);
    end;
  end
  else
  begin
    Index := 0;
    i := 0;
    while i <> -1 do
    begin
      p := AllocMem(1024);
      i := EnumMruList(FList, Index, p, 1024);

      if i = 1024 then
      begin
        FreeMem(p);
        p := AllocMem(64000);
        i := EnumMruList(FList, 0, p, 64000);
      end;

      if i <> -1 then
      begin
        Result := True;
        if Assigned(FonEnumData) then
          FOnEnumData(Self, p, i, Index);
      end;
      Inc(Index);
    end;
  end;
end;

{**************************************************}

function TJvMruList.FindData(Value: Pointer; Size: Integer): Integer;
begin
  if FList <> 0 then
    Result := FindMruData(FList, Value, Size, nil)
  else
    Result := -1;
end;

{**************************************************}

function TJvMruList.FindString(Value: string): Integer;
begin
  if FList <> 0 then
    Result := FindMruString(FList, PChar(Value), nil)
  else
    Result := -1;
end;

{**************************************************}

function TJvMruList.GetItem(Index: Integer): Boolean;
var
  st, st2: string;
  i: Integer;
  p: Pointer;
begin
  Result := False;
  if FList = 0 then
    Exit;

  if FType = dtstring then
  begin
    SetLength(st, 256);
    i := EnumMruList(FList, Index, PChar(st), 256);
    if i <> -1 then
    begin
      Result := True;
      if (Assigned(FOnEnumText)) then
      begin
        st2 := st;
        SetLength(st2, i);
        FOnEnumText(Self, st2, Index);
      end;
    end;
  end
  else
  begin
    p := AllocMem(1024);
    i := EnumMruList(FList, Index, p, 1024);

    if i = 1024 then
    begin
      FreeMem(p);
      p := AllocMem(64000);
      i := EnumMruList(FList, 0, p, 64000);
    end;

    if i <> -1 then
    begin
      Result := True;
      if (Assigned(FonEnumData)) then
      begin
        FOnEnumData(Self, p, i, Index);
      end;
    end;
  end;
end;

{**************************************************}

function TJvMruList.GetItemsCount: Integer;
begin
  if FList <> 0 then
    Result := EnumMruList(FList, -1, nil, 0)
  else
    Result := 0;
end;
{**************************************************}

function TJvMruList.GetMostRecentItem: Boolean;
begin
  Result := GetItem(0);
end;

{**************************************************}

procedure TJvMruList.ReCreateList;
var
  FLst: TMruList;
begin
  if FList <> 0 then
    FreeMruList(FList);

  if FSubKey <> '' then
  begin
    FLst.cbSize := SizeOf(FList);
    FLst.nMaxItems := FMax;
    case FType of
      dtstring:
        begin
          FLst.dwFlags := MRUF_STRING_LIST;
          FLst.lpfnComparestring := nil;
        end;
      dtBinary:
        begin
          FLst.dwFlags := MRUF_BINARY_LIST;
          FLst.lpfnCompareData := nil;
        end;
    end;
    case FKey of
      hkClassesRoot:
        FLst.hKey := HKEY_CLASSES_ROOT;
      hkCurrentUser:
        FLst.hKey := HKEY_CURRENT_USER;
      hkLocalMachine:
        FLst.hKey := HKEY_LOCAL_MACHINE;
      hkUsers:
        FLst.hKey := HKEY_USERS;
      hkCurrentConfig:
        FLst.hKey := HKEY_CURRENT_CONFIG;
    end;
    FLst.lpszSubKey := PChar(FSubKey);

    FList := CreateMruList(@FLst);
    if FList = 0 then
      raise EMruException.Create(RC_ErrorCreatingMRU);
  end;
end;

{**************************************************}

procedure TJvMruList.SetKey(const Value: TJvHKey);
begin
  if Value <> FKey then
  begin
    FKey := Value;
    ReCreateList;
  end;
end;

{**************************************************}

procedure TJvMruList.SetMax(const Value: Integer);
begin
  if Value <> FMax then
  begin
    FMax := Value;
    ReCreateList;
  end;
end;

{**************************************************}

procedure TJvMruList.SetSubKey(const Value: string);
begin
  if Value <> FSubKey then
  begin
    FSubKey := Value;
    ReCreateList;
  end;
end;

{**************************************************}

procedure TJvMruList.SetType(const Value: TJvDataType);
begin
  if Value <> FType then
  begin
    FType := Value;
    ReCreateList;
  end;
end;

end.
