{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAppInfo.PAS, released on 2002-05-26.

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

{ A class that functions as a base class for saving / loading arbitrary application
  data to the registry or an inifile. }

unit JvAppInfo;

interface

uses
  Windows, Classes, SysUtils;

type
  TJvAppInfo = class(TPersistent)
  private
    FUseRegistry: Boolean;
    FRegKey: DWORD;
    FSavePath: string;
    FSection: string;
    FUnAssigned: string;
    procedure CheckPath;
    function LoadIni: Boolean;
    function LoadRegistry: Boolean;
    function SaveIni: Boolean;
    function SaveRegistry: Boolean;
  public
    constructor Create;
    procedure Assign(Source:TPersistent); override;
    function Save:Boolean; virtual;
    function Load:Boolean; virtual;
    property SavePath: string read FSavePath write FSavePath;
    //  If set to True, SavePath is interpreted as a registry path
    property UseRegistry: Boolean read FUseRegistry write FUseRegistry;
    property RegRootKey: DWORD read FRegKey write FRegKey;
    property Section: string read FSection write FSection;
    property UnAssignedValue: string read FUnAssigned write FUnAssigned;
  end;

implementation

uses
  IniFiles, Registry, TypInfo,
  JvTypes;

const
  cInvalidPropertyFmt = 'Invalid property: %s';

function TJvAppInfo.LoadRegistry: Boolean;
var
  I: Integer;
  PropList: TPropList;
  Reg: TRegIniFile;
  Value: string;
begin
  Result := False;
  CheckPath;
  Reg := TRegIniFile.Create('');
  I := 0;
  try
    Reg.RootKey := RegRootKey;
    if not Reg.OpenKey(SavePath, False) then
      Exit;
    if GetPropList(ClassInfo, tkProperties, @PropList) > 0 then
      while Assigned(PropList[I]) and (I < High(PropList)) do
      begin
        Value := Reg.ReadString(Section, PropList[I].Name, FUnAssigned);
        if Value <> FUnAssigned then
          case PropList[I].PropType^.Kind of
            tkInteger, tkEnumeration:
              SetOrdProp(Self, PropList[I], StrToInt(Value));
            tkFloat:
              SetFloatProp(Self, PropList[I], StrToFloat(Value));
            tkString, tkLString:
              SetStrProp(Self, PropList[I], Value);
          else
            raise EJVCLException.CreateFmt(cInvalidPropertyFmt, [PropList[I].Name]);
          end;
        Inc(I);
      end;
  finally
    Reg.Free;
  end;
  Result := True;
end;

function TJvAppInfo.LoadIni: Boolean;
var
  I: Integer;
  PropList: TPropList;
  Ini: TIniFile;
  Value: string;
begin
  CheckPath;
  Ini := TIniFile.Create(SavePath);
  try
    I := 0;
    if GetPropList(ClassInfo, tkProperties, @PropList) > 0 then
      while Assigned(PropList[I]) and (I < High(PropList)) do
      begin
        Value := Ini.ReadString(Section, PropList[i].Name, FUnAssigned);
        if Value <> FUnAssigned then
          case PropList[I].PropType^.Kind of
            tkInteger, tkEnumeration:
              SetOrdProp(Self, PropList[I], StrToInt(Value));
            tkFloat:
              SetFloatProp(Self, PropList[I], StrToFloat(Value));
            tkString,tkLString:
              SetStrProp(Self, PropList[I], Value);
          else
            raise EJVCLException.CreateFmt(cInvalidPropertyFmt, [PropList[I].Name]);
          end;
        Inc(I);
      end;
  finally
    Ini.Free;
  end;
  Result := True;
end;

function TJvAppInfo.SaveRegistry: Boolean;
var
  I: Integer;
  PropList: TPropList;
  Reg: TRegIniFile;
  Value: string;
begin
  Reg := TRegIniFile.Create('');
  try
    Reg.RootKey := RegRootKey;
    Reg.OpenKey(SavePath, True);
    I := 0;
    if GetPropList(ClassInfo, tkProperties, @PropList) > 0 then
      while Assigned(PropList[I]) and (I < High(PropList)) and (PropList[I].Name <> '') do
      begin
        case PropList[I].PropType^.Kind of
          tkInteger, tkEnumeration:
            Value := IntToStr(GetOrdProp(Self, PropList[I]));
          tkFloat:
            Value := FloatToStr(GetFloatProp(Self, PropList[I]));
          tkString, tkLString:
            Value := GetStrProp(Self, PropList[I]);
        else
          raise EJVCLException.CreateFmt(cInvalidPropertyFmt, [PropList[I].Name]);
        end;
        Reg.WriteString(Section, PropList[i].Name, Value);
        Inc(I);
      end;
  finally
    Reg.Free;
  end;
  Result := True;
end;

function TJvAppInfo.SaveIni: Boolean;
var
  I: Integer;
  PropList: TPropList;
  Ini: TIniFile;
  Value: string;
begin
  CheckPath;
  Ini := TIniFile.Create(SavePath);
  I := 0;
  try
    if GetPropList(ClassInfo, tkProperties, @PropList) > 0 then
      while Assigned(PropList[I]) and (I < High(PropList)) and (PropList[I].Name <> '') do
      begin
        case PropList[I].PropType^.Kind of
          tkInteger, tkEnumeration:
            Value := IntToStr(GetOrdProp(Self, PropList[I]));
          tkFloat:
            Value := FloatToStr(GetFloatProp(Self, PropList[I]));
          tkString, tkLString:
            Value := GetStrProp(Self, PropList[I]);
          else
            raise EJVCLException.CreateFmt(cInvalidPropertyFmt, [PropList[I].Name]);
          end;
          Ini.WriteString(Section, PropList[I].Name, Value);
          Inc(I);
      end;
  finally
    Ini.Free;
  end;
  Result := True;
end;

function TJvAppInfo.Load: Boolean;
begin
  CheckPath;
  if UseRegistry then
    Result := LoadRegistry
  else
    Result := LoadIni;
end;

function TJvAppInfo.Save: Boolean;
begin
  CheckPath;
  if UseRegistry then
    Result := SaveRegistry
  else
    Result := SaveIni;
end;

procedure TJvAppInfo.CheckPath;
begin
  if SavePath = '' then
    raise EJVCLException.Create('No path specified');
end;

constructor TJvAppInfo.Create;
begin
  inherited Create;
  FRegKey := HKEY_CURRENT_USER;
  FUnAssigned := '';
end;

procedure TJvAppInfo.Assign(Source: TPersistent);
begin
  if Source is TJvAppInfo then
  begin
    SavePath := TJvAppInfo(Source).SavePath;
    UseRegistry := TJvAppInfo(Source).UseRegistry;
    RegRootKey := TJvAppInfo(Source).RegRootKey;
    Section := TJvAppInfo(Source).Section;
    UnAssignedValue := TJvAppInfo(Source).UnAssignedValue;
    Exit;
  end;
  inherited Assign(Source);
end;

end.



