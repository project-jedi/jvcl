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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

{A class that functions as a base class for saving / loading arbitrary application
data to the registry or an inifile.
}
unit JvAppInfo;

interface
uses
  Windows, SysUtils, Classes;

type
  TJvAppInfo = class(TPersistent)
  private
    FUseRegistry: boolean;
    FRegKey: DWORD;
    FSavePath: string;
    FSection: string;
    FUnAssigned: string;
    procedure CheckPath;
    function LoadIni: boolean;
    function LoadRegistry: boolean;
    function SaveIni: boolean;
    function SaveRegistry: boolean;
  public
    constructor Create;
    procedure Assign(Source:TPersistent);override;
    function Save:boolean;virtual;
    function Load:boolean;virtual;
    property SavePath:string read FSavePath write FSavePath;
    //  If set to true, SavePath is interpreted as a registry path
    property UseRegistry:boolean read FUseRegistry write FUseRegistry;
    property RegRootKey:DWORD read FRegKey write FRegKey;
    property Section:string read FSection write FSection;
    property UnAssignedValue:string read FUnAssigned write FUnAssigned;
  end;


implementation
uses
  Registry, iniFiles, TypInfo;

{ TJvAppInfo }


function TJvAppInfo.LoadRegistry:boolean;
var i:integer; PropList:TPropList;Reg:TRegIniFile;Value:string;
begin
  Result := false;
  CheckPath;
  Reg := TRegIniFile.Create('');
  i := 0;
  try
    Reg.RootKey := RegRootKey;
    if not Reg.OpenKey(SavePath,false) then Exit;
    if GetPropList(ClassInfo,tkProperties,@PropList) > 0 then
      while Assigned(PropList[i]) and (i < High(PropList)) do
      begin
        Value := Reg.ReadString(Section,PropList[i].Name,FUnAssigned);
        if (Value <> FUnAssigned) then
          case PropList[i].PropType^.Kind of
            tkInteger,tkEnumeration:    SetOrdProp(self,PropList[i],StrToInt(Value));
            tkFloat:                    SetFloatProp(self,PropList[i],StrToFloat(Value));
            tkString,tkLString:         SetStrProp(self,PropList[i],Value);
          else
             raise Exception.CreateFmt('Invalid property: %s',[PropList[i].Name]);
          end;
        Inc(i);
      end;
  finally
    Reg.Free;
  end;
  Result := true;
end;

function TJvAppInfo.LoadIni:boolean;
var i:integer; PropList:TPropList;Ini:TIniFile;Value:string;
begin
  CheckPath;
  Ini := TIniFile.Create(SavePath);
  try
    i := 0;
    if GetPropList(ClassInfo,tkProperties,@PropList) > 0 then
      while Assigned(PropList[i]) and (i < High(PropList)) do
      begin
        Value := Ini.ReadString(Section,PropList[i].Name,FUnAssigned);
        if (Value <> FUnAssigned) then
          case PropList[i].PropType^.Kind of
            tkInteger,tkEnumeration: SetOrdProp(self,PropList[i],StrToInt(Value));
            tkFloat:                 SetFloatProp(self,PropList[i],StrToFloat(Value));
            tkString,tkLString:      SetStrProp(self,PropList[i],Value);
          else
             raise Exception.CreateFmt('Invalid property: %s',[PropList[i].Name]);
          end;
        Inc(i);
      end;
  finally
    Ini.Free;
  end;
  Result := true;
end;

function TJvAppInfo.SaveRegistry:boolean;
var i:integer; PropList:TPropList;Reg:TRegIniFile;Value:string;
begin
  Reg := TRegIniFile.Create('');
  try
    Reg.RootKey := RegRootKey;
    Reg.OpenKey(SavePath,true);
    i := 0;
    if GetPropList(ClassInfo,tkProperties,@PropList) > 0 then
      while Assigned(PropList[i]) and (i < High(PropList)) and (PropList[i].Name <> '') do
      begin
        case PropList[i].PropType^.Kind of
          tkInteger,tkEnumeration:
            Value := IntToStr(GetOrdProp(self,PropList[i]));
          tkFloat:
            Value := FloatToStr(GetFloatProp(self,PropList[i]));
          tkString,tkLString:
            Value := GetStrProp(self,PropList[i]);
        else
          raise Exception.CreateFmt('Invalid property: %s',[PropList[i].Name]);
        end;
        Reg.WriteString(Section,PropList[i].Name,Value);
        Inc(i);
      end;
  finally
    Reg.Free;
  end;
  Result := true;
end;

function TJvAppInfo.SaveIni:boolean;
var i:integer; PropList:TPropList;Ini:TIniFile;Value:string;
begin
  CheckPath;
  Ini := TIniFile.Create(SavePath);
  i := 0;
  try
    if GetPropList(ClassInfo,tkProperties,@PropList) > 0 then
      while Assigned(PropList[i]) and (i < High(PropList)) and (PropList[i].Name <> '') do
      begin
        case PropList[i].PropType^.Kind of
          tkInteger,tkEnumeration:
            Value := IntToStr(GetOrdProp(self,PropList[i]));
          tkFloat:
            Value := FloatToStr(GetFloatProp(self,PropList[i]));
          tkString,tkLString:
            Value := GetStrProp(self,PropList[i]);
          else
             raise Exception.CreateFmt('Invalid property: %s',[PropList[i].Name]);
          end;
          Ini.WriteString(Section,PropList[i].Name,Value);
          Inc(i);
      end;
  finally
    Ini.Free;
  end;
  Result := true;
end;


function TJvAppInfo.Load: boolean;
begin
  CheckPath;
  if UseRegistry then
    Result := LoadRegistry
  else
    Result := LoadIni;
end;

function TJvAppInfo.Save: boolean;
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
    raise Exception.Create('No path specified');
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
    SavePath         := TJvAppInfo(Source).SavePath;
    UseRegistry      := TJvAppInfo(Source).UseRegistry;
    RegRootKey       := TJvAppInfo(Source).RegRootKey;
    Section          := TJvAppInfo(Source).Section;
    UnAssignedValue  := TJvAppInfo(Source).UnAssignedValue;
    Exit;
  end;
  inherited Assign(SOurce);
end;

end.



