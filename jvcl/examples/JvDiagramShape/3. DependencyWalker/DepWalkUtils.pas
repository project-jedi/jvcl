{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit DepWalkUtils;

interface

uses
  Classes, SysUtils, Controls;

function WaitCursor: IUnknown;
function ChangeCursor(NewCursor: TCursor): IUnknown;
procedure SuspendRedraw(AControl: TWinControl; Suspend: boolean);
function YesNo(const ACaption, AMsg: string): boolean;

procedure strTokenize(const S: string; Delims: TSysCharSet; Results: TStrings);
function GetBorlandLibPath(Version: integer; ForDelphi: boolean): string;
function GetExpandedLibRoot(Version: integer; ForDelphi: boolean): string;
procedure GetPathList(Version: integer; ForDelphi: boolean; Strings: TStrings);
procedure GetSystemPaths(Strings: TStrings);
procedure MakeEditNumeric(EditHandle: integer);


implementation
uses
  Windows, Forms, Messages, Registry, JclSysInfo; 

type
  // (p3) class that changes and restores the screen cursor automatically
  TChangeCursor = class(TInterfacedObject)
  private
    FOldCursor: TCursor;
  public
    constructor Create(NewCursor: TCursor);
    destructor Destroy; override;
  end;

{ TChangeCursor }

constructor TChangeCursor.Create(NewCursor: TCursor);
begin
  inherited Create;
  FOldCursor := Screen.Cursor;
  Screen.Cursor := NewCursor;
end;

destructor TChangeCursor.Destroy;
begin
  Screen.Cursor := FOldCursor;
  inherited;
end;

function WaitCursor: IUnknown;
begin
  Result := TChangeCursor.Create(crHourGlass);
end;

function ChangeCursor(NewCursor: TCursor): IUnknown;
begin
  Result := TChangeCursor.Create(NewCursor);
end;

// (3) shows a message box with Yes and No buttons

function YesNo(const ACaption, AMsg: string): boolean;
begin
  Result := MessageBox(GetFocus, PChar(AMsg), PChar(ACaption),
    MB_YESNO or MB_ICONQUESTION or MB_TASKMODAL) = IDYES;
end;

// suspend/resumes the drawing of a TWinControl

procedure SuspendRedraw(AControl: TWinControl; Suspend: boolean);
begin
  AControl.Perform(WM_SETREDRAW, Ord(not Suspend), 0);
  if not Suspend then
    RedrawWindow(AControl.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INTERNALPAINT or RDW_INVALIDATE or RDW_UPDATENOW or RDW_ALLCHILDREN);
end;

procedure strTokenize(const S: string; Delims: TSysCharSet; Results: TStrings);
var I, J: integer; tmp: string;
begin
  I := 1;
  J := 1;
  while true do
  begin
    while (I <= Length(S)) and not (S[i] in Delims) do
      Inc(I);
    tmp := trim(Copy(S, J, I - J));
    if tmp <> '' then
      Results.Add(tmp);
    if (I <= Length(S)) and (S[I] in Delims) then
      Inc(I); // skip the delimiter
    J := I;
    if i > Length(S) then
      Break;
  end;
end;

function GetBorlandLibPath(Version: integer; ForDelphi: boolean): string;
const
  cLibPath: array[boolean] of PChar = ('\Software\Borland\C++Builder\%d.0\Library',
    '\Software\Borland\Delphi\%d.0\Library');
var ALibPath: string;
begin
  ALibPath := Format(cLibPath[ForDelphi], [Version]);
  with TRegistry.Create do // defaults to HKCU - just what we want
  try
    if OpenKeyReadOnly(ALibPath) and ValueExists('Search Path') then
      Result := ReadString('Search Path')
    else
      Result := '';
  finally
    Free;
  end;
end;

function GetExpandedLibRoot(Version: integer; ForDelphi: boolean): string;
const
  cLibPath: array[boolean] of PChar = ('\Software\Borland\C++Builder\%d.0',
    '\Software\Borland\Delphi\%d.0');
var ALibPath: string;
begin
  ALibPath := Format(cLibPath[ForDelphi], [Version]);
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(ALibPath) and ValueExists('RootDir') then
      Result := ReadString('RootDir')
    else
      Result := '';
  finally
    Free;
  end;
end;

procedure GetPathList(Version: integer; ForDelphi: boolean; Strings: TStrings);
const
  cRootDirMacro: array[boolean] of PChar = ('$(BCB)', '$(DELPHI)');
var S, T: string;
begin
  S := GetBorlandLibPath(Version, ForDelphi);
  T := GetExpandedLibRoot(Version, ForDelphi);
  S := StringReplace(S, cRootDirMacro[ForDelphi], T, [rfReplaceAll, rfIgnoreCase]);
  StrTokenize(S, [';'], Strings);
end;

procedure GetSystemPaths(Strings: TStrings);
var S: string;
begin
  JclSysInfo.GetEnvironmentVar('PATH', S, true);
  strTokenize(S, [';'], Strings);
end;

procedure MakeEditNumeric(EditHandle: integer);
begin
  SetWindowLong(EditHandle, GWL_STYLE, GetWindowLong(EditHandle, GWL_STYLE) or ES_NUMBER);
end;

function GetFullNamePath(AComponent:TComponent):string;
begin
  Result := AComponent.GetNamePath;
  while AComponent.Owner <> nil do
  begin
    AComponent := AComponent.Owner;
    Result := AComponent.GetNamePath + '.' + Result;
  end;
  if (Length(Result) > 1) and (Result[1] = '.') then
  begin
    Move(Result[2],Result[1],Length(Result));
    SetLength(Result,Length(Result)-1);
  end;
end;

end.

