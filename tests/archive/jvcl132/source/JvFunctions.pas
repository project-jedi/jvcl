{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFunctions.PAS, released on 2001-02-28.

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

unit JvFunctions;

interface

uses
  Windows, Graphics, Messages, SysUtils, ShellApi;

{$IFNDEF DELPHI6_UP}
type
  EOSError = class(EWin32Error);
{$ENDIF}

//Transform an icon to a bitmap
function IconToBitmap(ico: HIcon): TBitmap;
{$EXTERNALSYM IconToBitmap}

//Open an object with the shell (url or something like that)
procedure OpenObject(Value: PChar); overload;
{$EXTERNALSYM OpenObject}
procedure OpenObject(Value: string); overload;
{$EXTERNALSYM OpenObject}

//Raise the last Exception
procedure RaiseLastWin32; overload;
{$EXTERNALSYM RaiseLastWin32}
procedure RaiseLastWin32(Text: string); overload;
{$EXTERNALSYM RaiseLastWin32}
//Raise the last Exception with a small comment from your part
//Same as linux function ;)
procedure PError(Text: string);
{$EXTERNALSYM PError}

//Return the maximum of three integers
function GetMax(i, j, k: Integer): Integer;
{$EXTERNALSYM GetMax}

//Return the minimum of three integers
function GetMin(i, j, k: Integer): Integer;
{$EXTERNALSYM GetMin}

//Convert RGB Values to HSV
procedure RgbToHSV(r, g, b: Integer; var h, s, v: Integer);
{$EXTERNALSYM RgbToHSV}

//Get version of Shell.dll
function GetShellVersion: Integer;
{$EXTERNALSYM GetShellVersion}

{$IFNDEF DELPHI6_UP}
{ D5 compatibility functions }
procedure RaiseLastOSError;
function IncludeTrailingPathDelimiter(const APath: string): string;
{$ENDIF}

implementation

resourcestring
  SWin32Error = 'Win32 Error.  Code: %d.'#10'%s';

var
  ShellVersion: Integer;

{$IFNDEF DELPHI6_UP}

procedure RaiseLastOSError;
begin
  RaiseLastWin32Error;
end;

function IncludeTrailingPathDelimiter(const APath: string): string;
begin
  if (Length(APath) > 0) and (APath[Length(APath)] <> '\') then
    Result := APath + '\'
  else
    Result := APath;
end;
{$ENDIF}

  {*****************************************************}

function IconToBitmap(ico: HIcon): TBitmap;
var
  i: TPicture;
begin
  i := TPicture.Create;
  i.Icon.Handle := ico;
  Result := TBitmap.Create;
  Result.Height := 32;
  Result.Width := 32;
  Result.Canvas.Draw(0, 0, i.icon);
  i.Free;
end;

{*****************************************************}

procedure OpenObject(Value: string);
begin
  OpenObject(PChar(Value));
end;

{*****************************************************}

procedure OpenObject(Value: PChar);
begin
  ShellExecute(0, nil, Value, nil, nil, SW_NORMAL);
end;

{**************************************************}

procedure RaiseLastWin32;
begin
  PError('');
end;

{**************************************************}

procedure RaiseLastWin32(Text: string);
begin
  PError(Text);
end;

{**************************************************}

procedure PError(Text: string);
var
  lastError: Integer;
  st: string;
begin
  lastError := GetLastError;
  if lastError <> 0 then
  begin
    st := Format(SWin32Error, [LastError, SysErrorMessage(LastError)]);
    if (Text <> '') then
      st := Text + ':' + st;

    raise EOSError.Create(st);
  end;
end;

{**************************************************}

function GetMax(i, j, k: Integer): Integer;
begin
  if j > i then
    i := j;
  if k > i then
    i := k;
  Result := i;
end;

{**************************************************}

function GetMin(i, j, k: Integer): Integer;
begin
  if j < i then
    i := j;
  if k < i then
    i := k;
  Result := i;
end;

{**************************************************}

procedure RgbToHSV(r, g, b: Integer; var h, s, v: Integer);
var
  Delta: Integer;
  Min, Max: Integer;
begin
  Min := GetMin(r, g, b);
  Max := GetMax(r, g, b);
  v := Max;
  Delta := Max - Min;
  if Max = 0 then
    s := 0
  else
    s := (255 * Delta) div Max;
  if s = 0 then
    h := 0
  else
  begin
    if r = Max then
      h := (60 * (g - b)) div Delta
    else if g = Max then
      h := 120 + (60 * (b - r)) div Delta
    else
      h := 240 + (60 * (r - g)) div Delta;
    if h < 0 then
      h := h + 360;
  end;
end;

{**************************************************}

function GetShellVersion: Integer;
var
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  if ShellVersion = 0 then
  begin
    InfoSize := GetFileVersionInfoSize('shell32.dll', Wnd);
    if InfoSize <> 0 then
    begin
      GetMem(VerBuf, InfoSize);
      try
        if GetFileVersionInfo('shell32.dll', Wnd, InfoSize, VerBuf) then
          if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
            ShellVersion := FI.dwFileVersionMS;
      finally
        FreeMem(VerBuf);
      end;
    end;
  end;
  Result := ShellVersion;
end;

end.
