{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDirectories.PAS, released on 2001-02-28.

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

unit JvDirectories;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Registry, JvTypes, JvComponent;

type
  TJvDirectories = class(TJvComponent)
  private
    FBidon: string;
    buf: array[0..250] of Char;
    function GetCurrent: string;
    function GetWindowsDirectory: string;
    function GetSystemDirectory: string;
    function GetTempPath: string;
    function GetValueAtIndex(Index: Integer): string;
    function GetProgramFiles: string;
  protected
    function CheckLastChar(Value: string): string;
  public
  published
    property CurrentDirectory: string read GetCurrent write FBidon stored False;
    property WindowsDirectory: string read GetWindowsDirectory write FBidon stored False;
    property SystemDirectory: string read GetSystemDirectory write FBidon stored False;
    property TempPath: string read GetTempPath write Fbidon stored False;
    property ApplicationData: string index 0 read GetValueAtIndex write FBidon stored False;
    property Cache: string index 1 read GetValueAtIndex write FBidon stored False;
    property Cookies: string index 2 read GetValueAtIndex write FBidon stored False;
    property Desktop: string index 3 read GetValueAtIndex write FBidon stored False;
    property Favorites: string index 4 read GetValueAtIndex write FBidon stored False;
    property Fonts: string index 5 read GetValueAtIndex write FBidon stored False;
    property History: string index 6 read GetValueAtIndex write FBidon stored False;
    property NetHood: string index 7 read GetValueAtIndex write FBidon stored False;
    property Personal: string index 8 read GetValueAtIndex write FBidon stored False;
    property Programs: string index 9 read GetValueAtIndex write FBidon stored False;
    property ProgramFiles: string read GetProgramFiles write FBidon stored False;
    property Recent: string index 10 read GetValueAtIndex write FBidon stored False;
    property SendTo: string index 11 read GetValueAtIndex write FBidon stored False;
    property StartMenu: string index 12 read GetValueAtIndex write FBidon stored False;
    property Startup: string index 13 read GetValueAtIndex write FBidon stored False;
    property Templates: string index 14 read GetValueAtIndex write FBidon stored False;
  end;

implementation

resourcestring
  RC_ShellFolders = 'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders\';

var
  DirectoryList: array[0..14] of string = ('AppData', 'Cache', 'Cookies', 'Desktop',
    'Favorites', 'Fonts', 'History', 'NetHood', 'Personal', 'Programs', 'Recent',
    'SendTo', 'Start Menu', 'Statup', 'Templates');

  {******************************************************}

function TJvDirectories.GetCurrent: string;
begin
  GetCurrentDirectory(250, buf);
  Result := CheckLastChar(buf);
end;

{******************************************************}

function TJvDirectories.GetWindowsDirectory: string;
begin
  Windows.GetWindowsDirectory(buf, 250);
  Result := CheckLastChar(buf);
end;

{******************************************************}

function TJvDirectories.GetSystemDirectory: string;
begin
  Windows.GetSystemDirectory(buf, 250);
  Result := CheckLastChar(buf);
end;

{******************************************************}

function TJvDirectories.GetTempPath: string;
begin
  Windows.GetTempPath(250, buf);
  Result := CheckLastChar(buf);
end;

{******************************************************}

function TJvDirectories.GetValueAtIndex(Index: Integer): string;
begin
  try
    with TRegistry.Create do
    begin
      OpenKey(RC_ShellFolders, False);
      Result := CheckLastChar(ReadString(DirectoryList[Index]));
      Free;
    end;
  except
    Result := '';
  end;
end;

{******************************************************}

function TJvDirectories.CheckLastChar(Value: string): string;
begin
  //Check if the last Char is a \, and add one if necessary
  if (Length(Value) > 0) and (Value[Length(Value)] <> '\') then
    Value := Value + '\';
  Result := Value;
end;

{******************************************************}

function TJvDirectories.GetProgramFiles: string;
begin
  try
    with TRegistry.Create do
    begin
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKey('Software\Microsoft\Windows\CurrentVersion\', False);
      Result := CheckLastChar(ReadString('ProgramFilesDir'));
      Free;
    end;
  except
    Result := '';
  end;
end;

end.
