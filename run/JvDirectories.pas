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

{$I JVCL.INC}

unit JvDirectories;

interface

uses
  Windows, SysUtils, Classes, Registry,
  JvComponent;

type
  TJvDirectories = class(TJvComponent)
  private
    FDummy: string;
    function GetCurrent: string;
    function GetWindowsDirectory: string;
    function GetSystemDirectory: string;
    function GetTempPath: string;
    function GetValueAtIndex(Index: Integer): string;
    function GetProgramFiles: string;
    function GetCommonIndex(const Index: Integer): string;
    function GetAllUsersIndex(const Index: Integer): string;
  protected
    function CheckLastChar(Value: string): string;
  public
  published
    property CurrentDirectory: string read GetCurrent write FDummy stored False;
    property WindowsDirectory: string read GetWindowsDirectory write FDummy stored False;
    property SystemDirectory: string read GetSystemDirectory write FDummy stored False;
    property TempPath: string read GetTempPath write FDummy stored False;
    property ApplicationData: string index 0 read GetValueAtIndex write FDummy stored False;
    property Cache: string index 1 read GetValueAtIndex write FDummy stored False;
    property Cookies: string index 2 read GetValueAtIndex write FDummy stored False;
    property Desktop: string index 3 read GetValueAtIndex write FDummy stored False;
    property Favorites: string index 4 read GetValueAtIndex write FDummy stored False;
    property Fonts: string index 5 read GetValueAtIndex write FDummy stored False;
    property History: string index 6 read GetValueAtIndex write FDummy stored False;
    property NetHood: string index 7 read GetValueAtIndex write FDummy stored False;
    property Personal: string index 8 read GetValueAtIndex write FDummy stored False;
    property Programs: string index 9 read GetValueAtIndex write FDummy stored False;
    property ProgramFiles: string read GetProgramFiles write FDummy stored False;
    property Recent: string index 10 read GetValueAtIndex write FDummy stored False;
    property SendTo: string index 11 read GetValueAtIndex write FDummy stored False;
    property StartMenu: string index 12 read GetValueAtIndex write FDummy stored False;
    property Startup: string index 13 read GetValueAtIndex write FDummy stored False;
    property Templates: string index 14 read GetValueAtIndex write FDummy stored False;

    property CommonAdminTools: string index 0 read GetCommonIndex write FDummy stored false;
    property CommonAppData: string index 1 read GetCommonIndex write FDummy stored false;
    property CommonDesktop: string index 2 read GetCommonIndex write FDummy stored false;
    property CommonDocuments: string index 3 read GetCommonIndex write FDummy stored false;
    property CommonPrograms: string index 4 read GetCommonIndex write FDummy stored false;
    property CommonStartMenu: string index 5 read GetCommonIndex write FDummy stored false;
    property CommonStartup: string index 6 read GetCommonIndex write FDummy stored false;
    property CommonTemplates: string index 7 read GetCommonIndex write FDummy stored false;
    property CommonPersonal: string index 8 read GetCommonIndex write FDummy stored false;

//    property AllUsersAdminTools:string index 0 read GetAllUsersIndex write FDummy stored false;
    property AllUsersAppData: string index 1 read GetAllUsersIndex write FDummy stored false;
    property AllUsersDesktop: string index 2 read GetAllUsersIndex write FDummy stored false;
    property AllUsersDocuments: string index 3 read GetAllUsersIndex write FDummy stored false;
    property AllUsersPrograms: string index 4 read GetAllUsersIndex write FDummy stored false;
    property AllUsersStartMenu: string index 5 read GetAllUsersIndex write FDummy stored false;
    property AllUsersStartup: string index 6 read GetAllUsersIndex write FDummy stored false;
    property AllUsersTemplates: string index 7 read GetAllUsersIndex write FDummy stored false;
    property AllUsersFavorites: string index 9 read GetAllUsersIndex write FDummy stored false;
  end;

implementation

resourcestring
  RC_ShellFolders = 'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders\';
  RC_allFolders = 'Software\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders\';

var
  DirectoryList: array [0..14] of PChar =
   ('AppData', 'Cache', 'Cookies', 'Desktop', 'Favorites',
    'Fonts', 'History', 'NetHood', 'Personal', 'Programs', 'Recent',
    'SendTo', 'Start Menu', 'Startup', 'Templates');
  CommonDirectoryList: array [0..9] of PChar =
   ('Common Administrative Tools', 'Common AppData', 'Common  Desktop',
    'Common  Documents', 'Common Programs', 'Common Start Menu',
    'Common Startup', 'Common Templates', 'Personal', 'Common Favorites');

function TJvDirectories.GetCurrent: string;
var
  Buffer: array [0..MAX_PATH] of Char;
begin
  GetCurrentDirectory(SizeOf(Buffer), Buffer);
  Result := CheckLastChar(Buffer);
end;

function TJvDirectories.GetWindowsDirectory: string;
var
  Buffer: array [0..MAX_PATH] of Char;
begin
  Windows.GetWindowsDirectory(Buffer, SizeOf(Buffer));
  Result := CheckLastChar(Buffer);
end;

function TJvDirectories.GetSystemDirectory: string;
var
  Buffer: array [0..MAX_PATH] of Char;
begin
  Windows.GetSystemDirectory(Buffer, SizeOf(Buffer));
  Result := CheckLastChar(Buffer);
end;

function TJvDirectories.GetTempPath: string;
var
  Buffer: array [0..MAX_PATH] of Char;
begin
  Windows.GetTempPath(SizeOf(Buffer), Buffer);
  Result := CheckLastChar(Buffer);
end;

function TJvDirectories.GetValueAtIndex(Index: Integer): string;
begin
  try
    with TRegistry.Create do
    try
      OpenKey(RC_ShellFolders, False);
      Result := CheckLastChar(ReadString(DirectoryList[Index]));
    finally
      Free;
    end;
  except
    Result := '';
  end;
end;

function TJvDirectories.CheckLastChar(Value: string): string;
begin
  //Check if the last Char is a \, and add one if necessary
  if (Length(Value) > 0) and (Value[Length(Value)] <> '\') then
    Value := Value + '\';
  Result := Value;
end;

function TJvDirectories.GetProgramFiles: string;
begin
  try
    with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKey('Software\Microsoft\Windows\CurrentVersion\', False);
      Result := CheckLastChar(ReadString('ProgramFilesDir'));
    finally
      Free;
    end;
  except
    Result := '';
  end;
end;

function TJvDirectories.GetCommonIndex(const Index: Integer): string;
begin
  try
    with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKey(RC_ShellFolders, False);
      Result := CheckLastChar(ReadString(CommonDirectoryList[Index]));
    finally
      Free;
    end;
  except
    Result := '';
  end;
end;

function ExpandEnvVar(const Value: string): string;
var
  Buffer: array [0..MAX_PATH] of Char;
begin
  ExpandEnvironmentStrings(PChar(Value), Buffer, MAX_PATH - 1);
  Result := Buffer;
end;

function TJvDirectories.GetAllUsersIndex(const Index: Integer): string;
begin
  try
    with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKey(RC_ShellFolders, False);
      Result := CheckLastChar(ExpandEnvVar(ReadString(CommonDirectoryList[Index])));
    finally
      Free;
    end;
  except
    Result := '';
  end;
end;

end.

