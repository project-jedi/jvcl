{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCreateShortcut.PAS, released on 2001-02-28.

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
{$IFDEF DELPHI6_UP}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF}
{$IFDEF LINUX}
This unit is only supported on Windows!
{$ENDIF}

unit JvCreateShortcut;

{*******************************************************}
{  Modifications:                                       }
{    1/10/2000  Changed procedure to functions          }
{*******************************************************}

{$OBJEXPORTALL On}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ShlObj, ActiveX, ComObj,
  Registry, JvTypes, JvComponent;

type
  TJvCreateShortcut = class(TJvComponent)
  published
    function CreateStartMenuShortcut(GroupName: string; FileName: string;
      Parameters: string; linkname: string): string;
    function CreateDesktopShortcut(FileName: string; Parameters: string;
      linkname: string): string;
    function CreateShortcut(FileName: string; Parameters: string;
      linkname: string; Directory: string): string;
  end;

implementation

resourcestring
  RC_ExplorerKey = 'Software\Microsoft\Windows\CurrentVersion\Explorer';

  {*********************************************************}

function TJvCreateShortcut.CreateStartMenuShortcut(GroupName: string;
  FileName: string; Parameters: string; linkname: string): string;
var
  MyObject: IUnknown;
  MySLink: IShellLink;
  MyPFile: IPersistFile;
  Directory: string;
  WFileName: WideString;
  // (rom) TRegIni is silly use TRegistry
  MyReg: TRegIniFile;
begin
  MyObject := CreateComObject(CLSID_ShellLink);
  MySLink := MyObject as IShellLink;
  MyPFile := MyObject as IPersistFile;
  with MySLink do
  begin
    SetArguments(PChar(Parameters));
    SetPath(PChar(FileName));
    SetWorkingDirectory(PChar(ExtractFilePath(FileName)));
  end;
  MyReg := TRegIniFile.Create(RC_ExplorerKey);
  if GroupName = '' then
    Directory := MyReg.ReadString('Shell Folders', 'Programs', '')
  else
    Directory := MyReg.ReadString('Shell Folders', 'Programs', '') + '\' + GroupName;
  ForceDirectories(Directory);
  WFileName := Directory + '\' + linkname + '.lnk';
  MyPFile.Save(PWChar(WFileName), False);
  MyReg.Free;
  Result := WFileName;
end;

{*********************************************************}

function TJvCreateShortcut.CreateDesktopShortcut(FileName: string;
  Parameters: string; linkname: string): string;
var
  MyObject: IUnknown;
  MySLink: IShellLink;
  MyPFile: IPersistFile;
  Directory: string;
  WFileName: WideString;
  MyReg: TRegIniFile;
begin
  MyObject := CreateComObject(CLSID_ShellLink);
  MySLink := MyObject as IShellLink;
  MyPFile := MyObject as IPersistFile;
  with MySLink do
  begin
    SetArguments(PChar(Parameters));
    SetPath(PChar(FileName));
    SetWorkingDirectory(PChar(ExtractFilePath(FileName)));
  end;
  MyReg := TRegIniFile.Create(RC_ExplorerKey);
  Directory := MyReg.ReadString('Shell Folders', 'Desktop', '');
  WFileName := Directory + '\' + linkname + '.lnk';
  MyPFile.Save(PWChar(WFileName), False);
  Result := WFileName;
  MyReg.Free;
end;

{*********************************************************}

function TJvCreateShortcut.CreateShortcut(FileName: string; Parameters: string;
  linkname: string; Directory: string): string;
var
  MyObject: IUnknown;
  MySLink: IShellLink;
  MyPFile: IPersistFile;
  WFileName: WideString;
begin
  if (Directory <> '') and (Directory[Length(Directory)] <> '\') then
    Directory := Directory + '\';
  ForceDirectories(Directory);
  MyObject := CreateComObject(CLSID_ShellLink);
  MySLink := MyObject as IShellLink;
  MyPFile := MyObject as IPersistFile;
  with MySLink do
  begin
    SetArguments(PChar(Parameters));
    SetPath(PChar(FileName));
    SetWorkingDirectory(PChar(ExtractFilePath(FileName)));
  end;
  WFileName := Directory + linkname + '.lnk';
  Result := WFileName;
  MyPFile.Save(PWChar(WFileName), False);
end;

end.
