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

unit ClassUtils;

interface

uses
  Classes, Dialogs, SysUtils, Strings, ShellApi, Graphics, Windows, Messages,
  Controls, Forms, Declarations;

type
  TUtils = class
  public
    class function GetBuild: string;
    class function GetFileInfo(const FileName: string): TFixedFileInfo;
  end;

implementation

{**********************************************************************}
class function TUtils.GetBuild: string;
begin
  with GetFileInfo(Application.ExeName) do
    Result := '(Build '+IntToStr(Major) + '.' + IntToStr(Minor) + '.' +
      IntToStr(Release) + '.' +  IntToStr(Build)+')';
end;
{**********************************************************************}
class function TUtils.GetFileInfo(const FileName: string): TFixedFileInfo;
var
  Handle, VersionSize: DWord;
  SubBlock: string;
  Temp: Pointer;
  Data: Pointer;
begin
  SubBlock := '\';
  VersionSize := GetFileVersionInfoSize( PChar(FileName), Handle );
  if VersionSize > 0 then
  begin
    GetMem( Temp, VersionSize );
    try
      if GetFileVersionInfo( PChar( FileName ), Handle, VersionSize, Temp ) then
        if VerQueryValue( Temp, PChar( SubBlock ), Data, VersionSize ) then
          Result := PFixedFileInfo( Data )^;
    finally
      FreeMem( Temp );
    end;
  end;
end;
{**********************************************************************}

end.

