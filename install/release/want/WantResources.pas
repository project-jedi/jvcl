(****************************************************************************
 * WANT - A build management tool.                                          *
 * Copyright (c) 2001-2003 Juancarlo Anez, Caracas, Venezuela.              *
 * All rights reserved.                                                     *
 *                                                                          *
 * This library is free software; you can redistribute it and/or            *
 * modify it under the terms of the GNU Lesser General Public               *
 * License as published by the Free Software Foundation; either             *
 * version 2.1 of the License, or (at your option) any later version.       *
 *                                                                          *
 * This library is distributed in the hope that it will be useful,          *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        *
 * Lesser General Public License for more details.                          *
 *                                                                          *
 * You should have received a copy of the GNU Lesser General Public         *
 * License along with this library; if not, write to the Free Software      *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA *
 ****************************************************************************)
{
    @brief 

    @author Juanco Añez
}

unit WantResources;

interface

uses
  Classes,
  SysUtils,
  JclSysUtils,
  JclFileUtils,
  WildPaths;

const
  SwitchChars           = ['-', '/'];

  C_EOL = #13#10;

  BooleanToString : array[boolean] of string = ('false', 'true');

resourcestring
  WantUsageText   =
             'Usage:'                                                     + C_EOL +
             '  want [options] [target]'                                  + C_EOL +
                                                                            C_EOL +
             'Options:'                                                   + C_EOL +
             '  -h, -H, -?, -help      Displays this help text.'             + C_EOL +
             '  -L                     Displays licensing information.'      + C_EOL +
             '  -buildfile <file>      Specifies the build file. Default is want.xml' + C_EOL +
             '  -Dname=value           Define property "name".'              + C_EOL +
             '  -q, -quiet, -warnings  Be very quiet. '                      + C_EOL +
             '  -verbose               Be extra verbose.'                    + C_EOL +
             '  -debug                 Print debugging information.'         + C_EOL +
             '  -nocolor               Do not use color in console output.'  + C_EOL +
             '  -info                  Display task and attribute info.'     + C_EOL;



  F_WantStartupFailed        = 'Want startup failed';

  F_WantError                = '!!! %s !!!';
  F_TaskError                 = '!!! %s !!!';
  F_TaskFailure               = '%s';

  F_BuildStartMsg             = 'buildfile: %s';
  F_BuildDoneMsg              = 'build complete';
  F_BuildFailedMsg            = 'BUILD FAILED';

  F_BuildFileNotFound         = 'Cannot find %s';
  F_BuildTargetUnhandledError = '%s: %s';
  F_BuildTargetNotFound       = 'target [%s] not found';

  F_TargetStartMsg            = '--> %s';

  F_ExpectedTagError          = 'expected <%s>';
  F_ParseError                = '(%d): %s';
  F_ParseAttributeError       = '(%d): Unknown attribute %s.%s';
  F_ParseChildError           = '(%d): Unknown element <%s><%s>';
  F_ParseChildTextError       = '(%d): Element <%s> does not accept text';

  F_WantClassNotFound        = 'Want class <%s> not found';
  F_DuplicateWantClass       = 'Duplicate Want tag <%s> in class <%s>';


function ConvertToBoolean(const aValue: String): Boolean;

function  Copyright: string;
function  License :string;
procedure Usage;
function  GetVersionString: string;
function  GetStringResource(Name :string):string;

implementation

uses
  Windows;

function ConvertToBoolean(const aValue: String): Boolean;
var
  s: String;
begin
  s := LowerCase(Trim(aValue)) + ' ';

  case s[1] of
    'f': Result := False;
    'n': Result := False;
    '0': Result := False;
  else
    Result := True;
  end;
end;

function GetVersionString: string;
var
  AFileVer: TJclFileVersionInfo;
begin
  try
    AFileVer := TJclFileVersionInfo.Create(ParamStr(0));
    try
      Result := AFileVer.FileVersion;
    finally
      FreeAndNil(AFileVer);
    end;
  except
    Result := '?.?.?.?';
  end;
end;


function GetStringResource(Name :string):string;
var
  FindHandle: THandle;
  ResHandle: THandle;
  ResPtr: Pointer;

  procedure RaiseError(ErrorTxt: string);
  begin
    raise Exception.Create('Internal error: ' + ErrorTxt + ' ' +
      '[WantBase.License]');
  end;
begin
  FindHandle := FindResource(HInstance, PChar(Name), 'TEXT');
  if FindHandle <> 0 then
  begin
    ResHandle := LoadResource(HInstance, FindHandle);
    try
      if ResHandle <> 0 then
      begin
        ResPtr := LockResource(ResHandle);
        try
          if ResPtr <> Nil then
            Result := PChar(ResPtr)
          else
            RaiseError('LockResource failed');
        finally
          UnlockResource(ResHandle);
        end;
      end
      else
        RaiseError('LoadResource failed');
   finally
     FreeResource(FindHandle);
   end;
  end
  else
    RaiseError('FindResource failed');
end;

function Copyright: string;
begin
  Result :=
   'WANT - A Build Management tool. v' + GetVersionString         + C_EOL +
   'Copyright (c) 2001-2003 Juancarlo Anez, Caracas, Venezuela.'  + C_EOL +
   'All rights reserved'                                          + C_EOL;
end;

function License :string;
begin
  try
    Result := GetStringResource('LICENSE');
  except
    Result := Copyright;
  end;
end;

procedure Usage;
begin
  Writeln(WantUsageText);
end;


end.
