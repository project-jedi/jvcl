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

