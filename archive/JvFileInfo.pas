{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFileInfo.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 att users dott sourceforge dott net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{Provides an interface to most of what is returned by the ShGetFileInfo function
    to allow easier access to information about a files  type, attributes, icon
    image and icon handle. Most of the file info functions can be called with a
    non-existent file to return "generic" info about a specific file type. }

unit JvFileInfo;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls,
  ShellAPI,
  JvComponent, JvTypes;

type
  TJvExeType = (etNone, etMSDos, etWin16, etWin32, etConsole);
  TJvIconModifier = (imNormal, imOverlay, imSelected, imOpen, imShellSize, imSmall);

  TJvFileInfo = class(TJvComponent)
  private
    FLargeImages: TImageList;
    FSmallImages: TImageList;
    FFileName: TFileName;
    FModifier: TJvIconModifier;
    FIcon: TIcon;
    function GetSmallImages: TImageList;
    function GetLargeImages: TImageList;
    procedure SetIcon(const Value: TIcon);
  protected
    FIntDummy: Integer;
    FStrDummy: string;
    FExeDummy: TJvExeType;
    FHandleDummy: THandle;
    function GetIconIndex: Integer;
    function GetDisplayName: string;
    function GeTJvExeType: TJvExeType;
    function GetAttributes: Integer;
    function GetIconLocation: string;
    function GetTypeString: string;
    function GetIconHandle: THandle;
    function GetAttrString: string;
    procedure SetFileName(Value: TFileName);
    procedure SetModifier(Value: TJvIconModifier);
  public
    property LargeImages: TImageList read FLargeImages;
    property SmallImages: TImageList read FSmallImages;
    property IconHandle: THandle read GetIconHandle stored False;
    property Attributes: Integer read GetAttributes stored False;
    function GetCustomInformation(Value: string): TJvFileInfoRec;
  published
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property FileName: TFileName read FFileName write SetFileName stored False;
    property Modifier: TJvIconModifier read FModifier write SetModifier default imNormal;
    property IconIndex: Integer read GetIconIndex write FIntDummy stored False;
    property DisplayName: string read GetDisplayName write FStrDummy stored False;
    property ExeType: TJvExeType read GeTJvExeType write FExeDummy stored False;
    property AttrString: string read GetAttrString write FStrDummy stored False;
    property IconLocation: string read GetIconLocation write FStrDummy stored False;
    property TypeString: string read GetTypeString write FStrDummy stored False;
    property Icon: TIcon read FIcon write SetIcon stored False;
  end;

implementation

uses
  Registry;

constructor TJvFileInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIcon := TIcon.Create;
  if not (csDesigning in ComponentState) then
  begin
    GetLargeImages;
    GetSmallImages;
  end;
  SetFileName(FFileName);
end;

destructor TJvFileInfo.Destroy;
begin
  FLargeImages.Free;
  FSmallImages.Free;
  FIcon.Free;
  inherited Destroy;
end;

function TJvFileInfo.GetLargeImages: TImageList;
var
  SysIL: THandle;
  Sfi: TSHFileInfo;
begin
  if not Assigned(FLargeImages) then
    FLargeImages := TImageList.Create(Self);
  SysIL := SHGetFileInfo('', 0, Sfi, SizeOf(TSHFileInfo), SHGFI_SYSICONINDEX or SHGFI_LARGEICON);
  if SysIL <> 0 then
    FLargeImages.Handle := SysIL;
  FLargeImages.ShareImages := True;
  Result := FLargeImages;
end;

function TJvFileInfo.GetSmallImages: TImageList;
var
  SysIL: THandle;
  Sfi: TSHFileInfo;
begin
  if not Assigned(FSmallImages) then
    FSmallImages := TImageList.Create(Self);
  SysIL := SHGetFileInfo('', 0, Sfi, SizeOf(TSHFileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  if SysIL <> 0 then
    FSmallImages.Handle := SysIL;
  FSmallImages.ShareImages := True;
  Result := FSmallImages;
end;

procedure TJvFileInfo.SetModifier(Value: TJvIconModifier);
begin
  FModifier := Value;
  GetIconHandle;
end;

procedure TJvFileInfo.SetFileName(Value: TFileName);
begin
  FFileName := Value;
  GetIconHandle;
end;

{ returns index of icon for filename in the systemlist }

function TJvFileInfo.GetIconIndex: Integer;
var
  Sfi: TSHFileInfo;
begin
  SHGetFileInfo(PChar(FFileName), 0, Sfi, SizeOf(TSHFileInfo), SHGFI_SYSICONINDEX);
  Result := Sfi.iIcon;
end;

function TJvFileInfo.GetDisplayName: string;
var
  Sfi: TSHFileInfo;
begin
  SHGetFileInfo(PChar(FFileName), 0, Sfi, SizeOf(TSHFileInfo), SHGFI_DISPLAYNAME);
  Result := Sfi.szDisplayName;
end;

function TJvFileInfo.GeTJvExeType: TJvExeType;
var
  Sfi: TSHFileInfo;
  Res: Integer; // sLo,sHi:string;
begin
  Result := etNone;
  Res := SHGetFileInfo(PChar(FFileName), 0, Sfi, SizeOf(TSHFileInfo), SHGFI_EXETYPE);
  if Res = 0 then
    Exit;
  case Lo(Res) of
    77:
      Result := etMSDos;
    78:
      Result := etWin16;
    80:
      Result := etWin32;
  else
    Result := etConsole; { ? }
  end;
end;

function TJvFileInfo.GetAttributes: Integer;
// var    Sfi: TSHFileInfo;
begin
{ this doesn't work, use "old" method instead }
{
  SHGetFileInfo(PChar(FFileName), 0, Sfi, SizeOf(TSHFileInfo), SHGFI_ATTRIBUTES);
  Result := Sfi.dwAttributes;}
  Result := GetFileAttributes(PChar(FFileName));
end;

function TJvFileInfo.GetAttrString: string;
var
  I: Integer;
begin
  I := GetAttributes;
  Result := '';
  if (I and FILE_ATTRIBUTE_NORMAL) <> 0 then
    Exit; { no attributes }
  if (I and FILE_ATTRIBUTE_ARCHIVE) <> 0 then
    Result := Result + 'A';
  if (I and FILE_ATTRIBUTE_COMPRESSED) <> 0 then
    Result := Result + 'C';
  if (I and FILE_ATTRIBUTE_DIRECTORY) <> 0 then
    Result := Result + 'D';
  if (I and FILE_ATTRIBUTE_HIDDEN) <> 0 then
    Result := Result + 'H';
  if (I and FILE_ATTRIBUTE_READONLY) <> 0 then
    Result := Result + 'R';
  if (I and FILE_ATTRIBUTE_SYSTEM) <> 0 then
    Result := Result + 'S';
end;

function StrTrimAll(const S: string; const Chars: TSysCharSet): string;
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    if not (S[I] in Chars) then
      Result := Result + S[I];
end;

function AddDot(S: string): string;
begin
  Result := S;
  if (Length(Result) > 0) and (Result[1] <> '.') then
    Result := '.' + Result;
end;

function ExpandEnvVar(const Value: string): string;
var
  Dest: array [0..MAX_PATH] of Char;
begin
  ExpandEnvironmentStrings(PChar(Value), Dest, MAX_PATH - 1);
  Result := Dest;
end;

function GetAdvancedIconLocation(const FileName: string; var iIcon: Integer): string;
var
  Reg: TRegistry;
  Ext, sPath, Tmp: string;
  I: Integer;
  Sfi: TSHFileInfo;
begin
  // first try the easy way:
  SHGetFileInfo(PChar(FileName), 0, Sfi, SizeOf(TSHFileInfo), SHGFI_ICON or SHGFI_ICONLOCATION);
  Result := Sfi.szDisplayName;
  if Result <> '' then
  begin
    iIcon := Sfi.iIcon;
    Exit;
  end;

  if Pos('.', FileName) > 0 then
    Ext := ExtractFileExt(StrTrimAll(FileName, ['"', '''']))
  else
    Ext := AddDot(FileName);

  if Length(Ext) = 0 then
    Exit;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    // is the key present ?
    if Reg.OpenKey(Ext, False) then
      // get ID to associated program:
      Result := Reg.ReadString('');
    if Reg.OpenKey('\' + Result + '\DefaultIcon', False) then
      Result := Reg.ReadString(''); // path (and possibly index) to icon location
    if Length(Result) > 0 then
    begin
      if Pos('%1', Result) > 0 then
        Result := FileName; // instance specific icon
      I := Pos(',', Result);
      sPath := '';
      if I > 0 then
      begin
        sPath := Copy(Result, I + 1, MaxInt);
        Result := Copy(Result, 1, I - 1);
      end;
      Tmp := '';
      for I := 1 to Length(sPath) do
        if not (sPath[I] in ['-', '0'..'9']) then
          Continue
        else
          Tmp := Tmp + sPath[I];
      iIcon := Abs(StrToIntDef(Tmp, 0)); // convert to positive index
    end
  finally
    Reg.Free;
  end;
  Result := ExpandEnvVar(Result); // replace any environment variables in path (like %systemroot%)
end;

function TJvFileInfo.GetIconLocation: string;
var
  Sfi: TSHFileInfo;
  iIcon: Integer;
begin
  { this doesn't seem to work on files, only on directories (always returns an empty string)... }
  SHGetFileInfo(PChar(FFileName), 0, Sfi, SizeOf(TSHFileInfo), SHGFI_ICONLOCATION);
  Result := Sfi.szDisplayName;
  if Result = '' then
    Result := StrTrimAll(GetAdvancedIconLocation(FFileName, iIcon), ['"']);
end;

function TJvFileInfo.GetTypeString: string;
var
  Sfi: TSHFileInfo;
begin
  SHGetFileInfo(PChar(FFileName), 0, Sfi, SizeOf(TSHFileInfo), SHGFI_TYPENAME);
  Result := Sfi.szTypeName;
  if Result = '' then
    Result := AnsiUpperCase(Copy(ExtractFileExt(FFileName), 2, MaxInt)) + ' file';
end;

function TJvFileInfo.GetIconHandle: THandle;
const
  Modifier: array [TJvIconModifier] of Integer =
    (0, SHGFI_LINKOVERLAY, SHGFI_SELECTED, SHGFI_OPENICON, SHGFI_SHELLICONSIZE, SHGFI_SMALLICON);
var
  Sfi: TSHFileInfo;
begin
  SHGetFileInfo(PChar(FFileName), 0, Sfi, SizeOf(TSHFileInfo),
    SHGFI_SYSICONINDEX or SHGFI_ICON or Modifier[FModifier]);
  Result := Sfi.hIcon;
  FIcon.Handle := Sfi.hIcon;
end;

procedure TJvFileInfo.SetIcon(const Value: TIcon);
begin
// do nothing
end;

function TJvFileInfo.GetCustomInformation(Value: string): TJvFileInfoRec;
var
  Tmp: SHFILEINFO;
  Flags: Cardinal;
begin
  Flags := SHGFI_ICONLOCATION;
  SHGetFileInfo(PChar(Value), 0, Tmp, SizeOf(SHFILEINFO), Flags);
  Result.Location := Tmp.szDisplayName;

  Flags := SHGFI_DISPLAYNAME + SHGFI_ATTRIBUTES + SHGFI_TYPENAME + SHGFI_SYSICONINDEX;
  SHGetFileInfo(PChar(Value), 0, Tmp, SizeOf(SHFILEINFO), Flags);
  Result.DisplayName := Tmp.szDisplayName;
  Result.Attributes := Tmp.dwAttributes;
  Result.TypeName := Tmp.szTypeName;
  Result.SysIconIndex := Tmp.iIcon;

  Flags := SHGFI_EXETYPE + SHGFI_ICON;
  Result.ExeType := SHGetFileInfo(PChar(Value), 0, Tmp, SizeOf(SHFILEINFO), Flags);
  Result.Icon := Tmp.hIcon;
end;

end.

