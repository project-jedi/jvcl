{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFileInfo.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ShellAPI, JvComponent, JvTypes;

type
  TJvExeType=(etNone,etMSDos,etWin16,etWin32,etConsole);
  TJvIconModifier=(imNormal,imOverlay,imSelected,imOpen,imShellSize,imSmall);

  TJvFileInfo = class(TJvComponent)
  private
    { Private declarations }
    FLargeImages:TImageList;
    FSmallImages:TImageList;
    FFilename:TFilename;
    FModifier:TJvIconModifier;
    FIcon: TIcon;
    function GetSmallImages:TImageList;
    function GetLargeImages:TImageList;
    procedure SetIcon(const Value: TIcon);
  protected
    { Protected declarations }
    FIntDummy:integer;
    FStrDummy:string;
    FExeDummy:TJvExeType;
    FHandleDummy:THandle;
    function GetIconIndex:integer;
    function GetDisplayName:string;
    function GeTJvExeType:TJvExeType;
    function GetAttributes:integer;
    function GetIconLocation:string;
    function GetTypeString:string;
    function GetIconHandle:THandle;
    function GetAttrString:string;
    procedure SetFileName(Value:TFilename);
    procedure SetModifier(Value:TJvIconModifier);
  public
    { Public declarations }
    property LargeImages:TImageList read FLargeImages;
    property SmallImages:TImageList read FSmallImages;
    property IconHandle:THandle read GetIconHandle stored false;
    property Attributes:integer read GetAttributes stored false;
    function GetCustomInformation(Value: string): TFileInformation;
  published
    { Published declarations }
    constructor Create(Aowner:TComponent);override;
    destructor Destroy;override;
    property Filename:TFilename read FFilename write SetFilename stored false;
    property Modifier:TJvIconModifier read FModifier write SetModifier default imNormal;
    property IconIndex:integer read GetIconIndex write FIntDummy stored false;
    property DisplayName:string read GetDisplayName write FStrDummy stored false;
    property ExeType:TJvExeType read GeTJvExeType write FExeDummy stored false;
    property AttrString:string read GetAttrString write FStrDummy stored false;
    property IconLocation:string read GetIconLocation write FStrDummy stored false;
    property TypeString:string read GetTypeString write FStrDummy stored false;
    property Icon:TIcon read FIcon write SetIcon stored false;
  end;


implementation
uses
  Registry;
  
constructor TJvFileInfo.Create(Aowner:TComponent);
begin
  inherited Create(AOwner);
  FIcon := TIcon.Create;
  if not (csDesigning in ComponentState) then
  begin
    GetLargeImages;
    GetSmallImages;
  end;
  SetFilename(FFilename);
end;

destructor TJvFileInfo.Destroy;
begin
  if Assigned(FLargeImages) then
    FLargeImages.Free;
  if Assigned(FSmallImages) then
    FSmallImages.Free;
  FIcon.Free;
  inherited Destroy;
end;

function TJvFileInfo.GetLargeImages:TImageList;
var SysIL: THandle; sfi: TSHFileInfo;
begin
  if not Assigned(FLargeImages) then
    FLargeImages := TImageList.Create(Self);
  SysIL := SHGetFileInfo('', 0, sfi, SizeOf(sfi),
     SHGFI_SYSICONINDEX or SHGFI_LARGEICON);
  if SysIL <> 0 then
    FLargeImages.Handle := SysIL;
  FLargeImages.ShareImages := true;
  Result := FLargeImages;
end;

function TJvFileInfo.GetSmallImages:TImageList;
var SysIL: THandle; sfi: TSHFileInfo;
begin
  if not Assigned(FSmallImages) then
    FSmallImages := TImageList.Create(Self);
  SysIL := SHGetFileInfo('', 0, sfi, SizeOf(sfi),
     SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  if SysIL <> 0 then
    FSmallImages.Handle := SysIL;
  FSmallImages.ShareImages := true;
  Result := FSmallImages;
end;

procedure TJvFileInfo.SetModifier(Value:TJvIconModifier);
begin
  FModifier := Value;
  GetIconHandle
end;

procedure TJvFileInfo.SetFileName(Value:TFilename);
begin
  FFilename := Value;
  GetIconHandle;
end;

{ returns index of icon for filename in the systemlist }
function TJvFileInfo.GetIconIndex:integer;
var    sfi: TSHFileInfo;
begin
  SHGetFileInfo(PChar(FFilename), 0, sfi, SizeOf(TSHFileInfo),
                 SHGFI_SYSICONINDEX);
  Result := sfi.iIcon;
end;

function TJvFileInfo.GetDisplayName:string;
var sfi: TSHFileInfo;
begin
  SHGetFileInfo(PChar(FFilename), 0, sfi, SizeOf(TSHFileInfo),
                 SHGFI_DISPLAYNAME);
  Result := sfi.szDisplayName;
end;

function TJvFileInfo.GeTJvExeType:TJvExeType;
var sfi: TSHFileInfo;Res:integer;// sLo,sHi:string;
begin
  Result := etNone;
  Res := SHGetFileInfo(PChar(FFilename), 0, sfi, SizeOf(TSHFileInfo),
                 SHGFI_EXETYPE);
  if Res = 0 then Exit;
  case Lo(Res) of
    77: Result := etMSDos;
    78: Result := etWin16;
    80: Result := etWin32;
  else
    Result := etConsole; { ? }
  end;
end;

function TJvFileInfo.GetAttributes:integer;
// var    sfi: TSHFileInfo;
begin
{ this doesn't work, use "old" method instead }
{
  SHGetFileInfo(PChar(FFilename), 0, sfi, SizeOf(TSHFileInfo),
                 SHGFI_ATTRIBUTES);
  Result := sfi.dwAttributes;}
  Result := GetFileAttributes(PChar(FFilename));
end;

function TJvFileInfo.GetAttrString:string;
var i:integer;
begin
  i := GetAttributes;
  Result := '';
  if boolean(i and FILE_ATTRIBUTE_NORMAL) then
    Exit; { no attributes }
  if boolean(i and FILE_ATTRIBUTE_ARCHIVE) then
    Result := Result + 'A';
  if boolean(i and FILE_ATTRIBUTE_COMPRESSED) then
    Result := Result + 'C';
  if boolean(i and FILE_ATTRIBUTE_DIRECTORY) then
    Result := Result + 'D';
  if boolean(i and FILE_ATTRIBUTE_HIDDEN) then
    Result := Result + 'H';
  if boolean(i and FILE_ATTRIBUTE_READONLY) then
    Result := Result + 'R';
  if boolean(i and FILE_ATTRIBUTE_SYSTEM) then
    Result := Result + 'S';
end;

function strTrimAll(const S : String; const Chars : TSysCharSet) : String;
var i,j:integer;
begin
  j := Length(S);
  for i := 1 to j do
    if not (S[i] in Chars) then
      Result := Result + S[i];
end;

function AddDot(S:string):string;
begin
  Result := S;
  if (Length(Result) > 0) and (Result[1] <> '.') then
    Result := '.' + Result;
end;

function ExpandEnvVar(const Value: string): string;
var aDest: array[0..MAX_PATH] of char;
begin
  ExpandEnvironmentStrings(PChar(Value), aDest, MAX_PATH - 1);
  Result := aDest;
end;

function GetAdvancedIconLocation(const Filename:string;var iIcon:integer):string;
var Reg:TRegistry;Ext,sPath,tmp:string;i:integer;sfi:TShFileInfo;
begin
  // first try the easy way:
  SHGetFileInfo(PChar(Filename),0,sfi,sizeof(sfi),SHGFI_ICON or SHGFI_ICONLOCATION);
  Result := sfi.szDisplayName;
  if Result <> '' then
  begin
    iIcon := sfi.iIcon;
    Exit;
  end;

  if Pos('.',Filename) > 0 then
    Ext := ExtractFileExt(strTrimAll(Filename,['"','''']))
  else
    Ext := AddDot(Filename);

  if (Length(Ext)  = 0) then Exit;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    // is the key present ?
    if Reg.OpenKey(Ext,false) then
      // get ID to associated program:
      Result := Reg.ReadString('');
    if Reg.OpenKey('\' + Result + '\DefaultIcon',false) then
      Result := Reg.ReadString(''); // path (and possibly index) to icon location
    if Length(Result) > 0 then
    begin
      if Pos('%1',Result) > 0 then
        Result := Filename; // instance specific icon
      i := Pos(',',Result);
      sPath := '';
      if i > 0 then
      begin
        sPath := Copy(Result,i + 1,MaxInt);
        Result := Copy(Result,1,i - 1);
      end;
      tmp := '';
      for i := 1 to Length(sPath) do
        if not (sPath[i] in ['-','0'..'9']) then
          Continue
        else
          tmp := tmp + sPath[i];
      iIcon := Abs(StrToIntDef(tmp,0)); // convert to positive index
    end
  finally
    Reg.Free;
  end;
  Result := ExpandEnvVar(Result); // replace any environment variables in path (like %systemroot%)
end;

function TJvFileInfo.GetIconLocation:string;
var sfi: TSHFileInfo;iIcon:integer;
begin
  { this doesn't seem to work on files, only on directories (always returns an empty string)... }
  SHGetFileInfo(PChar(FFilename), 0, sfi, SizeOf(TSHFileInfo),
                 SHGFI_ICONLOCATION);
    Result := sfi.szDisplayName;
  if Result = '' then
    Result := strTrimAll(GetAdvancedIconLocation(FFilename,iIcon),['"']);
end;

function TJvFileInfo.GetTypeString:string;
var    sfi: TSHFileInfo;
begin
  SHGetFileInfo(PChar(FFilename), 0, sfi, SizeOf(TSHFileInfo),
                 SHGFI_TYPENAME);
  Result := sfi.szTypeName;
  if Result = '' then
    Result := AnsiUpperCase(Copy(ExtractFileExt(FFilename),2,MaxInt)) + ' file';
end;

function TJvFileInfo.GetIconHandle:THandle;
const
  aModifier:array [TJvIconModifier] of integer = (0,SHGFI_LINKOVERLAY,SHGFI_SELECTED,SHGFI_OPENICON,SHGFI_SHELLICONSIZE,SHGFI_SMALLICON);
var
  sfi: TSHFileInfo;
begin
  SHGetFileInfo(PChar(FFilename), 0, sfi, SizeOf(TSHFileInfo),
                 SHGFI_SYSICONINDEX or SHGFI_ICON or aModifier[FModifier]);
  Result := sfi.hIcon;
  FIcon.Handle := sfi.hIcon;
end;

procedure TJvFileInfo.SetIcon(const Value: TIcon);
begin
// do nothing
end;

function TJvFileInfo.GetCustomInformation(Value: string): TFileInformation;
var
  tmp: SHFILEINFO;
  flags: Cardinal;
begin
  flags := SHGFI_ICONLOCATION;
  SHGetFileInfo(PChar(Value), 0, tmp, SizeOf(tmp), flags);
  Result.Location := tmp.szDisplayName;

  flags := SHGFI_DISPLAYNAME + SHGFI_ATTRIBUTES + SHGFI_TYPENAME + SHGFI_SYSICONINDEX;
  SHGetFileInfo(PChar(Value), 0, tmp, SizeOf(tmp), flags);
  Result.DisplayName := tmp.szDisplayName;
  Result.Attributes := tmp.dwAttributes;
  Result.TypeName := tmp.szTypeName;
  Result.SysIconIndex := tmp.iIcon;

  flags := SHGFI_EXETYPE + SHGFI_ICON;
  Result.ExeType := SHGetFileInfo(PChar(Value), 0, tmp, SizeOf(tmp), flags);
  Result.Icon := tmp.hIcon;
end;

end.


