(****************************************************************************
 * WANT - A build management tool.                                          *
 * Copyright (c) 1995-2003 Juancarlo Anez, Caracas, Venezuela.              *
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

    @author Juancarlo Añez
}

unit JalPaths;

interface
uses
  SysUtils,
  Classes,

  JalStrings;


const
  WildChars        = '?*';
  InvalidPathChars = string(';'{$IFDEF LINUX} + ':' {$ENDIF});

  {$IFDEF LINUX}
  SystemPathDelimiter: string = '/';
  {$ELSE}
  SystemPathDelimiter: string = '\';
  {$ENDIF}

type
  EPathException = class(Exception);
  EFileOpException    = class(EPathException);

  TFileAttribute = (
    ReadOnly,  {= $00000001 }
    Hidden,    {= $00000002 }
    SysFile,   {= $00000004 }
    VolumeID,  {= $00000008 }
    Directory, {= $00000010 }
    Archive,   {= $00000020 }
    NoFile     {= -1}
  );
  TFileAttributes = set of TFileAttribute;

const
  AnyFileAttribute = [ReadOnly..Archive];

type
  IPath   = interface;
  TIPaths = array of IPath;

  TPath   = type string;
  TPaths  = TStringDynArray;

  IPath = interface
  ['{EB2D9F52-739B-4939-B8F8-0A7FC638251B}']

    function Concat(Other :IPath)  :IPath; overload;
    function Concat(Other :string) :IPath; overload;

    function Length :Word;

    function Server    :string;
    function Drive     :string;
    function Directory :string;
    function Resource  :string;

    function Super     :IPath;

    function Attributes :TFileAttributes;
    function SystemAttributes :Longint;

    function IsAbsolute :boolean;
    function IsWindowsPath :boolean;

    function IsDirectory :boolean;

    function AsString :string;
    function AsLocalPath :string;

    function Move(FromBase, ToBase :IPath) :IPath;

    function Split :TStringDynArray;

    function Time :TDateTime;
    function SystemTime :Longint;

    function Find(Pattern: string;
                   IncludeAttr: TFileAttributes = AnyFileAttribute;
                   ExcludeAttr: TFileAttributes = []
                   ): TIPaths;

    procedure MakeDir;

    function  GetPath :string;
    procedure SetPath(const Value: string);

    property path :string read GetPath write SetPath;
  end;

function  NewPath(const Rep :string = '') :IPath;

function  CurrentDir: IPath;

function  FileAttributesToSystemAttributes(const Attr: TFileAttributes):Byte;
function  SystemAttributesToFileAttributes(Attr: Integer) :TFileAttributes;
function  TimeToSystemFileTime(const Time: TDateTime):Integer;


implementation

type
  TPathImpl = class(TInterfacedObject, IPath)
  protected
    _Path :string;

    function  GetPath :string;
    procedure SetPath(const Value: string);
    procedure FindPaths( S :TStrings;
                         Pattern : string;
                         IncludeAttr: TFileAttributes;
                         ExcludeAttr: TFileAttributes
                         );
  public
    constructor Create(Rep :string);

    function Concat(Other :IPath)  :IPath; overload;
    function Concat(Other :string) :IPath; overload;

    function Length :Word;

    function Server    :string;
    function Drive     :string;
    function Directory :string;
    function Resource  :string;

    function Super     :IPath;

    function Attributes :TFileAttributes;
    function SystemAttributes :Longint;

    function IsAbsolute :boolean;
    function IsWindowsPath :boolean;

    function IsDirectory :boolean;

    function AsString :string;
    function AsLocalPath :string;

    function Time :TDateTime;
    function SystemTime :Longint;

    function Move(FromBase, ToBase :IPath) :IPath;

    function Split :TStringDynArray;

    procedure MakeDir;

    function Find(Pattern: string;
                   IncludeAttr: TFileAttributes = AnyFileAttribute;
                   ExcludeAttr: TFileAttributes = []
                   ): TIPaths;

    property path :string read GetPath write SetPath;
  end;

function NewPath(const Rep :string) :IPath;
begin
  Result := TPathImpl.Create(Rep);
end;

function  CurrentDir: IPath;
begin
  Result := NewPath(SysUtils.GetCurrentDir);
end;

function FileAttributesToSystemAttributes(const Attr: TFileAttributes): Byte;
begin
  Result := Byte(Attr);
end;

function SystemAttributesToFileAttributes(Attr: Integer): TFileAttributes;
begin
  Result := TFileAttributes(Byte(Attr));
end;


procedure AssertIsSystemIndependentPath(const Path: TPath);
begin
  if Pos(SystemPathDelimiter, Path) <> 0 then
    raise EPathException.Create( Format( '"%s" looks like a system path. Expected a system independent one.',
                                  [Path])
                            );
end;

function  TimeToSystemFileTime(const Time: TDateTime):Integer;
begin
  Result := DateTimeToFileDate(Time);
end;

function StringsToIPaths(S: TStrings): TIPaths;
var
  i: Integer;
begin
  SetLength(Result, S.Count);
  for i := 0 to S.Count-1 do
    Result[i] := NewPath(S[i]);
end;

procedure CheckPath(Path :TPath);
var
  i :Integer;
begin
  for i := 1 to Length(InvalidPathChars) do
  begin
    if Pos(InvalidPathChars[i], Path) <> 0 then
      raise EPathException.Create('invalid path chars in ' + Path);
  end;
end;

function ToPath(SystemPath: TPath; const BasePath: TPath= ''): TPath;
begin
  CheckPath(SystemPath);
  CheckPath(BasePath);

  Result := SystemPath;
  if BasePath <> '' then
    Result := ExtractRelativePath(BasePath, Result);
  if (Length(Result) >= 2)
  and (Result[2] = ':')
  and (Result[1] in ['a'..'z', 'A'..'Z'])
  then
  begin
    Result[1] := LowerCase(''+Result[1])[1];
    Result := SystemPathDelimiter + Result;
  end;
  Result := StringReplace(Result, SystemPathDelimiter, '/', [rfReplaceAll]);
end;

function  SuperPath(const Path: TPath): TPath;
var
  p   : Integer;
begin
  AssertIsSystemIndependentPath(Path);

  if (Path = '.') or (Path = '') then
    Result := '..'
  else
  begin
    Result := Path;
    p := LastDelimiter('/', Result);
    if p = Length(Result) then
    begin
      Result := Copy(Result, 1, p-1);
      p := LastDelimiter('/', Result);
    end;
    Result := Copy(Result, 1, p-1);
  end;
end;




function PathDrive(const Path: TPath): string;
var
  p: Integer;
begin
  p := Pos(':', Path);
  Result := Copy(Path, p-1, 1);
end;

function PathServer(const Path: TPath): string;
var
  P: string;
begin
  if StrLeft(Path, 2) <> '//' then
    Result := ''
  else
  begin
    P := Copy(Path, 3, Length(Path));
    Result := StrToken(P, '/');
  end;
end;

function RemovePathDrive(Path: TPath):TPath;
var
  p: Integer;
begin
  Result := Path;
  p := Pos(':', Result);
  Delete(Result, 1, p);
end;

function RemovePathServer(Path: TPath):TPath;
var
  Server: string;
begin
  Result := Path;
  Server := PathServer(Path);
  if Server <> '' then
    Delete(Result, 1, 3 + Length(Server));
end;



{ TPathImpl }

function TPathImpl.AsLocalPath: string;
begin
   Result := Path;
   if (Drive <> '') and (StrLeft(Result, 1) = '/') then
     Delete(Result,1, 1);
   Result := StringReplace(Result, '/', SystemPathDelimiter, [rfReplaceAll]);
   if IsAbsolute then
     Result := ExpandFileName(Result);

   CheckPath(Result);
end;

function TPathImpl.AsString: string;
begin
  Result := path;
end;

function TPathImpl.Concat(Other: string): IPath;
begin
  Result := Concat(NewPath(Other));
end;

function TPathImpl.Concat(Other: IPath): IPath;
var
  Parts: TPaths;
  i    : Integer;
  P1   : string;
begin
  Parts := nil;
  if (Length = 0)
  or Other.IsAbsolute then
    Result := Other
  else if Other.Length = 0 then
    Result := Self
  else begin
    P1 := Self.AsString;
    if StrLast(P1) = '/' then
      Delete(P1, System.Length(P1), 1);
    Parts := Other.Split;
    for i := Low(Parts) to High(Parts) do
    begin
      if Parts[i] = '..' then
        P1 := SuperPath(P1)
      else if Parts[i] = '.' then
        // do nothing
      else
        P1 := P1 + '/' + Parts[i];
    end;
    Result := NewPath(P1);
  end;
end;

constructor TPathImpl.Create(Rep: string);
begin
  CheckPath(Rep);
  inherited Create;
  Self.path := Rep;
end;

function TPathImpl.Directory: string;
begin
  Result := RemovePathDrive(RemovePathServer(Path));
end;

function TPathImpl.Drive: string;
var
  p: Integer;
begin
  if not IsWindowsPath then
    Result := ''
  else
  begin
    p := Pos(':', Path);
    Result := Copy(Path, p-1, 1);
  end;
end;

function TPathImpl.Find(Pattern: string; IncludeAttr, ExcludeAttr: TFileAttributes): TIPaths;
var
  S: TStringList;
begin
  S := TStringList.Create;
  S.Sorted := True;
  try
    FindPaths(S, Pattern, IncludeAttr, ExcludeAttr);
    Result := StringsToIPaths(S);
  finally
    S.Free;
  end;
end;

procedure TPathImpl.FindPaths(S: TStrings; Pattern :string; IncludeAttr, ExcludeAttr: TFileAttributes);
var
  Search: TSearchRec;
  SearchResult: Integer;
  Local       :string;
begin
  Local := Concat(Pattern).AsLocalPath;

  SearchResult := FindFirst(Local, faAnyFile, Search);
  try
    while SearchResult = 0 do
    begin
      if  ((SystemAttributesToFileAttributes(Search.Attr) * IncludeAttr) <> [])
      and ((SystemAttributesToFileAttributes(Search.Attr) * ExcludeAttr) =  [])
      and (Search.Name <> '.' )
      and (Search.Name <> '..' )
      then                       
        S.Add(ToPath(Search.Name, Self.Path));
      SearchResult := FindNext(Search);
    end;
  finally
    FindClose(Search);
  end;
end;

function TPathImpl.IsAbsolute: boolean;
begin
  Result := (System.Length(path) > 0) and (path[1] = '/')
            or (System.Length(path) >= 3) and (path[2] = ':') and (path[3] = '/');
end;

function TPathImpl.IsWindowsPath: boolean;
begin
  Result := Pos(':', Path) <> 0;
end;

function TPathImpl.Length: Word;
begin
  Result := System.Length(path);
end;

function TPathImpl.Move(FromBase, ToBase: IPath): IPath;
begin
  if (FromBase.path <> '') and (Pos(FromBase.path+'/', Self.Path) = 1) then
    Result := ToBase.Concat(Copy(Self.Path, 2+FromBase.Length, Self.Length))
  else
    Result := ToBase.Concat(Path);
end;

function TPathImpl.Resource: string;
var
  p :Integer;
begin
  p := LastDelimiter('/', Path);
  if p < Length then
    Result := Copy(Path, p+1, Length)
  else
    Result := Path;
end;

function TPathImpl.Server: string;
var
  P: string;
begin
  if StrLeft(Path, 2) <> '//' then
    Result := ''
  else
  begin
    P := Copy(Path, 3, Length);
    Result := StrToken(P, '/');
  end;
end;

procedure TPathImpl.SetPath(const Value: string);
begin
  _Path := ToPath(Value);
end;

function TPathImpl.Split: TStringDynArray;
begin
  Result := StringToArray(path, '/');
end;

function TPathImpl.Super: IPath;
var
  p   : Integer;
begin
  if (Path = '.') or (Path = '') then
    Result := NewPath('..')
  else
  begin
    p := LastDelimiter('/', Path);
    if p = Length then
      p := LastDelimiter('/', Copy(Path, 1, p-1));
    Result := NewPath(Copy(Path, 1, p-1));
  end;
end;


function TPathImpl.GetPath: string;
begin
  Result := _Path;
end;

function TPathImpl.SystemTime: Longint;
begin
  Result := SysUtils.FileAge(asLocalPath);
  if Result < 0 then
    Result := 0;
end;

function TPathImpl.Time: TDateTime;
var
  STime: Longint;
begin
  STime := SystemTime;
  if STime <= 0 then
    Result := 0
  else
    Result := FileDateToDateTime(STime);
end;

function TPathImpl.IsDirectory: boolean;
begin
  Result := (Attributes * [NoFile, JalPaths.Directory]) = [JalPaths.Directory];
end;

function TPathImpl.Attributes: TFileAttributes;
var
  Attr :Integer;
begin
  Attr := SystemAttributes;
  if Attr < 0 then
    Result := [NoFile]
  else
    Result := TFileAttributes(Byte(Attr));
end;


function TPathImpl.SystemAttributes: Longint;
begin
  Result := Byte(SysUtils.FileGetAttr(asLocalPath));
end;

procedure TPathImpl.MakeDir;
begin
  if (Length > 0)
  and (Path[Length] <> ':')  // Oops! Windows specific!
  and not IsDirectory then
  begin
    Super.MakeDir;
    SysUtils.CreateDir(asLocalPath);
    if not IsDirectory then
      raise EFileOpException.Create(Format('Could not create directory "%s"', [Path]));
  end;
end;


end.
