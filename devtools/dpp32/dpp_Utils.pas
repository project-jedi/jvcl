{**************************************************************************************************}
{                                                                                                  }
{ Delphi language Preprocessor (dpp32)                                                             }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is dpp_Utils.pas                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Andreas Hausladen                                  }
{ Portions created by these individuals are Copyright (C) of these individuals.                    }
{                                                                                                  }
{**************************************************************************************************}
unit dpp_Utils;
{$define HASHTABLE}
interface
uses
{$ifdef MSWINDOWS}
  Windows, SysUtils, Classes, RTLConsts;
{$endif}
{$ifdef LINUX}  
  Libc, SysUtils, Classes, RTLConsts;
{$endif}

type
  TBooleanList = class(TObject)
  private
    FCount: Integer;
    FList: array of Boolean;
    FLast: Boolean;
    function GetItems(Index: Integer): Boolean;
    procedure SetItems(Index: Integer; const Value: Boolean);
  public
    constructor Create;
    function Add(Value: Boolean): Integer;
    procedure Delete(Index: Integer);
    procedure Clear;

    procedure DeleteLast;
    procedure ToggleLast;

    property Last: Boolean read FLast;
    property Count: Integer read FCount;
    property Items[Index: Integer]: Boolean read GetItems write SetItems;
  end;

  TRedirectTable = array of record
                              Text: string;
                              Index: Integer;
                            end;


procedure MakeStringHash(const Text: string; Index: Integer; var Table: TRedirectTable);
function FindStringHash(const Text: string; const Table: TRedirectTable; CaseSensitive: Boolean): Integer;
procedure DelStringHash(const Text: string; var Table: TRedirectTable; CaseSensitive: Boolean); overload;
procedure DelStringHash(Index: Integer; var Table: TRedirectTable); overload;

type
  TFilenameMapper = class(TStringList)
  private
    FHashTable: TRedirectTable;
  public
    procedure AddFilename(const Name, Filename: string);
    function FindFilename(const Name: string; var Filename: string): Boolean;

    procedure Clear; override;
  end;


// *****************************************************************************
// **************************** File handling **********************************
// *****************************************************************************

function CopyFile(const SourceFileName, DestFileName: String;
  NativeCopy: Boolean = True): Boolean;
// Copy Source to Dest using a native copy function or the built in.

function MoveFile(const SourceFilename, DestFilename: String): Boolean;
// Moves the file Source to Dest. If renaming is not possible the file is moved
// by copy and delete.

function FileExistsX(const Filename: string): Boolean;
// Like SysUtils.FileExists() but on Windows it is faster.

procedure FileToString(const Filename: string; out Content: string);
// Reads a file into the string Content.

procedure StringToFile(const Filename, Content: string);
// Writes a file from the string Content.

function GetPreProcessedFilename(const Filename: string; IncludeIndex: Integer = 0): string;
// Returns the corresponding preprocessed Filename.
// IncludeIndex = 0  -> *.i.*
// IncludeIndex > 0  -> *.iX.*   where X=IncludeIndex


type
  TExTestMethod = function(const Filename: string): Boolean of object;

function TestFilenames(const Paths, Filename: string; ExTestMethod: TExTestMethod = nil): string;
// Returns the file name of an existing file by seaching Paths.

function FollowRelativePath(BaseDir, Filename: string): string;
// Expands the relative file path Filename based on BaseDir.

function CompareFileNames(const FileName1, FileName2: string): Integer;
// Compares the two file names

// *****************************************************************************
// ************************** String handling **********************************
// *****************************************************************************

function CountCharsStop(Ch, StopCh: Char; P: PChar): Integer;
function CountChars(Ch: Char; const S: string): Integer;
// CountChars() gets the number of char Ch in P. It stops seeking on char StopCh.

function PosCharSet(CS: TSysCharSet; const S: string): Integer;
// Returns the index of the first char in S which is in CS.

function PosChar(Ch: Char; const S: string): Integer;
// Returns the index of the first char Ch in S.

function StartsText(const StartText, Text: string): Boolean;
// Returns TRUE if StartText is the beginning of Text

function RemoveQuotes(const Text: string): string;
// Removes the embracing quotes ( " and ' )

function IsStrEmpty(const Text: string): Boolean;
// Returns true if all chars in Text are in [#1..#32].

function IndexOfStrText(List: TStrings; const StrText: string;
  CaseSensitive: Boolean): Integer;
// Returns the index of StrText in List.

function IndexOfFilename(Files: TStrings; const Filename: string): Integer;
// Returns the index of the file name in Files (uses CompareFileNames).

procedure PathListToStrings(const Paths: string; List: TStrings);
// Converts Paths (path;path or path:path) to a string list.


implementation

{ TBooleanList }

constructor TBooleanList.Create;
begin
  inherited Create;
  FLast := True;
end;

function TBooleanList.Add(Value: Boolean): Integer;
begin
  if FCount >= Length(FList) then
    SetLength(FList, FCount + 10); // allocate more than 1 saves some memory with SysMemoryManger
  FList[FCount] := Value;
  Result := FCount;
  Inc(FCount);

  FLast := Value;
end;

procedure TBooleanList.Clear;
begin
  FCount := 0;
  SetLength(FList, 0);
  FLast := True;
end;

procedure TBooleanList.Delete(Index: Integer);
begin
  Dec(FCount);
  if FCount mod 10 = 0 then
    SetLength(FList, FCount);
  if FCount > 0 then
    FLast := FList[FCount - 1]
  else
    FLast := True;
end;

procedure TBooleanList.DeleteLast;
begin
  if Count > 0 then
    Delete(Count - 1)
  else
    FLast := True;
end;

function TBooleanList.GetItems(Index: Integer): Boolean;
begin
{  if Cardinal(Index) >= Cardinal(Count) then
    TList.Error(@SListIndexError, Index);}
  Result := FList[Index];
end;

procedure TBooleanList.SetItems(Index: Integer; const Value: Boolean);
begin
{  if Cardinal(Index) >= Cardinal(Count) then
    TList.Error(@SListIndexError, Index);}
  FList[Index] := Value;
  if Index = Count - 1 then FLast := Value;
end;

procedure TBooleanList.ToggleLast;
begin
  if Count > 0 then
    Items[Count - 1] := not Items[Count - 1];
end;

{ TFilenameMapper }

procedure TFilenameMapper.AddFilename(const Name, Filename: string);
begin
  MakeStringHash(Name, Add(Name) + 1, FHashTable);
  Add(Filename);
end;

procedure TFilenameMapper.Clear;
begin
  SetLength(FHashTable, 0);
  inherited Clear;
end;

function TFilenameMapper.FindFilename(const Name: string; var Filename: string): Boolean;
{$ifdef HASHTABLE}
var Index: Integer;
begin
  Index := FindStringHash(Name, FHashTable, {$ifdef MSWINDOWS}False{$endif}{$ifdef LINUX}True{$endif});
  if Index > 0 then
  begin
    Result := True;
    Filename := Strings[Index];
  end
  else
    Result := False;
end;
{$else}
var
  i: Integer;
  cnt: Integer;
begin
  cnt := Count;
  i := 0;
  while i < cnt do
  begin
    if CompareFileNames(Name, Strings[i]) = 0 then
    begin
      Result := True;
      Filename := Strings[i + 1];
      Exit;
    end;
    Inc(i, 2);
  end;
  Result := False;
end;
{$endif}

// *****************************************************************************
// **************************** File handling **********************************
// *****************************************************************************

function CopyFile(const SourceFileName, DestFileName: String;
  NativeCopy: Boolean = True): Boolean;

 function BuiltInCopyFile: Boolean;
 var
   InFile, OutFile: TFileStream;
{$ifdef MSWINDOWS}
   CreationTime, LastWriteTime, LastAccessTime: TFileTime;
{$endif}
{$ifdef LINUX}
   st: TStatBuf;
{$endif}
 begin
   Result := True;
   try
     InFile := TFileStream.Create(SourceFileName, fmOpenRead or fmShareDenyWrite);
     try
       OutFile := TFileStream.Create(DestFileName, fmCreate or fmShareExclusive);
       try
         try
           OutFile.CopyFrom(InFile, 0);
         except
           Result := False;
         end;
{$ifdef MSWINDOWS}
         GetFileTime(InFile.Handle, @CreationTime, @LastAccessTime, @LastWriteTime);
         SetFileTime(OutFile.Handle, @CreationTime, @LastAccessTime, @LastWriteTime);
{$endif}
       finally
         OutFile.Free;
       end;
{$ifdef LINUX}
       FileSetDate(DestFileName, FileGetDate(InFile.Handle));
       if fstat(InFile.Handle, st) = 0 then
         Libc.chmod(PChar(DestFileName), st.st_mode);
{$endif}
     finally
       InFile.Free;
     end;
{$ifdef MSWINDOWS}
     SetFileAttributes(PChar(DestFileName), GetFileAttributes(PChar(SourceFileName)));
{$endif}
   except
     Result := False;
   end;
 end;

begin
{$ifdef MSWINDOWS}
  if NativeCopy then
    Result := Windows.CopyFile(PChar(SourceFileName), PChar(DestFileName), False)
  else
{$endif}
  Result := BuiltInCopyFile;
end;

// *****************************************************************************

function MoveFile(const SourceFilename, DestFilename: String): Boolean;
begin
  Result := False;
  if (SourceFilename = '') or (DestFilename = '') or
     (not FileExists(SourceFilename)) then Exit;

  ForceDirectories(ExtractFilePath(DestFilename)); // create directories

  if FileExists(DestFilename) then // delete destination file if exist
  begin
{$ifdef MSWINDOWS}
    SetFileAttributes(PChar(DestFilename), 0);
{$endif}
{$ifdef LINUX}
    FileSetReadOnly(DestFilename, False);
{$endif}
    DeleteFile(DestFilename);
  end;

  if not RenameFile(SourceFilename, DestFilename) then
  begin
    if CopyFile(SourceFilename, DestFilename, True) then
    begin
     // delete source file
{$ifdef MSWINDOWS}
      SetFileAttributes(PChar(SourceFilename), 0);
{$endif}
{$ifdef LINUX}
      FileSetReadOnly(SourceFilename, False);
{$endif LINUX}
      DeleteFile(SourceFilename);
      Result := True;
    end;
  end
  else
    Result := True;
end;

// *****************************************************************************

function FileExistsX(const Filename: string): Boolean;
{$ifdef MSWINDOWS}
var Attrib: Cardinal;
begin
  Attrib := GetFileAttributes(PChar(Filename));
  Result := (Attrib <> $FFFFFFFF) and (Attrib and FILE_ATTRIBUTE_DIRECTORY = 0);
end;
{$else}
asm
  JMP FileExists
end;
{$endif}

// *****************************************************************************

procedure FileToString(const Filename: string; out Content: string);
var
  Stream: TStream;
  Len: Integer;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    Len := Stream.Size;
    SetLength(Content, Len);
    if Len > 0 then Stream.ReadBuffer(Content[1], Len);
  finally
    Stream.Free;
  end;
end;

// *****************************************************************************

function GetPreProcessedFilename(const Filename: string; IncludeIndex: Integer = 0): string;
var Ext, NewExt: string;
begin
  if IncludeIndex = 0 then NewExt := '.i' else NewExt := Format('.i%d', [IncludeIndex]);

  Ext := ExtractFileExt(Filename);
  Result := ChangeFileExt(Filename, NewExt) + Ext;
end;

// *****************************************************************************

function TestFilenames(const Paths, Filename: string; ExTestMethod: TExTestMethod): string;
var
  List: TStrings;
  i: Integer;
begin
  List := TStringList.Create;
  try
    PathListToStrings(Paths, List); // does never return empty list items
    for i := 0 to List.Count - 1 do
    begin
      Result := List.Strings[i];
      if Result[Length(Result)] <> PathDelim then
        Result := Result + PathDelim + Filename
      else
        Result := Result + Filename;
      if Assigned(ExTestMethod) then if ExTestMethod(Result) then Exit;
      if FileExistsX(Result) then Exit;
    end;
  finally
    List.Free;
  end;
  Result := '';
end;

// *****************************************************************************

function FollowRelativePath(BaseDir, Filename: string): string;
var
  ps: Integer;
  s: string;
begin
  Result := Filename;
  if Filename = '' then Exit;
  if Filename[1] = PathDelim then
    Result := ExtractFileDrive(BaseDir) + Filename
  else
  begin
    BaseDir := ExcludeTrailingPathDelimiter(BaseDir);
    ps := PosChar(PathDelim, Filename);
    while ps > 0 do
    begin
      s := Copy(Filename, 1, ps - 1);
      Delete(Filename, 1, ps);
      if s = '..' then
        BaseDir := ExtractFileDir(BaseDir)
      else if s <> '.' then
        BaseDir := BaseDir + PathDelim + s;
      ps := PosChar(PathDelim, Filename);
    end;
  end;
  if Pointer(Filename) <> nil then // <= => Length(Filename) > 0 then
    Result := BaseDir + PathDelim + Filename
  else
    Result := BaseDir;
end;

// *****************************************************************************

procedure StringToFile(const Filename, Content: string);
var Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    if Length(Content) > 0 then
      Stream.WriteBuffer(Content[1], Length(Content));
  finally
    Stream.Free;
  end;
end;

// *****************************************************************************

function CompareFileNames(const FileName1, FileName2: string): Integer; assembler
asm
{$ifdef MSWINDOWS}
  JMP   CompareText
{$endif}
{$ifdef LINUX}
  JMP   CompareStr
{$endif}
end;

// *****************************************************************************
// ************************** String handling **********************************
// *****************************************************************************

function CountCharsStop(Ch, StopCh: Char; P: PChar): Integer;
begin
  Result := 0;
  while not (P[0] in [#0, StopCh]) do
  begin
    if P[0] = Ch then Inc(Result);
    Inc(P);
  end;
end;

function CountChars(Ch: Char; const S: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
    if S[i] = Ch then
      Inc(Result);
end;

// *****************************************************************************

function PosCharSet(CS: TSysCharSet; const S: string): Integer;
begin
  for Result := 1 to Length(S) do
    if S[Result] in CS then Exit;
  Result := 0;
end;

// *****************************************************************************

function PosChar(Ch: Char; const S: string): Integer; assembler;
asm
  //  AL = Ch
  // EDX = const S
  PUSH   ESI
  MOV    ESI, EDX  // ESI = S

 // String empty ?
  OR     EDX, EDX
  JZ     @@TheEnd

@@loop:
  MOV    AH, [ESI]
  INC    ESI
 // Char found ?
  CMP    AL, AH
  JZ     @@TheEnd
 // String-End ?
  OR     AH, AH
  JNZ    @@loop

  MOV    ESI, EDX

@@TheEnd:
  MOV    EAX, ESI
  SUB    EAX, EDX

  POP    ESI
end;

// *****************************************************************************

type
  PStrRec = ^StrRec;
  StrRec = packed record
    refCnt: Longint;
    length: Longint;
  end;

function StartsText(const StartText, Text: string): Boolean;
begin
  if Pointer(Text) = nil then
    Result := Pointer(StartText) = nil
  else
    Result := StrLIComp(Pointer(Text),
                        Pointer(StartText),
                        PStrRec(Integer(StartText) - SizeOf(StrRec)).length) = 0;
end;

// *****************************************************************************

function RemoveQuotes(const Text: string): string;
var Len: Integer;
begin
  Result := Text;
  Len := Length(Result);
  if (Len > 0) and
     (Result[1] = Result[Len]) and (Result[1] in ['''', '"']) then
  begin
    Delete(Result, Len, 1);
    Delete(Result, 1, 1);
  end;
end;

// *****************************************************************************

function IsStrEmpty(const Text: string): Boolean;
var i: Integer;
begin
  Result := False;
  for i := 1 to Length(Text) do
    if Text[i] > ' ' then Exit;
  Result := True;
end;

// *****************************************************************************

function IndexOfStrText(List: TStrings; const StrText: string;
  CaseSensitive: Boolean): Integer;
var cmp: function(const S1, S2: string): Integer;
begin
  if List <> nil then
  begin
    if CaseSensitive then cmp := CompareStr else cmp := CompareText;

    for Result := 0 to List.Count - 1 do
      if cmp(List.Strings[Result], StrText) = 0 then Exit;
  end;
  Result := -1;
end;

// *****************************************************************************

function IndexOfFilename(Files: TStrings; const Filename: string): Integer;
begin
  if Files <> nil then
  begin
    for Result := 0 to Files.Count - 1 do
      if CompareFileNames(Files.Strings[Result], Filename) = 0 then Exit;
  end;
  Result := -1;
end;

// *****************************************************************************

procedure PathListToStrings(const Paths: string; List: TStrings);
var
  s: string;
  F, P: PChar;
begin
  P := PChar(Paths);
  while P[0] <> #0 do
  begin
    F := P;
    while not (P[0] in [#0, PathSep]) do Inc(P);
    if F < P then
    begin
      SetString(s, F, P - F);
      s := RemoveQuotes(s);
      if Length(s) > 0 then List.Add(s);
    end;
    if P[0] <> #0 then Inc(P);
  end;
end;

// *****************************************************************************
// ***************************** String Hash ***********************************
// *****************************************************************************

function StringHash(const Text: string): Integer;
var a, i, ch, Len: Integer;
begin
  Len := Length(Text);
  a := Len - 6;
  if a <= 0 then a := 1;
  Result := 0;
  for i := Len downto a do
  begin
    ch := Byte(Text[i]);
    if ch >= Byte('a') then Dec(ch, 32);
    Dec(ch, 48); // no chars below '0' are allowed
    Inc(Result, ch);
  end;
end;

{procedure NextHash(var hash: Integer);
begin
  Inc(hash, 29);
end;}

procedure MakeStringHash(const Text: string; Index: Integer; var Table: TRedirectTable);
var hash, len: Integer;
begin
  hash := StringHash(Text);
  len := Length(Table);
  repeat
    if hash >= len then
    begin
      SetLength(Table, hash + 1);
      Table[hash].Text := Text;
      Table[hash].Index := Index;
      Exit;
    end
    else
    if Table[hash].Index = 0 then
    begin
      Table[hash].Text := Text;
      Table[hash].Index := Index;
      Exit;
    end;

//    NextHash(hash);
    Inc(hash, 29);
  until False;
end;

function GetStringHashTableIndex(const Text: string; const Table: TRedirectTable; CaseSensitive: Boolean): Integer;
var hash, len: Integer;
begin
  Result := -1;
  hash := StringHash(Text);
  len := Length(Table);

  if CaseSensitive then
  begin
    while (hash < len) and (Table[hash].Index <> 0) do
    begin
      if Table[hash].Text = Text then
      begin
        Result := hash;
        Exit;
      end;
//      NextHash(hash);
      Inc(hash, 29);
    end;
  end
  else
  begin
    while (hash < len) and (Table[hash].Index <> 0) do
    begin
      if (SameText(Table[hash].Text, Text)) then
      begin
        Result := hash;
        Exit;
      end;
//      NextHash(hash);
      Inc(hash, 29);
    end;
  end;
end;

function FindStringHash(const Text: string; const Table: TRedirectTable; CaseSensitive: Boolean): Integer;
begin
  Result := GetStringHashTableIndex(Text, Table, CaseSensitive);
  if Result = -1 then Result := 0 else Result := Table[Result].Index;
end;

procedure DelStringHash(const Text: string; var Table: TRedirectTable; CaseSensitive: Boolean);
var Index, len: Integer;
begin
  Index := GetStringHashTableIndex(Text, Table, CaseSensitive);
  if Index >= 0 then
  begin
//    Table[Index].Text := '';
    Table[Index].Index := 0;

   // shrink table
    len := Length(Table);
    while (len > 0) and (Table[len - 1].Index = 0) do Dec(len);
    SetLength(Table, len);
  end;
end;

procedure DelStringHash(Index: Integer; var Table: TRedirectTable);
var i, len: Integer;
begin
  for i := 0 to High(Table) do
    if Table[i].Index = Index then
    begin
//      Table[i].Text := '';
      Table[i].Index := 0;
    end;

 // shrink table
  len := Length(Table);
  while (len > 0) and (Table[len - 1].Index = 0) do Dec(len);
  SetLength(Table, len);
end;

end.
