{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSearchFiles.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-07-24

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

unit JvSearchFiles;

{$I JEDI.INC}
{$IFDEF COMPILER6_UP}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}
{$IFDEF LINUX}
This unit is only supported on Windows!
{$ENDIF}

interface

uses
  Classes, SysUtils, Windows, JvComponent;

const
  { Taken from WinNT.h }
  FILE_ATTRIBUTE_SPARSE_FILE = $200;
  FILE_ATTRIBUTE_REPARSE_POINT = $400;
  FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = $2000;
  FILE_ATTRIBUTE_ENCRYPTED = $4000;

type
  TJvAttrFlagKind = (tsMustBeSet, tsDontCare, tsMustBeUnSet);
  TJvSearchOption = (soSorted, soAllowDuplicates, soStripDirs, soIncludeSubDirs,
    soOwnerData);
  TJvSearchOptions = set of TJvSearchOption;
  TJvSearchType = (stFileAttr, stDirAttr, stMinSize, stMaxSize,
    stLastChangeAfter, stLastChangeBefore, stFileMask, stFileMaskCaseSensitive,
    stSearchFiles, stSearchDirs);
  TJvSearchTypes = set of TJvSearchType;
  TJvFileSearchEvent = procedure(Sender: TObject; const AName: string) of
    object;
  TJvSearchFilesError = procedure(Sender: TObject; var Handled: Boolean) of
    object;
  TJvCheckEvent = procedure(Sender: TObject; var Result: Boolean) of object;
  TJvErrorResponse = (erAbort, erIgnore, erRaise);

  TJvSearchAttributes = class(TPersistent)
  private
    FIncludeAttr: DWORD;
    FExcludeAttr: DWORD;
    function GetAttr(const Index: Integer): TJvAttrFlagKind;
    procedure SetAttr(const Index: Integer; Value: TJvAttrFlagKind);
    procedure ReadIncludeAttr(Reader: TReader);
    procedure ReadExcludeAttr(Reader: TReader);
    procedure WriteIncludeAttr(Writer: TWriter);
    procedure WriteExcludeAttr(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
    property IncludeAttr: DWORD read FIncludeAttr write FIncludeAttr;
    property ExcludeAttr: DWORD read FExcludeAttr write FExcludeAttr;
  published
    property ReadOnly: TJvAttrFlagKind index FILE_ATTRIBUTE_READONLY read GetAttr
      write SetAttr stored False;
    property Hidden: TJvAttrFlagKind index FILE_ATTRIBUTE_HIDDEN
      read GetAttr write SetAttr stored False;
    property System: TJvAttrFlagKind index FILE_ATTRIBUTE_SYSTEM
      read GetAttr write SetAttr stored False;
    property Archive: TJvAttrFlagKind index FILE_ATTRIBUTE_ARCHIVE
      read GetAttr write SetAttr stored False;
    property Normal: TJvAttrFlagKind index FILE_ATTRIBUTE_NORMAL
      read GetAttr write SetAttr stored False;
    property Temporary: TJvAttrFlagKind index FILE_ATTRIBUTE_TEMPORARY
      read GetAttr write SetAttr stored False;
    property SparseFile: TJvAttrFlagKind index FILE_ATTRIBUTE_SPARSE_FILE
      read GetAttr write SetAttr stored False;
    property ReparsePoint: TJvAttrFlagKind index FILE_ATTRIBUTE_REPARSE_POINT
      read GetAttr write SetAttr stored False;
    property Compressed: TJvAttrFlagKind index FILE_ATTRIBUTE_COMPRESSED
      read GetAttr write SetAttr stored False;
    property OffLine: TJvAttrFlagKind index FILE_ATTRIBUTE_OFFLINE
      read GetAttr write SetAttr stored False;
    property NotContentIndexed: TJvAttrFlagKind index
      FILE_ATTRIBUTE_NOT_CONTENT_INDEXED read GetAttr write SetAttr stored
      False;
    property Encrypted: TJvAttrFlagKind index FILE_ATTRIBUTE_ENCRYPTED read
      GetAttr write SetAttr stored False;
  end;

  TJvSearchParams = class(TPersistent)
  private
    FMaxSizeHigh: Cardinal;
    FMaxSizeLow: Cardinal;
    FMinSizeHigh: Cardinal;
    FMinSizeLow: Cardinal;
    FLastChangeBefore: TDateTime;
    FLastChangeBeforeFT: TFileTime;
    FLastChangeAfter: TDateTime;
    FLastChangeAfterFT: TFileTime;
    FFileAttributes: TJvSearchAttributes;
    FDirAttributes: TJvSearchAttributes;
    FSearchTypes: TJvSearchTypes;
    FFileMasks: TStrings;
    FCaseFileMasks: TStrings;
    FFileMaskSeperator: Char;
    procedure SetDirAttributes(const Value: TJvSearchAttributes);
    procedure SetFileAttributes(const Value: TJvSearchAttributes);
    function GetMaxSize: Int64;
    function GetMinSize: Int64;
    procedure SetMaxSize(const Value: Int64);
    procedure SetMinSize(const Value: Int64);
    procedure SetLastChangeAfter(const Value: TDateTime);
    procedure SetLastChangeBefore(const Value: TDateTime);
    procedure SetFileMasks(const Value: TStrings);
    procedure SetFileMask(const Value: string);
    procedure FileMasksChange(Sender: TObject);
    function GetFileMask: string;
    procedure UpdateCaseMasks;
    procedure SetSearchTypes(const Value: TJvSearchTypes);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Check(const AFindData: TWin32FindData): Boolean;
    property FileMask: string read GetFileMask write SetFileMask;
    property FileMaskSeperator: Char read FFileMaskSeperator write
      FFileMaskSeperator default ';';
  published
    property DirAttributes: TJvSearchAttributes read FDirAttributes write
      SetDirAttributes;
    property FileAttributes: TJvSearchAttributes read FFileAttributes write
      SetFileAttributes;
    property SearchTypes: TJvSearchTypes read FSearchTypes write SetSearchTypes
      default [stFileMask, stSearchFiles];
    property MinSize: Int64 read GetMinSize write SetMinSize;
    property MaxSize: Int64 read GetMaxSize write SetMaxSize;
    property LastChangeAfter: TDateTime read FLastChangeAfter write
      SetLastChangeAfter;
    property LastChangeBefore: TDateTime read FLastChangeBefore write
      SetLastChangeBefore;
    property FileMasks: TStrings read FFileMasks write SetFileMasks;
  end;

  TJvSearchFiles = class(TJvComponent)
  private
    FSearching: Boolean;
    FTotalDirectories: Integer;
    FTotalFiles: Integer;
    FTotalFileSize: Int64;
    FRootDirectory: string;
    FOnFindFile: TJvFileSearchEvent;
    FOnFindDirectory: TJvFileSearchEvent;
    FOptions: TJvSearchOptions;
    FOnAbort: TNotifyEvent;
    FOnError: TJvSearchFilesError;
    FDirs: TStrings;
    FFiles: TStrings;
    FFindData: TWin32FindData;
    FAborting: Boolean;
    FErrorResponse: TJvErrorResponse;
    FOnCheck: TJvCheckEvent;
    FOnBeginScanDir: TJvFileSearchEvent;
    procedure SetOptions(const Value: TJvSearchOptions);
    procedure SetSearchParams(const Value: TJvSearchParams);
  protected
    { FSearchParams has protected visibility so it can be set in ancestors }
    FSearchParams: TJvSearchParams;
    procedure DoBeginScanDir(const ADirName: string); virtual;
    procedure DoFindFile(const APath: string); virtual;
    procedure DoFindDir(const APath: string); virtual;
    procedure DoAbort; virtual;
    function DoCheck: Boolean; virtual;
    function HandleError: Boolean; virtual;

    procedure Init; virtual;
    function EnumFiles(const ADirectoryName: string; Dirs: TStrings): Boolean;
    function InternalSearch(const ADirectoryName: string): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Abort;
    function Search: Boolean;

    property FindData: TWin32FindData read FFindData;
    property Files: TStrings read FFiles;
    property Directories: TStrings read FDirs;
    property Searching: Boolean read FSearching;
    property TotalDirectories: Integer read FTotalDirectories;
    property TotalFileSize: Int64 read FTotalFileSize;
    property TotalFiles: Integer read FTotalFiles;
  published
    property RootDirectory: string read FRootDirectory write FRootDirectory;
    property Options: TJvSearchOptions read FOptions write SetOptions default
      [soIncludeSubDirs];
    property ErrorResponse: TJvErrorResponse read FErrorResponse write
      FErrorResponse default erAbort;
    property SearchParams: TJvSearchParams read FSearchParams write
      SetSearchParams;
    property OnBeginScanDir: TJvFileSearchEvent read FOnBeginScanDir write
      FOnBeginScanDir;
    property OnFindFile: TJvFileSearchEvent read FOnFindFile write FOnFindFile;
    property OnFindDirectory: TJvFileSearchEvent read FOnFindDirectory write
      FOnFindDirectory;
    property OnAbort: TNotifyEvent read FOnAbort write FOnAbort;
    property OnError: TJvSearchFilesError read FOnError write FOnError;
    property OnCheck: TJvCheckEvent read FOnCheck write FOnCheck;
  end;

implementation

uses
  JclFileUtils, JclStrings, JclDateTime, Math;
//  Forms; // Application.ProcessMessages

{ Maybe TJvSearchFiles should be implemented with FindFirst, FindNext.
  There isn't a good reason to use FindFirstFile, FindNextFile instead of
  FindFirst, FindNext; except to prevent a little overhead perhaps. }

// (p3)  
procedure ProcessMessages;
var M: TMsg;
begin
  if PeekMessage(M,0,0,0,pm_Remove) then
  begin
    TranslateMessage(M);
    DispatchMessage(M);
  end;
end;
  
{ TJvSearchFiles }

procedure TJvSearchFiles.Abort;
begin
  if not FSearching then
    Exit;

  FAborting := True;
  DoAbort;
end;

constructor TJvSearchFiles.Create(AOwner: TComponent);
begin
  inherited;
  FFiles := TStringList.Create;
  FDirs := TStringList.Create;
  FSearchParams := TJvSearchParams.Create;

  { defaults }
  Options := [soIncludeSubDirs];
  ErrorResponse := erAbort;
end;

destructor TJvSearchFiles.Destroy;
begin
  FFiles.Free;
  FDirs.Free;
  FSearchParams.Free;
  inherited;
end;

procedure TJvSearchFiles.DoAbort;
begin
  if Assigned(FOnAbort) then
    FOnAbort(Self);
end;

procedure TJvSearchFiles.DoBeginScanDir(const ADirName: string);
begin
  if Assigned(OnBeginScanDir) then
    FOnBeginScanDir(Self, ADirName);
end;

function TJvSearchFiles.DoCheck: Boolean;
begin
  if Assigned(FOnCheck) then
  begin
    Result := False;
    FOnCheck(Self, Result);
  end
  else
    Result := FSearchParams.Check(FFindData)
end;

procedure TJvSearchFiles.DoFindDir(const APath: string);
var
  DirName: string;
  FileSize: Int64;
begin
  Inc(FTotalDirectories);

  with FindData do
  begin
    if soStripDirs in Options then
      DirName := cFilename
    else
      DirName := APath + cFilename;

    if not (soOwnerData in Options) then
      FDirs.Add(DirName);

    Int64Rec(FileSize).Lo := nFileSizeLow;
    Int64Rec(FileSize).Hi := nFileSizeHigh;
    Inc(FTotalFileSize, FileSize);

    { NOTE: soStripDirs also applies to the event }
    if Assigned(FOnFindDirectory) then
      FOnFindDirectory(Self, DirName);
  end;
end;

procedure TJvSearchFiles.DoFindFile(const APath: string);
var
  FileName: string;
  FileSize: Int64;
begin
  Inc(FTotalFiles);

  with FindData do
  begin
    if soStripDirs in Options then
      FileName := cFilename
    else
      FileName := APath + cFilename;

    if not (soOwnerData in Options) then
      FFiles.Add(FileName);

    Int64Rec(FileSize).Lo := nFileSizeLow;
    Int64Rec(FileSize).Hi := nFileSizeHigh;
    Inc(FTotalFileSize, FileSize);

    { NOTE: soStripDirs also applies to the event }
    if Assigned(FOnFindFile) then
      FOnFindFile(Self, FileName);
  end;
end;

function TJvSearchFiles.EnumFiles(const ADirectoryName: string;
  Dirs: TStrings): Boolean;
var
  Handle: THandle;
  Finished: Boolean;
begin
  DoBeginScanDir(ADirectoryName);

  { Always scan the full directory - ie use * as mask - this seems faster
    then first using a mask, and then scanning the directory for subdirs }

  Handle := Windows.FindFirstFile(PChar(ADirectoryName + '*'), FFindData);
  Result := Handle <> INVALID_HANDLE_VALUE;
  if not Result then
  begin
    Result := GetLastError = ERROR_FILE_NOT_FOUND;
    Exit;
  end;

  Finished := False;
  try
    while not Finished do
    begin
      // (p3) no need to bring in the Forms unit for this:
      if not IsConsole then
        ProcessMessages;

      { After ProcessMessages, the user can have called Abort,
        so check it }
      if FAborting then
      begin
        Result := False;
        Exit;
      end;

      with FFindData do
      begin
        { Is it a directory? }
        if dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY > 0 then
        begin
          { Filter out '.' and '..'
            Other dir names can't begin with a '.' }
          if cFileName[0] <> '.' then
          begin
            if DoCheck then
              DoFindDir(ADirectoryName);

            Dirs.Add(cFileName);
          end;
        end
        else if DoCheck then
          DoFindFile(ADirectoryName);
      end;

      if not Windows.FindNextFile(Handle, FFindData) then
      begin
        Finished := True;
        Result := GetLastError = ERROR_NO_MORE_FILES;
      end;
    end;
  finally
    Result := Windows.FindClose(Handle) and Result;
  end;
end;

function TJvSearchFiles.HandleError: Boolean;
begin
  { ErrorResponse = erIgnore : Result = True
    ErrorResponse = erAbort  : Result = False
    ErrorResponse = erRaise  : The last error is raised.

    If a user implements an OnError event handler, these results can be
    overridden.
  }

  if FAborting then
  begin
    Result := False;
    Exit;
  end;

  Result := FErrorResponse = erIgnore;
  if Assigned(FOnError) then
    FOnError(Self, Result);
  if (FErrorResponse = erRaise) and not Result then
    RaiseLastOSError;
end;

procedure TJvSearchFiles.Init;
begin
  FTotalFileSize := 0;
  FTotalDirectories := 0;
  FTotalFiles := 0;
  FDirs.Clear;
  FFiles.Clear;
  FAborting := False;
end;

function TJvSearchFiles.InternalSearch(const ADirectoryName: string): Boolean;
var
  List: TStringList;
  DirSep: string;
  i: Integer;
begin
  List := TStringList.Create;
  try
    DirSep := IncludeTrailingPathDelimiter(ADirectoryName);

    Result := EnumFiles(DirSep, List) or HandleError;
    if not Result then
      Exit;

    { I think it would be better to do no recursion; Don't know if it can
      be easy implemented - if you want to keep the depth first search -
      and without doing a lot of TList moves }
    for i := 0 to List.Count - 1 do
    begin
      Result := InternalSearch(DirSep + List[i]);
      if not Result then
        Exit;
    end;
  finally
    List.Free;
  end;
end;

function TJvSearchFiles.Search: Boolean;
begin
  Result := False;
  if Searching then
    Exit;

  Init;

  FSearching := True;
  try
    Result := InternalSearch(FRootDirectory);
  finally
    FSearching := False;
  end;
end;

procedure TJvSearchFiles.SetOptions(const Value: TJvSearchOptions);
var
  ChangedOptions: TJvSearchOptions;
begin
  { I'm not sure, what to do when the user changes property Options, while
    the component is searching for files. As implemented now, the component
    just changes the options, and doesn't ensure that the properties hold
    for all data. For example unsetting flag soStripDirs while searching,
    results in a file list with values stripped, and other values not stripped.

    An other option could be to raise an exception when the user tries to
    change Options while the component is searching. But because no serious
    harm is caused - by changing Options, while searching - the component
    doen't do that.
  }
  { (p3) you could also do:
    if Searching then Exit;
  }

  if FOptions <> Value then
  begin
    ChangedOptions := FOptions + Value - (FOptions * Value);

    FOptions := Value;

    if soSorted in ChangedOptions then
    begin
      TStringList(FDirs).Sorted := soSorted in FOptions;
      TStringList(FFiles).Sorted := soSorted in FOptions;
    end;

    if soAllowDuplicates in ChangedOptions then
    begin
      if soAllowDuplicates in FOptions then
      begin
        TStringList(FDirs).Duplicates := dupAccept;
        TStringList(FFiles).Duplicates := dupAccept;
      end
      else
      begin
        TStringList(FDirs).Duplicates := dupIgnore;
        TStringList(FFiles).Duplicates := dupIgnore;
      end;
    end;

    // soStripDirs; soIncludeSubDirs; soOwnerData
  end;
end;

procedure TJvSearchFiles.SetSearchParams(const Value: TJvSearchParams);
begin
  FSearchParams.Assign(Value);
end;

{ TJvSearchAttributes }

procedure TJvSearchAttributes.Assign(Source: TPersistent);
begin
  if Source is TJvSearchAttributes then
  begin
    IncludeAttr := TJvSearchAttributes(Source).IncludeAttr;
    ExcludeAttr := TJvSearchAttributes(Source).ExcludeAttr;
    Exit;
  end;
  inherited;
end;

procedure TJvSearchAttributes.DefineProperties(Filer: TFiler);
var
  Ancestor: TJvSearchAttributes;
  Attr: DWORD;
begin
  Attr := 0;
  Ancestor := TJvSearchAttributes(Filer.Ancestor);
  if Assigned(Ancestor) then
    Attr := Ancestor.FIncludeAttr;
  Filer.DefineProperty('IncludeAttr', ReadIncludeAttr, WriteIncludeAttr,
    Attr <> FIncludeAttr);
  if Assigned(Ancestor) then
    Attr := Ancestor.FExcludeAttr;
  Filer.DefineProperty('ExcludeAttr', ReadExcludeAttr, WriteExcludeAttr,
    Attr <> FExcludeAttr);
end;

function TJvSearchAttributes.GetAttr(const Index: Integer): TJvAttrFlagKind;
begin
  if FIncludeAttr and Index > 0 then
    Result := tsMustBeSet
  else if FExcludeAttr and Index > 0 then
    Result := tsMustBeUnSet
  else
    Result := tsDontCare;
end;

procedure TJvSearchAttributes.ReadExcludeAttr(Reader: TReader);
begin
  FExcludeAttr := Reader.ReadInteger;
end;

procedure TJvSearchAttributes.ReadIncludeAttr(Reader: TReader);
begin
  FIncludeAttr := Reader.ReadInteger;
end;

procedure TJvSearchAttributes.SetAttr(const Index: Integer;
  Value: TJvAttrFlagKind);
begin
  case Value of
    tsMustBeSet:
      begin
        FIncludeAttr := FIncludeAttr or DWORD(Index);
        FExcludeAttr := FExcludeAttr and not Index;
      end;
    tsMustBeUnSet:
      begin
        FIncludeAttr := FIncludeAttr and not Index;
        FExcludeAttr := FExcludeAttr or DWORD(Index);
      end;
    tsDontCare:
      begin
        FIncludeAttr := FIncludeAttr and not Index;
        FExcludeAttr := FExcludeAttr and not Index;
      end;
  end;
end;

procedure TJvSearchAttributes.WriteExcludeAttr(Writer: TWriter);
begin
  Writer.WriteInteger(FExcludeAttr);
end;

procedure TJvSearchAttributes.WriteIncludeAttr(Writer: TWriter);
begin
  Writer.WriteInteger(FIncludeAttr);
end;

{ TJvSearchParams }

procedure TJvSearchParams.Assign(Source: TPersistent);
var
  Src: TJvSearchParams;
begin
  if Source is TJvSearchParams then
  begin
    Src := TJvSearchParams(Source);
    MaxSize := Src.MaxSize;
    MinSize := Src.MinSize;
    LastChangeBefore := Src.LastChangeBefore;
    LastChangeAfter := Src.LastChangeAfter;
    FileAttributes.Assign(Src.FileAttributes);
    DirAttributes.Assign(Src.DirAttributes);
    SearchTypes := Src.SearchTypes;
    FileMasks.Assign(Src.FileMasks);
    FileMaskSeperator := Src.FileMaskSeperator;
    Exit;
  end;
  inherited;
end;

function TJvSearchParams.Check(const AFindData: TWin32FindData): Boolean;
var
  i: Integer;
  FileName: string;
begin
  Result := False;
  with AFindData do
  begin
    if dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0 then
    begin
      // This is a file
      if not (stSearchFiles in FSearchTypes) then
        Exit;

      if stFileAttr in FSearchTypes then
      begin
        { Note that if you set a flag in both ExcludeAttr and IncludeAttr
          the search always returns False }
        if dwFileAttributes and FileAttributes.ExcludeAttr > 0 then
          Exit;
        if dwFileAttributes and FileAttributes.IncludeAttr <>
          FileAttributes.IncludeAttr then
          Exit;
      end;
    end
    else
    begin
      // This is a dir
      if not (stSearchDirs in FSearchTypes) then
        Exit;

      if stDirAttr in FSearchTypes then
      begin
        { Note that if you set a flag in both ExcludeAttr and IncludeAttr
          the search always returns False }
        if dwFileAttributes and DirAttributes.ExcludeAttr > 0 then
          Exit;
        if dwFileAttributes and DirAttributes.IncludeAttr <>
          DirAttributes.IncludeAttr then
          Exit;
      end;
    end;
    if stMinSize in FSearchTypes then
    begin
      if (nFileSizeHigh < FMinSizeHigh) or
        ((nFileSizeHigh = FMinSizeHigh) and (nFileSizeLow < FMinSizeLow)) then
        Exit;
    end;
    if stMaxSize in FSearchTypes then
    begin
      if (nFileSizeHigh > FMaxSizeHigh) or
        ((nFileSizeHigh = FMaxSizeHigh) and (nFileSizeLow > FMaxSizeLow)) then
        Exit;
    end;
    if stLastChangeAfter in FSearchTypes then
    begin
      if Windows.CompareFileTime(ftLastWriteTime, FLastChangeAfterFT) < 0 then
        Exit;
    end;
    if stLastChangeBefore in FSearchTypes then
    begin
      if Windows.CompareFileTime(ftLastWriteTime, FLastChangeBeforeFT) > 0 then
        Exit;
    end;
    if (stFileMask in FSearchTypes) and (FFileMasks.Count > 0) then
    begin
      { StrMatches in JclStrings.pas is case-sensitive, thus for non case-
        sensitive search we have to do a little trick. The filename is
        upper-cased and compared with masks that are also upper-cased.
        This is a bit clumsy; a better solution would be to do this in
        StrMatches.

        I guess a lot of masks have the format 'mask*' or '*.ext'; so
        if you could specifiy to do a left or right scan in StrMatches
        would be better too. Note that if no char follows a '*', the
        result is always true; this isn't implemented so in StrMatches }

      if stFileMaskCaseSensitive in SearchTypes then
        FileName := cFileName
      else
        FileName := UpperCase(cFileName);

      i := 0;
      while (i < FFileMasks.Count) and
        not JclStrings.StrMatches(FCaseFileMasks[i], FileName) do
        Inc(i);
      if i >= FFileMasks.Count then
        Exit;
    end;
  end;
  Result := True;
end;

constructor TJvSearchParams.Create;
begin
  FFileAttributes := TJvSearchAttributes.Create;
  FDirAttributes := TJvSearchAttributes.Create;
  FFileMasks := TStringList.Create;
  TStringList(FFileMasks).OnChange := FileMasksChange;
  FCaseFileMasks := TStringList.Create;

  { defaults }
  FFileMaskSeperator := ';';
  FSearchTypes := [stFileMask, stSearchFiles];
end;

destructor TJvSearchParams.Destroy;
begin
  FFileAttributes.Free;
  FDirAttributes.Free;
  FFileMasks.Free;
  FCaseFileMasks.Free;
  inherited;
end;

procedure TJvSearchParams.FileMasksChange(Sender: TObject);
begin
  UpdateCaseMasks;
end;

function TJvSearchParams.GetFileMask: string;
begin
  Result := JclStrings.StringsToStr(FFileMasks, FileMaskSeperator);
end;

function TJvSearchParams.GetMaxSize: Int64;
begin
  Int64Rec(Result).Lo := FMaxSizeLow;
  Int64Rec(Result).Hi := FMaxSizeHigh;
end;

function TJvSearchParams.GetMinSize: Int64;
begin
  Int64Rec(Result).Lo := FMinSizeLow;
  Int64Rec(Result).Hi := FMinSizeHigh;
end;

procedure TJvSearchParams.SetDirAttributes(
  const Value: TJvSearchAttributes);
begin
  FDirAttributes.Assign(Value);
end;

procedure TJvSearchParams.SetFileAttributes(
  const Value: TJvSearchAttributes);
begin
  FFileAttributes.Assign(Value);
end;

procedure TJvSearchParams.SetFileMask(const Value: string);
begin
  JclStrings.StrToStrings(Value, FileMaskSeperator, FFileMasks);
end;

procedure TJvSearchParams.SetFileMasks(const Value: TStrings);
begin
  FFileMasks.Assign(Value);
end;

procedure TJvSearchParams.SetLastChangeAfter(const Value: TDateTime);
var
  DosFileTime: Longint;
  LocalFileTime: TFileTime;
begin
  { Value must be >= 1-1-1980 }
  DosFileTime := DateTimeToDosDateTime(Value);
  if not Windows.DosDateTimeToFileTime(LongRec(DosFileTime).Hi,
    LongRec(DosFileTime).Lo, LocalFileTime) or
    not Windows.LocalFileTimeToFileTime(LocalFileTime, FLastChangeAfterFT) then
    RaiseLastOSError;

  FLastChangeAfter := Value;
end;

procedure TJvSearchParams.SetLastChangeBefore(const Value: TDateTime);
var
  DosFileTime: Longint;
  LocalFileTime: TFileTime;
begin
  { Value must be >= 1-1-1980 }
  DosFileTime := DateTimeToDosDateTime(Value);
  if not Windows.DosDateTimeToFileTime(LongRec(DosFileTime).Hi,
    LongRec(DosFileTime).Lo, LocalFileTime) or
    not Windows.LocalFileTimeToFileTime(LocalFileTime, FLastChangeBeforeFT) then
    RaiseLastOSError;

  FLastChangeBefore := Value;
end;

procedure TJvSearchParams.SetMaxSize(const Value: Int64);
begin
  FMaxSizeHigh := Int64Rec(Value).Hi;
  FMaxSizeLow := Int64Rec(Value).Lo;
end;

procedure TJvSearchParams.SetMinSize(const Value: Int64);
begin
  FMinSizeHigh := Int64Rec(Value).Hi;
  FMinSizeLow := Int64Rec(Value).Lo;
end;

procedure TJvSearchParams.SetSearchTypes(const Value: TJvSearchTypes);
var
  ChangedValues: TJvSearchTypes;
begin
  if FSearchTypes = Value then
    Exit;

  ChangedValues := FSearchTypes + Value - (FSearchTypes * Value);
  FSearchTypes := Value;

  if stFileMaskCaseSensitive in ChangedValues then
    UpdateCaseMasks;
end;

procedure TJvSearchParams.UpdateCaseMasks;
var
  i: Integer;
begin
  FCaseFileMasks.Assign(FFileMasks);

  if not (stFileMaskCaseSensitive in SearchTypes) then
    for i := 0 to FCaseFileMasks.Count - 1 do
      FCaseFileMasks[i] := UpperCase(FCaseFileMasks[i]);
end;

end.

