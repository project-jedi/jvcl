{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSearchFiles.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):
David Frauzel (DF)
Remko Bonte

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc}

unit JvSearchFiles;

{ Wrapper for a file search engine. }

interface

uses
  Classes, SysUtils,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  JvComponent, JvJCLUtils;

const
  { Taken from WinNT.h }
  FILE_ATTRIBUTE_SPARSE_FILE = $200;
  {$EXTERNALSYM FILE_ATTRIBUTE_SPARSE_FILE}

  FILE_ATTRIBUTE_REPARSE_POINT = $400;
  {$EXTERNALSYM FILE_ATTRIBUTE_REPARSE_POINT}

  FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = $2000;
  {$EXTERNALSYM FILE_ATTRIBUTE_NOT_CONTENT_INDEXED}

  FILE_ATTRIBUTE_ENCRYPTED = $4000;
  {$EXTERNALSYM FILE_ATTRIBUTE_ENCRYPTED}

type
  TJvAttrFlagKind = (tsMustBeSet, tsDontCare, tsMustBeUnSet);
  TJvDirOption = (doExcludeSubDirs, doIncludeSubDirs, doExcludeInvalidDirs,
    doExcludeCompleteInvalidDirs);
  { doExcludeSubDirs
      Only search in root directory.
    doIncludeSubDirs
      Search in root directory and it's sub-directories.
    doExcludeInvalidDirs
      Search in root directory and it's sub-directories; do not search in
      an invalid directory, but do search in the sub-directories of an
      invalid directory.
    doExcludeCompleteInvalidDirs
      Search in root directory and it's sub-directories; do not search in
      an invalid directory, and the sub-directories of an invalid directory.

    Invalid directory = directory with params that doesn't agree with the
      params specified by DirParams.
  }

  TJvSearchOption = (soAllowDuplicates, soCheckRootDirValid,
    soExcludeFilesInRootDir, soOwnerData, soSearchDirs, soSearchFiles, soSorted,
    soStripDirs);
  TJvSearchOptions = set of TJvSearchOption;
  { soAllowDuplicates
      Allow duplicate file/dir names in property Files and Directories.
    soCheckRootDirValid
      Check if the root-directory is valid; Must DirOption must be equal to
      doExcludeSubDirs or doExcludeCompleteInvalidDirs, otherwise this flag is
      ignored.
    soExcludeFilesInRootDir
      Do not search in the root directory.
    soOwnerData
      Do not fill property Files and Directories while searching
    soSearchDirs
      Search for directories; ie trigger OnFindDirectory event and update
      totals [TotalDirectories, TotalFileSize] when a valid directory is found.
    soSearchFiles
      Search for files; ie trigger OnFindFile event and update totals
      [TotalFileSize, TotalFiles] when a valid file is found.
    soSorted
      Keep the values in property Files and Directories sorted.
    soStripDirs
      Strip the path of a dir/file name before inserting it in property
      Files and Directories
  }

  TJvSearchType = (stAttribute, stFileMask, stFileMaskCaseSensitive,
    stLastChangeAfter, stLastChangeBefore, stMaxSize, stMinSize);
  TJvSearchTypes = set of TJvSearchType;

  TJvFileSearchEvent = procedure(Sender: TObject; const AName: string) of object;
  TJvSearchFilesError = procedure(Sender: TObject; var Handled: Boolean) of object;
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
    { DefineProperties is used to publish properties IncludeAttr and
      ExcludeAttr }
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
      FILE_ATTRIBUTE_NOT_CONTENT_INDEXED read GetAttr write SetAttr stored False;
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
    FSearchTypes: TJvSearchTypes;
    FFileMasks: TStringList;
    FCaseFileMasks: TStringList;
    FFileMaskSeperator: Char;
    FAttributes: TJvSearchAttributes;
    procedure FileMasksChange(Sender: TObject);
    function GetFileMask: string;
    function GetMaxSize: Int64;
    function GetMinSize: Int64;
    function GetFileMasks: TStrings;
    function IsLastChangeAfterStored: Boolean;
    function IsLastChangeBeforeStored: Boolean;
    procedure SetAttributes(const Value: TJvSearchAttributes);
    procedure SetFileMasks(const Value: TStrings);
    procedure SetFileMask(const Value: string);
    procedure SetLastChangeAfter(const Value: TDateTime);
    procedure SetLastChangeBefore(const Value: TDateTime);
    procedure SetMaxSize(const Value: Int64);
    procedure SetMinSize(const Value: Int64);
    procedure SetSearchTypes(const Value: TJvSearchTypes);
    procedure UpdateCaseMasks;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Check(const AFindData: TWin32FindData): Boolean;
    property FileMask: string read GetFileMask write SetFileMask;
    property FileMaskSeperator: Char read FFileMaskSeperator write
      FFileMaskSeperator default ';';
  published
    property Attributes: TJvSearchAttributes read FAttributes write SetAttributes;
    property SearchTypes: TJvSearchTypes read FSearchTypes write SetSearchTypes default [];
    property MinSize: Int64 read GetMinSize write SetMinSize;
    property MaxSize: Int64 read GetMaxSize write SetMaxSize;
    property LastChangeAfter: TDateTime read FLastChangeAfter write SetLastChangeAfter
      stored IsLastChangeAfterStored;
    property LastChangeBefore: TDateTime read FLastChangeBefore write SetLastChangeBefore
      stored IsLastChangeBeforeStored;
    property FileMasks: TStrings read GetFileMasks write SetFileMasks;
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
    FOnProgress: TNotifyEvent;
    FDirectories: TStringList;
    FFiles: TStringList;
    FFindData: TWin32FindData;
    FAborting: Boolean;
    FErrorResponse: TJvErrorResponse;
    FOnCheck: TJvCheckEvent;
    FOnBeginScanDir: TJvFileSearchEvent;
    FDirOption: TJvDirOption;
    FDirParams: TJvSearchParams;
    FFileParams: TJvSearchParams;
    FRecurseDepth: Integer;
    function GetIsRootDirValid: Boolean;
    function GetIsDepthAllowed(const ADepth: Integer): Boolean;
    function GetDirectories: TStrings;
    function GetFiles: TStrings;
    procedure SetDirParams(const Value: TJvSearchParams);
    procedure SetFileParams(const Value: TJvSearchParams);
    procedure SetOptions(const Value: TJvSearchOptions);
  protected
    procedure DoBeginScanDir(const ADirName: string); virtual;
    procedure DoFindFile(const APath: string); virtual;
    procedure DoFindDir(const APath: string); virtual;
    procedure DoAbort; virtual;
    procedure DoProgress; virtual;
    function DoCheckDir: Boolean; virtual;
    function DoCheckFile: Boolean; virtual;
    function HandleError: Boolean; virtual;
    procedure Init; virtual;
    function EnumFiles(const ADirectoryName: string; Dirs: TStrings;
      const Search: Boolean): Boolean;
    function InternalSearch(const ADirectoryName: string;
      const Search: Boolean; var ADepth: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Abort;
    function Search: Boolean;
    property FindData: TWin32FindData read FFindData;
    property Files: TStrings read GetFiles;
    property Directories: TStrings read GetDirectories;
    property IsRootDirValid: Boolean read GetIsRootDirValid;
    property Searching: Boolean read FSearching;
    property TotalDirectories: Integer read FTotalDirectories;
    property TotalFileSize: Int64 read FTotalFileSize;
    property TotalFiles: Integer read FTotalFiles;
  published
    property DirOption: TJvDirOption read FDirOption write FDirOption default doIncludeSubDirs;
    // RecurseDepth sets the number of subfolders to search. If 0, all subfolders
    // are searched (as long as doIncludeSubDirs is true)
    property RecurseDepth: Integer read FRecurseDepth write FRecurseDepth default 0;
    property RootDirectory: string read FRootDirectory write FRootDirectory;
    property Options: TJvSearchOptions read FOptions write SetOptions default [soSearchFiles];
    property ErrorResponse: TJvErrorResponse read FErrorResponse write
      FErrorResponse default erAbort;
    property DirParams: TJvSearchParams read FDirParams write SetDirParams;
    property FileParams: TJvSearchParams read FFileParams write SetFileParams;
    property OnBeginScanDir: TJvFileSearchEvent read FOnBeginScanDir write
      FOnBeginScanDir;
    property OnFindFile: TJvFileSearchEvent read FOnFindFile write FOnFindFile;
    property OnFindDirectory: TJvFileSearchEvent read FOnFindDirectory write
      FOnFindDirectory;
    property OnAbort: TNotifyEvent read FOnAbort write FOnAbort;
    property OnError: TJvSearchFilesError read FOnError write FOnError;
    { Maybe add a flag to Options to disable OnCheck }
    property OnCheck: TJvCheckEvent read FOnCheck write FOnCheck;
    // (rom) replaced ProcessMessages with OnProgress event
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  end;

implementation

uses
  JclStrings, JclDateTime;

{ Maybe TJvSearchFiles should be implemented with FindFirst, FindNext.
  There isn't a good reason to use FindFirstFile, FindNextFile instead of
  FindFirst, FindNext; except to prevent a little overhead perhaps. }

const
  CDate1_1_1980 = 29221;

//=== TJvSearchFiles =========================================================

constructor TJvSearchFiles.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFiles := TStringList.Create;
  FDirectories := TStringList.Create;
  FDirParams := TJvSearchParams.Create;
  FFileParams := TJvSearchParams.Create;

  { defaults }
  Options := [soSearchFiles];
  DirOption := doIncludeSubDirs;
  ErrorResponse := erAbort;
  //FFileParams.SearchTypes := [stFileMask];
end;

destructor TJvSearchFiles.Destroy;
begin
  FFiles.Free;
  FDirectories.Free;
  FFileParams.Free;
  FDirParams.Free;
  inherited Destroy;
end;

procedure TJvSearchFiles.Abort;
begin
  if not FSearching then
    Exit;
  FAborting := True;
  DoAbort;
end;

procedure TJvSearchFiles.DoAbort;
begin
  if Assigned(FOnAbort) then
    FOnAbort(Self);
end;

procedure TJvSearchFiles.DoProgress;
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self);
end;

procedure TJvSearchFiles.DoBeginScanDir(const ADirName: string);
begin
  if Assigned(FOnBeginScanDir) then
    FOnBeginScanDir(Self, ADirName);
end;

function TJvSearchFiles.DoCheckDir: Boolean;
begin
  if Assigned(FOnCheck) then
  begin
    Result := False;
    FOnCheck(Self, Result);
  end
  else
    Result := FDirParams.Check(FFindData)
end;

function TJvSearchFiles.DoCheckFile: Boolean;
begin
  if Assigned(FOnCheck) then
  begin
    Result := False;
    FOnCheck(Self, Result);
  end
  else
    Result := FFileParams.Check(FFindData)
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
      DirName := cFileName
    else
      DirName := APath + cFileName;

    if not (soOwnerData in Options) then
      Directories.Add(DirName);

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
      FileName := cFileName
    else
      FileName := APath + cFileName;

    if not (soOwnerData in Options) then
      Files.Add(FileName);

    Int64Rec(FileSize).Lo := nFileSizeLow;
    Int64Rec(FileSize).Hi := nFileSizeHigh;
    Inc(FTotalFileSize, FileSize);

    { NOTE: soStripDirs also applies to the event }
    if Assigned(FOnFindFile) then
      FOnFindFile(Self, FileName);
  end;
end;

function TJvSearchFiles.EnumFiles(const ADirectoryName: string;
  Dirs: TStrings; const Search: Boolean): Boolean;
var
  Handle: THandle;
  Finished: Boolean;
  DirOK: Boolean;
begin
  DoBeginScanDir(ADirectoryName);

  { Always scan the full directory - ie use * as mask - this seems faster
    then first using a mask, and then scanning the directory for subdirs }
  Handle := FindFirstFile(PChar(ADirectoryName + '*'), FFindData);
  Result := Handle <> INVALID_HANDLE_VALUE;
  if not Result then
  begin
    Result := GetLastError in [ERROR_FILE_NOT_FOUND, ERROR_ACCESS_DENIED];;
    Exit;
  end;

  Finished := False;
  try
    while not Finished do
    begin
      // (p3) no need to bring in the Forms unit for this:
      if not IsConsole then
        DoProgress;
      { After DoProgress, the user can have called Abort,
        so check it }
      if FAborting then
      begin
        Result := False;
        Exit;
      end;

      with FFindData do
        { Is it a directory? }
        if dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY > 0 then
        begin
          { Filter out '.' and '..'
            Other dir names can't begin with a '.' }

          {                         | Event | AddDir | SearchInDir
           -----------------------------------------------------------------
            doExcludeSubDirs        |
              True                  |   Y       N           N
              False                 |   N       N           N
            doIncludeSubDirs        |
              True                  |   Y       Y           Y
              False                 |   N       Y           Y
            doExcludeInvalidDirs    |
              True                  |   Y       Y           Y
              False                 |   N       Y           N
            doExcludeCompleteInvalidDirs |
              True                  |   Y       Y           Y
              False                 |   N       N           N
          }
          if cFileName[0] <> '.' then
            { Use case to prevent unnecessary calls to DoCheckDir }
            case DirOption of
              doExcludeSubDirs, doIncludeSubDirs:
                begin
                  if Search and (soSearchDirs in Options) and DoCheckDir then
                    DoFindDir(ADirectoryName);
                  if DirOption = doIncludeSubDirs then
                    Dirs.AddObject(cFileName, TObject(True))
                end;
              doExcludeInvalidDirs, doExcludeCompleteInvalidDirs:
                begin
                  DirOK := DoCheckDir;
                  if Search and (soSearchDirs in Options) and DirOK then
                    DoFindDir(ADirectoryName);

                  if (DirOption = doExcludeInvalidDirs) or DirOK then
                    Dirs.AddObject(cFileName, TObject(DirOK));
                end;
            end;
        end
        else
          if Search and (soSearchFiles in Options) and DoCheckFile then
          DoFindFile(ADirectoryName);

      if not FindNextFile(Handle, FFindData) then
      begin
        Finished := True;
        Result := GetLastError = ERROR_NO_MORE_FILES;
      end;
    end;
  finally
    Result := FindClose(Handle) and Result;
  end;
end;

function TJvSearchFiles.GetIsRootDirValid: Boolean;
var
  Handle: THandle;
begin
  Handle := FindFirstFile(PChar(ExcludeTrailingPathDelimiter(FRootDirectory)),
    FFindData);
  Result := Handle <> INVALID_HANDLE_VALUE;
  if not Result then
    Exit;

  try
    with FFindData do
      Result := (dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY > 0) and
        (cFileName[0] <> '.') and DoCheckDir;
  finally
    FindClose(Handle);
  end;
end;

function TJvSearchFiles.GetIsDepthAllowed(const ADepth: Integer): Boolean;
begin
  Result := (FRecurseDepth = 0) or (ADepth <= FRecurseDepth);
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

function TJvSearchFiles.GetDirectories: TStrings;
begin
  Result := FDirectories;
end;

function TJvSearchFiles.GetFiles: TStrings;
begin
  Result := FFiles;
end;

procedure TJvSearchFiles.Init;
begin
  FTotalFileSize := 0;
  FTotalDirectories := 0;
  FTotalFiles := 0;
  Directories.Clear;
  Files.Clear;
  FAborting := False;
end;

function TJvSearchFiles.InternalSearch(const ADirectoryName: string; const Search: Boolean;
  var ADepth: Integer): Boolean;
var
  List: TStringList;
  DirSep: string;
  I: Integer;
begin
  List := TStringList.Create;
  try
    DirSep := IncludeTrailingPathDelimiter(ADirectoryName);

    Result := EnumFiles(DirSep, List, Search) or HandleError;
    if not Result then
      Exit;

    { DO NOT set Result := False; the search should continue, this is not an error. }
    Inc(ADepth); 
    if not GetIsDepthAllowed(ADepth) then
      Exit;

    { I think it would be better to do no recursion; Don't know if it can
      be easy implemented - if you want to keep the depth first search -
      and without doing a lot of TList moves }
    for I := 0 to List.Count - 1 do
    begin
      Result := InternalSearch(DirSep + List[I], List.Objects[I] <> nil, ADepth);
      if not Result then
        Exit;
    end;
  finally
    List.Free;
    Dec(ADepth); 
  end;
end;

function TJvSearchFiles.Search: Boolean;
var
  SearchInRootDir: Boolean;
  ADepth: Integer;
begin
  Result := False;
  if Searching then
    Exit;

  Init;

  FSearching := True;
  try
    { Search in root directory?

                            | soExcludeFiles | soCheckRootDirValid | Else
                            |  InRootDir     |                     |
                            |                |  Valid  | not Valid |
    --------------------------------------------------------------------------
    doExcludeSubDirs        |   No Search    |  True   | No Search | True
    doIncludeSubDirs        |   False        |  True   | False     | True
    doExcludeInvalidDirs    |   False        |  True   | False     | True
    doExcludeCompleteInvalidDirs |   False   |  True   | No Search | True
    }
    SearchInRootDir := not (soExcludeFilesInRootDir in Options)
      and (not (soCheckRootDirValid in Options) or IsRootDirValid);

    if not SearchInRootDir and ((DirOption = doExcludeSubDirs) or
      ((DirOption = doExcludeCompleteInvalidDirs) and
      (soCheckRootDirValid in Options))) then
    begin
      Result := True;
      Exit;
    end;

    ADepth := 0;
    Result := InternalSearch(FRootDirectory, SearchInRootDir, ADepth);
  finally
    FSearching := False;
  end;
end;

procedure TJvSearchFiles.SetDirParams(const Value: TJvSearchParams);
begin
  FDirParams.Assign(Value);
end;

procedure TJvSearchFiles.SetFileParams(const Value: TJvSearchParams);
begin
  FFileParams.Assign(Value);
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
  // (rom) even better the search should use a local copy which stays unchanged

  if FOptions <> Value then
  begin
    ChangedOptions := FOptions + Value - (FOptions * Value);

    FOptions := Value;

    if soSorted in ChangedOptions then
    begin
      FDirectories.Sorted := soSorted in FOptions;
      FFiles.Sorted := soSorted in FOptions;
    end;

    if soAllowDuplicates in ChangedOptions then
    begin
      if soAllowDuplicates in FOptions then
      begin
        FDirectories.Duplicates := dupAccept;
        FFiles.Duplicates := dupAccept;
      end
      else
      begin
        FDirectories.Duplicates := dupIgnore;
        FFiles.Duplicates := dupIgnore;
      end;
    end;
    // soStripDirs; soIncludeSubDirs; soOwnerData
  end;
end;

//=== TJvSearchAttributes ====================================================

procedure TJvSearchAttributes.Assign(Source: TPersistent);
begin
  if Source is TJvSearchAttributes then
  begin
    IncludeAttr := TJvSearchAttributes(Source).IncludeAttr;
    ExcludeAttr := TJvSearchAttributes(Source).ExcludeAttr;
  end
  else
    inherited Assign(Source);
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
  else
    if FExcludeAttr and Index > 0 then
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

//=== TJvSearchParams ========================================================

constructor TJvSearchParams.Create;
begin
  // (rom) added inherited Create
  inherited Create;
  FAttributes := TJvSearchAttributes.Create;
  FFileMasks := TStringList.Create;
  FFileMasks.OnChange := FileMasksChange;
  FCaseFileMasks := TStringList.Create;

  { defaults }
  FFileMaskSeperator := ';';
  { Set to 1-1-1980 }
  FLastChangeBefore := CDate1_1_1980;
  FLastChangeAfter := CDate1_1_1980;
end;

destructor TJvSearchParams.Destroy;
begin
  FAttributes.Free;
  FFileMasks.Free;
  FCaseFileMasks.Free;
  inherited Destroy;
end;

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
    SearchTypes := Src.SearchTypes;
    FileMasks.Assign(Src.FileMasks);
    FileMaskSeperator := Src.FileMaskSeperator;
    Attributes.Assign(Src.Attributes);
  end
  else
    inherited Assign(Source);
end;

function TJvSearchParams.Check(const AFindData: TWin32FindData): Boolean;
var
  I: Integer;
  FileName: string;
begin
  Result := False;
  with AFindData do
  begin
    if stAttribute in FSearchTypes then
    begin
      { Note that if you set a flag in both ExcludeAttr and IncludeAttr
        the search always returns False }
      if dwFileAttributes and Attributes.ExcludeAttr > 0 then
        Exit;
      if dwFileAttributes and Attributes.IncludeAttr <> Attributes.IncludeAttr then
        Exit;
    end;

    if stMinSize in FSearchTypes then
      if (nFileSizeHigh < FMinSizeHigh) or
        ((nFileSizeHigh = FMinSizeHigh) and (nFileSizeLow < FMinSizeLow)) then
        Exit;
    if stMaxSize in FSearchTypes then
      if (nFileSizeHigh > FMaxSizeHigh) or
        ((nFileSizeHigh = FMaxSizeHigh) and (nFileSizeLow > FMaxSizeLow)) then
        Exit;
    if stLastChangeAfter in FSearchTypes then
      if CompareFileTime(ftLastWriteTime, FLastChangeAfterFT) < 0 then
        Exit;
    if stLastChangeBefore in FSearchTypes then
      if CompareFileTime(ftLastWriteTime, FLastChangeBeforeFT) > 0 then
        Exit;
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
        FileName := AnsiUpperCase(cFileName);

      I := 0;
      while (I < FFileMasks.Count) and
        not JclStrings.StrMatches(FCaseFileMasks[I], FileName) do
        Inc(I);
      if I >= FFileMasks.Count then
        Exit;
    end;
  end;
  Result := True;
end;

procedure TJvSearchParams.FileMasksChange(Sender: TObject);
begin
  UpdateCaseMasks;
end;

function TJvSearchParams.GetFileMask: string;
begin
  Result := JclStrings.StringsToStr(FileMasks, FileMaskSeperator);
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

function TJvSearchParams.GetFileMasks: TStrings;
begin
  Result := FFileMasks;
end;

function TJvSearchParams.IsLastChangeAfterStored: Boolean;
begin
  Result := FLastChangeBefore <> CDate1_1_1980;
end;

function TJvSearchParams.IsLastChangeBeforeStored: Boolean;
begin
  Result := FLastChangeBefore <> CDate1_1_1980;
end;

procedure TJvSearchParams.SetAttributes(const Value: TJvSearchAttributes);
begin
  FAttributes.Assign(Value);
end;

procedure TJvSearchParams.SetFileMask(const Value: string);
begin
  JclStrings.StrToStrings(Value, FileMaskSeperator, FileMasks);
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
    not LocalFileTimeToFileTime(LocalFileTime, FLastChangeAfterFT) then
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
    not LocalFileTimeToFileTime(LocalFileTime, FLastChangeBeforeFT) then
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
  I: Integer;
begin
  FCaseFileMasks.Assign(FileMasks);

  if not (stFileMaskCaseSensitive in SearchTypes) then
    for I := 0 to FCaseFileMasks.Count - 1 do
      FCaseFileMasks[I] := AnsiUpperCase(FCaseFileMasks[I]);
end;

end.

