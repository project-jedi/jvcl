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

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}
{$IFDEF DELPHI6_UP}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}
{$IFDEF LINUX}
This unit is only supported on Windows!
{$ENDIF}

{ Wrapper for a file search engine. }
unit JvSearchFiles;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, FileCtrl,
  JvComponent;

type
  TJvSearchOption = (soReadOnly, soHidden, soSysFile, soVolumeID, soDirectory, soArchive, soAnyFile);
  TJvSearchOptions = set of TJvSearchOption;
  TJvSearchEvent = procedure(Sender: TObject; SearchRec: TWin32FindData; Path: string) of object;

  TJvSearchFiles = class(TJvComponent)
  private
    { Private declarations }
    FTotalFileSize: Cardinal;
    FItems: TStrings;
    FDirs: TStrings;
    FMask: string;
    FPath: string;
    FSorted, FAllowDuplicates: boolean;
    FOptions: TJvSearchOptions;
    FStripDirs: boolean;
    FDoSubs: boolean;
    FAbort: boolean;
    FMaskList: TStringlist;
    FMaskDelim: char;
    FOnFindFile: TJvSearchEvent;
    FOnFindDir: TJvSearchEvent;
    FOnClose: TNotifyEvent;
    FOnAbort: TNotifyEvent;
    FOwnerData: boolean;
    FSearching: boolean;
    procedure BuildMaskList;
    procedure SetMask(Value: string);
    procedure SetOptions(Value: TJvSearchOptions);
    procedure SetDoSubs(Value: boolean);
    procedure SetStripDirs(Value: boolean);
    procedure FindAllFiles(Path, Filename: string; Flags: integer);
    procedure FindAllMasks(Path: string; Filemasks: Tstrings; Flags: integer);
    function GetDrive: char;
    procedure SetDrive(Value: Char);
    procedure SetOwnerData(const Value: boolean);
  protected
    { Protected declarations }
    procedure FindDir(FindData: TWin32FindData; Path: string); virtual;
    procedure FindFile(FindData: TWin32FindData; Path: string); virtual;
    procedure DoClose; virtual;
    procedure DoAbort; virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Search: boolean;
    procedure Abort;
    property Files: TStrings read FItems;
    property Directories: TStrings read FDirs;
    property TotalFileSize: Cardinal read FTotalFileSize;
    property Searching: boolean read FSearching;
  published
    { Published declarations }
    property FileMask: string read FMask write SetMask;
    property FilePath: string read FPath write FPath;
    property OwnerData: boolean read FOwnerData write SetOwnerData;
    property Sorted: boolean read FSorted write FSorted default false;
    property AllowDuplicates: boolean read FAllowDuplicates write FAllowDuplicates default true;
    property FileMaskDelimiter: char read FMaskDelim write FMaskDelim default ';';
    property Options: TJvSearchOptions read FOptions write SetOptions default [soAnyFile, soDirectory];
    property StripDirs: boolean read FStripDirs write SetStripDirs default False;
    property IncludeSubDirs: boolean read FDoSubs write SetDoSubs default True;
    property OnFindFile: TJvSearchEvent read FOnFindFile write FOnFindFile;
    property OnFindDirectory: TJvSearchEvent read FOnFindDir write FOnFindDir;
    property OnAbort: TNotifyEvent read FOnAbort write FOnAbort;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

  { tries to create a valid mask from the input mask }
function MakeValidMask(const Mask: string): string;

implementation

{ adds backslash to end of path if not already there }

function AddPathBackslash(Path: string): string;
begin
  Result := Path;
  if (Length(Path) > 1) and (AnsiLastChar(Path) <> '\') then
    Result := Path + '\';
end;

function MakeValidMask(const Mask: string): string;
const
  cInvalid = ['\', '/', ':', '"', '<', '>', '|'];
var l, i: integer; HasStar: boolean;
begin
  l := Length(Mask);
  Result := '';
  HasStar := false;
  for i := 1 to l do
  begin
    if Mask[i] in cInvalid then
      Continue;
    if (Mask[i] = '*') and HasStar then
      Break
    else
      HasStar := true;
    if Mask[i] = '.' then
      HasStar := false;
    //    if Mask[i] in [',',';'] then Exit;
      Result := Result + Mask[i];
  end;
end;

{ Return a proper current dir. If no drive or path is specified in Filename , a
drive designation will be prepended otherwise Filename is returned unchanged}

function GetProperCurrentDir(Filename: string): string;
begin
  Result := Filename;
  if SetCurrentDir(ExtractFileDir(Filename)) then
    Result := Filename
  else if ExtractFilePath(Filename) = '' then
    Result := ExpandFileName(Filename);
end;

constructor TJvSearchFiles.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TStringList.Create;
  FDirs := TStringList.Create;
  FMaskList := TStringList.Create;
  FDoSubs := True;
  FStripDirs := False;
  FAbort := False;
  FOptions := [soAnyFile, soDirectory];
  FAllowDuplicates := true;
  FMaskDelim := ';';
end;

destructor TJvSearchFiles.Destroy;
begin
  FItems.Clear;
  FDirs.Clear;
  FItems.Free;
  FDirs.Free;
  FMaskList.Free;
  inherited Destroy;
end;

function TJvSearchFiles.Search: boolean;
const FOpts: array[TJvSearchOption] of integer = (faReadOnly, faHidden, faSysFile,
    faVolumeID, faDirectory, faArchive, faAnyFile);

var FFlags: integer; i: TJvSearchOption;
var NewDrive, CurDrive: char;
begin
  if (FMaskList.Count = 0) then
    raise Exception.Create('No filemask specified');
  FSearching := true;
  FFlags := 0;
  FAbort := False;
  FItems.Clear;
  FTotalFileSize := 0;
  if not DirectoryExists(FPath) then
    raise Exception.CreateFmt('%s does not exist!', [FPath]);
  for i := Low(FOpts) to High(FOpts) do
    if i in FOptions then
      FFlags := FFlags or FOpts[I];

  //  FMask := GetProperCurrentDir(FMask);
  CurDrive := GetDrive;
  NewDrive := ExtractFileDir(FPath)[1];
  if (NewDrive <> '') and (NewDrive <> CurDrive) then
    SetDrive(NewDrive);

  //  FindAllFiles(ExtractFilePath(FMask),ExtractFilename(FMask),FFlags);
  if FSorted then
  begin
    TStringlist(FDirs).Sorted := true;
    TStringlist(FItems).Sorted := true;
    if not FAllowDuplicates then
    begin
      TStringlist(FDirs).Duplicates := dupIgnore;
      TStringlist(FItems).Duplicates := dupIgnore;
    end;
  end;
  FPath := AddPathBackSlash(FPath);
  FindAllMasks(FPath, FMaskList, FFlags);
  SetDrive(CurDrive);
  DoClose;
  Result := True;
  FSearching := false;
end;

procedure TJvSearchFiles.Abort;
begin
  FAbort := True;
  DoAbort;
  FSearching := false;
end;

procedure TJvSearchFiles.SetMask(Value: string);
begin
  if FMask <> Value then
  begin
    FMask := Value;
    BuildMaskList;
  end;
end;

procedure TJvSearchFiles.BuildMaskList;
var tmp: string; i: integer;
begin
  FMaskList.Clear;
  if FMask = '' then
    Exit;
  tmp := FMask;
  i := 1;
  while i <= Length(tmp) do
  begin
    if tmp[i] = FMaskDelim then
    begin
      FMaskList.Add(Copy(tmp, 1, i - 1));
      tmp := Copy(tmp, i + 1, MaxInt);
      i := 0;
    end;
    Inc(i);
  end;
  if tmp <> '' then
    FMaskList.Add(tmp);
end;

procedure TJvSearchFiles.SetOptions(Value: TJvSearchOptions);
begin
  if FOptions <> Value then
    FOptions := Value;
end;

procedure TJvSearchFiles.SetDoSubs(Value: boolean);
begin
  if FDoSubs <> Value then
    FDoSubs := Value;
end;

procedure TJvSearchFiles.SetStripDirs(Value: boolean);
begin
  if FStripDirs <> Value then
    FStripDirs := Value;
end;

procedure TJvSearchFiles.FindAllMasks(Path: string; Filemasks: Tstrings; Flags: integer);
var FHandle: THandle; FindData: TWin32FindData; Cont: bool; i: integer;
begin
  { do this dir (files only) }
  for i := 0 to Filemasks.Count - 1 do
  begin
    FHandle := FindFirstFile(PChar(Path + Filemasks[i]), FindData);
    try
      Cont := FHandle <> INVALID_HANDLE_VALUE;
      while Cont do
      begin
        Application.ProcessMessages;
        if FAbort then
        begin
          if FHandle <> INVALID_HANDLE_VALUE then
            Windows.FindClose(FHandle);
          FHandle := INVALID_HANDLE_VALUE;
          Exit;
        end;
        if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0) and (FindData.dwFileAttributes and Flags > 0) then
        begin
          if not FOwnerData then
          begin
            if FStripDirs then
              FItems.Add(FindData.cFilename)
            else
              FItems.Add(Path + FindData.cFilename);
          end;
          FTotalFileSize := FTotalFileSize + FindData.nFileSizeHigh + FindData.nFileSizeLow;
          FindFile(FindData, Path);
        end;
        Cont := Windows.FindNextFile(FHandle, FindData);
      end;
    finally
      if FHandle <> INVALID_HANDLE_VALUE then
        Windows.FindClose(FHandle);
    end;
  end;
  if FDoSubs then
  begin
    { do subdirs: skip true files }
    FHandle := FindFirstFile(PChar(Path + '*.*'), FindData);
    try
      Cont := FHandle <> INVALID_HANDLE_VALUE;
      while Cont do
      begin
        Application.ProcessMessages;
        if FAbort then
        begin
          if FHandle <> INVALID_HANDLE_VALUE then
            Windows.FindClose(FHandle);
          FHandle := INVALID_HANDLE_VALUE;
          Exit;
        end;
        if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY > 0)
          and (FindData.cFileName[0] <> '.') and (FindData.dwFileAttributes and Flags > 0) then
        begin
          if not FOwnerData then
            FDirs.Add(Path + FindData.cFilename);
          FindDir(FindData, Path);
          FTotalFileSize := FTotalFileSize + FindData.nFileSizeHigh + FindData.nFileSizeLow;
          FindAllMasks(Path + FindData.cFilename + '\', Filemasks, Flags);
        end;
        Cont := Windows.FindNextFile(FHandle, FindData);
      end;
    finally
      if FHandle <> INVALID_HANDLE_VALUE then
        Windows.FindClose(FHandle);
    end;
  end;
end; //

procedure TJvSearchFiles.FindAllFiles(Path, Filename: string; Flags: integer);
var FHandle: THandle; FindData: TWin32FindData; Cont: bool;
begin
  if FDoSubs then
  begin
    { do subdirs: skip true files }
    FHandle := FindFirstFile(PChar(Path + '*.*'), FindData);
    try
      Cont := FHandle <> INVALID_HANDLE_VALUE;
      while Cont do
      begin
        Application.ProcessMessages;
        if FAbort then
        begin
          if FHandle <> INVALID_HANDLE_VALUE then
            Windows.FindClose(FHandle);
          Exit;
        end;
        if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY > 0)
          and (FindData.cFileName[0] <> '.') and (FindData.dwFileAttributes and Flags > 0) then
        begin
          if not FOwnerData then
            FDirs.Add(Path + FindData.cFilename);
          FindDir(FindData, Path);
          FindAllFiles(Path + FindData.cFilename + '\', Filename, Flags);
        end;
        Cont := Windows.FindNextFile(FHandle, FindData);
      end;
    finally
      if FHandle <> INVALID_HANDLE_VALUE then
        Windows.FindClose(FHandle);
    end;
  end;

  { do this dir (files only) }
  FHandle := FindFirstFile(PChar(Path + Filename), FindData);
  try
    Cont := FHandle <> INVALID_HANDLE_VALUE;
    while Cont do
    begin
      Application.ProcessMessages;
      if FAbort then
      begin
        if FHandle <> INVALID_HANDLE_VALUE then
          Windows.FindClose(FHandle);
        Exit;
      end;
      if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0) and (FindData.dwFileAttributes and Flags > 0) then
      begin
        if not FOwnerData then
        begin
          if FStripDirs then
            FItems.Add(FindData.cFilename)
          else
            FItems.Add(Path + FindData.cFilename);
        end;
        FindFile(FindData, Path);
      end;
      Cont := Windows.FindNextFile(FHandle, FindData);
    end;
  finally
    if FHandle <> INVALID_HANDLE_VALUE then
      Windows.FindClose(FHandle);
  end;
end;

procedure TJvSearchFiles.FindDir(FindData: TWin32FindData; Path: string);
begin
  if Assigned(FOnFindDir) then
    FOnFindDir(self, FindData, Path);
end;

procedure TJvSearchFiles.FindFile(FindData: TWin32FindData; Path: string);
begin
  if Assigned(FOnFindFile) then
    FOnFindFile(self, FindData, Path);
end;

procedure TJvSearchFiles.DoClose;
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TJvSearchFiles.DoAbort;
begin
  if Assigned(FOnAbort) then
    FOnAbort(Self);
end;

function TJvSearchFiles.GetDrive: char;
begin
  Result := GetCurrentDir[1];
end;

procedure TJvSearchFiles.SetDrive(Value: char);
begin
  SetCurrentDir(Format('%s:', [Value]));
end;

procedure TJvSearchFiles.SetOwnerData(const Value: boolean);
begin
  if FOwnerData <> Value then
  begin
    Abort;
    FOwnerData := Value;
    if FOwnerDAta then
    begin
      FDirs.Clear;
      FItems.Clear;
    end;
  end;
end;

end.

