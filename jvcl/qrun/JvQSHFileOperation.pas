{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSHFileOp.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  A wrapper component for the SHFileOperation function

Known Issues:
  fofConfirmMouse does nothing
-----------------------------------------------------------------------------}
// $Id$

unit JvQSHFileOperation;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, ShellAPI,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes, QControls, 
  Qt, 
  JvQBaseDlg;

const
  // new supported constants
  FOF_NOCOPYSECURITYATTRIBS = $800;
  {$EXTERNALSYM FOF_NOCOPYSECURITYATTRIBS}

  FOF_NORECURSION = $1000;
  {$EXTERNALSYM FOF_NORECURSION}

  // IE 5 and up
  FOF_NO_CONNECTED_ELEMENTS = $2000;
  {$EXTERNALSYM FOF_NO_CONNECTED_ELEMENTS}

  // IE 5.01 and up
  FOF_NORECURSEREPARSE = $8000;
  {$EXTERNALSYM FOF_NORECURSEREPARSE}

  FOF_WANTNUKEWARNING = $4000;
  {$EXTERNALSYM FOF_WANTNUKEWARNING}

type
  // type of operation to perform
  TJvShFileMappingEvent = procedure(Sender: TObject; const OldFileName, NewFileName: string) of object;
  TJvSHFileOpType = (foCopy, foDelete, foMove, foRename);

  TJvSHFileOption = (fofAllowUndo, fofConfirmMouse, fofFilesOnly, fofMultiDestFiles,
    fofNoConfirmation, fofNoConfirmMkDir, fofRenameOnCollision, fofSilent,
    fofSimpleProgress, fofWantMappingHandle, fofNoErrorUI, fofNoCopySecurityAttributes,
    fofNoRecursion, fofNoConnectedElements, fofNoRecurseParse, fofWantNukeWarning);
  TJvSHFileOptions = set of TJvSHFileOption;

  TJvSHFileOperation = class(TJvCommonDialog)
  private
    FSourceFiles: TStringList;
    FDestFiles: TStringList;
    FOperation: TJvSHFileOpType;
    FOptions: TJvSHFileOptions;
    FTitle: string;
    FOnFileMapping: TJvShFileMappingEvent;
    function GetSourceFiles: TStrings;
    function GetDestFiles: TStrings;
    procedure SetSourceFiles(Value: TStrings);
    procedure SetDestFiles(Value: TStrings);
  protected
    // returns a Handle to the window that owns this dialog
    function GetWinHandle: THandle; virtual;
    procedure DoFileMapping(const OldFileName, NewFileName: string); virtual;
  public
    // performs the Operation and returns True if no errors occurred
    function Execute: Boolean; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // the files to perform the operation on (one file on each row).
    // Filenames can contain wildcards
    property SourceFiles: TStrings read GetSourceFiles write SetSourceFiles;
    // A list of destination filenames. If this is a folder, only need to add folder once
    // Otherwise, destfiles should match sourcefiles exactly
    property DestFiles: TStrings read GetDestFiles write SetDestFiles;
    // the operation to perform when Execute is called
    property Operation: TJvSHFileOpType read FOperation write FOperation default foCopy;
    /// Options for the Operation
    property Options: TJvSHFileOptions read FOptions write FOptions default [fofAllowUndo, fofFilesOnly];
    // Title of the progress dialog
    property Title: string read FTitle write FTitle;
    // Called when a file was renamed (but only if fofRenameOnCollision and fofWantMappingHandle are both True)
    property OnFileMapping: TJvShFileMappingEvent read FOnFileMapping write FOnFileMapping;
  end;

implementation

uses
  JvQConsts, JvQResources, JvQTypes;

type
  // helper object for file mappings
  PShHandleToMappings = ^TShHandleToMappings;
  TShHandleToMappings = packed record
    Count: UINT;
    PNameMappings: PSHNameMapping;
  end;

constructor TJvSHFileOperation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSourceFiles := TStringList.Create;
  FDestFiles := TStringList.Create;
  FOperation := foCopy;
  FOptions := [fofAllowUndo, fofFilesOnly];
end;

destructor TJvSHFileOperation.Destroy;
begin
  FSourceFiles.Free;
  FDestFiles.Free;
  inherited Destroy;
end;

{** returns True if no error occurred and user didn't abort }

function TJvSHFileOperation.Execute: Boolean;
const
  AOperation: array [TJvSHFileOpType] of UINT =
    (FO_COPY, FO_DELETE, FO_MOVE, FO_RENAME);
  AOption: array [TJvSHFileOption] of Word =
    (FOF_ALLOWUNDO, FOF_CONFIRMMOUSE, FOF_FILESONLY, FOF_MULTIDESTFILES,
     FOF_NOCONFIRMATION, FOF_NOCONFIRMMKDIR, FOF_RENAMEONCOLLISION,
     FOF_SILENT, FOF_SIMPLEPROGRESS, FOF_WANTMAPPINGHANDLE, FOF_NOERRORUI,
     FOF_NOCOPYSECURITYATTRIBS, FOF_NORECURSION, FOF_NO_CONNECTED_ELEMENTS,
     FOF_NORECURSEREPARSE, FOF_WANTNUKEWARNING);
var
  SFOS: TShFileOpStruct;
  I: TJvSHFileOption;
  J: Integer;
  ppFrom, ppTo: string;
  PNameMapping: PSHNameMapping;
  PNameCount: UINT;
  S, D: string;
begin
  if Length(FSourceFiles.Text) = 0 then
    EJVCLException.CreateRes(@RsENoFilesSpecifiedToTJvSHFileOperatio);

  FillChar(SFOS, SizeOf(TShFileOpStruct), #0);

  with SFOS do
  begin
    fAnyOperationsAborted := False;
    fFlags := 0;
    for I := Low(TJvSHFileOption) to High(TJvSHFileOption) do // Iterate
      if I in FOptions then
        fFlags := fFlags or AOption[I];
    hNameMappings := nil; // this is never used ???
    lpszProgressTitle := PChar(FTitle);
    ppFrom := '';
    ppTo := '';
    for J := 0 to FSourceFiles.Count - 1 do
      ppFrom := ppFrom + ExpandUNCFilename(FSourceFiles[J]) + #0;
    ppFrom := ppFrom + #0;
    pFrom := PChar(ppFrom);

    for J := 0 to FDestFiles.Count - 1 do
      ppTo := ppTo + ExpandUNCFilename(FDestFiles[J]) + #0;
    ppTo := ppTo + #0;
    pTo := PChar(ppTo);

    wFunc := AOperation[FOperation];
    Wnd := GetWinHandle; // (Owner as TForm).Handle;
  end;
  Result := SHFileOperation(SFOS) = 0;
  Result := Result and not SFOS.fAnyOperationsAborted;

  PNameMapping := Pointer(SFOS.hNameMappings);
  if PNameMapping <> nil then
  begin
    PNameCount := PShHandleToMappings(PNameMapping)^.Count;
    PNameMapping := PShHandleToMappings(PNameMapping)^.PNameMappings;
    while PNameCount > 0 do
    begin
      if (PNameMapping.cchOldPath > 0) and (PNameMapping.cchNewPath > 0) then
      begin
        if Win32Platform = VER_PLATFORM_WIN32_NT then
          SetLength(S, PNameMapping.cchOldPath * 2)
        else
          SetLength(S, PNameMapping.cchOldPath);
        Move(PNameMapping.pszOldPath[0], S[1], Length(S));
        if Win32Platform = VER_PLATFORM_WIN32_NT then
          SetLength(D, PNameMapping.cchNewPath * 2)
        else
          SetLength(D, PNameMapping.cchNewPath);
        Move(PNameMapping.pszNewPath[0], D[1], Length(D));
        if Win32Platform = VER_PLATFORM_WIN32_NT then
        begin
          // (p3) ShFileOp returns widechars on NT platforms
          {$WARNINGS OFF}
          S := WideCharToString(PWideChar(S + #0));
          D := WideCharToString(PWideChar(D + #0));
          {$WARNINGS ON}
        end;
        DoFileMapping(S, D);
      end;
      Inc(PNameMapping);
      Dec(PNameCount);
    end;
    ShFreeNameMappings(Cardinal(SFOS.hNameMappings));
  end;
end;

function TJvSHFileOperation.GetSourceFiles: TStrings;
begin
  Result := FSourceFiles;
end;

function TJvSHFileOperation.GetDestFiles: TStrings;
begin
  Result := FDestFiles;
end;

procedure TJvSHFileOperation.SetSourceFiles(Value: TStrings);
begin
  FSourceFiles.Assign(Value);
end;

procedure TJvSHFileOperation.SetDestFiles(Value: TStrings);
begin
  FDestFiles.Assign(Value);
end;

function TJvSHFileOperation.GetWinHandle: THandle;
begin
  if Owner is TWinControl then  
    Result := QWidget_winId(TWinControl(Owner).Handle) 
  else
    Result := GetForegroundWindow;
end;

procedure TJvSHFileOperation.DoFileMapping(const OldFileName, NewFileName: string);
begin
  if Assigned(FOnFileMapping) then
    FOnFileMapping(Self, OldFileName, NewFileName);
end;

end.

