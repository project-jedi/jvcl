{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCabFile.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvCabFile;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  JvTypes, JvComponent;

type
  TCabInfo = record
    CabinetPath: string;
    CabinetFile: string;
    DiskName: string;
    Id: Shortint;
    CabinetNumber: Shortint;
  end;

  TOnCabInfo = procedure(Sender: TObject; CabInfo: TCabInfo) of object;
  TOnExtracted = procedure(Sender: TObject; Successed: Boolean; var Cont: Boolean;
    Source, Dest: string) of object;
  TOnExtractFile = procedure(Sender: TObject; FileName: string; DestPath: string) of object;
  TOnNeedNewCabinet = procedure(Sender: TObject; var Cont: Boolean; CabInfo: TCabInfo;
    var NewPath: string) of object;

  TJvCabFile = class(TJvComponent)
  private
    FFileName: TFileName;
    FFiles: TStringList;
    FOnCabInfo: TOnCabInfo;
    FOnFiles: TNotifyEvent;
    FOnExtracted: TOnExtracted;
    FDestPath: string;
    FOnExtractFile: TOnExtractFile;
    FOnNeed: TOnNeedNewCabinet;
    FTmpString: string;
    FDll: HMODULE;
    procedure SetFileName(const Value: TFileName);
    procedure SetFiles(const Value: TStringList);
    procedure RefreshFiles;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExtractAll(DestPath: string): Boolean;
    function ExtractFile(FileName: string; DestPath: string): Boolean;
  published
    property FileName: TFileName read FFileName write SetFileName;
    property Files: TStringList read FFiles write SetFiles;
    property OnCabInfo: TOnCabInfo read FOnCabInfo write FOnCabInfo;
    property OnFilesListed: TNotifyEvent read FOnFiles write FonFiles;
    property OnFileExtracted: TOnExtracted read FOnExtracted write FOnExtracted;
    property OnStartFileExtraction: TOnExtractFile read FOnExtractFile write FonExtractFile;
    property OnNeedNewCabinet: TOnNeedNewCabinet read FonNeed write FOnNeed;
  end;

implementation

resourcestring
  RC_ErrorSetupDll = 'Unable to find SetupApi.dll';

const
  SPFILENOTIFY_CABINETINFO    = $00000010;
  SPFILENOTIFY_FILEINCABINET  = $00000011;
  SPFILENOTIFY_NEEDNEWCABINET = $00000012;
  SPFILENOTIFY_FILEEXTRACTED  = $00000013;
  SPFILENOTIFY_FILEOPDELAYED  = $00000014;
  FILEOP_ABORT   = 0;
  FILEOP_DOIT    = 1;
  FILEOP_SKIP    = 2;
  FILEOP_RETRY   = FILEOP_DOIT;
  FILEOP_NEWPATH = 4;

  cSetupIterateCabinet = 'SetupIterateCabinetA';

type
  UINT_PTR = DWORD;

  TSPFileCallback = function(Context: Pointer; Notification: UINT;
    Param1, Param2: UINT_PTR): UINT; stdcall;
  TSetupIterateCabinet = function(const CabinetFile: PAnsiChar; Reserved: DWORD;
    MsgHandler: TSPFileCallback; Context: Pointer): LongBool; stdcall;

  PFileInCabinetInfo = ^TFileInCabinetInfo;
  FILE_IN_CABINET_INFO = packed record
    NameInCabinet: PAnsiChar;
    FileSize: DWORD;
    Win32Error: DWORD;
    DosDate: Word;
    DosTime: Word;
    DosAttribs: Word;
    FullTargetName: array [0..MAX_PATH - 1] of AnsiChar;
  end;
  TFileInCabinetInfo = FILE_IN_CABINET_INFO;

  PCabinetInfo = ^TCabinetInfo;
  CABINET_INFO = packed record
    CabinetPath: PAnsiChar;
    CabinetFile: PAnsiChar;
    DiskName: PAnsiChar;
    SetId: Word;
    CabinetNumber: Word;
  end;
  TCabinetInfo = CABINET_INFO;

  PFilePaths = ^TFilePaths;
  FILEPATHS = packed record
    Target: PAnsiChar;
    Source: PAnsiChar; // not used for delete operations
    Win32Error: UINT;
    Flags: DWORD; // such as SP_COPY_NOSKIP for copy errors
  end;
  TFilePaths = FILEPATHS;

constructor TJvCabFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFiles := TStringList.Create;
  FFileName := '';
  FDll := LoadLibrary('SETUPAPI.DLL');
  if FDll = 0 then
    raise EJVCLException.Create(RC_ErrorSetupDll);
end;

destructor TJvCabFile.Destroy;
begin
  FFiles.Free;
  if FDll <> 0 then
    FreeLibrary(FDll);
  inherited Destroy;
end;

procedure TJvCabFile.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
  RefreshFiles;
end;

procedure TJvCabFile.SetFiles(const Value: TStringList);
begin
  //do nothing !!!!
end;

function CBack(Context: Pointer; Notification: UINT; Param1, Param2: UINT_PTR): UINT; stdcall;
var
  Cab: PFileInCabinetInfo;
  Sender: TJvCabFile;
  CabInfo: TCabInfo;
  ParamInfo: PCabinetInfo;
begin
  Result := ERROR_BAD_COMMAND;
  if Context <> nil then
    Sender := TJvCabFile(Context^)
  else
    Exit;

  //this callback is only for listing files in a cabinet ... pouet pouet !
  if Notification = SPFILENOTIFY_FILEINCABINET then //found a file in the cabinet
  begin
    Result := FILEOP_SKIP;
    Cab := PFileInCabinetInfo(Param1);
    Sender.FFiles.Add(StrPas(Cab^.NameInCabinet));
  end
  else
  if Notification = SPFILENOTIFY_CABINETINFO then //give cabinet info
  begin
    if Assigned(Sender.FOnCabInfo) and (Param1 <> 0) then
    begin
      ParamInfo := PCabinetInfo(Param1);
      CabInfo.CabinetPath := StrPas(ParamInfo^.CabinetPath);
      CabInfo.CabinetFile := StrPas(ParamInfo^.CabinetFile);
      CabInfo.DiskName := StrPas(ParamInfo^.DiskName);
      CabInfo.Id := ParamInfo^.SetId;
      CabInfo.CabinetNumber := ParamInfo^.CabinetNumber;
      Sender.FOnCabInfo(Sender, CabInfo);
    end;
    Result := 0;
  end;
end;

function CExtract(Context: Pointer; Notification: UINT; Param1, Param2: UINT_PTR): UINT; stdcall;
var
  Cab: PFileInCabinetInfo;
  Sender: TJvCabFile;
  CabInfo: TCabInfo;
  Cont: Boolean;
  Pathes: TFilePaths;
  Path: string;
  I: Integer;
begin
  Result := ERROR_BAD_COMMAND;
  if Context <> nil then
    Sender := TJvCabFile(Context^)
  else
    Exit;

  //this callback is only for listing files in a cabinet ...
  if Notification = SPFILENOTIFY_CABINETINFO then
    Result := 0
  else
  if Notification = SPFILENOTIFY_FILEINCABINET then //found a file in the cabinet
  begin
    try
      Result := FILEOP_DOIT;
      Cab := PFileInCabinetInfo(Param1);

      if Sender.FDestPath[Length(Sender.FDestPath)] = '\' then
      begin
        //extract all
        Path := Sender.FDestPath + StrPas(Cab^.NameInCabinet);
        for I := 1 to Length(Path) do
          Cab^.FullTargetName[I-1] := Path[I];
        Cab^.FullTargetName[Length(Path)] := #0;

        if Assigned(Sender.FOnExtractFile) then
          Sender.FOnExtractFile(Sender, Cab^.FullTargetName, Sender.FDestPath);
      end
      else
      begin
        //Extract specific file
        if UpperCase(ExtractFileName(Sender.FDestPath)) = UpperCase(StrPas(Cab^.NameInCabinet)) then
        begin
          Path := Sender.FDestPath;
          for I := 1 to Length(Path) do
            Cab^.FullTargetName[I-1] := Path[I];
          Cab^.FullTargetName[Length(Path)] := #0;
          if Assigned(Sender.FOnExtractFile) then
            Sender.FOnExtractFile(Sender, Cab^.FullTargetName, Sender.FDestPath);
        end
        else
          Result := FILEOP_SKIP;
      end;
    except
      Result := FILEOP_SKIP;
    end;
  end
  else
  if Notification = SPFILENOTIFY_FILEEXTRACTED then
  begin
    Cont := True;
    if Param1 <> 0 then
      Pathes := PFilePaths(Param1)^;
    if Assigned(Sender.FOnExtracted) then
      Sender.FOnExtracted(Sender, (Pathes.Win32Error = NO_ERROR), Cont,
        StrPas(Pathes.Source), StrPas(Pathes.Target));
    if Cont then
      Result := NO_ERROR
    else
      Result := ERROR_BAD_COMMAND;
  end
  else
  if Notification = SPFILENOTIFY_NEEDNEWCABINET then
  begin
    if Param1 <> 0 then
    begin
      CabInfo.CabinetPath := StrPas(PCabinetInfo(Param1)^.CabinetPath);
      CabInfo.CabinetFile := StrPas(PCabinetInfo(Param1)^.CabinetFile);
      CabInfo.DiskName := StrPas(PCabinetInfo(Param1)^.DiskName);
      CabInfo.Id := PCabinetInfo(Param1)^.SetId;
      CabInfo.CabinetNumber := PCabinetInfo(Param1)^.CabinetNumber;
      Cont := True;
      Path := '';
      if Assigned(Sender.FOnNeed) then
      begin
        Sender.FOnNeed(Sender, Cont, CabInfo, Path);
        Sender.FTmpString := Path;
        PDWORD(Param2)^ := DWORD(PChar(Sender.FTmpString));
      end
      else
        Result := ERROR_BAD_COMMAND;
    end
    else
      Result := ERROR_BAD_COMMAND;
  end;
end;

procedure TJvCabFile.RefreshFiles;
var
  SetupIterateCabinet: TSetupIterateCabinet;
begin
  SetupIterateCabinet := GetProcAddress(FDll, cSetupIterateCabinet);
  FFiles.Clear;
  if SetupIterateCabinet(PChar(FFileName), 0, CBack, @Self) then
    if Assigned(FOnFiles) then
      FOnFiles(Self);
end;

function TJvCabFile.ExtractAll(DestPath: string): Boolean;
var
  SetupIterateCabinet: TSetupIterateCabinet;
begin
  SetupIterateCabinet := GetProcAddress(FDll, cSetupIterateCabinet);
  if DestPath[Length(DestPath)] <> '\' then
    DestPath := DestPath + '\';
  FDestPath := DestPath;
  Result := SetupIterateCabinet(PChar(FFileName), 0, CExtract, @Self);
end;

function TJvCabFile.ExtractFile(FileName, DestPath: string): Boolean;
var
  SetupIterateCabinet: TSetupIterateCabinet;
begin
  SetupIterateCabinet := GetProcAddress(FDll, cSetupIterateCabinet);
  if DestPath[Length(DestPath)] <> '\' then
    DestPath := DestPath + '\';
  FDestPath := DestPath + FileName;
  Result := SetupIterateCabinet(PChar(FFileName), 0, CExtract, @Self);
end;

end.

