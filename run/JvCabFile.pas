{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCABFile.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse att buypin dott com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvCabFile;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, SysUtils, Classes,
  JvTypes, JvComponentBase;

type
  TCABInfo = record
    CabinetPath: string;
    CabinetFile: string;
    DiskName: string;
    Id: Shortint;
    CabinetNumber: Shortint;
  end;

  TOnCABInfo = procedure(Sender: TObject; CABInfo: TCABInfo) of object;
  TOnExtracted = procedure(Sender: TObject; Successed: Boolean; var Cont: Boolean;
    Source, Dest: string) of object;
  TOnExtractFile = procedure(Sender: TObject; FileName: string; DestPath: string) of object;
  TOnNeedNewCabinet = procedure(Sender: TObject; var Cont: Boolean; CABInfo: TCABInfo;
    var NewPath: string) of object;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvCABFile = class(TJvComponent)
  private
    FFileName: TFileName;
    FFiles: TStringList;
    FOnCABInfo: TOnCABInfo;
    FOnFiles: TNotifyEvent;
    FOnExtracted: TOnExtracted;
    FDestPath: string;
    FOnExtractFile: TOnExtractFile;
    FOnNeed: TOnNeedNewCabinet;
    FTmpString: string;
    function GetFiles: TStrings;
    procedure SetFileName(const Value: TFileName);
    procedure SetFiles(const Value: TStrings);
    procedure RefreshFiles;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExtractAll(DestPath: string): Boolean;
    function ExtractFile(FileName: string; DestPath: string): Boolean;
  published
    property FileName: TFileName read FFileName write SetFileName;
    property Files: TStrings read GetFiles write SetFiles;
    property OnCABInfo: TOnCABInfo read FOnCABInfo write FOnCABInfo;
    property OnFilesListed: TNotifyEvent read FOnFiles write FOnFiles;
    property OnFileExtracted: TOnExtracted read FOnExtracted write FOnExtracted;
    property OnStartFileExtraction: TOnExtractFile read FOnExtractFile write FOnExtractFile;
    property OnNeedNewCabinet: TOnNeedNewCabinet read FOnNeed write FOnNeed;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  StrUtils, WinConvTypes, JvSetupAPI,
  JvConsts, JvResources;

constructor TJvCABFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFiles := TStringList.Create;
  FFileName := '';
  LoadSetupApi;
  if not IsSetupApiLoaded then
    raise EJVCLException.CreateRes(@RsEErrorSetupDll);
end;

destructor TJvCABFile.Destroy;
begin
  FFiles.Free;
  UnloadSetupApi;
  inherited Destroy;
end;

procedure TJvCABFile.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
  RefreshFiles;
end;

function TJvCABFile.GetFiles: TStrings;
begin
  Result := FFiles;
end;

procedure TJvCABFile.SetFiles(const Value: TStrings);
begin
  //do nothing !!!!
end;

function CBack(Context: Pointer; Notification: UINT; Param1, Param2: UINT_PTR): UINT; stdcall;
var
  CAB: PFileInCabinetInfo;
  Sender: TJvCABFile;
  CABInfo: TCABInfo;
  ParamInfo: PCabinetInfo;
begin
  Result := ERROR_BAD_COMMAND;
  if Context <> nil then
    Sender := TJvCABFile(Context^)
  else
    Exit;

  // this callback is only for listing files in a Cabinet ... pouet pouet !
  if Notification = SPFILENOTIFY_FILEINCABINET then // found a file in the Cabinet
  begin
    Result := FILEOP_SKIP;
    CAB := PFileInCabinetInfo(Param1);
    Sender.Files.Add(StrPas(CAB^.NameInCabinet));
  end
  else
  if Notification = SPFILENOTIFY_CABINETINFO then // give Cabinet info
  begin
    if Assigned(Sender.FOnCABInfo) and (Param1 <> 0) then
    begin
      ParamInfo := PCabinetInfo(Param1);
      CABInfo.CabinetPath := StrPas(ParamInfo^.CabinetPath);
      CABInfo.CabinetFile := StrPas(ParamInfo^.CabinetFile);
      CABInfo.DiskName := StrPas(ParamInfo^.DiskName);
      CABInfo.Id := ParamInfo^.SetId;
      CABInfo.CabinetNumber := ParamInfo^.CabinetNumber;
      Sender.FOnCABInfo(Sender, CABInfo);
    end;
    Result := 0;
  end;
end;

function CExtract(Context: Pointer; Notification: UINT; Param1, Param2: UINT_PTR): UINT; stdcall;
type
  PUINT_PTR = ^UINT_PTR;
var
  CAB: PFileInCabinetInfo;
  Sender: TJvCABFile;
  CABInfo: TCABInfo;
  Cont: Boolean;
  Pathes: TFilePaths;
  Path: string;
  I: Integer;
begin
  Result := ERROR_BAD_COMMAND;
  if Context <> nil then
    Sender := TJvCABFile(Context^)
  else
    Exit;

  // this callback is only for listing files in a Cabinet ...
  if Notification = SPFILENOTIFY_CABINETINFO then
    Result := 0
  else
  if Notification = SPFILENOTIFY_FILEINCABINET then // found a file in the Cabinet
  begin
    try
      Result := FILEOP_DOIT;
      CAB := PFileInCabinetInfo(Param1);

      if Sender.FDestPath[Length(Sender.FDestPath)] = '\' then
      begin
        // extract all
        Path := Sender.FDestPath + StrPas(CAB^.NameInCabinet);
        for I := 1 to Length(Path) do
          CAB^.FullTargetName[I-1] := Path[I];
        CAB^.FullTargetName[Length(Path)] := #0;

        if Assigned(Sender.FOnExtractFile) then
          Sender.FOnExtractFile(Sender, CAB^.FullTargetName, Sender.FDestPath);
      end
      else
      begin
        // Extract specific file
        if AnsiEndsText(StrPas(CAB^.NameInCabinet), Sender.FDestPath) then
        begin
          Path := Sender.FDestPath;
          for I := 1 to Length(Path) do
            CAB^.FullTargetName[I-1] := Path[I];
          CAB^.FullTargetName[Length(Path)] := #0;
          if Assigned(Sender.FOnExtractFile) then
            Sender.FOnExtractFile(Sender, CAB^.FullTargetName, Sender.FDestPath);
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
      CABInfo.CabinetPath := StrPas(PCabinetInfo(Param1)^.CabinetPath);
      CABInfo.CabinetFile := StrPas(PCabinetInfo(Param1)^.CabinetFile);
      CABInfo.DiskName := StrPas(PCabinetInfo(Param1)^.DiskName);
      CABInfo.Id := PCabinetInfo(Param1)^.SetId;
      CABInfo.CabinetNumber := PCabinetInfo(Param1)^.CabinetNumber;
      Cont := True;
      Path := '';
      if Assigned(Sender.FOnNeed) then
      begin
        Sender.FOnNeed(Sender, Cont, CABInfo, Path);
        Sender.FTmpString := Path;
        PUINT_PTR(Param2)^ := UINT_PTR(PChar(Sender.FTmpString));
      end
      else
        Result := ERROR_BAD_COMMAND;
    end
    else
      Result := ERROR_BAD_COMMAND;
  end;
end;

procedure TJvCABFile.RefreshFiles;
begin
  Files.Clear;
  if SetupIterateCabinet(PChar(FFileName), 0, CBack, @Self) then
    if Assigned(FOnFiles) then
      FOnFiles(Self);
end;

function TJvCABFile.ExtractAll(DestPath: string): Boolean;
begin
  if DestPath[Length(DestPath)] <> PathDelim then
    DestPath := DestPath + PathDelim;
  FDestPath := DestPath;
  Result := SetupIterateCabinet(PChar(FFileName), 0, CExtract, @Self);
end;

function TJvCABFile.ExtractFile(FileName, DestPath: string): Boolean;
begin
  if DestPath[Length(DestPath)] <> PathDelim then
    DestPath := DestPath + PathDelim;
  FDestPath := DestPath + FileName;
  Result := SetupIterateCabinet(PChar(FFileName), 0, CExtract, @Self);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
