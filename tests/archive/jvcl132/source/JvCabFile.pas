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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvCabFile;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SetupApi, JvTypes, JvComponent;

type
  TCabInfo = record
    CabinetPath: string;
    CabinetFile: string;
    DiskName: string;
    Id: Shortint;
    CabinetNumber: Shortint;
  end;
{$EXTERNALSYM TCabInfo}
  TOnCabInfo = procedure(Sender: TObject; CabInfo: TCabInfo) of object;
{$EXTERNALSYM TOnCabInfo}
  TOnExtracted = procedure(Sender: TObject; Successed: Boolean; var Continue: Boolean; Source, Dest: string) of object;
{$EXTERNALSYM TOnExtracted}
  TOnExtractFile = procedure(Sender: TObject; FileName: string; DestPath: string) of object;
{$EXTERNALSYM TOnExtractFile}
  TOnNeedNewCabinet = procedure(Sender: TObject; var Continue: Boolean; CabInfo: TCabInfo; var NewPath: string) of
    object;
{$EXTERNALSYM TOnNeedNewCabinet}

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
    FDll: THandle;
    FSetupIterateCabinet: TSetupIterateCabinet;
    procedure SetFileName(const Value: TFileName);
    procedure SetFiles(const Value: TStringList);
    procedure RefreshFiles;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property FileName: TFileName read FFileName write SetFileName;
    property Files: TStringList read FFiles write SetFiles;
    property OnCabInfo: TOnCabInfo read FOnCabInfo write FOnCabInfo;
    property OnFilesListed: TNotifyEvent read FOnFiles write FonFiles;
    property OnFileExtracted: TOnExtracted read FOnExtracted write FOnExtracted;
    property OnStartFileExtraction: TOnExtractFile read FOnExtractFile write FonExtractFile;
    property OnNeedNewCabinet: TOnNeedNewCabinet read FonNeed write FOnNeed;
    function ExtractAll(destpath: string): Boolean;
    function ExtractFile(FileName: string; DestPath: string): Boolean;
  end;

implementation

resourcestring
  RC_SetupApiDll = 'Unable to find setupapi.dll';

  {**************************************************}

constructor TJvCabFile.Create(AOwner: TComponent);
begin
  inherited;
  FFiles := TStringList.Create;
  FFileName := '';
  FDll := LoadLibrary('setupapi.dll');
  if FDll <> 0 then
    FSetupIterateCabinet := GetProcAddress(FDll, 'SetupIterateCabinetA')
  else
    raise Exception.Create(RC_SetupApiDll);
end;
{**************************************************}

destructor TJvCabFile.Destroy;
begin
  FFiles.Free;
  if FDll <> 0 then
    FreeLibrary(FDll);
  inherited;
end;

{**************************************************}

procedure TJvCabFile.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
  RefreshFiles;
end;

{**************************************************}

procedure TJvCabFile.SetFiles(const Value: TStringList);
begin
  //do nothing !!!!
end;

{**************************************************}

function CBack(Context: Pointer; Notification: UINT; Param1, Param2: UINT_PTR): Integer; stdcall;
var
  cab: PFileInCabinetInfo;
  Sender: TJvCabFile;
  cabinfo: TCabInfo;
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
    cab := PFileInCabinetInfo(Param1);
    Sender.FFiles.Add(StrPas(cab^.NameInCabinet));
  end
  else if Notification = SPFILENOTIFY_CABINETINFO then //give cabinet info
  begin
    if Assigned(Sender.FOnCabInfo) and (Param1 <> 0) then
    begin
      ParamInfo := PCabinetInfo(Param1);
      cabinfo.CabinetPath := StrPas(ParamInfo^.CabinetPath);
      cabinfo.CabinetFile := StrPas(ParamInfo^.CabinetFile);
      cabinfo.DiskName := StrPas(ParamInfo^.DiskName);
      cabinfo.Id := ParamInfo^.SetId;
      cabinfo.CabinetNumber := ParamInfo^.CabinetNumber;
      Sender.FOnCabInfo(Sender, cabinfo);
    end;
    Result := 0;
  end;
end;

{**************************************************}

function CExtract(Context: Pointer; Notification: UINT; Param1, Param2: UINT_PTR): Integer; stdcall;
var
  cab: PFileInCabinetInfo;
  Sender: TJvCabFile;
  cabinfo: TCabInfo;
  continue: Boolean;
  FPath: TFilePaths;
  path: string;
  i: Integer;
begin
  Result := ERROR_BAD_COMMAND;
  if Context <> nil then
    Sender := TJvCabFile(Context^)
  else
    Exit;

  //this callback is only for listing files in a cabinet ...
  if Notification = SPFILENOTIFY_CABINETINFO then
    Result := 0
  else if Notification = SPFILENOTIFY_FILEINCABINET then //found a file in the cabinet
  begin
    try
      Result := FILEOP_DOIT;
      cab := PFileInCabinetInfo(Param1);

      if Sender.FDestPath[Length(Sender.FDestPath)] = '\' then
      begin
        //extract all
        path := Sender.FDestPath + StrPas(cab^.NameInCabinet);
        for i := 1 to Length(path) do
          cab^.FullTargetName[i - 1] := path[i];
        cab^.FullTargetName[Length(path)] := #0;

        if Assigned(Sender.FOnExtractFile) then
          Sender.FOnExtractFile(Sender, cab^.FullTargetName, Sender.FDestPath);
      end
      else
      begin
        //Extract specific file
        if UpperCase(ExtractFileName(Sender.FDestPath)) = UpperCase(StrPas(cab^.NameInCabinet)) then
        begin
          path := Sender.FDestPath;
          for i := 1 to Length(path) do
            cab^.FullTargetName[i - 1] := path[i];
          cab^.FullTargetName[Length(path)] := #0;
          if Assigned(Sender.FOnExtractFile) then
            Sender.FOnExtractFile(Sender, cab^.FullTargetName, Sender.FDestPath);
        end
        else
          Result := FILEOP_SKIP;
      end;
    except
      Result := FILEOP_SKIP;
    end;
  end
  else if Notification = SPFILENOTIFY_FILEEXTRACTED then
  begin
    continue := True;
    if Param1 <> 0 then
      FPath := PFilePaths(Param1)^;
    if Assigned(Sender.FOnExtracted) then
      Sender.FOnExtracted(Sender, (FPath.Win32Error = NO_ERROR), continue,
        StrPas(FPath.Source), StrPas(FPath.Target));
    if continue then
      Result := NO_ERROR
    else
      Result := ERROR_BAD_COMMAND;
  end
  else if Notification = SPFILENOTIFY_NEEDNEWCABINET then
  begin
    if Param1 <> 0 then
    begin
      cabinfo.CabinetPath := StrPas(PCabinetInfo(Param1)^.CabinetPath);
      cabinfo.CabinetFile := StrPas(PCabinetInfo(Param1)^.CabinetFile);
      cabinfo.DiskName := StrPas(PCabinetInfo(Param1)^.DiskName);
      cabinfo.Id := PCabinetInfo(Param1)^.SetId;
      cabinfo.CabinetNumber := PCabinetInfo(Param1)^.CabinetNumber;
      continue := True;
      path := '';
      if Assigned(Sender.FOnNeed) then
      begin
        Sender.FOnNeed(Sender, continue, cabinfo, path);
        Sender.FTmpString := path;
        PDWORD(Param2)^ := DWORD(PChar(Sender.FTmpString));
      end
      else
        Result := ERROR_BAD_COMMAND;
    end
    else
      Result := ERROR_BAD_COMMAND;
  end;
end;

{**************************************************}

procedure TJvCabFile.RefreshFiles;
begin
  FFiles.Clear;
  if Assigned(FSetupIterateCabinet) then
    if FSetupIterateCabinet(PChar(FFileName), 0, @CBack, @Self) then
      if Assigned(FOnFiles) then
        FOnFiles(Self);
end;

{**************************************************}

function TJvCabFile.ExtractAll(destpath: string): Boolean;
begin
  if destpath[Length(destpath)] <> '\' then
    DestPath := DestPath + '\';
  FDestPath := DestPath;
  if Assigned(FSetupIterateCabinet) then
    Result := FSetupIterateCabinet(PChar(FFileName), 0, @CExtract, @Self)
  else
    Result := False;
end;

{**************************************************}

function TJvCabFile.ExtractFile(FileName, DestPath: string): Boolean;
begin
  if destpath[Length(destpath)] <> '\' then
    DestPath := DestPath + '\';
  FDestPath := DestPath + FileName;
  if Assigned(FSetupIterateCabinet) then
    Result := FSetupIterateCabinet(PChar(FFileName), 0, @CExtract, @Self)
  else
    Result := False;
end;

end.
