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
{ The Original Code is dpp_FileInfos.pas                                                           }
{                                                                                                  }
{ The Initial Developer of the Original Code is Andreas Hausladen                                  }
{ Portions created by these individuals are Copyright (C) of these individuals.                    }
{                                                                                                  }
{**************************************************************************************************}
unit dpp_FileInfos;
interface
uses
{$ifdef MSWINDOWS}
  Windows, SysUtils, Classes, Contnrs, dpp_Utils;
{$endif}
{$ifdef LINUX}
  Libc, SysUtils, Classes, Contnrs, dpp_Utils;
{$endif}

type
  TFileInfo = class(TObject)
  private
    FFilename: string;
{$ifdef MSWINDOWS}
    FCreationTime, FLastAccessTime, FLastWriteTime: TFileTime;
    FAttributes: Cardinal;
{$endif}
{$ifdef LINUX}
    FStat: TStatBuf;
{$endif}
    procedure GetStatus;
  public
    constructor Create(const AFilename: string);
    procedure SetStatus(const Filename: string);

    property Filename: string read FFilename write FFilename;
  end;

  TFileInfoList = class(TObjectList)
  private
    function GetFiles(Index: string): TFileInfo;
    function GetItems(Index: Integer): TFileInfo;
  public
    function SaveInfos(const Filename: string): TFileInfo;
    procedure RestoreInfos(const Filename: string);
    procedure RestoreInfosTo(const Filename, NewFilename: string);
    property Items[Index: Integer]: TFileInfo read GetItems;
    property Files[Index: string]: TFileInfo read GetFiles;
  end;

implementation

{ TFileInfoList }

function TFileInfoList.GetFiles(Index: string): TFileInfo;
var i: Integer;
begin
  for i := 0 to Count - 1 do
    if CompareFileNames(Items[i].Filename, Index) = 0 then
    begin
      Result := Items[i];
      Exit;
    end;
  Result := nil;
end;

function TFileInfoList.GetItems(Index: Integer): TFileInfo;
begin
  Result := TFileInfo(inherited Items[Index]);
end;

procedure TFileInfoList.RestoreInfos(const Filename: string);
var fi: TFileInfo;
begin
  fi := Files[Filename];
  if fi <> nil then
    fi.SetStatus(Filename);
end;

procedure TFileInfoList.RestoreInfosTo(const Filename, NewFilename: string);
var fi: TFileInfo;
begin
  fi := Files[Filename];
  if fi <> nil then
    fi.SetStatus(NewFilename);
end;

function TFileInfoList.SaveInfos(const Filename: string): TFileInfo;
begin
  Result := TFileInfo.Create(Filename);
  Add(Result);
end;

{ TFileInfo }

constructor TFileInfo.Create(const AFilename: string);
begin
  inherited Create;
  FFilename := AFilename;
  GetStatus;
end;

{$ifdef MSWINDOWS}
procedure TFileInfo.GetStatus;
var h: THandle;
begin
  FAttributes := GetFileAttributes(PChar(FFilename));
  h := FileOpen(FFilename, fmOpenRead);
  if h <> INVALID_HANDLE_VALUE then
  begin
    GetFileTime(h, @FCreationTime, @FLastAccessTime, @FLastWriteTime);
    FileClose(h);
  end;
end;

procedure TFileInfo.SetStatus(const Filename: string);
var h: THandle;
begin
  h := FileOpen(Filename, fmOpenWrite);
  if h <> INVALID_HANDLE_VALUE then
  begin
    SetFileTime(h, @FCreationTime, @FLastAccessTime, @FLastWriteTime);
    FileClose(h);
  end;
  SetFileAttributes(PChar(Filename), FAttributes);
end;
{$endif}

{$ifdef LINUX}
procedure TFileInfo.GetStatus;
begin
  Libc.stat(PChar(FFilename), FStat);
end;

procedure TFileInfo.SetStatus(const Filename: string);
begin
  FileSetDate(FileName, FileAge(Filename));
  chmod(PChar(FileName), FStat.st_mode);
end;
{$endif}


end.
