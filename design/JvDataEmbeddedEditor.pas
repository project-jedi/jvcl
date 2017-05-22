{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDataEmbeddedEditor.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse att buypin dott com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDataEmbeddedEditor;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Dialogs,
  DesignEditors, DesignIntf, DesignMenus,
  JvDataEmbedded;

type
  TJvDataEmbeddedEditor = class(TComponentEditor)
  private
    procedure LoadDataFromFile(Comp: TJvDataEmbedded);
    procedure ViewAsText;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

implementation

uses
  JvDsgnConsts,
  Windows, ShellAPI,
  JclFileUtils;

procedure TJvDataEmbeddedEditor.LoadDataFromFile(Comp: TJvDataEmbedded);
var
  Stream: TFileStream;
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream(Comp.Data);
  with TOpenDialog.Create(nil) do
    try
      Filter := RsAllFilesFilter;
      if Execute then
      begin
        Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
        try
          MemStream.Clear;
          MemStream.CopyFrom(Stream, 0);
          // (rom) made this procedure a method and modify designer to make it work
          Designer.Modified;
        finally
          Stream.Free;
        end;
      end;
    finally
      Free;
    end;
end;

procedure TJvDataEmbeddedEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
  0:
    LoadDataFromFile(Component as TJvDataEmbedded);
  1:
    (Component as TJvDataEmbedded).Size := 0;
  2:
    ViewAsText;
  end;
end;

function TJvDataEmbeddedEditor.GetVerb(Index: Integer): string;
begin
  case Index of
  0:
    Result := RsLoadFromFileEllipsis;
  1:
    Result := RsClearEmbeddedData;
  2:
    Result := RsViewEmbeddedDataAsText;
  end;
end;

function TJvDataEmbeddedEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

procedure TJvDataEmbeddedEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  inherited PrepareItem(Index, AItem);
  case Index of
  1, 2:
    AItem.Enabled := (Component as TJvDataEmbedded).Data.Size > 0;
  end;
end;

procedure TJvDataEmbeddedEditor.ViewAsText;
var
  F: TFileStream;
  S: string;
begin
  S := FileGetTempName('JVCL');
  F := TFileStream.Create(S, fmCreate);
  try
    (Component as TJvDataEmbedded).DataSaveToStream(F);
  finally
    F.Free;
  end;
  ShellExecute(GetActiveWindow, 'open','notepad.exe',
    PChar(S), PChar(ExtractFilePath(S)), SW_SHOWNORMAL);
  // (p3) not 100% kosher, but seems to work most of the time
  // if anyone knows a better way to delete the temp file, please fix
  Sleep(300);
  DeleteFile(PChar(S));
end;

end.
