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

The Original Code is: JvDataEmbeddedEditor.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQDataEmbeddedEditor;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, QDialogs, 
  DesignEditors, DesignIntf, 
  JvQDataEmbedded;

type
  TJvDataEmbeddedComponentEditor = class(TComponentEditor)
  private
    procedure LoadDataFromFile(Comp: TJvDataEmbedded);
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

uses
  JvQDsgnConsts;

procedure TJvDataEmbeddedComponentEditor.LoadDataFromFile(Comp: TJvDataEmbedded);
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

procedure TJvDataEmbeddedComponentEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
    LoadDataFromFile(Component as TJvDataEmbedded);
end;

function TJvDataEmbeddedComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := RsLoadFromFileEllipsis;
end;

function TJvDataEmbeddedComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.

