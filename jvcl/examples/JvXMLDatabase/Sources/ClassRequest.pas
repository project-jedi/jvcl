{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit ClassRequest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ClassUtils, Strings, ClassScript;

type
  TRequestHandler = class
  public
    function Request(ADocument, AParams: string; AHeaders: TStringList;
      AResult: TStream; AUser: Integer): Boolean;
  end;

implementation

{ TRequestHandler }

{**********************************************************************}
function TRequestHandler.Request(ADocument, AParams: string;
  AHeaders: TStringList; AResult: TStream; AUser: Integer): Boolean;
var
 st: string;
 lStream: TFileStream;
begin
  result := true;

  //Get the file for the request
  st := StringReplace(ADocument, '/', '\', [rfReplaceAll]);
  if (st <> '') and (st[1] = '\') then
    st := Copy(st, 2, MAXINT);
  if (st = '') then
    st := RS_BASEFILE;
  st := ExtractFilePath(Application.ExeName) + 'Documents\' + st;

  //Return the raw file
  if FileExists(st) then
  begin
    if LowerCase(ExtractFileExt(st)) = RS_SCRIPT_EXT then
    begin
      with TScriptHandler.Create do
      try
        CurrentUser := AUser;
        Request(st, AParams, AResult, AHeaders);
      finally
        Free;
      end;
    end
    else
    begin
      lStream := TFileStream.Create(st, fmOpenRead and fmShareDenyWrite);
      try
        AResult.CopyFrom(lStream, 0);
      finally
        lStream.Free;
      end;
    end;

    st := LowerCase(ExtractFileExt(st));
    if st='.css' then
      AHeaders.Add('Content-type: text/css')
    else if st='.gif' then
      AHeaders.Add('Content-type: image/gif')
    else if st='.png' then
      AHeaders.Add('Content-type: image/png')
    else if (st='.jpg') or (st='.jpeg') then
      AHeaders.Add('Content-type: image/jpeg')
    else if st=RS_SCRIPT_EXT then
      AHeaders.Add('Content-type: text/html');
  end
  else
    result := false;
end;
{**********************************************************************}

end.
