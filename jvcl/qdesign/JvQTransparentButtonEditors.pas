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

The Original Code is: JvTransparentButtonEditors.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQTransparentButtonEditors;

interface

uses
  SysUtils, 
  DesignIntf, DesignEditors, DesignMenus, VCLEditors,   
  QImgList, 
  JvQDsgnEditors, JvQTransparentButton;

type
  TJvTBImagesProperty = class(TJvDefaultImageIndexProperty)
  protected
    function ImageList: TCustomImageList; override;
  end;

implementation

function TJvTBImagesProperty.ImageList: TCustomImageList;
begin
  if AnsiSameText(GetName,'ActiveIndex') then
    Result := (GetComponent(0) as TJvTransparentButton2).ActiveImage
  else
  if AnsiSameText(GetName,'DisabledIndex') then
    Result := (GetComponent(0) as TJvTransparentButton2).DisabledImage
  else
  if AnsiSameText(GetName,'DownIndex') then
    Result := (GetComponent(0) as TJvTransparentButton2).DownImage
  else
  if AnsiSameText(GetName,'GrayIndex') then
    Result := (GetComponent(0) as TJvTransparentButton2).GrayImage
  else
    Result := nil;
end;

end.
