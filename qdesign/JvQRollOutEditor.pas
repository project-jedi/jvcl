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

The Original Code is: JvRollOutEditor.PAS, released on 2002-01-16.

The Initial Developer of the Original Code is Peter Thörnqvist.
Portions created by Peter Thörnqvist are Copyright (C) 2004 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQRollOutEditor;

{$I jvcl.inc}

interface

uses 
  DesignIntf, DesignEditors,      
  QImgList, 
  JvQDsgnEditors, JvQRollOut;

type
  // property editor for IndexCollapsed and IndexExpanded on a TJvRollOut to
  // display the images from the imagelist and allow multiselect
  TJvRollOutOptionsImagesProperty = class(TJvDefaultImageIndexProperty)
  protected
    function ImageList: TCustomImageList; override;
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

  TJvRollOutDefaultEditor = class(TDefaultEditor)
  public
    procedure Edit; override;
  end;

implementation

//=== { TJvRollOutOptionsImagesProperty } ====================================

function TJvRollOutOptionsImagesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paMultiSelect];
end;

function TJvRollOutOptionsImagesProperty.ImageList: TCustomImageList;
begin
  Result := TJvRollOutImageOptions(GetComponent(0)).Images;
end;

//=== { TJvRollOutDefaultEditor } ============================================

procedure TJvRollOutDefaultEditor.Edit;
var
  R: TJvRollOut;
begin 
  if GetComponent is TJvRollOut then
  begin
    R := TJvRollOut(GetComponent);
    if R.MouseIsOnButton then
    begin
      R.Collapsed := not R.Collapsed;
      Designer.Modified;
    end;
  end; 
end;

end.
