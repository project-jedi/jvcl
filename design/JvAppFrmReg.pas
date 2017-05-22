{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAppFrmReg.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvAppFrmReg;

{$I jvcl.inc}

interface

procedure Register;

implementation

uses
  Classes, Graphics,
  DesignIntf,
  JvDsgnConsts,
  JvFormMagnet, JvFormTransparent, JvAppHotKey, JvGradientCaptionForm,
  JvGradientCaption, JvAppAnimatedIcon, JvAppEvent,
  JvFormAnimatedIcon, JvFormAnimation, JvFormWallpaper,
  JvAnimTitle, JvFormAutoSize, JvEmbeddedForms,
  JvFormWallpaperEditor;

{$R JvAppFrmReg.dcr}

procedure Register;
begin
  RegisterComponents(RsPaletteAppForm,
    [TJvAppEvents, TJvAppAnimatedIcon, TJvFormAnimatedIcon, TJvAnimTitle,
     TJvApplicationHotKey, TJvTransparentForm, TJvFormMagnet, TJvGradientCaption,
     TJvFormAnimation, TJvFormWallpaper, TJvFormAutoSize,
     TJvEmbeddedFormPanel, TJvEmbeddedInstanceFormPanel, TJvEmbeddedFormLink]);

  RegisterComponentEditor(TJvGradientCaption, TJvGradientCaptionEditor);
  //RegisterPropertyEditor(TypeInfo(TPicture), TJvFormWallpaper, 'Image', TJvFormWallpaperEditor);
end;

end.
