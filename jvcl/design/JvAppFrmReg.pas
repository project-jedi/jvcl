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
located at http://jvcl.sourceforge.net

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
  {$IFDEF COMPILER6_UP}
  DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  {$IFDEF VCL}
  JvFormMagnet, JvFormTransparent, JvAppHotKey, JvGradientCaptionForm,
  JvGradientCaption,
  {$ENDIF VCL}
  JvDsgnConsts,
  JvAppAnimatedIcon, JvAppEvent,
  JvFormAnimatedIcon, JvFormAnimation, JvFormWallpaper,
  JvAnimTitle, JvFormAutoSize, JvEmbeddedForms,
  JvFormWallpaperEditor;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvAppFrmReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
{$R ../Resources/JvAppFrmReg.dcr}
{$ENDIF UNIX}

procedure Register;
begin
  RegisterComponents(RsPaletteAppForm,
    [TJvAppEvents, TJvAppAnimatedIcon, TJvFormAnimatedIcon, TJvAnimTitle,
     {$IFDEF VCL}
     TJvApplicationHotKey, TJvTransparentForm, TJvFormMagnet, TJvGradientCaption,
     {$ENDIF VCL}
     TJvFormAnimation, TJvFormWallpaper, TJvFormAutoSize,
     TJvEmbeddedFormPanel, TJvEmbeddedInstanceFormPanel, TJvEmbeddedFormLink]);

  RegisterComponentEditor(TJvGradientCaption, TGradientCaptionEditor);
  //RegisterPropertyEditor(TypeInfo(TPicture), TJvFormWallpaper, 'Image', TJvFormWallpaperEditor);
end;

end.

