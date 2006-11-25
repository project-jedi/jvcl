{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: Jedi.Jvcl.Design.RegisterComponents.PAS, released on 2006-04-19.

The Initial Developers of the Original Code are: ahuser
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description :

Known Issues:
-----------------------------------------------------------------------------}

unit Jedi.Jvcl.Design.RegisterComponents;

interface

uses
  SysUtils, Classes;

procedure Register;

implementation

{$R JvCoreReg.dcr}

uses
  DbClient,
  // Core
  Jedi.Jvcl.JvPoweredBy,
  Jedi.Jvcl.JvAppStorage,
  Jedi.Jvcl.JvAppIniStorage,
  Jedi.Jvcl.JvAppXMLStorage,
  Jedi.Jvcl.JvAppStorageSelectList,
  Jedi.Jvcl.JvAutoComplete,
  // Components
  Jedi.Jvcl.JvAlarm,
  Jedi.Jvcl.JvConverter,
  Jedi.Jvcl.JvDataEmbedded,
  Jedi.Jvcl.JvEnterTab,
  Jedi.Jvcl.JvMergeManager,
  Jedi.Jvcl.JvPageManager;

resourcestring
  RsPaletteNonVisible = 'JVCL.NET Components';
  RsPaletteControls = 'JVCL.NET Controls';

procedure Register;
begin
  // Core
  RegisterComponents(RsPaletteNonVisible, [TJvPoweredByJCL, TJvPoweredByJVCL]);
  RegisterComponents(RsPaletteNonVisible, [TJvAppStorage,
    TJvAppIniFileStorage, TJvAppStorageSelectList, TJvAppXMLFileStorage]);
  RegisterComponents(RsPaletteNonVisible, [TJvLookupAutoComplete]);

  // Components
  RegisterComponents(RsPaletteNonVisual, [TJvAlarms, TJvConverter,
    TJvDataEmbedded,
    TJvEnterAsTab, TJvMergeManager, TJvPageManager{, TJvStrHolder, TJvMultiStringHolder}]);
end;

{$IFDEF RTL170_UP}

var
  AboutBoxServices: IOTAAboutBoxServices = nil;
  AboutBoxIndex: Integer = 0;

procedure RegisterAboutBox;
var
  ProductImage: HBITMAP;
begin
  Supports(BorlandIDEServices,IOTAAboutBoxServices, AboutBoxServices);
  Assert(Assigned(AboutBoxServices), RsENoAboutServices);
  ProductImage := LoadBitmap(FindResourceHInstance(HInstance), 'JVCLSPLASH');
  AboutBoxIndex := AboutBoxServices.AddProductInfo(RsAboutDialogTitle,
    RsAboutCopyright, RsAboutTitle, RsAboutDescription, 0,
    ProductImage, False, RsAboutLicenceStatus);
end;

procedure UnregisterAboutBox;
begin
  if (AboutBoxIndex <> 0) and Assigned(AboutBoxServices) then
  begin
    AboutBoxServices.RemoveProductInfo(AboutBoxIndex);
    AboutBoxIndex := 0;
    AboutBoxServices := nil;
  end;
end;

procedure RegisterSplashScreen;
var
  ProductImage: HBITMAP;
begin
  Assert(Assigned(SplashScreenServices), RsENoSplashServices);
  ProductImage := LoadBitmap(FindResourceHInstance(HInstance), 'JVCLSPLASH');
  SplashScreenServices.AddProductBitmap(RsAboutDialogTitle, ProductImage,
    False, RsAboutLicenceStatus);
end;

initialization
  RegisterSplashScreen;
  RegisterAboutBox;

finalization
  UnRegisterAboutBox;

{$ENDIF RTL170_UP}

end.
