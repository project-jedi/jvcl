{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCoreReg.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):
  Florent Ouchet (outchy)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvCoreReg;

{$I jvcl.inc}

interface

procedure Register;

implementation

uses
  {$IFDEF RTL170_UP}
  Windows, SysUtils,
  {$ENDIF RTL170_UP}
  Classes, Controls, StdCtrls, ExtCtrls, Graphics, ActnList, ImgList, Dialogs,
  ToolsAPI,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvTypes, JvDsgnConsts, JvDsgnConfig, JvJCLUtils, JVCLVer, JvComponentBase, JvActions,
  JvActnResForm, JvJVCLAboutForm, JvPoweredBy, JvIDEZoom, JvBaseDlg,
  JvDataProvider, JvDataProviderIntf, JvColorProvider, JvContextProvider,
  JvDsgnEditors, JvJVCLAboutEditor, JvBaseDlgEditor, JvColorEditor,
  JvPaintBoxEditor, JvColorProviderEditors, JvDataProviderEditors,
  JvBackgrounds, JvBackgroundEditors,
  JvAppRegistryStorage, JvAppIniStorage, JvAppStorage, JvAppStorageSelectList,
  JvAutoComplete, JvTranslateString;

{$R JvCoreReg.dcr}

procedure Register;
const
  BaseClass: TClass = TComponent;
begin
  {$IFDEF COMPILER7_UP}
  GroupDescendentsWith(TJvComponent, TControl);
  GroupDescendentsWith(TJvLookupAutoComplete, TControl);
  {$ENDIF COMPILER7_UP}

  RegisterComponents(RsPaletteNonVisual, [TJvJVCLAboutComponent,
    TJvContextProvider, TJvColorProvider, TJvColorMappingProvider]);
  RegisterComponents(RsPaletteNonVisual, [TJvBackground]);
  RegisterComponents(RsPaletteVisual, [TJvPoweredByJCL, TJvPoweredByJVCL]);

  RegisterComponents(RsPalettePersistence, [TJvAppStorage,
    TJvAppIniFileStorage, TJvAppStorageSelectList]);
  RegisterComponents(RsPalettePersistence, [TJvAppRegistryStorage]);

  RegisterComponents(RsPaletteNonVisual, [TJvLookupAutoComplete, TJvTranslateString]);

  RegisterPropertyEditor(TypeInfo(TJVCLAboutInfo), nil, 'AboutJVCL', TJVCLAboutDialogProperty);

  // The TJvPersistent class needs an editor for D5 and BCB5, but for
  // all other compilers, it doesn't need anything as it is declared as
  // a SubComponent. However, we want to hide the Name and Tag property
  // in this case, thus the registration of 'nil' property editors
  {$IFDEF COMPILER6_UP}
  RegisterPropertyEditor(TypeInfo(TComponentName), TJvPersistent, 'Name', nil);
  RegisterPropertyEditor(TypeInfo(Longint), TJvPersistent, 'Tag', nil);
  {$ELSE}
  RegisterPropertyEditor(TypeInfo(TJvPersistent), nil, '', TJvPersistentProperty);
  {$ENDIF COMPILER6_UP}

  if JvOptionRegisterGlobalDesignEditors then
  begin
    RegisterPropertyEditor(TypeInfo(TDate), nil, '', TJvDateExProperty);
    RegisterPropertyEditor(TypeInfo(TTime), nil, '', TJvTimeExProperty);
    RegisterPropertyEditor(TypeInfo(TDateTime), nil, '', TJvDateTimeExProperty);
    RegisterPropertyEditor(TypeInfo(TColor), TPersistent, '', TJvColorProperty);

    RegisterPropertyEditor(TypeInfo(string), BaseClass, 'InitialDir', TJvDirectoryProperty);
    RegisterPropertyEditor(TypeInfo(string), BaseClass, 'FolderName', TJvDirectoryProperty);
    RegisterPropertyEditor(TypeInfo(string), BaseClass, 'DirectoryName', TJvDirectoryProperty);
    RegisterPropertyEditor(TypeInfo(string), BaseClass, 'Hint', TJvHintProperty);
    RegisterPropertyEditor(TypeInfo(TCaption), BaseClass, '', TJvHintProperty);

    RegisterPropertyEditor(TypeInfo(Integer), BaseClass, '', TJvIntegerProperty);
    RegisterPropertyEditor(TypeInfo(Shortint), BaseClass, '', TJvIntegerProperty);
    RegisterPropertyEditor(TypeInfo(Smallint), BaseClass, '', TJvIntegerProperty);
    RegisterPropertyEditor(TypeInfo(Longint), BaseClass, '', TJvIntegerProperty);
    RegisterPropertyEditor(TypeInfo(Word), BaseClass, '', TJvIntegerProperty);
    RegisterPropertyEditor(TypeInfo(Byte), BaseClass, '', TJvIntegerProperty);
    RegisterPropertyEditor(TypeInfo(Cardinal), BaseClass, '', TJvIntegerProperty);

    RegisterPropertyEditor(TypeInfo(Single), BaseClass, '', TJvFloatProperty);
    RegisterPropertyEditor(TypeInfo(Double), BaseClass, '', TJvFloatProperty);
    RegisterPropertyEditor(TypeInfo(Extended), BaseClass, '', TJvFloatProperty);
    RegisterPropertyEditor(TypeInfo(Currency), BaseClass, '', TJvFloatProperty);

    RegisterComponentEditor(TPaintBox, TJvPaintBoxEditor);
    RegisterComponentEditor(TCommonDialog, TJvBaseDlgEditor);

    RegisterComponentEditor(TCustomImageList, TJvImageListEditor);
    RegisterComponentEditor(TImageList, TJvImageListEditor);
  end;

  RegisterPropertyEditor(TypeInfo(TShortCut), TJvComponent, '', TJvShortCutProperty);
  RegisterPropertyEditor(TypeInfo(TDayOfWeekName), nil, '', TJvWeekDayProperty);
  // DataProvider related editors
  RegisterPropertyEditor(TypeInfo(TJvColorProviderMapping), TPersistent, '', TJvColorProviderMappingProperty);
  RegisterPropertyEditor(TypeInfo(TJvDataConsumer), TPersistent, '', TJvDataConsumerProperty);
  RegisterPropertyEditor(TypeInfo(TJvDataItemID), TPersistent, '', TJvDataProviderItemIDProperty);
  RegisterPropertyEditor(TypeInfo(TJvDataContextID), TPersistent, '', TJvDataConsumerContextProperty);
  RegisterPropertyEditor(TypeInfo(TJvDataProviderTree), TComponent, '', TJvDataProviderTreeProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TJvDataConsumerClientNotifyItem, '', TJvConsumerNotifyComponentProperty);
  RegisterPropertyEditor(TypeInfo(TJvColorProviderAddColorStyle), nil, '', TJvColorProviderAddColorStyleEditor);
  RegisterComponentEditor(TJvCustomDataProvider, TJvProviderEditor);
  RegisterComponentEditor(TJvColorProvider, TJvColorProviderEditor);

  RegisterPropertyEditor(TypeInfo(TJvBackgroundClients), TJvBackground, 'Clients', TJvClientsProperty);

  RegisterActions(RsJVCLActionsCategory, [TJvSendMailAction, TJvWebAction], TJvStandardActions);
  RegisterZoom;
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
  AboutBoxIndex := AboutBoxServices.AddPluginInfo(RsAboutTitle, RsAboutDescription,
    ProductImage, False, RsAboutLicenceStatus);
end;

procedure UnregisterAboutBox;
begin
  if (AboutBoxIndex <> 0) and Assigned(AboutBoxServices) then
  begin
    AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
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
  SplashScreenServices.AddPluginBitmap(RsAboutDialogTitle, ProductImage,
    False, RsAboutLicenceStatus);
end;

initialization
  RegisterSplashScreen;
  RegisterAboutBox;

finalization
  UnRegisterAboutBox;

{$ENDIF RTL170_UP}

end.
