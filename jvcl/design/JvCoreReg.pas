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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvCoreReg;

interface

procedure Register;

implementation
uses
  Classes,
  {$IFDEF VCL}
  Controls, StdCtrls, ExtCtrls, Graphics, ActnList, ImgList, Dialogs,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QControls, QStdCtrls, QExtCtrls, QGraphics, QActnList, QImgList, QDialogs,
  QTypes,
  {$ENDIF VisualCLX}
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvTypes, JvDsgnConsts, JvJCLUtils, JVCLVer, JvComponent,
  JvActions, JvActnResForm, JvJVCLAboutForm, JvDsgnEditors, JvIDEZoom,
  JvJVCLAboutEditor, JvBaseDlgEditor, JvColorEditor, JvPaintBoxEditor,
  JvAppIniStorage, JvBackgrounds,
  {$IFDEF MSWINDOWS}
  JvAppRegistryStorage, JvContextProvider,
  JvColorProviderEditors, JvDataProviderEditors, JvDataProvider,
  JvDataProviderIntf, JvBackgroundEditors,
  {$ENDIF MSWINDOWS}
  {$IFDEF VCL}
  JvColorProvider,
  {$ENDIF VCL}
  JvAppStorage, JvAppStorageSelectList;

{$IFDEF VCL}
{$R ..\Resources\JvCoreReg.dcr}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$R ../Resources/JvCoreReg.dcr}
{$ENDIF  VisualCLX}

procedure Register;
const
  BaseClass: TClass = TComponent;
begin
  RegisterComponents(RsPaletteNonVisual, [TJvJVCLAboutComponent
    {$IFDEF MSWINDOWS}
    , TJvContextProvider, TJvColorProvider, TJvColorMappingProvider, TJvBackground
    {$ENDIF MSWINDOWS}
    ]);
  RegisterComponents(RsPalettePersistence, [TJvAppStorage,
     {$IFDEF MSWINDOWS}TJvAppRegistryStorage,{$ENDIF}
     TJvAppIniFileStorage, TJvAppStorageSelectList]);

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

  {$IFDEF JVCL_REGISTER_GLOBAL_DESIGNEDITORS}

  {$IFDEF VCL}
  RegisterPropertyEditor(TypeInfo(TDate), nil, '', TJvDateExProperty);
  RegisterPropertyEditor(TypeInfo(TTime), nil, '', TJvTimeExProperty);
  RegisterPropertyEditor(TypeInfo(TDateTime), nil, '', TJvDateTimeExProperty);
  RegisterPropertyEditor(TypeInfo(TColor), TPersistent, '', TJvColorProperty);
  {$ENDIF VCL}

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

  {$IFDEF VCL}
  RegisterComponentEditor(TCustomImageList, TJvImageListEditor);
  RegisterComponentEditor(TImageList, TJvImageListEditor);
  {$ENDIF VCL}

  {$ENDIF JVCL_REGISTER_GLOBAL_DESIGNEDITORS}

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

  RegisterPropertyEditor(TypeInfo(TJvBackgroundClients), TJvBackground,
    'Clients', TJvClientsProperty);

  RegisterActions(RsJVCLActionsCategory, [{$IFDEF MSWINDOWS}TJvSendMailAction,{$ENDIF}TJvWebAction], TJvStandardActions);
  RegisterZoom;
end;

end.
