{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

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

Last Modified: 2003-11-09

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvQCoreReg;

interface

procedure Register;

implementation
uses
  Classes,


  QControls, QStdCtrls, QExtCtrls, QGraphics, QActnList, QImgList, QDialogs,
  QTypes,


  DesignEditors, DesignIntf,

  JvQTypes, JvQDsgnConsts, JvQJCLUtils, JVQCLVer, JvQComponent,
  JvQActions, JvQActnResForm, JvQJVCLAboutForm, JvQDsgnEditors, JvQIDEZoom,
  JvQJVCLAboutEditor, JvQBaseDlgEditor, {JvQColorEditor,} JvQPaintBoxEditor,
  JvQAppIniStorage,
  {$IFDEF MSWINDOWS}
  JvQAppRegistryStorage,
  {$ENDIF MSWINDOWS}
  JvQColorProviderEditors, JvQDataProviderEditors, JvQDataProvider,
  JvQDataProviderIntf, JvQColorProvider,  JvQContextProvider,

  JvQAppStorage, JvQAppStorageSelectList;



{$R ../Resources/JvCoreReg.dcr}


procedure Register;
const
  BaseClass: TClass = TComponent;
begin
  RegisterComponents(RsPaletteNonVisual, [TJvJVCLAboutComponent
    , TJvContextProvider, TJvColorProvider, TJvColorMappingProvider
    ]);
  RegisterComponents(RsPalettePersistence, [TJvAppStorage,
     {$IFDEF MSWINDOWS}TJvAppRegistryStorage,{$ENDIF}
     TJvAppIniFileStorage, TJvAppStorageSelectList]);

  RegisterPropertyEditor(TypeInfo(TJVCLAboutInfo), nil, 'AboutJVCL' + 'X', TJVCLAboutDialogProperty);

  // The TJvPersistent class needs an editor for D5 and BCB5, but for
  // all other compilers, it doesn't need anything as it is declared as
  // a SubComponent. However, we want to hide the Name and Tag property
  // in this case, thus the registration of 'nil' property editors

  RegisterPropertyEditor(TypeInfo(TComponentName), TJvPersistent, 'Name', nil);
  RegisterPropertyEditor(TypeInfo(Longint), TJvPersistent, 'Tag', nil);


  {$IFDEF JVCL_REGISTER_GLOBAL_DESIGNEDITORS}
//  RegisterPropertyEditor(TypeInfo(TColor), TPersistent, '', TJvColorProperty);


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
//  RegisterComponentEditor(TCommonDialog, TJvBaseDlgEditor);



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
  RegisterActions(RsJVCLActionsCategory, [
  {$IFDEF MSWINDOWS}TJvSendMailAction,{$ENDIF}TJvWebAction], TJvStandardActions);
  RegisterZoom;
end;

end.
