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

unit JvQCoreReg;

interface

{$IFDEF MSWINDOWS}
{$DEFINE USEWINDOWS}
{$ENDIF MSWINDOWS}

procedure Register;

implementation

uses
  Classes,
  QControls, QStdCtrls, QExtCtrls, QGraphics, QActnList, QImgList, QDialogs, 
  QTypes,  
  DesignEditors, DesignIntf, 
  JvQTypes, JvQDsgnConsts, JvQJCLUtils, JVCLXVer, JvQComponent, JvQActions,
  JvQActnResForm, JvQJVCLAboutForm, JvQIDEZoom, JvQBaseDlg,
  JvQDataProvider, JvQDataProviderIntf, JvQColorProvider, JvQContextProvider,
  JvQDsgnEditors, JvQJVCLAboutEditor, JvQBaseDlgEditor, JvQColorEditor,
  JvQPaintBoxEditor, JvQColorProviderEditors, JvQDataProviderEditors, 
  {$IFDEF USEWINDOWS}
  JvQAppRegistryStorage,
  {$ENDIF USEWINDOWS}
  JvQAppIniStorage, JvQAppStorage, JvQAppStorageSelectList;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvCoreReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvCoreReg.dcr}
{$ENDIF LINUX}

procedure Register;
const
  BaseClass: TClass = TComponent;
begin 
  GroupDescendentsWith(TJvComponent, TControl); 

  RegisterComponents(RsPaletteNonVisual, [TJvJVCLAboutComponent,
    TJvContextProvider, TJvColorProvider, TJvColorMappingProvider]); 

  RegisterComponents(RsPalettePersistence, [TJvAppStorage,
    TJvAppIniFileStorage, TJvAppStorageSelectList]);
  {$IFDEF USEWINDOWS}
  RegisterComponents(RsPalettePersistence, [TJvAppRegistryStorage]);
  {$ENDIF USEWINDOWS}
  
  RegisterPropertyEditor(TypeInfo(TJVCLAboutInfo), nil, 'AboutJVCLX', TJVCLAboutDialogProperty); 

  // The TJvPersistent class needs an editor for D5 and BCB5, but for
  // all other compilers, it doesn't need anything as it is declared as
  // a SubComponent. However, we want to hide the Name and Tag property
  // in this case, thus the registration of 'nil' property editors 
  RegisterPropertyEditor(TypeInfo(TComponentName), TJvPersistent, 'Name', nil);
  RegisterPropertyEditor(TypeInfo(Longint), TJvPersistent, 'Tag', nil); 
 
  
  RegisterPropertyEditor(TypeInfo(TColor), nil, '', TJvColorProperty); 

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
 

  RegisterActions(RsJVCLActionsCategory, [{$IFDEF MSWINDOWS} TJvSendMailAction, {$ENDIF} TJvWebAction], TJvStandardActions);
  RegisterZoom;
end;

end.
