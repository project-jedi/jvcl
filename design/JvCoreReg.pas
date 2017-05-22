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
  Andreas Hausladen (ahuser)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvCoreReg;

{$I jvcl.inc}

interface

procedure Register;

implementation

uses
  {$IFDEF HAS_UNIT_SYSTEM_ACTIONS}
  System.Actions,
  {$ENDIF HAS_UNIT_SYSTEM_ACTIONS}
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  ActnList, ImgList,
  ToolsAPI,
  DesignEditors, DesignIntf,
  JvTypes, JvDsgnConsts, JvDsgnConfig, JvJCLUtils, JVCLVer, JvComponentBase, JvActions,
  JvActnResForm, JvJVCLAboutForm, JvPoweredBy, JvIDEZoom, JvBaseDlg,
  JvDataProvider, JvDataProviderIntf, JvColorProvider, JvContextProvider,
  JvDsgnEditors, JvJVCLAboutEditor, JvBaseDlgEditor, JvColorEditor,
  JvPaintBoxEditor, JvColorProviderEditors, JvDataProviderEditors,
  JvBackgrounds, JvBackgroundEditors,
  JvAppRegistryStorage, JvAppIniStorage, JvAppStorage, JvAppStorageSelectList,
  JvAutoComplete, JvTranslateString, JvStdEditActions,
  JvErrorIndicator, JvValidators, JvValidatorsEditorForm;

{$R JvCoreReg.dcr}

type
  TCustomActionClass = class of TCustomAction;

  { TJvStdEditActionsRes is used to copy the VCL's standard edit actions
    properties to the JVCL standard edit actions }
  TJvStdEditActionsRes = class(TComponent)
  private
    FStandardActions: TComponent;
    FActionList: TActionList;
  public
    constructor Create(AOwner: TComponent); override;

    function CreateAction(AActionClass: TCustomActionClass;
      const AStandardActionClassName: string): TCustomAction;
  end;

function FindComponentByClassName(AOwner: TComponent; const AClassName: string): TComponent;
var
  I: Integer;
begin
  for I := 0 to AOwner.ComponentCount - 1 do
  begin
    Result := AOwner.Components[I];
    if AnsiSameText(Result.ClassName, AClassName) then
      Exit;
  end;
  Result := nil;
end;

function FindComponentByClass(AOwner: TComponent; AComponentClass: TComponentClass): TComponent;
var
  I: Integer;
begin
  for I := 0 to AOwner.ComponentCount - 1 do
  begin
    Result := AOwner.Components[I];
    if Result.ClassType = AComponentClass then
      Exit;
  end;
  Result := nil;
end;

function ModuleEnumProc(HInstance: HINST; Data: Pointer): Boolean;
var
  StandardActionsClass: TComponentClass;
begin
  { Find the TStandardActions class from the dclstdXX.bpl package }
  StandardActionsClass := TComponentClass(GetProcAddress(HMODULE(HInstance), '@Actnres@TStandardActions@'));
  if StandardActionsClass <> nil then
  begin
    TJvStdEditActionsRes(Data).FStandardActions := StandardActionsClass.Create(Data);
    Result := False;
  end
  else
    Result := True;
end;

constructor TJvStdEditActionsRes.Create(AOwner: TComponent);
var
  StdActionList: TActionList;
begin
  inherited Create(AOwner);
  EnumModules(ModuleEnumProc, Self);
  if FStandardActions <> nil then
  begin
    StdActionList := TActionList(FindComponentByClass(FStandardActions, TActionList));
    if StdActionList <> nil then
    begin
      FActionList := TActionList.Create(Self);
      FActionList.Images := StdActionList.Images;

      { Create the JVCL standard edit actions }
      CreateAction(TJvEditCut, 'TEditCut');
      CreateAction(TJvEditCopy, 'TEditCopy');
      CreateAction(TJvEditPaste, 'TEditPaste');
      CreateAction(TJvEditSelectAll, 'TEditSelectAll');
      CreateAction(TJvEditUndo, 'TEditUndo');
      CreateAction(TJvEditDelete, 'TEditDelete');
    end;
  end;
end;

function TJvStdEditActionsRes.CreateAction(AActionClass: TCustomActionClass;
  const AStandardActionClassName: string): TCustomAction;
var
  StdAction: TCustomAction;
begin
  Result := AActionClass.Create(Self);
  Result.ActionList := FActionList;

  { Copy the localized properties }
  StdAction := TCustomAction(FindComponentByClassName(FStandardActions, AStandardActionClassName));
  if TObject(StdAction) is TCustomAction then
  begin
    Result.Caption := StdAction.Caption;
    //Result.Category := StdAction.Category; is overwritten by the IDE
    Result.Hint := StdAction.Hint;
    Result.Visible := StdAction.Visible;
    Result.Enabled := StdAction.Enabled;
    Result.ShortCut := StdAction.ShortCut;
    Result.Checked := StdAction.Checked;
    Result.HelpContext := StdAction.HelpContext;
    Result.ImageIndex := StdAction.ImageIndex;
  end;
end;


procedure Register;
const
  BaseClass: TClass = TComponent;
begin
  RegisterActions(RsJVCLEditActionsCategory, [TJvEditCut, TJvEditCopy, TJvEditPaste,
    TJvEditSelectAll, TJvEditUndo, TJvEditDelete], TJvStdEditActionsRes);

  {$IFDEF COMPILER7_UP}
  GroupDescendentsWith(TJvComponent, TControl);
  GroupDescendentsWith(TJvLookupAutoComplete, TControl);
  {$ENDIF COMPILER7_UP}

  RegisterComponents(RsPaletteNonVisual, [TJvJVCLAboutComponent,
    TJvContextProvider, TJvColorProvider, TJvColorMappingProvider]);
  RegisterComponents(RsPaletteNonVisual, [TJvBackground]);
  RegisterComponents(RsPaletteVisual, [TJvPoweredByJCL, TJvPoweredByJVCL]);

  RegisterComponents(RsPalettePersistence, [TJvAppStorage,
    TJvAppIniFileStorage, TJvAppStorageSelectList, TJvAppStorageSelectListEasyDialog]);
  RegisterComponents(RsPalettePersistence, [TJvAppRegistryStorage]);

  RegisterComponents(RsPaletteNonVisual, [TJvLookupAutoComplete, TJvTranslateString]);

  RegisterPropertyEditor(TypeInfo(TJVCLAboutInfo), nil, 'AboutJVCL', TJVCLAboutDialogProperty);

  RegisterPropertyEditor(TypeInfo(TComponentName), TJvPersistent, 'Name', nil);
  RegisterPropertyEditor(TypeInfo(Longint), TJvPersistent, 'Tag', nil);
  RegisterPropertyEditor(TypeInfo(TJvPersistent),nil, '', TJvPersistentPropertyEditor);

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

  // Validators
  RegisterComponents(RsPaletteValidators, [TJvValidators, TJvValidationSummary, TJvErrorIndicator]);
  RegisterNoIcon([TJvRequiredFieldValidator, TJvCompareValidator,
    TJvRangeValidator, TJvRegularExpressionValidator, TJvCustomValidator, TJvControlsCompareValidator]);

  RegisterComponentEditor(TJvValidators, TJvValidatorEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TJvErrorIndicator, 'ImageIndex', TJvDefaultImageIndexProperty);
//  RegisterPropertyEditor(TypeInfo(string), TJvCustomFormatEdit, 'Characters', TJvCharStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvBaseValidator, 'PropertyToValidate', TJvPropertyValidateProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvBaseValidator, 'CompareToProperty', TJvPropertyToCompareProperty);
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
  AboutBoxIndex := AboutBoxServices.AddPluginInfo(RsAboutTitle + ' ' + JVCL_VERSIONSTRING, RsAboutDescription,
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
  SplashScreenServices.AddPluginBitmap(RsAboutDialogTitle + ' ' + JVCL_VERSIONSTRING, ProductImage,
    False, RsAboutLicenceStatus);
end;

initialization
  RegisterSplashScreen;
  RegisterAboutBox;

finalization
  UnRegisterAboutBox;

{$ENDIF RTL170_UP}

end.