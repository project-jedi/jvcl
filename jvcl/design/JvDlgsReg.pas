{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDlgsReg.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDlgsReg;

{$I jvcl.inc}

interface

procedure Register;

implementation

uses
  {$IFDEF HAS_UNIT_SYSTEM_ACTIONS}
  System.Actions,
  {$ENDIF HAS_UNIT_SYSTEM_ACTIONS}
  Classes, Dialogs, ActnList,
  DesignEditors, DesignIntf,
  JvDsgnConsts,
  JvDialogActns,
  JvDsgnConfig,
  JvDialogActnResForm, JvDialogs, JvPageSetupTitled, JvPageSetup,
  JvAppletEditor,
  JvWinDialogs, JvAddPrinter, JvCustomFileMessageDialog, JvConnectNetwork, JvCopyError,
  JvDeleteError, JvRenameError, JvDiskPrompt, JvFindFiles,
  JvObjectPickerDialog, JvCustomFileMessageDialogEditor,
  JvBaseDlg, JvFindReplace, JvDSADialogs, JvTipOfDay, JvCommonExecDlg,
  JvDesktopAlert, JvDesktopAlertEditors, JvProgressComponent, JvSelectDirectory,
  JvImageDlg, JvLoginForm, JvDualList, JvProgressDialog, JvBaseDlgEditor,
  JvTipOfDayEditor, JvProgressComponentEditor, JvFindReplaceEditor;

{$R JvDlgsReg.dcr}

procedure Register;
const
  cAppletName = 'AppletName';
  cAppletIndex = 'AppletIndex';
begin
  RegisterComponents(RsPaletteDialog, [TJvOpenDialog, TJvSaveDialog]);
  RegisterComponents(RsPaletteDialog, [TJvSelectDirectory, TJvTipOfDay,
    TJvFindReplace, TJvDSADialog]);
  RegisterComponents(RsPaletteDialog, [TJvPageSetupDialog, TJvPageSetupTitledDialog,
    TJvColorDialog, TJvAppletDialog]);
  RegisterComponents(RsPaletteDialog, [TJvConnectNetwork, TJvDisconnectNetwork,
    TJvAddPrinterDialog, TJvFindFilesDialog, TJvFormatDriveDialog,
    TJvOrganizeFavoritesDialog, TJvComputerNameDialog, TJvChangeIconDialog,
    TJvShellAboutDialog, TJvRunDialog, TJvObjectPropertiesDialog,
    TJvNewLinkDialog, TJvAddHardwareDialog, TJvOpenWithDialog, TJvDiskFullDialog,
    TJvExitWindowsDialog, TJvOutOfMemoryDialog, TJvObjectPickerDialog,
    TJvImageDialog]);
  RegisterComponents(RsPaletteDialog, [TJvLoginDialog, TJvProgressDialog, TJvProgressComponent]);
  RegisterComponents(RsPaletteDialog, [TJvDiskPrompt, TJvCopyError, TJvDeleteError, TJvRenameError]);
  RegisterComponents(RsPaletteDialog, [TJvDesktopAlert, TJvDesktopAlertStack, TJvDualListDialog]);
  RegisterPropertyEditor(TypeInfo(TJvCustomDesktopAlertStyleHandler), TJvDesktopAlert, '',
    TJvCustomDesktopAlertStyleHandlerEditor);
  RegisterPropertyEditor(TypeInfo(TJvEditControlName), TJvFindReplace, 'EditControl', TJvFindReplaceProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvAppletDialog, cAppletName, TJvAppletNameProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TJvAppletDialog, cAppletIndex, TJvAppletIndexProperty);

  if JvOptionRegisterGlobalDesignEditors then
    RegisterComponentEditor(TCommonDialog, TJvBaseDlgEditor);

  RegisterComponentEditor(TJvCommonDialog, TJvBaseDlgEditor);
  RegisterComponentEditor(TJvProgressComponent, TJvProgressComponentEditor);
  RegisterComponentEditor(TJvOpenDialog, TJvBaseDlgEditor);
  RegisterComponentEditor(TJvSaveDialog, TJvBaseDlgEditor);

  RegisterComponentEditor(TJvCustomFileMessageDialog, TJvCustomFileMessageDialogEditor);
  RegisterComponentEditor(TJvTipOfDay, TJvTipOfDayEditor);
  RegisterActions(RsJVCLActionsCategory, [TJvBrowseForFolderAction,
    TJvSelectDirectoryAction, TJvConnectNetworkAction, TJvFloppyFormatAction,
    TJvOrganizeFavoritesAction, TJvControlPanelAction, TJvOpenFileAction,
    TJvSaveFileAction, TJvPageSetupAction, TJvPageSetupTitledAction],
    // TJvDialogActions is a datamodule with default settings for our dialog actions
    TJvDialogActions);
end;

end.