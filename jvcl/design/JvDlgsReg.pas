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

Last Modified: 2003-11-09

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDlgsReg;

interface

procedure Register;

implementation

uses
  Classes, Dialogs, ActnList,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvConsts, JvDsgnConsts,
  JvBaseDlg, JvFindReplace, JvDSADialogs, JvTipOfDay, JvWinDialogs,
  JvAddPrinter, JvCommonDialogD, JvCommonExecDlg,
  {$IFNDEF CBUILDER5}  // removed because CBUILDER5 cannot compile/link JvDialogActns
  JvDialogActns,
  {$ENDIF CBUILDER5}
  JvActnResForm, JvDialogs, JvProgressComponent, JvPageSetupTitled, JvPageSetup,
  JvConnectNetwork, JvSelectDirectory, JvCopyError, JvDeleteError,
  JvRenameError, JvDiskPrompt, JvFindFiles, JvImageDlg, JvLoginForm, JvDualList,
  JvProgressDialog, JvAppletEditor, JvObjectPickerDialog, JvBaseDlgEditor,
  JvCommonDialogDEditor, JvTipOfDayEditor;

{$R ..\resources\JvDlgsReg.dcr}

procedure Register;
begin
  RegisterComponents(SPaletteDialog, [TJvSelectDirectory,
    TJvOpenDialog, TJvSaveDialog, TJvTipOfDay, TJvFindReplace, TJvDSADialog,
    TJvPageSetupDialog, TJvPageSetupTitledDialog, TJvConnectNetwork,
    TJvDisconnectNetwork, TJvAddPrinterDialog, TJvFindFilesDialog,
    TJvFormatDriveDialog, TJvColorDialog, TJvOrganizeFavoritesDialog,
    TJvComputerNameDialog, TJvAppletDialog, TJvChangeIconDialog,
    TJvShellAboutDialog, TJvRunDialog, TJvObjectPropertiesDialog,
    TJvNewLinkDialog, TJvAddHardwareDialog, TJvOpenWithDialog,
    TJvDiskFullDialog, TJvExitWindowsDialog, TJvOutOfMemoryDialog,
    TJvObjectPickerDialog, TJvDualListDialog, TJvImageDialog, TJvLoginDialog,
    TJvProgressDialog, TJvProgressComponent, TJvDiskPrompt, TJvCopyError,
    TJvDeleteError, TJvRenameError]);

  RegisterPropertyEditor(TypeInfo(string), TJvAppletDialog, 'AppletName', TJvAppletNameProperty);
  RegisterPropertyEditor(TypeInfo(integer), TJvAppletDialog, 'AppletIndex', TJvAppletIndexProperty);

  {$IFDEF JVCL_REGISTER_GLOBAL_DESIGNEDITORS}
  RegisterComponentEditor(TCommonDialog, TJvBaseDlgEditor);
  {$ENDIF JVCL_REGISTER_GLOBAL_DESIGNEDITORS}
  RegisterComponentEditor(TJvCommonDialog, TJvBaseDlgEditor);
  RegisterComponentEditor(TJvOpenDialog, TJvBaseDlgEditor);
  RegisterComponentEditor(TJvSaveDialog, TJvBaseDlgEditor);
  RegisterComponentEditor(TJvCommonDialogP, TJvBaseDlgEditor);
  RegisterComponentEditor(TJvCommonDialogF, TJvBaseDlgEditor);
  RegisterComponentEditor(TJvCommonDialogD, TJvCommonDialogDEditor);
  RegisterComponentEditor(TJvTipOfDay, TJvTipOfDayEditor);
  {$IFNDEF CBUILDER5}  // removed because CBUILDER5 cannot compile/link JvDialogActns
  RegisterActions(SJVCLActionsCategory, [TJvBrowseForFolderAction,
    TJvSelectDirectoryAction, TJvConnectNetworkAction, TJvFloppyFormatAction,
    TJvOrganizeFavoritesAction, TJvControlPanelAction, TJvOpenFileAction,
    TJvSaveFileAction, TJvPageSetupAction, TJvPageSetupTitledAction],
    // TJvStandardActions is a datamodule with default settings for our actions
    TJvStandardActions);
  {$ENDIF CBUILDER5}
end;

end.
