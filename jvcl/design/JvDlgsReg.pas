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

{$I jvcl.inc}

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
  JvDsgnConsts,
  JvBaseDlg, JvFindReplace, JvDSADialogs, JvTipOfDay, JvWinDialogs,
  JvAddPrinter, JvCommonDialogD, JvCommonExecDlg,
  {$IFNDEF BCB5}  // removed because BCB5 cannot compile/link JvDialogActns
  JvDialogActns,
  {$ENDIF BCB5}
  JvActnResForm, JvDialogs, JvProgressComponent, JvPageSetupTitled, JvPageSetup,
  JvConnectNetwork, JvSelectDirectory, JvCopyError, JvDeleteError,
  JvRenameError, JvDiskPrompt, JvFindFiles, JvImageDlg, JvLoginForm, JvDualList,
  JvProgressDialog, JvAppletEditor, JvObjectPickerDialog, JvBaseDlgEditor,
  JvCommonDialogDEditor, JvTipOfDayEditor;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvDlgsReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvDlgsReg.dcr}
{$ENDIF LINUX}

procedure Register;
const
  cAppletName = 'AppletName';
  cAppletIndex = 'AppletIndex';
begin
  RegisterComponents(RsPaletteDialog, [TJvSelectDirectory,
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

  RegisterPropertyEditor(TypeInfo(string), TJvAppletDialog, cAppletName, TJvAppletNameProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TJvAppletDialog, cAppletIndex, TJvAppletIndexProperty);

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
  {$IFNDEF BCB5}  // removed because BCB5 cannot compile/link JvDialogActns
  RegisterActions(RsJVCLActionsCategory, [TJvBrowseForFolderAction,
    TJvSelectDirectoryAction, TJvConnectNetworkAction, TJvFloppyFormatAction,
    TJvOrganizeFavoritesAction, TJvControlPanelAction, TJvOpenFileAction,
    TJvSaveFileAction, TJvPageSetupAction, TJvPageSetupTitledAction],
    // TJvStandardActions is a datamodule with default settings for our actions
    TJvStandardActions);
  {$ENDIF BCB5}
end;

end.
