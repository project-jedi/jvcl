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
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDlgsReg;

{$I jvcl.inc}

{$IFDEF MSWINDOWS}
{$DEFINE USEWINDOWS}
{$ENDIF MSWINDOWS}

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

  {$IFDEF VCL}
   {$IFNDEF BCB5}  // removed because BCB5 cannot compile/link JvDialogActns
  JvDialogActns,
   {$ENDIF BCB5}
  JvDialogActnResForm, JvDialogs, JvPageSetupTitled, JvPageSetup,
  JvAppletEditor,
  {$ENDIF VCL}

  {$IFDEF VisualCLX}
  QExtDlgs,
  {$ENDIF VisualCLX}

  {$IFDEF USEWINDOWS}
  JvWinDialogs, JvAddPrinter, JvCommonDialogD, JvConnectNetwork, JvCopyError,
  JvDeleteError, JvRenameError, JvDiskPrompt, JvFindFiles,
  JvObjectPickerDialog, JvCommonDialogDEditor,
  {$ENDIF USEWINDOWS}

  JvBaseDlg, JvFindReplace, JvDSADialogs, JvTipOfDay, JvCommonExecDlg,
  JvDesktopAlert, JvProgressComponent, JvSelectDirectory, JvImageDlg,
  JvLoginForm, JvDualList, JvProgressDialog, JvBaseDlgEditor,
  JvTipOfDayEditor;

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
  {$IFDEF VCL}
  RegisterComponents(RsPaletteDialog, [TJvOpenDialog, TJvSaveDialog]);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  RegisterComponents(RsPaletteDialog, [TOpenPictureDialog, TSavePictureDialog, TPrinterSetupDialog]);
  {$ENDIF VisualCLX}
  RegisterComponents(RsPaletteDialog, [TJvSelectDirectory, TJvTipOfDay,
    TJvFindReplace, TJvDSADialog]);
  {$IFDEF VCL}
  RegisterComponents(RsPaletteDialog, [TJvPageSetupDialog, TJvPageSetupTitledDialog,
    TJvColorDialog, TJvAppletDialog]);
  {$ENDIF VCL}
  {$IFDEF USEWINDOWS}
  RegisterComponents(RsPaletteDialog, [TJvConnectNetwork, TJvDisconnectNetwork,
    TJvAddPrinterDialog, TJvFindFilesDialog, TJvFormatDriveDialog,
    TJvOrganizeFavoritesDialog, TJvComputerNameDialog, TJvChangeIconDialog,
    TJvShellAboutDialog, TJvRunDialog, TJvObjectPropertiesDialog,
    TJvNewLinkDialog, TJvAddHardwareDialog, TJvOpenWithDialog, TJvDiskFullDialog,
    TJvExitWindowsDialog, TJvOutOfMemoryDialog, TJvObjectPickerDialog,
    TJvImageDialog]);
  {$ENDIF USEWINDOWS}
  RegisterComponents(RsPaletteDialog, [TJvLoginDialog, TJvProgressDialog, TJvProgressComponent]);
  {$IFDEF USEWINDOWS}
  RegisterComponents(RsPaletteDialog, [TJvDiskPrompt, TJvCopyError,
    TJvDeleteError, TJvRenameError]);
  {$ENDIF USEWINDOWS}
  RegisterComponents(RsPaletteDialog, [TJvDesktopAlert, TJvDesktopAlertStack,
    TJvDualListDialog]);
  {$IFDEF VCL}
  RegisterPropertyEditor(TypeInfo(string), TJvAppletDialog, cAppletName, TJvAppletNameProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TJvAppletDialog, cAppletIndex, TJvAppletIndexProperty);
  {$ENDIF VCL}

  {$IFDEF JVCL_REGISTER_GLOBAL_DESIGNEDITORS}
  RegisterComponentEditor(TCommonDialog, TJvBaseDlgEditor);
  {$ENDIF JVCL_REGISTER_GLOBAL_DESIGNEDITORS}
  RegisterComponentEditor(TJvCommonDialog, TJvBaseDlgEditor);
  {$IFDEF VCL}
  RegisterComponentEditor(TJvOpenDialog, TJvBaseDlgEditor);
  RegisterComponentEditor(TJvSaveDialog, TJvBaseDlgEditor);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  RegisterComponentEditor(TOpenPictureDialog, TJvBaseDlgEditor);
  RegisterComponentEditor(TSavePictureDialog, TJvBaseDlgEditor);
  {$ENDIF VisualCLX}

  RegisterComponentEditor(TJvCommonDialogP, TJvBaseDlgEditor);
  RegisterComponentEditor(TJvCommonDialogF, TJvBaseDlgEditor);
  {$IFDEF USEWINDOWS}
  RegisterComponentEditor(TJvCommonDialogD, TJvCommonDialogDEditor);
  {$ENDIF USEWINDOWS}
  RegisterComponentEditor(TJvTipOfDay, TJvTipOfDayEditor);
  {$IFNDEF BCB5}  // removed because BCB5 cannot compile/link JvDialogActns
  {$IFDEF VCL}
  RegisterActions(RsJVCLActionsCategory, [TJvBrowseForFolderAction,
    TJvSelectDirectoryAction, TJvConnectNetworkAction, TJvFloppyFormatAction,
    TJvOrganizeFavoritesAction, TJvControlPanelAction, TJvOpenFileAction,
    TJvSaveFileAction, TJvPageSetupAction, TJvPageSetupTitledAction],
    // TJvDialogActions is a datamodule with default settings for our dialog actions
    TJvDialogActions);
  {$ENDIF VCL}
  {$ENDIF BCB5}
end;

end.
