{$I JVCL.INC}

unit JvDlgsReg;

interface

procedure Register;

implementation
uses
  Classes, DesignIntf, ActnList, JvBaseDlg, 
  JvDSADialogs, JvTipOfDay, JvWinDialogs, JvAddPrinter, JvCommonDialogD, JvCommonExecDlg,
  JvDialogActns, JvActnRes, JvDialogs, JvProgressForm, JvPageSetupTitled, JvPageSetup, JvConnectNetwork,
  JvSelectDirectory, JvCopyError, JvDeleteError, JvRenameError, JvDiskPrompt, JvFindFiles,
  JvImageDlg, JvLoginForm,
  JvAppletEditor, JvObjectPickerDialog, JvBaseDlgEditor, JvCommonDialogDEditor, JvTipOfDayEditor;

{.$R ..\resources\JvDlgsReg.dcr}

procedure Register;
begin
  RegisterComponents('Jv Dialogs',[
    TJvDSADialog, TJvTipOfDay,
    TJvFormatDriveDialog, TJvOrganizeFavoritesDialog, TJvAppletDialog,
    
    TJvProgressForm, TJvImageDlg, TJvLoginDialog 

    ]);

  RegisterPropertyEditor(TypeInfo(string), TJvAppletDialog, 'AppletName', TJvAppletNameProperty);
  RegisterPropertyEditor(TypeInfo(integer), TJvAppletDialog, 'AppletIndex', TJvAppletIndexProperty);
    
  {$IFDEF JVCL_REGISTER_GLOBAL_DESIGNEDITORS}
  RegisterComponentEditor(TCommonDialog, TJvBaseDlgEditor);
  {$ENDIF}
  RegisterComponentEditor(TJvCommonDialog, TJvBaseDlgEditor);
  RegisterComponentEditor(TJvOpenDialog, TJvBaseDlgEditor);
  RegisterComponentEditor(TJvSaveDialog, TJvBaseDlgEditor);
  RegisterComponentEditor(TJvCommonDialogP, TJvBaseDlgEditor);
  RegisterComponentEditor(TJvCommonDialogF, TJvBaseDlgEditor);
  RegisterComponentEditor(TJvCommonDialogD, TJvCommonDialogDEditor);
  RegisterComponentEditor(TJvTipOfDay, TJvTipOfDayEditor);
  RegisterActions('JVCL',
    [TJvBrowseForFolderAction, TJvSelectDirectoryAction, TJvConnectNetworkAction,
     TJvFloppyFormatAction, TJvOrganizeFavoritesAction, TJvControlPanelAction,
     TJvOpenFileAction, TJvSaveFileAction, TJvPageSetupAction,
     TJvPageSetupTitledAction],
    // TJvStandardActions is a datamodule with default settings for our actions
    TJvStandardActions);
end;

end.
