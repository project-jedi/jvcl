{$I JVCL.INC}

unit JvDlgsReg;

interface

procedure Register;

implementation
uses
  Classes, DesignIntf,
  JvDSADialogs, JvBaseDlg, JvTipOfDay, JvWinDialogs, JvAddPrinter, JvCommonDialogD, JvCommonExecDlg, JvDialogActns,
  JvDialogs, JvProgressForm, JvPageSetupTitled, JvPageSetup, JvConnectNetwork, JvSelectDirectory,
  JvCopyError, JvDeleteError, JvRenameError, JvDiskPrompt, JvFindFiles, JvObjectPickerDialog,
  JvBaseDlgEditor, JvCommonDialogDEditor, JvTipOfDayEditor;

{.$R ..\resources\JvDlgsReg.dcr}

procedure Register;
begin
//  RegisterComponents('',[]);
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
end;

end.
