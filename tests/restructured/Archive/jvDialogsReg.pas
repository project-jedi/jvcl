unit jvDialogsReg;
{$I JEDI.INC}
interface

uses
  Windows,
  Classes,
  Controls,
  {$IFNDEF Delphi6_UP}
  DsgnIntf,
  {$ELSE}
  DesignIntf,
  DesignEditors,
  {$ENDIF}
  Dialogs,
  Forms,
  jvBaseDlg,
  jvBaseDlgEditor,
  jvConnectNetwork,
  jvDisconnectNetwork,
  jvAddPrinter,
  jvWinDialogs;


procedure Register;

implementation

{$R jvWinDialogs.dcr}

procedure Register;
begin
  RegisterComponents('Jv WinDialogs',
  [TJvConnectNetwork,
   TjvDisconnectNetwork,
   TJvOrganizeFavoritesDialog,
//   TJvFormatDialog,
   TJvComputerNameDialog,
//   TJvBrowseFolderDialog,
   TJvControlPanelDialog,
   TJvAppletDialog,
   TJvChangeIconDialog,
   TJvShellAboutDialog,
   TJvRunDialog,
   TJvObjectPropertiesDialog,
   TJvNewLinkDialog,
   TJvAddHardwareDialog,
   TJvOpenWithDialog,
   TJvDiskFullDialog,
   TJvExitWindowsDialog,
   TJvOutOfMemoryDialog,
   TJvOpenDialog2000,
   TJvSaveDialog2000,
   TJvAddPrinterDialog]);

   RegisterPropertyEditor(TypeInfo(String), TJvAppletDialog, 'AppletName', TAppletFileProperty);
   RegisterComponentEditor(TJvCommonDialog, TJvBaseDlgEditor);
   RegisterComponentEditor(TJvCommonDialogP,TJvBaseDlgEditor);
end;

end.
