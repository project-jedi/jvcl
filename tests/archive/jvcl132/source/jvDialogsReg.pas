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

type
  TAppletFileProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;


procedure Register;

implementation

{$R jvWinDialogs.dcr}

procedure Register;
begin
  RegisterComponents('Jv WinDialogs',
  [TJvConnectNetwork,
   TjvDisconnectNetwork,
   TJvOrganizeFavoritesDialog,
   TJvFormatDialog,
   TJvComputerNameDialog,
   TJvBrowseFolderDialog,
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



{property editor for TJvAppletDialog component}
procedure TAppletFileProperty.Edit;
var
  APFileOpen: TOpenDialog;
begin
  APFileOpen := TOpenDialog.Create(Application);
  APFileOpen.Filename := GetValue;
  APFileOpen.Filter := 'Applet File (*.cpl)|*.cpl';
  APFileOpen.Options := APFileOpen.Options + [ofPathMustExist,
    ofFileMustExist];
  try
    if APFileOpen.Execute then SetValue(APFileOpen.Filename);
  finally
    APFileOpen.Free;
  end;
end;

function TAppletFileProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;





end.
