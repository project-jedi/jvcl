{$I JEDI.INC}
unit jvWinDialogsReg;

interface

uses
  Windows,
  Classes,
  Controls,
  jvWinDialogs,
  {$IFNDEF DELPHI6_UP}
  DsgnIntf,
  {$ELSE}
  DesignIntf,
  DesignEditors,
  {$ENDIF}
  Dialogs,
  Forms;

type
  TAppletFileProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Jv WinDialogs',
  [TJvOrganizeFavoritesDialog,
   TJvFormatDialog,
   TJvBrowseFolderDialog,
   TJvComputerNameDialog,
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
   TJvOutOfMemoryDialog]);
   RegisterPropertyEditor(TypeInfo(String), TJvAppletDialog, 'AppletName', TAppletFileProperty);
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
