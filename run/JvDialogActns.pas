{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStdActions.PAS, released on 2002-10-06.

The Initial Developer of the Original Code is Peter Th�rnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:

-----------------------------------------------------------------------------}
// $Id$

unit JvDialogActns;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, ActnList, StdActns,
  JvBaseDlg, JvBrowseFolder, JvSelectDirectory, JvConnectNetwork,
  JvWinDialogs, JvDialogs, JvPageSetupTitled, JvPageSetup;

type
  TJvCommonDialogClass = class of TJvCommonDialog;

  TJvCommonDialogAction = class(TCustomAction)
  private
    FExecuteResult: Boolean;
    FOnAccept: TNotifyEvent;
    FOnCancel: TNotifyEvent;
    FBeforeExecute: TNotifyEvent;
    FAfterExecute: TNotifyEvent;
  protected
    FDialog: TJvCommonDialog;
    function GetDialogClass: TJvCommonDialogClass; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure ExecuteTarget(Target: TObject); override;
    property ExecuteResult: Boolean read FExecuteResult;
  published
    property Caption;
    property Enabled;
    property HelpContext;
    property Hint;
    property ImageIndex;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property OnAccept: TNotifyEvent read FOnAccept write FOnAccept;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property BeforeExecute: TNotifyEvent read FBeforeExecute write FBeforeExecute;
    property AfterExecute: TNotifyEvent read FAfterExecute write FAfterExecute;
  end;

  // (rom) renamed to match renamed TJvBrowseForFolder
  TJvBrowseForFolderAction = class(TJvCommonDialogAction)
  private
    function GetDialog: TJvBrowseForFolderDialog;
  protected
    function GetDialogClass: TJvCommonDialogClass; override;
  published
    property Dialog: TJvBrowseForFolderDialog read GetDialog;
  end;

  TJvSelectDirectoryAction = class(TJvCommonDialogAction)
  private
    function GetDialog: TJvSelectDirectory;
  protected
    function GetDialogClass: TJvCommonDialogClass; override;
  published
    property Dialog: TJvSelectDirectory read GetDialog;
  end;

  TJvConnectNetworkAction = class(TJvCommonDialogAction)
  private
    function GetDialog: TJvNetworkConnect;
  protected
    function GetDialogClass: TJvCommonDialogClass; override;
  published
    property Dialog: TJvNetworkConnect read GetDialog;
  end;

  TJvFloppyFormatAction = class(TJvCommonDialogAction)
  private
    function GetDialog: TJvFormatDriveDialog;
  protected
    function GetDialogClass: TJvCommonDialogClass; override;
  published
    property Dialog: TJvFormatDriveDialog read GetDialog;
  end;

  TJvOrganizeFavoritesAction = class(TJvCommonDialogAction)
  private
    function GetDialog: TJvOrganizeFavoritesDialog;
  protected
    function GetDialogClass: TJvCommonDialogClass; override;
  published
    property Dialog: TJvOrganizeFavoritesDialog read GetDialog;
  end;

  TJvControlPanelAction = class(TJvCommonDialogAction)
  private
    function GetDialog: TJvAppletDialog;
  protected
    function GetDialogClass: TJvCommonDialogClass; override;
  published
    property Dialog: TJvAppletDialog read GetDialog;
  end;

  TJvOpenFileAction = class(TCommonDialogAction)
  private
    function GetDialog: TJvOpenDialog;
  protected
    function GetDialogClass: TCommonDialogClass; override;
  published
    property Caption;
    property Enabled;
    property HelpContext;
    property Hint;
    property ImageIndex;
    property ShortCut;
    property Visible;
    property SecondaryShortCuts;
    property OnAccept;
    property OnCancel;
  published
    property Dialog: TJvOpenDialog read GetDialog;
  end;

  TJvSaveFileAction = class(TJvOpenFileAction)
  private
    function GetDialog: TJvSaveDialog;
  protected
    function GetDialogClass: TCommonDialogClass; override;
  published
    property Dialog: TJvSaveDialog read GetDialog;
  end;

  TJvPageSetupAction = class(TJvCommonDialogAction)
  private
    function GetDialog: TJvPageSetupDialog;
  protected
    function GetDialogClass: TJvCommonDialogClass; override;
  published
    property Dialog: TJvPageSetupDialog read GetDialog;
  end;

  TJvPageSetupTitledAction = class(TJvCommonDialogAction)
  private
    function GetDialog: TJvPageSetupTitledDialog;
  protected
    function GetDialogClass: TJvCommonDialogClass; override;
  published
    property Dialog: TJvPageSetupTitledDialog read GetDialog;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation


//=== { TJvCommonDialogAction } ==============================================

constructor TJvCommonDialogAction.Create(AOwner: TComponent);
var
  DialogClass: TJvCommonDialogClass;
begin
  inherited Create(AOwner);
  DialogClass := GetDialogClass;
  if Assigned(DialogClass) then
  begin
    FDialog := DialogClass.Create(Self);
    FDialog.Name := Copy(DialogClass.ClassName, 2, Length(DialogClass.ClassName));
    FDialog.SetSubComponent(True);
  end;
  DisableIfNoHandler := False;
  Enabled := True;
end;

procedure TJvCommonDialogAction.ExecuteTarget(Target: TObject);
begin
  FExecuteResult := False;
  if Assigned(FDialog) then
  begin
    if Assigned(FBeforeExecute) then
      FBeforeExecute(Self);
    FExecuteResult := FDialog.Execute;
    if Assigned(FAfterExecute) then
      FAfterExecute(Self);
    if FExecuteResult then
    begin
      if Assigned(FOnAccept) then
        FOnAccept(Self)
    end
    else
    if Assigned(FOnCancel) then
      FOnCancel(Self);
  end;
end;

function TJvCommonDialogAction.GetDialogClass: TJvCommonDialogClass;
begin
  Result := nil;
end;

function TJvCommonDialogAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := True;
end;

//=== { TJvBrowseForFolderAction } ===========================================

function TJvBrowseForFolderAction.GetDialog: TJvBrowseForFolderDialog;
begin
  Result := TJvBrowseForFolderDialog(FDialog);
end;

function TJvBrowseForFolderAction.GetDialogClass: TJvCommonDialogClass;
begin
  Result := TJvBrowseForFolderDialog;
end;

//=== { TJvSelectDirectoryAction } ===========================================

function TJvSelectDirectoryAction.GetDialog: TJvSelectDirectory;
begin
  Result := TJvSelectDirectory(FDialog);
end;

function TJvSelectDirectoryAction.GetDialogClass: TJvCommonDialogClass;
begin
  Result := TJvSelectDirectory;
end;

//=== { TJvConnectNetworkAction } ============================================

function TJvConnectNetworkAction.GetDialog: TJvNetworkConnect;
begin
  Result := TJvNetworkConnect(FDialog);
end;

function TJvConnectNetworkAction.GetDialogClass: TJvCommonDialogClass;
begin
  Result := TJvNetworkConnect;
end;

//=== { TJvFloppyFormatAction } ==============================================

function TJvFloppyFormatAction.GetDialog: TJvFormatDriveDialog;
begin
  Result := TJvFormatDriveDialog(FDialog);
end;

function TJvFloppyFormatAction.GetDialogClass: TJvCommonDialogClass;
begin
  Result := TJvFormatDriveDialog;
end;

//=== { TJvOrganizeFavoritesAction } =========================================

function TJvOrganizeFavoritesAction.GetDialog: TJvOrganizeFavoritesDialog;
begin
  Result := TJvOrganizeFavoritesDialog(FDialog);
end;

function TJvOrganizeFavoritesAction.GetDialogClass: TJvCommonDialogClass;
begin
  Result := TJvOrganizeFavoritesDialog;
end;

//=== { TJvControlPanelAction } ==============================================

function TJvControlPanelAction.GetDialog: TJvAppletDialog;
begin
  Result := TJvAppletDialog(FDialog);
end;

function TJvControlPanelAction.GetDialogClass: TJvCommonDialogClass;
begin
  Result := TJvAppletDialog;
end;

//=== { TJvOpenFileAction } ==================================================

function TJvOpenFileAction.GetDialog: TJvOpenDialog;
begin
  Result := TJvOpenDialog(FDialog);
end;

function TJvOpenFileAction.GetDialogClass: TCommonDialogClass;
begin
  Result := TJvOpenDialog;
end;

//=== { TJvSaveFileAction } ==================================================

function TJvSaveFileAction.GetDialog: TJvSaveDialog;
begin
  Result := TJvSaveDialog(FDialog);
end;

function TJvSaveFileAction.GetDialogClass: TCommonDialogClass;
begin
  Result := TJvSaveDialog;
end;

//=== { TJvPageSetupAction } =================================================

function TJvPageSetupAction.GetDialog: TJvPageSetupDialog;
begin
  Result := TJvPageSetupDialog(FDialog);
end;

function TJvPageSetupAction.GetDialogClass: TJvCommonDialogClass;
begin
  Result := TJvPageSetupDialog;
end;

//=== { TJvPageSetupTitledAction } ===========================================

function TJvPageSetupTitledAction.GetDialog: TJvPageSetupTitledDialog;
begin
  Result := TJvPageSetupTitledDialog(FDialog);
end;

function TJvPageSetupTitledAction.GetDialogClass: TJvCommonDialogClass;
begin
  Result := TJvPageSetupTitledDialog;
end;

//=== { TCommonDialogAction } ================================================

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
