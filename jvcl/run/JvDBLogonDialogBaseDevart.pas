{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBLogonDialogOdac.pas, released on 2006-07-21.

The Initial Developer of the Original Code is Jens Fudickar
All Rights Reserved.

Contributor(s):
Jens Fudickar

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBLogonDialogBaseDevart;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF USE_3RDPARTY_DEVART_DAC}
  Classes, Forms, Controls, DBAccess,
  JvAppStorage, JvBaseDBLogonDialog,
  JvDynControlEngine, JvBaseDBPasswordDialog,
  {$ENDIF USE_3RDPARTY_DEVART_DAC}
  JvDynControlEngineIntf;

{$IFDEF USE_3RDPARTY_DEVART_DAC}
type

  TJvDBBaseDevartConnectDialog = class(TCustomConnectDialog)
  private
    FLogonDialogInternal: TJvBaseDBLogonDialog;
    function GetAfterTransferSessionDataToConnectionInfo: TJvLogonDialogConnectionInfoEvent;
    function GetAppStorage: TJvCustomAppStorage;
    function GetAppStoragePath: string;
    function GetBeforeTransferConnectionInfoToSessionData: TJvLogonDialogConnectionInfoEvent;
    function GetCurrentConnectionInfo: TJvBaseConnectionInfo;
    function GetDynControlEngine: TJvDynControlEngine;
    function GetOnDecryptPassword: TJvLogonDialogEncryptDecryptEvent;
    function GetOnEncryptPassword: TJvLogonDialogEncryptDecryptEvent;
    function GetOnFillDatabaseList: TJvLogonDialogFillListEvent;
    function GetOnFillShortcutList: TJvLogonDialogFillListEvent;
    function GetOnSessionConnect: TJvLogonDialogBaseSessionEvent;
    procedure SetAfterTransferSessionDataToConnectionInfo(const Value: TJvLogonDialogConnectionInfoEvent);
    procedure SetBeforeTransferConnectionInfoToSessionData(const Value: TJvLogonDialogConnectionInfoEvent);
    procedure SetDynControlEngine(const Value: TJvDynControlEngine);
    procedure SetOnDecryptPassword(const Value: TJvLogonDialogEncryptDecryptEvent);
    procedure SetOnEncryptPassword(const Value: TJvLogonDialogEncryptDecryptEvent);
    procedure SetOnFillDatabaseList(const Value: TJvLogonDialogFillListEvent);
    procedure SetOnFillShortcutList(const Value: TJvLogonDialogFillListEvent);
    procedure SetOnSessionConnect(const Value: TJvLogonDialogBaseSessionEvent);
  protected
    function CreateLogonDialogInternal: TJvBaseDBLogonDialog; virtual; abstract;
    function GetLogonDialogInternal: TJvBaseDBLogonDialog; virtual;
    property LogonDialogInternal: TJvBaseDBLogonDialog read GetLogonDialogInternal;
    function GetOptions: TJvBaseDBLogonDialogOptions; virtual;
    procedure SetAppStorage(Value: TJvCustomAppStorage);
    procedure SetAppStoragePath(Value: string); virtual;
    procedure SetOptions(const Value: TJvBaseDBLogonDialogOptions); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;
    function ExecuteOnSession(Session: TCustomDAConnection): Boolean;
    property CurrentConnectionInfo: TJvBaseConnectionInfo read GetCurrentConnectionInfo;
  published
    //1 This events gives you the possibility to modify the connection data after receiving the data from the current session
    property AfterTransferSessionDataToConnectionInfo: TJvLogonDialogConnectionInfoEvent read
        GetAfterTransferSessionDataToConnectionInfo write SetAfterTransferSessionDataToConnectionInfo;
    property AppStorage: TJvCustomAppStorage read GetAppStorage write SetAppStorage;
    property AppStoragePath: string read GetAppStoragePath write SetAppStoragePath;
    //1 This Event gives you the possibility to modify the connection data before it is transfered to the current session
    property BeforeTransferConnectionInfoToSessionData: TJvLogonDialogConnectionInfoEvent read
        GetBeforeTransferConnectionInfoToSessionData write SetBeforeTransferConnectionInfoToSessionData;
    property DynControlEngine: TJvDynControlEngine read GetDynControlEngine write SetDynControlEngine;
    property Options: TJvBaseDBLogonDialogOptions read GetOptions write SetOptions;
    property OnDecryptPassword: TJvLogonDialogEncryptDecryptEvent read GetOnDecryptPassword write SetOnDecryptPassword;
    property OnEncryptPassword: TJvLogonDialogEncryptDecryptEvent read GetOnEncryptPassword write SetOnEncryptPassword;
    //1 Event for filling the database list
    property OnFillDatabaseList: TJvLogonDialogFillListEvent read GetOnFillDatabaseList write SetOnFillDatabaseList;
    //1 Event for customizing the shortcut list
    property OnFillShortcutList: TJvLogonDialogFillListEvent read GetOnFillShortcutList write SetOnFillShortcutList;
    property OnSessionConnect: TJvLogonDialogBaseSessionEvent read GetOnSessionConnect write SetOnSessionConnect;
  end;
{$ENDIF USE_3RDPARTY_DEVART_DAC}

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

{$IFDEF USE_3RDPARTY_DEVART_DAC}
uses
  SysUtils, StdCtrls, Dialogs,
  JvDSADialogs,  JvResources;

//=== { TJvDBBaseDevartConnectDialog } =============================================

constructor TJvDBBaseDevartConnectDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLogonDialogInternal := CreateLogonDialogInternal;
end;

destructor TJvDBBaseDevartConnectDialog.Destroy;
begin
  FreeAndNil(FLogonDialogInternal);
  inherited Destroy;
end;

function TJvDBBaseDevartConnectDialog.Execute: Boolean;
begin
  Result := ExecuteOnSession(Connection);
end;

function TJvDBBaseDevartConnectDialog.ExecuteOnSession(Session: TCustomDAConnection):
    Boolean;
begin
  if Assigned(FLogonDialogInternal) then
  begin
    LogonDialogInternal.Session := Session;
    Result := LogonDialogInternal.Execute;
  end
  else
    Result := False;
end;

function TJvDBBaseDevartConnectDialog.GetAfterTransferSessionDataToConnectionInfo: TJvLogonDialogConnectionInfoEvent;
begin
  Result := LogonDialogInternal.AfterTransferSessionDataToConnectionInfo;
end;

function TJvDBBaseDevartConnectDialog.GetAppStorage: TJvCustomAppStorage;
begin
  Result := LogonDialogInternal.AppStorage;
end;

function TJvDBBaseDevartConnectDialog.GetAppStoragePath: string;
begin
  Result := LogonDialogInternal.AppStoragePath;
end;

function TJvDBBaseDevartConnectDialog.GetBeforeTransferConnectionInfoToSessionData: TJvLogonDialogConnectionInfoEvent;
begin
  Result := LogonDialogInternal.BeforeTransferConnectionInfoToSessionData;
end;

function TJvDBBaseDevartConnectDialog.GetCurrentConnectionInfo: TJvBaseConnectionInfo;
begin
  Result := LogonDialogInternal.CurrentConnectionInfo;
end;

function TJvDBBaseDevartConnectDialog.GetDynControlEngine: TJvDynControlEngine;
begin
  Result := LogonDialogInternal.DynControlEngine
end;

function TJvDBBaseDevartConnectDialog.GetLogonDialogInternal: TJvBaseDBLogonDialog;
begin
  Result := FLogonDialogInternal;
end;

function TJvDBBaseDevartConnectDialog.GetOnDecryptPassword: TJvLogonDialogEncryptDecryptEvent;
begin
  Result := LogonDialogInternal.OnDecryptPassword;
end;

function TJvDBBaseDevartConnectDialog.GetOnEncryptPassword: TJvLogonDialogEncryptDecryptEvent;
begin
  Result := LogonDialogInternal.OnEncryptPassword;
end;

function TJvDBBaseDevartConnectDialog.GetOnFillDatabaseList: TJvLogonDialogFillListEvent;
begin
  Result := LogonDialogInternal.OnFillDatabaseList;
end;

function TJvDBBaseDevartConnectDialog.GetOnFillShortcutList: TJvLogonDialogFillListEvent;
begin
  Result := LogonDialogInternal.OnFillShortcutList;
end;

function TJvDBBaseDevartConnectDialog.GetOnSessionConnect: TJvLogonDialogBaseSessionEvent;
begin
  Result := LogonDialogInternal.OnSessionConnect;
end;

function TJvDBBaseDevartConnectDialog.GetOptions: TJvBaseDBLogonDialogOptions;
begin
  Result := LogonDialogInternal.Options;
end;

procedure TJvDBBaseDevartConnectDialog.SetAfterTransferSessionDataToConnectionInfo(const Value:
    TJvLogonDialogConnectionInfoEvent);
begin
  LogonDialogInternal.AfterTransferSessionDataToConnectionInfo := Value;
end;

procedure TJvDBBaseDevartConnectDialog.SetAppStorage(Value: TJvCustomAppStorage);
begin
  LogonDialogInternal.AppStorage := Value;
end;

procedure TJvDBBaseDevartConnectDialog.SetAppStoragePath(Value: string);
begin
  LogonDialogInternal.AppStoragePath := Value;
end;

procedure TJvDBBaseDevartConnectDialog.SetBeforeTransferConnectionInfoToSessionData(const Value:
    TJvLogonDialogConnectionInfoEvent);
begin
  LogonDialogInternal.BeforeTransferConnectionInfoToSessionData := Value;
end;

procedure TJvDBBaseDevartConnectDialog.SetDynControlEngine(const Value: TJvDynControlEngine);
begin
  LogonDialogInternal.DynControlEngine := Value;
end;

procedure TJvDBBaseDevartConnectDialog.SetOnDecryptPassword(const Value: TJvLogonDialogEncryptDecryptEvent);
begin
  LogonDialogInternal.OnDecryptPassword := Value;
end;

procedure TJvDBBaseDevartConnectDialog.SetOnEncryptPassword(const Value: TJvLogonDialogEncryptDecryptEvent);
begin
  LogonDialogInternal.OnEncryptPassword := Value;
end;

procedure TJvDBBaseDevartConnectDialog.SetOnFillDatabaseList(const Value: TJvLogonDialogFillListEvent);
begin
  LogonDialogInternal.OnFillDatabaseList := Value;
end;

procedure TJvDBBaseDevartConnectDialog.SetOnFillShortcutList(const Value: TJvLogonDialogFillListEvent);
begin
  LogonDialogInternal.OnFillShortcutList := Value;
end;

procedure TJvDBBaseDevartConnectDialog.SetOnSessionConnect(const Value: TJvLogonDialogBaseSessionEvent);
begin
  LogonDialogInternal.OnSessionConnect:= Value;
end;

procedure TJvDBBaseDevartConnectDialog.SetOptions(const Value: TJvBaseDBLogonDialogOptions);
begin
  LogonDialogInternal.Options.Assign(Value);
end;

{$ENDIF USE_3RDPARTY_DEVART_DAC}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
