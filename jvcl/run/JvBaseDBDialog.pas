{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBaseDBLogonDialog.pas, released on 2006-07-21

The Initial Developer of the Original Code is Jens Fudickar
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvBaseDBDialog;

{$I jvcl.inc}

interface

uses
{$IFDEF UNITVERSIONING}
  JclUnitVersioning,
{$ENDIF UNITVERSIONING}
  JvDynControlEngine,
  Windows, Classes, JvBaseDlg, JvAppStorage, Forms, Controls;

type
  TJvBaseDBDialog = class(TJvCommonDialog)
  private
    FAppStorage: TJvCustomAppStorage;
    FAppStoragePath: string;
    FDBDialog: TForm;
    FDynControlEngine: TJvDynControlEngine;
    FSession: TComponent;
    FParentWnd: HWND;
    function GetDynControlEngine: TJvDynControlEngine;
  protected
    function CreateForm: TForm; virtual;
    procedure CreateFormControls(aForm: TForm); virtual;
    procedure AfterCreateFormControls(aForm: TForm); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAppStorage(Value: TJvCustomAppStorage); virtual;
    procedure SetAppStoragePath(Value: string); virtual;
    procedure SetSession(const Value: TComponent); virtual;
    property AppStorage: TJvCustomAppStorage read FAppStorage write SetAppStorage;
    property AppStoragePath: string read FAppStoragePath write SetAppStoragePath;
  public
    function Execute(ParentWnd: HWND): Boolean; overload; override;
    function SessionIsConnected: Boolean; virtual;
    property DBDialog: TForm read FDBDialog ;
    property Session: TComponent read FSession write SetSession;
  published
    property DynControlEngine: TJvDynControlEngine read GetDynControlEngine write FDynControlEngine;
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

uses
  SysUtils, Types,
  JvJCLUtils, // SetWindowLongPtr for older Delphi versions
  JvJVCLUtils;

function TJvBaseDBDialog.CreateForm: TForm;
begin
  Result := TForm(DynControlEngine.CreateForm('', ''));
  CreateFormControls(Result);
  if FParentWnd <> 0 then
    SetWindowLongPtr(Result.Handle, GWLP_HWNDPARENT, LONG_PTR(FParentWnd));
end;

procedure TJvBaseDBDialog.CreateFormControls(aForm: TForm);
begin
end;

procedure TJvBaseDBDialog.AfterCreateFormControls(aForm: TForm);
begin
end;

function TJvBaseDBDialog.Execute(ParentWnd: HWND): Boolean;
begin
  if not Assigned(Session) then
    Abort;
  FParentWnd := ParentWnd;
  FDBDialog := CreateForm;
  try
    AfterCreateFormControls(FDBDialog);
    FDBDialog.ShowModal;
    Result := FDBDialog.ModalResult = mrOk;
  finally
    FreeAndNil(FDBDialog);
  end;
end;

function TJvBaseDBDialog.GetDynControlEngine: TJvDynControlEngine;
begin
  if Assigned(FDynControlEngine) then
    Result := FDynControlEngine
  else
    Result := DefaultDynControlEngine;
end;

procedure TJvBaseDBDialog.Notification(AComponent: TComponent; Operation:
    TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
    if (AComponent = FAppStorage) then
      FAppStorage := nil
    else if (AComponent = FSession) then
      FSession := nil
    else if (AComponent = FDBDialog) then
      FDBDialog := nil;
end;

function TJvBaseDBDialog.SessionIsConnected: Boolean;
begin
  Result := False;
end;

procedure TJvBaseDBDialog.SetAppStorage(Value: TJvCustomAppStorage);
begin
  ReplaceComponentReference(Self, Value, TComponent(FAppStorage));
end;

procedure TJvBaseDBDialog.SetAppStoragePath(Value: string);
begin
  if Value <> AppStoragePath then
    FAppStoragePath := Value;
end;

procedure TJvBaseDBDialog.SetSession(const Value: TComponent);
begin
  ReplaceComponentReference(Self, Value, FSession);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
