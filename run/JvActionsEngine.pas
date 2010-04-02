{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvActionsEngine.Pas, released on 2007-03-12.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudicker  att oratool dott de]
Portions created by Jens Fudickar are Copyright (C) 2007 Jens Fudickar.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvActionsEngine;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows, ActnList, ImgList, Graphics,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  QWindows, QActnList, QImgList, QGraphics,
  {$ENDIF UNIX}
  Controls, Classes;

type
  TJvActionEngineBaseAction = class;

  TJvChangeActionComponent = procedure(ActionComponent: TComponent) of object;

  TJvActionBaseEngine = class(TComponent)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    function SupportsComponent(AComponent: TComponent): Boolean; virtual;
    function SupportsAction(AAction: TJvActionEngineBaseAction): Boolean; virtual;
  published
  end;

  TJvActionBaseEngineClass = class of TJvActionBaseEngine;

  TJvActionEngineList = class(TList)
  private
    function GetEngine(Index: Integer): TJvActionBaseEngine;
  public
    destructor Destroy; override;
    procedure RegisterEngine(AEngineClass: TJvActionBaseEngineClass);
    function GetControlEngine(AComponent: TComponent; AAction:
        TJvActionEngineBaseAction): TJvActionBaseEngine; virtual;
    function Supports(AComponent: TComponent; AAction: TJvActionEngineBaseAction =
        nil): Boolean;
    property Engine[Index: Integer]: TJvActionBaseEngine read GetEngine;
  end;

  TJvActionEngineBaseAction = class(TAction)
  private
    FActionComponent: TComponent;
    FControlEngine: TJvActionBaseEngine;
    FLastTarget: TObject;
    FOnChangeActionComponent: TJvChangeActionComponent;
  protected
    function DetectControlEngine(aActionComponent: TComponent): Boolean; virtual;
    function GetEngineList: TJvActionEngineList; virtual; abstract;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetActionComponent(const Value: TComponent); virtual;
    //1 This Procedure is called when the ActionComponent is changed
    procedure ChangeActionComponent(const AActionComponent: TComponent); virtual;
    procedure CheckEnabled(var AEnabled: Boolean); virtual;
    procedure CheckVisible(var AVisible: Boolean); virtual;
    procedure SetEnabled(Value: Boolean);
    procedure SetVisible(Value: Boolean);
    procedure SetImageIndex(Value: Integer);
    property ControlEngine: TJvActionBaseEngine read FControlEngine;
    property EngineList: TJvActionEngineList read GetEngineList;
    property ActionComponent: TComponent read FActionComponent write
        SetActionComponent;
    property LastTarget: TObject read FLastTarget;
    //1 Use this event to check the Enabled Flag depending on properties of the ActionComponent
    property OnChangeActionComponent: TJvChangeActionComponent read
        FOnChangeActionComponent write FOnChangeActionComponent;
  public
    constructor Create(AOwner: TComponent); override;
    function HandlesTarget(Target: TObject): Boolean; override;
  end;

  {$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile:
      '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );
  {$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils, Grids, TypInfo, StrUtils, Variants,
  JvResources, JvJVCLUtils;

//=== { TJvActionEngineList } ========================================

destructor TJvActionEngineList.Destroy;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    TJvActionBaseEngine(Items[I]).Free;
    Items[I] := nil;
    Delete(I);
  end;
  inherited Destroy;
end;

procedure TJvActionEngineList.RegisterEngine(AEngineClass: TJvActionBaseEngineClass);
begin
  Add(AEngineClass.Create(nil));
end;

function TJvActionEngineList.GetControlEngine(AComponent: TComponent; AAction:
    TJvActionEngineBaseAction): TJvActionBaseEngine;
var
  Ind: Integer;
begin
  Result := nil;
  for Ind := 0 to Count - 1 do
    if Engine[Ind].SupportsComponent(AComponent) then
      if not Assigned(AAction) or Engine[Ind].SupportsAction(AAction) then
      begin
        Result := TJvActionBaseEngine(Items[Ind]);
        Break;
      end;
end;

function TJvActionEngineList.GetEngine(Index: Integer): TJvActionBaseEngine;
begin
  Result := TJvActionBaseEngine(Items[Index]);
end;

function TJvActionEngineList.Supports(AComponent: TComponent; AAction:
    TJvActionEngineBaseAction = nil): Boolean;
begin
  Result := Assigned(GetControlEngine(AComponent, AAction));
end;

constructor TJvActionBaseEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TJvActionBaseEngine.SupportsComponent(AComponent: TComponent): Boolean;
begin
  Result := False;
end;

function TJvActionBaseEngine.SupportsAction(AAction:
    TJvActionEngineBaseAction): Boolean;
begin
  Result := False;
end;

constructor TJvActionEngineBaseAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLastTarget := nil;
  FControlEngine := nil;
  FActionComponent := nil;
end;

function TJvActionEngineBaseAction.DetectControlEngine(aActionComponent:
  TComponent): Boolean;
begin
  if Assigned(EngineList) and Assigned(aActionComponent) then
    FControlEngine := EngineList.GetControlEngine(aActionComponent, self)
  else
    FControlEngine := nil;
  Result := Assigned(FControlEngine);
end;

function TJvActionEngineBaseAction.HandlesTarget(Target: TObject): Boolean;
begin
  if Target is TComponent then
  begin
    ActionComponent := TComponent(Target);
    Result := Assigned(ControlEngine);
  end
  else
    Result := False;
end;

procedure TJvActionEngineBaseAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FActionComponent) then
    ActionComponent := nil;
end;

//=== { TJvActionEngineBaseAction } ========================================

procedure TJvActionEngineBaseAction.SetActionComponent(const Value: TComponent);
var
  intValue: TComponent;
  changed: Boolean;
begin
  if FLastTarget <> Value then
  begin
    FLastTarget := Value;
    if DetectControlEngine(Value) then
      intValue := Value
    else
      intValue := nil;
    Changed := FActionComponent <> intValue;
    ReplaceComponentReference(Self, intValue, FActionComponent);
    if changed then
      ChangeActionComponent(FActionComponent);
  end;
end;

procedure TJvActionEngineBaseAction.ChangeActionComponent(const
    AActionComponent: TComponent);
begin
  if AActionComponent <> nil then
    AActionComponent.FreeNotification(Self);
  if Assigned(OnChangeActionComponent) then
    OnChangeActionComponent(AActionComponent);
end;

procedure TJvActionEngineBaseAction.CheckEnabled(var AEnabled: Boolean);
begin
end;

procedure TJvActionEngineBaseAction.CheckVisible(var AVisible: Boolean);
begin
end;

procedure TJvActionEngineBaseAction.SetEnabled(Value: Boolean);
begin
  CheckEnabled (Value);
  if Enabled <> Value then
    Enabled := Value;
end;

procedure TJvActionEngineBaseAction.SetVisible(Value: Boolean);
begin
  CheckVisible(Value);
  if Visible <> Value then
    Visible := Value;
end;

procedure TJvActionEngineBaseAction.SetImageIndex(Value: Integer);
begin
  if ImageIndex <> Value then
    ImageIndex := Value;
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
