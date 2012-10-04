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
  Windows, ActnList, Graphics, ImgList,
  {$ENDIF MSWINDOWS}
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
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
    function GetControlEngine(AComponent: TComponent; AAction: TJvActionEngineBaseAction): TJvActionBaseEngine; virtual;
    function Supports(AComponent: TComponent; AAction: TJvActionEngineBaseAction = nil): Boolean;
    property Engine[Index: Integer]: TJvActionBaseEngine read GetEngine;
  end;

  TJvActionEngineBaseAction = class(TAction)
  private
    FActionComponent: TComponent;
    FControlEngine: TJvActionBaseEngine;
    FLastTarget: TComponent;
    FOnChangeActionComponent: TJvChangeActionComponent;
  protected
    //1 This Procedure is called when the ActionComponent is changed
    procedure ChangeActionComponent(const AActionComponent: TComponent); virtual;
    procedure CheckChecked(var AChecked: Boolean); virtual;
    procedure CheckEnabled(var AEnabled: Boolean); virtual;
    procedure CheckVisible(var AVisible: Boolean); virtual;
    function DetectControlEngine(aActionComponent: TComponent): Boolean; virtual;
    function GetEngineList: TJvActionEngineList; virtual; abstract;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetActionComponent(const Value: TComponent); virtual;
    property ControlEngine: TJvActionBaseEngine read FControlEngine;
    property EngineList: TJvActionEngineList read GetEngineList;
    property LastTarget: TComponent read FLastTarget;
    //1 Use this event to check the Enabled Flag depending on properties of the ActionComponent
    property OnChangeActionComponent: TJvChangeActionComponent read FOnChangeActionComponent write FOnChangeActionComponent;
  public
    constructor Create(AOwner: TComponent); override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure SetChecked(Value: Boolean); {$IFDEF RTL240_UP}override;{$ENDIF RTL240_UP}
    procedure SetEnabled(Value: Boolean); {$IFDEF RTL240_UP}override;{$ENDIF RTL240_UP}
    procedure SetImageIndex(Value: TImageIndex); {$IFDEF RTL240_UP}override;{$ENDIF RTL240_UP}
    procedure SetParentComponent(AParent: TComponent); override;
    procedure SetVisible(Value: Boolean); {$IFDEF RTL240_UP}override;{$ENDIF RTL240_UP}
    procedure UpdateTarget(Target: TObject); override;
    property ActionComponent: TComponent read FActionComponent write SetActionComponent;
  end;

type
  TJvActionBaseActionList = class(TActionList)
  //The idea of the Action Classes is to work different type of controls.
  //
  //Then we have a list of ActionEngines which have the availability to
  //validate find for a Component if it is supported or not.
  //For each new type of controls with specific need of handles a new Engine
  //must be created and registered. An example for these engines can be found
  //in "JvDBActionsEngineControlCxGrid.pas".
  //
  //When a ActionComponent is assigned the action tries to find the correct
  //engine based on the component and uses the engine for all further operations.
  //
  //There are two ways to assign a ActionComponent:
  //1. Assigning the component to the action list, then all actions in
  //   this list (which are based on TJvActionEngineBaseAction class)
  //   gets the ActionComponent assigned also.
  //2. Using the active control, like the normal action handling.
  private
    FActionComponent: TComponent;
    FOnChangeActionComponent: TJvChangeActionComponent;
  protected
    procedure SetActionComponent(Value: TComponent);
    property ActionComponent: TComponent read FActionComponent write SetActionComponent;
    property OnChangeActionComponent: TJvChangeActionComponent read FOnChangeActionComponent write FOnChangeActionComponent;
  public
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
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
  SysUtils, Variants,
  JvJVCLUtils;

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

function TJvActionEngineList.GetControlEngine(AComponent: TComponent; AAction: TJvActionEngineBaseAction):
    TJvActionBaseEngine;
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

function TJvActionEngineList.Supports(AComponent: TComponent; AAction: TJvActionEngineBaseAction = nil): Boolean;
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

function TJvActionBaseEngine.SupportsAction(AAction: TJvActionEngineBaseAction): Boolean;
begin
  Result := False;
end;

constructor TJvActionEngineBaseAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLastTarget := nil;
  FControlEngine := nil;
  if Assigned(AOwner) and (AOwner is TJvActionBaseActionList) then
    ActionComponent := TJvActionBaseActionList(AOwner).ActionComponent
  else
    FActionComponent := nil;
end;

procedure TJvActionEngineBaseAction.ChangeActionComponent(const
    AActionComponent: TComponent);
begin
  if Assigned(OnChangeActionComponent) then
    OnChangeActionComponent(AActionComponent);
end;

procedure TJvActionEngineBaseAction.CheckChecked(var AChecked: Boolean);
begin
end;

procedure TJvActionEngineBaseAction.CheckEnabled(var AEnabled: Boolean);
begin
end;

procedure TJvActionEngineBaseAction.CheckVisible(var AVisible: Boolean);
begin
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

procedure TJvActionEngineBaseAction.SetChecked(Value: Boolean);
begin
  CheckChecked (Value);
  if Checked <> Value then
  {$IFDEF RTL240_UP}
    inherited SetChecked (Value);
  {$ELSE}
    Checked := Value;
  {$ENDIF RTL240_UP}
end;

procedure TJvActionEngineBaseAction.SetEnabled(Value: Boolean);
begin
  CheckEnabled (Value);
  if Enabled <> Value then
  {$IFDEF RTL240_UP}
    inherited SetEnabled (Value);
  {$ELSE}
    Enabled := Value;
  {$ENDIF RTL240_UP}
end;

procedure TJvActionEngineBaseAction.SetImageIndex(Value: TImageIndex);
begin
  if ImageIndex <> Value then
  {$IFDEF RTL240_UP}
    inherited SetImageIndex (Value);
  {$ELSE}
    ImageIndex := Value;
  {$ENDIF RTL240_UP}
end;

procedure TJvActionEngineBaseAction.SetParentComponent(AParent: TComponent);
begin
  Inherited SetParentComponent(AParent);
  if AParent is TJvActionBaseActionList then
    ActionComponent := TJvActionBaseActionList(AParent).ActionComponent;
end;

procedure TJvActionEngineBaseAction.SetVisible(Value: Boolean);
begin
  CheckVisible(Value);
  if Visible <> Value then
  {$IFDEF RTL240_UP}
    inherited SetVisible (Value);
  {$ELSE}
    Visible := Value;
  {$ENDIF RTL240_UP}
end;

procedure TJvActionEngineBaseAction.UpdateTarget(Target: TObject);
begin
  if Assigned(ControlEngine) then
    ControlEngine.UpdateAction(self)
  else
    inherited UpdateTarget(Target);
end;

//=== { TJvDatabaseActionList } ==============================================

procedure TJvActionBaseActionList.SetActionComponent(Value: TComponent);
var
  I: Integer;
begin
  if ReplaceComponentReference(Self, Value, FActionComponent) then
  begin
    for I := 0 to ActionCount - 1 do
      if Actions[I] is TJvActionEngineBaseAction then
        TJvActionEngineBaseAction(Actions[I]).ActionComponent := Value;
    if Assigned(OnChangeActionComponent) then
      OnChangeActionComponent(Value);
  end;
end;

procedure TJvActionBaseActionList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FActionComponent then
      ActionComponent := nil;
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
