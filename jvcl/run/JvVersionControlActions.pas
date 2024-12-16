{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvVersionControlActions.Pas, released on 2008-07-13.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudicker  att oratool dott de]
Portions created by Jens Fudickar are Copyright (C) 2002 Jens Fudickar.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvVersionControlActions;

{$I jvcl.inc}

interface

Uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, ActnList, Graphics, Classes, JvActionsEngine,
  JvVersionControlActionsEngine, JclVersionControl;

type
  TJvChangeVersionControlComponent = procedure(VersionControlComponent: TComponent) of object;
  TJvVersionControlActionCheckEnabledEvent = procedure(aDataComponent : TComponent; aDatabaseControlEngine:
      TjvVersionControlActionEngine; var aEnabled : Boolean) of object;
  TJvVersionControlActionExecuteEvent = procedure(Sender: TObject; ControlEngine: TjvVersionControlActionEngine;
    DataComponent: TComponent) of object;
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvVersionControlActionList = class(TActionList)
  private
    FValidateVersionControlDriveList: string;
    FDisableActions: Boolean;
    FHideActions: Boolean;
    FIconType: Integer;
    FOnChangeVersionControlComponent: TJvChangeVersionControlComponent;
    FVersionControlComponent: TComponent;
    FVersionControlFilename: string;
    procedure SetValidateVersionControlDriveList(const Value: string);
    procedure SetDisableActions(const Value: Boolean);
    procedure SetHideActions(const Value: Boolean);
    procedure SetIconType(const Value: Integer);
    procedure SetVersionControlFilename(const Value: string);
  protected
    procedure SetVersionControlComponent(Value: TComponent);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    //1 If this property is filled then only the drives listed in the property will be checked and disabled
    property ValidateVersionControlDriveList: string read FValidateVersionControlDriveList write SetValidateVersionControlDriveList;
    // This property defines that actions will be disabled based on the supporting
    // version control system.
    property DisableActions: Boolean read FDisableActions write SetDisableActions default true;
    // This property defines that the actions will be hidden when they are disabled
    // ATTENTION : THIS FUNCTIONALITY IS ONLY WORKING WHEN THE VersionControlFilename IS CHANGED MANUALLY
    //             AND NOT AUTOMATICLY CALCULATED VIA THE VersionControlComponent
    //             Explanation : For an hidden action the update procedures of the action are not called and so the visibiliy will not be changed.
    property HideActions: Boolean read FHideActions write SetHideActions default false;
    property IconType: Integer read FIconType write SetIconType default -1;
    property VersionControlComponent: TComponent read FVersionControlComponent write SetVersionControlComponent;
    property VersionControlFilename: string read FVersionControlFilename write SetVersionControlFilename;
    property OnChangeVersionControlComponent: TJvChangeVersionControlComponent read FOnChangeVersionControlComponent
        write FOnChangeVersionControlComponent;
  end;

type
  TJvVersionControlBaseAction = class(TJvActionEngineBaseAction)
  private
    FActionType: TJclVersionControlActionType;
    FAfterExecute: TJvVersionControlActionExecuteEvent;
    FCurrentCache: TJclVersionControlCache;
    FCurrentPlugin: TJclVersionControlPlugin;
    FDisableAction: Boolean;
    FHideAction: Boolean;
    FIconType: Integer;
    FOnChangeVersionControlComponent: TJvChangeVersionControlComponent;
    FOnCheckEnabled: TJvVersionControlActionCheckEnabledEvent;
    FOnExecute: TJvVersionControlActionExecuteEvent;
    FValidateVersionControlDriveList: string;
    FVersionControlActionEngine: TjvVersionControlActionEngine;
    FVersionControlFilename: string;
    procedure CheckVisibility;
    function GetCurrentCache: TJclVersionControlCache;
    function GetCurrentPlugin: TJclVersionControlPlugin;
    function GetCurrentVersionControlFilename: string;
    function HideActionAllowed: Boolean;
    procedure SetActionType(const Value: TJclVersionControlActionType);
    procedure SetValidateVersionControlDriveList(const Value: string);
    procedure SetVersionControlFilename(const Value: string);
    procedure ValidateCurrentCachePlugin;
  protected
    //1 This Procedure is called when the ActionComponent is changed
    procedure ChangeActionComponent(const AActionComponent: TComponent); override;
    procedure CheckEnabled(var AEnabled: Boolean); override;
    function ValidateActionForFileDrive(const AFileName: string): Boolean;
    function GetEngineList: TJvActionEngineList; override;
    function GetVersionControlComponent: TComponent;
    procedure SetVersionControlComponent(Value: TComponent);
    property ActionType: TJclVersionControlActionType read FActionType write SetActionType;
    property VersionControlActionEngine: TjvVersionControlActionEngine read FVersionControlActionEngine;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; override;
    procedure ExecuteTarget(Target: TObject); override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure SetParentComponent(AParent: TComponent); override;
    procedure UpdateTarget(Target: TObject); override;
    property CurrentCache: TJclVersionControlCache read GetCurrentCache;
    property CurrentPlugin: TJclVersionControlPlugin read GetCurrentPlugin;
    property CurrentVersionControlFilename: string read GetCurrentVersionControlFilename;
    property DisableAction: Boolean read FDisableAction write FDisableAction default true;
    property HideAction: Boolean read FHideAction write FHideAction default false;
  published
    property IconType: Integer read FIconType write FIconType default -1;
    property ValidateVersionControlDriveList: string read FValidateVersionControlDriveList write SetValidateVersionControlDriveList;
    property VersionControlComponent: TComponent read GetVersionControlComponent write SetVersionControlComponent;
    property VersionControlFilename: string read FVersionControlFilename write SetVersionControlFilename;
    property AfterExecute: TJvVersionControlActionExecuteEvent read FAfterExecute write FAfterExecute;
    property OnChangeVersionControlComponent: TJvChangeVersionControlComponent read FOnChangeVersionControlComponent
        write FOnChangeVersionControlComponent;
    property OnCheckEnabled: TJvVersionControlActionCheckEnabledEvent read FOnCheckEnabled write FOnCheckEnabled;
    property OnExecute: TJvVersionControlActionExecuteEvent read FOnExecute write FOnExecute;
  end;

  TJvVersionControlCommonAction = class(TJvVersionControlBaseAction)
  published
    property ActionType;
  end;

  TJvVersionControlAddAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlAddSandboxAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlExploreAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlDiffAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlContextMenuAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlCommitSandboxAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlCommitAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlCheckoutSandboxAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlBranchSandboxAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlBranchAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlBlameAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlGraphAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlLogAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlLogSandboxAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlExploreSandboxAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlLockAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlRenameAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlRepoBrowserAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlRevertAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlStatusAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlTagAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlUnlockAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlUpdateToAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlUpdateAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlMergeAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlPropertiesAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlLockSandboxAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlMergeSandboxAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlPropertiesSandboxAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlRenameSandboxAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlRevertSandboxAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlStatusSandboxAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlTagSandboxAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlUpdateSandboxAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlUnlockSandboxAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TJvVersionControlUpdateSandboxToAction = class(TJvVersionControlBaseAction)
  public
    constructor Create(AOwner: TComponent); override;
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
  JvJVCLUtils, SysUtils;

constructor TJvVersionControlActionList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHideActions := false;
  FDisableActions := True;
  FIconType := -1;
end;

procedure TJvVersionControlActionList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FVersionControlComponent then
      VersionControlComponent := nil;
end;

procedure TJvVersionControlActionList.SetValidateVersionControlDriveList(const Value: string);
var
  I: Integer;
begin
  FValidateVersionControlDriveList := UpperCase(trim(Value));
  for I := 0 to ActionCount - 1 do
    if Actions[I] is TJvVersionControlBaseAction then
      TJvVersionControlBaseAction(Actions[I]).FValidateVersionControlDriveList := Value;
end;

procedure TJvVersionControlActionList.SetDisableActions(const Value: Boolean);
var
  I: Integer;
begin
  FDisableActions := Value;
  for I := 0 to ActionCount - 1 do
    if Actions[I] is TJvVersionControlBaseAction then
      TJvVersionControlBaseAction(Actions[I]).DisableAction := Value;
end;

procedure TJvVersionControlActionList.SetHideActions(const Value: Boolean);
var
  I: Integer;
begin
  FHideActions := Value;
  for I := 0 to ActionCount - 1 do
    if Actions[I] is TJvVersionControlBaseAction then
      TJvVersionControlBaseAction(Actions[I]).HideAction := Value;
end;

procedure TJvVersionControlActionList.SetIconType(const Value: Integer);
var
  I: Integer;
begin
  FIconType := Value;
  for I := 0 to ActionCount - 1 do
    if Actions[I] is TJvVersionControlBaseAction then
      TJvVersionControlBaseAction(Actions[I]).IconType := Value;
end;

//=== { TJvVersionControlActionList } ==============================================

procedure TJvVersionControlActionList.SetVersionControlComponent(Value: TComponent);
var
  I: Integer;
begin
  if ReplaceComponentReference(Self, Value, TComponent(FVersionControlComponent)) then
  begin
    for I := 0 to ActionCount - 1 do
      if Actions[I] is TJvVersionControlBaseAction then
        TJvVersionControlBaseAction(Actions[I]).VersionControlComponent := Value;
    if Assigned(OnChangeVersionControlComponent) then
      OnChangeVersionControlComponent(Value);
  end;
end;

procedure TJvVersionControlActionList.SetVersionControlFilename(const Value: string);
var
  I: Integer;
begin
  if FVersionControlFilename <> Value then
  begin
    FVersionControlFilename := Value;
    for I := 0 to ActionCount - 1 do
      if Actions[I] is TJvVersionControlBaseAction then
        TJvVersionControlBaseAction(Actions[I]).VersionControlFilename:= Value;
  end;
end;

//=== { TJvVersionControlBaseAction } ==============================================

constructor TJvVersionControlBaseAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if Assigned(AOwner) and (AOwner is TJvVersionControlActionList) then
    VersionControlComponent := TJvVersionControlActionList(AOwner).VersionControlComponent;
  FVersionControlActionEngine := Nil;
  if AOwner is TJvVersionControlActionList then
  begin
    FDisableAction := TJvVersionControlActionList(AOwner).DisableActions;
    FHideAction := TJvVersionControlActionList(AOwner).HideActions;
    FIconType := TJvVersionControlActionList(AOwner).IconType;
  end
  else
  begin
    FDisableAction := true;
    FHideAction := false;
    FIconType := -1;
  end;
end;

//=== { TJvActionEngineBaseAction } ========================================

procedure TJvVersionControlBaseAction.ChangeActionComponent(const AActionComponent:
    TComponent);
begin
  inherited ChangeActionComponent(AActionComponent);
  if Assigned(ControlEngine) and (ControlEngine is TjvVersionControlActionEngine) then
    FVersionControlActionEngine := TjvVersionControlActionEngine(ControlEngine)
  else
    FVersionControlActionEngine := Nil;
end;

procedure TJvVersionControlBaseAction.CheckEnabled(var AEnabled: Boolean);
begin
//  if Assigned(fOnCheckEnabled) then
//    fOnCheckEnabled (DataSet, VersionControlComponent, VersionControlActionEngine, aEnabled);
end;

procedure TJvVersionControlBaseAction.CheckVisibility;
var  AFileCache: TJclVersionControlCache;
begin
  if ValidateActionForFileDrive(CurrentVersionControlFilename) then
  begin
    AFileCache := CurrentCache;
    if HideActionAllowed and HideAction and not VersionControlActionInfo(ActionType).AllPlugins then
      SetVisible(Assigned(AFileCache) and Assigned(AFileCache.Plugin) and (ActionType in AFileCache.Plugin.SupportedActionTypes))
    else
      SetVisible(True);
  end
  else
    SetVisible(True);
end;

function TJvVersionControlBaseAction.ValidateActionForFileDrive(const AFileName: string): Boolean;
begin
  if ValidateVersionControlDriveList = '' then
    Result := True
  else
    Result := Pos(UpperCase(Copy(AFileName,1,1)),ValidateVersionControlDriveList) > 0;
end;

function TJvVersionControlBaseAction.Execute: Boolean;
var
  Index: Integer;
  APlugin: TJclVersionControlPlugin;
  AFileName: string;
  AFileCache: TJclVersionControlCache;
begin
  Result := False;
  if VersionControlActionInfo(ActionType).Sandbox then
  begin
    AFileCache := CurrentCache;
    if not Assigned(AFileCache) or VersionControlActionInfo(ActionType).AllPlugins then
      Exit;
//    if ActOnTopSandbox then
//    begin
//      for Index := AFileCache.SandboxCount - 1 downto 0 do
//        if ActionType in AFileCache.SandboxActions[Index] then
//      begin
//        if VersionControlActionInfo(ActionType).SaveFile and Assigned (VersionControlActionEngine) and
//           VersionControlActionEngine.SupportsSaveFile(VersionControlComponent) then
//          if not VersionControlActionEngine.SupportsNeedsSaveFile(VersionControlComponent) or
//            VersionControlActionEngine.NeedsSaveFile(VersionControlComponent) then
//            VersionControlActionEngine.SaveFile(VersionControlComponent, CurrentVersionControlFilename);
//        if VersionControlActionInfo(ActionType).SaveFile then
//          SaveModules(AFileCache.SandBoxes[Index], True);
//        AFileCache.Plugin.ExecuteAction(AFileCache.SandBoxes[Index], ActionType);
//        Exit;
//      end;
//    end
//    else
    begin
      for Index := 0 to AFileCache.SandboxCount - 1 do
        if ActionType in AFileCache.SandboxActions[Index] then
      begin
        if VersionControlActionInfo(ActionType).SaveFile and Assigned (VersionControlActionEngine) and
           VersionControlActionEngine.SupportsSaveFile(VersionControlComponent) then
          if not VersionControlActionEngine.SupportsNeedsSaveFile(VersionControlComponent) or
            VersionControlActionEngine.NeedsSaveFile(VersionControlComponent) then
            VersionControlActionEngine.SaveFile(VersionControlComponent, CurrentVersionControlFilename);
        AFileCache.Plugin.ExecuteAction(AFileCache.SandBoxes[Index], ActionType);
        Exit;
      end;
    end;
  end
  else
  begin
    AFileName := CurrentVersionControlFilename;
    if VersionControlActionInfo(ActionType).SaveFile and Assigned (VersionControlActionEngine) and
       VersionControlActionEngine.SupportsSaveFile(VersionControlComponent) then
      if not VersionControlActionEngine.SupportsNeedsSaveFile(VersionControlComponent) or
         VersionControlActionEngine.NeedsSaveFile(VersionControlComponent) then
        VersionControlActionEngine.SaveFile(VersionControlComponent, CurrentVersionControlFilename);

    if VersionControlActionInfo(ActionType).AllPlugins then
    begin
      for Index := 0 to VersionControlPluginList.Count - 1 do
      begin
        AFileCache := VersionControlPluginList.GetFileCache(AFileName,
            TJclVersionControlPlugin(VersionControlPluginList.Plugins[Index]));

        if ActionType in AFileCache.Actions then
        begin
          AFileCache.Plugin.ExecuteAction(AFileName, ActionType);
          Exit;
        end;
      end;
    end
    else
    begin
      APlugin := CurrentPlugin;
      if Assigned(APlugin) then
        APlugin.ExecuteAction(AFileName, ActionType);
    end;
  end;
  if Result and Assigned(FAfterExecute) then
    FAfterExecute(Self, VersionControlActionEngine, VersionControlComponent)
end;

procedure TJvVersionControlBaseAction.ExecuteTarget(Target: TObject);
begin
  if Assigned(FOnExecute) then
    FOnExecute(Self, VersionControlActionEngine, VersionControlComponent)
  else
    inherited ExecuteTarget(Target);
end;

function TJvVersionControlBaseAction.GetCurrentCache: TJclVersionControlCache;
begin
  ValidateCurrentCachePlugin;
  Result := fCurrentCache;
end;

function TJvVersionControlBaseAction.GetCurrentPlugin: TJclVersionControlPlugin;
begin
  ValidateCurrentCachePlugin;
  Result := fCurrentPlugin;
end;

function TJvVersionControlBaseAction.GetCurrentVersionControlFilename: string;
begin
  if VersionControlFileName <> '' then
    Result := VersionControlFileName
  else
    if Assigned(VersionControlActionEngine) and VersionControlActionEngine.SupportsGetFileName (VersionControlComponent) then
      Result := VersionControlActionEngine.GetFilename (VersionControlComponent)
    else
      Result := '';
end;

function TJvVersionControlBaseAction.GetEngineList: TJvActionEngineList;
begin
  Result := RegisteredVersionControlActionEngineList;
end;

function TJvVersionControlBaseAction.GetVersionControlComponent: TComponent;
begin
  Result := ActionComponent;
end;

function TJvVersionControlBaseAction.HandlesTarget(Target: TObject): Boolean;
begin
  if VersionControlFilename <> '' then
    Result := True
  else
    Result := Inherited HandlesTarget(Target);
end;

function TJvVersionControlBaseAction.HideActionAllowed: Boolean;
begin
  Result := (VersionControlFileName <> '') or not Assigned(VersionControlComponent);
end;

procedure TJvVersionControlBaseAction.SetActionType(const Value: TJclVersionControlActionType);
begin
  FActionType := Value;
  if Caption = '' then
    Caption := LoadResString(VersionControlActionInfo(Value).Caption);
  if Name = '' then
    Name := VersionControlActionInfo(Value).ActionName;
end;

procedure TJvVersionControlBaseAction.SetParentComponent(AParent: TComponent);
begin
  Inherited SetParentComponent(AParent);
  if AParent is TJvVersionControlActionList then
  begin
    FDisableAction := TJvVersionControlActionList(AParent).DisableActions;
    FHideAction := TJvVersionControlActionList(AParent).HideActions;
    FIconType := TJvVersionControlActionList(AParent).IconType;
  end;
end;

procedure TJvVersionControlBaseAction.SetValidateVersionControlDriveList(const Value: string);
begin
  FValidateVersionControlDriveList := UpperCase(trim(Value));
end;

procedure TJvVersionControlBaseAction.SetVersionControlComponent(Value: TComponent);
begin
  ActionComponent := Value;
  CheckVisibility;
end;

procedure TJvVersionControlBaseAction.SetVersionControlFilename(const Value: string);
begin
  if Value <> FVersionControlFilename then
  begin
    FVersionControlFilename := Value;
    CheckVisibility;
  end;
end;

procedure TJvVersionControlBaseAction.UpdateTarget(Target: TObject);
var
  IndexSandbox, IndexPlugin: Integer;
  AFileCache: TJclVersionControlCache;
  AFileName: string;
begin

  CheckVisibility;

  if DisableAction and ValidateActionForFileDrive(CurrentVersionControlFilename) then
  begin
    AFileCache := CurrentCache;
    if VersionControlActionInfo(ActionType).Sandbox then
    begin
      if VersionControlActionInfo(ActionType).AllPlugins then
      begin
        AFileName := CurrentVersionControlFilename;
        for IndexPlugin := 0 to VersionControlPluginList.Count - 1 do
        begin
          AFileCache := VersionControlPluginList.GetFileCache(AFileName,
                            VersionControlPluginList.Plugins[IndexPlugin]);
          for IndexSandbox := 0 to AFileCache.SandBoxCount - 1 do
            if ActionType in AFileCache.SandBoxActions[IndexSandbox] then
          begin
            SetEnabled(True);
            Exit;
          end;
          SetEnabled(False);
          Exit;
        end;
      end
      else // work for all plugin
      begin
        if Assigned(AFileCache) then
        begin
          for IndexSandbox := 0 to AFileCache.SandBoxCount - 1 do
            if ActionType in AFileCache.SandBoxActions[IndexSandbox] then
          begin
            SetEnabled(True);
            Exit;
          end;
          SetEnabled(False);
          Exit;
        end
        else
          SetEnabled(False);
      end;
      Exit;
    end
    else // file
    begin
      if VersionControlActionInfo(ActionType).AllPlugins then
      begin
        AFileName := CurrentVersionControlFilename;
        for IndexPlugin := 0 to VersionControlPluginList.Count - 1 do
        begin
          AFileCache := VersionControlPluginList.GetFileCache(AFileName,
                            VersionControlPluginList.Plugins[IndexPlugin]);
          if ActionType in AFileCache.Actions then
          begin
            SetEnabled(True);
            Exit;
          end;
        end;
        SetEnabled(False);
        Exit;
      end
      else // only the current plugin
      begin
        AFileCache := CurrentCache;
        SetEnabled(Assigned(AFileCache) and (ActionType in AFileCache.Actions));
      end;
    end;
  end
  else
    SetEnabled(True);
end;

procedure TJvVersionControlBaseAction.ValidateCurrentCachePlugin;
var
  Index: Integer;
  AFileName: string;
  APlugin: TJclVersionControlPlugin;
  ACache: TJclVersionControlCache;
begin
  FCurrentCache := nil;
  FCurrentPlugin := nil;
  AFileName := CurrentVersionControlFilename;
  for Index := 0 to VersionControlPluginList.Count - 1 do
  begin
    APlugin := TJclVersionControlPlugin(VersionControlPluginList.Plugins[Index]);
    ACache := VersionControlPluginList.GetFileCache(AFileName, APlugin);
    if ACache.Supported then
    begin
      FCurrentCache := ACache;
      FCurrentPlugin := APlugin;
      Exit;
    end;
  end;
end;

//=== { TJvVersionControlAddAction } ==============================================

constructor TJvVersionControlAddAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaAdd;
end;

//=== { TJvVersionControlAddSandboxAction } ==============================================

constructor TJvVersionControlAddSandboxAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaAddSandbox;
end;

//=== { TJvVersionControlExploreAction } ==============================================

constructor TJvVersionControlExploreAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaExplore;
end;

//=== { TJvVersionControlDiffAction } ==============================================

constructor TJvVersionControlDiffAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaDiff;
end;

//=== { TJvVersionControlContextMenuAction } ==============================================

constructor TJvVersionControlContextMenuAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaContextMenu;
end;

//=== { TJvVersionControlCommitSandboxAction } ==============================================

constructor TJvVersionControlCommitSandboxAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaCommitSandbox;
end;

//=== { TJvVersionControlCommitAction } ==============================================

constructor TJvVersionControlCommitAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaCommit;
end;

//=== { TJvVersionControlCheckoutSandboxAction } ==============================================

constructor TJvVersionControlCheckoutSandboxAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaCheckoutSandbox;
end;

//=== { TJvVersionControlBranchSandboxAction } ==============================================

constructor TJvVersionControlBranchSandboxAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaBranchSandbox;
end;

//=== { TJvVersionControlBranchAction } ==============================================

constructor TJvVersionControlBranchAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaBranch;
end;

//=== { TJvVersionControlBlameAction } ==============================================

constructor TJvVersionControlBlameAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaBlame;
end;

//=== { TJvVersionControlGraphAction } ==============================================

constructor TJvVersionControlGraphAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaGraph;
end;

//=== { TJvVersionControlLogAction } ==============================================

constructor TJvVersionControlLogAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaLog;
end;

//=== { TJvVersionControlLogSandboxAction } ==============================================

constructor TJvVersionControlLogSandboxAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaLogSandbox;
end;

//=== { TJvVersionControlExploreSandboxAction } ==============================================

constructor TJvVersionControlExploreSandboxAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaExploreSandbox;
end;

//=== { TJvVersionControlLockAction } ==============================================

constructor TJvVersionControlLockAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaLock;
end;

//=== { TJvVersionControlRenameAction } ==============================================

constructor TJvVersionControlRenameAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaRename;
end;

//=== { TJvVersionControlRepoBrowserAction } ==============================================

constructor TJvVersionControlRepoBrowserAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaRepoBrowser;
end;

//=== { TJvVersionControlRevertAction } ==============================================

constructor TJvVersionControlRevertAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaRevert;
end;

//=== { TJvVersionControlStatusAction } ==============================================

constructor TJvVersionControlStatusAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaStatus;
end;

//=== { TJvVersionControlTagAction } ==============================================

constructor TJvVersionControlTagAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaTag;
end;

//=== { TJvVersionControlUnlockAction } ==============================================

constructor TJvVersionControlUnlockAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaUnlock;
end;

//=== { TJvVersionControlUpdateToAction } ==============================================

constructor TJvVersionControlUpdateToAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaUpdateTo;
end;

//=== { TJvVersionControlUpdateAction } ==============================================

constructor TJvVersionControlUpdateAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaUpdate;
end;

//=== { TJvVersionControlMergeAction } ==============================================

constructor TJvVersionControlMergeAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaMerge;
end;

//=== { TJvVersionControlPropertiesAction } ==============================================

constructor TJvVersionControlPropertiesAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaProperties;
end;

//=== { TJvVersionControlLockSandboxAction } ==============================================

constructor TJvVersionControlLockSandboxAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaLockSandbox;
end;

//=== { TJvVersionControlMergeSandboxAction } ==============================================

constructor TJvVersionControlMergeSandboxAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaMergeSandbox;
end;

//=== { TJvVersionControlPropertiesSandboxAction } ==============================================

constructor TJvVersionControlPropertiesSandboxAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaPropertiesSandbox;
end;

//=== { TJvVersionControlRenameSandboxAction } ==============================================

constructor TJvVersionControlRenameSandboxAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaRenameSandbox;
end;

//=== { TJvVersionControlRevertSandboxAction } ==============================================

constructor TJvVersionControlRevertSandboxAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaRevertSandbox;
end;

//=== { TJvVersionControlStatusSandboxAction } ==============================================

constructor TJvVersionControlStatusSandboxAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaStatusSandbox;
end;

//=== { TJvVersionControlTagSandboxAction } ==============================================

constructor TJvVersionControlTagSandboxAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaTagSandbox;
end;

//=== { TJvVersionControlUpdateSandboxAction } ==============================================

constructor TJvVersionControlUpdateSandboxAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaUpdateSandbox;
end;

//=== { TJvVersionControlUnlockSandboxAction } ==============================================

constructor TJvVersionControlUnlockSandboxAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaUnlockSandbox;
end;

//=== { TJvVersionControlUpdateSandboxToAction } ==============================================

constructor TJvVersionControlUpdateSandboxToAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActionType := vcaUpdateSandboxTo;
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
