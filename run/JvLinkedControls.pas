{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvLinkedControls.PAS, released on 2004-01-26.

The Initial Developer of the Original Code is Peter Thörnqvist
Portions created by Peter Thörnqvist are Copyright (C) 2004 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2004-01-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvLinkedControls;

interface
uses
  SysUtils, Classes, Controls;

type
  TJvLinkedControlsOption = (loLinkChecked, loLinkEnabled);
  TJvLinkedControlsOptions = set of TJvLinkedControlsOption;
  TJvLinkedControl = class(TCollectionItem)
  private
    FOwnerControl, FControl:TControl;
    FOptions: TJvLinkedControlsOptions;
    FOriginalEnabled:boolean;
    procedure SetControl(const Value: TControl);
    procedure SetOptions(const Value: TJvLinkedControlsOptions);
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(Source:TPersistent);override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy;override;
  published
    property Control:TControl read FControl write SetControl;
    property Options:TJvLinkedControlsOptions read FOptions write SetOptions default [loLinkChecked, loLinkEnabled];
  end;

  TJvLinkedControls = class(TOwnedCollection)
  private
    FControl:TControl;
    FOnChange: TNotifyEvent;
    FRestoreEnabled: boolean;
    function GetItems(Index: integer): TJvLinkedControl;
    procedure SetItems(Index: integer; const Value: TJvLinkedControl);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    procedure Notification(AComponent:TComponent;Operation:TOperation);virtual;
    constructor Create(AControl:TControl);
    function Add:TJvLinkedControl;
    procedure Assign(Source:TPersistent);override;
    // If RestoreEnabled is true, TJvLinkedControls will try to restore the Enabled state
    // of linked controls when an item is changed or removed
    property Items[Index:integer]:TJvLinkedControl read GetItems write SetItems;default;
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  published
    property RestoreEnabled:boolean read FRestoreEnabled write FRestoreEnabled default true;
  end;

implementation

{ TJvLinkedControl }

procedure TJvLinkedControl.Assign(Source: TPersistent);
begin
  if (Source <> Self) and (Source is TJvLinkedControl) then
  begin
    Control := TJvLinkedControl(Source).Control;
    Options := TJvLinkedControl(Source).Options;
    Changed(false);
    Exit;
  end;
  inherited;
end;

constructor TJvLinkedControl.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  if (Collection is TJvLinkedControls) then
    FOwnerControl := TJvLinkedControls(Collection).FControl;
  FOptions := [loLinkChecked, loLinkEnabled];
end;

destructor TJvLinkedControl.Destroy;
begin
  if (FControl <> nil) and not (csDestroying in FControl.ComponentState) and
    (Collection is TJvLinkedControls) and TJvLinkedControls(Collection).RestoreEnabled then
       FControl.Enabled := FOriginalEnabled;
  inherited;
end;

function TJvLinkedControl.GetDisplayName: string;
begin
  if Control <> nil then
    Result := Control.Name
  else
    Result := inherited GetDisplayName;
end;

procedure TJvLinkedControl.SetControl(const Value: TControl);
begin
  if (FControl <> Value) then
  begin
    if (FOwnerControl = nil) and (Collection is TJvLinkedControls) then
      FOwnerControl := TJvLinkedControls(Collection).FControl;
    if (Value = FOwnerControl) and (FOwnerControl <> nil) then
      raise Exception.Create('Cannot link to owner control');
    if Assigned(FControl) then
    begin
      if Assigned(FOwnerControl) then
        FControl.RemoveFreeNotification(FOwnerControl);
      if (Collection is TJvLinkedControls) and TJvLinkedControls(Collection).RestoreEnabled then
        FControl.Enabled := FOriginalEnabled;
    end;
    if (FOwnerControl <> nil) and (csDestroying in FOwnerControl.ComponentState) then
      FControl := nil
    else
      FControl := Value;
    if Assigned(FControl) then
    begin
      FOriginalEnabled := FControl.Enabled;
      if Assigned(FOwnerControl) then
        FControl.FreeNotification(FOwnerControl);
    end;
    Changed(false);
  end;
end;

procedure TJvLinkedControl.SetOptions(const Value: TJvLinkedControlsOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    Changed(false);
  end;
end;

{ TJvLinkedControls }

function TJvLinkedControls.Add: TJvLinkedControl;
begin
  Result := TJvLinkedControl(inherited Add);
  Result.FOwnerControl := FControl;
end;

procedure TJvLinkedControls.Assign(Source: TPersistent);
var i:integer;
begin
  if (Source <> Self) and (Source is TJvLinkedControls) then
  begin
    BeginUpdate;
    try
      Clear;
      for i := 0 to TJvLinkedControls(Source).Count - 1 do
        Add.Assign(TJvLinkedControls(Source)[i]);
      RestoreEnabled := TJvLinkedControls(Source).RestoreEnabled;
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited;
end;

constructor TJvLinkedControls.Create(AControl: TControl);
begin
  inherited Create(AControl, TJvLinkedControl);
  FControl := AControl;
  FRestoreEnabled := true;
end;

function TJvLinkedControls.GetItems(Index: integer): TJvLinkedControl;
begin
  Result := TJvLinkedControl(inherited Items[Index]);
end;

procedure TJvLinkedControls.Notification(AComponent: TComponent; Operation: TOperation);
var i:integer;
begin
  if Assigned(FControl) and (csDestroying in FControl.ComponentState) then
    Exit;
  BeginUpdate;
  try
    if (AComponent is TControl) and (Operation = opRemove) then
      for i := 0 to Count - 1 do
        if Items[i].Control = AComponent then
          Items[i].Control := nil;
  finally
    EndUpdate;
  end;
end;

procedure TJvLinkedControls.SetItems(Index: integer;
  const Value: TJvLinkedControl);
begin
  inherited Items[Index] := Value;
end;

procedure TJvLinkedControls.Update(Item: TCollectionItem);
begin
  inherited;
  if Item <> nil then
    TJvLinkedControl(Item).FOwnerControl := FControl;
  if Assigned(FOnChange) then FOnChange(Self);
end;

end.

