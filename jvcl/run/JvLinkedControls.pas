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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvLinkedControls;

interface
uses
  SysUtils,
  {$IFDEF VCL}
  Controls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QControls,
  {$ENDIF VisualCLX}
  Classes;

type
  TJvLinkedControlsOption = (loLinkChecked, loLinkEnabled, loInvertChecked, loInvertEnabled);
  TJvLinkedControlsOptions = set of TJvLinkedControlsOption;

  TJvLinkedControl = class(TCollectionItem)
  private
    FOwnerControl, FControl: TControl;
    FOptions: TJvLinkedControlsOptions;
    FOriginalEnabled: Boolean;
    procedure SetControl(const Value: TControl);
    procedure SetOptions(const Value: TJvLinkedControlsOptions);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Control: TControl read FControl write SetControl;
    property Options: TJvLinkedControlsOptions read FOptions write SetOptions default [loLinkChecked, loLinkEnabled];
  end;

  TJvLinkedControls = class(TOwnedCollection)
  private
    FControl: TControl;
    FOnChange: TNotifyEvent;
    FRestoreEnabled: Boolean;
    function GetItems(Index: Integer): TJvLinkedControl;
    procedure SetItems(Index: Integer; const Value: TJvLinkedControl);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    // You must call Notification in the Owning controls overridden Notification
    // or hell will break loose when linked controls are removed!!!
    procedure Notification(AComponent: TComponent; Operation: TOperation); virtual;
    constructor Create(AControl: TControl);
    function Add: TJvLinkedControl;
    procedure Assign(Source: TPersistent); override;
    // If RestoreEnabled is True, TJvLinkedControls will try to restore the Enabled state
    // of linked controls when an item is changed or removed
    property Items[Index: Integer]: TJvLinkedControl read GetItems write SetItems; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property RestoreEnabled: Boolean read FRestoreEnabled write FRestoreEnabled default True;
  end;

function CheckLinkControlEnabled(Enabled, Checked: Boolean; Options: TJvLinkedControlsOptions): Boolean;

implementation

uses
  JvResources;

function CheckLinkControlEnabled(Enabled, Checked: Boolean; Options: TJvLinkedControlsOptions): Boolean;
var
  IsChecked, IsEnabled: Boolean;
begin
  if loInvertChecked in Options then
    IsChecked := not Checked
  else
    IsChecked := Checked;

  if loInvertEnabled in Options then
    IsEnabled := not Enabled
  else
    IsEnabled := Enabled;

  if (loLinkChecked in Options) and (loLinkEnabled in Options) then
    Result := IsChecked and IsEnabled
  else
    Result := ((loLinkChecked in Options) and IsChecked) or ((loLinkEnabled in Options) and IsEnabled);

  //  Result := ((loLinkChecked in Options) and ((not Checked and (loInvertChecked in Options) or (Checked and not (loInvertChecked in Options))))) or
  //            ((loLinkEnabled in Options) and (not Enabled and (loInvertEnabled in Options)) or (Enabled and not (loInvertEnabled in Options)));
end;

//=== { TJvLinkedControl } ===================================================

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
  inherited Destroy;
end;

procedure TJvLinkedControl.Assign(Source: TPersistent);
begin
  if Source is TJvLinkedControl then
  begin
    if Source <> Self then
    begin
      Control := TJvLinkedControl(Source).Control;
      Options := TJvLinkedControl(Source).Options;
      Changed(False);
    end;
  end
  else
    inherited Assign(Source);
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
  if FControl <> Value then
  begin
    if (FOwnerControl = nil) and (Collection is TJvLinkedControls) then
      FOwnerControl := TJvLinkedControls(Collection).FControl;
    if (Value = FOwnerControl) and (FOwnerControl <> nil) then
      raise Exception.CreateRes(@RsEOwnerLinkError);
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
    Changed(False);
  end;
end;

procedure TJvLinkedControl.SetOptions(const Value: TJvLinkedControlsOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    Changed(False);
  end;
end;

//=== { TJvLinkedControls } ==================================================

constructor TJvLinkedControls.Create(AControl: TControl);
begin
  inherited Create(AControl, TJvLinkedControl);
  FControl := AControl;
  FRestoreEnabled := True;
end;

function TJvLinkedControls.Add: TJvLinkedControl;
begin
  Result := TJvLinkedControl(inherited Add);
  Result.FOwnerControl := FControl;
end;

procedure TJvLinkedControls.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TJvLinkedControls then
  begin
    if Source <> Self then
    begin
      BeginUpdate;
      try
        Clear;
        for I := 0 to TJvLinkedControls(Source).Count - 1 do
          Add.Assign(TJvLinkedControls(Source)[I]);
        RestoreEnabled := TJvLinkedControls(Source).RestoreEnabled;
      finally
        EndUpdate;
      end;
    end;
  end
  else
    inherited Assign(Source);
end;

function TJvLinkedControls.GetItems(Index: Integer): TJvLinkedControl;
begin
  Result := TJvLinkedControl(inherited Items[Index]);
end;

procedure TJvLinkedControls.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  // make sure the owning control isn't being destroyed
  if Assigned(FControl) and (csDestroying in FControl.ComponentState) then
    Exit;
  BeginUpdate;
  try
    if (AComponent is TControl) and (Operation = opRemove) then
      for I := 0 to Count - 1 do
        if Items[I].Control = AComponent then
          Items[I].Control := nil;
  finally
    EndUpdate;
  end;
end;

procedure TJvLinkedControls.SetItems(Index: Integer;
  const Value: TJvLinkedControl);
begin
  inherited Items[Index] := Value;
end;

procedure TJvLinkedControls.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Item <> nil then
    TJvLinkedControl(Item).FOwnerControl := FControl;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.

