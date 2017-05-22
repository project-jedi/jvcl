{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStdEditActions.PAS, released on 2008-11-02

The Initial Developers of the Original Code are: Andreas Hausladen <Andreas dott Hausladen att gmx dott de>
Copyright (c) 2008 Andreas Hausladen
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org
-----------------------------------------------------------------------------}
// $Id$

unit JvStdEditActions;

{$I jvcl.inc}

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, ActnList, StdCtrls, Clipbrd;

type
  { The JVCL Edit standard actions automatically support a TWinControl that
    implements the IStandardEditActions interface. }
  IStandardEditActions = interface
    ['{38A87FE4-A1F4-4D47-A882-F7A3F9458264}']
    function CanUndo: Boolean;
    function CanRedo: Boolean; // not used at the moment
    function CanCut: Boolean;
    function CanCopy: Boolean;
    function CanPaste: Boolean;
    function CanSelectAll: Boolean;

    procedure Undo;
    procedure Redo; // not used at the moment
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure ClearSelection; // deletes the selected text
    procedure SelectAll;
  end;

  { Standard Editor actions }

  TJvEditAction = class(TAction)
  private
    FControl: TWinControl;
    procedure SetControl(Value: TWinControl);
  protected
    function SupportsControl(Value: TWinControl): Boolean; virtual;
    function GetEditControl(Target: TObject): TCustomEdit; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
    function HandlesTarget(Target: TObject): Boolean; override;

    property Control: TWinControl read FControl write SetControl;
  end;

  TJvEditCut = class(TJvEditAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TJvEditCopy = class(TJvEditAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TJvEditPaste = class(TJvEditAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TJvEditSelectAll = class(TJvEditAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TJvEditUndo = class(TJvEditAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TJvEditDelete = class(TJvEditAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    { UpdateTarget is required because TJvEditAction.UpdateTarget specifically
      checks to see if the action is TEditCut or TJvEditCopy }
    procedure UpdateTarget(Target: TObject); override;
  end;


implementation

uses
  JvJVCLUtils;

//=== { TJvEditAction } ==========================================================

type
  {$IFDEF COMPILER9_UP}
  TOpenCustomEdit = TCustomEdit;
  {$ELSE}
  TOpenCustomEdit = class(TCustomEdit);
  {$ENDIF COMPILER9_UP}

destructor TJvEditAction.Destroy;
begin
  if FControl <> nil then
    FControl.RemoveFreeNotification(Self);
  inherited Destroy;
end;

function TJvEditAction.GetEditControl(Target: TObject): TCustomEdit;
begin
  Result := Target as TCustomEdit;
end;

function TJvEditAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := ((Control <> nil) and (Target = Control) or
    (Control = nil) and (Target is TWinControl) and SupportsControl(TWinControl(Target))) and
    TWinControl(Target).Focused;
end;

procedure TJvEditAction.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Control) then
    Control := nil;
end;

procedure TJvEditAction.SetControl(Value: TWinControl);
begin
  if Value <> FControl then
  begin
    if not SupportsControl(Value) then
      Value := nil;
    ReplaceComponentReference(Self, Value, TComponent(FControl));
  end;
end;

function TJvEditAction.SupportsControl(Value: TWinControl): Boolean;
begin
  Result := (Value is TCustomEdit) or Supports(Value, IStandardEditActions);
end;

//=== { TJvEditCopy } ==========================================================

procedure TJvEditCopy.ExecuteTarget(Target: TObject);
var
  Intf: IStandardEditActions;
begin
  if Supports(Target, IStandardEditActions, Intf) then
    Intf.Copy
  else if Target is TCustomEdit then
    GetEditControl(Target).CopyToClipboard;
end;

procedure TJvEditCopy.UpdateTarget(Target: TObject);
var
  Intf: IStandardEditActions;
begin
  if Supports(Target, IStandardEditActions, Intf) then
    Enabled := Intf.CanCopy
  else if Target is TCustomEdit then
    Enabled := (GetEditControl(Target).SelLength > 0);
end;

//=== { TJvEditCut } ==========================================================

procedure TJvEditCut.ExecuteTarget(Target: TObject);
var
  Intf: IStandardEditActions;
begin
  if Supports(Target, IStandardEditActions, Intf) then
    Intf.Cut
  else if Target is TCustomEdit then
    GetEditControl(Target).CutToClipboard;
end;

procedure TJvEditCut.UpdateTarget(Target: TObject);
var
  Intf: IStandardEditActions;
begin
  if Supports(Target, IStandardEditActions, Intf) then
    Enabled := Intf.CanCut
  else if Target is TCustomEdit then
    Enabled := (GetEditControl(Target).SelLength > 0) and not TOpenCustomEdit(GetEditControl(Target)).ReadOnly;
end;

//=== { TJvEditPaste } ==========================================================

procedure TJvEditPaste.ExecuteTarget(Target: TObject);
var
  Intf: IStandardEditActions;
begin
  if Supports(Target, IStandardEditActions, Intf) then
    Intf.Paste
  else if Target is TCustomEdit then
    GetEditControl(Target).PasteFromClipboard;
end;

procedure TJvEditPaste.UpdateTarget(Target: TObject);
var
  Intf: IStandardEditActions;
begin
  if Supports(Target, IStandardEditActions, Intf) then
    Enabled :=  Intf.CanPaste
  else if Target is TCustomEdit then
    Enabled := Clipboard.HasFormat(CF_TEXT) and not TOpenCustomEdit(GetEditControl(Target)).ReadOnly;
end;

//=== { TJvEditSelectAll } ==========================================================

procedure TJvEditSelectAll.ExecuteTarget(Target: TObject);
var
  Intf: IStandardEditActions;
begin
  if Supports(Target, IStandardEditActions, Intf) then
    Intf.SelectAll
  else if Target is TCustomEdit then
    GetEditControl(Target).SelectAll;
end;

procedure TJvEditSelectAll.UpdateTarget(Target: TObject);
var
  Intf: IStandardEditActions;
begin
  if Supports(Target, IStandardEditActions, Intf) then
    Enabled := Intf.CanSelectAll
  else if Target is TCustomEdit then
    Enabled := Length(GetEditControl(Target).Text) > 0;
end;

//=== { TJvEditUndo } ==========================================================

procedure TJvEditUndo.ExecuteTarget(Target: TObject);
var
  Intf: IStandardEditActions;
begin
  if Supports(Target, IStandardEditActions, Intf) then
    Intf.Undo
  else if Target is TCustomEdit then
    GetEditControl(Target).Undo;
end;

procedure TJvEditUndo.UpdateTarget(Target: TObject);
var
  Intf: IStandardEditActions;
begin
  if Supports(Target, IStandardEditActions, Intf) then
    Enabled := Intf.CanUndo
  else if Target is TCustomEdit then
    Enabled := GetEditControl(Target).CanUndo and not TOpenCustomEdit(GetEditControl(Target)).ReadOnly;
end;

//=== { TJvEditDelete } ==========================================================

procedure TJvEditDelete.ExecuteTarget(Target: TObject);
var
  Intf: IStandardEditActions;
begin
  if Supports(Target, IStandardEditActions, Intf) then
    Intf.ClearSelection
  else if Target is TCustomEdit then
    GetEditControl(Target).ClearSelection;
end;

procedure TJvEditDelete.UpdateTarget(Target: TObject);
var
  Intf: IStandardEditActions;
begin
  if Supports(Target, IStandardEditActions, Intf) then
    Enabled := Intf.CanCut
  else if Target is TCustomEdit then
    Enabled := (GetEditControl(Target).SelLength > 0) and not TOpenCustomEdit(GetEditControl(Target)).ReadOnly;
end;

end.
