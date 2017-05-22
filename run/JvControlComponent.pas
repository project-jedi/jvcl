{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http:{www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvControlComponent.pas, released on 2004-09-21

The Initial Developer of the Original Code is Andr� Snepvangers [ASnepvangers att users.sourceforge.net]
Portions created by Andr� Snepvangers are Copyright (C) 2004 Andr� Snepvangers.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http:{jvcl.sourceforge.net

Known Issues:
  It is still possible to move the component in IDE outside the parent.
  It could also be called as a feature. Object Treeview shows the
  correct parent.
-----------------------------------------------------------------------------}
// $Id$

unit JvControlComponent;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Windows, Messages, Controls, Forms,
  JvComponentBase;

type
  TJvCustomControlComponent = class(TJvComponent)
  private
    FActive: Boolean;
    FParent: TWinControl;
    function GetDesignInfo: Longint;
    procedure SetDesignInfo(Value: Longint);
  protected
    procedure SetParent(const Value: TWinControl); virtual;
    function GetParent: TWinControl; virtual;
    procedure Loaded; override;
    procedure SetParentComponent(Value: TComponent); override;
    property Active: Boolean read FActive write FActive;
  public
    function GetParentComponent: TComponent; override;
    property DesignInfo: Longint read GetDesignInfo write SetDesignInfo;
    property Parent: TWinControl read GetParent write SetParent;
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

procedure TJvCustomControlComponent.SetParentComponent(Value: TComponent);
begin
  SetParent(TWinControl(Value));
end;

procedure TJvCustomControlComponent.SetParent(const Value: TWinControl);
begin
  if Value <> Parent then
    FParent := Value;
end;

function TJvCustomControlComponent.GetParentComponent: TComponent;
begin
  Result := GetParent;
end;

function TJvCustomControlComponent.GetParent: TWinControl;
begin
  Result := FParent;
end;

procedure TJvCustomControlComponent.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) and Assigned(Parent) and (Parent <> Owner) then
  begin
    { Changing the owner to the parent, has the advantage that
      it gets destroyed by the Parent. At the right time thus.
      To access a component with the Object Inspector
      Owner property should remain unchanged.
    }
    if Assigned(Owner) then
      Owner.RemoveComponent(Self);
    Parent.InsertComponent(Self); { owner := parent }
  end;
end;

function TJvCustomControlComponent.GetDesignInfo: Longint;
begin
  Result := inherited DesignInfo;
end;

procedure TJvCustomControlComponent.SetDesignInfo(Value: Longint);
var
  Pos: TPoint;
  FControl: TControl;
begin
  if (csDesigning in ComponentState) and (Owner is TWinControl) then
  begin
    Pos.X := TSmallPoint(Value).X; { left }
    Pos.Y := TSmallPoint(Value).Y; { top }
    if not Assigned(Parent) or (Parent = Owner) then { find the TWinControl where it is dropped }
    begin
      FControl := TWinControl(Owner).ControlAtPos(Pos, True, True);
      if not Assigned(FControl) then
        Parent := TWinControl(Owner)
      else
      if FControl is TControl then
        Parent := FControl.Parent
      else
        Parent := TWinControl(Parent);
    end;
  end;
  inherited DesignInfo := Value;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
