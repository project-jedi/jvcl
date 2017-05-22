{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudickar att oratool dott de]
All Rights Reserved.

Contributor(s):
Jens Fudickar [jens dott fudickar att oratool dott de]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDynControlEngineJVCLInspector;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Graphics,
  Classes,
  Controls, JvInspector, JvDynControlEngineIntf, JvDynControlEngine;

type

  TJvDynControlRTTIInspectorControl = class(TJvInspector, IUnknown,
      IJvDynControl, IJvDynControlRTTIInspectorControl)
  private
    fControlOnPropertyChange: TJvDynControlInspectorControlOnPropertyChangeEvent;
    fOnDisplayProperty: TJvDynControlInspectorControlOnDisplayPropertyEvent;
    fOnTranslatePropertyName:
        TJvDynControlInspectorControlOnTranslatePropertyNameEvent;
    OldPropertyName: string;
    procedure JvInspectorAfterItemCreate(Sender: TObject; Item:
        TJvCustomInspectorItem);
    procedure JvInspectorBeforeItemCreate(Sender: TObject; Data:
        TJvCustomInspectorData; var ItemClass: TJvInspectorItemClass);
    procedure JvInspectorControlOnItemSelected(Sender: TObject);
  protected
    //IJvDynControlRTTIInspectorControl
    function ControlGetOnDisplayProperty:
        TJvDynControlInspectorControlOnDisplayPropertyEvent;
    function ControlGetOnTranslatePropertyName:
        TJvDynControlInspectorControlOnTranslatePropertyNameEvent;
    procedure ControlSetOnDisplayProperty(const Value:
        TJvDynControlInspectorControlOnDisplayPropertyEvent); overload;
    procedure ControlSetOnTranslatePropertyName(const Value:
        TJvDynControlInspectorControlOnTranslatePropertyNameEvent);
    function GetControlDividerWidth: Integer;
    procedure SetControlDividerWidth(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);
    procedure ControlSetAnchors(Value: TAnchors);

    //IJvDynControlRTTIInspectorControl
    function ControlGetCurrentPropertyName: string;
    function ControlGetInspectedObject: TObject;
    function ControlGetVisibleItemsCount: Integer;
    procedure ControlSaveEditorValues;
    procedure ControlSetInspectedObject(const Value: TObject);
    function ControlIsPropertySupported(const aPropertyName : string): Boolean;
    function GetControlOnPropertyChange:
        TJvDynControlInspectorControlOnPropertyChangeEvent;
    procedure SetControlOnPropertyChange(const Value:
        TJvDynControlInspectorControlOnPropertyChangeEvent);
  end;

procedure RegisterJvDynControlRTTIInspectorControl(iEngine :
    TJvCustomDynControlEngine);

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
  SysUtils, Variants,
  JvJCLUtils;


//=== { TJvDynControlRTTIInspectorControl } ========================================

constructor TJvDynControlRTTIInspectorControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OldPropertyName := '';
end;

function TJvDynControlRTTIInspectorControl.ControlGetCurrentPropertyName:
    string;
begin
  if Assigned (Selected) then
    Result := Selected.Name
  else
    Result := '';
end;

procedure TJvDynControlRTTIInspectorControl.ControlSetDefaultProperties;
begin
  AfterItemCreate := JvInspectorAfterItemCreate;
  BeforeItemCreate := JvInspectorBeforeItemCreate;
  OnItemSelected := JvInspectorControlOnItemSelected;
end;

procedure TJvDynControlRTTIInspectorControl.ControlSetCaption(const Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlRTTIInspectorControl.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlRTTIInspectorControl.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlRTTIInspectorControl.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlRTTIInspectorControl.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlRTTIInspectorControl.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlRTTIInspectorControl.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

function TJvDynControlRTTIInspectorControl.ControlGetInspectedObject: TObject;
begin
  Result := InspectObject;
end;

function TJvDynControlRTTIInspectorControl.ControlGetOnDisplayProperty:
    TJvDynControlInspectorControlOnDisplayPropertyEvent;
begin
  Result := fOnDisplayProperty;
end;

function TJvDynControlRTTIInspectorControl.ControlGetOnTranslatePropertyName:
    TJvDynControlInspectorControlOnTranslatePropertyNameEvent;
begin
  Result := fOnTranslatePropertyName;
end;

function TJvDynControlRTTIInspectorControl.ControlGetVisibleItemsCount: Integer;
begin
  Result := VisibleCount;
end;

function TJvDynControlRTTIInspectorControl.ControlIsPropertySupported(const
    aPropertyName : string): Boolean;
begin
  Result := True;
end;

procedure TJvDynControlRTTIInspectorControl.JvInspectorAfterItemCreate(Sender:
    TObject; Item: TJvCustomInspectorItem);
begin
  if Assigned(Item) and Assigned(fOnTranslatePropertyName) then
    Item.DisplayName := fOnTranslatePropertyName(Item.Name);
end;

procedure TJvDynControlRTTIInspectorControl.JvInspectorBeforeItemCreate(Sender:
    TObject; Data: TJvCustomInspectorData; var ItemClass:
    TJvInspectorItemClass);
begin
  if Assigned(fOnDisplayProperty)and
     Assigned(Data) and
     (Data is TJvInspectorPropData) and
     Assigned(TJvInspectorPropData(Data).Instance)  then
  begin
    if not fOnDisplayProperty(Data.Name) then
      ItemClass := nil;
  end;
end;

procedure TJvDynControlRTTIInspectorControl.ControlSaveEditorValues;
begin
  SaveValues;
end;

procedure TJvDynControlRTTIInspectorControl.ControlSetInspectedObject(const
    Value: TObject);
begin
  InspectObject := Value;
end;

procedure TJvDynControlRTTIInspectorControl.ControlSetOnDisplayProperty(const
    Value: TJvDynControlInspectorControlOnDisplayPropertyEvent);
begin
  fOnDisplayProperty := Value;
end;

procedure TJvDynControlRTTIInspectorControl.ControlSetOnTranslatePropertyName(
    const Value: TJvDynControlInspectorControlOnTranslatePropertyNameEvent);
begin
  fOnTranslatePropertyName := Value;
end;

function TJvDynControlRTTIInspectorControl.GetControlDividerWidth: Integer;
begin
  Result := Divider;
end;

function TJvDynControlRTTIInspectorControl.GetControlOnPropertyChange:
    TJvDynControlInspectorControlOnPropertyChangeEvent;
begin
  Result := fControlOnPropertyChange;
end;

procedure TJvDynControlRTTIInspectorControl.JvInspectorControlOnItemSelected(
    Sender: TObject);
var
  NewPropertyName: string;
begin
  NewPropertyName := ControlGetCurrentPropertyName;
  if Assigned(fControlOnPropertyChange) then
    fControlOnPropertyChange(OldPropertyName, NewPropertyName);
  OldPropertyName := NewPropertyName;
end;

procedure TJvDynControlRTTIInspectorControl.SetControlDividerWidth(const Value:
    Integer);
begin
  Divider := Value;
end;

procedure TJvDynControlRTTIInspectorControl.SetControlOnPropertyChange(const
    Value: TJvDynControlInspectorControlOnPropertyChangeEvent);
begin
  fControlOnPropertyChange := Value;
end;

procedure RegisterJvDynControlRTTIInspectorControl(iEngine :
    TJvCustomDynControlEngine);
begin
  iEngine.RegisterControlType(jctRTTIInspector, TJvDynControlRTTIInspectorControl);
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