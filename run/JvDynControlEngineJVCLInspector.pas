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
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id: JvDynControlEngineJVCL.pas 11679 2008-01-08 01:59:25Z jfudickar $

unit JvDynControlEngineJVCLInspector;

{$I jvcl.inc}
{$I vclonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  ActnList, Graphics, ComCtrls, ImgList,
  Classes,
  Controls, JvInspector, JvDynControlEngineIntf, JvDynControlEngine;

type

  TJvDynControlRTTIInspectorControl = class(TJvInspector, IUnknown,
      IJvDynControl, IJvDynControlRTTIInspectorControl)
  private
    fOnDisplayProperty: TJvDynControlInspectorControlOnDisplayPropertyEvent;
    fOnTranslatePropertyName:
        TJvDynControlInspectorControlOnTranslatePropertyNameEvent;
    procedure JvInspectorAfterItemCreate(Sender: TObject; Item:
        TJvCustomInspectorItem);
    procedure JvInspectorBeforeItemCreate(Sender: TObject; Data:
        TJvCustomInspectorData; var ItemClass: TJvInspectorItemClass);
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
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);
    procedure ControlSetAnchors(Value: TAnchors);

    //IJvDynControlRTTIInspectorControl
    function ControlGetInspectedObject: TObject;
    function ControlGetVisibleItemsCount: Integer;
    procedure ControlSaveEditorValues;
    procedure ControlSetInspectedObject(const Value: TObject);
    function ControlIsPropertySupported(const aPropertyName : string): Boolean;
  end;

procedure RgisterJvDynControlRTTIInspectorControl (iEngine : TJvCustomDynControlEngine);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jvcl.svn.sourceforge.net:443/svnroot/jvcl/trunk/jvcl/run/JvDynControlEngineJVCL.pas $';
    Revision: '$Revision: 11679 $';
    Date: '$Date: 2008-01-08 02:59:25 +0100 (Di, 08 Jan 2008) $';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils,
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  JvDynControlEngineTools, JvDynControlEngineVCL, JvJCLUtils;


//=== { TJvDynControlRTTIInspectorControl } ========================================

procedure TJvDynControlRTTIInspectorControl.ControlSetDefaultProperties;
begin
  AfterItemCreate := JvInspectorAfterItemCreate;
  BeforeItemCreate := JvInspectorBeforeItemCreate;
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

procedure RgisterJvDynControlRTTIInspectorControl (iEngine : TJvCustomDynControlEngine);
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
