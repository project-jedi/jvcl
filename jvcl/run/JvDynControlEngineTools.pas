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

unit JvDynControlEngineTools;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Variants, Classes, Controls, StdCtrls, Forms,
  JvDynControlEngine;

function CreateDynControlDialog(const AFormCaption, AButton1Caption, AButton2Caption: string;
  const AButton1Click, AButton2Click: TNotifyEvent;
  var AMainPanel: TWinControl;
  ADynControlEngine: TJvDynControlEngine = nil): TCustomForm;

function JvDynControlVariantToBoolean(Value: Variant): Boolean;

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
  SysUtils;

function CreateDynControlDialog(const AFormCaption, AButton1Caption, AButton2Caption: string;
  const AButton1Click, AButton2Click: TNotifyEvent;
  var AMainPanel: TWinControl;
  ADynControlEngine: TJvDynControlEngine = nil): TCustomForm;
var
  DynControlEngine: TJvDynControlEngine;
  ButtonPanel: TWinControl;
  Form: TCustomForm;
  Button1, Button2: TButtonControl;
begin
  if Assigned(ADynControlEngine) then
    DynControlEngine := ADynControlEngine
  else
    DynControlEngine := DefaultDynControlEngine;
  Form := DynControlEngine.CreateForm(AFormCaption, '');
  TForm(Form).FormStyle := fsNormal;
  {$IFDEF COMPILER7_UP}
  TForm(Form).Position := poOwnerFormCenter;
  {$ELSE}
  TForm(Form).Position := poScreenCenter;
  {$ENDIF COMPILER7_UP};  
  TForm(Form).BorderIcons := [];
  TForm(Form).BorderStyle := bsDialog;

  ButtonPanel := DynControlEngine.CreatePanelControl(Form, Form, '', '', alBottom);
  AMainPanel := DynControlEngine.CreatePanelControl(Form, Form, '', '', alClient);
  if AButton1Caption <> '' then
  begin
    Button1 := DynControlEngine.CreateButton(Form, ButtonPanel, '', AButton1Caption, '', AButton1Click, True, False);
    ButtonPanel.Height := Button1.Height + 6;
    Button1.Top := 3;
    Button1.Anchors := [akTop, akRight];
  end
  else
    Button1 := nil;
  if AButton2Caption <> '' then
  begin
    Button2 := DynControlEngine.CreateButton(Form, ButtonPanel, '', AButton2Caption, '', AButton2Click, True, False);
    ButtonPanel.Height := Button2.Height + 6;
    Button2.Top := 3;
    Button2.Anchors := [akTop, akRight];
    Button2.Left := ButtonPanel.Width - Button2.Width - 5;
    if Assigned(Button1) then
      Button1.Left := Button2.Left - Button1.Width - 5;
  end
  else
    if Assigned(Button1) then
      Button1.Left := ButtonPanel.Width - Button1.Width - 5;
  Result := Form;
end;

function JvDynControlVariantToBoolean(Value: Variant): Boolean;
begin
  if VarIsNull(Value) then
    Result := False
  else if VarType(Value) = varBoolean then
    Result := Value
  else
    Result := UpperCase(VarToStr(Value)) = 'TRUE';
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
