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
// $Id$

unit JvDynControlEngineTools;

interface

Uses Classes, Controls, StdCtrls, Forms, ExtCtrls,
     JvDynControlEngine;

function CreateDynControlDialog  (const AFormCaption,
                                        AButton1Caption,
                                        AButton2Caption: string;
                                  const AButton1Click,
                                        AButton2Click : TNotifyEvent;
                                  var aMainPanel: TWinControl;
                                  aDynControlEngine : TJvDynControlEngine = NIL): TCustomForm;

implementation

function CreateDynControlDialog  (const AFormCaption,
                                        AButton1Caption,
                                        AButton2Caption: string;
                                  const AButton1Click,
                                        AButton2Click : TNotifyEvent;
                                  var aMainPanel: TWinControl;
                                  aDynControlEngine : TJvDynControlEngine = NIL): TCustomForm;
var DynControlEngine : TJvDynControlEngine;
    ButtonPanel      : TWinControl;
    Form             : TCustomForm;
    Button1,
    Button2: tButtonControl;
begin
  if Assigned(aDynControlEngine) then
    DynControlEngine := aDynControlEngine
  else
    DynControlEngine := DefaultDynControlEngine;
  Form := DynControlEngine.CreateForm (aFormCaption, '');
  With TForm(Form) DO
  begin
    FormStyle := fsNormal;
    Position := poScreenCenter;
    BorderIcons := [];
    BorderStyle := bsDialog;
  end;

  ButtonPanel := DynControlEngine.CreatePanelControl (Form, Form, '','', alBottom);
  aMainPanel := DynControlEngine.CreatePanelControl (Form, Form, '','', alClient);
  if aButton1Caption <> '' then
  begin
    Button1 := DynControlEngine.CreateButton(Form, ButtonPanel,'', aButton1Caption, '', aButton1Click, True, False);
    ButtonPanel.Height := Button1.Height + 6;
    Button1.Top := 3;
    Button1.Anchors := [akTop,akRight];
  end;
  if aButton2Caption <> '' then
  begin
    Button2 := DynControlEngine.CreateButton(Form, ButtonPanel,'', aButton2Caption, '', aButton2Click, True, False);
    ButtonPanel.Height := Button2.Height + 6;
    Button2.Top := 3;
    Button2.Anchors := [akTop,akRight];
    Button2.Left := ButtonPanel.Width-Button2.Width-5;
    Button1.Left := Button2.Left-Button1.Width-5;
  end
  else
    Button1.Left := ButtonPanel.Width-Button1.Width-5;
  Result := Form;
end;

end.
