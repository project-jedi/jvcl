{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExDBCtrls.pas, released on 2004-01-04

The Initial Developer of the Original Code is Andreas Hausladen [Andreas.Hausladen@gmx.de]
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

Last Modified: 2004-01-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I jvcl.inc}

unit JvExDBCtrls;
interface
uses
  {$IFDEF VCL}
  Windows, Messages, Controls, Forms, Buttons, DBCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QControls, QForms, QButtons, QDBCtrls,
  {$ENDIF VisualCLX}
  Classes, SysUtils,
  JvExControls, JvExButtons;

type
  JV_WINCONTROL_EVENTS(DBEdit)
  JV_CONTROL_EVENTS(DBText)
  JV_WINCONTROL_EVENTS(DBCheckBox)
  JV_WINCONTROL_EVENTS(DBComboBox)
  JV_WINCONTROL_EVENTS(DBListBox)
  JV_WINCONTROL_EVENTS(DBRadioGroup)
  JV_WINCONTROL_EVENTS(DBMemo)
  JV_WINCONTROL_EVENTS(DBImage)
  JV_WINCONTROL_EVENTS(DBNavigator)
  JV_WINCONTROL_EVENTS(DBLookupControl)
  JV_WINCONTROL_EVENTS(DBLookupListBox)
  JV_WINCONTROL_EVENTS(PopupDataList)
  JV_WINCONTROL_EVENTS(DBLookupComboBox)
{$IFDEF VCL}
  JV_WINCONTROL_EVENTS(DBRichEdit)
{$ENDIF VCL}
  JV_CONTROL_EVENTS_BEGIN1(NavButton, IJvSpeedButtonEvents)
  protected
   // TJvExSpeedButton
    procedure ButtonPressed(Sender: TSpeedButton; GroupIndex: Integer); dynamic;
  JV_CONTROL_EVENTS_END

implementation

JV_WINCONTROL_EVENTS_IMPL(DBEdit)
JV_CONTROL_EVENTS_IMPL(DBText)
JV_WINCONTROL_EVENTS_IMPL(DBCheckBox)
JV_WINCONTROL_EVENTS_IMPL(DBComboBox)
JV_WINCONTROL_EVENTS_IMPL(DBListBox)
JV_WINCONTROL_EVENTS_IMPL(DBRadioGroup)
JV_WINCONTROL_EVENTS_IMPL(DBMemo)
JV_WINCONTROL_EVENTS_IMPL(DBImage)
JV_WINCONTROL_EVENTS_IMPL(DBNavigator)
JV_WINCONTROL_EVENTS_IMPL(DBLookupControl)
JV_WINCONTROL_EVENTS_IMPL(DBLookupListBox)
JV_WINCONTROL_EVENTS_IMPL(PopupDataList)
JV_WINCONTROL_EVENTS_IMPL(DBLookupComboBox)
JV_WINCONTROL_EVENTS_IMPL(DBRichEdit)

JV_CONTROL_EVENTS_IMPLX(NavButton)

{$IFDEF VCL}

procedure TJvExNavButton.ButtonPressed(Sender: TSpeedButton;
  GroupIndex: Integer);
begin
  InheritMsg(Self, CM_BUTTONPRESSED, GroupIndex, Integer(Sender));
end;

procedure TJvExNavButton.Dispatch(var Message);
begin
  if not DispatchMsg(Self, Message) then
    case TMessage(Message).Msg of
      CM_BUTTONPRESSED:
        with TMessage(Message) do
          ButtonPressed(TSpeedButton(LParam), WParam);
    else
      inherited Dispatch(Message);
    end;
end;

{$ENDIF VCL}

end.
