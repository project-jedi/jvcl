{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExButtons.pas, released on 2004-01-04

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

unit JvExButtons;
interface
uses
  {$IFDEF VCL}
  Windows, Messages, Controls, Forms, Buttons, StdCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QControls, QForms, QButtons, QStdCtrls,
  {$ENDIF VisualCLX}
  Classes, SysUtils,
  JvExControls;

type
  IJvSpeedButtonEvents = interface(IJvControlEvents)
    ['{3EEBF34C-1B91-49C4-81DF-7DDF2C247698}']
    procedure ButtonPressed(Sender: TSpeedButton; GroupIndex: Integer);
  end;

  JV_CONTROL_EVENTS_BEGIN1(SpeedButton, IJvSpeedButtonEvents)
  protected
   // TJvExSpeedButton
    procedure ButtonPressed(Sender: TSpeedButton; GroupIndex: Integer); dynamic;
  JV_CONTROL_EVENTS_END

  JV_WINCONTROL_EVENTS(BitBtn)

implementation

JV_CONTROL_EVENTS_IMPLX(SpeedButton)

{$IFDEF VCL}
procedure TJvExSpeedButton.ButtonPressed(Sender: TSpeedButton; GroupIndex: Integer);
begin
  InheritMsg(Self, CM_BUTTONPRESSED, GroupIndex, Integer(Sender));
end;

procedure TJvExSpeedButton.Dispatch(var Message);
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

JV_WINCONTROL_EVENTS_IMPL(BitBtn)

end.
