{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExForms.pas, released on 2004-01-04

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

unit JvExForms;
interface
uses
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Qt, QGraphics, QControls, QForms,
  {$ENDIF VisualCLX}
  Classes, SysUtils,
  JvExControls;

type
  JV_WINCONTROL_EVENTS(ScrollingWinControl)
  JV_WINCONTROL_EVENTS(ScrollBox)
  JV_WINCONTROL_EVENTS(CustomFrame)
  JV_WINCONTROL_EVENTS(Frame)
  JV_CUSTOMCONTROL_EVENTS(CustomForm) // do not implement Painting()
  JV_CUSTOMCONTROL_EVENTS(Form) // do not implement Painting()

implementation

JV_WINCONTROL_EVENTS_IMPL(ScrollingWinControl)
JV_WINCONTROL_EVENTS_IMPL(ScrollBox)
JV_WINCONTROL_EVENTS_IMPL(CustomFrame)
JV_WINCONTROL_EVENTS_IMPL(Frame)
JV_CUSTOMCONTROL_EVENTS_IMPL(CustomForm)
JV_CUSTOMCONTROL_EVENTS_IMPL(Form)

end.
