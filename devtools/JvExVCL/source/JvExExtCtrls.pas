{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExExtCtrls.pas, released on 2004-01-04

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

unit JvExExtCtrls;
interface
uses
  {$IFDEF VCL}
  Windows, Messages, Controls, Forms, ExtCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QControls, QForms, QExtCtrls,
  {$ENDIF VisualCLX}
  Classes, SysUtils,
  JvExControls;

type
  JV_CONTROL_EVENTS(Shape)
  JV_CONTROL_EVENTS(PaintBox)
  JV_CONTROL_EVENTS(Image)
  JV_CONTROL_EVENTS(Bevel)
  JV_WINCONTROL_EVENTS(CustomPanel)
  JV_WINCONTROL_EVENTS(Panel)
  JV_WINCONTROL_EVENTS(CustomRadioGroup)
  JV_WINCONTROL_EVENTS(RadioGroup)
  JV_CONTROL_EVENTS(Splitter)

{$IFDEF VCL}
  JV_WINCONTROL_EVENTS(CustomControlBar)
  JV_WINCONTROL_EVENTS(ControlBar)
  JV_WINCONTROL_EVENTS(Page)
  JV_WINCONTROL_EVENTS(Notebook)
  JV_WINCONTROL_EVENTS(Header)
  {$IFDEF COMPILER6_UP}
  JV_CONTROL_EVENTS(BoundLabel)
  JV_WINCONTROL_EVENTS(CustomLabeledEdit)
  JV_WINCONTROL_EVENTS(LabeledEdit)
  JV_WINCONTROL_EVENTS(CustomColorBox)
  JV_WINCONTROL_EVENTS(ColorBox)
  {$ENDIF COMPILER6_UP}
{$ENDIF VCL}

implementation

JV_CONTROL_EVENTS_IMPL(Shape)
JV_CONTROL_EVENTS_IMPL(PaintBox)
JV_CONTROL_EVENTS_IMPL(Image)
JV_CONTROL_EVENTS_IMPL(Bevel)
JV_WINCONTROL_EVENTS_IMPL(CustomPanel)
JV_WINCONTROL_EVENTS_IMPL(Panel)
JV_WINCONTROL_EVENTS_IMPL(CustomRadioGroup)
JV_WINCONTROL_EVENTS_IMPL(RadioGroup)
JV_CONTROL_EVENTS_IMPL(Splitter)

{$IFDEF VCL}
JV_WINCONTROL_EVENTS_IMPL(CustomControlBar)
JV_WINCONTROL_EVENTS_IMPL(ControlBar)
JV_WINCONTROL_EVENTS_IMPL(Page)
JV_WINCONTROL_EVENTS_IMPL(Notebook)
JV_WINCONTROL_EVENTS_IMPL(Header)
 {$IFDEF COMPILER6_UP}
JV_CONTROL_EVENTS_IMPL(BoundLabel)
JV_WINCONTROL_EVENTS_IMPL(CustomLabeledEdit)
JV_WINCONTROL_EVENTS_IMPL(LabeledEdit)
JV_WINCONTROL_EVENTS_IMPL(CustomColorBox)
JV_WINCONTROL_EVENTS_IMPL(ColorBox)
 {$ENDIF COMPILER6_UP}
{$ENDIF VCL}

end.
