{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExExtCtrls.pas, released on 2004-01-04

The Initial Developer of the Original Code is Andreas Hausladen [Andreas dott Hausladen att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): André Snepvangers [asn dott att xs4all.nl] (Redesign)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvExExtCtrls;

{$I jvcl.inc}
{MACROINCLUDE JvExControls.macros}

WARNINGHEADER

interface

uses
  Windows, Messages, Graphics, Controls, Forms, ExtCtrls,
  Classes, SysUtils, JvTypes, JvThemes, JVCLVer, JvExControls;

type
  JV_CONTROL(Shape)
  JV_CONTROL(PaintBox)
  JV_CONTROL(Image)
  JV_CONTROL(Bevel)
  JV_WINCONTROL(CustomPanel)
  JV_WINCONTROL(CustomRadioGroup)
  JV_CONTROL(Splitter)
  JV_WINCONTROL(CustomControlBar)
  JV_WINCONTROL(ControlBar)
  JV_WINCONTROL(Panel)
  JV_WINCONTROL(RadioGroup)
  JV_WINCONTROL(Page)
  JV_WINCONTROL(Notebook)
  JV_WINCONTROL(Header)
  {$IFDEF COMPILER6_UP}
  JV_CONTROL(BoundLabel)
  JV_WINCONTROL(CustomLabeledEdit)
  JV_WINCONTROL(LabeledEdit)
  JV_WINCONTROL(CustomColorBox)
  JV_WINCONTROL(ColorBox)
  {$ENDIF COMPILER6_UP}

implementation

JV_CONTROL_IMPL(Shape)
JV_CONTROL_IMPL(PaintBox)
JV_CONTROL_IMPL(Image)
JV_CONTROL_IMPL(Bevel)
JV_WINCONTROL_IMPL(CustomPanel)
JV_WINCONTROL_IMPL(CustomRadioGroup)
JV_WINCONTROL_IMPL(CustomControlBar)
JV_WINCONTROL_IMPL(ControlBar)
JV_WINCONTROL_IMPL(Panel)
JV_WINCONTROL_IMPL(RadioGroup)
JV_WINCONTROL_IMPL(Page)
JV_WINCONTROL_IMPL(Notebook)
JV_WINCONTROL_IMPL(Header)
{$IFDEF COMPILER6_UP}
JV_CONTROL_IMPL(BoundLabel)
JV_WINCONTROL_IMPL(CustomLabeledEdit)
JV_WINCONTROL_IMPL(LabeledEdit)
JV_WINCONTROL_IMPL(CustomColorBox)
JV_WINCONTROL_IMPL(ColorBox)
{$ENDIF COMPILER6_UP}
JV_CONTROL_IMPL(Splitter)

end.
