{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExForms.pas, released on 2004-01-04

The Initial Developer of the Original Code is Andreas Hausladen [Andreas dott Hausladen att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQExForms;

{$I jvcl.inc}
{MACROINCLUDE JvQExControls.macros}

WARNINGHEADER

interface

uses
  QGraphics, QControls, QForms, QExtCtrls,
  Qt, QWindows, QMessages, QToolWin,
  Classes, SysUtils,
  JvQTypes, JvQThemes, JVCLXVer, JvQExControls;

type
  JV_CUSTOMFORM(CustomForm)
  JV_CUSTOMFORM(Form)
  JV_WINCONTROL(CustomFrame)
  JV_WINCONTROL(Frame)
  JV_WINCONTROL(ScrollBox)
  JV_WINCONTROL(ScrollingWidget)
  JV_CUSTOMCONTROL(ToolWindow)

implementation


const
  UISF_HIDEFOCUS = 1;
  UISF_HIDEACCEL = 2;
  UIS_SET        = 1;
  UIS_CLEAR      = 2;
  UIS_INITIALIZE = 3;

JV_CUSTOMFORM_IMPL(CustomForm)
JV_CUSTOMFORM_IMPL(Form)
JV_WINCONTROL_IMPL(CustomFrame)
JV_WINCONTROL_IMPL(Frame)
JV_WINCONTROL_IMPL(ScrollBox)
JV_WINCONTROL_IMPL(ScrollingWidget)
JV_CUSTOMCONTROL_IMPL(ToolWindow)

{$DEFINE UnitName 'JvQExForms.pas'}

UNITVERSION

end.
