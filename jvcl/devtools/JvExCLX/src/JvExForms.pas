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

Contributor(s): André Snepvangers [asn dott att xs4all.nl] (Redesign)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvExForms;

{$I jvcl.inc}
{MACROINCLUDE JvExControls.macros}

WARNINGHEADER

interface

uses
  Windows, Messages, Graphics, Controls, Forms, ToolWin,
  Classes, SysUtils,
  JvTypes, JvThemes, JVCLVer, JvExControls;

type
  JV_WINCONTROL(ScrollingWinControl)
  JV_WINCONTROL(ScrollBox)
  JV_WINCONTROL(CustomFrame)
  JV_WINCONTROL(Frame)
  JV_CUSTOMFORM(CustomForm) // do not implement Painting() but CreateNew
  JV_CUSTOMFORM(Form) // do not implement Painting() but CreateNew
  JV_WINCONTROL(ToolWindow)

implementation

const
  UISF_HIDEFOCUS = 1;
  UISF_HIDEACCEL = 2;
  UIS_SET        = 1;
  UIS_CLEAR      = 2;
  UIS_INITIALIZE = 3;

JV_WINCONTROL_IMPL(ScrollingWinControl)
JV_WINCONTROL_IMPL(ScrollBox)
JV_WINCONTROL_IMPL(CustomFrame)
JV_WINCONTROL_IMPL(Frame)
JV_CUSTOMFORM_IMPL(CustomForm)
JV_CUSTOMFORM_IMPL(Form)
JV_WINCONTROL_IMPL(ToolWindow)

end.
