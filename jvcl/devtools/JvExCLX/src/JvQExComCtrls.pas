{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExComCtrls.pas, released on 2004-01-04

The Initial Developer of the Original Code is Andreas Hausladen [Andreas dott Hausladen att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQExComCtrls;

{$I jvcl.inc}
{MACROINCLUDE JvQExControls.macros}

WARNINGHEADER

interface

uses
  Classes, SysUtils,
  QGraphics, QControls, QForms, QComCtrls, QExtCtrls,
  Qt, QWindows, QMessages,
  JvQTypes, JvQThemes, JVCLXVer, JvQExControls;

type
  JV_WINCUSTOMCONTROL(CustomHeaderControl)
  JV_WINCUSTOMCONTROL(HeaderControl)
  JV_WINCUSTOMCONTROL(CustomIconView)
  JV_WINCUSTOMCONTROL(IconView)
  JV_WINCUSTOMCONTROL(CustomViewControl)
  JV_WINCUSTOMCONTROL(CustomListView)
  JV_WINCUSTOMCONTROL(ListView)
  JV_WINCONTROL(CustomSpinEdit)
  JV_WINCONTROL(SpinEdit)
  JV_CUSTOMCONTROL(CustomTabControl)
  JV_CUSTOMCONTROL(TabControl)
  JV_WINCUSTOMCONTROL(CustomTreeView)
  JV_WINCUSTOMCONTROL(TreeView)

  JV_CUSTOMCONTROL(Animate)
  JV_CUSTOMCONTROL(PageControl)
  JV_CONTROL(ProgressBar)
  JV_CUSTOMCONTROL(StatusBar)
  JV_CUSTOMCONTROL(TabSheet)
  JV_CUSTOMCONTROL(ToolBar)
  JV_CUSTOMCONTROL(ToolButton)
  JV_WINCONTROL(TrackBar)

implementation

JV_WINCUSTOMCONTROL_IMPL(CustomHeaderControl)
JV_WINCUSTOMCONTROL_IMPL(HeaderControl)
JV_WINCUSTOMCONTROL_IMPL(CustomIconView)
JV_WINCUSTOMCONTROL_IMPL(IconView)
JV_WINCUSTOMCONTROL_IMPL(CustomViewControl)
JV_WINCUSTOMCONTROL_IMPL(CustomListView)
JV_WINCUSTOMCONTROL_IMPL(ListView)
JV_WINCONTROL_IMPL(CustomSpinEdit)
JV_WINCONTROL_IMPL(SpinEdit)
JV_CUSTOMCONTROL_IMPL(CustomTabControl)
JV_CUSTOMCONTROL_IMPL(TabControl)
JV_WINCUSTOMCONTROL_IMPL(CustomTreeView)
JV_WINCUSTOMCONTROL_IMPL(TreeView)

JV_CUSTOMCONTROL_IMPL(Animate)
JV_CUSTOMCONTROL_IMPL(PageControl)
JV_CONTROL_IMPL(ProgressBar)
JV_CUSTOMCONTROL_IMPL(StatusBar)
JV_CUSTOMCONTROL_IMPL(TabSheet)
JV_CUSTOMCONTROL_IMPL(ToolBar)
JV_CUSTOMCONTROL_IMPL(ToolButton)
JV_WINCONTROL_IMPL(TrackBar)

{$DEFINE UnitName 'JvQExComCtrls.pas'}
UNITVERSION

end.
