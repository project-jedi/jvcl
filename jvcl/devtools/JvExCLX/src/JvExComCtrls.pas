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

Contributor(s): André Snepvangers [asn dott att xs4all.nl] (Redesign)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvExComCtrls;

{$I jvcl.inc}

{MACROINCLUDE JvExControls.macros}

WARNINGHEADER

interface

uses
  Windows, Messages, Graphics, Controls, Forms, ComCtrls,
  Classes, SysUtils,
  JvTypes, JvThemes, JVCLVer, JvExControls;

type
  {$IFDEF COMPILER6_UP}
  JV_WINCONTROL(CustomHeaderControl)
  {$ENDIF COMPILER6_UP}
  JV_WINCONTROL(HeaderControl)
  JV_WINCONTROL(CustomTreeView)
  JV_WINCONTROL(TreeView)
  JV_WINCONTROL(CustomListView)
  JV_WINCONTROL(ListView)
  JV_WINCONTROL(PageControl)
  JV_WINCONTROL(CustomTabControl)
  JV_WINCONTROL(TabControl)
  JV_WINCONTROL(TrackBar)
  JV_WINCONTROL(Animate)

  {$IFDEF COMPILER6_UP}
  JV_WINCONTROL(CustomComboBoxEx)
  JV_WINCONTROL(CustomStatusBar)
  JV_WINCONTROL(ComboBoxEx)
  {$ENDIF COMPILER6_UP}
  JV_WINCONTROL(CoolBar)

  JV_WINCONTROL(CommonCalendar)
  JV_WINCONTROL(MonthCalendar)
  JV_WINCONTROL(CustomHotKey)
  JV_WINCONTROL(HotKey)
  JV_WINCONTROL(CustomUpDown)
  JV_WINCONTROL(UpDown)
  JV_WINCONTROL(DateTimePicker)
  JV_WINCONTROL(PageScroller)
  JV_WINCONTROL(ProgressBar)
  JV_WINCONTROL(StatusBar)
  JV_WINCONTROL(TabSheet)
  JV_WINCONTROL(ToolBar)
  JV_CONTROL(ToolButton)

implementation

{$IFDEF COMPILER6_UP}
JV_WINCONTROL_IMPL(CustomComboBoxEx)
JV_WINCONTROL_IMPL(CustomStatusBar)
JV_WINCONTROL_IMPL(ComboBoxEx)
JV_WINCONTROL_IMPL(CustomHeaderControl)
{$ENDIF COMPILER6_UP}
JV_WINCONTROL_IMPL(HeaderControl)
JV_WINCONTROL_IMPL(CustomListView)
JV_WINCONTROL_IMPL(ListView)
JV_WINCONTROL_IMPL(CustomTreeView)
JV_WINCONTROL_IMPL(TreeView)
JV_WINCONTROL_IMPL(TrackBar)
JV_WINCONTROL_IMPL(Animate)
JV_WINCONTROL_IMPL(CoolBar)
JV_WINCONTROL_IMPL(CommonCalendar)
JV_WINCONTROL_IMPL(MonthCalendar)

JV_WINCONTROL_IMPL(CustomHotKey)
JV_WINCONTROL_IMPL(HotKey)

JV_WINCONTROL_IMPL(CustomUpDown)
JV_WINCONTROL_IMPL(UpDown)

JV_WINCONTROL_IMPL(DateTimePicker)

JV_WINCONTROL_IMPL(ProgressBar)
JV_WINCONTROL_IMPL(PageControl)
JV_WINCONTROL_IMPL(PageScroller)
JV_WINCONTROL_IMPL(CustomTabControl)
JV_WINCONTROL_IMPL(TabControl)
JV_WINCONTROL_IMPL(TabSheet)
JV_WINCONTROL_IMPL(ToolBar)
JV_WINCONTROL_IMPL(StatusBar)
JV_CONTROL_IMPL(ToolButton)

end.
