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
  JV_WINCONTROL(Animate)
  JV_WINCONTROL(CoolBar)
  JV_WINCONTROL(DateTimePicker)
  JV_CUSTOMCONTROL(HeaderControl)
  JV_WINCONTROL(CustomHotKey)
  JV_WINCONTROL(HotKey)
  JV_WINCONTROL(CommonCalendar)
  JV_WINCONTROL(MonthCalendar)
  JV_CUSTOMCONTROL(CustomListView)
  JV_CUSTOMCONTROL(ListView)
  JV_CUSTOMCONTROL(PageControl)
  JV_WINCONTROL(PageScroller)
  JV_WINCONTROL(ProgressBar)
  JV_CUSTOMCONTROL(StatusBar)
  JV_CUSTOMCONTROL(CustomTabControl)
  JV_CUSTOMCONTROL(TabControl)
  JV_WINCONTROL(TabSheet)
  JV_WINCONTROL(ToolBar)
  JV_CONTROL(ToolButton)
  JV_WINCONTROL(TrackBar)
  JV_CUSTOMCONTROL(CustomTreeView)
  JV_CUSTOMCONTROL(TreeView)
  JV_WINCONTROL(CustomUpDown)
  JV_WINCONTROL(UpDown)
  {$IFDEF COMPILER6_UP}
  JV_CUSTOMCONTROL(CustomHeaderControl)
  JV_CUSTOMCONTROL(CustomComboBoxEx)
  JV_CUSTOMCONTROL(CustomStatusBar)
  JV_CUSTOMCONTROL(ComboBoxEx)
  {$ENDIF COMPILER6_UP}

implementation

JV_WINCONTROL_IMPL(Animate)
JV_WINCONTROL_IMPL(CoolBar)
JV_WINCONTROL_IMPL(DateTimePicker)
JV_CUSTOMCONTROL_IMPL(HeaderControl)
JV_WINCONTROL_IMPL(CustomHotKey)
JV_WINCONTROL_IMPL(HotKey)
JV_CUSTOMCONTROL_IMPL(CustomListView)
JV_CUSTOMCONTROL_IMPL(ListView)
JV_WINCONTROL_IMPL(CommonCalendar)
JV_WINCONTROL_IMPL(MonthCalendar)
JV_CUSTOMCONTROL_IMPL(PageControl)
JV_WINCONTROL_IMPL(PageScroller)
JV_WINCONTROL_IMPL(ProgressBar)
JV_CUSTOMCONTROL_IMPL(StatusBar)
JV_CUSTOMCONTROL_IMPL(CustomTabControl)
JV_CUSTOMCONTROL_IMPL(TabControl)
JV_WINCONTROL_IMPL(TabSheet)
JV_WINCONTROL_IMPL(ToolBar)
JV_CONTROL_IMPL(ToolButton)
JV_WINCONTROL_IMPL(TrackBar)
JV_CUSTOMCONTROL_IMPL(CustomTreeView)
JV_CUSTOMCONTROL_IMPL(TreeView)
JV_WINCONTROL_IMPL(CustomUpDown)
JV_WINCONTROL_IMPL(UpDown)

{$IFDEF COMPILER6_UP}
JV_CUSTOMCONTROL_IMPL(CustomComboBoxEx)
JV_CUSTOMCONTROL_IMPL(CustomStatusBar)
JV_CUSTOMCONTROL_IMPL(ComboBoxEx)
JV_CUSTOMCONTROL_IMPL(CustomHeaderControl)
{$ENDIF COMPILER6_UP}

end.            
