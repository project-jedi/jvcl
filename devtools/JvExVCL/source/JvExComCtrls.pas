{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExComCtrls.pas, released on 2004-01-04

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

unit JvExComCtrls;
interface
uses
  {$IFDEF VCL}
  Windows, Messages, Controls, Forms, ComCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QControls, QForms, QComCtrls,
  {$ENDIF VisualCLX}
  Classes, SysUtils,
  JvExControls;

type
  JV_WINCONTROL_EVENTS(CustomTabControl)
  JV_WINCONTROL_EVENTS(TabControl)
  JV_WINCONTROL_EVENTS(TabSheet)
  JV_WINCONTROL_EVENTS(PageControl)
  JV_WINCONTROL_EVENTS(StatusBar)
{$IFDEF VCL}
  JV_WINCONTROL_EVENTS(ProgressBar)
{$ELSE}
  JV_CONTROL_EVENTS(ProgressBar)
{$ENDIF VCL}
{$IFDEF COMPILER6_UP}
  JV_WINCONTROL_EVENTS(CustomHeaderControl)
 {$IFDEF VCL}
  JV_WINCONTROL_EVENTS(CustomStatusBar)
 {$ENDIF VCL}
{$ENDIF COMPILER6_UP}
  JV_WINCONTROL_EVENTS(HeaderControl)
  JV_WINCONTROL_EVENTS(CustomTreeView)
  JV_WINCONTROL_EVENTS(TreeView)
  JV_WINCONTROL_EVENTS(CustomListView)
  JV_WINCONTROL_EVENTS(ListView)

{$DEFINE ANIMATE}
{$IFDEF COMPILER6_UP}
 {$IF not declared(TAnimate)}
  {$UNDEF ANIMATE}
 {$IFEND}
{$ENDIF COMPILER6_UP}

{$IFDEF ANIMATE}
  JV_WINCONTROL_EVENTS(Animate)
{$ENDIF ANIMATE}
  JV_CONTROL_EVENTS(ToolButton)
  JV_WINCONTROL_EVENTS(ToolBar)
{$IFDEF VCL}
  JV_WINCONTROL_EVENTS(CustomHotKey)
  JV_WINCONTROL_EVENTS(HotKey)
  JV_WINCONTROL_EVENTS(CustomUpDown)
  JV_WINCONTROL_EVENTS(UpDown)
  JV_WINCONTROL_EVENTS(CoolBar)
  JV_WINCONTROL_EVENTS(CommonCalendar)
  JV_WINCONTROL_EVENTS(MonthCalendar)
  JV_WINCONTROL_EVENTS(DateTimePicker)
  JV_WINCONTROL_EVENTS(PageScroller)
 {$IFDEF COMPILER6_UP}
  JV_WINCONTROL_EVENTS(CustomComboBoxEx)
  JV_WINCONTROL_EVENTS(ComboBoxEx)
 {$ENDIF COMPILER6_UP}
{$ENDIF VCL}

implementation

JV_WINCONTROL_EVENTS_IMPL(CustomTabControl)
JV_WINCONTROL_EVENTS_IMPL(TabControl)
JV_WINCONTROL_EVENTS_IMPL(TabSheet)
JV_WINCONTROL_EVENTS_IMPL(PageControl)
JV_WINCONTROL_EVENTS_IMPL(StatusBar)
{$IFDEF VCL}
JV_WINCONTROL_EVENTS_IMPL(ProgressBar)
{$ELSE}
JV_CONTROL_EVENTS_IMPL(ProgressBar)
{$ENDIF VCL}
{$IFDEF COMPILER6_UP}
JV_WINCONTROL_EVENTS_IMPL(CustomHeaderControl)
 {$IFDEF VCL}
JV_WINCONTROL_EVENTS_IMPL(CustomStatusBar)
 {$ENDIF VCL}
{$ENDIF COMPILER6_UP}
JV_WINCONTROL_EVENTS_IMPL(HeaderControl)
JV_WINCONTROL_EVENTS_IMPL(CustomTreeView)
JV_WINCONTROL_EVENTS_IMPL(TreeView)
JV_WINCONTROL_EVENTS_IMPL(CustomListView)
JV_WINCONTROL_EVENTS_IMPL(ListView)

{$IFDEF ANIMATE}
JV_WINCONTROL_EVENTS_IMPL(Animate)
{$ENDIF ANIMATE}
JV_CONTROL_EVENTS_IMPL(ToolButton)
JV_WINCONTROL_EVENTS_IMPL(ToolBar)

{$IFDEF VCL}
JV_WINCONTROL_EVENTS_IMPL(CustomHotKey)
JV_WINCONTROL_EVENTS_IMPL(HotKey)
JV_WINCONTROL_EVENTS_IMPL(CustomUpDown)
JV_WINCONTROL_EVENTS_IMPL(UpDown)
JV_WINCONTROL_EVENTS_IMPL(CoolBar)
JV_WINCONTROL_EVENTS_IMPL(CommonCalendar)
JV_WINCONTROL_EVENTS_IMPL(MonthCalendar)
JV_WINCONTROL_EVENTS_IMPL(DateTimePicker)
JV_WINCONTROL_EVENTS_IMPL(PageScroller)
 {$IFDEF COMPILER6_UP}
JV_WINCONTROL_EVENTS_IMPL(CustomComboBoxEx)
JV_WINCONTROL_EVENTS_IMPL(ComboBoxEx)
 {$ENDIF COMPILER6_UP}
{$ENDIF VCL}

end.
