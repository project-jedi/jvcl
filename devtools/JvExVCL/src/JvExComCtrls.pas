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

{$I jvcl.inc}
{MACROINCLUDE JvExControls.macros}

WARNINGHEADER

unit JvExComCtrls;

interface

uses
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, ComCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Qt, QGraphics, QControls, QForms, QComCtrls, Types, QWindows,
  {$ENDIF VisualCLX}
  Classes, SysUtils,
  JvTypes, JvThemes, JVCLVer, JvExControls;

{$IFDEF VCL}
 {$DEFINE NeedMouseEnterLeave}
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
  {$DEFINE NeedMouseEnterLeave}
 {$IFEND}
{$ENDIF VisualCLX}

{$DEFINE ANIMATE}
{$IFDEF COMPILER6_UP}
 {$IF not declared(TAnimate)}
  {$UNDEF ANIMATE}
 {$IFEND}
{$ENDIF COMPILER6_UP}

type
  {$IFDEF VCL}
  JV_WINCONTROL_EVENTS(ProgressBar)
  JV_WINCONTROL_EVENTS(CustomTabControl)
  JV_WINCONTROL_EVENTS(TabControl)
  JV_WINCONTROL_EVENTS(TabSheet)
  JV_WINCONTROL_EVENTS(PageControl)
  JV_WINCONTROL_EVENTS(StatusBar)
  JV_WINCONTROL_EVENTS(ToolBar)
  JV_CONTROL_EVENTS(ToolButton)
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  JV_CONTROL_EVENTS(ProgressBar)
  JV_CUSTOMCONTROL_EVENTS(CustomTabControl)
  JV_CUSTOMCONTROL_EVENTS(TabControl)
  JV_CUSTOMCONTROL_EVENTS(TabSheet)
  JV_CUSTOMCONTROL_EVENTS(PageControl)
  JV_CUSTOMCONTROL_EVENTS(StatusBar)
  JV_STDCONTROL_EVENTS(CustomViewControl)
  JV_CUSTOMCONTROL_EVENTS(ToolBar)
  JV_CUSTOMCONTROL_EVENTS(ToolButton)
  {$ENDIF VisualCLX}
  JV_STDCONTROL_EVENTS(TrackBar)
  {$IFDEF COMPILER6_UP}
  JV_WINCONTROL_EVENTS(CustomHeaderControl)
  {$IFDEF VCL}
  JV_WINCONTROL_EVENTS(CustomStatusBar)
  {$ENDIF VCL}
  {$ENDIF COMPILER6_UP}
  JV_WINCONTROL_EVENTS(HeaderControl)
  JV_STDCONTROL_EVENTS(CustomTreeView)
  JV_STDCONTROL_EVENTS(TreeView)
  JV_STDCONTROL_EVENTS(CustomListView)
  JV_STDCONTROL_EVENTS(ListView)
  {$IFDEF ANIMATE}
  {$IFDEF VCL}
  JV_WINCONTROL_EVENTS(Animate)
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  JV_CUSTOMCONTROL_EVENTS(Animate)
  {$ENDIF VisualCLX}
  {$ENDIF ANIMATE}
  {$IFDEF VCL}
  JV_WINCONTROL_EVENTS(CustomHotKey)
  JV_WINCONTROL_EVENTS(CustomUpDown)
  JV_WINCONTROL_EVENTS(CoolBar)
  JV_WINCONTROL_EVENTS(MonthCalendar)
  JV_WINCONTROL_EVENTS(HotKey)
  JV_WINCONTROL_EVENTS(UpDown)
  JV_WINCONTROL_EVENTS(CommonCalendar)
  JV_WINCONTROL_EVENTS(DateTimePicker)
  JV_WINCONTROL_EVENTS(PageScroller)
  {$IFDEF COMPILER6_UP}
  JV_WINCONTROL_EVENTS(CustomComboBoxEx)
  JV_WINCONTROL_EVENTS(ComboBoxEx)
  {$ENDIF COMPILER6_UP}
  {$ENDIF VCL}


implementation

{$IFDEF VCL}
JV_WINCONTROL_EVENTS_IMPL(ProgressBar)
JV_WINCONTROL_EVENTS_IMPL(CustomTabControl)
JV_WINCONTROL_EVENTS_IMPL(TabControl)
JV_WINCONTROL_EVENTS_IMPL(TabSheet)
JV_WINCONTROL_EVENTS_IMPL(PageControl)
JV_WINCONTROL_EVENTS_IMPL(StatusBar)
JV_WINCONTROL_EVENTS_IMPL(ToolBar)
JV_CONTROL_EVENTS_IMPL(ToolButton)
{$ENDIF VCL}
{$IFDEF VisualCLX}
JV_CONTROL_EVENTS_IMPL(ProgressBar)
JV_CUSTOMCONTROL_EVENTS_IMPL(CustomTabControl)
JV_CUSTOMCONTROL_EVENTS_IMPL(TabControl)
JV_CUSTOMCONTROL_EVENTS_IMPL(TabSheet)
JV_CUSTOMCONTROL_EVENTS_IMPL(PageControl)
JV_CUSTOMCONTROL_EVENTS_IMPL(StatusBar)
JV_STDCONTROL_EVENTS_IMPL(CustomViewControl)
JV_CUSTOMCONTROL_EVENTS_IMPL(ToolBar)
JV_CUSTOMCONTROL_EVENTS_IMPL(ToolButton)
{$ENDIF VisualCLX}
JV_STDCONTROL_EVENTS_IMPL(TrackBar)
{$IFDEF COMPILER6_UP}
JV_WINCONTROL_EVENTS_IMPL(CustomHeaderControl)
{$IFDEF VCL}
JV_WINCONTROL_EVENTS_IMPL(CustomStatusBar)
{$ENDIF VCL}
{$ENDIF COMPILER6_UP}
JV_WINCONTROL_EVENTS_IMPL(HeaderControl)
JV_STDCONTROL_EVENTS_IMPL(CustomTreeView)
JV_STDCONTROL_EVENTS_IMPL(TreeView)
JV_STDCONTROL_EVENTS_IMPL(CustomListView)
JV_STDCONTROL_EVENTS_IMPL(ListView)
{$IFDEF ANIMATE}
{$IFDEF VCL}
JV_WINCONTROL_EVENTS_IMPL(Animate)
{$ENDIF VCL}
{$IFDEF VisualCLX}
JV_CUSTOMCONTROL_EVENTS_IMPL(Animate)
{$ENDIF VisualCLX}
{$ENDIF ANIMATE}
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
