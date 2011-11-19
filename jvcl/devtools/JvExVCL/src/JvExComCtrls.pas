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
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvExComCtrls;

{$I jvcl.inc}
{MACROINCLUDE JvExControls.macros}

WARNINGHEADER

interface

uses
  Windows, Messages, Types,
  SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvTypes, JvThemes, JVCLVer, JvExControls;

type
  WINCONTROL_DECL_DEFAULT(CustomHeaderControl)

  WINCONTROL_DECL_DEFAULT(HeaderControl)

  WINCONTROL_DECL_DEFAULT(CustomTreeView)

  WINCONTROL_DECL_DEFAULT(TreeView)

  WINCONTROL_DECL_DEFAULT(CustomListView)

  WINCONTROL_DECL_DEFAULT(ListView)

  WINCONTROL_DECL_DEFAULT(PageControl)

  WINCONTROL_DECL_DEFAULT(CustomTabControl)

  WINCONTROL_DECL_DEFAULT(TabControl)

  WINCONTROL_DECL_DEFAULT(TrackBar)

  WINCONTROL_DECL_DEFAULT(Animate)

  WINCONTROL_DECL_DEFAULT(CustomComboBoxEx)

  WINCONTROL_DECL_DEFAULT(CustomStatusBar)

  WINCONTROL_DECL_DEFAULT(ComboBoxEx)

  WINCONTROL_DECL_DEFAULT(CoolBar)

  WINCONTROL_DECL_DEFAULT(CommonCalendar)

  WINCONTROL_DECL_DEFAULT(MonthCalendar)

  WINCONTROL_DECL_DEFAULT(CustomHotKey)

  WINCONTROL_DECL_DEFAULT(HotKey)

  WINCONTROL_DECL_DEFAULT(CustomUpDown)

  WINCONTROL_DECL_DEFAULT(UpDown)

  WINCONTROL_DECL_DEFAULT(DateTimePicker)

  WINCONTROL_DECL_DEFAULT(PageScroller)

  WINCONTROL_DECL_DEFAULT(ProgressBar)

  WINCONTROL_DECL_DEFAULT(StatusBar)

  WINCONTROL_DECL_DEFAULT(TabSheet)

  WINCONTROL_DECL_DEFAULT(ToolBar)

  CONTROL_DECL_DEFAULT(ToolButton)

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

WINCONTROL_IMPL_DEFAULT(CustomComboBoxEx)

WINCONTROL_IMPL_DEFAULT(CustomStatusBar)

WINCONTROL_IMPL_DEFAULT(ComboBoxEx)

WINCONTROL_IMPL_DEFAULT(CustomHeaderControl)

WINCONTROL_IMPL_DEFAULT(HeaderControl)

WINCONTROL_IMPL_DEFAULT(CustomListView)

WINCONTROL_IMPL_DEFAULT(ListView)

WINCONTROL_IMPL_DEFAULT(CustomTreeView)

WINCONTROL_IMPL_DEFAULT(TreeView)

WINCONTROL_IMPL_DEFAULT(TrackBar)

WINCONTROL_IMPL_DEFAULT(Animate)

WINCONTROL_IMPL_DEFAULT(CoolBar)

WINCONTROL_IMPL_DEFAULT(CommonCalendar)

WINCONTROL_IMPL_DEFAULT(MonthCalendar)

WINCONTROL_IMPL_DEFAULT(CustomHotKey)

WINCONTROL_IMPL_DEFAULT(HotKey)

WINCONTROL_IMPL_DEFAULT(CustomUpDown)

WINCONTROL_IMPL_DEFAULT(UpDown)

WINCONTROL_IMPL_DEFAULT(DateTimePicker)

WINCONTROL_IMPL_DEFAULT(ProgressBar)

WINCONTROL_IMPL_DEFAULT(PageControl)

WINCONTROL_IMPL_DEFAULT(PageScroller)

WINCONTROL_IMPL_DEFAULT(CustomTabControl)

WINCONTROL_IMPL_DEFAULT(TabControl)

WINCONTROL_IMPL_DEFAULT(TabSheet)

WINCONTROL_IMPL_DEFAULT(ToolBar)

WINCONTROL_IMPL_DEFAULT(StatusBar)

CONTROL_IMPL_DEFAULT(ToolButton)

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.