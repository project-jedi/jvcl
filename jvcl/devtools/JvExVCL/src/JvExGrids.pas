{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExGrids.pas, released on 2004-01-04

The Initial Developer of the Original Code is Andreas Hausladen [Andreas dott Hausladen att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvExGrids;

{$I jvcl.inc}
{MACROINCLUDE JvExControls.macros}

WARNINGHEADER

interface

uses
  Windows, Messages, Graphics, Controls, Forms, Grids,
  {$IFDEF COMPILER6_UP}
  Types,
  {$ENDIF COMPILER6_UP}
  Classes, SysUtils,
  JvTypes, JvThemes, JVCLVer, JvExControls;

 {$DEFINE NeedMouseEnterLeave}

type
{$IFDEF COMPILER6_UP}
  {$DEFINE HAS_GRID_EDITSTYLE}
{$ENDIF COMPILER6_UP}

  {$IFNDEF HAS_GRID_EDITSTYLE}
  // Compiler 5 and VisualCLX do not have TEditStyle
  TEditStyle = (esSimple, esEllipsis, esPickList);
  {$ENDIF HAS_GRID_EDITSTYLE}


  JV_WINCONTROL_EVENTS(InplaceEdit)
  JV_WINCONTROL_EVENTS(CustomGrid)
  {$IFDEF COMPILER6_UP}
  JV_WINCONTROL_EVENTS(CustomDrawGrid)
  JV_WINCONTROL_EVENTS(InplaceEditList)
  {$ENDIF COMPILER6_UP}

  JV_WINCONTROL_EVENTS_BEGIN(DrawGrid)
  JV_CONSTRUCTOR
  {$IFNDEF HAS_GRID_EDITSTYLE}
  protected
    function GetEditStyle(ACol, ARow: Longint): TEditStyle; dynamic;
  {$ENDIF !HAS_GRID_EDITSTYLE}
  JV_WINCONTROL_EVENTS_END(DrawGrid)

  JV_WINCONTROL_EVENTS_BEGIN(StringGrid)
  JV_CONSTRUCTOR
  {$IFNDEF HAS_GRID_EDITSTYLE}
  protected
    function GetEditStyle(ACol, ARow: Longint): TEditStyle; dynamic;
  {$ENDIF !HAS_GRID_EDITSTYLE}
  JV_CUSTOMCONTROL_EVENTS_END(StringGrid)

implementation

JV_WINCONTROL_EVENTS_IMPL(InplaceEdit)
JV_WINCONTROL_EVENTS_IMPL(CustomGrid)
{$IFDEF COMPILER6_UP}
JV_WINCONTROL_EVENTS_IMPL(CustomDrawGrid)
JV_WINCONTROL_EVENTS_IMPL(InplaceEditList)
{$ENDIF COMPILER6_UP}

JV_CUSTOMCONTROL_EVENTS_IMPL_BEGIN(DrawGrid)
{$IFNDEF HAS_GRID_EDITSTYLE}
function TJvExDrawGrid.GetEditStyle(ACol, ARow: Longint): TEditStyle;
begin
  Result := esSimple;
end;
{$ENDIF !HAS_GRID_EDITSTYLE}
JV_CUSTOMCONTROL_EVENTS_IMPL_END(DrawGrid)

JV_CUSTOMCONTROL_EVENTS_IMPL_BEGIN(StringGrid)
{$IFNDEF HAS_GRID_EDITSTYLE}
function TJvExStringGrid.GetEditStyle(ACol, ARow: Longint): TEditStyle;
begin
  Result := esSimple;
end;
{$ENDIF !HAS_GRID_EDITSTYLE}
JV_CUSTOMCONTROL_EVENTS_IMPL_END(StringGrid)

end.
