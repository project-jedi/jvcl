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

Contributor(s): André Snepvangers [asn dott att xs4all.nl] (Redesign)

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
  Classes, SysUtils,
  JvTypes, JvThemes, JVCLVer, JvExControls;


type
{$IFNDEF COMPILER6_UP}
  TEditStyle = (esSimple, esEllipsis, esPickList);
{$ENDIF !COMPILER6_UP}

  {$UNDEF BOUNDSCHANGED_DIRECTIVE}
  {$DEFINE BOUNDSCHANGED_DIRECTIVE override}
  JV_WINCONTROL(InplaceEdit)
  {$UNDEF BOUNDSCHANGED_DIRECTIVE}
  {$DEFINE BOUNDSCHANGED_DIRECTIVE virtual}

  JV_WINCONTROL(CustomGrid)

  {$IFDEF COMPILER6_UP}
  JV_WINCONTROL(CustomDrawGrid)

  {$UNDEF BOUNDSCHANGED_DIRECTIVE}
  {$DEFINE BOUNDSCHANGED_DIRECTIVE override}
  JV_WINCONTROL(InplaceEditList)
  {$UNDEF BOUNDSCHANGED_DIRECTIVE}
  {$DEFINE BOUNDSCHANGED_DIRECTIVE virtual}

  {$ENDIF COMPILER6_UP}

  JV_WINCONTROL_BEGIN(DrawGrid)
  {$IFNDEF COMPILER6_UP}
  protected
    function GetEditStyle(ACol, ARow: Longint): TEditStyle; dynamic;
  {$ENDIF !COMPILER6_UP}
  JV_WINCONTROL_END(DrawGrid)

  JV_WINCONTROL_BEGIN(StringGrid)
  {$IFNDEF COMPILER6_UP}
  protected
    function GetEditStyle(ACol, ARow: Longint): TEditStyle; dynamic;
  {$ENDIF COMPILER6_UP}
  JV_WINCONTROL_END(StringGrid)

implementation

{$UNDEF BOUNDSCHANGED_CODE}
{$DEFINE BOUNDSCHANGED_CODE inherited BoundsChanged;}
JV_WINCONTROL_IMPL(InplaceEdit)
{$UNDEF BOUNDSCHANGED_CODE}
{$DEFINE BOUNDSCHANGED_CODE}

JV_WINCONTROL_IMPL(CustomGrid)

{$IFDEF COMPILER6_UP}
JV_WINCONTROL_IMPL(CustomDrawGrid)

{$UNDEF BOUNDSCHANGED_CODE}
{$DEFINE BOUNDSCHANGED_CODE inherited BoundsChanged;}
JV_WINCONTROL_IMPL(InplaceEditList)
{$UNDEF BOUNDSCHANGED_CODE}
{$DEFINE BOUNDSCHANGED_CODE}

{$ENDIF COMPILER6_UP}

{$IFNDEF COMPILER6_UP}
function TJvExDrawGrid.GetEditStyle(ACol, ARow: Longint): TEditStyle;
begin
  Result := esSimple;
end;
{$ENDIF !COMPILER6_UP}

JV_WINCONTROL_IMPL(DrawGrid)

{$IFNDEF COMPILER6_UP}
function TJvExStringGrid.GetEditStyle(ACol, ARow: Longint): TEditStyle;
begin
  Result := esSimple;
end;
{$ENDIF !COMPILER6_UP}

JV_WINCONTROL_IMPL(StringGrid)

end.
