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
  Windows, Messages,
  {$IFDEF HAS_UNIT_TYPES}
  Types,
  {$ENDIF HAS_UNIT_TYPES}
  SysUtils, Classes, Graphics, Controls, Forms, Grids,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvConsts, JvTypes, JvThemes, JVCLVer, JvExControls;

type
  {$IFDEF COMPILER5}
  // Compiler 5 and VisualCLX do not have TEditStyle
  TEditStyle = (esSimple, esEllipsis, esPickList);
  {$ENDIF COMPILER5}

  WINCONTROL_DECL_DEFAULT(InplaceEdit)

  WINCONTROL_DECL_DEFAULT(CustomGrid)

  {$IFDEF COMPILER6_UP}

  WINCONTROL_DECL_DEFAULT(CustomDrawGrid)

  WINCONTROL_DECL_DEFAULT(InplaceEditList)

  TJvExPubInplaceEditList = class(TJvExInplaceEditList)
  COMMON_PUBLISHED
  end;
  {$ENDIF COMPILER6_UP}

  TJvExDrawGrid = class(TDrawGrid, IJvExControl)
  WINCONTROL_DECL
  {$IFDEF COMPILER5}
  protected
    function GetEditStyle(ACol, ARow: Longint): TEditStyle; dynamic;
  {$ENDIF COMPILER5}
  end;

  TJvExStringGrid = class(TStringGrid, IJvExControl)
  WINCONTROL_DECL
  {$IFDEF COMPILER5}
  protected
    function GetEditStyle(ACol, ARow: Longint): TEditStyle; dynamic;
  {$ENDIF COMPILER5}
  end;

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

WINCONTROL_IMPL_DEFAULT(InplaceEdit)

WINCONTROL_IMPL_DEFAULT(CustomGrid)

{$IFDEF COMPILER6_UP}

WINCONTROL_IMPL_DEFAULT(CustomDrawGrid)

WINCONTROL_IMPL_DEFAULT(InplaceEditList)

{$ENDIF COMPILER6_UP}

WINCONTROL_IMPL_DEFAULT(DrawGrid)

{$IFDEF COMPILER5}
function TJvExDrawGrid.GetEditStyle(ACol, ARow: Longint): TEditStyle;
begin
  Result := esSimple;
end;
{$ENDIF COMPILER5}

WINCONTROL_IMPL_DEFAULT(StringGrid)

{$IFDEF COMPILER5}
function TJvExStringGrid.GetEditStyle(ACol, ARow: Longint): TEditStyle;
begin
  Result := esSimple;
end;
{$ENDIF COMPILER5}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
