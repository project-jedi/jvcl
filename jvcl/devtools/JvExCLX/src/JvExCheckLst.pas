{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExCheckLst.pas, released on 2004-01-04

The Initial Developer of the Original Code is Andreas Hausladen [Andreas dott Hausladen att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): André Snepvangers [asn dott att xs4all.nl] (Redesign)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvExCheckLst;

{$I jvcl.inc}
{MACROINCLUDE JvExControls.macros}

WARNINGHEADER

interface

uses
  Windows, Messages, Graphics, Controls, Forms, CheckLst,
  Classes, SysUtils,
  JvTypes, JvThemes, JVCLVer, JvExControls;


type
  JV_WINCONTROL(CheckListBox)

implementation

JV_WINCONTROL_IMPL(CheckListBox)

end.
