{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: vclonly.inc, released on 2004-05-16.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

// This inc file depends on jedi.inc which has to
// be included first (usually indirectly through
// the inclusion of jvcl.inc).

// Suppress platform warnings which are irrelevant
// because the including unit can only be compiled
// with the VCL which implies Windows platform.

{$IFDEF SUPPORTS_PLATFORM_WARNINGS}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF SUPPORTS_PLATFORM_WARNINGS}

// Cause a compilation error for any component library except the VCL.

{$IFNDEF VCL}
  {$IFDEF SUPPORTS_COMPILETIME_MESSAGES}
    {$MESSAGE FATAL 'This unit is only supported for VCL!'}
  {$ELSE}
    'This unit is only supported for VCL!'
  {$ENDIF SUPPORTS_COMPILETIME_MESSAGES}
{$ENDIF !VCL}
