{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDsgnTypes.PAS, released on 2003-10-12.

The Initial Developer of the Original Code is Robert Marquardt [robert_marquardt att gmx dott de]
Portions created by Robert Marquardt are Copyright (C) 2003 Robert Marquardt .
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDsgnTypes;

{$I jvcl.inc}

interface

uses
  {$IFDEF COMPILER6_UP}
  DesignIntf;
  {$ELSE}
  DsgnIntf;
  {$ENDIF COMPILER6_UP}

type
  {$IFDEF COMPILER6_UP}
  IJvDesigner = DesignIntf.IDesigner;
  IJvFormDesigner = DesignIntf.IDesigner;
  {$ELSE}
  IJvDesigner = IFormDesigner;
  IJvFormDesigner = IFormDesigner;
  {$ENDIF COMPILER6_UP}

implementation

end.

