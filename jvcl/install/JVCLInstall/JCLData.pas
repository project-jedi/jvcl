{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JCLData.pas, released on 2004-11-05.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JCLData;

{$I jvcl.inc}

interface

const
  { JCLVersion specifies the string that is displayed when the JVCL Installer
    requires the user to install or compile the JCL. Changing this constant
    will only change the displayed string but not the JCL version testing
    code. }
  JCLMinVersion = '2.2.1.3845';
  JCLVersion = '2.2';

  { JCLDcpFiles specifies the .dcp files that must exist in the JCLDcpDir in
    order to mark the JCL installation as valid. }
  JCLDcpFiles: array[0..0] of string = (
    'Jcl.dcp'
  );

implementation

end.