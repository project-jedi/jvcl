{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvEDIDBBufferingReg.PAS, released on 2004-03-05.

The Initial Developer of the Original Code is Raymond Alexander .
Portions created by Joe Doe are Copyright (C) 2003 Raymond Alexander.

All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvEDIDBBufferingReg;

interface

procedure Register;

implementation

uses
  Classes,
  JvDsgnConsts,
  JvEDIDBBuffering {, JvEDITCPServer, JvEDITCPClient};

{$IFDEF VCL}
{$R ..\Resources\JvEDIDBBufferingReg.dcr}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$R ../Resources/JvEDIDBBufferingReg.dcr}
{$ENDIF VisualCLX}

procedure Register;
begin
 RegisterComponents(RsPaletteEDI, [TJvEDIDBSpecProfiler,
   TJvEDIDBSEFProfiler, TJvEDIDBBuffer {TJvEDITCPServer, TJvEDITCPClient}]);
end;

end.

