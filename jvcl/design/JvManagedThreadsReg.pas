{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvManagedThreadsReg.PAS, released on 2002-09-24.

The Initial Developer of the Original Code is Erwin Molendijk.
Portions created by Erwin Molendijk are Copyright (C) 2002 Erwin Molendijk.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-09-25

Known Issues:
-----------------------------------------------------------------------------}

unit JvManagedThreadsReg;

interface

uses
  Classes,
  {$IFDEF USEJVCL}
  JvDsgnConsts,
  {$ENDIF}
  JvMtComponents;

procedure Register;

implementation

{$R ..\Resources\JvManagedThreadsReg.dcr}

{$IFNDEF USEJVCL}
resourcestring
  RsPaletteMTThreads = 'Jv Threading';
{$ENDIF}

procedure Register;
begin
  RegisterComponents(RsPaletteMTThreads, [TJvMtManager, TJvMtThread,
    TJvMtThreadToVCL, TJvMtVCLToThread, TJvMtThreadToThread, TJvMtSection,
    TJvMtCountingSection, TJvMtMonitorSection]);
end;

end.

