{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: MTRegister.PAS, released on 2002-09-24.

The Initial Developer of the Original Code is Erwin Molendijk.
Portions created by Erwin Molendijk are Copyright (C) 2002 Erwin Molendijk.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2002-09-25


Known Issues:
-----------------------------------------------------------------------------}

unit JvMtComponentsReg;

interface

Uses
  Classes, JvMtComponents;

procedure Register;

implementation

procedure Register;
const
  MTThreadsPage = 'Jv Threading';
begin
  RegisterComponents(MTThreadsPage, [TJvMtManager]);
  RegisterComponents(MTThreadsPage, [TJvMtThread]);

  RegisterComponents(MTThreadsPage, [TJvMtThreadToVCL]);
  RegisterComponents(MTThreadsPage, [TJvMtVCLToThread]);
  RegisterComponents(MTThreadsPage, [TJvMtThreadToThread]);

  RegisterComponents(MTThreadsPage, [TJvMtSection]);
  RegisterComponents(MTThreadsPage, [TJvMtCountingSection]);
  RegisterComponents(MTThreadsPage, [TJvMtMonitorSection]);
end;

end.
