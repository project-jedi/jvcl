{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBDatePickerEdit, released on 2002-09-XX.

The Initial Developer of the Original Code is Oliver Giesen [giesen@lucatec.com]
Portions created by Oliver Giesen are Copyright (C) 2002 Lucatec GmbH.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2002-09-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDatePickerReg;

interface

procedure Register;

implementation

uses
  Classes,
  {$IFNDEF DelphiPersonalEdition}
  JvDBDatePickerEdit,
  {$ENDIF}
  JvCheckedMaskEdit, JvDatePickerEdit, JvxDConst;

{.$R ..\Resources\JvDatePickerReg.dcr}

procedure Register;
begin
  RegisterComponents(srJvCompositesPalette, [TJvCheckedMaskEdit, TJvDatePickerEdit]);
  {$IFNDEF DelphiPersonalEdition}
  RegisterComponents(srJvDataControlsPalette, [TJvDBDatePickerEdit]);
  {$ENDIF}
end;

end.
