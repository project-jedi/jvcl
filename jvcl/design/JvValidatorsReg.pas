{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvValidatorsReg.PAS, released on 2003-01-01.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com] .
Portions created by Peter Thörnqvist are Copyright (C) 2003 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2003-01-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JVCL.INC}
{$I WINDOWSONLY.INC}
unit JvValidatorsReg;

interface
{$R ..\resources\JvValidatorsReg.dcr}
procedure Register;

implementation
uses
  Classes, JvErrProvider, JvValidators, JvValidatorsEditor, JvDsgnEditors,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf
  {$ELSE}
  DsgnIntf
  {$ENDIF};


procedure Register;
begin
  RegisterComponents('Jv Validators',
    [TJvValidators, TJvValidationSummary,TJvErrorProvider]);
  RegisterNoIcon([
    TJvRequiredFieldValidator,
      TJvCompareValidator,
      TJvRangeValidator,
      TJvRegularExpressionValidator,
      TJvCustomValidator]);

  RegisterComponentEditor(TJvValidators, TJvValidatorComponent);
  RegisterPropertyEditor(typeinfo(integer),TJvErrorProvider,'ImageIndex',TJvDefaultImageIndexProperty);
//  RegisterPropertyEditor(typeinfo(string),TJvCustomFormatEdit,'Characters',TJvCharStringProperty);
  RegisterPropertyEditor(typeinfo(string), TJvBaseValidator, 'PropertyToValidate', TJvPropertyValidateProperty);
  {$IFNDEF COMPILER6_UP}
  RegisterPropertyEditor(typeinfo(TComponent),TComponent,'ValidationSummary',TJvValidationSummaryProperty);
  RegisterPropertyEditor(typeinfo(TComponent),TComponent,'ErrorProvider',TJvErrorProviderProperty);
  {$ENDIF}
end;

end.

