{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvValidatorsReg.PAS, released on 2003-01-01.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 att users dott sourceforge dott net] .
Portions created by Peter Thörnqvist are Copyright (C) 2003 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvValidatorsReg;

{$I jvcl.inc}

interface

procedure Register;

implementation

uses
  Classes,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvDsgnConsts,
  JvErrorIndicator, JvValidators, JvValidatorsEditorForm, JvDsgnEditors;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvValidatorsReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
{$R ../Resources/JvValidatorsReg.dcr}
{$ENDIF UNIX}

procedure Register;
begin
  RegisterComponents(RsPaletteValidators, [TJvValidators,
    TJvValidationSummary, TJvErrorIndicator]);
  RegisterNoIcon([TJvRequiredFieldValidator, TJvCompareValidator,
    TJvRangeValidator, TJvRegularExpressionValidator, TJvCustomValidator, TJvControlsCompareValidator]);

  RegisterComponentEditor(TJvValidators, TJvValidatorComponent);
  RegisterPropertyEditor(TypeInfo(Integer), TJvErrorIndicator,
    'ImageIndex', TJvDefaultImageIndexProperty);
//  RegisterPropertyEditor(TypeInfo(string), TJvCustomFormatEdit, 'Characters', TJvCharStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvBaseValidator,
    'PropertyToValidate', TJvPropertyValidateProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvBaseValidator,
    'PropertyToCompare', TJvPropertyToCompareProperty);
  {$IFDEF COMPILER5}
  RegisterPropertyEditor(TypeInfo(TComponent), TComponent,
    'ValidationSummary', TJvValidationSummaryProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TComponent,
    'ErrorIndicator', TJvErrorIndicatorProperty);
  {$ENDIF COMPILER5}
end;

end.

