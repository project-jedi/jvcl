{$I JVCL.INC}
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

