unit JvValidateEditReg;

interface
uses
  Classes, JvValidateEdit, DesignEditors, DesignIntf;

type
  TJvCharStringProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

procedure Register;

implementation
uses
  JvCharStrEditor;

procedure Register;
begin
  RegisterComponents('Jv Convert', [TJvValidateEdit]);
  RegisterPropertyEditor(typeinfo(string),TJvCustomValidateEdit,'Characters',TJvCharStringProperty);
end;


{ TJvCharStringProperty }

procedure TJvCharStringProperty.Edit;
var S:String;
begin
  S := GetValue;
  if TfrmJvCharEditDlg.Edit(S) then
    SetValue(S);
end;

function TJvCharStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

end.
