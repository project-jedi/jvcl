unit JvBehaviorLabelProperty;

interface
uses
  Classes, SysUtils, DsgnIntf;

type
  TJvLabelBehaviorProperty = class(TStringProperty)
  public
    procedure SetValue(const Value: string); override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure Register;

implementation
uses
  JvBehaviorLabel, Dialogs;


procedure Register;
begin
  RegisterComponents('JVCL Test', [TJvBehaviorLabel]);
  RegisterPropertyEditor(typeinfo(TJvLabelBehaviorName),TJvBehaviorLabel,'Behavior',TJvLabelBehaviorProperty);
end;

{ TJvLabelBehaviorProperty }

function TJvLabelBehaviorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paRevertable, paMultiSelect, paReadonly];
end;

procedure TJvLabelBehaviorProperty.GetValues(Proc: TGetStrProc);
var S:TStringlist;i:integer;
begin
  S := TStringlist.Create;
  try
    GetRegisteredLabelBehaviorOptions(S);
    S.Sort;
    for i := 0 to S.Count -1 do
      Proc(S[i]);
  finally
    S.Free;
  end;
end;

procedure TJvLabelBehaviorProperty.SetValue(const Value: string);
var List:IDesignerSelections;
begin
  inherited;
  Designer.Modified;
  List := CreateSelectionList;
  Designer.GetSelections(List);
  Designer.SetSelections(List);
end;

end.

