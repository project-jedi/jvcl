{$I JVCL.INC}
unit JvBehaviorLabelProperty;

interface
uses
  Classes, SysUtils, {$IFNDEF COMPILER6_UP}DsgnIntf{$ELSE}DesignIntf, DesignEditors{$ENDIF};

type
  TJvLabelBehaviorProperty = class(TStringProperty)
  public
    function AutoFill: Boolean; override;
    procedure SetValue(const Value: string); override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure Register;

implementation
uses
  JvxDConst, JvBehaviorLabel;


procedure Register;
begin
  RegisterComponents(srJvStandardPalette, [TJvBehaviorLabel]);
  RegisterPropertyEditor(typeinfo(TJvLabelBehaviorName),TJvBehaviorLabel,'Behavior',TJvLabelBehaviorProperty);
end;

{ TJvLabelBehaviorProperty }

function TJvLabelBehaviorProperty.AutoFill: Boolean;
begin
  Result := inherited AutoFill;
  // if you want to fix the flickering when double-clicking a value, uncomment line below:
//  Result := false;
end;

function TJvLabelBehaviorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paRevertable, paMultiSelect];
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
var List:IDesignerSelections;ADesigner:{$IFDEF COMPILER6_UP}IDesigner{$ELSE}IFormDesigner{$ENDIF};
begin
  inherited;
  List := CreateSelectionList;
  Designer.GetSelections(List);
  ADesigner := Designer;  // keep Designer alive
  ADesigner.SetSelections(nil);
  ADesigner.SetSelections(List);
//  Designer.Modified;
end;

end.

