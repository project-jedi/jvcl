unit JvCoreIntfReg;

interface
uses
  SysUtils, DesignEditors, DesignIntf;

type
  TJVCLCoreIntfEditor = class(TClassProperty)
  public
    procedure GetProperties(Proc: TGetPropProc); override;
  end;

procedure Register;

implementation
uses
  Classes, TypInfo, JvCoreIntf;


procedure Register;
begin
  RegisterComponents('JVCL', [TJvIntfLabel, TJvIntfButton]);
  RegisterPropertyEditor(typeinfo(TJVCLCoreComponent),nil,'',TJVCLCoreIntfEditor);
end;

{ TJVCLCoreIntfEditor }
{
//  TComponent variant
procedure TJVCLCoreIntfEditor.GetProperties(Proc: TGetPropProc);
var
  LComponents: IDesignerSelections;
  LDesigner: IDesigner;
begin
  LComponents := TDesignerSelections.Create;
  Designer.GetSelections(LComponents);
  if LComponents.Count > 0 then
  begin
    if not Supports(FindRootDesigner(LComponents[0]), IDesigner, LDesigner) then
      LDesigner := Designer;
    GetComponentProperties(LComponents, tkAny, LDesigner, Proc, nil);
  end;
end;
}

//  TClass with tkAny

procedure TJVCLCoreIntfEditor.GetProperties(Proc: TGetPropProc);
var
  I: Integer;
  J: Integer;
  Components: IDesignerSelections;
begin
  Components := TDesignerSelections.Create;
  for I := 0 to PropCount - 1 do
  begin
    J := GetOrdValueAt(I);
    if J <> 0 then
      Components.Add(TComponent(GetOrdValueAt(I)))
  end;
  if Components.Count > 0 then
    GetComponentProperties(Components, tkAny, Designer, Proc);
end;

end.
