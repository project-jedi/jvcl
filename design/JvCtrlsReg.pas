{$I JVCL.INC}

unit JvCtrlsReg;

interface

procedure Register;

implementation
uses
  Classes, DesignIntf, JvBehaviorLabel,
  JvBehaviorLabelEditor;

{.$R ..\resources\JvCtrlsReg.dcr}

procedure Register;
begin
  RegisterPropertyEditor(typeinfo(TJvLabelBehaviorName),TJvBehaviorLabel,'Behavior',TJvLabelBehaviorProperty);
end;

end.
