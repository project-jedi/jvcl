program DBDTPDemo;

uses
  Forms, JvDBDateTimePickerMainFormU; 

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvDBDateTimePickerMainForm, JvDBDateTimePickerMainForm);
  Application.Run;
end.
