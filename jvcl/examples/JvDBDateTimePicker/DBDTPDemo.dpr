program DBDTPDemo;

uses
  Forms,
  JvDBDateTimePickerMainFormU in 'JvDBDateTimePickerMainFormU.pas' {JvDBDateTimePickerMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvDBDateTimePickerMainForm, JvDBDateTimePickerMainForm);
  Application.Run;
end.
