program BPRfix;

uses
  Forms,
  BPRfixForm in 'BPRfixForm.pas' {FormMain},
  BPRfixScanForm in 'BPRfixScanForm.pas' {FormScan};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

