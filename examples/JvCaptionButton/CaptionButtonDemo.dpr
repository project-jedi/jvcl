program CaptionButtonDemo;

uses
  Forms,
  CaptionBtnMainFormU in 'CaptionBtnMainFormU.pas' {CaptionBtnMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TCaptionBtnMainForm, CaptionBtnMainForm);
  Application.Run;
end.
