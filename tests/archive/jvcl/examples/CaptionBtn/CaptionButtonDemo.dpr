program CaptionButtonDemo;

uses
  Forms, CaptionBtnMainFormU;
  
{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TCaptionBtnMainForm, CaptionBtnMainForm);
  Application.Run;
end.
