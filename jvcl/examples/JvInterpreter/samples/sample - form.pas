unit SampleForm;

interface

// interface's uses clause is ignored
// by OnGetUnitSource event
uses Windows, Messages, Forms;

implementation

uses fModalForm;

function main: string;
var
  Form: TForm;
begin
  Form := TMyModalForm.Create(nil);
  Form.ShowModal;
  Form.Free;
end;

end.
