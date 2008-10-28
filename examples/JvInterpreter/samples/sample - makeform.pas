var
  MyForm: TForm;
begin
  MyForm := JvInterpreterMakeForm(SamplesDir+'fModalForm.pas');
  if MyForm.ShowModal = mrOk then
    showmessage('OK button clicked');
  MyForm.Free;
end;
