var
  MyForm: TForm;
begin
  MyForm := JvInterpreterMakeForm(ExePath + 'samples\fModalForm.pas');
  if MyForm.ShowModal = mrOk then
    showmessage('OK button clicked');
  MyForm.Free;
end;
