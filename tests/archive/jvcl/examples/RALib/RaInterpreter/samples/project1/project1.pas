unit project1;

uses
  Unit2  {Form2},
  Unit4  {DataModule1};

function main: string;
begin
  DataModule1 := TDataModule1.Create(nil);
  Form2 := TForm2.Create(nil);
  Form2.ShowModal;
  Form2.Free;
  DataModule1.Free;
end;

end.
