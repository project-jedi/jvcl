program SampelProject1;

uses
  Unit2 in 'Unit2.pas' {Form2},
  Unit3 in 'Unit3.pas' {Form3},
  Unit4 in 'Unit4.pas' {DataModule1: TDataModule};

function main: string;
begin
  DataModule1 := TDataModule1.Create(nil);
  Form2 := TForm2.Create(nil);
  Form2.ShowModal;
  Form2.Free;
  DataModule1.Free;
end;

end.
