program DBTree;

uses
  Forms,
  fJvDBTree in 'fJvDBTree.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TJvDBTreeView  component demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
