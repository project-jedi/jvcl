program DBTree;

uses
  Forms,
  fDBTree in 'fDBTree.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TRADBTreeView component demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
