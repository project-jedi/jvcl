program DockingInCode;
{New JVCL3 demo showing how to do Docking from Code, and also
 showing use of new center dock area.}
uses
  Forms,
  MainFm in 'MainFm.pas' {MainForm},
  DocFm in 'DocFm.pas' {DocForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDocForm, DocForm);
  Application.Run;
end.
