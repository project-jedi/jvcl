program gl_Demo;

uses
  Forms,
  glDemo in 'glDemo.pas' {Form1},
  hshape in 'hshape.pas' {Form2},
  glHelpPanel_demo in 'glHelpPanel_demo.pas' {fTglHelpPanel};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TGLDemoFrm, GLDemoFrm);
  Application.CreateForm(THShapeFrm, HShapeFrm);
//  Application.CreateForm(TfTglHelpPanel, fTglHelpPanel);
  Application.Run;
end.
