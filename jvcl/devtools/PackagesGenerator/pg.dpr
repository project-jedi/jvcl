program pg;

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMain},
  FileUtils in 'FileUtils.pas',
  TargetDialog in 'TargetDialog.pas' {frmTargets},
  GenerateUtils in 'GenerateUtils.pas',
  KnownTagsForm in 'KnownTagsForm.pas' {frmKnownTags},
  FormTypeDialog in 'FormTypeDialog.pas' {frmFormType};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmTargets, frmTargets);
  Application.CreateForm(TfrmKnownTags, frmKnownTags);
  Application.CreateForm(TfrmFormType, frmFormType);
  Application.Run;
end.
