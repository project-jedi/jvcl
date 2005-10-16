program pgEdit;

uses
  Forms,
  SysUtils,
  MainForm in 'MainForm.pas' {frmMain},
  FileUtils in 'FileUtils.pas',
  TargetDialog in 'TargetDialog.pas' {frmTargets},
  GenerateUtils in 'GenerateUtils.pas',
  KnownTagsForm in 'KnownTagsForm.pas' {frmKnownTags},
  FormTypeDialog in 'FormTypeDialog.pas' {frmFormType},
  AdvancedOptionsForm in 'AdvancedOptionsForm.pas' {frmAdvancedOptions},
  GenerationMessagesForm in 'GenerationMessagesForm.pas' {frmGenMessages},
  ModelsForm in 'ModelsForm.pas' {frmModels},
  ConditionParser in 'ConditionParser.pas',
  PackageInformation in '..\Common\PackageInformation.pas',
  PackageModels in '..\Common\PackageModels.pas';

{$R *.res}

begin
  Application.Initialize;

  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmTargets, frmTargets);
  Application.CreateForm(TfrmKnownTags, frmKnownTags);
  Application.CreateForm(TfrmFormType, frmFormType);
  Application.CreateForm(TfrmAdvancedOptions, frmAdvancedOptions);
  Application.CreateForm(TfrmGenMessages, frmGenMessages);
  Application.CreateForm(TfrmModels, frmModels);
  Application.Run;
end.
