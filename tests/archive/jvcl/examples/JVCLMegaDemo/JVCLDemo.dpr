program jvcldemo;

uses
  Forms,
  Unitmain in 'Unitmain.pas' {Mainform},
  JvFormsU in 'JvFormsU.pas' {JvFormsFrm: TFrame},
  jvAnimatedTitelform in 'jvAnimatedTitelform.pas' {frAnimatedTitel},
  jvPerforatedform in 'jvPerforatedform.pas' {frPerforatedForm},
  jvTransparentFormd in 'jvTransparentFormd.pas' {frTransparentForm},
  jvanimatedformicondemo in 'jvanimatedformicondemo.pas' {frAnimatedFormIcon},
  jvanimatedappicondemo in 'jvanimatedappicondemo.pas' {frAnimatedApplicationicon},
  jvFormplacedemo in 'jvFormplacedemo.pas' {frFormplace},
  jvFormanimationdemo in 'jvFormanimationdemo.pas' {frFormAnimation},
  jvAutosizeformdemo in 'jvAutosizeformdemo.pas' {frAutosize},
  jvMagnetformdemo in 'jvMagnetformdemo.pas' {frmagnet},
  jvGradientformdemo in 'jvGradientformdemo.pas' {frgradient},
  JvDialogsU in 'JvDialogsU.pas' {JvDialogsFrm: TFrame},
  JvEditsU in 'JvEditsU.pas' {JvEditsFrm: TFrame},
  JvGraphicalU in 'JvGraphicalU.pas' {JvGraphicalFrm: TFrame},
  JvChoosersU in 'JvChoosersU.pas' {JvChoosersFrm: TFrame},
  JvFilesU in 'JvFilesU.pas' {JvFilesFrm: TFrame},
  JvLabelsU in 'JvLabelsU.pas' {JvLabelsFrm: TFrame},
  JvUtilsU in 'JvUtilsU.pas' {JvUtilsFrm: TFrame},
  JvPanelsU in 'JvPanelsU.pas' {JvPanelsFrm: TFrame},
  JvDateTimeU in 'JvDateTimeU.pas' {JvDateTimeFrm: TFrame},
  JvControlsU in 'JvControlsU.pas' {JvControls: TFrame},
  jvAniViewerU in 'jvAniViewerU.pas' {jvAniViewer: TFrame},
  JvSearchFileU in 'JvSearchFileU.pas' {JvSearchFileFrm: TFrame},
  JvMousePositionnerU in 'JvMousePositionnerU.pas' {JvMousePositionnerFrm: TFrame},
  JvDataEmbeddedPU in 'JvDataEmbeddedPU.pas' {JvDataEmbeddedFrm: TFrame},
  JvBmpAnimatorU in 'JvBmpAnimatorU.pas' {JvBmpAnimatorFrm: TFrame},
  JvArrowButtonU in 'JvArrowButtonU.pas' {JvArrowButtonFrm: TFrame},
  JvClipboardViewerU in 'JvClipboardViewerU.pas' {JvClipboardViewerFrm: TFrame},
  JvBrowseFolderU in 'JvBrowseFolderU.pas' {JvBrowseFolderFrm: TFrame},
  jvInstallLabelU in 'jvInstallLabelU.pas' {JvInstallLabelFrm: TFrame};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'JVCL Demo Application';
  Application.CreateForm(TMainform, Mainform);
  Application.Run;
end.
