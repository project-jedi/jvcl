//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\source\JvFormAlarms.pas", Jvformalarms, FormAlarm);
USEFORMNS("..\source\JvFormImage.pas", Jvformimage, FormImg);
USEFORMNS("..\source\JvFormParser.pas", Jvformparser, FormParsers);
USEFORMNS("..\source\JvFormPatch.pas", Jvformpatch, foPatch);
USEFORMNS("..\source\JvFormWallpaperEdit.pas", Jvformwallpaperedit, foWallpaperChooser);
USEFORMNS("..\source\JvProfiler32.pas", Jvprofiler32, profreport);
USEFORMNS("..\source\JvColorForm.pas", Jvcolorform, JvClrFrm);
USEFORMNS("..\source\JvExcptDlg.pas", Jvexcptdlg, JvErrorDialog);
USEFORMNS("..\source\JvFDualLst.pas", Jvfduallst, JvDualListForm);
USEFORMNS("..\source\JvSbSetup.pas", Jvsbsetup, JvSpeedbarSetupWindow);
USEFORMNS("..\source\JvxLogin.pas", Jvxlogin, JvLoginForm);
USEFORMNS("..\source\JvChPswDlg.pas", Jvchpswdlg, JvChPswdForm);
USEFORMNS("..\source\JvDBExcpt.pas", Jvdbexcpt, JvBdeErrorDlg);
USEFORMNS("..\source\JvFormProgress.pas", Jvformprogress, FormProg);
USEFORMNS("..\Source\JvHLEdPropDlg.pas", Jvhledpropdlg, JvHLEditorParamsForm);
USEFORMNS("..\source\JvJVCLAbout.pas", Jvjvclabout, JvJVCLAboutForm);
USEFORMNS("..\source\JvProgressFrm.pas", Jvprogressfrm, frmProgress);
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
 