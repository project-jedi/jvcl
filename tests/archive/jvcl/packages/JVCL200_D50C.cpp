//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JVCL200_D50C.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("..\source\JVCLReg.pas");
USEUNIT("..\Source\JvAlarmsEditor.pas");
USEUNIT("..\Source\JvBaseDlgEditor.pas");
USEUNIT("..\Source\JvCommonDialogDEditor.pas");
USEUNIT("..\Source\JvDataEmbeddedEditor.pas");
USEUNIT("..\Source\JvExchListboxes.pas");
USEFORMNS("..\Source\JvFormLists.pas", Jvformlists, FormListb);
USEFORMNS("..\Source\JvFormLog.pas", Jvformlog, foLog);
USEUNIT("..\Source\JvHtmlParserEditor.pas");
USEUNIT("..\Source\JvLogFile.pas");
USEUNIT("..\Source\JvLoginDlg.pas");
USEUNIT("..\Source\JvPatcherEditor.pas");
USEUNIT("..\Source\JVCLMiscal.pas");
USEUNIT("..\Source\JvOutlookEdit.pas");
USEFORMNS("..\Source\JvDateTimeDlg.pas", Jvdatetimedlg, frmSelectDateTimeDlg);
USEUNIT("..\Source\JvDsgnEditors.pas");
USEUNIT("..\Source\JvTimeLineEdit.pas");
USEUNIT("..\Source\JvOutEdit.pas");
USEFORMNS("..\Source\JvStrLEdit.pas", Jvstrledit, JvStrEditDlg);
USEUNIT("..\Source\JvOLBarEditor.pas");
USEFORMNS("..\Source\JvOBEdFrm.pas", Jvobedfrm, frmOLBarEditFrm);
USEUNIT("..\Source\JvChNtfyProperty.pas");
USEUNIT("..\Source\JvLCProperty.pas");
USEUNIT("..\Source\JvTipProperty.pas");
USEUNIT("..\source\JvAppletProperty.pas");
USEUNIT("..\source\JvLConst.pas");
USEUNIT("..\source\JvHintProp.pas");
USEUNIT("..\source\JvColors.pas");
USEUNIT("..\source\JvDsgn.pas");
USEUNIT("..\source\JvCtlReg.pas");
USEUNIT("..\source\JvTooReg.pas");
USEUNIT("..\source\JvDBReg.pas");
USEUNIT("..\source\JvBDEReg.pas");
USEFORMNS("..\source\JvImagPrvw.pas", Jvimagprvw, ImageForm);
USEFORMNS("..\source\JvCheckItm.pas", Jvcheckitm, JvCheckItemsEditor);
USEFORMNS("..\source\JvDirFrm.pas", Jvdirfrm, JvDirectoryListDialog);
USEUNIT("..\source\JvFormWallpaperEditor.pas");
USEFORMNS("..\source\JvIColEdit.pas", Jvicoledit, IconListDialog);
USEFORMNS("..\source\JvGradedit.pas", Jvgradedit, GradCaptionsEditor);
USEFORMNS("..\source\JvJVCLAbout.pas", Jvjvclabout, JvJVCLAboutForm);
USEFORMNS("..\source\JvMinMaxEd.pas", Jvminmaxed, MinMaxInfoEditDialog);
USEFORMNS("..\source\JvPgMngrEd.pas", Jvpgmngred, JvProxyEditor);
USEFORMNS("..\source\JvPictEdit.pas", Jvpictedit, PictureEditDialog);
USEFORMNS("..\source\JvPresrDsn.pas", Jvpresrdsn, JvFormPropsDlg);
USEFORMNS("..\source\JvQBndDlg.pas", Jvqbnddlg, JvQueryParamsDialog);
USEFORMNS("..\source\JvSbEdit.pas", Jvsbedit, JvSpeedbarEditor);
USEFORMNS("..\source\JvSelDSFrm.pas", Jvseldsfrm, JvSelectDataSetForm);
USEFORMNS("..\source\JvTimLstEd.pas", Jvtimlsted, JvTimerItemsEditor);
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("JVCL200_R50C.bpi");
USEPACKAGE("dclstd50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("vclbde50.bpi");
USEPACKAGE("vcljpg50.bpi");
#//---------------------------------------------------------------------------
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

