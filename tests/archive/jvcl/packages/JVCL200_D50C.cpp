//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------

USEUNIT("..\source\JVCLReg.pas");
USEUNIT("..\Source\JvAlarmsEditor.pas");
USEUNIT("..\Source\JvBaseDlgEditor.pas");
USEUNIT("..\Source\JvCommonDialogDEditor.pas");
USEUNIT("..\Source\JvDataEmbeddedEditor.pas");
USEFORMNS("..\Source\JvFormLog.pas", Jvformlog, foLog);
USEUNIT("..\Source\JvHtmlParserEditor.pas");
USEUNIT("..\Source\JvLogFile.pas");
USEUNIT("..\Source\JvPatcherEditor.pas");
USEFORMNS("..\Source\JvDateTimeDlg.pas", Jvdatetimedlg, frmSelectDateTimeDlg);
USEUNIT("..\Source\JvDsgnEditors.pas");
USEUNIT("..\Source\JvTimeLineEdit.pas");
USEUNIT("..\Source\JvOutEdit.pas");
USEFORMNS("..\Source\JvStrLEdit.pas", Jvstrledit, JvStrEditDlg);
USEFORMNS("..\Source\JvOLBEditor.pas", Jvolbeditor, frmOLBEditor);
USEUNIT("..\Source\JvChNtfyProperty.pas");
USEUNIT("..\source\JvAppletProperty.pas");
USEUNIT("..\source\JvColors.pas");
USEUNIT("..\source\JvCtlReg.pas");
USEUNIT("..\source\JvDBReg.pas");
USEUNIT("..\source\JvBDEReg.pas");
USEUNIT("..\source\JvBehaviorLabelProperty.pas")
USEFORMNS("..\source\JvImagPrvw.pas", Jvimagprvw, ImageForm);
USEFORMNS("..\source\JvCheckItm.pas", Jvcheckitm, JvCheckItemsEditor);
USEFORMNS("..\source\JvDirFrm.pas", Jvdirfrm, JvDirectoryListDialog);
USEUNIT("..\source\JvFormWallpaperEditor.pas");
USEFORMNS("..\source\JvIColEdit.pas", Jvicoledit, IconListDialog);
USEFORMNS("..\source\JvGradedit.pas", Jvgradedit, GradCaptionsEditor);
USEFORMNS("..\source\JvMinMaxEd.pas", Jvminmaxed, MinMaxInfoEditDialog);
USEFORMNS("..\source\JvPgMngrEd.pas", Jvpgmngred, JvProxyEditor);
USEFORMNS("..\source\JvPictEdit.pas", Jvpictedit, PictureEditDialog);
USEFORMNS("..\source\JvPresrDsn.pas", Jvpresrdsn, JvFormPropsDlg);
USEFORMNS("..\source\JvQBndDlg.pas", Jvqbnddlg, JvQueryParamsDialog);
USEFORMNS("..\source\JvSbEdit.pas", Jvsbedit, JvSpeedbarEditor);
USEFORMNS("..\source\JvSelDSFrm.pas", Jvseldsfrm, JvSelectDataSetForm);
USEFORMNS("..\source\JvTimLstEd.pas", Jvtimlsted, JvTimerItemsEditor);
USEUNIT("..\source\JvActions.pas");
USEFORMNS("..\source\JvActnRes.pas", Jvactnres, JvStandardActions); /* TDataModule: File Type */
USEFORMNS("..\source\JvBandObjectDLLWizardForm.pas", Jvbandobjectdllwizardform, zWizardForm);
USEUNIT("..\source\JvBandObjectDLLWizard.pas");
USEFORMNS("..\source\JvhtHintEditor.pas", Jvhthinteditor, JvHintEditor);
USEUNIT("..\source\JvIDEZoom.pas");
USEUNIT("..\source\JvIReg.pas");
USEFORMNS("..\source\JvPluginParams.pas", Jvpluginparams, frmPluginParams);
USEUNIT("..\source\JvPluginWizard.pas");
USEFORMNS("..\source\JvRegAutoEditor.pas", Jvregautoeditor, JvRegEditor);
USEUNIT("..\source\JvSchedEvntReg.pas");
USEUNIT("..\source\JvTipOfDayProp.pas");
USEFORMNS("..\source\JvScheduleEditor.pas", Jvscheduleeditor, frmScheduleEditor);
USEUNIT("..\source\JvToolReg.pas");
USERES("JVCL200_D50C.res");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("JVCL200_R50C.bpi");
USEPACKAGE("dclstd50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("vclbde50.bpi");
USEPACKAGE("vcljpg50.bpi");
USEPACKAGE("dsnide50.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("DCLDb50.bpi");
USEUNIT("..\source\JvaDsgn.pas");
USEUNIT("..\source\JvaCtlReg.pas");
USEUNIT("..\source\JvaDBReg.pas");
USEUNIT("..\source\JvJVCLAboutProperty.pas");
USEUNIT("..\source\JvxDConst.pas");
USEUNIT("..\source\JvTransLEDReg.pas");
//---------------------------------------------------------------------------
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

