//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\Source\JvRegAutoEditor.pas", Jvregautoeditor, JvRegEditor);
USEFORMNS("..\Source\JvHTHintEditor.pas", Jvhthinteditor, JvHintEditor);
USEFORMNS("..\Source\JvFormLog.pas", Jvformlog, foLog);
USEFORMNS("..\Source\JvDateTimeDlg.pas", Jvdatetimedlg, frmSelectDateTimeDlg);
USEFORMNS("..\Source\JvStrLEdit.pas", Jvstrledit, JvStrEditDlg);
USEFORMNS("..\source\JvOLBEditor.pas", Jvolbeditor, frmOLBEditor);
USEFORMNS("..\source\JvImagPrvw.pas", Jvimagprvw, ImageForm);
USEFORMNS("..\source\JvCheckItm.pas", Jvcheckitm, JvCheckItemsEditor);
USEFORMNS("..\source\JvDirFrm.pas", Jvdirfrm, JvDirectoryListDialog);
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
USEFORMNS("..\source\JvBandObjectDLLWizardForm.pas", Jvbandobjectdllwizardform, zWizardForm);
USEFORMNS("..\source\JvPluginParams.pas", Jvpluginparams, frmPluginParams);
USEFORMNS("..\source\JvActnRes.pas", Jvactnres, JvStandardActions); /* TDataModule: File Type */
USEFORMNS("..\source\JvScheduleEditor.pas", Jvscheduleeditor, FrmScheduleEditor);
USEFORMNS("..\source\JvID3v2Editor.pas", Jvid3v2editor, JvID3FramesEditor);
USEFORMNS("..\source\JvID3v2Define.pas", Jvid3v2define, JvID3DefineDlg);
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
