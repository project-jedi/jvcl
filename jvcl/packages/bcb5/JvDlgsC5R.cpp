//---------------------------------------------------------------------------
// BCB5 NOTE - The JvDialogActns unit has been removed from this package. Due
//             to what appears to be a BCB5 bug, including JvDialogActns in
//             the package causes a BCB5 linker error.
//
//             The linker error occurs even when the JvDialogActns.pas file
//             is reduced to contain only these lines:
//
//                unit JvDialogActns;
//
//                interface
//
//                uses
//                  JvBaseDlg;
//
//                type
//                  TJvCommonDialogFClass = class of TJvCommonDialogF;
//
//                end.
//
//             If you know why this might be, please post a note to the JVCL
//             newsgroup (http server: forums.talkto.net, group jedi.vcl).
//
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("JvDlgsC5R.res");
USEUNIT("..\..\run\JvWinDialogs.pas");
USEUNIT("..\..\run\JvAddPrinter.pas");
USEUNIT("..\..\run\JvCommonDialogD.pas");
USEUNIT("..\..\run\JvCommonExecDlg.pas");
USEUNIT("..\..\run\JvConnectNetwork.pas");
USEUNIT("..\..\run\JvCopyError.pas");
USEUNIT("..\..\run\JvDeleteError.pas");
// USEUNIT("..\..\run\JvDialogActns.pas");
USEUNIT("..\..\run\JvDialogs.pas");
USEUNIT("..\..\run\JvDiskPrompt.pas");
USEUNIT("..\..\run\JvDSADialogs.pas");
USEUNIT("..\..\run\JvDualList.pas");
USEUNIT("..\..\run\JvDualListForm.pas");
USEUNIT("..\..\run\JvExceptionForm.pas");
USEUNIT("..\..\run\JvFindFiles.pas");
USEUNIT("..\..\run\JvFindReplace.pas");
USEUNIT("..\..\run\JvImageDlg.pas");
USEUNIT("..\..\run\JvImageForm.pas");
USEUNIT("..\..\run\JvLoginForm.pas");
USEUNIT("..\..\run\JvObjectPickerDialog.pas");
USEUNIT("..\..\run\JvPageSetup.pas");
USEUNIT("..\..\run\JvPageSetupTitled.pas");
USEUNIT("..\..\run\JvProgressComponent.pas");
USEUNIT("..\..\run\JvProgressDialog.pas");
USEUNIT("..\..\run\JvProgressForm.pas");
USEUNIT("..\..\run\JvRenameError.pas");
USEUNIT("..\..\run\JvSelectDirectory.pas");
USEUNIT("..\..\run\JvTipOfDay.pas");
USEUNIT("..\..\common\ObjSel.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vcljpg50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("JvCoreC5R.bpi");
USEPACKAGE("JvSystemC5R.bpi");
USEPACKAGE("JvStdCtrlsC5R.bpi");
USEPACKAGE("bcbsmp50.bpi");
USEPACKAGE("bcbie50.bpi");

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
