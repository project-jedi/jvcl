//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\run\JvDualListForm.pas", Jvduallistform, JvDualListForm);
USEFORMNS("..\..\run\JvExceptionForm.pas", Jvexceptionform, JvErrorDialog);
USEFORMNS("..\..\run\JvImageForm.pas", Jvimageform, FormImg);
USEFORMNS("..\..\run\JvLoginForm.pas", Jvloginform, JvLoginForm);
USEFORMNS("..\..\run\JvProgressForm.pas", Jvprogressform, frmProgress);
USEPACKAGE("");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("JvCoreC5R.bpi");
USEPACKAGE("JvSystemC5R.bpi");
USEPACKAGE("JvStdCtrlsC5R.bpi");
USEPACKAGE("vclx50.bpi");
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
