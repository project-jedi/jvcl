//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\run\JvBDECheckPasswordForm.pas", Jvbdecheckpasswordform, JvChPswdForm);
USEFORMNS("..\..\run\JvBDEExceptionForm.pas", Jvbdeexceptionform, JvBdeErrorDlg);
USEPACKAGE("");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("db");
USEPACKAGE("bde");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("JvStdCtrlsC5R.bpi");
USEPACKAGE("JvDBC5R.bpi");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("JvSystemC5R.bpi");
USEPACKAGE("JvDlgsC5R.bpi");
USEPACKAGE("JvCoreC5R.bpi");
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
