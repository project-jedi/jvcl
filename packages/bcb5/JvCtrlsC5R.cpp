//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\run\JvColorForm.pas", Jvcolorform, JvClrFrm);
USEFORMNS("..\..\run\JvSpeedbarSetupForm.pas", Jvspeedbarsetupform, JvSpeedbarSetupWindow);
USEPACKAGE("");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("bcbsmp50.bpi");
USEPACKAGE("bcbie50.bpi");
USEPACKAGE("JvSystemC5R.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("JvStdCtrlsC5R.bpi");
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
