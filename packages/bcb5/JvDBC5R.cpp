//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\run\JvDBQueryParamsForm.pas", Jvdbqueryparamsform, JvQueryParamsDialog);
USEPACKAGE("");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("db");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("JvStdCtrlsC5R.bpi");
USEPACKAGE("bcbsmp50.bpi");
USEPACKAGE("bcbie50.bpi");
USEPACKAGE("JvCoreC5R.bpi");
USEPACKAGE("JvSystemC5R.bpi");
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
