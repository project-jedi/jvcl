//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\design\JvSelectDataSetForm.pas", Jvselectdatasetform, JvSelectDataSetForm);
USEFORMNS("..\..\design\JvCsvDataForm.pas", Jvcsvdataform, JvCsvDefStrDialog);
USEPACKAGE("");
USEPACKAGE("dsnide50.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("JvDBC5R.bpi");
USEPACKAGE("dcldb50.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("db");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("JvCoreC5R.bpi");
USEPACKAGE("JvSystemC5R.bpi");
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
