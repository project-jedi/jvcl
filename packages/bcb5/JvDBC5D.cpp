//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvDBC5D.res");
USEUNIT("..\..\design\JvSelectDataSetForm.pas");
USEUNIT("..\..\design\JvCsvDataEditor.pas");
USEUNIT("..\..\design\JvCsvDataForm.pas");
USEUNIT("..\..\design\JvDBEditors.pas");
USEUNIT("..\..\design\JvDBReg.pas");
USEPACKAGE("JvStdCtrlsC5R.bpi");
USEPACKAGE("JvSystemC5R.bpi");
USEPACKAGE("JvDBC5R.bpi");
USEPACKAGE("JvCoreC5R.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("dcldb50.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vcldb50.bpi");
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
