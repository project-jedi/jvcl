//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvBDEC5R.res");
USEUNIT("..\..\run\JvBdeUtils.pas");
USEUNIT("..\..\run\JvBDECheckPasswordForm.pas");
USEUNIT("..\..\run\JvBDEExceptionForm.pas");
USEUNIT("..\..\run\JvBDEFilter.pas");
USEUNIT("..\..\run\JvBDEIndex.pas");
USEUNIT("..\..\run\JvBDELists.pas");
USEUNIT("..\..\run\JvBDELoginDialog.pas");
USEUNIT("..\..\run\JvBDEMemTable.pas");
USEUNIT("..\..\run\JvBDEMove.pas");
USEUNIT("..\..\run\JvBDEProgress.pas");
USEUNIT("..\..\run\JvBDEQBE.pas");
USEUNIT("..\..\run\JvBDEQuery.pas");
USEUNIT("..\..\run\JvBDESecurity.pas");
USEUNIT("..\..\run\JvBDESQLScript.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("vclbde50.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("JvSystemC5R.bpi");
USEPACKAGE("JvDlgsC5R.bpi");
USEPACKAGE("JvStdCtrlsC5R.bpi");
USEPACKAGE("JvDBC5R.bpi");
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
