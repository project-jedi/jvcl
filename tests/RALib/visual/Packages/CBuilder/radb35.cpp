//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("radb35.res");
USEPACKAGE("vcl35.bpi");
USEPACKAGE("vcldb35.bpi");
USEUNIT("JvSQLS.pas");
USEUNIT("JvDBLookupTreeView.pas");
USEUNIT("JvDBMove.pas");
USEUNIT("JvDBTreeView.pas");
USEUNIT("JvDBUtil.pas");
USEUNIT("JvDBConst.pas");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
    return 1;
}
//---------------------------------------------------------------------------
