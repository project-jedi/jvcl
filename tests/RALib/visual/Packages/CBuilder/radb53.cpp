//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEPACKAGE("Vcl50.bpi");
USEPACKAGE("Vcldb50.bpi");
USEPACKAGE("VCLBDE50.bpi");
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

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
