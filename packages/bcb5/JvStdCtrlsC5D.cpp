//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvStdCtrlsC5D.res");
USEUNIT("..\..\design\JvStdCtrlsReg.pas");
USEUNIT("..\..\design\JvCheckedItemsForm.pas");
USEUNIT("..\..\design\JvProgressEditor.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("dclstd50.bpi");
USEPACKAGE("JvCoreC5D.bpi");
USEPACKAGE("JvStdCtrlsC5R.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("JvSystemC5D.bpi");
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
