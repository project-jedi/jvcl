//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvPrintPreviewC5D.res");
USEUNIT("..\..\design\JvPreviewReg.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("JvPrintPreviewC5R.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("JvCoreC5R.bpi");

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

