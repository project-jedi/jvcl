//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvValidatorsC5D.res");
USEUNIT("..\..\design\JvValidatorsReg.pas");
USEUNIT("..\..\design\JvValidatorsEditorForm.pas");
USEPACKAGE("JvCoreC5D.bpi");
USEPACKAGE("JvValidatorsC5R.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("dclstd50.bpi");
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

