//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvHMIC5D.res");
USEUNIT("..\..\design\JvHMIReg.pas");
USEUNIT("..\..\design\JvSegmentedLEDDisplayEditors.pas");
USEUNIT("..\..\design\JvSegmentedLEDDisplayMappingForm.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("JvCoreC5D.bpi");
USEPACKAGE("JvHMIC5R.bpi");

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

