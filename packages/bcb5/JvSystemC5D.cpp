//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvSystemC5D.res");
USEPACKAGE("JvSystemC5R.bpi");
USEPACKAGE("JvCoreC5D.bpi");
USEPACKAGE("CJCL50.bpi");
USEUNIT("..\..\design\JvTimerListEditor.pas");
USEUNIT("..\..\design\JvChangeNotifyEditor.pas");
USEUNIT("..\..\design\JvFormPropertiesForm.pas");
USEUNIT("..\..\design\JvMinMaxForm.pas");
USEUNIT("..\..\design\JvPerfStatEditor.pas");
USEUNIT("..\..\design\JvSystemReg.pas");
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
