//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvDlgsC5D.res");
USEUNIT("..\..\design\JvTipOfDayEditor.pas");
USEUNIT("..\..\design\JvCommonDialogDEditor.pas");
USEUNIT("..\..\design\JvAppletEditor.pas");
USEUNIT("..\..\design\JvDlgsReg.pas");
USEPACKAGE("Vcl50.bpi");
USEPACKAGE("dsnide50.bpi");
USEPACKAGE("JvDlgsC5R.bpi");
USEPACKAGE("JvCoreC5D.bpi");



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

