//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvNetC5D.res");
USEUNIT("..\..\design\JvNetReg.pas");
USEUNIT("..\..\design\JvHtmlParserEditor.pas");
USEUNIT("..\..\design\JvMailEditor.pas");
USEUNIT("..\..\run\JvParserForm.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("JvNetC5R.bpi");
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
