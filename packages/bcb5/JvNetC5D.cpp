//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\run\JvParserForm.pas", Jvparserform, FormParsers);
USEPACKAGE("dsnide50.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("JvNetC5R.bpi");
USEPACKAGE("");
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
