//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\design\JvSpeedbarForm.pas", Jvspeedbarform, JvSpeedbarEditor);
USEFORMNS("..\..\design\JvHTHintForm.pas", Jvhthintform, JvHintEditor);
USEPACKAGE("");
USEPACKAGE("dsnide50.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("JvCtrlsC5R.bpi");
USEPACKAGE("dclstd50.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("bcbie50.bpi");
USEPACKAGE("bcbsmp50.bpi");
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
