//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\design\JvPageManagerForm.pas", Jvpagemanagerform, JvProxyEditor);
USEFORMNS("..\..\run\JvPatchForm.pas", Jvpatchform, FoPatch);
USEPACKAGE("");
USEPACKAGE("JvCoreC5D.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("dsnide50.bpi");
USEPACKAGE("dclstd50.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("JvCmpC5R.bpi");
USEPACKAGE("JvStdCtrlsC5R.bpi");
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
