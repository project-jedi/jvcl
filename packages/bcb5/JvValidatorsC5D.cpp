//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\design\JvValidatorsEditorForm.pas", Jvvalidatorseditorform, frmValidatorsEditor);
USEPACKAGE("JvCoreC5R.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("");
USEPACKAGE("dsnide50.bpi");
USEPACKAGE("dclstd50.bpi");
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

