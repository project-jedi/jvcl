//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\run\JvJVCLAboutForm.pas", Jvjvclaboutform, JvJVCLAboutForm);
USEFORMNS("..\..\design\JvActnResForm.pas", Jvactnresform, JvStandardActions); /* TDataModule: File Type */
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("CJCL50.bpi");
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
 
