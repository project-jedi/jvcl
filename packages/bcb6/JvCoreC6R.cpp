//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\run\JvJVCLAboutForm.pas", Jvjvclaboutform, JvJVCLAboutForm);
USEFORMNS("..\..\design\JvActnResForm.pas", Jvactnresform, JvStandardActions); /* TDataModule: File Type */
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
 