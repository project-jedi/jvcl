//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\run\JvBDECheckPasswordForm.pas", Jvbdecheckpasswordform, JvChPswdForm);
USEFORMNS("..\..\run\JvBDEExceptionForm.pas", Jvbdeexceptionform, JvBdeErrorDlg);
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
