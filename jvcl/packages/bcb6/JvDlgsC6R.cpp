//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\run\JvDualListForm.pas", Jvduallistform, JvDualListForm);
USEFORMNS("..\..\run\JvExceptionForm.pas", Jvexceptionform, JvErrorDialog);
USEFORMNS("..\..\run\JvImageForm.pas", Jvimageform, FormImg);
USEFORMNS("..\..\run\JvLoginForm.pas", Jvloginform, JvLoginForm);
USEFORMNS("..\..\run\JvProgressForm.pas", Jvprogressform, frmProgress);
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
