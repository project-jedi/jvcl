//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvWizardC5D.res");
USEUNIT("..\..\design\JvWizardReg.pas");
USEUNIT("..\..\design\JvWizardAboutInfoForm.pas");
USEUNIT("..\..\design\JvWizardEditorForm.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("JvWizardC5R.bpi");
USEPACKAGE("JvCoreC5R.bpi");
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
