//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\design\JvWizardAboutInfoForm.pas", Jvwizardaboutinfoform, JvWizardAboutDialog);
USEFORMNS("..\..\design\JvWizardEditorForm.pas", Jvwizardeditorform, JvWizardPageListEditor);
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
