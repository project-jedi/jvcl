//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\design\JvWizardAboutInfoForm.pas", Jvwizardaboutinfoform, JvWizardAboutDialog);
USEFORMNS("..\..\design\JvWizardEditorForm.pas", Jvwizardeditorform, JvWizardPageListEditor);
USEPACKAGE("");
USEPACKAGE("dsnide50.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("JvWizardC5R.bpi");
USEPACKAGE("vclx50.bpi");
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
