//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvWizardC5R.res");
USEUNIT("..\..\run\JvWizardRouteMapSteps.pas");
USEUNIT("..\..\run\JvWizard.pas");
USEUNIT("..\..\run\JvWizardCommon.pas");
USEUNIT("..\..\run\JvWizardRouteMapNodes.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("JvCoreC5R.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("bcbsmp50.bpi");
USEPACKAGE("bcbie50.bpi");

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

