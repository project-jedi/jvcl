//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\design\JvGradientCaptionForm.pas", Jvgradientcaptionform, GradCaptionsEditor);
USEFORMNS("..\..\run\JvWallpaperEditForm.pas", Jvwallpapereditform, FoWallpaperChooser);
USEPACKAGE("");
USEPACKAGE("dsnide50.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("JvAppFrmC5R.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vclx50.bpi");
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
