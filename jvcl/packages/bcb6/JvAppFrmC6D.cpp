//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\design\JvGradientCaptionForm.pas", Jvgradientcaptionform, GradCaptionsEditor);
USEFORMNS("..\..\run\JvWallpaperEditForm.pas", Jvwallpapereditform, FoWallpaperChooser);
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
