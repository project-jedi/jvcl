//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvAppFrmC5D.res");
USEUNIT("..\..\design\JvGradientCaptionForm.pas");
USEUNIT("..\..\design\JvAppFrmReg.pas");
USEUNIT("..\..\design\JvFormWallpaperEditor.pas");
USEUNIT("..\..\run\JvWallpaperEditForm.pas");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("JvAppFrmC5R.bpi");
USEPACKAGE("vcl50.bpi");
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
