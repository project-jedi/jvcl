//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvHMIC5R.res");
USEUNIT("..\..\run\JvSLDMappingEditorDialog.pas");
USEUNIT("..\..\run\JvDialButton.pas");
USEUNIT("..\..\run\JvLED.pas");
USEUNIT("..\..\run\JvSegmentedLEDDisplay.pas");
USEUNIT("..\..\run\JvSegmentedLEDDisplayMapperFrame.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("CJCL50.bpi");
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
