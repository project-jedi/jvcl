//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvCmpC5R.res");
USEUNIT("..\..\run\JvWinHelp.pas");
USEUNIT("..\..\run\JvAlarms.pas");
USEUNIT("..\..\run\JvConverter.pas");
USEUNIT("..\..\run\JvCreateProcess.pas");
USEUNIT("..\..\run\JvDataEmbedded.pas");
USEUNIT("..\..\run\JvEasterEgg.pas");
USEUNIT("..\..\run\JvEnterTab.pas");
USEUNIT("..\..\run\JvMergeManager.pas");
USEUNIT("..\..\run\JvMouseGesture.pas");
USEUNIT("..\..\run\JvPageManager.pas");
USEUNIT("..\..\run\JvPatchFile.pas");
USEUNIT("..\..\run\JvPrint.pas");
USEUNIT("..\..\run\JvProfilerForm.pas");
USEUNIT("..\..\run\JvStringHolder.pas");
USEUNIT("..\..\run\JvTimeLimit.pas");
USEUNIT("..\..\run\JvTranslator.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("bcbsmp50.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("JvSystemC5R.bpi");
USEPACKAGE("JvCoreC5R.bpi");
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
