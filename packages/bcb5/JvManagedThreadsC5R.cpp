//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvManagedThreadsC5R.res");
USEUNIT("..\..\run\JvMTThreading.pas");
USEUNIT("..\..\run\JvMTComponents.pas");
USEUNIT("..\..\run\JvMTConsts.pas");
USEUNIT("..\..\run\JvMTData.pas");
USEUNIT("..\..\run\JvMTSync.pas");
USEUNIT("..\..\run\JvMTSyncMon.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("JvCoreC5R.bpi");
USEPACKAGE("CJCL50.bpi");

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

