//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvPrintPreviewC5R.res");
USEUNIT("..\..\run\JvPrvwRender.pas");
USEUNIT("..\..\run\JvPrvwDoc.pas");
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

