//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvCmpC5D.res");
USEUNIT("..\..\design\JvPatcherEditor.pas");
USEUNIT("..\..\design\JvCmpReg.pas");
USEUNIT("..\..\design\JvDataEmbeddedEditor.pas");
USEUNIT("..\..\design\JvPageManagerForm.pas");
USEUNIT("..\..\run\JvPatchForm.pas");
USEPACKAGE("JvCoreC5D.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("dclstd50.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("JvCmpC5R.bpi");
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
