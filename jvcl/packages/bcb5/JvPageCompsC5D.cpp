//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvPageCompsC5D.res");
USEUNIT("..\..\design\JvTreeItemsEditorForm.pas");
USEUNIT("..\..\design\JvPageLinkEditorForm.pas");
USEUNIT("..\..\design\JvPageListTreeViewReg.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("dclstd50.bpi");
USEPACKAGE("JvPageCompsC5R.bpi");
USEPACKAGE("JvCoreC5D.bpi");
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
