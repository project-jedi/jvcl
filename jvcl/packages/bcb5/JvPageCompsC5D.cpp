//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\design\JvTreeItemsEditorForm.pas", Jvtreeitemseditorform, frmTreeViewItems);
USEFORMNS("..\..\design\JvPageLinkEditorForm.pas", Jvpagelinkeditorform, frmJvTreeViewLinksEditor);
USEPACKAGE("");
USEPACKAGE("dsnide50.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vclx50.bpi");
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
