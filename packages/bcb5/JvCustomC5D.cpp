//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\design\JvOutlookBarForm.pas", Jvoutlookbarform, FrmOLBEditor);
USEFORMNS("..\..\design\JvScheduleEditorForm.pas", Jvscheduleeditorform, FrmScheduleEditor);
USEPACKAGE("");
USEPACKAGE("dsnide50.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("dclstd50.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("JvCoreC5D.bpi");
USEPACKAGE("JvStdCtrlsC5R.bpi");
USEPACKAGE("JvCustomC5R.bpi");
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
