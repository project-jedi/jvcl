//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvCustomC5D.res");
USEUNIT("..\..\design\JvTimeLineEditor.pas");
USEUNIT("..\..\design\JvCustomReg.pas");
USEUNIT("..\..\design\JvHLEditEditor.pas");
USEUNIT("..\..\design\JvLookoutEditor.pas");
USEUNIT("..\..\design\JvOutlookBarEditors.pas");
USEUNIT("..\..\design\JvOutlookBarForm.pas");
USEUNIT("..\..\design\JvScheduleEditorForm.pas");
USEUNIT("..\..\design\JvScheduleEditors.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("dclstd50.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("JvCoreC5D.bpi");
USEPACKAGE("JvStdCtrlsC5R.bpi");
USEPACKAGE("JvCustomC5R.bpi");
USEPACKAGE("dsnide50.bpi");
USEPACKAGE("JVCOREC5R.bpi");
USEPACKAGE("JVSYSTEMC5R.bpi");
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
