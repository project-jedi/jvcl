//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEPACKAGE("vcl50.bpi");
USEUNIT("JvaDsgn.pas");
USEFORMNS("JvRegAutoEditor.pas", JvRegAutoEditor, RegEditor);
USEFORMNS("JvHTHintEditor.pas", JvHTHintEditor, HintEditor);
USEUNIT("JvIDEZoom.pas");
USEUNIT("JvaCtlReg.pas");
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
