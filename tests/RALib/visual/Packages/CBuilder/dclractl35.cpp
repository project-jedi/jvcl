//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("dclractl35.res");
USEPACKAGE("vcl35.bpi");
USEUNIT("JvaCtlReg.pas");
USEUNIT("JvIDEZoom.pas");
USEFORMNS("JvHTHintEditor.pas", JvHTHintEditor, HintEditor);
USEUNIT("JvaDsgn.pas");
USEFORMNS("JvRegAutoEditor.pas", JvRegAutoEditor, RegEditor);
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
    return 1;
}
//---------------------------------------------------------------------------
