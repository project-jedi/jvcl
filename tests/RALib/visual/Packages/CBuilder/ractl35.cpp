//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("ractl35.res");
USEPACKAGE("vcl35.bpi");
USEUNIT("JvScrollMax.pas");
USEUNIT("JvComponentPanel.pas");
USEUNIT("JvCtlConst.pas");
USEUNIT("JvEditor.pas");
USEUNIT("JvHint.pas");
USEUNIT("JvHLEditor.pas");
USEUNIT("JvHLEdPropDlg.pas");
USEUNIT("JvHLParser.pas");
USEUNIT("JvHooks.pas");
USEUNIT("JvHtControls.pas");
USEUNIT("JvRegAuto.pas");
USEUNIT("JvButtons.pas");
USEUNIT("JvaScrollText.pas");
USEUNIT("JvStrUtil.pas");
USEUNIT("JvDlg.pas");
USEUNIT("JvDsgnIntf.pas");
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
