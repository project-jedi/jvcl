//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("ractl53.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("JvStrUtil.pas");
USEUNIT("JvComponentPanel.pas");
USEUNIT("JvCtlConst.pas");
USEUNIT("JvDlg.pas");
USEUNIT("JvDsgnIntf.pas");
USEUNIT("JvEditor.pas");
USEUNIT("JvHint.pas");
USEUNIT("JvHLEditor.pas");
USEFORMNS("JvHLEdPropDlg.pas", JvHLEdPropDlg, RAHLEditorParamsForm);
USEUNIT("JvHLParser.pas");
USEUNIT("JvHooks.pas");
USEUNIT("JvHtControls.pas");
USEUNIT("JvRegAuto.pas");
USEUNIT("JvScrollMax.pas");
USEUNIT("JvaScrollText.pas");
USEUNIT("JvButtons.pas");
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
