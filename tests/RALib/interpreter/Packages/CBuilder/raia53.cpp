//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEPACKAGE("Vcl50.bpi");
USEPACKAGE("Vcldb50.bpi");
USEPACKAGE("VCLBDE50.bpi");
USEPACKAGE("Qrpt50.bpi");
USEPACKAGE("ractl53.bpi");
USEPACKAGE("rai53.bpi");
USEUNIT("JvInterpreter_Windows.pas");
USEUNIT("JvInterpreter_Classes.pas");
USEUNIT("JvInterpreter_ComCtrls.pas");
USEUNIT("JvInterpreter_Contnrs.pas");
USEUNIT("JvInterpreter_Controls.pas");
USEUNIT("JvInterpreter_Db.pas");
USEUNIT("JvInterpreter_DbCtrls.pas");
USEUNIT("JvInterpreter_DbGrids.pas");
USEUNIT("JvInterpreter_DBTables.pas");
USEUNIT("JvInterpreter_Dialogs.pas");
USEUNIT("JvInterpreter_ExtCtrls.pas");
USEUNIT("JvInterpreter_Forms.pas");
USEUNIT("JvInterpreter_Graphics.pas");
USEUNIT("JvInterpreter_Grids.pas");
USEUNIT("JvInterpreter_Menus.pas");
USEUNIT("JvInterpreter_Quickrpt.pas");
USEUNIT("JvInterpreter_JvEditor.pas");
USEUNIT("JvInterpreter_JvInterpreter.pas");
USEUNIT("JvInterpreter_JvRegAuto.pas");
USEUNIT("JvInterpreter_JvUtils.pas");
USEUNIT("JvInterpreter_StdCtrls.pas");
USEUNIT("JvInterpreter_System.pas");
USEUNIT("JvInterpreter_SysUtils.pas");
USEUNIT("JvInterpreter_all.pas");
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
