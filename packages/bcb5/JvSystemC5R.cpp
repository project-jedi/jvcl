//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

USERES("JvSystemC5R.res");
USEPACKAGE("JvCoreC5R.bpi");
USEPACKAGE("CJCL50.bpi");
USEUNIT("..\..\run\JvXmlDatabase.pas");
USEUNIT("..\..\run\JvCaret.pas");
USEUNIT("..\..\run\JvChangeNotify.pas");
USEUNIT("..\..\run\JvClipboardMonitor.pas");
USEUNIT("..\..\run\JvClipboardViewer.pas");
USEUNIT("..\..\run\JvCommStatus.pas");
USEUNIT("..\..\run\JvComputerInfo.pas");
USEUNIT("..\..\run\JvDdeCmd.pas");
USEUNIT("..\..\run\JvDeviceChanged.pas");
USEUNIT("..\..\run\JvDirectories.pas");
USEUNIT("..\..\run\JvDragDrop.pas");
USEUNIT("..\..\run\JvFormPlacement.pas");
USEUNIT("..\..\run\JvHidControllerClass.pas");
USEUNIT("..\..\run\JvHook.pas");
USEUNIT("..\..\run\JvJoystick.pas");
USEUNIT("..\..\run\JvKeyboardStates.pas");
USEUNIT("..\..\run\JvMRUList.pas");
USEUNIT("..\..\run\JvMRUManager.pas");
USEUNIT("..\..\run\JvNTEventLog.pas");
USEUNIT("..\..\run\JvPerfMon95.pas");
USEUNIT("..\..\run\JvPropsStorage.pas");
USEUNIT("..\..\run\JvRas32.pas");
USEUNIT("..\..\run\JvScreenResolution.pas");
USEUNIT("..\..\run\JvScreenSaver.pas");
USEUNIT("..\..\run\JvSearchFiles.pas");
USEUNIT("..\..\run\JvShellHook.pas");
USEUNIT("..\..\run\JvSHFileOperation.pas");
USEUNIT("..\..\run\JvSimpleXml.pas");
USEUNIT("..\..\run\JvSoundControl.pas");
USEUNIT("..\..\run\JvSystemColors.pas");
USEUNIT("..\..\run\JvThread.pas");
USEUNIT("..\..\run\JvThreadTimer.pas");
USEUNIT("..\..\run\JvTimer.pas");
USEUNIT("..\..\run\JvTimerList.pas");
USEUNIT("..\..\run\JvWndProcHook.pas");
USEUNIT("..\..\run\JvAppInst.pas");
USEUNIT("..\..\common\Ras32.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("vcljpg50.bpi");
USEPACKAGE("bcbsmp50.bpi");
USEPACKAGE("bcbie50.bpi");


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

