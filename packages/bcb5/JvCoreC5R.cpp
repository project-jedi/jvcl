//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvCoreC5R.res");
USEUNIT("..\..\common\JvConsts.pas");
USEUNIT("..\..\run\JVCLVer.pas");
USEUNIT("..\..\run\JvComponent.pas");
USEUNIT("..\..\run\JvVersionInfo.pas");
USEUNIT("..\..\run\JvTypes.pas");
USEUNIT("..\..\run\JvJCLUtils.pas");
USEUNIT("..\..\run\JvMaxPixel.pas");
USEUNIT("..\..\run\JvProgressUtils.pas");
USEUNIT("..\..\run\JvBaseDlg.pas");
USEUNIT("..\..\run\JvActions.pas");
USEUNIT("..\..\run\JvMouseTimer.pas");
USEUNIT("..\..\run\JvDsgnIntf.pas");
USEUNIT("..\..\run\JvDataProvider.pas");
USEUNIT("..\..\run\JvDataProviderImpl.pas");
USEUNIT("..\..\run\JvContextProvider.pas");
USEUNIT("..\..\run\JvAppStore.pas");
USEUNIT("..\..\run\JvAppRegistryStore.pas");
USEUNIT("..\..\run\JvAppIniStore.pas");
USEUNIT("..\..\run\JvJVCLAboutForm.pas");
USEUNIT("..\..\design\JvActnResForm.pas");
USEUNIT("..\..\run\JvJVCLUtils.pas");
USEUNIT("..\..\run\JvThemes.pas");
USEUNIT("..\..\run\JvColorProvider.pas");
USEUNIT("..\..\run\JvClxUtils.pas");
USEPACKAGE("cjcl50.bpi");
USEPACKAGE("bcbsmp50.bpi");
USEPACKAGE("Vcl50.bpi");
USEPACKAGE("vcljpg50.bpi");
USEPACKAGE("Vclx50.bpi");
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

