//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvCoreC5D.res");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("JvCoreC5R.bpi");
USEUNIT("..\..\design\JvStringsForm.pas");
USEUNIT("..\..\design\JvBaseDlgEditor.pas");
USEUNIT("..\..\design\JvColorEditor.pas");
USEUNIT("..\..\design\JvCoreReg.pas");
USEUNIT("..\..\design\JvDataContextManagerForm.pas");
USEUNIT("..\..\design\JvDataProviderEditors.pas");
USEUNIT("..\..\design\JvDataProviderItemDesign.pas");
USEUNIT("..\..\design\JvDesignerUtils.pas");
USEUNIT("..\..\design\JvDsgnConsts.pas");
USEUNIT("..\..\design\JvDsgnEditors.pas");
USEUNIT("..\..\design\JvIDEZoom.pas");
USEUNIT("..\..\design\JvJVCLAboutEditor.pas");
USEUNIT("..\..\design\JvPaintBoxEditor.pas");
USEUNIT("..\..\design\JvProviderToolbarFrame.pas");
USEUNIT("..\..\design\JvProviderTreeListFrame.pas");
USEUNIT("..\..\design\JvBaseDsgnForm.pas");
USEUNIT("..\..\design\JvBaseDsgnFrame.pas");
USEUNIT("..\..\design\JvBaseDsgnToolbarFrame.pas");
USEUNIT("..\..\design\JvDataConsumerContextSelectForm.pas");
USEUNIT("..\..\design\JvDataConsumerItemSelectForm.pas");
USEUNIT("..\..\design\JvDataProviderDesignerForm.pas");
USEUNIT("..\..\design\JvDateTimeForm.pas");
USEUNIT("..\..\design\JvStdToolbarDsgnFrame.pas");
USEUNIT("..\..\design\JvColorProviderEditors.pas");
USEUNIT("..\..\design\JvColorProviderDesignerForm.pas");
USEUNIT("..\..\design\JvProviderTreeListDsgnFrame.pas");
USEPACKAGE("dclstd50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("");
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
