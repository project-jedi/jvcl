//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\design\JvStringsForm.pas", Jvstringsform, JvStrEditDlg);
USEFORMNS("..\..\design\JvProviderTreeListFrame.pas", Jvprovidertreelistframe, fmeJvProviderTreeList); /* TFrame: File Type */
USEFORMNS("..\..\design\JvBaseDsgnForm.pas", Jvbasedsgnform, JvBaseDesign);
USEFORMNS("..\..\design\JvBaseDsgnFrame.pas", Jvbasedsgnframe, fmeJvBaseDesign); /* TFrame: File Type */
USEFORMNS("..\..\design\JvBaseDsgnToolbarFrame.pas", Jvbasedsgntoolbarframe, fmeJvBaseToolbarDesign); /* TFrame: File Type */
USEFORMNS("..\..\design\JvDataConsumerContextSelectForm.pas", Jvdataconsumercontextselectform, frmDataConsumerContextSelect);
USEFORMNS("..\..\design\JvDataConsumerItemSelectForm.pas", Jvdataconsumeritemselectform, frmJvDataConsumerItemSelect);
USEFORMNS("..\..\design\JvDataProviderDesignerForm.pas", Jvdataproviderdesignerform, frmDataProviderDesigner);
USEFORMNS("..\..\design\JvDateTimeForm.pas", Jvdatetimeform, frmSelectDateTimeDlg);
USEFORMNS("..\..\design\JvStdToolbarDsgnFrame.pas", Jvstdtoolbardsgnframe, fmeJvStdToolbarDesign); /* TFrame: File Type */
USEFORMNS("..\..\design\JvColorProviderDesignerForm.pas", Jvcolorproviderdesignerform, frmJvColorProviderDesigner);
USEFORMNS("..\..\design\JvProviderTreeListDsgnFrame.pas", Jvprovidertreelistdsgnframe, fmeJvProviderTreeListDsgn); /* TFrame: File Type */
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
