//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\design\JvCompEditorTemplateForm.pas", Jvcompeditortemplateform, JvgCompEditorTemplate);
USEFORMNS("..\..\design\JvgComponentListEditorForm.pas", Jvgcomponentlisteditorform, JvgCompListEditor);
USEFORMNS("..\..\design\JvgLabelEditorForm.pas", Jvglabeleditorform, JvgLabelEditorDlg);
USEFORMNS("..\..\design\JvgLogicItemEditorForm.pas", Jvglogicitemeditorform, JvgLogicItemEditor);
USEFORMNS("..\..\design\JvgLogicsEditorForm.pas", Jvglogicseditorform, JvgLogicsEditor);
USEFORMNS("..\..\design\JvgMultiResourceEditorForm.pas", Jvgmultiresourceeditorform, JvgMultipleResourceEdit);
USEFORMNS("..\..\design\JvgReportEditorForm.pas", Jvgreporteditorform, JvgReportEditorForm);
USEFORMNS("..\..\design\JvgReportParamEditorForm.pas", Jvgreportparameditorform, JvgReportParamEditor);
USEFORMNS("..\..\design\JvgReportParamsForm.pas", Jvgreportparamsform, JvgReportParamsForm);
USEFORMNS("..\..\design\JvgRTFPreviewForm.pas", Jvgrtfpreviewform, JvgRTFPreview);
USEPACKAGE("");
USEPACKAGE("dsnide50.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("JvGlobusC5R.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("vclsmp.bpi");
USEPACKAGE("JvStdCtrlsC5R.bpi");
USEPACKAGE("JvCoreC5D.bpi");
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
