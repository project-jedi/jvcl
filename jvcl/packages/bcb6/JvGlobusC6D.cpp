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
