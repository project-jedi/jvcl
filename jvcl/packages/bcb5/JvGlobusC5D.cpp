//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvGlobusC5D.res");
USEUNIT("..\..\design\JvgShadowEditor.pas");
USEUNIT("..\..\design\JvCompEditorTemplateForm.pas");
USEUNIT("..\..\design\JvgCompDescription.pas");
USEUNIT("..\..\design\JvgComponentListEditorForm.pas");
USEUNIT("..\..\design\JvgHelpPanelEditor.pas");
USEUNIT("..\..\design\JvgLabelEditorForm.pas");
USEUNIT("..\..\design\JvgLogicItemEditorForm.pas");
USEUNIT("..\..\design\JvgLogicsEditorForm.pas");
USEUNIT("..\..\design\JvgMultiResourceEditorForm.pas");
USEUNIT("..\..\design\JvgMultiResources.pas");
USEUNIT("..\..\design\JvgPropertyCenter.pas");
USEUNIT("..\..\design\JvgReportEditorForm.pas");
USEUNIT("..\..\design\JvgReportParamEditorForm.pas");
USEUNIT("..\..\design\JvgReportParamsForm.pas");
USEUNIT("..\..\design\JvgRTFPreviewForm.pas");
USEUNIT("..\..\design\JvGlobusReg.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("vcljpg50.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("JvGlobusC5R.bpi");
USEPACKAGE("vclsmp50.bpi");
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
