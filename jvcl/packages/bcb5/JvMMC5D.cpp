//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvMMC5D.res");
USEUNIT("..\..\design\JvVirtualKeyEditorForm.pas");
USEUNIT("..\..\design\JvAnimatedEditor.pas");
USEUNIT("..\..\design\JvAVICaptureEditors.pas");
USEUNIT("..\..\design\JvDirectoryListForm.pas");
USEUNIT("..\..\design\JvIconListForm.pas");
USEUNIT("..\..\design\JvID3v2DefineForm.pas");
USEUNIT("..\..\design\JvID3v2EditorForm.pas");
USEUNIT("..\..\design\JvImagePreviewForm.pas");
USEUNIT("..\..\design\JvMMReg.pas");
USEUNIT("..\..\design\JvPictureEditForm.pas");
USEUNIT("..\..\design\JvPictureEditors.pas");
USEPACKAGE("dsnide50.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("JvMMC5R.bpi");
USEPACKAGE("JvStdCtrlsC5R.bpi");
USEPACKAGE("JvCmpC5R.bpi");
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
