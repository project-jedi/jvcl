//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\design\JvVirtualKeyEditorForm.pas", Jvvirtualkeyeditorform, frmJvVirtualKeyEditor);
USEFORMNS("..\..\design\JvDirectoryListForm.pas", Jvdirectorylistform, JvDirectoryListDialog);
USEFORMNS("..\..\design\JvIconListForm.pas", Jviconlistform, IconListDialog);
USEFORMNS("..\..\design\JvID3v2DefineForm.pas", Jvid3v2defineform, JvID3DefineDlg);
USEFORMNS("..\..\design\JvID3v2EditorForm.pas", Jvid3v2editorform, JvID3FramesEditor);
USEFORMNS("..\..\design\JvImagePreviewForm.pas", Jvimagepreviewform, ImageForm);
USEFORMNS("..\..\design\JvPictureEditForm.pas", Jvpictureeditform, PictureEditDialog);
USEPACKAGE("");
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
