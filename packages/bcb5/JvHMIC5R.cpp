//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\run\JvSLDMappingEditorDialog.pas", Jvsldmappingeditordialog, frmSLDMappingEditorDialog);
USEFORMNS("..\..\run\JvSegmentedLEDDisplayMapperFrame.pas", Jvsegmentedleddisplaymapperframe, fmeJvSegmentedLEDDisplayMapper); /* TFrame: File Type */
USEPACKAGE("");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("JvCoreC5R.bpi");
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
