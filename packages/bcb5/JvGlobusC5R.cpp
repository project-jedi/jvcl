//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\run\JvgAlignForm.pas", Jvgalignform, AlignForm);
USEFORMNS("..\..\run\JvgCheckVersionInfoForm.pas", Jvgcheckversioninfoform, JvgfCheckVersionInfo);
USEFORMNS("..\..\run\JvgQPrintPreviewForm.pas", Jvgqprintpreviewform, JvgfPrintPreview);
USEFORMNS("..\..\run\JvgQPrintSetupForm.pas", Jvgqprintsetupform, JvgPrintSetup);
USEPACKAGE("");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("db");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("qrpt.bpi");
USEPACKAGE("vclie.bpi");
USEPACKAGE("bde");
USEPACKAGE("JvStdCtrlsC5R.bpi");
USEPACKAGE("JvSystemC5R.bpi");
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
