//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\run\JvYearGridEditForm.pas", Jvyeargrideditform, YearGridEditF);
USEFORMNS("..\..\run\JvGridPreviewForm.pas", Jvgridpreviewform, JvGridPreviewF);
USEFORMNS("..\..\run\JvPainterEffectsForm.pas", Jvpaintereffectsform, PainterEffectsF);
USEFORMNS("..\..\run\JvPainterQBForm.pas", Jvpainterqbform, PainterQBF);
USEFORMNS("..\..\run\JvQuickPreviewForm.pas", Jvquickpreviewform, QuickPreviewF);
USEFORMNS("..\..\run\JvSpellerForm.pas", Jvspellerform, JvSpellerFrm);
USEPACKAGE("");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("JvCoreC5R.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("vcljpg50.bpi");
USEPACKAGE("vclsmp.bpi");
USEPACKAGE("bcbsmp50.bpi");
USEPACKAGE("bcbie50.bpi");
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
