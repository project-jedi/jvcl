//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\run\JvYearGridEditForm.pas", Jvyeargrideditform, YearGridEditF);
USEFORMNS("..\..\run\JvGridPreviewForm.pas", Jvgridpreviewform, JvGridPreviewF);
USEFORMNS("..\..\run\JvPainterEffectsForm.pas", Jvpaintereffectsform, PainterEffectsF);
USEFORMNS("..\..\run\JvPainterQBForm.pas", Jvpainterqbform, PainterQBF);
USEFORMNS("..\..\run\JvQuickPreviewForm.pas", Jvquickpreviewform, QuickPreviewF);
USEFORMNS("..\..\run\JvSpellerForm.pas", Jvspellerform, JvSpellerFrm);
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
