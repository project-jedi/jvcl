//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvSystemPopupProj.res");
USEFORM("JvSystemPopupMainFormU.cpp", JvSystemPopupMainForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TJvSystemPopupMainForm), &JvSystemPopupMainForm);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
