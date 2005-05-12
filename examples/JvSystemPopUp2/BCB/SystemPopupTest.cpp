//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("SystemPopupTest.res");
USEFORM("JvSystemPopup2MainFormU.cpp", JvSystemPopup2MainForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TJvSystemPopup2MainForm), &JvSystemPopup2MainForm);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
