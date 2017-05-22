//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("TipsDemo.res");
USEFORM("TipOfDayMainFormU.cpp", TipOfDayMainForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TTipOfDayMainForm), &TipOfDayMainForm);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
