//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("B4_JvInterpreterTest.res");
USEFORMNS("fJvInterpreterTest.pas", FJvInterpretertest, Test);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TTest), &Test);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
