//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("B4_DBMove.res");
USEFORMNS("fDBMove.pas", Fdbmove, Form1);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TForm1), &Form1);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
