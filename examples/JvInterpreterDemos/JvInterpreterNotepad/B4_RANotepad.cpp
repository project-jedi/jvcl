//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("B4_RANotepad.res");
USEFORMNS("fMain.pas", Fmain, Main);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TMain), &Main);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
