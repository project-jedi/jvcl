//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("B4_RAControls.res");
USEFORMNS("fRAControls.pas", Fracontrols, MainForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TMainForm), &MainForm);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
