//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("DockOptionDemo_CB.res");
USEFORMNS("MainForm.pas", Mainform, Main_Form);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TMain_Form), &Main_Form);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
