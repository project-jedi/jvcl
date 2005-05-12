//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvShellHookDemo.res");
USEFORM("JvShellHookDemoMainFormU.cpp", JvShellHookDemoMainForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TJvShellHookDemoMainForm), &JvShellHookDemoMainForm);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
