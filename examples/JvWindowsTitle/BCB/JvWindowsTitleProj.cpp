//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvWindowsTitleProj.res");
USEFORM("JvWindowsTitleMainFormU.cpp", JvWindowsTitleMainForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TJvWindowsTitleMainForm), &JvWindowsTitleMainForm);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
