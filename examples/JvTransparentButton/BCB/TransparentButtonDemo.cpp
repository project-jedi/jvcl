//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("TransparentButtonDemo.res");
USEFORM("TransBtnFormMainU.cpp", TransBtnFormMain);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TTransBtnFormMain), &TransBtnFormMain);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
