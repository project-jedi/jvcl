//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("ShFileOpDemo.res");
USEFORM("JvShFileOperationMainFormU.cpp", JvShFileOperationMainForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TJvShFileOperationMainForm), &JvShFileOperationMainForm);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
