//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEFORM("fThread.cpp", Form1);
USEUNIT("..\..\..\run\JvThread.pas");
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
