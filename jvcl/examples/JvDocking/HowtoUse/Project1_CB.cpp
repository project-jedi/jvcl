//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("Project1_CB.res");
USEFORMNS("Unit1.pas", Unit1, Form1);
USEFORMNS("Unit2.pas", Unit2, Form2);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TForm1), &Form1);
                 Application->CreateForm(__classid(TForm2), &Form2);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
