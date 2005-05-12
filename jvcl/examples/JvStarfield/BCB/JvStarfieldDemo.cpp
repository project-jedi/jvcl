//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvStarfieldDemo.res");
USEFORM("StarFieldMain.cpp", StarfieldMainForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TStarfieldMainForm), &StarfieldMainForm);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
