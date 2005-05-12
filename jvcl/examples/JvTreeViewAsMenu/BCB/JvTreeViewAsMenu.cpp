//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvTreeViewAsMenu.res");
USEFORM("JvTreeViewAsMenuMainFormU.cpp", JvTreeViewAsMenuMainForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TJvTreeViewAsMenuMainForm), &JvTreeViewAsMenuMainForm);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
