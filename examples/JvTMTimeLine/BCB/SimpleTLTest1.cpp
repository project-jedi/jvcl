//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("SimpleTLTest1.res");
USEFORM("TMTimeLineMainFormU.cpp", TMTimeLineMainForm);
USEFORM("frmMemoEdit.cpp", MemoEditFrm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TTMTimeLineMainForm), &TMTimeLineMainForm);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
