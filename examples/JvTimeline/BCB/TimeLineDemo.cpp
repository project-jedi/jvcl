//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("TimeLineDemo.res");
USEFORM("TimelineNotesFormU.cpp", TimelineNotesForm);
USEFORM("TimeLineMainFormU.cpp", TimelineMainForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TTimelineMainForm), &TimelineMainForm);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
