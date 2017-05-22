//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USEFORM("MainForm.cpp", frmMain);
USEFORM("WelcomeForm.cpp", frmWelcome);
USEFORM("JvFormsForm.cpp", frmJvForms);
USEFORM("WallpaperForm.cpp", frmWallpaper);
USEFORM("AnimatedTitleForm.cpp", frmAnimatedTitle);
USEFORM("TransparentForm.cpp", frmTransparent);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->CreateForm(__classid(TfrmMain), &frmMain);
     Application->CreateForm(__classid(TfrmTransparent), &frmTransparent);
     Application->Run();
  }
  catch (Exception &exception)
  {
     Application->ShowException(&exception);
  }
  catch (...)
  {
     try
     {
       throw Exception("");
     }
     catch (Exception &exception)
     {
       Application->ShowException(&exception);
     }
  }
  return 0;
}
//---------------------------------------------------------------------------
