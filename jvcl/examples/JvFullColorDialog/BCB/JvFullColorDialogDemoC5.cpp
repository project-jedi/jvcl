//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvFullColorDialogDemoC5.res");
USEFORM("MainForm.cpp", frmMain);
USELIB("VCLSMP50.LIB");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->CreateForm(__classid(TfrmMain), &frmMain);
     Application->Run();
  }
  catch (Exception &exception)
  {
     Application->ShowException(&exception);
  }
  return 0;
}
//---------------------------------------------------------------------------
