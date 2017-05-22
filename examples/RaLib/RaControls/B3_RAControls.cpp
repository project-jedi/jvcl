//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("B3_RAControls.res");
USEFORMNS("fRAControls.pas", Fracontrols, Form1);
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
