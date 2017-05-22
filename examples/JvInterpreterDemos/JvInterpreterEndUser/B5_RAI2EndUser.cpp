//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("B5_JvInterpreterEndUser.res");
USEFORMNS("Unit1.pas", Unit1, DataModule1); /* TDataModule: File Type */
USEFORMNS("Unit2.pas", Unit2, Form2);
USEFORMNS("fReports.pas", Freports, Reports);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
    Application->Initialize();
    Application->CreateForm(__classid(TDataModule1), &DataModule1);
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

