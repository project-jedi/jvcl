//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Profiler32MainFormU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvComponentBase"
#pragma link "JvProfilerForm"
#pragma resource "*.dfm"
TProfiler32MainForm *Profiler32MainForm;
//---------------------------------------------------------------------------
__fastcall TProfiler32MainForm::TProfiler32MainForm(TComponent* Owner)
        : TForm(Owner)
{
  P->Names = ListBox1->Items;
  P->Sorted = true;
  P->Enabled = true;
  FTerminated = false;
  DefCaption = "JvProfiler 32 Test program";
#if ( SET_DECIMAL_SEPARATOR > 0)
  DecimalSeparator   = '.';
#endif
}
//---------------------------------------------------------------------------
void __fastcall TProfiler32MainForm::FormClose(TObject *Sender,
      TCloseAction &Action)
{
  FTerminated = true;
  P->Enabled = false;
  P->Stop();

}
//---------------------------------------------------------------------------
void __fastcall TProfiler32MainForm::FormKeyDown(TObject *Sender,
      WORD &Key, TShiftState Shift)
{
  if( Key == 27 ) /* test for ESCAPE key */
  {
     FTerminated = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TProfiler32MainForm::UseIdBtnClick(TObject *Sender)
{
 int i,j,k;

  FTerminated = false;
  randomize();
  P->Start();
  //{ make distributed randomize to get some results }
  Screen->Cursor = crHourGlass;
  UseIdBtn->Enabled   = false;
  UseNameBtn->Enabled = false;
  ResultBtn->Enabled  = false;
  try
  {
    k = random(100);
    Progress->Max = k;
    for( j = 0;j<=k;++j )
    {
      Progress->Position = j;
      Caption = Format("%s - to do: %d", OPENARRAY(TVarRec, (DefCaption.c_str(),(k-j)) ) );
      i = random(ListBox1->Items->Count);
      // { use string ID instead }
      P->EnterName(P->Names->Strings[i]);
      ::SleepEx(10 * j,false);
      P->ExitName(P->Names->Strings[i]);
      Application->ProcessMessages();
      if( FTerminated )
      {
        break;
      }  
    }
  }
  __finally
  {
    Screen->Cursor      = crDefault;
    UseIdBtn->Enabled   = true;
    UseNameBtn->Enabled = true;
    ResultBtn->Enabled  = true;
  }
  P->Stop();
  Beep();
  Progress->Position = 0;

}
//---------------------------------------------------------------------------
void __fastcall TProfiler32MainForm::UseNameBtnClick(TObject *Sender)
{
 int i,j,k;

  FTerminated = false;
  randomize();
  //{ just randomize to get some results }
  Screen->Cursor      = crHourGlass;
  UseIdBtn->Enabled   = false;
  UseNameBtn->Enabled = false;
  ResultBtn->Enabled  = false;
  P->Start();
  try
  {
    k = random(133);
    Progress->Max = k;
    for( j = 0;j<= k; ++j)
    {
      Progress->Position = j;
      Caption = Format("%s - to do: %d", OPENARRAY(TVarRec, (DefCaption.c_str(),(k-j)) ) );
      i = random(ListBox1->Items->Count);
      //{ use integer ID (Names[i] ID = i) }
      P->EnterID(i);
      SleepEx(random(333),false);
      P->ExitID(i);
      Application->ProcessMessages();
      if( FTerminated )
      {
        break;
      }
    }
  }
  __finally
  {
    Screen->Cursor      = crDefault;
    UseIdBtn->Enabled   = true;
    UseNameBtn->Enabled = true;
    ResultBtn->Enabled  = true;
  }
  P->Stop();
  Beep();
  Progress->Position = 0;

}
//---------------------------------------------------------------------------
void __fastcall TProfiler32MainForm::ResultBtnClick(TObject *Sender)
{
  /*
     If you have some exception problems with sorting in result
     it can be casued by different decimal separators.
     In this case you should search for SET_DECIMAL_SEPARATOR
     in this module and set your own decimal separator.
  */
  P->ShowReport();
}
//---------------------------------------------------------------------------
