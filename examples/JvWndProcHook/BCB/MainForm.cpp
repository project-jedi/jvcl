//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::DoButtonClick(TObject *Sender)
{
  Caption = Format("Button %s clicked", OPENARRAY(TVarRec, ( dynamic_cast<TJvCaptionButton *>(Sender)->Caption ) ) )  ;
}

void __fastcall TfrmMain::btnAddClick(TObject *Sender)
{
 TJvCaptionButton *B;
 AnsiString sAS;
 int uiBtnCount;
  B = new TJvCaptionButton(this);
  B->OnClick = DoButtonClick;
  B->Caption =  sAS.sprintf("%c", ('A' + FButtonCount));
  /* This part is a little different from original Delphi code example */
  uiBtnCount = lbButtons->Items->Count;
  if(uiBtnCount>=30)
  {
   uiBtnCount=30;
  }

  B->ButtonLeft = -15;
  B->ButtonLeft = B->ButtonLeft*uiBtnCount + B->ButtonWidth + 2;

  /*
     Here is supported 255 buttons only. When we add more buttons then
     UniqueName function will throw an exception. The exception is not
     solved here !!! Keep in your mind, it is an example only.
  */
  B->Name = UniqueName("JvCaptionButton");

  lbButtons->Items->AddObject(B->Caption,B);
  ++FButtonCount;

}
//---------------------------------------------------------------------------


AnsiString __fastcall TfrmMain::UniqueName(const AnsiString BaseName)
{
 int i;
 AnsiString asResult;
  i = 1;
  asResult = BaseName + IntToStr(i);
  while( FindComponent(asResult) != NULL )
  {
    ++i;
    asResult = BaseName + IntToStr(i);
    if( i > 255 )
    {
      throw Exception("Unable to create unique name!");
    }  
  }
  return asResult;
}

void __fastcall TfrmMain::btnDeleteClick(TObject *Sender)
{
 int i;

  i = lbButtons->ItemIndex;
  if (i > -1)
  {
    delete  dynamic_cast<TJvCaptionButton *>(lbButtons->Items->Objects[i]);
    lbButtons->Items->Delete(i);
  }
  if( i >= lbButtons->Items->Count)
  {
    --i;
  }
  lbButtons->ItemIndex = i;
  if( lbButtons->Items->Count == 0 )
  {
    FButtonCount = 0;
  }

}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::btnRecreateWndClick(TObject *Sender)
{
  RecreateWnd();        
}
//---------------------------------------------------------------------------

