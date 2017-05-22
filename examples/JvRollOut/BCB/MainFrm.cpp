//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MainFrm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvComponent"
#pragma link "JvExExtCtrls"
#pragma link "JvRollOut"
#pragma resource "*.dfm"
TForm1 *Form1;

#define COMPILER6_UP

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
        : TForm(Owner)
{
#ifdef COMPILER6_UP
  JvRollOutAction2->AutoCheck = true;
#endif
}
//---------------------------------------------------------------------------
void __fastcall TForm1::JvRollOutAction1Execute(TObject *Sender)
{
  Caption = "Action 1 executed!";

}
//---------------------------------------------------------------------------
void __fastcall TForm1::JvRollOutAction2Execute(TObject *Sender)
{
  Caption = "Action 2 executed!";
#ifndef COMPILER6_UP
  // Delphi 5 doesn't have TAction.AutoCheck
  TAction *pTA = dynamic_cast<TAction *>(Sender);
  if(pTA!=NULL)
  {
    pTA->Checked = !pTA->Checked;
  }
//  with Sender as TAction do
//    Checked := not Checked;
#endif

}
//---------------------------------------------------------------------------
void __fastcall TForm1::chkShowFocusClick(TObject *Sender)
{
  for(int i = 0;i < ComponentCount;++i)
  {
    if( Components[i]->ClassNameIs("TJvRollOut") )
    {
      dynamic_cast<TJvRollOut *>(Components[i])->ShowFocus = chkShowFocus->Checked;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::chkTabStopClick(TObject *Sender)
{
  for(int i = 0;i < ComponentCount;++i)
  {
    if( Components[i]->ClassNameIs("TJvRollOut") )
    {
      dynamic_cast<TJvRollOut *>(Components[i])->TabStop = chkTabStop->Checked;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::chkGroupIndexClick(TObject *Sender)
{
  for(int i = 0;i < ComponentCount;++i)
  {
    if( Components[i]->ClassNameIs("TJvRollOut") )
    {
      dynamic_cast<TJvRollOut *>(Components[i])->GroupIndex = static_cast<int >(chkGroupIndex->Checked);;
    }
  }

}
//---------------------------------------------------------------------------
void __fastcall TForm1::chkToggleAnywhereClick(TObject *Sender)
{
  for(int i = 0;i < ComponentCount;++i)
  {
    if( Components[i]->ClassNameIs("TJvRollOut") )
    {
      dynamic_cast<TJvRollOut *>(Components[i])->ToggleAnywhere = chkToggleAnywhere->Checked;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::chkHideButtonClick(TObject *Sender)
{

  const TColor cTopColor[2] = {clBtnHighlight, clNone};
  const TColor cBtmColor[2] = {clBtnShadow, clNone};

  for(int i = 0;i < ComponentCount;++i)
  {
    if( Components[i]->ClassNameIs("TJvRollOut") )
    {
      dynamic_cast<TJvRollOut *>(Components[i])->Colors->ButtonTop = cTopColor[chkHideButton->Checked];
      dynamic_cast<TJvRollOut *>(Components[i])->Colors->ButtonBottom = cBtmColor[chkHideButton->Checked];
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::chkHideFrameClick(TObject *Sender)
{
  const TColor cTopColor[2] = {clBtnShadow, clNone};
  const TColor cBtmColor[2] = {clBtnHighlight, clNone};
  for(int i = 0;i < ComponentCount;++i)
  {
    if( Components[i]->ClassNameIs("TJvRollOut") )
    {
      dynamic_cast<TJvRollOut *>(Components[i])->Colors->FrameTop = cTopColor[chkHideButton->Checked];
      dynamic_cast<TJvRollOut *>(Components[i])->Colors->FrameBottom= cBtmColor[chkHideButton->Checked];
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::chkImagesClick(TObject *Sender)
{
  for(int i = 0;i < ComponentCount;++i)
  {
    if( Components[i]->ClassNameIs("TJvRollOut") )
    {
      if(chkImages->Checked)
      {
        dynamic_cast<TJvRollOut *>(Components[i])->ImageOptions->Images = ImageList1;
      }
      else
      {
        dynamic_cast<TJvRollOut *>(Components[i])->ImageOptions->Images = NULL;
      }
    }
  }
}
//---------------------------------------------------------------------------
