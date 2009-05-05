/******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):
   korecek: translation from Delphi to BCB

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************/
// $Id$
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "OLBarMaonFormU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvExControls"
#pragma link "JvOutlookBar"
#pragma resource "*.dfm"
TOLBarMainForm *OLBarMainForm;
//---------------------------------------------------------------------------
__fastcall TOLBarMainForm::TOLBarMainForm(TComponent* Owner)
        : TForm(Owner)
{
   RichEdit1->WordWrap = true;
}
//---------------------------------------------------------------------------
void __fastcall TOLBarMainForm::Button1Click(TObject *Sender)
{
  JvOutlookBar1->LargeImages = ImageList1;
  JvOutlookBar1->SmallImages = ImageList2;
}
//---------------------------------------------------------------------------
void __fastcall TOLBarMainForm::Button2Click(TObject *Sender)
{
  JvOutlookBar1->LargeImages = NULL;
  JvOutlookBar1->SmallImages = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TOLBarMainForm::Button3Click(TObject *Sender)
{
 int i;
 TFontDialog *FD;
  FD = new TFontDialog(NULL);
  __try
  {
    if( !chkButtonFont->Checked )
    {
      FD->Font = JvOutlookBar1->Font;
    }
    else
    {
      FD->Font = JvOutlookBar1->ActivePage->Font;
    }
    if( FD->Execute() )
    {
      if( !chkButtonFont->Checked )
      {
        JvOutlookBar1->Font = FD->Font; // this sets the font of all the pages
      }
      else
      {
        for( i = 0;i<JvOutlookBar1->Pages->Count; ++i)
        {
          JvOutlookBar1->Pages->Items[i]->Font = FD->Font; // this sets the button's fonts!
        }
      }
    }
  }  
  __finally
  {
    delete FD;
  }

}
//---------------------------------------------------------------------------
void __fastcall TOLBarMainForm::JvOutlookBar1ButtonClick(TObject *Sender, int Index)
{
 TJvOutlookBarPage *P;
  
  if (Index > -1)
  {
    P = JvOutlookBar1->Pages->Items[JvOutlookBar1->ActivePageIndex ];
    Caption = Format("Clicked button %s on page %s",OPENARRAY(TVarRec,( P->Buttons->Items[Index]->Caption, P->Caption ) ) );
  }

}
//---------------------------------------------------------------------------
void __fastcall TOLBarMainForm::JvOutlookBar1PageChange(TObject *Sender,
      int Index)
{
  if( Index > -1 )
  {
    Caption = Format("Page changed to %s",OPENARRAY(TVarRec,( JvOutlookBar1->Pages->Items[Index]->Caption) ) );
  }
}
//---------------------------------------------------------------------------
void __fastcall TOLBarMainForm::JvOutlookBar1PageChanging(TObject *Sender,
      int Index, bool &AllowChange)
{
//  with JvOutlookBar1 do
    if( (JvOutlookBar1->ActivePageIndex > -1) && (Index > -1) )
    {
      Caption = Format("Page changing from %s to %s",
        OPENARRAY(TVarRec,( JvOutlookBar1->Pages->Items[JvOutlookBar1->ActivePageIndex ]->Caption,
                            JvOutlookBar1->Pages->Items[Index]->Caption ) ) );
    }
}
//---------------------------------------------------------------------------
void __fastcall TOLBarMainForm::JvOutlookBar1ContextPopup(TObject *Sender, TPoint &MousePos,
                                                          bool &Handled)
{
 TJvOutlookBar *pJLOB;
 TJvOutlookBarPage *pOLBP;
 TJvOutlookBarButton *pOLBB;

  pJLOB = dynamic_cast<TJvOutlookBar *>(Sender);
  if(pJLOB!=NULL)
  {
 // with Sender as TJvOutlookbar do
   pOLBP = dynamic_cast< TJvOutlookBarPage *>(pJLOB->PopUpObject);
   if( pOLBP != 0 )
   {
     pJLOB->PopupMenu = popPage;
   }
   else
   {
     pOLBB = dynamic_cast<TJvOutlookBarButton *>(pJLOB->PopUpObject);
     if( pOLBB != 0 )
     {
       pJLOB->PopupMenu = popButton;
     }
     else
     {
       pJLOB->PopupMenu = popOL;
     }
   }
  }
}
//---------------------------------------------------------------------------

void __fastcall TOLBarMainForm::acSmallButtonsExecute(TObject *Sender)
{
  acSmallButtons->Checked = !acSmallButtons->Checked;
  JvOutlookBar1->ButtonSize = acSmallButtons->Checked?olbsSmall:olbsLarge;
}
//---------------------------------------------------------------------------

void __fastcall TOLBarMainForm::chkFlatClick(TObject *Sender)
{
  JvOutlookBar1->BorderStyle = chkFlat->Checked?bsNone:bsSingle;
}
//---------------------------------------------------------------------------

void __fastcall TOLBarMainForm::acEditButtonCaptionExecute(TObject *Sender)
{
  TJvOutlookBarButton *pOLBB;
  pOLBB = dynamic_cast<TJvOutlookBarButton *>( JvOutlookBar1->PopUpObject );
  if( pOLBB != 0 )
  {
    pOLBB->EditCaption();
  }
}
//---------------------------------------------------------------------------

void __fastcall TOLBarMainForm::acEditPageCaptionExecute(TObject *Sender)
{
  TJvOutlookBarPage *pOLBP;
  pOLBP = dynamic_cast<TJvOutlookBarPage *>(JvOutlookBar1->PopUpObject);
  if( pOLBP != 0 )
  {
    pOLBP->EditCaption();
  }
}
//---------------------------------------------------------------------------

