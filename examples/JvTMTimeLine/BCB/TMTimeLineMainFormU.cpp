/******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 att users dott sourceforge dott net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

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

#include "TMTimeLineMainFormU.h"
#include "frmMemoEdit.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
#pragma link "JvTMTimeLine"
TTMTimeLineMainForm *TMTimeLineMainForm;
//---------------------------------------------------------------------------
__fastcall TTMTimeLineMainForm::TTMTimeLineMainForm(TComponent* Owner)
        : TForm(Owner)
{
  ForceCurrentDirectory = true;
  JvTimeLine1               = new TJvTMTimeline(this);
  JvTimeLine1->Parent       = this;
  JvTimeLine1->PopupMenu    = popTimeLine;
  JvTimeLine1->OnChange     = DoDateChange;
  JvTimeLine1->OnClick      = DoClick;
  JvTimeLine1->OnDblClick   = DoDblClick;
  JvTimeLine1->Images       = il16;
  JvTimeLine1->Align        = alClient;
  JvTimeLine1->Hint         = "Double-click a date to edit it's memo content.\n\rRight-click to display pop-up menu.";
  dtpSelDate->Date     = Date();
  dtpFirstDate->Date   = Date()-7;
  dtpImageDate->Date   = Date()+7;
  udDayWidth->Position = JvTimeLine1->DayWidth;
  chkReadOnly->Checked = JvTimeLine1->ReadOnly;
  JvTimeLine1->Date    = dtpFirstDate->Date;
  JvTimeLine1->SelDate = dtpSelDate->Date;
  lbObjFontStyle->Checked[2] = true;
  for(int i = 0;i<il16->Count;++i)
  {
    TListItem* pLI=lvImages->Items->Add();
    pLI->ImageIndex = i;
    pLI->Caption = IntToStr(i);
  }
  Splitter1->Top = JvTimeLine1->Height + 5;
}
//---------------------------------------------------------------------------
void __fastcall TTMTimeLineMainForm::udScrollSmallClick(TObject *Sender,
      TUDBtnType Button)
{
  JvTimeLine1->SmallChange = udScrollSmall->Position;        
}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::DoDateChange(TObject* Sender)
{
  dtpFirstDate->Date = JvTimeLine1->Date;
  StatusBarResize(Sender);
}


void __fastcall TTMTimeLineMainForm::DoClick(TObject* Sender)
{
  dtpSelDate->Date   = JvTimeLine1->SelDate;
  dtpImageDate->Date = JvTimeLine1->SelDate;
}


void __fastcall TTMTimeLineMainForm::DoDblClick(TObject* Sender)
{
  mnuEditMemo->Click();
}

void __fastcall TTMTimeLineMainForm::StatusBarResize(TObject *Sender)
{
  StatusBar->Panels->Items[0]->Text = Format("Visible days: %d",OPENARRAY(TVarRec,(JvTimeLine1->VisibleDays)));
  StatusBar->Panels->Items[1]->Text = Format("Last visible date: %s",OPENARRAY(TVarRec,(DateToStr(JvTimeLine1->LastVisibleDate))));

}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::mnuEditMemoClick(TObject *Sender)
{
 TStringList *S;
 int i;
 TIcon *Ico;

// WARNING: if you store integers or other ordinal values in the Objects array
// you will get an AV if you call the ClearObjects method:
//  JvTimeLine1.Objects[JvTimeLine1.Date] := TObject(Random(100));

  S = dynamic_cast<TStringList* >( JvTimeLine1->Objects[JvTimeLine1->SelDate]);
  // here's a trick: extract the image from the imagelist and assign t to the icon property of the form:
  i = JvTimeLine1->ImageIndex[JvTimeLine1->SelDate];
  if( i > -1 )
  {
    Ico = new TIcon();
    il16->GetIcon(i,Ico);
  }
  else
  {
    Ico = NULL;
  }
  if( S == NULL )
  {
    S =  new TStringList();
  }
  // TIP: add a class function to dialogs that you show modally.
  // That way, you can keep all the creating and freeing stuff in the
  // dialog unit instead of in the calling unit.
  // This reduces the dialog call to a one-liner:
  TMemoEditFrm_Edit(S,JvTimeLine1->SelDate,Ico); // the Edit function automatically updates S if the user clicked OK in the dialog
  AnsiString sTmp = Trim(S->Text);
  if( sTmp.Length() == 0 )
  { // there is no text, so free the stringlist to conserve memory
    delete S;
    S = NULL;
  }
  JvTimeLine1->Objects[JvTimeLine1->SelDate] = S; // either way, store the value (nil or TStringlist)
  // if Objects[JvTimeLine1.Date] has a non-nil value, the day number is underlined for that date
  delete Ico;

}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::mnuInsertImageClick(TObject *Sender)
{
  JvTimeLine1->ImageIndex[JvTimeLine1->SelDate] = udImageNo->Position;
}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::mnuRemoveImageClick(TObject *Sender)
{
  JvTimeLine1->ImageIndex[JvTimeLine1->SelDate] = -1;
}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::mnuTodayClick(TObject *Sender)
{
  JvTimeLine1->Date = Date() - JvTimeLine1->VisibleDays / 2;
}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::mnuGotoDateClick(TObject *Sender)
{
  JvTimeLine1->Date = JvTimeLine1->SelDate - (JvTimeLine1->VisibleDays / 2);
}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::udScrollLargeClick(TObject *Sender,
      TUDBtnType Button)
{
  JvTimeLine1->LargeChange = udScrollLarge->Position;
}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::udDayWidthClick(TObject *Sender,
      TUDBtnType Button)
{
  JvTimeLine1->DayWidth = udDayWidth->Position;
  udDayWidth->Position  = JvTimeLine1->DayWidth;
  StatusBarResize(Sender);
}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::udPenWidthClick(TObject *Sender,
      TUDBtnType Button)
{
  JvTimeLine1->Selection->Pen->Width = udPenWidth->Position;
}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::udButtonWidthClick(TObject *Sender,
      TUDBtnType Button)
{
  JvTimeLine1->ButtonWidth = udButtonWidth->Position;
  StatusBarResize(Sender);
}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::chkReadOnlyClick(TObject *Sender)
{
  JvTimeLine1->ReadOnly = chkReadOnly->Checked;
  StatusBarResize(Sender);
}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::chkFlatClick(TObject *Sender)
{
  TBorderStyle cBStyle[2] = {bsSingle,bsNone};

  JvTimeLine1->BorderStyle = cBStyle[chkFlat->Checked];

}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::chkRClickClick(TObject *Sender)
{
  JvTimeLine1->RightClickSelect = chkRClick->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::chkEnabledClick(TObject *Sender)
{
  JvTimeLine1->Enabled = chkEnabled->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::chkShowTodayClick(TObject *Sender)
{
  JvTimeLine1->ShowToday = chkShowToday->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::chkShowTodayIconClick(TObject *Sender)
{
  JvTimeLine1->ShowTodayIcon = chkShowTodayIcon->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::chkShowWeeksClick(TObject *Sender)
{
  JvTimeLine1->ShowWeeks = chkShowWeeks->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::chkShowMonthsClick(TObject *Sender)
{
  JvTimeLine1->ShowMonths = chkShowMonths->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::btnColorClick(TObject *Sender)
{
  TColorDialog *pCD;
  pCD = new TColorDialog(NULL);


  try
  {
    pCD->Color = JvTimeLine1->Color;
    if( pCD-> Execute() )
    {
      JvTimeLine1->Color = pCD->Color;
    }
  }
  __finally
  {
    delete pCD;
  }

}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::btnLineColorClick(TObject *Sender)
{
  TColorDialog *pCD;
  pCD = new TColorDialog(NULL);


  try
  {
    pCD->Color = JvTimeLine1->LineColor;;
    if( pCD-> Execute() )
    {
      JvTimeLine1->LineColor = pCD->Color;
    }
  }
  __finally
  {
    delete pCD;
  }
}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::btnFontClick(TObject *Sender)
{
  TFontDialog* pFD;
  pFD = new TFontDialog(NULL);
  try
  {
    pFD->Font = JvTimeLine1->Font;
    if( pFD->Execute() )
    {
      JvTimeLine1->Font = pFD->Font;
    }
  }
  __finally
  {
    delete pFD;
  }

}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::btnTodayColorClick(TObject *Sender)
{
  TColorDialog* pCD;
  pCD = new TColorDialog(NULL);

  try
  {
    pCD->Color = JvTimeLine1->TodayColor;
    if( pCD->Execute() )
    {
      JvTimeLine1->TodayColor = pCD->Color;
    }
  }
  __finally
  {
    delete pCD;
  }

}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::btnMonthFontClick(TObject *Sender)
{
  TFontDialog* pFD;
  pFD = new TFontDialog(NULL);
  try
  {
    pFD->Font = JvTimeLine1->MonthFont;
    if( pFD->Execute() )
    {
      JvTimeLine1->MonthFont = pFD->Font;
    }
  }
  __finally
  {
    delete pFD;
  }
}
//---------------------------------------------------------------------------


void __fastcall TTMTimeLineMainForm::btnPenColorClick(TObject *Sender)
{
  TColorDialog* pCD;
  pCD = new TColorDialog(NULL);

  try
  {
    pCD->Color = JvTimeLine1->Selection->Pen->Color;
    if( pCD->Execute() )
    {
      JvTimeLine1->Selection->Pen->Color = pCD->Color;
    }
  }
  __finally
  {
    delete pCD;
  }
}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::btnLoadClick(TObject *Sender)
{
  TOpenDialog* pOD;
  pOD = new TOpenDialog(NULL);
  try
  {
    pOD->Filter = "Timeline files|*.tm|All files|*.*";
    pOD->DefaultExt = "TM";
    if( pOD->Execute() )
    {
      JvTimeLine1->OnReadObject = DoObjectLoad;
      JvTimeLine1->LoadFromFile(pOD->FileName);
    }
  }
  __finally
  {
    delete pOD;
  }

}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::DoObjectLoad(TObject* Sender, TStream* Stream, TObject *&AObject)
{
 AnsiString S;
 int Count;

  // Get the length of the string:
  Stream->Read(&Count,sizeof(Count));
  S.SetLength(Count);
  // need we read any more ?
  if( Count > 0 )
  {
    Stream->Read(&S[1],Count);
    AObject = new TStringList();
    dynamic_cast<TStringList* >(AObject)->Text = S;
  }
}

void __fastcall TTMTimeLineMainForm::DoObjectSave(TObject * Sender, TStream * Stream,const TObject * AObject)
{
 AnsiString S;
 int Count;

  TStringList *pSL = (TStringList *)(AObject);

  S = pSL->Text;
  Count = S.Length();
  // save length of string
  Stream->Write(&Count,sizeof(Count));
  // need we store anything ?
  if( Count > 0 )
  {
    Stream->Write(&S[1],Count);
  }
}

void __fastcall TTMTimeLineMainForm::btnSaveClick(TObject *Sender)
{
  TSaveDialog* pSD;
  pSD = new TSaveDialog(NULL);
  try
  {
    pSD->Filter = "Timeline files|*.tm|All files|*.*";
    pSD->DefaultExt = "TM";
    if( pSD->Execute() )
    {
      JvTimeLine1->OnWriteObject = DoObjectSave;
      JvTimeLine1->SaveToFile(pSD->FileName);
    }
  }
  __finally
  {
    delete pSD;
  }

}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::btnAddClick(TObject *Sender)
{
  JvTimeLine1->ImageIndex[dtpImageDate->DateTime] = udImageNo->Position;
}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::dtpFirstDateChange(TObject *Sender)
{
  JvTimeLine1->Date = dtpFirstDate->Date;
}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::dtpSelDateChange(TObject *Sender)
{
  JvTimeLine1->SelDate = dtpSelDate->Date;
}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::lbObjFontStyleClickCheck(
      TObject *Sender)
{
  TFontStyles F;

  F = TFontStyles() >> fsBold >> fsItalic >> fsUnderline >> fsStrikeOut;

    if( lbObjFontStyle->Checked[0])
    {
      F = F + TFontStyles() << fsBold;
    }
    if( lbObjFontStyle->Checked[1] )
    {
      F = F + TFontStyles() << fsItalic ;
    }
    if( lbObjFontStyle->Checked[2] )
    {
      F = F + TFontStyles() << fsUnderline;
    }
    if( lbObjFontStyle->Checked[3] )
    {
      F = F + TFontStyles() << fsStrikeOut;
    }

  JvTimeLine1->ObjectsFontStyle = F;

}
//---------------------------------------------------------------------------

// just update the controls when an item in the listview is clicked
void __fastcall TTMTimeLineMainForm::lvImagesSelectItem(TObject *Sender,
      TListItem *Item, bool Selected)
{
  if( /*Assigned(Item)*/(Item!=NULL) && Selected )
  {
    udImageNo->Position = Item->ImageIndex;
  }
}
//---------------------------------------------------------------------------

// Free any stringlists still around in the Objects array by calling the ClearObjects method
// Do NOT call this method unless the Objects array actually contains
// TObjects (or descendants): calling this method if you store ordinal values
// (like integers), will cause an AV
// You can freely mix object types in the array: the will be freed correctly anyway
void __fastcall TTMTimeLineMainForm::FormCloseQuery(TObject *Sender,
      bool &CanClose)
{
  JvTimeLine1->ClearObjects();
}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::FormMouseWheelDown(TObject *Sender,
      TShiftState Shift, TPoint &MousePos, bool &Handled)
{
  if(  !JvTimeLine1->Focused()
      &&
       ( ControlAtPos(ScreenToClient(MousePos),false,true) == JvTimeLine1 /*TJvTMTimeline*/ )
     )
  {
    Handled = true;
    if( Shift.Contains(ssCtrl) )
    {
       JvTimeLine1->ScrollDate(this,-udScrollSmall->Position);
    }
    else
    {
       JvTimeLine1->ScrollDate(this,-udScrollLarge->Position);
    }
  }

}
//---------------------------------------------------------------------------

void __fastcall TTMTimeLineMainForm::FormMouseWheelUp(TObject *Sender,
      TShiftState Shift, TPoint &MousePos, bool &Handled)
{
  if(  !JvTimeLine1->Focused()
      &&
       ( ControlAtPos(ScreenToClient(MousePos),false,true) == JvTimeLine1 /*TJvTMTimeline*/ )
     )
  {
    Handled = true;
    if( Shift.Contains(ssCtrl) )
    {
      JvTimeLine1->ScrollDate(this,udScrollSmall->Position);
    }
    else
    {
      JvTimeLine1->ScrollDate(this,udScrollLarge->Position);
    }
  }

}
//---------------------------------------------------------------------------

