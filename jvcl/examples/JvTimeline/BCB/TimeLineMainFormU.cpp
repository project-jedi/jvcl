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

#include "TimeLineMainFormU.h"
#include "TimelineNotesFormU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvComponent"
#pragma link "JvExControls"
#pragma link "JvTimeLine"
#pragma resource "*.dfm"

TTimelineMainForm *TimelineMainForm;
//---------------------------------------------------------------------------
__fastcall TTimelineMainForm::TTimelineMainForm(TComponent* Owner)
        : TForm(Owner)
{
  FCurColor                 = TimeLine1->Color;
  cbDragging->ItemIndex     = 0;
  TimelineNotesForm         = new TTimelineNotesForm(NULL);
  cbDraggingChange(NULL);
  TimeLine1->ShowSelection  = false;
  TimeLine1->DoubleBuffered = false;
}
//---------------------------------------------------------------------------

__fastcall TTimelineMainForm::~TTimelineMainForm(void)
{
  int i;

  delete TimelineNotesForm;
 // { free allocated memory }
  for( i = 0;i<TimeLine1->Items->Count;++i)
  {
    if( TimeLine1->Items->Items[i]->Data != NULL)
    {
      delete (pItemData)TimeLine1->Items->Items[i]->Data;
      TimeLine1->Items->Items[i]->Data = NULL;
    }
  }

}

void __fastcall TTimelineMainForm::cbDraggingChange(TObject *Sender)
{
  TimeLine1->DragLine = false;
  switch(cbDragging->ItemIndex)
  {
   case 0:
   case 1:
    {
       TimeLine1->DragMode = dmManual;
    }break;
   case 2:
    {
       TimeLine1->DragMode = dmAutomatic;
    }break;
  }

}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::FormResize(TObject *Sender)
{
  TimeLine1->Height = Height/2;
  if( Panel2->Height < 250)
  {
    TimeLine1->Height = Height - 300;
  }

}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::btnColorClick(TObject *Sender)
{
  TColorDialog* pCD;
  pCD = new TColorDialog(NULL);

    pCD->Color = FCurColor;
    if( pCD->Execute() )
    {
      FCurColor = pCD->Color;
    }
    delete pCD;

}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::btnAddClick(TObject *Sender)
{
  TJvTimeItem* aItem;

  aItem             = TimeLine1->Items->Add();
  aItem->Caption    = edCaption->Text;
  aItem->ImageIndex = StrToIntDef(edImIndex->Text, -1);
  aItem->Level      = StrToIntDef(edLevel->Text, 0);
  aItem->Date       = dtpItemDate->Date;
  aItem->Color      = FCurColor;
  if( FCurColor != clWhite )
  {
    aItem->TextColor = clWhite;
  }
  else
  {
    aItem->TextColor = clBlack;
  }

}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::btnAutoClick(TObject *Sender)
{
  TimeLine1->BeginUpdate();
  TimeLine1->AutoLevels(chkComplete->Checked, chkReset->Checked);
  TimeLine1->EndUpdate();
}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::chkMonthsClick(TObject *Sender)
{
  TimeLine1->ShowMonthNames = chkMonths->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::chkMultiClick(TObject *Sender)
{
  TimeLine1->MultiSelect = chkMulti->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::chkNoImagesClick(TObject *Sender)
{
  if( chkNoImages->Checked )
  {

    TimeLine1->Images = NULL;
    TimeLine1->ItemHeight = 16;
    udItemHeight->Position = 16;
  }
  else
  {
    chkLargeClick(NULL);
  }

}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::chkAutoSizeClick(TObject *Sender)
{
   TimeLine1->AutoSize     = chkAutoSize->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::chkSupportClick(TObject *Sender)
{
   TimeLine1->VertSupports = chkSupport->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::chkLargeClick(TObject *Sender)
{
  if( chkNoImages->Checked )
  {
    return;
  }
  if( chkLarge->Checked )
  {
    TimeLine1->Images      = ImageList2;
    TimeLine1->ItemHeight  = 50;
    udItemHeight->Position = 50;
  }
  else
  {
    TimeLine1->Images      = ImageList1;
    TimeLine1->ItemHeight  = 36;
    udItemHeight->Position = 36;
  }

}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::chkOwnerDrawClick(TObject *Sender)
{
  if( chkOwnerDraw->Checked )
  {
    chkAutoSize->Checked = false;
    TimeLine1->Style     = tlOwnerDrawFixed;
  }
  else
  {
    TimeLine1->Style     = tlDefault;
  }
}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::chkFlatClick(TObject *Sender)
{
  TimeLine1->Flat = chkFlat->Checked;

}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::udYrSizeClick(TObject *Sender,
      TUDBtnType Button)
{
   TimeLine1->YearWidth = udYrSize->Position;
}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::udItemHeightClick(TObject *Sender,
      TUDBtnType Button)
{
   TimeLine1->ItemHeight = udItemHeight->Position;
}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::dtpFirstDateChange(TObject *Sender)
{
   TimeLine1->FirstVisibleDate   = dtpFirstDate->Date;
   dtpFirstDate->Date            = TimeLine1->FirstVisibleDate;
}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::btnFontClick(TObject *Sender)
{
  TFontDialog* pFD;
  pFD = new TFontDialog(NULL);

    pFD->Font = TimeLine1->Font;
    if( pFD->Execute() )
    {
      TimeLine1->Font = pFD->Font;
    }
    delete pFD;

}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::btnYrFontClick(TObject *Sender)
{
  TFontDialog* pFD;
  pFD = new TFontDialog(NULL);

    pFD->Font = TimeLine1->YearFont;
    if( pFD->Execute() )
    {
      TimeLine1->YearFont = pFD->Font;
    }
    
    delete pFD;

}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::ColorBtnClick(TObject *Sender)
{
  TColorDialog* pCD;
  pCD = new TColorDialog(NULL);

    pCD->Color = TimeLine1->Color;
    if( pCD->Execute() )
    {
      TimeLine1->Color = pCD->Color;
    }

    delete pCD;

}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::chkHelpYearClick(TObject *Sender)
{
  TimeLine1->HelperYears = chkHelpYear->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::btnSaveClick(TObject *Sender)
{
  TSaveDialog* pSD;
  pSD = new TSaveDialog(this);

    pSD->Filter = "Item files (*.itm) |*.itm| All files (*.*)|*.*";
    pSD->InitialDir = ExtractFilePath(Application->ExeName);
    if( pSD->Execute() )
    {
      TimeLine1->BeginUpdate();
      TimeLine1->SaveToFile(pSD->FileName);
      TimeLine1->EndUpdate();
    }
    delete pSD;

}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::btnLoadClick(TObject *Sender)
{
  TOpenDialog* pOD;
  pOD = new TOpenDialog(this);

    pOD->Filter = "Item files (*.itm) |*.itm| All files (*.*)|*.*";
    pOD->InitialDir = ExtractFilePath(Application->ExeName);

    if( pOD->Execute() )
    {
      TimeLine1->BeginUpdate();
      for(int i = 0;i<TimeLine1->Items->Count;++i)
      {
        if( TimeLine1->Items->Items[i]->Data != NULL )
        {
          delete (pItemData)TimeLine1->Items->Items[i]->Data;
          TimeLine1->Items->Items[i]->Data = NULL;
        }
      }
      TimeLine1->Items->Clear();
      TimeLine1->LoadFromFile(pOD->FileName);
      TimeLine1->EndUpdate();
    }

    delete pOD;

}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::TimeLine1ItemClick(TObject *Sender,
      TJvTimeItem *Item)
{
  Caption = Item->Caption;        
}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::TimeLine1MouseMove(TObject *Sender,
      TShiftState Shift, int X, int Y)
{
  StatusBar1->Panels->Items[0]->Text = DateToStr(TimeLine1->DateAtPos(X)) + " (approx.)";
}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::TimeLine1MouseDown(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
{
  if( (Button == mbLeft) && (cbDragging->ItemIndex == 1) )
  {
    TimeLine1->BeginDrag(Mouse->DragImmediate, Mouse->DragThreshold);
  }
}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::TimeLine1Click(TObject *Sender)
{
  if( TimeLine1->Selected != NULL )
  {                                
    Caption = Format("%s (%s)",
      OPENARRAY(TVarRec, ( TimeLine1->Selected->Caption, DateToStr(TimeLine1->Selected->Date) ) ) );
  }
}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::TimeLine1DragDrop(TObject *Sender,
      TObject *Source, int X, int Y)
{
  AnsiString S;

  if( (Sender == Source) && (TimeLine1->Selected != NULL ) )
  {
    S = DateToStr(TimeLine1->DateAtPos(X));

    if( InputQuery("Confirm move", Format("Move \"%s\" to new date:", OPENARRAY(TVarRec, (TimeLine1->Selected->Caption) ) ), S ) )
    {
      TimeLine1->Selected->Date  = StrToDate(S);
      TimeLine1->Selected->Level = TimeLine1->LevelAtPos(Y);
    }
  }

}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::TimeLine1DragOver(TObject *Sender,
      TObject *Source, int X, int Y, TDragState State, bool &Accept)
{
  Accept = (Sender == Source);
}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::TimeLine1DrawItem(TObject *Sender,
      TCanvas *Canvas, TJvTimeItem *Item, TRect &R)
{
 AnsiString S;
  //  Canvas.Brush.Color := clBlack;
  //  Canvas.FrameRect(R);

  //  Canvas.Brush.Color := RGB(Random(255),Random(255),Random(255));
  Canvas->FillRect(R);
  if( TimeLine1->Images != NULL )
  {

    TimeLine1->Images->Draw(Canvas, R.Left, R.Top, Item->ImageIndex, True);
    {
       if( Item->Selected )
       {
          TimeLine1->Images->Draw(Canvas,R.Left,R.Top,random(TimeLine1->Images->Count),True);
       }
       else
       {
          TimeLine1->Images->Draw(Canvas,R.Right - TimeLine1->Images->Width,R.Top,Item->ImageIndex, True);
       }
    }
  }

  //  if (Random > 0.5) and Item.Selected then
  //    S := strrev(Item.Caption)
  //  else
  S = Item->Caption;
  
  //  Canvas.Font.Color := Canvas.Brush.Color xor clWhite;
  S = " " + S;
  DrawText(Canvas->Handle,  S.c_str(), -1, &R, (DT_LEFT | DT_BOTTOM | DT_SINGLELINE));

}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::TimeLine1ItemMoved(TObject *Sender,
      TJvTimeItem *Item, TDateTime &NewStartDate, int &NewLevel)
{
 AnsiString S;

  if( TimeLine1->Dragging() )
  {
     return;
  }

  S = DateToStr(NewStartDate);
  if( !InputQuery("Confirm move", Format("Move \"%s\" to new date:", OPENARRAY(TVarRec ,(Item->Caption) ) ), S) )
  {
     NewStartDate = Item->Date;
     NewLevel     = Item->Level;
  }
}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::TimeLine1LoadItem(TObject *Sender,
      TJvTimeItem *Item, TStream *Stream)
{
  AnsiString S;
  pItemData aData;
  char ch;

  Stream->Read(&ch, 1);

  while( ch != 27 /* ESC */)
  {
    S = S + ch;
    Stream->Read(&ch, 1);
  }

  if (S != "@@@") // then { nothing there }
  {
    aData = new TItemData();
    aData->Text = S;
    Item->Data = aData;
  }

}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::Changecaption1Click(TObject *Sender)
{
 AnsiString S;

  if( TimeLine1->Selected != NULL)
  {
    S = TimeLine1->Selected->Caption;
    if( InputQuery("Change caption", "Change caption to:", S) )
    {
      TimeLine1->Selected->Caption = S;
    }
  }

}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::Move1Click(TObject *Sender)
{
  AnsiString S;

  if( TimeLine1->Selected != NULL )
  {
    S = DateToStr(TimeLine1->Selected->Date);
    if( InputQuery("Move item", "Move to new date:", S) )
    {
      TimeLine1->Selected->Date = StrToDate(S);
    }
  }
}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::remove1Click(TObject *Sender)
{
  if( TimeLine1->Selected != NULL )
  {
    if( MessageDlg("Sure you want to delete this item?", mtConfirmation, TMsgDlgButtons() << mbYes << mbNo, 0) == mrYes )
    {
      if( TimeLine1->Selected->Data != NULL )
      {
        delete (pItemData)TimeLine1->Selected->Data;
      }
      TimeLine1->Selected->Remove();
    }
  }
}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::Disable1Click(TObject *Sender)
{
  if( TimeLine1->Selected != NULL )
  {
    TimeLine1->Selected->Enabled = !TimeLine1->Selected->Enabled;
  }
}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::Up1Click(TObject *Sender)
{
  if( (TimeLine1->Selected != NULL) && (TimeLine1->Selected->Level > 0) )
  {
    TimeLine1->Selected->Level = TimeLine1->Selected->Level - 1;
  }
  else
  {
     if( TimeLine1->Selected != NULL )
     {
       ShowMessage("Can't move this item further up!");
     }
  }
}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::Down1Click(TObject *Sender)
{
  TimeLine1->Selected->Level = TimeLine1->Selected->Level + 1;
}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::Notes1Click(TObject *Sender)
{
 pItemData aData;
 TPoint P;

  if( TimeLine1->Selected != NULL )
  {
    if( TimeLine1->Selected->Data == NULL )
    {
      aData = new TItemData();
      aData->Text = "";
    }
    else
    {
      aData = (pItemData)TimeLine1->Selected->Data;
    }
    TimelineNotesForm->Caption = Format("Notes for \"%s\":", OPENARRAY(TVarRec, (TimeLine1->Selected->Caption)) );
    TimelineNotesForm->Memo1->Text = aData->Text;
    GetCursorPos(&P);
    TimelineNotesForm->Left = P.x - 12;
    TimelineNotesForm->Top  = P.y - 32;
    TimelineNotesForm->ShowModal();
    aData->Text = TimelineNotesForm->Memo1->Text;
    TimeLine1->Selected->Data = aData;
  }

}
//---------------------------------------------------------------------------

void __fastcall TTimelineMainForm::chkWidthAsClick(TObject *Sender)
{
 const TJvTimeItemType aType[2]={asPixels, asDays};

  for(int i = 0;i<TimeLine1->Items->Count;++i)
  {
    TimeLine1->Items->Items[i]->WidthAs = aType[chkWidthAs->Checked];
  }
}
//---------------------------------------------------------------------------

