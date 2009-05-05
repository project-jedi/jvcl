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
#include <windows>
#pragma hdrstop

#include "JvOutlookBarCustomDrawDemoMainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvComponentBase"
#pragma link "JvExControls"
#pragma link "JvNavigationPane"
#pragma link "JvOutlookBar"
#pragma resource "*.dfm"
TJvOutlookBarCustomDrawDemoMainFrm *JvOutlookBarCustomDrawDemoMainFrm;
//---------------------------------------------------------------------------
__fastcall TJvOutlookBarCustomDrawDemoMainFrm::TJvOutlookBarCustomDrawDemoMainFrm(TComponent* Owner)
        : TForm(Owner)
{
  ComboBox1->ItemIndex = 0;
//  JvOutlookBar1->OnCustomDraw = DoCustomDraw;
  ComboBox2->ItemIndex = 0;
  ComboBox1Change(ComboBox1);
  ComboBox2Change(ComboBox2);
}
//---------------------------------------------------------------------------
void __fastcall TJvOutlookBarCustomDrawDemoMainFrm::DoCustomDraw(TObject *Sender,
                    TCanvas *ACanvas, TRect &ARect, TJvOutlookBarCustomDrawStage AStage,
                    int AIndex, bool ADown, bool AInside, bool &DefaultDraw)
{
  DefaultDraw = false;
  switch(AStage)
  {
    case odsBackground:
    {
       GradientFillRect(ACanvas, ARect,
                        JvNavPaneStyleManager1->Colors->HeaderColorFrom,
                        JvNavPaneStyleManager1->Colors->HeaderColorTo,
                        fdTopToBottom, 255);
    }break;
    case odsPage:
    {
       GradientFillRect(ACanvas,ARect,
                        JvNavPaneStyleManager1->Colors->ButtonColorFrom,
                        JvNavPaneStyleManager1->Colors->ButtonColorTo,
                        fdTopToBottom, 255);
    }break;
    case odsPageButton:
    {
       GradientFillRect(ACanvas,ARect,
                        JvNavPaneStyleManager1->Colors->HeaderColorFrom,
                        JvNavPaneStyleManager1->Colors->HeaderColorTo,
                        fdTopToBottom, 255);
       if( ADown )
       {
         OffsetRect(ARect,1,1);
       }  
       ACanvas->Font->Color = clWhite;
       DrawText(ACanvas->Handle, JvOutlookBar1->Pages->Items[AIndex]->Caption.c_str(),
                JvOutlookBar1->Pages->Items[AIndex]->Caption.Length(), &ARect,
                DT_SINGLELINE | DT_VCENTER | DT_CENTER);
    }break;
    case odsButtonFrame:
    {
      if( ADown )
      {
        ACanvas->Brush->Color = clNavy;
      }
      else
      {
        ACanvas->Brush->Color = clBlack;
      }
      ACanvas->FrameRect(ARect);
      InflateRect(&ARect,-1,-1);
      if(!ADown )
      {
        ACanvas->Brush->Color = clWhite;
      }
      ACanvas->FrameRect(ARect);
    }break;
    case odsButton:
    {
      DefaultDraw = true;
    }break;
  }
}
//---------------------------------------------------------------------------
void __fastcall TJvOutlookBarCustomDrawDemoMainFrm::ComboBox1Change(TObject *Sender)
{
  JvNavPaneStyleManager1->Theme = static_cast<TJvNavPanelTheme >(ComboBox1->ItemIndex);
  JvOutlookBar1->Invalidate();

}
//---------------------------------------------------------------------------
void __fastcall TJvOutlookBarCustomDrawDemoMainFrm::ComboBox2Change(TObject *Sender)
{
  JvOutlookBar1->ButtonSize = static_cast<TJvBarButtonSize >(ComboBox2->ItemIndex);
}
//---------------------------------------------------------------------------
