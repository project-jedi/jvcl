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

#include "TransBtnFormMainU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvButton"
#pragma link "JvComponent"
#pragma link "JvExControls"
#pragma link "JvTransparentButton"
#pragma resource "*.dfm"
TTransBtnFormMain *TransBtnFormMain;

#define TEST_NO_USE_DOWN  (1)

//---------------------------------------------------------------------------
__fastcall TTransBtnFormMain::TTransBtnFormMain(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------


AnsiString TTransBtnFormMain::GetOS(void)
{
 AnsiString sResult;

  switch(Win32Platform)
  {
    case VER_PLATFORM_WIN32_NT     : {sResult = "Windows NT 4.0";}break;;
    case VER_PLATFORM_WIN32_WINDOWS: {sResult = "Windows 95";}break;
    case VER_PLATFORM_WIN32s       : {sResult = "Windows 3.1 with Win32s";}break;
  }

  return sResult;
}
void __fastcall TTransBtnFormMain::TransparentButton1Click(TObject *Sender)
{
  TransparentButton2->Enabled = !TransparentButton2->Enabled;
  TransparentButton3->Enabled = !TransparentButton2->Enabled;

}
//---------------------------------------------------------------------------
void __fastcall TTransBtnFormMain::TransparentButton1MouseDown(
      TObject *Sender, TMouseButton Button, TShiftState Shift, int X,
      int Y)
{
   this->Caption = "Down";
}
//---------------------------------------------------------------------------
void __fastcall TTransBtnFormMain::TransparentButton1MouseEnter(
      TObject *Sender)
{
   this->Caption = "MouseEnter";
}
//---------------------------------------------------------------------------
void __fastcall TTransBtnFormMain::TransparentButton1MouseLeave(
      TObject *Sender)
{
   this->Caption = "MouseLeave";
}
//---------------------------------------------------------------------------
void __fastcall TTransBtnFormMain::TransparentButton1MouseUp(
      TObject *Sender, TMouseButton Button, TShiftState Shift, int X,
      int Y)
{
   this->Caption = "Up";
}
//---------------------------------------------------------------------------
void __fastcall TTransBtnFormMain::Button1Click(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------
void __fastcall TTransBtnFormMain::TransparentButton3Click(TObject *Sender)
{
  ShowMessage("Clicked button. (try the shortkey Alt+W too)");
}
//---------------------------------------------------------------------------
void __fastcall TTransBtnFormMain::TransparentButton6Click(TObject *Sender)
{
#if (TEST_NO_USE_DOWN>0)
  TransparentButton6->Down = !TransparentButton6->Down;
#endif
  ShowMessage("OS is: " + GetOS() + "\n(Note that an OnClick event is generated when going down and when going up)");

}
//---------------------------------------------------------------------------
void __fastcall TTransBtnFormMain::TransparentButton10Click(
      TObject *Sender)
{
  TransparentButton6->Down = !TransparentButton6->Down;
}
//---------------------------------------------------------------------------
void __fastcall TTransBtnFormMain::Exit2Click(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------
void __fastcall TTransBtnFormMain::JvTransparentButton28Click(
      TObject *Sender)
{
  JvTransparentButton27->Enabled = !JvTransparentButton27->Enabled;
}
//---------------------------------------------------------------------------
void __fastcall TTransBtnFormMain::JvTransparentButton26Click(
      TObject *Sender)
{
  JvTransparentButton26->Down = !JvTransparentButton26->Down;
}
//---------------------------------------------------------------------------
void __fastcall TTransBtnFormMain::FormActivate(TObject *Sender)
{
  if (!Activated)
  {
    Activated = True;
    Image2->Picture->Assign(Image3->Picture);
  }
}
//---------------------------------------------------------------------------

void __fastcall TTransBtnFormMain::FormKeyPress(TObject *Sender, char &Key)
{
  if (Key == 13)
  {
    Key = 0;
    TransparentButton1->OnClick(NULL);
  }
}
//---------------------------------------------------------------------------


void __fastcall TTransBtnFormMain::PageControl1Change(TObject *Sender)
{
  Invalidate();        
}
//---------------------------------------------------------------------------

