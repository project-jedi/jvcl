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

#include "fThread.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvComponent"
#pragma link "JvThread"
#pragma link "JvThreadDialog"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::JvThread1Execute(TObject *Sender, Pointer Params)
{
  int i, j, k;
  bool FL_Break=false;

  //Do the job here
  k = 0;
  for(i=0;(i<100) && !FL_Break;++i)
  {
    for(j=0;j<100;++j)
    {
      ++k;
      /*
         I included here ::Sleep(0) unlike the original Delphi code to
         allow the other threads to process as well. (c)VK
      */
      ::Sleep(0); 
      //To use global variable/objects, you have to synchronize (to avoid conflicts)
      Form1->Value = k;
      JvThread1->Synchronize(Form1->Stats1);

      if( JvThread1->Terminated)
      {
        FL_Break = true;
        break;
      }
    }
  }
  if(!JvThread1->Terminated)
  {
    JvThread1->Synchronize(Form1->enableButton1);
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::JvThread2Execute(TObject *Sender, Pointer Params)
{
  int i, j, k;
  bool FL_Break=false;
  //Do the job here
  k = 0;
  for(i=0;(i<100) && !FL_Break;++i)
  {
    for(j=0;j<10;++j)
    {
      k +=5; //This is the only difference with the other thread
      ::Sleep(13);
      //To use global variable/objects, you have to synchronize (to avoid conflicts)
      Form1->Value2 = k;
      JvThread2->Synchronize(Form1->Stats2);

      if( JvThread2->Terminated )
      {
        FL_Break = true;
        break;
      }
    }
  }
  if(!JvThread2->Terminated)
  {
     JvThread2->Synchronize(Form1->enableButton2);
  }   
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Stats1(void)
{
  Label2->Caption = IntToStr(Value);
}


void __fastcall TForm1::Stats2(void)
{
  Label4->Caption = IntToStr(Value2);
}

void __fastcall TForm1::Button1Click(TObject *Sender)
{
  TButton *pB;

  JvThread1->ThreadDialog = NULL;
  JvThread1->Execute(this);
  pB = dynamic_cast<TButton *>(Sender);
  if(pB!=NULL)
  {
    pB->Enabled = false;
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button2Click(TObject *Sender)
{
 TButton *pB;
 
  JvThread2->ThreadDialog = NULL;
  JvThread2->Execute(this);
  pB = dynamic_cast<TButton *>(Sender);
  if(pB!=NULL)
  {
    pB->Enabled = false;
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button3Click(TObject *Sender)
{
  JvThread1->ThreadDialog = JvThreadSimpleDialog1;
  JvThread1->Execute(this);
  //(Sender as TButton).Enabled := False;

}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button4Click(TObject *Sender)
{
  JvThread2->ThreadDialog = JvThreadAnimateDialog1;
  JvThread2->Execute(this);
  //(Sender as TButton).Enabled := False;

}
//---------------------------------------------------------------------------


void __fastcall TForm1::enableButton1(void)
{
  Button1->Enabled = true;
}
//---------------------------------------------------------------------------


void __fastcall TForm1::enableButton2(void)
{
  Button2->Enabled = true;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  if(!JvThread1->Terminated)
  {
    JvThread1->Terminate();
  }
  if(!JvThread2->Terminated)
  {
    JvThread2->Terminate();
  }
  while( (!JvThread1->Terminated) && (!JvThread2->Terminated) )
  {
    ::Sleep(100);
    Application->ProcessMessages();
  }

}
//---------------------------------------------------------------------------

