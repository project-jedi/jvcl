/******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2004 Project JEDI

 Original author: Olivier Sannier (obones@meloo.com)

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

#include "MainForm.h"
#include <TeeProcs.hpp>
#include <math.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvComponent"
#pragma link "JvMTComponents"
#pragma link "TeeProcs"
#pragma resource "*.dfm"
TfrmMain *frmMain;

bool __fastcall TfrmMain::PointInsideCritical(Extended X, Extended Y)
{
  if (CritShape->Shape == stEllipse)
    return PointInEllipse(Point(floor(X),floor(Y)), CritShape->BoundsRect);
  else if (CritShape->Shape == stRectangle)
    return ((Y>CritShape->Top) && (Y<CritShape->Top+CritShape->Height) &&
      (X>CritShape->Left) && (X<CritShape->Left+CritShape->Width));
  else
    return False;
}

void __fastcall TfrmMain::BounceBall()
{
  TBallMove* M;
  int AntiSpeed;
  Extended X,Y;
  Extended DX=0,DY=0;
  bool InsideCrit;

  randomize();
  AntiSpeed = 10+random(90);
  X = 32;
  Y = 32;
  do DX = 10-random(20); while (abs(DX)<1);
  do DY = 10-random(20); while (abs(DY)<1);
  InsideCrit = false;

  try
  {
    while (true)
    {
      CurrentMTThread()->CheckTerminate();
      Sleep(AntiSpeed);

      M = new TBallMove();
      M->Ticket = CurrentMTThread()->Ticket;
      M->X = floor(X);
      M->Y = floor(Y);
      Buffer->Write(M);

      if ((X+DX<32) || (X+DX>ScrollBox->Width-32)) DX = -DX;
      if ((Y+DY<32) || (Y+DY>ScrollBox->Height-32)) DY = -DY;

      X = X+DX;
      Y = Y+DY;

      if ((!InsideCrit) && PointInsideCritical(X,Y))
      {
        Section->Enter();
        InsideCrit = true;
      }
      else if (InsideCrit && (!PointInsideCritical(X,Y)))
      {
        InsideCrit = false;
        Section->Leave();
      }
    }
  }
  __finally
  {
    if (InsideCrit)
      Section->Leave();
  }
}

TShape* __fastcall TfrmMain::FindBall(TMTTicket Ticket)
{
  int I = ScrollBox->ComponentCount-1;
  while ((I != -1) &&
         (ScrollBox->Components[I]->Tag != Ticket))
    I--;

  if (I != -1)
    return dynamic_cast<TShape*>(ScrollBox->Components[I]);
  else
    return NULL;
}

void __fastcall TfrmMain::AddBall(TMTTicket Ticket)
{
  TShape* Ball = new TShape(ScrollBox);
  Ball->Tag = Ticket;
  Ball->Width = 16;
  Ball->Height = 16;
  Ball->Shape = stEllipse;
  Ball->Left = 0;
  Ball->Top = 0;
  Ball->Parent = ScrollBox;
  Ball->Visible = True;
}

void __fastcall TfrmMain::MoveBall(TBallMove* AMove)
{
  TShape* Ball = FindBall(AMove->Ticket);
  if (Ball != NULL)
  {
    Ball->Top = AMove->Y-8;
    Ball->Left = AMove->X-8;
  }
}

void __fastcall TfrmMain::RemoveBall(TMTTicket Ticket)
{
  delete FindBall(Ticket);
}

//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormClose(TObject *Sender, TCloseAction &Action)
{
  OutputDebugString("FORM CLOSE");
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::Button1Click(TObject *Sender)
{
  BallThread->RunCopy();
  AddBall(BallThread->Ticket);
  Memo1->Lines->Add("Starting ball with ticket "+IntToStr(BallThread->Ticket));
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::Button2Click(TObject *Sender)
{
  ThreadManager->TerminateThreads();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormDestroy(TObject *Sender)
{
  OutputDebugString("FORM DESTROY");
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::BufferCanRead(TObject *Sender)
{
  TBallMove* M = dynamic_cast<TBallMove*>(Buffer->Read());
  try
  {
    MoveBall(M);
  }
  __finally
  {
    delete M;
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::BallThreadExecute(TJvMTThread *Sender,
      TJvMTSingleThread *MTThread)
{
  BounceBall();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::BallThreadFinished(TJvMTThread *Sender,
      TJvMTSingleThread *MTThread)
{
  OutputDebugString("FINISH BALL");
  Memo1->Lines->Add("Ending ball with ticket "+IntToStr(MTThread->Ticket));
  RemoveBall(MTThread->Ticket);
}
//---------------------------------------------------------------------------
