/******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2004 Project JEDI

 Original author: Olivier Sannier (obones att altern dott org)

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

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvComponent.hpp"
#include "JvMTComponents.hpp"
#include <ExtCtrls.hpp>
class TBallMove : public TObject
{
public:
    TMTTicket Ticket;
    int X,Y;
};

//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TButton *Button1;
  TMemo *Memo1;
  TScrollBox *ScrollBox;
  TShape *CritShape;
  TButton *Button2;
  TJvMTManager *ThreadManager;
  TJvMTThread *BallThread;
  TJvMTThreadToVCL *Buffer;
  TJvMTCountingSection *Section;
  void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
  void __fastcall Button1Click(TObject *Sender);
  void __fastcall Button2Click(TObject *Sender);
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall BufferCanRead(TObject *Sender);
  void __fastcall BallThreadExecute(TJvMTThread *Sender,
          TJvMTSingleThread *MTThread);
  void __fastcall BallThreadFinished(TJvMTThread *Sender,
          TJvMTSingleThread *MTThread);
private:	// User declarations
  bool __fastcall PointInsideCritical(Extended X, Extended Y);
  void __fastcall BounceBall();

  TShape* __fastcall FindBall(TMTTicket Ticket);
  void __fastcall AddBall(TMTTicket Ticket);
  void __fastcall MoveBall(TBallMove* AMove);
  void __fastcall RemoveBall(TMTTicket Ticket);
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
