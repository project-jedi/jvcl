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
//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvBehaviorLabel.hpp"
#include "JvExStdCtrls.hpp"
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TJvBehaviorLabel *lblTyping;
  TJvBehaviorLabel *lblSpecial;
  TJvBehaviorLabel *lblScrolling;
  TJvBehaviorLabel *lblCodeBreaker;
  TJvBehaviorLabel *lblBouncing;
  TJvBehaviorLabel *lblBlinking;
  TJvBehaviorLabel *lblAppearing;
  TButton *btnType;
  TButton *btnSpecial;
  TButton *btnScroll;
  TButton *btnCodeBreak;
  TButton *btnBounce;
  TButton *btnBlink;
  TButton *btnAppear;
  TButton *btnAll;
  void __fastcall btnCodeBreakClick(TObject *Sender);
  void __fastcall btnAppearClick(TObject *Sender);
  void __fastcall btnBlinkClick(TObject *Sender);
  void __fastcall btnBounceClick(TObject *Sender);
  void __fastcall btnScrollClick(TObject *Sender);
  void __fastcall btnSpecialClick(TObject *Sender);
  void __fastcall btnTypeClick(TObject *Sender);
  void __fastcall btnAllClick(TObject *Sender);
private:	// User declarations
  void __fastcall DoCodeBreakStart(TObject* Sender);
  void __fastcall DoCodeBreakStop(TObject* Sender);
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
