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
#include "JvUrlListGrabber.hpp"
#include <ComCtrls.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TLabel *lblExpl;
  TMemo *memExplanation;
  TGroupBox *grbDynamic;
  TButton *btnGoDynamic;
  TGroupBox *grbDesign;
  TButton *btnGoDesign;
  TButton *btnClear;
  TButton *btnStop;
  TMemo *memUrls;
  TStatusBar *StatusBar1;
  TJvUrlListGrabber *julGrabber;
  void __fastcall btnClearClick(TObject *Sender);
  void __fastcall btnStopClick(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall btnGoDynamicClick(TObject *Sender);
  void __fastcall btnGoDesignClick(TObject *Sender);
private:	// User declarations
  TJvUrlListGrabber* grabber;
  void _fastcall DoHandleError(TObject* Sender, AnsiString ErrorMsg);
  void _fastcall DoProgressEvent(TObject* Sender, __int64 Position, __int64 TotalSize,
    AnsiString Url, bool& Continue);
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
  void __fastcall grabberConnectionClosed(TJvUrlListGrabber* Sender, TJvCustomUrlGrabber* Grabber);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
