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

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TPanel *pnlBottom;
  TLabel *Label2;
  TBevel *Bevel1;
  TMemo *reResult;
  TButton *btnCopy;
  TStatusBar *StatusBar1;
  TPanel *pnlTop;
  TLabel *Label1;
  TMemo *reSource;
  TCheckBox *chkTrim;
  TButton *btnEncode;
  TButton *btnDecode;
  TCheckBox *chkUseUTF8;
  TCheckBox *chkUseClipboard;
  void __fastcall btnEncodeClick(TObject *Sender);
  void __fastcall btnDecodeClick(TObject *Sender);
  void __fastcall btnCopyClick(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormResize(TObject *Sender);
private:	// User declarations
  void __fastcall DisplayTime(int MS);
protected:
  void __fastcall WMDropFiles(TWMDropFiles& Message);
  BEGIN_MESSAGE_MAP
    MESSAGE_HANDLER(WM_DROPFILES, TWMDropFiles, WMDropFiles)
  END_MESSAGE_MAP(TForm);
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
