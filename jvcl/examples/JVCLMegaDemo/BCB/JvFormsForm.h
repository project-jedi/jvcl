/******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2004 Project JEDI

 Original author: Olivier Sannier (obones att altern dott org)

 This is a port from the demo written in Delphi by
   Ralf Grenzing [ralfgspam@gmx.de]
   Uwe Rupprecht [uwe-rupprecht@gmx.de]
   Michael Beck [mbeck1@compuserve.com]
   Angus Johnson [ajohnson@rpi.net.au]

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

#ifndef JvFormsFormH
#define JvFormsFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
//---------------------------------------------------------------------------
class TfrmJvForms : public TForm
{
__published:	// IDE-managed Components
  TBitBtn *BitBtn9;
  TBitBtn *BitBtn8;
  TBitBtn *BitBtn7;
  TBitBtn *BitBtn6;
  TBitBtn *BitBtn5;
  TBitBtn *BitBtn4;
  TBitBtn *BitBtn3;
  TBitBtn *BitBtn2;
  TBitBtn *BitBtn12;
  TBitBtn *BitBtn11;
  TBitBtn *BitBtn10;
  TBitBtn *BitBtn1;
  void __fastcall BitBtn1Click(TObject *Sender);
  void __fastcall BitBtn2Click(TObject *Sender);
  void __fastcall BitBtn4Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
  TForm* TheForm;
  __fastcall TfrmJvForms(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmJvForms *frmJvForms;
//---------------------------------------------------------------------------
#endif
