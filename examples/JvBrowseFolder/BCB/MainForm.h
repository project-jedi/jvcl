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
#include "JvBaseDlg.hpp"
#include "JvBrowseFolder.hpp"
#include "JvComponent.hpp"
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TJvBrowseForFolderDialog *JvBrowseFolder1;
  TGroupBox *GroupBox2;
  TLabel *Label4;
  TLabel *Label5;
  TEdit *Edit4;
  TCheckBox *CheckBox1;
  TCheckBox *CheckBox2;
  TCheckBox *CheckBox3;
  TCheckBox *CheckBox4;
  TCheckBox *CheckBox5;
  TCheckBox *CheckBox6;
  TCheckBox *CheckBox7;
  TCheckBox *CheckBox8;
  TComboBox *ComboBox1;
  TGroupBox *GroupBox1;
  TLabel *Label1;
  TLabel *Label2;
  TLabel *Label3;
  TEdit *Edit1;
  TEdit *Edit2;
  TEdit *Edit3;
  TButton *Button1;
  void __fastcall Button1Click(TObject *Sender);
  void __fastcall JvBrowseFolder1Change(TObject *Sender,
          const AnsiString Directory);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
