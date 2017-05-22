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

#ifndef ChangeDirDlgH
#define ChangeDirDlgH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <JvChangeNotify.hpp>
//---------------------------------------------------------------------------
class TfrmChangeNotificationDirDlg : public TForm
{
__published:	// IDE-managed Components
  TLabel *Label1;
  TGroupBox *GroupBox1;
  TCheckBox *cbAttributes;
  TCheckBox *cbDirNames;
  TCheckBox *cbFileNames;
  TCheckBox *cbSize;
  TCheckBox *cbWrite;
  TCheckBox *cbSubTrees;
  TEdit *Edit1;
  TButton *Button3;
  TButton *Button1;
  TButton *btnOK;
  void __fastcall Button1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmChangeNotificationDirDlg(TComponent* Owner);
  static bool Execute(AnsiString& Directory, TJvChangeActions& Options, bool& IncludeSubDirs);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmChangeNotificationDirDlg *frmChangeNotificationDirDlg;
//---------------------------------------------------------------------------
#endif
