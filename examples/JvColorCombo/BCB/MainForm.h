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
#include "JvColorCombo.hpp"
#include "JvCombobox.hpp"
#include "JvExStdCtrls.hpp"
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TMemo *memInfo;
  TLabel *Label3;
  TLabel *Label2;
  TLabel *Label1;
  TJvColorComboBox *JvColorComboBox1;
  TEdit *edNameTemplate;
  TCheckBox *chkAllowCustom;
  TComboBox *cbDisplayStyle;
  TButton *btnCustColors;
  TButton *btnColorNames;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall btnColorNamesClick(TObject *Sender);
  void __fastcall JvColorComboBox1NewColor(TObject *Sender, TColor Color,
          AnsiString &DisplayName, bool &AllowAdd);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall btnCustColorsClick(TObject *Sender);
  void __fastcall cbDisplayStyleChange(TObject *Sender);
  void __fastcall JvColorComboBox1Change(TObject *Sender);
  void __fastcall chkAllowCustomClick(TObject *Sender);
private:	// User declarations
  void __fastcall LoadSettings();
  void __fastcall SaveSettings();
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
