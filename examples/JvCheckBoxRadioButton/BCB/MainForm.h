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
#include "JvCheckBox.hpp"
#include "JvComponent.hpp"
#include "JvEdit.hpp"
#include "JvExControls.hpp"
#include "JvExStdCtrls.hpp"
#include "JvLabel.hpp"
#include "JvRadioButton.hpp"
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TJvRadioButton *rbOption3;
  TJvRadioButton *rbOption2;
  TJvRadioButton *rbOption1;
  TPanel *pnlInfo;
  TJvLabel *lblInfo;
  TJvLabel *lblPrefix;
  TJvLabel *lblOption3;
  TJvLabel *lblOption2;
  TJvLabel *lblOption1;
  TImageList *ImageList1;
  TJvEdit *edPrefix;
  TJvCheckBox *chkShowToolTips;
  TJvCheckBox *chkShowPrefix;
private:	// User declarations
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
