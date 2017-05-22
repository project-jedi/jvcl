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
#include "JvCheckListBox.hpp"
#include "JvDBDotNetControls.hpp"
#include "JvDotNetControls.hpp"
#include "JvEdit.hpp"
#include "JvExCheckLst.hpp"
#include "JvExComCtrls.hpp"
#include "JvExForms.hpp"
#include "JvExMask.hpp"
#include "JvExStdCtrls.hpp"
#include "JvHotKey.hpp"
#include "JvListBox.hpp"
#include "JvMaskEdit.hpp"
#include "JvMemo.hpp"
#include "JvScrollBox.hpp"
#include <CheckLst.hpp>
#include <ComCtrls.hpp>
#include <DBCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Graphics.hpp>
#include <Mask.hpp>
#include "JvToolEdit.hpp"
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TPageControl *PageControl1;
  TTabSheet *TabSheet4;
  TGroupBox *GroupBox6;
  TJvDotNetEdit *JvDotNetEdit1;
  TJvDotNetHotKey *dxDNHotKey1;
  TJvDotNetMaskEdit *JvDotNetMaskEdit1;
  TJvDotNetMemo *JvDotNetMemo1;
  TJvDotNetListBox *dxDNListBox1;
  TJvDotNetCheckListBox *dxDNCheckListBox1;
  TJvDotNetScrollBox *dxDNScrollBox1;
  TGroupBox *GroupBox7;
  TJvDotNetDBEdit *JvDotNetDbEdit1;
  TJvDotNetDBMemo *JvDotNetDbMemo1;
  TJvDotNetDBRichEdit *JvDotNetDbRichEdit1;
  TJvDotNetDBListBox *JvDotNetDbListBox1;
  TPanel *Shape1;
  TImage *Image2;
  TLabel *Label8;
  void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
