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
#include "JvComCtrls.hpp"
#include "JvDialogs.hpp"
#include "JvExComCtrls.hpp"
#include "JvSimpleXml.hpp"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TLabel *Label1;
  TEdit *edXMLFile;
  TButton *Button1;
  TJvTreeView *JvTreeView1;
  TJvSimpleXML *JvSimpleXml1;
  TJvOpenDialog *JvOpenDialog1;
  void __fastcall Button1Click(TObject *Sender);
private:	// User declarations
  void __fastcall LoadFromFile(const AnsiString Filename);
  void __fastcall ParseIntoTreeView(TJvSimpleXMLElem* AnXMLNode, TTreeNode* ATreeNode);
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
