/******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):
   korecek: translation from Delphi to BCB

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

#ifndef JvTreeViewAsMenuMainFormUH
#define JvTreeViewAsMenuMainFormUH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvComCtrls.hpp"
#include "JvExComCtrls.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TJvTreeViewAsMenuMainForm : public TForm
{
__published:	// IDE-managed Components
        TLabel *Label1;
        TJvPageControl *JvPageControl1;
        TTabSheet *TabSheet1;
        TButton *Button1;
        TTabSheet *TabSheet2;
        TListBox *ListBox1;
        TTabSheet *TabSheet3;
        TListBox *ListBox2;
        TTabSheet *TabSheet4;
        TRadioGroup *RadioGroup1;
        TTabSheet *TabSheet5;
        TPanel *Panel1;
        TJvTreeView *JvTreeView1;
        void __fastcall JvTreeView1PageChanged(TObject *Sender,
          TTreeNode *Item, TTabSheet *Page);
private:	// User declarations
public:		// User declarations
        __fastcall TJvTreeViewAsMenuMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TJvTreeViewAsMenuMainForm *JvTreeViewAsMenuMainForm;
//---------------------------------------------------------------------------
#endif
