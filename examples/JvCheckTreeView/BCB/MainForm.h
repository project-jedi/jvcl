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
#include "JvCheckTreeView.hpp"
#include "JvComCtrls.hpp"
#include "JvExComCtrls.hpp"
#include <ComCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TUpDown *udImageIndex;
  TStatusBar *StatusBar1;
  TPopupMenu *popTree;
  TMenuItem *Nodetype1;
  TMenuItem *mnuNormal;
  TMenuItem *mnuCheckBox;
  TMenuItem *mnuRadioItem;
  TMenuItem *mnuChecked;
  TMenuItem *mnuDelete;
  TMenuItem *N1;
  TMenuItem *Clear1;
  TLabel *Label6;
  TLabel *Label5;
  TLabel *Label4;
  TLabel *Label3;
  TLabel *Label2;
  TLabel *Label1;
  TJvCheckTreeView *JvCheckTreeView1;
  TImageList *ilStandard;
  TImageList *ilFlatChecks;
  TImageList *ilChecks;
  TEdit *edText;
  TEdit *edImageIndex;
  TCheckBox *chkFlat;
  TCheckBox *chkChecked;
  TComboBox *cbStyle;
  TComboBox *cbNodeType;
  TComboBox *cbCascadeOptions;
  TComboBox *cbCascadeLevels;
  TButton *btnRandom;
  TButton *btnAddChild;
  TButton *btnAdd;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall btnAddClick(TObject *Sender);
  void __fastcall btnAddChildClick(TObject *Sender);
  void __fastcall cbNodeTypeChange(TObject *Sender);
  void __fastcall mnuNormalClick(TObject *Sender);
  void __fastcall mnuCheckBoxClick(TObject *Sender);
  void __fastcall mnuRadioItemClick(TObject *Sender);
  void __fastcall mnuCheckedClick(TObject *Sender);
  void __fastcall mnuDeleteClick(TObject *Sender);
  void __fastcall chkFlatClick(TObject *Sender);
  void __fastcall btnRandomClick(TObject *Sender);
  void __fastcall cbStyleChange(TObject *Sender);
  void __fastcall cbCascadeLevelsChange(TObject *Sender);
  void __fastcall Clear1Click(TObject *Sender);
  void __fastcall cbCascadeOptionsChange(TObject *Sender);
  void __fastcall JvCheckTreeView1ContextPopup(TObject *Sender,
          TPoint &MousePos, bool &Handled);
  void __fastcall JvCheckTreeView1Toggling(TObject *Sender,
          TTreeNode *Node, bool &AllowChange);
  void __fastcall JvCheckTreeView1Toggled(TObject *Sender,
          TTreeNode *Node);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
  void __fastcall SetupNode(TTreeNode* Node);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
