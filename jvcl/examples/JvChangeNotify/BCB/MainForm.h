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
#include "JvChangeNotify.hpp"
#include "JvComponent.hpp"
#include <Buttons.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TPanel *Panel1;
  TSpeedButton *btnStart;
  TLabel *Label2;
  TLabel *Label4;
  TButton *btnAdd;
  TButton *btnDelete;
  TEdit *Edit1;
  TUpDown *udInterval;
  TButton *btnClear;
  TListView *ListView1;
  TListBox *ListBox2;
  TLabel *Label3;
  TJvChangeNotify *JvChangeNotify1;
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall btnDeleteClick(TObject *Sender);
  void __fastcall btnAddClick(TObject *Sender);
  void __fastcall btnStartClick(TObject *Sender);
  void __fastcall btnClearClick(TObject *Sender);
  void __fastcall JvChangeNotify1ChangeNotify(TObject *Sender,
          AnsiString Dir, TJvChangeActions Actions);
  void __fastcall ListView1DblClick(TObject *Sender);
private:	// User declarations
  void __fastcall ResetCaptions(bool Invert);
  void __fastcall DeleteItem(TListItem* li);
  void __fastcall EditItem(TListItem* li);
protected:
  void __fastcall WMGetMinMaxInfo(TWMGetMinMaxInfo &Msg);
BEGIN_MESSAGE_MAP
  MESSAGE_HANDLER(WM_GETMINMAXINFO, TWMGetMinMaxInfo, WMGetMinMaxInfo)
END_MESSAGE_MAP(TForm)
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
