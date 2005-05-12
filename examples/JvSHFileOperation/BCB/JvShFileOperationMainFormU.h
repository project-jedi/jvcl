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

#ifndef JvShFileOperationMainFormUH
#define JvShFileOperationMainFormUH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvBaseDlg.hpp"
#include "JvSHFileOperation.hpp"
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TJvShFileOperationMainForm : public TForm
{
__published:	// IDE-managed Components
        TLabel *JvLabel1;
        TLabel *JvLabel2;
        TBevel *JvBevel1;
        TLabel *JvLabel3;
        TButton *btnCopy;
        TButton *btnMove;
        TButton *btnRename;
        TButton *btnDelete;
        TMemo *memSource;
        TMemo *memDest;
        TGroupBox *JvGroupBox1;
        TLabel *Label1;
        TCheckBox *chkUndo;
        TCheckBox *chkFiles;
        TCheckBox *chkMulti;
        TCheckBox *chkNoConfirm;
        TCheckBox *chkNoDirCreate;
        TCheckBox *chkRename;
        TCheckBox *chkSilent;
        TCheckBox *chkSimple;
        TCheckBox *chkMappings;
        TCheckBox *chkNoErrors;
        TEdit *edTitle;
        TCheckBox *chkNoSecAttrs;
        TCheckBox *chkNoRecurse;
        TCheckBox *chkNoConElem;
        TCheckBox *chkNoParse;
        TCheckBox *chkWantNukes;
        TMemo *memMessages;
        TJvSHFileOperation *JvSHFileOperation1;
        void __fastcall JvSHFileOperation1FileMapping(TObject *Sender,
          const AnsiString OldFileName, const AnsiString NewFileName);
        void __fastcall btnCopyClick(TObject *Sender);
        void __fastcall btnMoveClick(TObject *Sender);
        void __fastcall btnRenameClick(TObject *Sender);
        void __fastcall btnDeleteClick(TObject *Sender);
private:
        void DoIt(TJvSHFileOpType AType, const AnsiString OKMsg);	// User declarations
public:		// User declarations
        __fastcall TJvShFileOperationMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TJvShFileOperationMainForm *JvShFileOperationMainForm;
//---------------------------------------------------------------------------
#endif
