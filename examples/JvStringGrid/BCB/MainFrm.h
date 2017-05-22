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

#ifndef MainFrmH
#define MainFrmH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvExGrids.hpp"
#include "JvStringGrid.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
        TJvStringGrid *JvSg;
        TPanel *Panel1;
        TLabel *Label1;
        TLabel *Label2;
        TLabel *Label3;
        TEdit *edRow;
        TUpDown *udRows;
        TEdit *edCol;
        TUpDown *udCols;
        TButton *RowInsert;
        TButton *RowDelete;
        TButton *RowHide;
        TButton *RowShow;
        TButton *ColInsert;
        TButton *ColDelete;
        TButton *ColHide;
        TButton *ColShow;
        TRichEdit *reData;
        TButton *Clear;
        TButton *btnAddRow;
        TButton *btnDelRow;
        TButton *btnAddCol;
        TButton *btnDelCol;
        void __fastcall btnAddRowClick(TObject *Sender);
        void __fastcall btnDelRowClick(TObject *Sender);
        void __fastcall RowInsertClick(TObject *Sender);
        void __fastcall RowDeleteClick(TObject *Sender);
        void __fastcall RowHideClick(TObject *Sender);
        void __fastcall RowShowClick(TObject *Sender);
        void __fastcall btnAddColClick(TObject *Sender);
        void __fastcall btnDelColClick(TObject *Sender);
        void __fastcall ColInsertClick(TObject *Sender);
        void __fastcall ColDeleteClick(TObject *Sender);
        void __fastcall ColHideClick(TObject *Sender);
        void __fastcall ColShowClick(TObject *Sender);
        void __fastcall ClearClick(TObject *Sender);
private:	// User declarations
        void UpdateTrackers(void);
public:		// User declarations
        __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
