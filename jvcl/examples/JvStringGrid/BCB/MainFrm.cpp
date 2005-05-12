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

#include <vcl.h>
#pragma hdrstop

#include "MainFrm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvExGrids"
#pragma link "JvStringGrid"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void TForm1::UpdateTrackers(void)
{
  udRows->Min = -1; // JvSG.FixedRows;
  udCols->Min = -1; // JvSG.FixedCols;
  udRows->Max = JvSg->RowCount;
  udCols->Max = JvSg->ColCount;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::btnAddRowClick(TObject *Sender)
{
  JvSg->InsertRow(JvSg->RowCount+1);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::btnDelRowClick(TObject *Sender)
{
  JvSg->RemoveRow(JvSg->RowCount-1);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::RowInsertClick(TObject *Sender)
{
  JvSg->InsertRow(udRows->Position)->Assign(reData->Lines);
  UpdateTrackers();

}
//---------------------------------------------------------------------------

void __fastcall TForm1::RowDeleteClick(TObject *Sender)
{
  JvSg->RemoveRow(udRows->Position);
  UpdateTrackers();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::RowHideClick(TObject *Sender)
{
  JvSg->HideRow(udRows->Position);
  UpdateTrackers();

}
//---------------------------------------------------------------------------

void __fastcall TForm1::RowShowClick(TObject *Sender)
{
  JvSg->ShowRow(udRows->Position,0);
  UpdateTrackers();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::btnAddColClick(TObject *Sender)
{
  JvSg->InsertCol(JvSg->ColCount+1);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::btnDelColClick(TObject *Sender)
{
  JvSg->RemoveCol(JvSg->ColCount-1);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ColInsertClick(TObject *Sender)
{
  JvSg->InsertCol(udCols->Position)->Assign(reData->Lines);
  UpdateTrackers();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ColDeleteClick(TObject *Sender)
{
  JvSg->RemoveCol(udCols->Position);
  UpdateTrackers();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ColHideClick(TObject *Sender)
{
  JvSg->HideCol(udCols->Position);
  UpdateTrackers();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ColShowClick(TObject *Sender)
{
  JvSg->ShowCol(udCols->Position,0);
  UpdateTrackers();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ClearClick(TObject *Sender)
{
  JvSg->Clear();
  UpdateTrackers();
}
//---------------------------------------------------------------------------

