//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include "Unit2.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvDBGrid"
#pragma link "JvDBUltimGrid"
#pragma link "JvExDBGrids"
#pragma resource "*.dfm"
TMainForm *MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
   : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormShow(TObject *Sender)
{
   Table1->Open();
   TSortFields InitialSort;
   InitialSort.set_length(2);
   InitialSort[0].Name = "Category";
   InitialSort[0].Order = JvGridSort_ASC;
   InitialSort[1].Name = "Common_Name";
   InitialSort[1].Order = JvGridSort_ASC;
   MyUltimGrid->Sort(InitialSort);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::MyUltimGridIndexNotFound(TJvDBUltimGrid *Sender,
   TSortFields FieldsToSort, AnsiString IndexFieldNames, AnsiString DescFields, bool &Retry)
{
   // Nota bene: you need to update Biolife.db to Paradox 7 if you want a sort
   //            in descending order. Otherwise, the index creation will fail.

   int NewIndex = Application->MessageBox(("Index not found for field: "
                + IndexFieldNames + ((DescFields == "") ? "":" (DESC)")
                + "\r\nDo you want to create an index ?").c_str(), "New index ?", MB_YESNO);
   if (NewIndex == ID_YES)
   {
      AnsiString NewIndexName = IndexFieldNames;
      while (NewIndexName.Pos(";") > 0)
         NewIndexName = NewIndexName.Delete(NewIndexName.Pos(";"), 1);
      while (NewIndexName.Pos(" ") > 0)
         NewIndexName = NewIndexName.Delete(NewIndexName.Pos(" "), 1);
      NewIndexName = "Idx_" + NewIndexName + ((DescFields == "") ? "":"_DESC");

      Table1->Close();
      try
      {
         Table1->AddIndex(NewIndexName, IndexFieldNames,
                          TIndexOptions() << ixCaseInsensitive, DescFields);
      }
      catch (const Exception& ErrAddIdx)
      {
         ShowMessage("AddIndex(" + NewIndexName + ") failed: " + ErrAddIdx.Message);
      }
      Table1->Open();
      Retry = true;
   }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormClose(TObject *Sender, TCloseAction &Action)
{
   Table1->Close();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::B_SearchClick(TObject *Sender)
{
   MyUltimGrid->SearchFields->Clear();
   MyUltimGrid->SearchFields->Add("Category");
   MyUltimGrid->SearchFields->Add("Common_Name");
   MyUltimGrid->SearchFields->Add("Species Name");
   MyUltimGrid->SearchFields->Add("Notes");
   bool SearchOK = MyUltimGrid->Search(ValueToSearch->Text, ResultCol, ResultField,
                                       false, false, true);
   B_SearchNext->Enabled = SearchOK;
   if (SearchOK)
      ShowMessage("Hurrah! Text found in field " + Trim(ResultField->FieldName));
   else
      ShowMessage("Text not found");
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::B_SearchNextClick(TObject *Sender)
{
   if (!MyUltimGrid->SearchNext(ResultCol, ResultField, false, false, true))
   {
      ShowMessage("Text not found - End of search");
      B_SearchNext->Enabled = false;
   }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::MyUltimGridDrawColumnCell(TObject *Sender, const TRect &Rect,
   int DataCol, TColumn *Column, TGridDrawState State)
{
   // Hey fish, let me see your face...hum...your fins
   if (Column->Field->FieldName == "Graphic")
   {
      Graphics::TBitmap *Image = new Graphics::TBitmap();
      try
      {
         Image->Assign((TBlobField *)Column->Field);
         if (Image->Height > Rect.Bottom - Rect.Top)
            Image->Height = Rect.Bottom - Rect.Top;
         if (Image->Width > Rect.Right - Rect.Left)
            Image->Width = Rect.Right - Rect.Left;
         MyUltimGrid->Canvas->FillRect(Rect);
         MyUltimGrid->Canvas->Draw(Rect.Left, Rect.Top, Image);
      }
      __finally
      {
         delete Image;
      }
   }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::B_ResizeColsClick(TObject *Sender)
{
   MyUltimGrid->RowsHeight = 129;
   MyUltimGrid->InitializeColumnsWidth(70, false, false);
   MyUltimGrid->AutoSizeColumnIndex = JvGridResizeLastVisibleCol;
   MyUltimGrid->AutoSizeColumns = true;
}
//---------------------------------------------------------------------------
