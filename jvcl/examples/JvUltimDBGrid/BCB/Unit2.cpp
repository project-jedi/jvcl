//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include "Unit2.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvDBGrid"
#pragma link "JvDBUltimGrid"
#pragma link "JvExDBGrids"
#pragma link "JvDBImage"
#pragma resource "*.dfm"
TForm2 *Form2;
//---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent* Owner)
   : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm2::FormShow(TObject *Sender)
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
void __fastcall TForm2::MyUltimGridIndexNotFound(TJvDBUltimGrid *Sender,
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
void __fastcall TForm2::FormClose(TObject *Sender, TCloseAction &Action)
{
   Table1->Close();
}
//---------------------------------------------------------------------------
void __fastcall TForm2::B_SearchClick(TObject *Sender)
{
   MyUltimGrid->SearchFields->Clear();
   MyUltimGrid->SearchFields->Add("Category");
   MyUltimGrid->SearchFields->Add("Common_Name");
   MyUltimGrid->SearchFields->Add("Species Name");
   MyUltimGrid->SearchFields->Add("Notes");
   if (MyUltimGrid->Search(ValueToSearch->Text, ResultCol, ResultField, false, true))
      ShowMessage("Text found in field " + Trim(ResultField->FieldName));
   else
      ShowMessage("Text not found");
}
//---------------------------------------------------------------------------
void __fastcall TForm2::B_SearchNextClick(TObject *Sender)
{
   if (!MyUltimGrid->SearchNext(ResultCol, ResultField, false, true))
   {
      ShowMessage("Text not found - End of search");
   }
}
//---------------------------------------------------------------------------
