//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvDBCombobox"
#pragma link "JvDBGrid"
#pragma link "JvDBUltimGrid"
#pragma link "JvExDBGrids"
#pragma link "JvExStdCtrls"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner): TForm(Owner)
{
   OldRowsHeight = JvDBGrid1->RowsHeight;
   Compteur = 0;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormShow(TObject *Sender)
{
   TSortFields InitialSort;
   InitialSort.set_length(3);
   InitialSort[0].Name = "Category";
   InitialSort[0].Order = JvGridSort_ASC;
   InitialSort[1].Name = "Licenses";
   InitialSort[1].Order = JvGridSort_DESC;
   InitialSort[2].Name = "Software";
   InitialSort[2].Order = JvGridSort_ASC;
   JvDBGrid1->Sort(InitialSort);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
   DisplayList = new TStringList;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormDestroy(TObject *Sender)
{
   delete DisplayList;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FillUpList()
{
   if ((DisplayList == NULL) || (!LookupTable->Active))
      return;
   DisplayList->Clear();
   LookupTable->First();
   while (!LookupTable->Eof)
   {
      DisplayList->Add(LookupTable->FieldByName("CodeLogiciel")->AsString
               + "=" + LookupTable->FieldByName("LibelleLog")->AsString);
      LookupTable->Next();
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::MainTableCategoryGetText(TField *Sender, AnsiString &Text,
   bool DisplayText)
{
   if (DisplayList == NULL)
      return;
   if (DisplayList->IndexOfName(Sender->AsString) == -1)
      FillUpList(); // Key value not found -> the list is (re)loaded
   Text = DisplayList->Values[Sender->AsString];
}
//---------------------------------------------------------------------------
void __fastcall TForm1::B_RowHeightClick(TObject *Sender)
{
   if (OldRowsHeight == JvDBGrid1->RowsHeight)
      JvDBGrid1->RowsHeight = OldRowsHeight * 1.5;
   else
      JvDBGrid1->RowsHeight = OldRowsHeight;
   JvDBGrid1->RowResize = !JvDBGrid1->RowResize;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::B_ConnectClick(TObject *Sender)
{
   ADOConnection1->Connected = !ADOConnection1->Connected;
   MainTable->Active = ADOConnection1->Connected;
   LookupTable->Active = ADOConnection1->Connected;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::B_TitleIndicClick(TObject *Sender)
{
   Compteur++;
   if (Compteur == 1)
   {
     JvDBGrid1->Options = JvDBGrid1->Options >> dgTitles;
   }
   else if (Compteur == 2)
   {
     JvDBGrid1->Options = JvDBGrid1->Options >> dgIndicator;
   }
   else if (Compteur == 3)
   {
     JvDBGrid1->Options = JvDBGrid1->Options << dgTitles;
   }
   else if (Compteur == 4)
   {
     JvDBGrid1->Options = JvDBGrid1->Options << dgIndicator;
     Compteur = 0;
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::B_WordWrapClick(TObject *Sender)
{
   JvDBGrid1->WordWrap = !JvDBGrid1->WordWrap;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::B_ShowEditClick(TObject *Sender)
{
   if (JvDBGrid1->Options.Contains(dgAlwaysShowEditor))
     JvDBGrid1->Options = JvDBGrid1->Options >> dgAlwaysShowEditor;
   else
     JvDBGrid1->Options = JvDBGrid1->Options << dgAlwaysShowEditor;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::DBMemo1Enter(TObject *Sender)
{
   // Text is selected automatically
   ((TDBMemo *)Sender)->SelectAll();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::JvDBComboBox1KeyPress(TObject *Sender, char &Key)
{
   if (Key == 13)
      JvDBGrid1->CloseControl();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::DBLookupComboBox1KeyPress(TObject *Sender, char &Key)
{
   if (Key == 13)
      JvDBGrid1->CloseControl();
   else
   if (Key == 27)
      DBLookupComboBox1->Field->Value = DBLookupComboBox1->Field->OldValue;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::JvDBGrid1RestoreGridPosition(TJvDBUltimGrid *Sender,
   Pointer SavedBookmark, int SavedRowPos)
{
   // Perfect cursor replacement
   if (MainTable->BookmarkValid(SavedBookmark))
      MainTable->Recordset->Bookmark = *(POleVariant)SavedBookmark;
   try {MainTable->Resync(TResyncMode() << rmExact);} catch (...) {}
}
//---------------------------------------------------------------------------
void __fastcall TForm1::B_SearchClick(TObject *Sender)
{
   // Search of W (uppercase) in Software field, then search of 1 in Licenses field
   int ResultCol;
   TField *ResultField;
   bool Found = false;
   JvDBGrid1->SaveGridPosition();
   JvDBGrid1->SearchFields->Clear();
   JvDBGrid1->SearchFields->Add("Software");
   if (JvDBGrid1->Search("W", ResultCol, ResultField, true, false, true))
   {
      Found = (MainTable->FieldByName("Licenses")->AsString == "1");
      while (!Found)
      {
         if (!JvDBGrid1->SearchNext(ResultCol, ResultField, true, false, true))
            break;
         Found = (MainTable->FieldByName("Licenses")->AsString == "1");
      }
   }
   if (Found)
      ShowMessage("Result found:\r\n" + ResultField->AsString);
   else
   {
      JvDBGrid1->RestoreGridPosition();
      ShowMessage("Not found");
   }
}
//---------------------------------------------------------------------------
