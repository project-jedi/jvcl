//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include "Unit1.h"
#include "Unit2.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvDBGrid"
#pragma link "JvExDBGrids"
#pragma link "JvComponent"
#pragma link "JvDBLookup"
#pragma link "JvExControls"
#pragma link "JvDBCombobox"
#pragma link "JvExStdCtrls"
#pragma link "JvDBUltimGrid"
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
   DisplayList->Clear();
   ADOTable2->First();
   while (!ADOTable2->Eof)
   {
      DisplayList->Add(ADOTable2->FieldByName("CodeLogiciel")->AsString
               + "=" + ADOTable2->FieldByName("LibelleLog")->AsString);
      ADOTable2->Next();
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ADOTable1CategoryGetText(TField *Sender,
      AnsiString &Text, bool DisplayText)
{
   if (DisplayList == NULL) return;
   if (DisplayList->IndexOfName(Sender->AsString) == -1)
      FillUpList(); // Key value not found -> the list is (re)loaded
   Text = DisplayList->Values[Sender->AsString];
   DisplayText = true;
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
   ADOTable1->Active = ADOConnection1->Connected;
   ADOTable2->Active = ADOConnection1->Connected;
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
void __fastcall TForm1::JvDBGrid1ShowEditor(TObject *Sender, TField *Field,
      bool &AllowEdit)
{
   Form1->Caption = "EDIT: " + Field->FieldName + " = " + Field->AsString;
//   AllowEdit = (Field->FieldName != "Licenses");
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
void __fastcall TForm1::B_Grid2Click(TObject *Sender)
{
   Form2->ShowModal();
}
//---------------------------------------------------------------------------
