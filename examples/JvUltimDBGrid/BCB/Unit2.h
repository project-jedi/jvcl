//---------------------------------------------------------------------------
#ifndef Unit2H
#define Unit2H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvDBGrid.hpp"
#include "JvDBUltimGrid.hpp"
#include "JvExDBGrids.hpp"
#include <Db.hpp>
#include <DBGrids.hpp>
#include <DBTables.hpp>
#include <Grids.hpp>
#include <ExtCtrls.hpp>
#include <Buttons.hpp>
#include "JvDBImage.hpp"
#include <DBCtrls.hpp>
//---------------------------------------------------------------------------
class TForm2 : public TForm
{
__published: // Composants gérés par l'EDI
   TTable *Table1;
   TDataSource *DataSource1;
   TJvDBUltimGrid *MyUltimGrid;
   TPanel *Panel1;
   TEdit *ValueToSearch;
   TLabel *Label1;
   TBitBtn *B_Search;
   TBitBtn *B_SearchNext;
   TDBNavigator *DBNavigator1;
   void __fastcall FormShow(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall MyUltimGridIndexNotFound(TJvDBUltimGrid *Sender,
          TSortFields FieldsToSort, AnsiString IndexFieldNames,
          AnsiString DescFields, bool &Retry);
   void __fastcall B_SearchClick(TObject *Sender);
   void __fastcall B_SearchNextClick(TObject *Sender);
private:	    // Déclarations utilisateur
public:	    // Déclarations utilisateur
   int ResultCol;
   TField *ResultField;
   __fastcall TForm2(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm2 *Form2;
//---------------------------------------------------------------------------
#endif
