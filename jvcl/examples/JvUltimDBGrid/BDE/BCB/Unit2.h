//---------------------------------------------------------------------------
#ifndef Unit2H
#define Unit2H
//---------------------------------------------------------------------------
#include "JvDBGrid.hpp"
#include "JvDBUltimGrid.hpp"
#include "JvExDBGrids.hpp"
#include <Buttons.hpp>
#include <Classes.hpp>
#include <Controls.hpp>
#include <Db.hpp>
#include <DBCtrls.hpp>
#include <DBGrids.hpp>
#include <DBTables.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include <StdCtrls.hpp>
//---------------------------------------------------------------------------
class TMainForm : public TForm
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
   TBitBtn *B_ResizeCols;
   void __fastcall FormShow(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall MyUltimGridIndexNotFound(TJvDBUltimGrid *Sender,
          TSortFields FieldsToSort, AnsiString IndexFieldNames,
          AnsiString DescFields, bool &Retry);
   void __fastcall B_SearchClick(TObject *Sender);
   void __fastcall B_SearchNextClick(TObject *Sender);
   void __fastcall MyUltimGridDrawColumnCell(TObject *Sender, const TRect &Rect,
          int DataCol, TColumn *Column, TGridDrawState State);
   void __fastcall B_ResizeColsClick(TObject *Sender);
private:	    // Déclarations utilisateur
public:	    // Déclarations utilisateur
   int ResultCol;
   TField *ResultField;
   __fastcall TMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif
