//---------------------------------------------------------------------------
#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include "JvDBCombobox.hpp"
#include "JvDBGrid.hpp"
#include "JvDBUltimGrid.hpp"
#include "JvExDBGrids.hpp"
#include "JvExStdCtrls.hpp"
#include <ADODB.hpp>
#include <Classes.hpp>
#include <Controls.hpp>
#include <Db.hpp>
#include <DBCtrls.hpp>
#include <DBGrids.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include <StdCtrls.hpp>
#include <DB.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// Composants gérés par l'EDI
   TJvDBUltimGrid *JvDBGrid1;
   TADOConnection *ADOConnection1;
   TADOTable *MainTable;
   TDataSource *DataSource1;
   TDBMemo *DBMemo1;
   TDataSource *DataSource2;
   TADOTable *LookupTable;
   TJvDBComboBox *JvDBComboBox1;
   TDBLookupComboBox *DBLookupComboBox1;
   TPanel *Panel1;
   TButton *B_Connect;
   TButton *B_TitleIndic;
   TButton *B_WordWrap;
   TButton *B_RowHeight;
   TButton *B_ShowEdit;
   TAutoIncField *MainTableRefLogiciel;
   TWideStringField *MainTableSoftware;
   TWideStringField *MainTableCategory;
   TBooleanField *MainTableFirstBool;
   TBooleanField *MainTableSecondBool;
   TSmallintField *MainTableLicenses;
   TBCDField *MainTablePrice;
   TMemoField *MainTableComment;
   TButton *B_Search;
   void __fastcall B_RowHeightClick(TObject *Sender);
   void __fastcall B_ConnectClick(TObject *Sender);
   void __fastcall B_TitleIndicClick(TObject *Sender);
   void __fastcall B_WordWrapClick(TObject *Sender);
   void __fastcall B_ShowEditClick(TObject *Sender);
   void __fastcall MainTableCategoryGetText(TField *Sender,
          AnsiString &Text, bool DisplayText);
   void __fastcall FormCreate(TObject *Sender);
   void __fastcall FormDestroy(TObject *Sender);
   void __fastcall JvDBComboBox1KeyPress(TObject *Sender, char &Key);
   void __fastcall DBLookupComboBox1KeyPress(TObject *Sender, char &Key);
   void __fastcall FormShow(TObject *Sender);
   void __fastcall DBMemo1Enter(TObject *Sender);
   void __fastcall JvDBGrid1RestoreGridPosition(TJvDBUltimGrid *Sender,
          Pointer SavedBookmark, int SavedRowPos);
   void __fastcall B_SearchClick(TObject *Sender);
private:	// Déclarations utilisateur
   int OldRowsHeight, Compteur;
   TStringList* DisplayList;
   void __fastcall FillUpList();
public:		// Déclarations utilisateur
   __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
