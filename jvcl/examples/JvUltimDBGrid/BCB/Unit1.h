//---------------------------------------------------------------------------
#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvDBGrid.hpp"
#include "JvExDBGrids.hpp"
#include <ADODB.hpp>
#include <Db.hpp>
#include <DBGrids.hpp>
#include <Grids.hpp>
#include <DBCtrls.hpp>
#include "JvComponent.hpp"
#include "JvDBLookup.hpp"
#include "JvExControls.hpp"
#include "JvDBCombobox.hpp"
#include "JvExStdCtrls.hpp"
#include <ExtCtrls.hpp>
#include "JvDBUltimGrid.hpp"
#include <DB.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// Composants gérés par l'EDI
   TJvDBUltimGrid *JvDBGrid1;
   TADOConnection *ADOConnection1;
   TADOTable *ADOTable1;
   TDataSource *DataSource1;
   TDBMemo *DBMemo1;
   TDataSource *DataSource2;
   TADOTable *ADOTable2;
   TJvDBComboBox *JvDBComboBox1;
   TDBLookupComboBox *DBLookupComboBox1;
   TPanel *Panel1;
   TButton *B_Connect;
   TButton *B_TitleIndic;
   TButton *B_WordWrap;
   TButton *B_RowHeight;
   TButton *B_ShowEdit;
   TAutoIncField *ADOTable1RefLogiciel;
   TWideStringField *ADOTable1Software;
   TWideStringField *ADOTable1Category;
   TBooleanField *ADOTable1FirstBool;
   TBooleanField *ADOTable1SecondBool;
   TSmallintField *ADOTable1Licenses;
   TBCDField *ADOTable1Price;
   TMemoField *ADOTable1Comment;
   TButton *B_Grid2;
   void __fastcall B_RowHeightClick(TObject *Sender);
   void __fastcall B_ConnectClick(TObject *Sender);
   void __fastcall JvDBGrid1ShowEditor(TObject *Sender, TField *Field,
          bool &AllowEdit);
   void __fastcall B_TitleIndicClick(TObject *Sender);
   void __fastcall B_WordWrapClick(TObject *Sender);
   void __fastcall B_ShowEditClick(TObject *Sender);
   void __fastcall ADOTable1CategoryGetText(TField *Sender,
          AnsiString &Text, bool DisplayText);
   void __fastcall FormCreate(TObject *Sender);
   void __fastcall FormDestroy(TObject *Sender);
   void __fastcall JvDBComboBox1KeyPress(TObject *Sender, char &Key);
   void __fastcall DBLookupComboBox1KeyPress(TObject *Sender, char &Key);
   void __fastcall B_Grid2Click(TObject *Sender);
   void __fastcall FormShow(TObject *Sender);
   void __fastcall DBMemo1Enter(TObject *Sender);
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
