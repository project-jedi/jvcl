//---------------------------------------------------------------------------

#ifndef mainB6H
#define mainB6H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvComponent.hpp"
#include "JvUIB.hpp"
//---------------------------------------------------------------------------
typedef struct TADTRecord
{
   String COUNTRY;
   String CURRENCY;

} TARecord;


class TfrmMain : public TForm
{
__published:	// IDE-managed Components
        TButton *go;
        TJvUIBDataBase *DataBase;
        TJvUIBTransaction *Transaction;
        TJvUIBQuery *Query;
        void __fastcall goClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
        __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
