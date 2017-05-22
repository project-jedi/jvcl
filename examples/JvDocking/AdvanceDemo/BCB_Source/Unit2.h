//---------------------------------------------------------------------------

#ifndef Unit2H
#define Unit2H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvDockControlForm.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TForm2 : public TForm
{
__published:	// IDE-managed Components
  TPanel *Panel1;
  TMemo *Memo1;
  TJvDockClient *lbDockClient1;
  void __fastcall lbDockClient1FormHide(TObject *Sender);
  void __fastcall lbDockClient1FormShow(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TForm2(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm2 *Form2;
//---------------------------------------------------------------------------
#endif
