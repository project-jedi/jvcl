//---------------------------------------------------------------------------

#ifndef Unit4H
#define Unit4H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvDockControlForm.hpp"
//---------------------------------------------------------------------------
class TForm4 : public TForm
{
__published:	// IDE-managed Components
  TJvDockClient *lbDockClient1;
  TMemo *Memo1;
  void __fastcall lbDockClient1FormHide(TObject *Sender);
  void __fastcall lbDockClient1FormShow(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TForm4(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm4 *Form4;
//---------------------------------------------------------------------------
#endif
