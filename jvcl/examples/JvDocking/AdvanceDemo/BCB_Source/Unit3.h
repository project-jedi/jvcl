//---------------------------------------------------------------------------

#ifndef Unit3H
#define Unit3H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvDockControlForm.hpp"
//---------------------------------------------------------------------------
class TForm3 : public TForm
{
__published:	// IDE-managed Components
  TJvDockClient *lbDockClient1;
  TMemo *Memo1;
  void __fastcall lbDockClient1FormHide(TObject *Sender);
  void __fastcall lbDockClient1FormShow(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TForm3(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm3 *Form3;
//---------------------------------------------------------------------------
#endif
