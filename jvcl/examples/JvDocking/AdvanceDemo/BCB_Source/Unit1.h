//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvDockControlForm.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
  TJvDockClient *lbDockClient1;
  TMemo *Memo1;
  void __fastcall lbDockClient1FormHide(TObject *Sender);
  void __fastcall lbDockClient1FormShow(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
