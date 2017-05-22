//---------------------------------------------------------------------------

#ifndef TestFormH
#define TestFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TfrmTest : public TForm
{
__published:	// IDE-managed Components
  TPanel *PanelForLabel;
  TLabel *lblTest;
  TEdit *Edit1;
  TMemo *mmChanges;
  void __fastcall Edit1Change1(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmTest(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmTest *frmTest;
//---------------------------------------------------------------------------
#endif
