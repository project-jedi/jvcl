//---------------------------------------------------------------------------

#ifndef DockWindowFormH
#define DockWindowFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvComponent.hpp"
#include "JvDockControlForm.hpp"
//---------------------------------------------------------------------------
class TfrmDockWindow : public TForm
{
__published:	// IDE-managed Components
  TMemo *Memo1;
  TJvDockClient *lbDockClient1;
private:	// User declarations
public:		// User declarations
  __fastcall TfrmDockWindow(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmDockWindow *frmDockWindow;
//---------------------------------------------------------------------------
#endif
