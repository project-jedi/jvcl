//---------------------------------------------------------------------------

#ifndef TransparentFormH
#define TransparentFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvComponent.hpp"
#include "JvExControls.hpp"
#include "JvFormTransparent.hpp"
#include "JvLabel.hpp"
//---------------------------------------------------------------------------
class TfrmTransparent : public TForm
{
__published:	// IDE-managed Components
  TJvTransparentForm *JvTransparentForm1;
  TJvLabel *JvLabel1;
private:	// User declarations
public:		// User declarations
  __fastcall TfrmTransparent(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmTransparent *frmTransparent;
//---------------------------------------------------------------------------
#endif
