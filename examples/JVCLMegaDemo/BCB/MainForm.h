//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvCaptionPanel.hpp"
#include "JvComponent.hpp"
#include "JvExControls.hpp"
#include "JvExExtCtrls.hpp"
#include "JvOutlookBar.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TPanel *Panel1;
  TJvOutlookBar *JvOutlookBar1;
  TPanel *Panel2;
  TButton *btnLoadIde;
  TJvCaptionPanel *JvCaptionPanel1;
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall JvOutlookBar1ButtonClick(TObject *Sender, int Index);
  void __fastcall btnLoadIdeClick(TObject *Sender);
private:	// User declarations
  void __fastcall CreateDemoForm(int ID, bool ShowForm =true);
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
