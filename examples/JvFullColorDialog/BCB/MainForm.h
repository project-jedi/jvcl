//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvFullColorCtrls.hpp"
#include "JvFullColorDialogs.hpp"
#include "JvFullColorForm.hpp"
#include "JvFullColorSpaces.hpp"
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TJvFullColorLabel *JvFullColorLabel;
  TLabel *LabelInfo;
  TJvFullColorDialog *JvFullColorDialog;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall JvFullColorLabelDblClick(TObject *Sender);
  void __fastcall JvFullColorDialogApply(TObject *Sender,
          TJvFullColor AFullColor);
private:	// User declarations
  void __fastcall UpdateCaption (TJvFullColorLabel* ALabel);
  void __fastcall UpdateAllCaptions();
  void __fastcall CreateLabel(TJvFullColorLabel* &LColorLabel, int& X, int& Y, TJvFullColor AFullColor);
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
