//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvFullColorDialogs.hpp"
#include "JvFullColorSpaces.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TBevel *Bevel;
  TImage *Image;
  TLabel *LabelImage;
  TMemo *Memo;
  TComboBox *ComboBoxFileName;
  TJvFullColorCircleDialog *JvFullColorCircleDialog;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall ComboBoxFileNameClick(TObject *Sender);
  void __fastcall MemoKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
  void __fastcall MemoKeyPress(TObject *Sender, char &Key);
private:	// User declarations
  TImage* Images[7];
  TMemo* Memos[7];
  void __fastcall CustomizeDblClick(TObject* Sender);
  void __fastcall RotateCustomValues();
  void __fastcall FormatMemo(TMemo* AMemo, const TJvColorDelta* Delta);
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
