//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvBmpAnimator.hpp"
#include "JvComponent.hpp"
#include "JvExControls.hpp"
#include <ComCtrls.hpp>
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TUpDown *UpDown2;
  TUpDown *UpDown1;
  TCheckBox *Transparent;
  TButton *OnOff;
  TLabel *Label2;
  TLabel *Label1;
  TImageList *ImageList1;
  TEdit *Edit2;
  TEdit *Edit1;
  TJvBmpAnimator *BmpAnimator1;
  void __fastcall OnOffClick(TObject *Sender);
  void __fastcall Edit1Change(TObject *Sender);
  void __fastcall Edit2Change(TObject *Sender);
  void __fastcall TransparentClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
