//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvArrowButton.hpp"
#include "JvComponent.hpp"
#include "JvExControls.hpp"
#include <ImgList.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TPopupMenu *PopupMenu1;
  TMenuItem *Add1;
  TMenuItem *Delete1;
  TMenuItem *Edit1;
  TMenuItem *Replace1;
  TMenuItem *N1;
  TMenuItem *Close1;
  TImageList *ImageList1;
  TJvArrowButton *ArrowButton2;
  TJvArrowButton *ArrowButton1;
  void __fastcall Close1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
