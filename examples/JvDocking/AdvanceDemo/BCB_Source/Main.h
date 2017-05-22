//---------------------------------------------------------------------------

#ifndef MainH
#define MainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvDockControlForm.hpp"
#include "JvDockDelphiStyle.hpp"
#include "JvDockVCStyle.hpp"
#include "JvDockVIDStyle.hpp"
#include "JvDockVSNetStyle.hpp"
#include <ComCtrls.hpp>
#include <Menus.hpp>
#include <ToolWin.hpp>
#include "JvComponent.hpp"

//---------------------------------------------------------------------------
class TMainForm : public TForm
{
__published:	// IDE-managed Components
  TJvDockServer *lbDockServer1;
  TMainMenu *MainMenu1;
  TMenuItem *DockForm_Menu;
  TMenuItem *DelphiStyle;
  TMenuItem *VCStyle;
  TMenuItem *VIDStyle;
  TMenuItem *VSNETStyle;
  TMenuItem *ShowWindow_Menu;
  TMenuItem *DockInfo_Menu;
  TMenuItem *SaveToFile;
  TMenuItem *LoadFromFile;
  TMenuItem *N24;
  TMenuItem *SaveToReg;
  TMenuItem *LoadFromReg;
  TMenuItem *DockOption_Menu;
  TMenuItem *TopDocked;
  TMenuItem *BottomDocked;
  TMenuItem *LeftDocked;
  TMenuItem *RightDocked;
  TMenuItem *N31;
  TMenuItem *AllDocked;
  TJvDockVIDStyle *JvDockVIDStyle1;
  TJvDockVCStyle *JvDockVCStyle1;
  TJvDockDelphiStyle *JvDockDelphiStyle1;
  TJvDockVSNetStyle *JvDockVSNetStyle1;
  TPopupMenu *PopupMenu2;
  TMenuItem *ClientTopDocked;
  TMenuItem *ClientBottomDocked;
  TMenuItem *ClientLeftDocked;
  TMenuItem *ClientRightDocked;
  TMenuItem *N20;
  TMenuItem *ClientEachOtherDocked;
  TMenuItem *ClientAllDocked;
  TMenuItem *N21;
  TMenuItem *ClientDockorFloat;
  TMenuItem *ClientHide;
  TMemo *Memo1;
  TToolBar *ToolBar1;
  TToolButton *ToolButton1;
  TToolButton *ToolButton2;
  TToolButton *ToolButton3;
  TToolButton *ToolButton4;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall DelphiStyleClick(TObject *Sender);
  void __fastcall VCStyleClick(TObject *Sender);
  void __fastcall VIDStyleClick(TObject *Sender);
  void __fastcall VSNETStyleClick(TObject *Sender);
  void __fastcall SaveToFileClick(TObject *Sender);
  void __fastcall LoadFromFileClick(TObject *Sender);
  void __fastcall SaveToRegClick(TObject *Sender);
  void __fastcall LoadFromRegClick(TObject *Sender);
  void __fastcall TopDockedClick(TObject *Sender);
  void __fastcall BottomDockedClick(TObject *Sender);
  void __fastcall LeftDockedClick(TObject *Sender);
  void __fastcall RightDockedClick(TObject *Sender);
  void __fastcall AllDockedClick(TObject *Sender);
  void __fastcall ClientTopDockedClick(TObject *Sender);
  void __fastcall ClientBottomDockedClick(TObject *Sender);
  void __fastcall ClientLeftDockedClick(TObject *Sender);
  void __fastcall ClientRightDockedClick(TObject *Sender);
  void __fastcall ClientEachOtherDockedClick(TObject *Sender);
  void __fastcall ClientAllDockedClick(TObject *Sender);
  void __fastcall ClientDockorFloatClick(TObject *Sender);
  void __fastcall ClientHideClick(TObject *Sender);
  void __fastcall PopupMenu2Popup(TObject *Sender);
private:  // User declarations
  int FForm1Count,
      FForm2Count,
      FForm3Count,
      FForm4Count;
  void __fastcall ShowDockWindowMenuClick(TObject *Sender);
  void AddItemToShowDockMenu(TForm * pForm);
public:		// User declarations
  __fastcall TMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif
