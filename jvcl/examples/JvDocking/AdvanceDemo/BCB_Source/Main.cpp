//---------------------------------------------------------------------------

#include <vcl.h>
#include <Winreg.h>
#pragma hdrstop

#include "Main.h"
#include "Unit1.h"
#include "Unit2.h"
#include "Unit3.h"
#include "Unit4.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvDockControlForm"
#pragma link "JvDockDelphiStyle"
#pragma link "JvDockVCStyle"
#pragma link "JvDockVIDStyle"
#pragma link "JvDockVSNetStyle"
#pragma resource "*.dfm"
TMainForm *MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::FormCreate(TObject *Sender)
{
  FForm1Count = 0;
  FForm2Count = 0;
  FForm3Count = 0;
  FForm4Count = 0;
  TopDocked->Checked = lbDockServer1->TopDock;
  BottomDocked->Checked = lbDockServer1->BottomDock;
  LeftDocked->Checked = lbDockServer1->LeftDock;
  RightDocked->Checked = lbDockServer1->RightDock;
  AllDocked->Checked = lbDockServer1->EnableDock;
  Memo1->WordWrap = True;
}

//---------------------------------------------------------------------------
void __fastcall TMainForm::ShowDockWindowMenuClick(TObject *Sender)
{
  //TODO: Add your source code here
  TMenuItem *pMenuItem = (TMenuItem*)Sender;
  TForm *pForm = (TForm*)pMenuItem->Tag;
  if(pMenuItem->Checked)
  {
    if(GetFormVisible(pForm))
    {
      HideDockForm(pForm);
      pMenuItem->Checked = FALSE;
    }else
      ShowDockForm(pForm);
  }else
  {
    ShowDockForm(pForm);
    pMenuItem->Checked = TRUE;
  }
}

void TMainForm::AddItemToShowDockMenu(TForm * pForm)
{
  //TODO: Add your source code here
  TMenuItem *pMenuItem = NewItem(pForm->Caption, 0, TRUE, TRUE,
    ShowDockWindowMenuClick, 0, "");
  ShowWindow_Menu->Add(pMenuItem);
  pMenuItem->Tag = (int)pForm;
  pForm->Tag = (int)pMenuItem;
}


void __fastcall TMainForm::DelphiStyleClick(TObject *Sender)
{
  TForm1 *pForm;
  pForm = new TForm1(Application);
  pForm->Caption = pForm->Caption + " _ " + IntToStr(FForm1Count);
  FForm1Count++;
  AddItemToShowDockMenu(pForm);
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::VCStyleClick(TObject *Sender)
{
  TForm2 *pForm;
  pForm = new TForm2(Application);
  pForm->Caption = pForm->Caption + " _ " + IntToStr(FForm2Count);
  FForm2Count++;
  AddItemToShowDockMenu(pForm);
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::VIDStyleClick(TObject *Sender)
{
  TForm3 *pForm;
  pForm = new TForm3(Application);
  pForm->Caption = pForm->Caption + " _ " + IntToStr(FForm3Count);
  FForm3Count++;
  AddItemToShowDockMenu(pForm);
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::VSNETStyleClick(TObject *Sender)
{
  TForm4 *pForm;
  pForm = new TForm4(Application);
  pForm->Caption = pForm->Caption + " _ " + IntToStr(FForm4Count);
  FForm4Count++;
  AddItemToShowDockMenu(pForm);
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::SaveToFileClick(TObject *Sender)
{
  SaveDockTreeToFile(ExtractFilePath(Application->ExeName) + "DockInfo.ini");
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::LoadFromFileClick(TObject *Sender)
{
  LoadDockTreeFromFile(ExtractFilePath(Application->ExeName) + "DockInfo.ini");
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::SaveToRegClick(TObject *Sender)
{
  SaveDockTreeToReg((ULONG)HKEY_CURRENT_USER, "\\Software\\DockInfo");
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::LoadFromRegClick(TObject *Sender)
{
  LoadDockTreeFromReg((ULONG)HKEY_CURRENT_USER, "\\Software\\DockInfo");
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::TopDockedClick(TObject *Sender)
{
  TopDocked->Checked = !TopDocked->Checked;
  lbDockServer1->TopDock = TopDocked->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::BottomDockedClick(TObject *Sender)
{
  BottomDocked->Checked = !BottomDocked->Checked;
  lbDockServer1->BottomDock = BottomDocked->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::LeftDockedClick(TObject *Sender)
{
  LeftDocked->Checked = !LeftDocked->Checked;
  lbDockServer1->LeftDock = LeftDocked->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::RightDockedClick(TObject *Sender)
{
  RightDocked->Checked = !RightDocked->Checked;
  lbDockServer1->RightDock = RightDocked->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::AllDockedClick(TObject *Sender)
{
  AllDocked->Checked = !AllDocked->Checked;
  lbDockServer1->EnableDock = AllDocked->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ClientTopDockedClick(TObject *Sender)
{
  TJvDockClient *pDockClient = FindDockClient((TForm*)PopupMenu2->PopupComponent);
  if (pDockClient != NULL)
  {
    ClientTopDocked->Checked = !ClientTopDocked->Checked;
    pDockClient->TopDock = ClientTopDocked->Checked;
  }
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ClientBottomDockedClick(TObject *Sender)
{
  TJvDockClient *pDockClient = FindDockClient((TForm*)PopupMenu2->PopupComponent);
  if (pDockClient != NULL)
  {
    ClientBottomDocked->Checked = !ClientBottomDocked->Checked;
    pDockClient->TopDock = ClientBottomDocked->Checked;
  }
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ClientLeftDockedClick(TObject *Sender)
{
  TJvDockClient *pDockClient = FindDockClient((TForm*)PopupMenu2->PopupComponent);
  if (pDockClient != NULL)
  {
    ClientLeftDocked->Checked = !ClientLeftDocked->Checked;
    pDockClient->TopDock = ClientLeftDocked->Checked;
  }
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ClientRightDockedClick(TObject *Sender)
{
  TJvDockClient *pDockClient = FindDockClient((TForm*)PopupMenu2->PopupComponent);
  if (pDockClient != NULL)
  {
    ClientRightDocked->Checked = !ClientRightDocked->Checked;
    pDockClient->TopDock = ClientRightDocked->Checked;
  }
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ClientEachOtherDockedClick(TObject *Sender)
{
  TJvDockClient *pDockClient = FindDockClient((TForm*)PopupMenu2->PopupComponent);
  if (pDockClient != NULL)
  {
    ClientEachOtherDocked->Checked = !ClientEachOtherDocked->Checked;
    pDockClient->EachOtherDock = ClientEachOtherDocked->Checked;
  }
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ClientAllDockedClick(TObject *Sender)
{
  TJvDockClient *pDockClient = FindDockClient((TForm*)PopupMenu2->PopupComponent);
  if (pDockClient != NULL)
  {
    ClientAllDocked->Checked = !ClientAllDocked->Checked;
    pDockClient->EnableDock = ClientAllDocked->Checked;
  }
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ClientDockorFloatClick(TObject *Sender)
{
  TJvDockClient *pDockClient = FindDockClient((TForm*)PopupMenu2->PopupComponent);
  if (pDockClient != NULL)
    pDockClient->RestoreChild();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ClientHideClick(TObject *Sender)
{
  TJvDockClient *pDockClient = FindDockClient((TForm*)PopupMenu2->PopupComponent);
  if (pDockClient != NULL)
    pDockClient->HideParentForm();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::PopupMenu2Popup(TObject *Sender)
{
  TJvDockClient *pDockClient = FindDockClient((TForm*)PopupMenu2->PopupComponent);
  if (pDockClient != NULL)
  {
    ClientTopDocked->Checked = pDockClient->TopDock;
    ClientBottomDocked->Checked = pDockClient->BottomDock;
    ClientLeftDocked->Checked = pDockClient->LeftDock;
    ClientRightDocked->Checked = pDockClient->RightDock;
    ClientEachOtherDocked->Checked = pDockClient->EachOtherDock;
    ClientAllDocked->Checked = pDockClient->EnableDock;
    if(pDockClient->DockState == JvDockState_Floating)
      ClientDockorFloat->Caption = "Dock";
    else ClientDockorFloat->Caption = "Float";
  }
}
//---------------------------------------------------------------------------

