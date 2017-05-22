//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RegTVMainFormU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvComponent"
#pragma link "JvExComCtrls"
#pragma link "JvRegistryTreeView"
#pragma resource "*.dfm"
TRegTVMainForm *RegTVMainForm;
//---------------------------------------------------------------------------
__fastcall TRegTVMainForm::TRegTVMainForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TRegTVMainForm::ListView1Change(TObject *Sender,
      TListItem *Item, TItemChange Change)
{
  StatusBar1->Panels->Items[0]->Text = RegistryTreeView1->CurrentPath;
}
//---------------------------------------------------------------------------
void __fastcall TRegTVMainForm::RegistryTreeView1Expanded(TObject *Sender,
      TTreeNode *Node)
{
  Screen->Cursor = crDefault;
}
//---------------------------------------------------------------------------
void __fastcall TRegTVMainForm::RegistryTreeView1Expanding(TObject *Sender,
      TTreeNode *Node, bool &AllowExpansion)
{
  Screen->Cursor = crHourGlass;
}
//---------------------------------------------------------------------------
void __fastcall TRegTVMainForm::Importregistryfile1Click(TObject *Sender)
{
  TOpenDialog *pOD;
  pOD = new TOpenDialog(NULL);

  try
  {
    pOD->Filter = "Registry files|*.reg;*.key|All files|*.*";
    pOD->InitialDir = ".";
    if( pOD-> Execute() )
    {
      RegistryTreeView1->LoadKey(pOD->FileName);
    }
  }
  __finally
  {
    delete pOD;
  }

}
//---------------------------------------------------------------------------
void __fastcall TRegTVMainForm::Exportregistryfile1Click(TObject *Sender)
{
 TSaveDialog *pSD;
  pSD = new TSaveDialog(NULL);
//  with TSaveDialog.Create(nil) do
  try
  {
    pSD->Filter = "Registry files|*.reg;*.key|All files|*.*";
    pSD->InitialDir = ".";
    if( pSD->Execute() )
    {
      RegistryTreeView1->SaveKey(pSD->FileName);
    }
  }
  __finally
  {
    delete pSD;
  }

}
//---------------------------------------------------------------------------
void __fastcall TRegTVMainForm::Connectnetworkdrive1Click(TObject *Sender)
{
  //  RegConnectRegistry();
}
//---------------------------------------------------------------------------
void __fastcall TRegTVMainForm::Disconnectnetworkdrive1Click(
      TObject *Sender)
{
  // RegCloseKey(RemoteRegKey);
}
//---------------------------------------------------------------------------
void __fastcall TRegTVMainForm::Exit1Click(TObject *Sender)
{
   Close();
}
//---------------------------------------------------------------------------
void __fastcall TRegTVMainForm::Key1Click(TObject *Sender)
{
  TTreeNode *pTNd;
  pTNd = RegistryTreeView1->Items->AddChild(RegistryTreeView1->Selected,"New Key");

    pTNd->ImageIndex    = 1;
    pTNd->SelectedIndex = 2;
    pTNd->MakeVisible();
    pTNd->EditText();

}
//---------------------------------------------------------------------------
void __fastcall TRegTVMainForm::Delete1Click(TObject *Sender)
{
  if( RegistryTreeView1->Selected != NULL)
  {
    RegistryTreeView1->Selected->Delete();
  }
}
//---------------------------------------------------------------------------
void __fastcall TRegTVMainForm::Rename1Click(TObject *Sender)
{
  if( RegistryTreeView1->Selected != NULL)
  {
    RegistryTreeView1->Selected->EditText();
  }
}
//---------------------------------------------------------------------------
void __fastcall TRegTVMainForm::Statusbar2Click(TObject *Sender)
{
  StatusBar1->Visible = Statusbar2->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TRegTVMainForm::Refresh1Click(TObject *Sender)
{
  Screen->Cursor = crHourGlass;
  try
  {
    RegistryTreeView1->RefreshNode(RegistryTreeView1->Selected);
  }
  __finally
  {
    Screen->Cursor = crDefault;
  }

}
//---------------------------------------------------------------------------
void __fastcall TRegTVMainForm::DoFavoriteClick(TObject * Sender)
{
 TTreeNode *N;
 TMenuItem *pMI = dynamic_cast<TMenuItem *>(Sender );
  if(pMI!=NULL)
  {
   N = reinterpret_cast<TTreeNode * >( pMI->Tag);

   if( N != NULL)
   {
     N->Selected = true;
     N->Focused  = true;
     N->MakeVisible();
   }
  }
}
//---------------------------------------------------------------------------
void __fastcall TRegTVMainForm::Addtofavorites1Click(TObject *Sender)
{
  String S;
  TMenuItem *m;

  if( RegistryTreeView1->Selected != NULL )
  {
    S = RegistryTreeView1->CurrentPath;
    if( InputQuery("Add to favorites","Name:",S) && (S != "") )
    {
      m = new TMenuItem(mmMain);
      m->Caption = S;
      m->Tag     = reinterpret_cast<int >(RegistryTreeView1->Selected);
      m->OnClick = DoFavoriteClick;
      m->AutoHotkeys = maManual;
      Favorites1->Add(m);
    }
  }
}
//---------------------------------------------------------------------------


void __fastcall TRegTVMainForm::AboutRegistryeditordemo1Click(
      TObject *Sender)
{
  AnsiString stMsg;
  stMsg =  "Demo of the TRegistryTreeview component.\n\r";
  stMsg += "NOTE: edits are not saved to the registry.";
  ShowMessage(stMsg);

}
//---------------------------------------------------------------------------

