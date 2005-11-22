//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "InfoFrm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
//TfrmInfo *frmInfo;
//---------------------------------------------------------------------------
__fastcall TfrmInfo::TfrmInfo(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TfrmInfo::FormClose(TObject *Sender, TCloseAction &Action)
{
  Action = caFree;
}
//---------------------------------------------------------------------------


void TfrmInfo::LoadFile(const AnsiString FileName)
{
 TStringList *S;
 int i;

  if( !FileExists(FileName) )
  {
    reInfo->Lines->Text = Format("'%s': file not found", OPENARRAY(TVarRec, (FileName) ) );
    return;
  }
  S = new TStringList();
  try
  {
    S->LoadFromFile(FileName);
    for( i = 0;i<S->Count;++i)
    {
      if( S->Strings[i].Pos("EXPORT") == 1 )
      {
        int siPos = S->Strings[i].Pos("\"") + 1;
        S->Strings[i] = S->Strings[i].SubString( siPos , MaxInt);
        S->Strings[i] = S->Strings[i].SubString(1, (S->Strings[i].Length() - 1)  );
        S->Strings[i] = S->Strings[i].Trim();
      }
      else
      {
        S->Strings[i] = S->Strings[i].Trim();
      }
    }    
    if( S->Count < 2 )
    {
      // tdump.exe errors on some files, such as Shell32.dll
      // When it errors, it outputs less than 2 lines
      S->Add("\r\nAn error occured running tdump.exe on this DLL");
    }
    else
    {
      S->Insert(2, "EXPORTED FUNCTIONS:\r\n\===================");
    }
    reInfo->Lines = S;
    reInfo->SelStart = 0;
    reInfo->Perform(EM_SCROLLCARET, 0, 0);
  }
  __finally
  {
    delete S;
  }
}

void TfrmInfo_View(const String FileName,const String Title)
{
  TfrmInfo *frmInfo;

  frmInfo = NULL;
  for(int i = 0;(i < Screen->FormCount) && (frmInfo==NULL);++i)
  {
    frmInfo = dynamic_cast<TfrmInfo *>( Screen->Forms[i]);
  }
  if( frmInfo == NULL )
  {
    frmInfo = new TfrmInfo(Application);
  }
  frmInfo->Caption = Format("Viewing content of %s",  OPENARRAY(TVarRec, (Title) ) );
  frmInfo->LoadFile(FileName);
  frmInfo->Show();

}
