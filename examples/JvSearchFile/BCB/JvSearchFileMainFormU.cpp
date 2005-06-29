//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "JvSearchFileMainFormU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvAppIniStorage"
#pragma link "JvAppStorage"
#pragma link "JvComponent"
#pragma link "JvExMask"
#pragma link "JvFormPlacement"
#pragma link "JvSearchFiles"
#pragma link "JvToolEdit"
#pragma resource "*.dfm"
TJvSearchFileMainForm *JvSearchFileMainForm;
//---------------------------------------------------------------------------
__fastcall TJvSearchFileMainForm::TJvSearchFileMainForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------

static bool ContainsText(const AnsiString Filename,AnsiString AText)
{
  TMemoryStream *S;
  AnsiString tmp;
  bool flResult;
  flResult = false;
  S = new TMemoryStream();
  try
  {
    S->LoadFromFile(Filename);
    if( S->Memory != NULL)
    {
      tmp = (PChar)S->Memory;
      tmp = AnsiLowerCase(tmp);
      flResult = (tmp.Pos(AnsiLowerCase(AText)) > 0);
    }
  }
  __finally
  {
    delete S;
  }
  return flResult;
}
//---------------------------------------------------------------------------

void __fastcall TJvSearchFileMainForm::OptionsChange(TObject * Sender)
{
//  rbInclude->Enabled = (cbContainText->Text != "");
//  rbExclude->Enabled = rbInclude->Enabled;
  StatusBar1->Panels->Items[0]->Text = "Ready";
  StatusBar1->Update();
}
//---------------------------------------------------------------------------
void __fastcall TJvSearchFileMainForm::FormCloseQuery(TObject *Sender,
      bool &CanClose)
{
    btnCancel->Click();
}
//---------------------------------------------------------------------------


void TJvSearchFileMainForm::AddSearchTextToComboBox(void)
{
    if( (cbContainText->Text != "")
       &&
        (cbContainText->Items->IndexOf(cbContainText->Text) < 0) /* is Text already in the ComboBox */
       )
    {
        cbContainText->Items->Add(cbContainText->Text);
    }
}
//---------------------------------------------------------------------------
void __fastcall TJvSearchFileMainForm::btnCancelClick(TObject *Sender){
  JvSearchFile1->Abort();
  btnCancel->Enabled = false;
}
//---------------------------------------------------------------------------
void __fastcall TJvSearchFileMainForm::btnSearchClick(TObject *Sender)
{
  btnSearch->Enabled = false;
  btnCancel->Enabled = true;
  Screen->Cursor = crHourGlass;
  try
  {
    if( chkClearList->Checked )
    {
      reFoundFiles->Lines->Clear();
    }
    AddSearchTextToComboBox();
    JvSearchFile1->Files->Clear();
    JvSearchFile1->Directories->Clear();
    JvSearchFile1->FileParams->FileMasks->Text = edFileMask->Text;
    if( chkRecursive->Checked )
    {
      JvSearchFile1->DirOption = doIncludeSubDirs;
    }
    else
    {
      JvSearchFile1->DirOption  = doExcludeSubDirs;
    }
    // don't store file and folder names - we do that in the memo
    JvSearchFile1->Options = TJvSearchOptions(JvSearchFile1->Options) << soOwnerData;
    JvSearchFile1->RootDirectory = JvDirectoryBox1->EditText;
    JvSearchFile1->Search();
  }
  __finally
  {
    StatusBar1->Panels->Items[0]->Text =
                 Format("(%d matching items found)",ARRAYOFCONST( (reFoundFiles->Lines->Count) ) );
    btnSearch->Enabled = true;
    btnCancel->Enabled = false;
    Screen->Cursor     = crDefault;
  }
}
//---------------------------------------------------------------------------
void __fastcall TJvSearchFileMainForm::JvSearchFile1BeginScanDir(
      TObject *Sender, const AnsiString AName)
{
#if __BORLANDC__ < 0x0560
  StatusBar1->Panels->Items[0]->Text = Format("Searching in %s...",ARRAYOFCONST( (Jvjclutils::ExcludeTrailingPathDelimiter(AName)) ) );
#else
  StatusBar1->Panels->Items[0]->Text = Format("Searching in %s...",ARRAYOFCONST( (ExcludeTrailingPathDelimiter(AName)) ) );
#endif
  StatusBar1->Update();
}
//---------------------------------------------------------------------------

void __fastcall TJvSearchFileMainForm::JvSearchFile1FindFile(
      TObject *Sender, const AnsiString AName)
{
  StatusBar1->Panels->Items[0]->Text = Format("Searching in %s...",ARRAYOFCONST( (AName) ) );
  StatusBar1->Update();
  if(cbContainText->Text != "")
  {
    if( rbInclude->Checked != ContainsText(AName,cbContainText->Text) )
    {
      return ;
    }
  }
  if( !chkNoDupes->Checked
      ||
      (reFoundFiles->Lines->IndexOf(AName) < 0)
    )
  {
    reFoundFiles->Lines->Add(AName);
  }
}
//---------------------------------------------------------------------------

void __fastcall TJvSearchFileMainForm::JvSearchFile1Progress(
      TObject *Sender)
{
   Application->ProcessMessages();        
}
//---------------------------------------------------------------------------

