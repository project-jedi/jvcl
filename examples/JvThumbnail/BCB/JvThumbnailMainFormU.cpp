//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "JvThumbnailMainFormU.h"
#include "JvThumbnailChildFormU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvBaseThumbnail"
#pragma link "JvCombobox"
#pragma link "JvComponent"
#pragma link "JvDriveCtrls"
#pragma link "JvExControls"
#pragma link "JvExForms"
#pragma link "JvExStdCtrls"
#pragma link "JvListBox"
#pragma link "JvSpecialProgress"
#pragma link "JvThumbViews"
#pragma link "JvExMask"
#pragma link "JvSpin"
#pragma resource "*.dfm"
TJvThumbnailMainForm *JvThumbnailMainForm;
//---------------------------------------------------------------------------
__fastcall TJvThumbnailMainForm::TJvThumbnailMainForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TJvThumbnailMainForm::FormShow(TObject *Sender)
{
  CheckBox1->Checked = thumbView1->AutoScrolling;
  CheckBox2->Checked = thumbView1->AutoHandleKeyb;
  CheckBox3->Checked = thumbView1->Sorted;
  CheckBox4->Checked = thumbView1->MinMemory;
  SpinEdit1->Value   = thumbView1->ThumbGap;
  SpinEdit2->MaxValue = 0;
  SpinEdit1->MinValue = 0;
  RadioGroup1->ItemIndex = thumbView1->AlignView;
  RadioGroup2->ItemIndex = thumbView1->ScrollMode;
  NewDir   = false;
  Scanning = false;

}
//---------------------------------------------------------------------------

void __fastcall TJvThumbnailMainForm::DirectoryListBox1Change(TObject *Sender)
{
  if(!Scanning)
  {
    do //repeat
    {
      thumbView1->Directory = DirectoryListBox1->Directory;
    }
    while( thumbView1->Directory != DirectoryListBox1->Directory);
  }
  else
  {
    NewDir = true;
  }
}
//---------------------------------------------------------------------------

void __fastcall TJvThumbnailMainForm::RadioGroup1Click(TObject *Sender)
{
  thumbView1->AlignView = static_cast<TViewType >(RadioGroup1->ItemIndex);
}
//---------------------------------------------------------------------------

void __fastcall TJvThumbnailMainForm::thumbView1Change(TObject *Sender)
{
  Panel4->Caption = thumbView1->SelectedFile;
}
//---------------------------------------------------------------------------

void __fastcall TJvThumbnailMainForm::thumbView1DblClick(TObject *Sender)
{
 TJvThumbnailChildForm *JvThumbnailChildForm;
 TJvThumbView *pJTV;
 TJvThumbnail *pJTN;

  pJTV = dynamic_cast<TJvThumbView *>(Sender);
  pJTN = dynamic_cast<TJvThumbnail *>(Sender);

  JvThumbnailChildForm = new TJvThumbnailChildForm(this);
  JvThumbnailChildForm->DriveComboBox2->Drive = DriveComboBox1->Drive;
  JvThumbnailChildForm->DirectoryListBox2->Directory = DirectoryListBox1->Directory;
  if(pJTV!=NULL)
  {
    JvThumbnailChildForm->FileListBox1->FileName = pJTV->SelectedFile;
  }
  if(pJTN!=NULL)
  {
    JvThumbnailChildForm->FileListBox1->FileName = pJTN->FileName;
  }
  JvThumbnailChildForm->ShowModal();

  delete JvThumbnailChildForm;

}
//---------------------------------------------------------------------------

void __fastcall TJvThumbnailMainForm::thumbView1KeyUp(TObject *Sender,
      WORD &Key, TShiftState Shift)
{
  //  Panel4->Caption= thumbView1->SelectedFile;
}
//---------------------------------------------------------------------------

void __fastcall TJvThumbnailMainForm::thumbView1MouseUp(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
{
  //  Panel4->Caption = thumbView1->SelectedFile;
}
//---------------------------------------------------------------------------

void __fastcall TJvThumbnailMainForm::thumbView1ScanProgress(
      TObject *Sender, int Position, bool &Stop)
{
  JvSpecialProgress1->Position = Position;
  Stop = NewDir;
}
//---------------------------------------------------------------------------

void __fastcall TJvThumbnailMainForm::thumbView1StartScanning(
      TObject *Sender, int Max)
{
  Scanning = true;
  //  DirectoryListBox1->Enabled = false;
  Button1->Enabled = true;
  JvSpecialProgress1->Maximum = Max;
  //  JvSpecialProgress1->Visible = true;
}
//---------------------------------------------------------------------------

void __fastcall TJvThumbnailMainForm::thumbView1StopScanning(
      TObject *Sender)
{
  Scanning = false;
  //  DirectoryListBox1->Enabled = true;
  SpinEdit2->MaxValue = thumbView1->Count - 1;
  NewDir = false;
  Button1->Enabled = false;
  JvSpecialProgress1->Position = 0;
}
//---------------------------------------------------------------------------

void __fastcall TJvThumbnailMainForm::TrackBar1Change(TObject *Sender)
{
  thumbView1->Size = TrackBar1->Position;
}
//---------------------------------------------------------------------------

void __fastcall TJvThumbnailMainForm::CheckBox1Click(TObject *Sender)
{
  thumbView1->AutoScrolling = CheckBox1->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TJvThumbnailMainForm::CheckBox2Click(TObject *Sender)
{
  thumbView1->AutoHandleKeyb = CheckBox2->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TJvThumbnailMainForm::CheckBox4Click(TObject *Sender)
{
  thumbView1->MinMemory = CheckBox4->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TJvThumbnailMainForm::RadioGroup2Click(TObject *Sender)
{
  thumbView1->ScrollMode = static_cast<TScrollMode >(RadioGroup2->ItemIndex);
}
//---------------------------------------------------------------------------

void __fastcall TJvThumbnailMainForm::Button1Click(TObject *Sender)
{
  NewDir = true;        
}
//---------------------------------------------------------------------------

void __fastcall TJvThumbnailMainForm::Button2Click(TObject *Sender)
{
    thumbView1DblClick(thumbView1);
}
//---------------------------------------------------------------------------

void __fastcall TJvThumbnailMainForm::SpinEdit1Change(TObject *Sender)
{
  if( SpinEdit1->Text != "")
  {
    thumbView1->ThumbGap = SpinEdit1->Value;
  }
}
//---------------------------------------------------------------------------

void __fastcall TJvThumbnailMainForm::SpinEdit2Change(TObject *Sender)
{
  thumbView1->Selected = SpinEdit2->Value;        
}
//---------------------------------------------------------------------------


