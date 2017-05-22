//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "JvThumbnailChildFormU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvBaseThumbnail"
#pragma link "JvExExtCtrls"
#pragma link "JvThumbImage"
#pragma link "JvThumbnails"
#pragma resource "*.dfm"
//TJvThumbnailChildForm *JvThumbnailChildForm;
//---------------------------------------------------------------------------
__fastcall TJvThumbnailChildForm::TJvThumbnailChildForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TJvThumbnailChildForm::FormShow(TObject *Sender)
{
  //thumbimage1.Picture.Free;
  titlePlaceGr->ItemIndex = ThumbNail1->TitlePlacement;
  AngleGr->ItemIndex      = ThumbImage1->Angle;
}
//---------------------------------------------------------------------------
void __fastcall TJvThumbnailChildForm::FileListBox1Change(TObject *Sender){
  if( FileListBox1->FileName!="")
  {
    ThumbNail1->FileName = FileListBox1->FileName;
  }
}
//---------------------------------------------------------------------------
void __fastcall TJvThumbnailChildForm::Button2Click(TObject *Sender)
{
  ThumbImage1->ChangeRGB(RedBar->Position,GreenBar->Position,BlueBar->Position);
  ThumbImage1->Contrast(contrastBar->Position);
  ThumbImage1->Lightness(LightnessBar->Position);
  RedBar->Position   = 0;
  GreenBar->Position = 0;
  BlueBar->Position   = 0;
  contrastBar->Position  = 0;
  LightnessBar->Position = 0;
}
//---------------------------------------------------------------------------
void __fastcall TJvThumbnailChildForm::ThumbNail1Click(TObject *Sender)
{
  if( ThumbNail1->FileName!="")
  {
    ThumbImage1->LoadFromFile(ThumbNail1->FileName);
  }
}
//---------------------------------------------------------------------------
void __fastcall TJvThumbnailChildForm::Button4Click(TObject *Sender)
{
  ThumbImage1->Invert();
}
//---------------------------------------------------------------------------
void __fastcall TJvThumbnailChildForm::Button5Click(TObject *Sender)
{
  ThumbImage1->Grayscale();
}
//---------------------------------------------------------------------------
void __fastcall TJvThumbnailChildForm::AngleGrClick(TObject *Sender)
{
    ThumbImage1->Angle = static_cast<TAngle >(AngleGr->ItemIndex);
}
//---------------------------------------------------------------------------
void __fastcall TJvThumbnailChildForm::asButtonCBClick(TObject *Sender)
{
  ThumbNail1->AsButton = asButtonCB->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TJvThumbnailChildForm::autoloadCBClick(TObject *Sender)
{
  ThumbNail1->AutoLoad = autoloadCB->Checked;        
}
//---------------------------------------------------------------------------
void __fastcall TJvThumbnailChildForm::minMemCBClick(TObject *Sender)
{
  ThumbNail1->MinimizeMemory=minMemCB->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TJvThumbnailChildForm::titlePlaceGrClick(TObject *Sender)
{
  ThumbNail1->TitlePlacement = static_cast<TTitlePos>(titlePlaceGr->ItemIndex); 
}
//---------------------------------------------------------------------------

void __fastcall TJvThumbnailChildForm::Panel6Resize(TObject *Sender)
{
  DriveComboBox2->Height = Panel9->ClientHeight;
  DriveComboBox2->Width  = Panel9->ClientWidth;
}
//---------------------------------------------------------------------------

void __fastcall TJvThumbnailChildForm::Panel8Resize(TObject *Sender)
{
  RedBar->Width = Panel8->ClientWidth;        
}
//---------------------------------------------------------------------------

void __fastcall TJvThumbnailChildForm::Panel10Resize(TObject *Sender)
{
  FilterComboBox1->Width  = Panel10->ClientWidth;
  FilterComboBox1->Height = Panel10->ClientHeight;
}
//---------------------------------------------------------------------------


