//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ScrollWinMainFormU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvComponent"
#pragma link "JvExControls"
#pragma link "JvExForms"
#pragma link "JvLookOut"                                                
#pragma link "JvScrollPanel"
#pragma link "JvScrollPanel"
#pragma resource "*.dfm"
TJvScrollingWindowMainForm *JvScrollingWindowMainForm;

 AnsiString BoolOnOff[2]={" Off ", " On"};
#ifdef BCB6_UP                        
  AnsiString AlignStr[7]={"alNone", "alTop", "alBottom", "alLeft",
                          "alRight", "alClient", "alCustom"}; /*sizeof( TAlign )*/
#else           
  AnsiString AlignStr[6]={"alNone", "alTop", "alBottom", "alLeft",
                          "alRight", "alClient"}; /*sizeof(TAlign)*/
#endif

//---------------------------------------------------------------------------
__fastcall TJvScrollingWindowMainForm::TJvScrollingWindowMainForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TJvScrollingWindowMainForm::Button1Click(TObject *Sender)
{
 int i, tmp;

/* AutoArrange only moves children - not the panel itself,
 so you'll have to do it manually: */

  //with ScrollPanel1 do
    if( ScrollPanel1->ScrollDirection == sdVertical )
    {

      tmp = ScrollPanel1->Width;
      ScrollPanel1->ScrollDirection = sdHorizontal;
      if( ScrollPanel1->Align == alRight)
      {
        ScrollPanel1->Align = alTop;
      }
      else
      {
        ScrollPanel1->Align = alBottom;
      }
      ScrollPanel1->Height = tmp;
    }
    else
    {
      tmp = ScrollPanel1->Height;
      ScrollPanel1->ScrollDirection = sdVertical;
      if( ScrollPanel1->Align == alTop )
      {
        ScrollPanel1->Align = alLeft;
      }
      else
      {
        ScrollPanel1->Align = alRight;
      }
      ScrollPanel1->TJvScrollingWindow::Width = tmp;
    }
    //   { Adjust the TDividers }
 // with ScrollPanel1 do
    for( i = 0;i<ScrollPanel1->ControlCount;++i)
    {
      if( ScrollPanel1->Controls[i]->ClassNameIs("TJvDivider") )
      {
        dynamic_cast<TJvDivider *>(ScrollPanel1->Controls[i])->Vertical = ScrollPanel1->Align;// in [alTop,alBottom];
      }
    }
  Caption = "Align " + AlignStr[ScrollPanel1->Align];

}
//---------------------------------------------------------------------------
void __fastcall TJvScrollingWindowMainForm::Button2Click(TObject *Sender)
{
  ScrollPanel1->AutoHide = !ScrollPanel1->AutoHide;
  Caption = "Hidden" + BoolOnOff[ScrollPanel1->AutoHide];
}
//---------------------------------------------------------------------------
void __fastcall TJvScrollingWindowMainForm::Button3Click(TObject *Sender)
{
  ScrollPanel1->Flat = !ScrollPanel1->Flat;
  Caption = "Flat" + BoolOnOff[ScrollPanel1->Flat];
}
//---------------------------------------------------------------------------
void __fastcall TJvScrollingWindowMainForm::Button4Click(TObject *Sender)
{
  ScrollPanel1->AutoRepeat = !ScrollPanel1->AutoRepeat;
  Caption = "AutoRepeat" + BoolOnOff[ScrollPanel1->AutoRepeat];
}
//---------------------------------------------------------------------------
void __fastcall TJvScrollingWindowMainForm::Exit1Click(TObject *Sender)
{
  Close();        
}
//---------------------------------------------------------------------------
void __fastcall TJvScrollingWindowMainForm::FormResize(TObject *Sender)
{
  Panel1->Top  = (Height - Panel1->Height) / 2;
  Panel1->Left = (Width  - Panel1->Width)  / 2;
}
//---------------------------------------------------------------------------
void __fastcall TJvScrollingWindowMainForm::CheckBox1Click(TObject *Sender)
{
  ScrollPanel1->Enabled = CheckBox1->Checked;        
}
//---------------------------------------------------------------------------
void __fastcall TJvScrollingWindowMainForm::ExpressButton1Click(
      TObject *Sender)
{
 TComponent *pComp;

  pComp = dynamic_cast<TComponent *>(Sender);
  if(pComp!=NULL)
  {
    Caption = pComp->Name;
  }
}
//---------------------------------------------------------------------------

void __fastcall TJvScrollingWindowMainForm::UpDown1Click(TObject *Sender,
      TUDBtnType Button)
{
  ScrollPanel1->BorderWidth = UpDown1->Position;        
}
//---------------------------------------------------------------------------

