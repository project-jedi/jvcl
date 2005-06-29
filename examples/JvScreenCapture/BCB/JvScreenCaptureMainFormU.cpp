//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "JvScreenCaptureMainFormU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JclGraphics"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  Graphics::TBitmap *B;

   B = new Graphics::TBitmap();
   try
   {
     ScreenShot(B, true);  // Default value does not exist in C5
     this->Image1->Picture->Bitmap->Assign(B);
   }
   __finally
   {
     delete B;
   }

}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button2Click(TObject *Sender)
{

  Word AFormat;
  THandle AData;
  HPALETTE APalette;
  TClipboard *Clipboard;

   this->Image1->Picture->Bitmap->SaveToClipboardFormat(AFormat, AData, APalette);

   Clipboard = new TClipboard();
   try
   {
    Clipboard->SetAsHandle(AFormat, AData);
   }
   __finally
   {
     delete Clipboard;
   }
}
//---------------------------------------------------------------------------
