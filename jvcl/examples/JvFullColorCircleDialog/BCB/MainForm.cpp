//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvFullColorDialogs"
#pragma link "JvFullColorSpaces"
#pragma resource "*.dfm"

#include <vector>

const AnsiString RsCustomize = "Dbl-click to customize";

class ColorDeltaVector : public std::vector<TJvColorDelta*>
{
public:
  ColorDeltaVector();
  ~ColorDeltaVector();
};

ColorDeltaVector ColorDeltas;

TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}

void __fastcall TfrmMain::CustomizeDblClick(TObject* Sender)
{
  if (JvFullColorCircleDialog->Execute())
    RotateCustomValues();
}

void __fastcall TfrmMain::RotateCustomValues()
{
  RotateBitmap(Image->Picture->Bitmap,Images[6]->Picture->Bitmap,JvFullColorCircleDialog->Delta);
  FormatMemo(Memos[6],JvFullColorCircleDialog->Delta);
}

void __fastcall TfrmMain::FormatMemo(TMemo* AMemo, const TJvColorDelta* Delta)
{
  AMemo->Lines->Clear();
//  with ColorSpaceManager, ColorSpace[Delta.ColorID], AMemo.Lines do
  {
    TJvColorSpace* cspace = ColorSpaceManager()->ColorSpace[Delta->ColorID];
    AMemo->Lines->Add(Format("%s (%s)",ARRAYOFCONST((Name, cspace->ShortName))));
    for (int i = axIndex0; i <= axIndex2; i++)
    {
      TJvAxisIndex Index = static_cast<TJvAxisIndex>(i);
      AMemo->Lines->Add(Format("%s : %d, %d, %d",
              ARRAYOFCONST((cspace->AxisName[Index],
                 Delta->AxisRed->Constituents[Index]->Value,
                 Delta->AxisGreen->Constituents[Index]->Value,
                 Delta->AxisBlue->Constituents[Index]->Value))));
    }
                 
    if (AMemo == Memos[6])
      AMemo->Lines->Add(RsCustomize);
  }
}

AnsiString IncludeTrailingPathDelimiter(const AnsiString &S)
{
  AnsiString Result = S;
  if (!IsPathDelimiter(Result, Result.Length()))
    Result += Sysutils::PathDelim;
  return Result;
}

//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  int X, Y;
  int PitchX, PitchY;
  TImage* LImage;
  TMemo* LMemo;
  TBevel* LBevel;
  int Index;
  TSearchRec LSearchRec;

  if (FindFirst(IncludeTrailingPathDelimiter(GetCurrentDir())+"*.bmp",faAnyFile,LSearchRec) == 0)
  {
    do
    {
      ComboBoxFileName->Items->Add(LSearchRec.Name);
    }
    while (FindNext(LSearchRec) == 0);
  }
  FindClose(LSearchRec);

  PitchX = Memo->Width + 32;
  PitchY = Memo->Top + Memo->Height - Image->Top + 31;
  Index = 0;
  Image->Picture->Bitmap = new Graphics::TBitmap();
  for (X = 0; X <= 3; X++)
    for (Y = 0; Y <= 1; Y++)
      if ((X != 0) || (Y != 0))
      {
        LBevel = new TBevel(this);
        LBevel->Parent = this;
        LBevel->Style = Extctrls::bsRaised;
        LBevel->SetBounds(Bevel->Left+X*PitchX, Bevel->Top+Y*PitchY, Bevel->Width, Bevel->Height);
        LImage = new Extctrls::TImage(this);
        LImage->Parent = this;
        LImage->Stretch = false;
        LImage->Picture->Bitmap = new Graphics::TBitmap();
        LImage->SetBounds(Image->Left+X*PitchX, Image->Top+Y*PitchY, Image->Width, Image->Height);
        LMemo = new TMemo(this);
        LMemo->Parent = this;
        LMemo->BorderStyle = bsNone;
        LMemo->ParentColor = True;
        LMemo->OnKeyDown = MemoKeyDown;
        LMemo->OnKeyPress = MemoKeyPress;
        LMemo->SetBounds(Memo->Left+X*PitchX, Memo->Top+Y*PitchY, Memo->Width, Memo->Height);
        LMemo->Alignment = taCenter;
        if ((X == 3) && (Y == 1))
        {
          LImage->OnDblClick = CustomizeDblClick;
          LMemo->OnDblClick = CustomizeDblClick;
          ClientWidth = LMemo->Left+LMemo->Width-1+Memo->Left;
          ClientHeight = LMemo->Top+LMemo->Height-1+Image->Top;
        }
        Memos[Index] = LMemo;
        Images[Index] = LImage;
        ++Index;
      }
      
  ComboBoxFileName->ItemIndex = 0;
  ComboBoxFileNameClick(ComboBoxFileName);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::ComboBoxFileNameClick(TObject *Sender)
{
  int Index;

  if (Image->Picture->Bitmap != NULL)
    Image->Picture->Bitmap = new Graphics::TBitmap();
  Image->Picture->Bitmap->LoadFromFile(ComboBoxFileName->Items->Strings[ComboBoxFileName->ItemIndex]);
//  with Memos[6]->Lines do
  {
    Memos[6]->Lines->Clear();
    Memos[6]->Lines->Add(RsCustomize);
  }
  Images[6]->Picture->Bitmap->FreeImage();
  for (Index = 0; Index < 6; Index++)
  {
    Images[Index]->Picture->Bitmap->FreeImage();
    RotateBitmap(Image->Picture->Bitmap,Images[Index]->Picture->Bitmap,ColorDeltas[Index]);
    FormatMemo(Memos[Index],ColorDeltas[Index]);
  }
  RotateCustomValues();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::MemoKeyDown(TObject *Sender, WORD &Key,
      TShiftState Shift)
{
  Key = 0;  // discard any key because Enabled=False affects the text rendering
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::MemoKeyPress(TObject *Sender, char &Key)
{
  Key = 0; // discard any key because Enabled=False affects the text rendering
}
//---------------------------------------------------------------------------

ColorDeltaVector::ColorDeltaVector()
{
  TJvColorDelta* Delta;
  
  Delta = new TJvColorDelta();
  Delta->ColorID = csRGB;
  Delta->AxisRed->Constituents[axIndex0]->Value = 100;
  Delta->AxisRed->Constituents[axIndex0]->SaturationMethod = smRange;
  Delta->AxisRed->Constituents[axIndex1]->Value = 0;
  Delta->AxisRed->Constituents[axIndex1]->SaturationMethod = smRange;
  Delta->AxisRed->Constituents[axIndex2]->Value = 0;
  Delta->AxisRed->Constituents[axIndex2]->SaturationMethod = smRange;
  Delta->AxisGreen->Constituents[axIndex0]->Value = 0;
  Delta->AxisGreen->Constituents[axIndex0]->SaturationMethod = smRange;
  Delta->AxisGreen->Constituents[axIndex1]->Value = 0;
  Delta->AxisGreen->Constituents[axIndex1]->SaturationMethod = smRange;
  Delta->AxisGreen->Constituents[axIndex2]->Value = 0;
  Delta->AxisGreen->Constituents[axIndex2]->SaturationMethod = smRange;
  Delta->AxisBlue->Constituents[axIndex0]->Value = 0;
  Delta->AxisBlue->Constituents[axIndex0]->SaturationMethod = smRange;
  Delta->AxisBlue->Constituents[axIndex1]->Value = 0;
  Delta->AxisBlue->Constituents[axIndex1]->SaturationMethod = smRange;
  Delta->AxisBlue->Constituents[axIndex2]->Value = 50;
  Delta->AxisBlue->Constituents[axIndex2]->SaturationMethod = smRange;
  push_back(Delta);

  Delta = new TJvColorDelta();
  Delta->ColorID = csHLS;
  Delta->AxisRed->Constituents[axIndex0]->Value = 0;
  Delta->AxisRed->Constituents[axIndex0]->SaturationMethod = smRange;
  Delta->AxisRed->Constituents[axIndex1]->Value = 0;
  Delta->AxisRed->Constituents[axIndex1]->SaturationMethod = smRange;
  Delta->AxisRed->Constituents[axIndex2]->Value = 0;
  Delta->AxisRed->Constituents[axIndex2]->SaturationMethod = smRange;
  Delta->AxisGreen->Constituents[axIndex0]->Value = 40;
  Delta->AxisGreen->Constituents[axIndex0]->SaturationMethod = smRange;
  Delta->AxisGreen->Constituents[axIndex1]->Value = 0;
  Delta->AxisGreen->Constituents[axIndex1]->SaturationMethod = smRange;
  Delta->AxisGreen->Constituents[axIndex2]->Value = 0;
  Delta->AxisGreen->Constituents[axIndex2]->SaturationMethod = smRange;
  Delta->AxisBlue->Constituents[axIndex0]->Value = 0;
  Delta->AxisBlue->Constituents[axIndex0]->SaturationMethod = smRange;
  Delta->AxisBlue->Constituents[axIndex1]->Value = 0;
  Delta->AxisBlue->Constituents[axIndex1]->SaturationMethod = smRange;
  Delta->AxisBlue->Constituents[axIndex2]->Value = 0;
  Delta->AxisBlue->Constituents[axIndex2]->SaturationMethod = smRange;
  push_back(Delta);

  Delta = new TJvColorDelta();
  Delta->ColorID = csHSV;
  Delta->AxisRed->Constituents[axIndex0]->Value = 0;
  Delta->AxisRed->Constituents[axIndex0]->SaturationMethod = smRange;
  Delta->AxisRed->Constituents[axIndex1]->Value = -176;
  Delta->AxisRed->Constituents[axIndex1]->SaturationMethod = smRange;
  Delta->AxisRed->Constituents[axIndex2]->Value = -180;
  Delta->AxisRed->Constituents[axIndex2]->SaturationMethod = smRange;
  Delta->AxisGreen->Constituents[axIndex0]->Value = 0;
  Delta->AxisGreen->Constituents[axIndex0]->SaturationMethod = smRange;
  Delta->AxisGreen->Constituents[axIndex1]->Value = 0;
  Delta->AxisGreen->Constituents[axIndex1]->SaturationMethod = smRange;
  Delta->AxisGreen->Constituents[axIndex2]->Value = 0;
  Delta->AxisGreen->Constituents[axIndex2]->SaturationMethod = smRange;
  Delta->AxisBlue->Constituents[axIndex0]->Value = 0;
  Delta->AxisBlue->Constituents[axIndex0]->SaturationMethod = smRange;
  Delta->AxisBlue->Constituents[axIndex1]->Value = 0;
  Delta->AxisBlue->Constituents[axIndex1]->SaturationMethod = smRange;
  Delta->AxisBlue->Constituents[axIndex2]->Value = 0;
  Delta->AxisBlue->Constituents[axIndex2]->SaturationMethod = smRange;
  push_back(Delta);

  Delta = new TJvColorDelta();
  Delta->ColorID = csYUV;
  Delta->AxisRed->Constituents[axIndex0]->Value = 0;
  Delta->AxisRed->Constituents[axIndex0]->SaturationMethod = smRange;
  Delta->AxisRed->Constituents[axIndex1]->Value = 38;
  Delta->AxisRed->Constituents[axIndex1]->SaturationMethod = smRange;
  Delta->AxisRed->Constituents[axIndex2]->Value = -100;
  Delta->AxisRed->Constituents[axIndex2]->SaturationMethod = smRange;
  Delta->AxisGreen->Constituents[axIndex0]->Value = 0;
  Delta->AxisGreen->Constituents[axIndex0]->SaturationMethod = smRange;
  Delta->AxisGreen->Constituents[axIndex1]->Value = 168;
  Delta->AxisGreen->Constituents[axIndex1]->SaturationMethod = smRange;
  Delta->AxisGreen->Constituents[axIndex2]->Value = 0;
  Delta->AxisGreen->Constituents[axIndex2]->SaturationMethod = smRange;
  Delta->AxisBlue->Constituents[axIndex0]->Value = 0;
  Delta->AxisBlue->Constituents[axIndex0]->SaturationMethod = smRange;
  Delta->AxisBlue->Constituents[axIndex1]->Value = 0;
  Delta->AxisBlue->Constituents[axIndex1]->SaturationMethod = smRange;
  Delta->AxisBlue->Constituents[axIndex2]->Value = 0;
  Delta->AxisBlue->Constituents[axIndex2]->SaturationMethod = smRange;
  push_back(Delta);

  Delta = new TJvColorDelta();
  Delta->ColorID = csHLS;
  Delta->AxisRed->Constituents[axIndex0]->Value = 0;
  Delta->AxisRed->Constituents[axIndex0]->SaturationMethod = smRange;
  Delta->AxisRed->Constituents[axIndex1]->Value = -30;
  Delta->AxisRed->Constituents[axIndex1]->SaturationMethod = smRange;
  Delta->AxisRed->Constituents[axIndex2]->Value = 0;
  Delta->AxisRed->Constituents[axIndex2]->SaturationMethod = smRange;
  Delta->AxisGreen->Constituents[axIndex0]->Value = 0;
  Delta->AxisGreen->Constituents[axIndex0]->SaturationMethod = smRange;
  Delta->AxisGreen->Constituents[axIndex1]->Value = -30;
  Delta->AxisGreen->Constituents[axIndex1]->SaturationMethod = smRange;
  Delta->AxisGreen->Constituents[axIndex2]->Value = 0;
  Delta->AxisGreen->Constituents[axIndex2]->SaturationMethod = smRange;
  Delta->AxisBlue->Constituents[axIndex0]->Value = 0;
  Delta->AxisBlue->Constituents[axIndex0]->SaturationMethod = smRange;
  Delta->AxisBlue->Constituents[axIndex1]->Value = -30;
  Delta->AxisBlue->Constituents[axIndex1]->SaturationMethod = smRange;
  Delta->AxisBlue->Constituents[axIndex2]->Value = 0;
  Delta->AxisBlue->Constituents[axIndex2]->SaturationMethod = smRange;
  push_back(Delta);

  Delta = new TJvColorDelta();
  Delta->ColorID = csXYZ;
  Delta->AxisRed->Constituents[axIndex0]->Value = 0;
  Delta->AxisRed->Constituents[axIndex0]->SaturationMethod = smRange;
  Delta->AxisRed->Constituents[axIndex1]->Value = 0;
  Delta->AxisRed->Constituents[axIndex1]->SaturationMethod = smRange;
  Delta->AxisRed->Constituents[axIndex2]->Value = 0;
  Delta->AxisRed->Constituents[axIndex2]->SaturationMethod = smRange;
  Delta->AxisGreen->Constituents[axIndex0]->Value = 0;
  Delta->AxisGreen->Constituents[axIndex0]->SaturationMethod = smRange;
  Delta->AxisGreen->Constituents[axIndex1]->Value = 0;
  Delta->AxisGreen->Constituents[axIndex1]->SaturationMethod = smRange;
  Delta->AxisGreen->Constituents[axIndex2]->Value = 0;
  Delta->AxisGreen->Constituents[axIndex2]->SaturationMethod = smRange;
  Delta->AxisBlue->Constituents[axIndex0]->Value = 80;
  Delta->AxisBlue->Constituents[axIndex0]->SaturationMethod = smRange;
  Delta->AxisBlue->Constituents[axIndex1]->Value = 0;
  Delta->AxisBlue->Constituents[axIndex1]->SaturationMethod = smRange;
  Delta->AxisBlue->Constituents[axIndex2]->Value = 0;
  Delta->AxisBlue->Constituents[axIndex2]->SaturationMethod = smRange;
  push_back(Delta);
}

ColorDeltaVector::~ColorDeltaVector()
{
  ColorDeltaVector::iterator it;

  for(it=begin(); it!=end(); it++)
    delete *it;
}
