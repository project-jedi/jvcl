//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvFullColorCtrls"
#pragma link "JvFullColorDialogs"
#pragma link "JvFullColorForm"
#pragma link "JvFullColorSpaces"
#pragma resource "*.dfm"
TfrmMain *frmMain;

void __fastcall TfrmMain::UpdateAllCaptions()
{
  int Index;

  for (Index = 0; Index < ControlCount; Index++)
    if (dynamic_cast<TJvFullColorLabel*>(Controls[Index]))
       UpdateCaption(dynamic_cast<TJvFullColorLabel*>(Controls[Index]));
}

void __fastcall TfrmMain::UpdateCaption(TJvFullColorLabel* ALabel)
{
  unsigned int Index;
  TColor LColor;

  TJvColorSpace* cspace = ColorSpaceManager()->ColorSpace[ColorSpaceManager()->GetColorSpaceID(ALabel->LabelColor)];
//  with ColorSpaceManager, ColorSpace[GetColorSpaceID(ALabel.LabelColor)] do
    if (cspace->ID == csDEF)
    {
      //with TJvDEFColorSpace(ColorSpace[csDEF]) do
      TJvDEFColorSpace* defcspace = dynamic_cast<TJvDEFColorSpace*>(ColorSpaceManager()->ColorSpace[csDEF]);
      {
        LColor = defcspace->ConvertToColor(ALabel->LabelColor);
        for (Index = 0; Index < defcspace->ColorCount; Index++)
        {
          if (defcspace->ColorValue[Index]==LColor)
          {
            ALabel->Caption = Format("%s : %s",ARRAYOFCONST((defcspace->ShortName,defcspace->ColorPrettyName[Index])));
            break;
          }

          if (Index == defcspace->ColorCount)
            ALabel->Caption = defcspace->ShortName+" : Invalid color";
        }
      }
    }
    else
      ALabel->Caption = Format("%s : %s=%d ; %s=%d ; %s=%d",
            ARRAYOFCONST((cspace->ShortName,
            cspace->AxisName[axIndex0],
            GetAxisValue(ALabel->LabelColor,axIndex0),
            cspace->AxisName[axIndex1],
            GetAxisValue(ALabel->LabelColor,axIndex1),
            cspace->AxisName[axIndex2],
            GetAxisValue(ALabel->LabelColor,axIndex2))));
}

void __fastcall TfrmMain::CreateLabel(TJvFullColorLabel* &LColorLabel, int& X, int& Y, TJvFullColor AFullColor)
{
  Y += JvFullColorLabel->Height+10;
  if (Y > ClientHeight)
  {
    Y = JvFullColorLabel->Top;
    X += JvFullColorLabel->Width+10;
  }
  LColorLabel = new TJvFullColorLabel(this);
  LColorLabel->Parent = this;
  LColorLabel->SetBounds(X,Y,JvFullColorLabel->Width,JvFullColorLabel->Height);
  LColorLabel->LabelColor = AFullColor;
  LColorLabel->OnDblClick = JvFullColorLabelDblClick;
}

//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}

#define Min(a, b)  (((a) < (b)) ? (a) : (b))
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  int Index;
  TJvDEFColorSpace* LDEFColorSpace;
  TJvFullColorLabel* LColorLabel;
  int X, Y;

  X = JvFullColorLabel->Left;
  Y = JvFullColorLabel->Top;

  {
    TJvColorSpaceManager* csman = ColorSpaceManager();
    LDEFColorSpace = dynamic_cast<TJvDEFColorSpace*>(csman->ColorSpace[csDEF]);
    for (Index = 1; Index <= Min(LDEFColorSpace->ColorCount-1,9); Index++)
      CreateLabel(LColorLabel, X, Y, csman->ConvertFromColor(LDEFColorSpace->ColorValue[Index]));
    {
      TJvColorSpace* cspace = csman->ColorSpace[csHLS];
      CreateLabel(LColorLabel, X, Y, cspace->ConvertFromColor(clRed));
      CreateLabel(LColorLabel, X, Y, cspace->ConvertFromColor(clLime));
      CreateLabel(LColorLabel, X, Y, cspace->ConvertFromColor(clBlue));
      CreateLabel(LColorLabel, X, Y, cspace->ConvertFromColor(clYellow));
    }
    CreateLabel(LColorLabel, X, Y, csman->ColorSpace[csYUV]->ConvertFromColor(clWhite));
    CreateLabel(LColorLabel, X, Y, csman->ColorSpace[csYCC]->ConvertFromColor(clPurple));
    CreateLabel(LColorLabel, X, Y, csman->ColorSpace[csHSV]->ConvertFromColor(clAqua));
    CreateLabel(LColorLabel, X, Y, csman->ColorSpace[csYIQ]->ConvertFromColor(clOlive));
    CreateLabel(LColorLabel, X, Y, csman->ColorSpace[csLAB]->ConvertFromColor(clMaroon));
    CreateLabel(LColorLabel, X, Y, csman->ColorSpace[csDEF]->ConvertFromColor(clAppWorkSpace));
  }
  UpdateAllCaptions();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::JvFullColorLabelDblClick(TObject *Sender)
{
  TJvFullColorLabel* label = dynamic_cast<TJvFullColorLabel*>(Sender);

  JvFullColorDialog->FullColor = label->LabelColor;
  JvFullColorDialog->Tag = reinterpret_cast<int>(Sender);
  if (JvFullColorDialog->Execute())
  {
    label->LabelColor = JvFullColorDialog->FullColor;
    UpdateCaption(label);
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::JvFullColorDialogApply(TObject *Sender,
      TJvFullColor AFullColor)
{
  TJvFullColorDialog* dialog = dynamic_cast<TJvFullColorDialog*>(Sender);

  reinterpret_cast<TJvFullColorLabel*>(dialog->Tag)->LabelColor = dialog->FullColor;
  UpdateCaption(reinterpret_cast<TJvFullColorLabel*>(dialog->Tag));
}
//---------------------------------------------------------------------------

