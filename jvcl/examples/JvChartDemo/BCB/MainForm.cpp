/******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2004 Project JEDI

 Original author: Olivier Sannier (obones@meloo.com)

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************/
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvChart"
#pragma link "JvComponent"
#pragma link "JvExControls"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::ButtonNewValuesClick(TObject *Sender)
{
  int I;
  int nValueCount;
  double hgt, hg0;

  Randomize;
  nValueCount = 20;
  Chart->ResetGraphModule();

  for (I = 0; I < nValueCount; I++)
  {
    if (I > 0)
    {
      hgt = (random((I % 5) * 250) * 5 + 7500);
      hg0 = random((I % 3) * 650) + 1003;
    }
    else
    {
      hgt = 7000; // first element must be fixed for debug 0/1 offset purposes
      hg0 = 1000;
    }
    // Set Data.Value[Pen, Series] = dataValue ...
    Chart->Data->Value[0][I] = hgt;
    Chart->Data->Value[1][I] = hg0;
    Chart->Data->Value[2][I] = hgt - hg0;
    Chart->Options->XLegends->Add(FormatDateTime("yyyy-mm-dd", (Now() - 3.0) + (I / 16)));
  }
  {
    TJvChartOptions* opts = Chart->Options;

    opts->Title = "Chart Title";
    opts->XAxisHeader = "Date/Time";
    opts->YAxisHeader = "Readings (ug/m3)";
    opts->PenCount = 3;

    opts->PenLegends->Clear();
    opts->PenLegends->Add("HgT");
    opts->PenLegends->Add("Hg0");
    opts->PenLegends->Add("Hg2+");

    opts->PenUnit->Clear();
    opts->PenUnit->Add("ug/m3");
    opts->PenUnit->Add("ug/m3");
    opts->PenUnit->Add("ug/m3");

    //opts->ShowLegend = true;
    
    opts->ChartKind = ckChartLine;
  }
  Chart->AutoFormatGraph();
   //Chart->ResizeChartCanvas();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::SpeedButton1Click(TObject *Sender)
{
  Chart->GraphToClipboard();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormResize(TObject *Sender)
{
  if (Chart == NULL)
    Chart->ResizeChartCanvas();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::SpeedButton2Click(TObject *Sender)
{
  Chart->PrintGraph();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::SpeedButton3Click(TObject *Sender)
{
  Chart->Options->ChartKind = ckChartBar;
   // ShowAsBar;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::SpeedButton4Click(TObject *Sender)
{
//   Chart.ShowAsLine; this show it without marks
  Chart->Options->ChartKind = ckChartLineWithMarkers;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::SpeedButton5Click(TObject *Sender)
{
  Chart->Options->ChartKind = ckChartStackedBarAverage;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::SpeedButton6Click(TObject *Sender)
{
  Chart->Options->ChartKind = ckChartStackedBar;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::SpeedButton7Click(TObject *Sender)
{
  Chart->PivotData(); // TODO: CRASH.
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::SpeedButton8Click(TObject *Sender)
{
  //Chart.ShowAsBarWithAve;
  Chart->Options->ChartKind = ckChartBarAverage;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::SpeedButton10Click(TObject *Sender)
{
  // Chart.ShowAsPie;
  Chart->Options->ChartKind = ckChartPieChart;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::SpeedButton12Click(TObject *Sender)
{
  //Chart.ShowAsMark;
  Chart->Options->ChartKind = ckChartMarkers;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::SpeedButton11Click(TObject *Sender)
{
  // Get the current font for the Header text...
  FontDialog1->Font->Assign(Chart->Options->AxisFont);

  if (FontDialog1->Execute())
    // Set the font for the Header text...
    Chart->Options->AxisFont = FontDialog1->Font;
  Chart->PlotGraph();
  Chart->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::SpeedButton14Click(TObject *Sender)
{
  Chart->Options->ChartKind = ckChartDeltaAverage;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  if (Chart != NULL)
    //   Chart.ShowAsLineWithMark;
    ButtonNewValuesClick(Sender);
}
//---------------------------------------------------------------------------
