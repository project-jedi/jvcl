/******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2004 Project JEDI

 Original author: Olivier Sannier (obones att altern dott org)

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

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <Controls.hpp>
#include <Classes.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "StatClasses.h"
#include "JvChart.hpp"
#include "JvComponent.hpp"
#include "JvExControls.hpp"
#include <Buttons.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TJvChart *Chart;
  TSplitter *Splitter1;
  TPanel *PanelTop;
  TSpeedButton *ButtonBarChart;
  TSpeedButton *ButtonLine;
  TSpeedButton *ButtonStackedBarAve;
  TSpeedButton *ButtonStackedBar;
  TSpeedButton *ButtonBarAve;
  TSpeedButton *ButtonPie;
  TSpeedButton *ButtonMarkers;
  TSpeedButton *ButtonDeltaAverage;
  TSpeedButton *ButtonLineMarker;
  TLabel *Label1;
  TListBox *ListBox1;
  TColorDialog *ColorDialog1;
  TFontDialog *FontDialog1;
  TMainMenu *MainMenu1;
  TMenuItem *Demo1;
  TMenuItem *SetHeaderFont1;
  TMenuItem *N2;
  TMenuItem *Scrolling1;
  TMenuItem *CopyToClipboard1;
  TMenuItem *Print1;
  TMenuItem *PrintOptions1;
  TMenuItem *N1;
  TMenuItem *Generatenewrandomvalues1;
  TMenuItem *ShowgapinLineChart1;
  TMenuItem *DateTimeAxisMode;
  TMenuItem *MenuSecondaryAxisMode;
  TMenuItem *N4;
  TMenuItem *ShowDataInListbox1;
  TMenuItem *LargeDataset576samples1;
  TMenuItem *Help1;
  TMenuItem *About1;
  TMenuItem *AboutJVCL301;
  TTimer *Timer1;
  TPrinterSetupDialog *PrinterSetupDialog1;
  TPrintDialog *PrintDialog1;
  void __fastcall FormResize(TObject *Sender);
  void __fastcall ButtonBarChartClick(TObject *Sender);
  void __fastcall ButtonLineClick(TObject *Sender);
  void __fastcall ButtonLineMarkerClick(TObject *Sender);
  void __fastcall ButtonStackedBarClick(TObject *Sender);
  void __fastcall ButtonStackedBarAveClick(TObject *Sender);
  void __fastcall ButtonBarAveClick(TObject *Sender);
  void __fastcall ButtonPieClick(TObject *Sender);
  void __fastcall ButtonMarkersClick(TObject *Sender);
  void __fastcall ButtonDeltaAverageClick(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall ShowgapinLineChart1Click(TObject *Sender);
  void __fastcall Print1Click(TObject *Sender);
  void __fastcall Generatenewrandomvalues1Click(TObject *Sender);
  void __fastcall CopyToClipboard1Click(TObject *Sender);
  void __fastcall SetHeaderFont1Click(TObject *Sender);
  void __fastcall About1Click(TObject *Sender);
  void __fastcall AboutJVCL301Click(TObject *Sender);
  void __fastcall Scrolling1Click(TObject *Sender);
  void __fastcall Timer1Timer(TObject *Sender);
  void __fastcall ShowDataInListbox1Click(TObject *Sender);
  void __fastcall LargeDataset576samples1Click(TObject *Sender);
  void __fastcall DateTimeAxisModeClick(TObject *Sender);
  void __fastcall PrintOptions1Click(TObject *Sender);
  void __fastcall MenuSecondaryAxisModeClick(TObject *Sender);
  void __fastcall ListBox1DblClick(TObject *Sender);
  void __fastcall ListBox1Click(TObject *Sender);
private:	// User declarations
    // Our waveform generator uses the following as state-variables:
   int FGenerationIndex;
   int Foo,Foo1,Foo2;
   double FHgt, FHg0, FHg2p;
   TStatArray FStatHgt,FStatHg0;
   double Fdt,Fds;

protected:
  void _Generate();
  void _StoreValue(int I);
  int _QAProblemScatter();
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
  void NewValues();
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
