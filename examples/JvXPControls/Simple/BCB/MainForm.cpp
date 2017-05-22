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
// $Id$
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvComponent"
#pragma link "JvExControls"
#pragma link "JvXPButtons"
#pragma link "JvXPCheckCtrls"
#pragma link "JvXPContainer"
#pragma link "JvXPCore"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}

DWORD RemoveTitleBar(HANDLE hWindow, bool Hide = True)
{
  RECT R;
  DWORD Result;

  Result = GetWindowLong(hWindow, GWL_STYLE);
  if (Hide)
    Result = Result & !WS_CAPTION;
  else
    Result = Result | WS_CAPTION;
  GetClientRect(hWindow, &R);
  SetWindowLong(hWindow, GWL_STYLE, Result);
  AdjustWindowRect(&R, Result, GetMenu(hWindow)!=0);
  SetWindowPos(hWindow, 0, 0, 0, (R.right - R.left), (R.bottom - R.top),
    SWP_NOMOVE | SWP_NOZORDER | SWP_FRAMECHANGED | SWP_NOSENDCHANGING);

  return Result;
}

//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  RemoveTitleBar(Handle);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::acBtn1Execute(TObject *Sender)
{
//  
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::acBtn3Execute(TObject *Sender)
{
//  
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnCloseClick(TObject *Sender)
{
  Close();  
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkOfficeStyleClick(TObject *Sender)
{
  styleOffice->Theme = static_cast<TJvXPTheme>(chkOfficeStyle->Checked);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkToggleEnableClick(TObject *Sender)
{
  acBtn1->Enabled = !chkToggleEnable->Checked;
  acBtn3->Enabled = !chkToggleEnable->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::cntHeaderMouseDown(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
{
  ReleaseCapture();
  Perform(WM_SYSCOMMAND, 0xF012, 0);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::cntHeaderPaint(TObject *Sender, TRect &Rect,
      TCanvas *ACanvas, TFont *AFont)
{
  int i;


    for (i = Rect.Top; i <= Rect.Bottom; i++)
    {
      ACanvas->Pen->Color = clGray;
      ACanvas->Rectangle(Rect.Left + 1, Rect.Top + i << 1, Rect.Right - 1,
        Rect.Top + i << 1 + 1);
    }
    ACanvas->Brush->Color = clBtnFace;
    ::DrawText(ACanvas->Handle, (" " + Application->Title + " ").c_str(), -1, &Rect, DT_SINGLELINE |
      DT_VCENTER | DT_CENTER | DT_END_ELLIPSIS);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::cntNetPanelPaint(TObject *Sender, TRect &Rect,
      TCanvas *ACanvas, TFont *AFont)
{
  TControl* Control;
  TColor EdgeColor;

  if (dynamic_cast<TControl*>(Sender))
  {
    Control = dynamic_cast<TControl*>(Sender);
    if (dynamic_cast<TForm*>(Control->Parent))
    {
      EdgeColor = dynamic_cast<TForm*>(Control->Parent)->Color;
      ACanvas->Pixels[0][0] = EdgeColor;
      ACanvas->Pixels[dynamic_cast<TControl*>(Control)->Width - 1][0] = EdgeColor;
    }
  }
}
//---------------------------------------------------------------------------
