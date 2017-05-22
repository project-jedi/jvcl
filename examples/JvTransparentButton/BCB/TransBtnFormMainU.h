/******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):
   korecek: translation from Delphi to BCB

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

#ifndef TransBtnFormMainUH
#define TransBtnFormMainUH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvButton.hpp"
#include "JvComponent.hpp"
#include "JvExControls.hpp"
#include "JvTransparentButton.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Graphics.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TTransBtnFormMain : public TForm
{
__published:	// IDE-managed Components
        TLabel *Label1;
        TLabel *Label3;
        TJvTransparentButton *JvTransparentButton21;
        TPageControl *PageControl1;
        TTabSheet *TabSheet1;
        TImage *Image3;
        TJvTransparentButton *TransparentButton1;
        TJvTransparentButton *TransparentButton2;
        TJvTransparentButton *TransparentButton3;
        TJvTransparentButton *TransparentButton7;
        TJvTransparentButton *TransparentButton8;
        TJvTransparentButton *TransparentButton9;
        TJvTransparentButton *TransparentButton6;
        TJvTransparentButton *TransparentButton10;
        TJvTransparentButton *TransparentButton11;
        TJvTransparentButton *TransparentButton5;
        TJvTransparentButton *TransparentButton15;
        TJvTransparentButton *TransparentButton12;
        TJvTransparentButton *TransparentButton13;
        TJvTransparentButton *TransparentButton14;
        TTabSheet *TabSheet2;
        TImage *Image2;
        TJvTransparentButton *JvTransparentButton22;
        TJvTransparentButton *JvTransparentButton23;
        TJvTransparentButton *JvTransparentButton24;
        TJvTransparentButton *JvTransparentButton25;
        TJvTransparentButton *JvTransparentButton26;
        TJvTransparentButton *JvTransparentButton27;
        TJvTransparentButton *JvTransparentButton28;
        TButton *Button1;
        TPopupMenu *PopupMenu1;
        TMenuItem *Open1;
        TMenuItem *Save1;
        TMenuItem *Allgroups1;
        TMenuItem *Selectedgroups1;
        TMenuItem *Delete1;
        TMenuItem *SaveAs1;
        TMenuItem *N1;
        TMenuItem *Exit1;
        TMenuItem *Previous1;
        TMenuItem *N2;
        TMenuItem *Exit2;
        TImageList *ImageList1;
        void __fastcall TransparentButton1Click(TObject *Sender);
        void __fastcall TransparentButton1MouseDown(TObject *Sender,
          TMouseButton Button, TShiftState Shift, int X, int Y);
        void __fastcall TransparentButton1MouseEnter(TObject *Sender);
        void __fastcall TransparentButton1MouseLeave(TObject *Sender);
        void __fastcall TransparentButton1MouseUp(TObject *Sender,
          TMouseButton Button, TShiftState Shift, int X, int Y);
        void __fastcall Button1Click(TObject *Sender);
        void __fastcall TransparentButton3Click(TObject *Sender);
        void __fastcall TransparentButton6Click(TObject *Sender);
        void __fastcall TransparentButton10Click(TObject *Sender);
        void __fastcall Exit2Click(TObject *Sender);
        void __fastcall JvTransparentButton28Click(TObject *Sender);
        void __fastcall JvTransparentButton26Click(TObject *Sender);
        void __fastcall FormActivate(TObject *Sender);
        void __fastcall FormKeyPress(TObject *Sender, char &Key);
        void __fastcall PageControl1Change(TObject *Sender);
private:
        AnsiString GetOS(void);	// User declarations
        bool Activated;
public:		// User declarations
        __fastcall TTransBtnFormMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TTransBtnFormMain *TransBtnFormMain;
//---------------------------------------------------------------------------
#endif
