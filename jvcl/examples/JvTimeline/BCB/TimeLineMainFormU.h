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

#ifndef TimeLineMainFormUH
#define TimeLineMainFormUH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <CheckLst.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include <JvTMTimeLine.hpp>
#include "JvComponent.hpp"
#include "JvExControls.hpp"
#include "JvTimeLine.hpp"
//---------------------------------------------------------------------------
typedef struct sTItemData
{
   AnsiString Text;
} TItemData  ;

typedef TItemData* pItemData;

class TTimelineMainForm : public TForm
{
__published:	// IDE-managed Components
        TSplitter *Splitter1;
        TStatusBar *StatusBar1;
        TJvTimeLine *TimeLine1;
        TPanel *Panel2;
        TLabel *Label6;
        TLabel *Label8;
        TGroupBox *GroupBox1;
        TLabel *Label4;
        TLabel *Label1;
        TLabel *Label2;
        TLabel *Label3;
        TButton *btnAdd;
        TEdit *edCaption;
        TEdit *edImIndex;
        TDateTimePicker *dtpItemDate;
        TUpDown *udImIndex;
        TEdit *edLevel;
        TUpDown *udLevel;
        TButton *btnColor;
        TGroupBox *GroupBox2;
        TCheckBox *chkMonths;
        TCheckBox *chkMulti;
        TCheckBox *chkNoImages;
        TCheckBox *chkWidthAs;
        TCheckBox *chkAutoSize;
        TCheckBox *chkSupport;
        TCheckBox *chkLarge;
        TCheckBox *chkOwnerDraw;
        TButton *btnAuto;
        TCheckBox *chkReset;
        TCheckBox *chkComplete;
        TCheckBox *chkFlat;
        TCheckBox *chkHelpYear;
        TGroupBox *GroupBox3;
        TLabel *Label5;
        TLabel *Label7;
        TLabel *Label9;
        TButton *btnYrFont;
        TButton *btnFont;
        TEdit *edYrSize;
        TUpDown *udYrSize;
        TEdit *edItemHeight;
        TUpDown *udItemHeight;
        TDateTimePicker *dtpFirstDate;
        TButton *ColorBtn;
        TComboBox *cbDragging;
        TButton *btnSave;
        TButton *btnLoad;
        TImageList *ImageList1;
        TPopupMenu *PopupMenu1;
        TMenuItem *Changecaption1;
        TMenuItem *Move1;
        TMenuItem *remove1;
        TMenuItem *Disable1;
        TMenuItem *N2;
        TMenuItem *Up1;
        TMenuItem *Down1;
        TMenuItem *N1;
        TMenuItem *Notes1;
        TImageList *ImageList2;
        void __fastcall cbDraggingChange(TObject *Sender);
        void __fastcall FormResize(TObject *Sender);
        void __fastcall btnColorClick(TObject *Sender);
        void __fastcall btnAddClick(TObject *Sender);
        void __fastcall btnAutoClick(TObject *Sender);
        void __fastcall chkMonthsClick(TObject *Sender);
        void __fastcall chkMultiClick(TObject *Sender);
        void __fastcall chkNoImagesClick(TObject *Sender);
        void __fastcall chkAutoSizeClick(TObject *Sender);
        void __fastcall chkSupportClick(TObject *Sender);
        void __fastcall chkLargeClick(TObject *Sender);
        void __fastcall chkOwnerDrawClick(TObject *Sender);
        void __fastcall chkFlatClick(TObject *Sender);
        void __fastcall udYrSizeClick(TObject *Sender, TUDBtnType Button);
        void __fastcall udItemHeightClick(TObject *Sender,
          TUDBtnType Button);
        void __fastcall dtpFirstDateChange(TObject *Sender);
        void __fastcall btnFontClick(TObject *Sender);
        void __fastcall btnYrFontClick(TObject *Sender);
        void __fastcall ColorBtnClick(TObject *Sender);
        void __fastcall chkHelpYearClick(TObject *Sender);
        void __fastcall btnSaveClick(TObject *Sender);
        void __fastcall btnLoadClick(TObject *Sender);
        void __fastcall TimeLine1ItemClick(TObject *Sender,
          TJvTimeItem *Item);
        void __fastcall TimeLine1MouseMove(TObject *Sender,
          TShiftState Shift, int X, int Y);
        void __fastcall TimeLine1MouseDown(TObject *Sender,
          TMouseButton Button, TShiftState Shift, int X, int Y);
        void __fastcall TimeLine1Click(TObject *Sender);
        void __fastcall TimeLine1DragDrop(TObject *Sender, TObject *Source,
          int X, int Y);
        void __fastcall TimeLine1DragOver(TObject *Sender, TObject *Source,
          int X, int Y, TDragState State, bool &Accept);
        void __fastcall TimeLine1DrawItem(TObject *Sender, TCanvas *Canvas,
          TJvTimeItem *Item, TRect &R);
        void __fastcall TimeLine1ItemMoved(TObject *Sender,
          TJvTimeItem *Item, TDateTime &NewStartDate, int &NewLevel);
        void __fastcall TimeLine1LoadItem(TObject *Sender,
          TJvTimeItem *Item, TStream *Stream);
        void __fastcall Changecaption1Click(TObject *Sender);
        void __fastcall Move1Click(TObject *Sender);
        void __fastcall remove1Click(TObject *Sender);
        void __fastcall Disable1Click(TObject *Sender);
        void __fastcall Up1Click(TObject *Sender);
        void __fastcall Down1Click(TObject *Sender);
        void __fastcall Notes1Click(TObject *Sender);
        void __fastcall chkWidthAsClick(TObject *Sender);
private:	// User declarations
        TColor FCurColor;
public:		// User declarations
        __fastcall TTimelineMainForm(TComponent* Owner);
protected:
        virtual __fastcall ~TTimelineMainForm(void);
};
//---------------------------------------------------------------------------
extern PACKAGE TTimelineMainForm *TimelineMainForm;
//---------------------------------------------------------------------------
#endif
