/******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 att users dott sourceforge dott net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

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

#ifndef TMTimeLineMainFormUH
#define TMTimeLineMainFormUH
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
//---------------------------------------------------------------------------
class TTMTimeLineMainForm : public TForm
{
__published:	// IDE-managed Components
        TSplitter *Splitter1;
        TPanel *Panel1;
        TLabel *Label6;
        TLabel *Label7;
        TLabel *Label18;
        TLabel *Label19;
        TLabel *Label20;
        TListView *lvImages;
        TGroupBox *gbDates;
        TLabel *Label2;
        TLabel *Label3;
        TLabel *Label4;
        TLabel *Label5;
        TDateTimePicker *dtpFirstDate;
        TDateTimePicker *dtpSelDate;
        TEdit *edImageNo;
        TUpDown *udImageNo;
        TDateTimePicker *dtpImageDate;
        TButton *btnAdd;
        TGroupBox *gbAppearance;
        TLabel *Label13;
        TCheckBox *chkReadOnly;
        TCheckBox *chkFlat;
        TCheckBox *chkRClick;
        TCheckBox *chkEnabled;
        TCheckBox *chkShowToday;
        TCheckBox *chkShowWeeks;
        TCheckBox *chkShowMonths;
        TCheckListBox *lbObjFontStyle;
        TCheckBox *chkShowTodayIcon;
        TGroupBox *gbMisc;
        TButton *btnLoad;
        TButton *btnSave;
        TGroupBox *gbFonts;
        TButton *btnFont;
        TButton *btnColor;
        TButton *btnMonthFont;
        TButton *btnTodayColor;
        TButton *btnLineColor;
        TButton *btnPenColor;
        TGroupBox *gbWidths;
        TLabel *Label1;
        TLabel *Label8;
        TLabel *Label9;
        TLabel *Label10;
        TLabel *Label12;
        TLabel *Label11;
        TLabel *Label14;
        TLabel *Label15;
        TLabel *Label16;
        TLabel *Label17;
        TEdit *edDayWidth;
        TUpDown *udDayWidth;
        TEdit *edPenWidth;
        TUpDown *udPenWidth;
        TEdit *edScrollSmall;
        TUpDown *udScrollSmall;
        TEdit *edScrollLarge;
        TUpDown *udScrollLarge;
        TEdit *edButtonWidth;
        TUpDown *udButtonWidth;
        TStatusBar *StatusBar;
        TPopupMenu *popTimeLine;
        TMenuItem *mnuEditMemo;
        TMenuItem *mnuInsertImage;
        TMenuItem *mnuRemoveImage;
        TMenuItem *N1;
        TMenuItem *mnuToday;
        TMenuItem *mnuGotoDate;
        TImageList *il16;
        void __fastcall udScrollSmallClick(TObject *Sender, TUDBtnType Button);
        void __fastcall StatusBarResize(TObject *Sender);
        void __fastcall mnuEditMemoClick(TObject *Sender);
        void __fastcall mnuInsertImageClick(TObject *Sender);
        void __fastcall mnuRemoveImageClick(TObject *Sender);
        void __fastcall mnuTodayClick(TObject *Sender);
        void __fastcall mnuGotoDateClick(TObject *Sender);
        void __fastcall udScrollLargeClick(TObject *Sender,
          TUDBtnType Button);
        void __fastcall udDayWidthClick(TObject *Sender,
          TUDBtnType Button);
        void __fastcall udPenWidthClick(TObject *Sender,
          TUDBtnType Button);
        void __fastcall udButtonWidthClick(TObject *Sender,
          TUDBtnType Button);
        void __fastcall chkReadOnlyClick(TObject *Sender);
        void __fastcall chkFlatClick(TObject *Sender);
        void __fastcall chkRClickClick(TObject *Sender);
        void __fastcall chkEnabledClick(TObject *Sender);
        void __fastcall chkShowTodayClick(TObject *Sender);
        void __fastcall chkShowTodayIconClick(TObject *Sender);
        void __fastcall chkShowWeeksClick(TObject *Sender);
        void __fastcall chkShowMonthsClick(TObject *Sender);
        void __fastcall btnColorClick(TObject *Sender);
        void __fastcall btnLineColorClick(TObject *Sender);
        void __fastcall btnFontClick(TObject *Sender);
        void __fastcall btnTodayColorClick(TObject *Sender);
        void __fastcall btnMonthFontClick(TObject *Sender);
        void __fastcall btnPenColorClick(TObject *Sender);
        void __fastcall btnLoadClick(TObject *Sender);
        void __fastcall btnSaveClick(TObject *Sender);
        void __fastcall btnAddClick(TObject *Sender);
        void __fastcall dtpFirstDateChange(TObject *Sender);
        void __fastcall dtpSelDateChange(TObject *Sender);
        void __fastcall lbObjFontStyleClickCheck(TObject *Sender);
        void __fastcall lvImagesSelectItem(TObject *Sender,
          TListItem *Item, bool Selected);
        void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
        void __fastcall FormMouseWheelDown(TObject *Sender,
          TShiftState Shift, TPoint &MousePos, bool &Handled);
        void __fastcall FormMouseWheelUp(TObject *Sender,
          TShiftState Shift, TPoint &MousePos, bool &Handled);
private:	// User declarations
        TJvTMTimeline* JvTimeLine1;
        void __fastcall DoDateChange(TObject* Sender);
        void __fastcall DoClick(TObject* Sender);
        void __fastcall DoDblClick(TObject* Sender);
        void __fastcall DoObjectLoad(TObject* Sender, TStream* Stream, TObject *&AObject);
        void __fastcall DoObjectSave(TObject * Sender, TStream * Stream, const TObject * AObject);
public:		// User declarations                                     
        __fastcall TTMTimeLineMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TTMTimeLineMainForm *TMTimeLineMainForm;
//---------------------------------------------------------------------------
#endif
