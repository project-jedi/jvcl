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

#ifndef fThreadH
#define fThreadH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvComponent.hpp"
#include "JvThread.hpp"
#include "JvThreadDialog.hpp"
#include "JvComponentBase.hpp"
#include <ExtCtrls.hpp>
#include "JvExMask.hpp"
#include "JvSpin.hpp"
#include <Mask.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
        TPanel *Panel1;
        TJvThread *JvThread1;
        TJvThread *JvThread2;
        TJvThreadSimpleDialog *JvThreadSimpleDialog1;
        TJvThreadAnimateDialog *JvThreadAnimateDialog1;
        TTimer *tmrStatus;
        TGroupBox *GroupBox1;
        TButton *btnStartJob1;
        TButton *btnStartJob1SimpleDlg;
        TGroupBox *GroupBox2;
        TButton *btnStartJob2;
        TButton *btnStartJob2AnimatedDlg;
        TCheckBox *cbExclusive2;
        TJvSpinEdit *seMaxCount2;
        TLabel *Label1;
        TLabel *Label3;
        TComboBox *cbPriority2;
        TButton *btnTerminate2;
        TCheckBox *cbExclusive1;
        TJvSpinEdit *seMaxCount1;
        TLabel *Label2;
        TLabel *Label4;
        TComboBox *cbPriority1;
        TButton *btnTerminate1;
        TGroupBox *GroupBox3;
        TMemo *Memo;
        TRadioButton *rbExOnBegin;
        TRadioButton *rbExOnBeforeResume;
        TRadioButton *rbExOnFinish;
        TRadioButton *rbExOnFinishAll;
        TRadioButton *rbNoEx;
        TCheckBox *cbAutoStart2;
        TCheckBox *cbAutoStart1;
        TTimer *tmrAuto;
        TButton *btnSuspendAll1;
        TButton *btnResumeAll1;
        TButton *btnSuspendAll2;
        TButton *btnResumeAll2;
        TCheckBox *cbShowMsgBeforeExit2;
        TPanel *Panel2;
        TLabel *lbCount1;
        TLabel *lbStats1;
        TPanel *Panel3;
        TLabel *lbStats2;
        TLabel *lbCount2;
        TCheckBox *cbDeferredExecution2;
        TCheckBox *cbDeferredExecution1;
        TGroupBox *GroupBox4;
        TLabel *Label6;
        TButton *btnSuspendItself;
        TButton *btnRaiseException;
        TCheckBox *cbJobTerminated1;
        TCheckBox *cbJobTerminated2;
        TLabel *Label5;
        TLabel *Label7;
        TButton *btnShowState;
        TCheckBox *cbDeferredDeletion2;
        TCheckBox *cbDeferredDeletion1;
        TButton *btnRemoveZombie;
    TButton *btnExecuteAndWait;
    TButton *btnDynamicCreation;
    TJvThreadSimpleDialog *JvThreadSimpleDialog3;
        void __fastcall JvThread1Execute(TObject *Sender, Pointer Params);
        void __fastcall JvThread2Execute(TObject *Sender, Pointer Params);
        void __fastcall btnStartJob1Click(TObject *Sender);
        void __fastcall btnStartJob2Click(TObject *Sender);
        void __fastcall btnStartJob1SimpleDlgClick(TObject *Sender);
        void __fastcall btnStartJob2AnimatedDlgClick(TObject *Sender);
        void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
        void __fastcall JvThreadBeforeResume(TObject *Sender);
        void __fastcall JvThread2Finish(TObject *Sender);
        void __fastcall JvThread2FinishAll(TObject *Sender);
        void __fastcall MemoDblClick(TObject *Sender);
        void __fastcall btnTerminate2Click(TObject *Sender);
        void __fastcall cbExclusive1Click(TObject *Sender);
        void __fastcall tmrStatusTimer(TObject *Sender);
        void __fastcall cbPriority2Change(TObject *Sender);
        void __fastcall cbExclusive2Click(TObject *Sender);
        void __fastcall seMaxCount2Change(TObject *Sender);
        void __fastcall seMaxCount1Change(TObject *Sender);
        void __fastcall cbPriority1Change(TObject *Sender);
        void __fastcall btnTerminate1Click(TObject *Sender);
        void __fastcall JvThreadBegin(TObject *Sender);
        void __fastcall JvThread1Finish(TObject *Sender);
        void __fastcall JvThread1FinishAll(TObject *Sender);
        void __fastcall tmrAutoTimer(TObject *Sender);
        void __fastcall btnSuspendAll1Click(TObject *Sender);
        void __fastcall btnResumeAll1Click(TObject *Sender);
        void __fastcall btnSuspendAll2Click(TObject *Sender);
        void __fastcall btnResumeAll2Click(TObject *Sender);
        void __fastcall btnRaiseExceptionClick(TObject *Sender);
        void __fastcall cbDeferredExecution1Click(TObject *Sender);
        void __fastcall cbDeferredExecution2Click(TObject *Sender);
        void __fastcall btnSuspendItselfClick(TObject *Sender);
        void __fastcall btnShowStateClick(TObject *Sender);
        void __fastcall cbDeferredDeletion1Click(TObject *Sender);
        void __fastcall cbDeferredDeletion2Click(TObject *Sender);
        void __fastcall btnRemoveZombieClick(TObject *Sender);
    void __fastcall btnExecuteAndWaitClick(TObject *Sender);
    void __fastcall btnDynamicCreationClick(TObject *Sender);
private:	// User declarations
protected:
public:		// User declarations
        int ThreadID1, Value1;
        int ThreadID2, Value2;

        __fastcall TForm1(TComponent* Owner);
        void __fastcall Stats1(void);
        void __fastcall Stats2(void);
        void __fastcall DynamicExecute(TObject *Sender, Pointer Params);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
