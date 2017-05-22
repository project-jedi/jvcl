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
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvCombobox.hpp"
#include "JvDriveCtrls.hpp"
#include "JvExStdCtrls.hpp"
#include "JvListBox.hpp"
#include "JvAni.hpp"
#include <FileCtrl.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
        TJvDriveCombo *JvDriveCombo1;
        TJvDirectoryListBox *JvDirectoryListBox1;
        TJvFileListBox *JvFileListBox1;
        TButton *btnSave;
        TSaveDialog *SaveDialog1;
        TPanel *Panel1;
        TPanel *Panel2;
        TPanel *Panel3;
        TMemo *Memo1;
        TImage *Image1;
  TImage *ImageIcons;
  TImage *ImageFrames;
  TLabel *Label1;
  TLabel *Frames;
        void __fastcall FormActivate(TObject *Sender);
  void __fastcall JvFileListBox1Click(TObject *Sender);
  void __fastcall btnSaveClick(TObject *Sender);
private:	// User declarations
        bool Activated;
public:		// User declarations
        __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
