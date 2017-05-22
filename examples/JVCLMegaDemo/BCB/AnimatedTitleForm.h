/******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2004 Project JEDI

 Original author: Olivier Sannier (obones att altern dott org)

 This is a port from the demo written in Delphi by
   Ralf Grenzing [ralfgspam@gmx.de]
   Uwe Rupprecht [uwe-rupprecht@gmx.de]
   Michael Beck [mbeck1@compuserve.com]
   Angus Johnson [ajohnson@rpi.net.au]

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

#ifndef AnimatedTitleFormH
#define AnimatedTitleFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
class TfrmAnimatedTitle : public TForm
{
__published:	// IDE-managed Components
private:	// User declarations
public:		// User declarations
  __fastcall TfrmAnimatedTitle(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmAnimatedTitle *frmAnimatedTitle;
//---------------------------------------------------------------------------
#endif
