//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "JvProgressDialogMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmProgressDialogDemo *frmProgressDialogDemo;
//---------------------------------------------------------------------------
__fastcall TfrmProgressDialogDemo::TfrmProgressDialogDemo(TComponent* Owner)
        : TForm(Owner)
{
  /*
    Demo for JvProgressDialog: shows off most important properties and events.
    Component is created at run-time so does not need to be installed but the units
    JvProgressDialog.pas and JvProgressFrm.pas must be on the path
  */
  pd = NULL;
}
//---------------------------------------------------------------------------

void TfrmProgressDialogDemo::DoModalShow(void)
{
  char cCancel[2][5]={{""}, {"not "}};

  // OnProgress and Interval is used in modal mode
  pd->OnProgress = DoDialogProgress;
  if( chkShowEvents->Checked )
  {
    // Execute returns true if the dialog was closed without user intervention
    // and false if the user clicked the Cancel button (or hit Esc when Cancel button is visible)
    ShowMessageFmt("User did %scancel according to Execute", OPENARRAY(TVarRec,(cCancel[pd->Execute()])) );
  }
  else
  {
    pd->Execute();
  }
}

void TfrmProgressDialogDemo::DoNonModalShow(void)
{
  // Show, Hide and Cancelled is used in non-modal mood
  pd->OnProgress = NULL; // not needed
  pd->Show();
  while( pd->Position <= pd->Max ) // just loop...
  {
    if( pd->Cancelled )
    {
      break;
    }
    pd->Position = pd->Position + random(10);
    pd->Text = Format(edText->Text, OPENARRAY(TVarRec,(pd->Position )) );
    ::Sleep(pd->Interval);
    Application->ProcessMessages();
  }
  pd->Hide();
}

void __fastcall TfrmProgressDialogDemo::DoDialogProgress(TObject* Sender, bool & AContinue)
{
  // notice that you change the properties of the dialog component
  // and these changes are reflected in the dialog
  pd->Position = pd->Position + random(10);
  pd->Text = Format(edText->Text, OPENARRAY(TVarRec,(pd->Position )));
  // AContinue controls if the dialog should remain visible or not
  AContinue = ( pd->Position <= pd->Max );
}

void __fastcall TfrmProgressDialogDemo::DoDialogCancel(TObject *Sender)
{
  // notice that the ShowMessage always appears *before* the progress dialog is hidden
  if( chkShowEvents->Checked )
  {
    ShowMessage("OnCancel: User cancelled");
  }
  // save the position where the user cancelled:
  FCancelPosition = pd->Position;
}

void __fastcall TfrmProgressDialogDemo::DoDialogClose(TObject * Sender)
{
  if( chkShowEvents->Checked )
  {
    ShowMessage("OnClose: dialog closed");
  }
}

void __fastcall TfrmProgressDialogDemo::DoDialogShow(TObject * Sender)
{
  if( chkShowEvents->Checked )
  {
    ShowMessage("OnShow: showing dialog");
  }
  // set initial value
  pd->Text = Format(edText->Text,OPENARRAY(TVarRec,(pd->Position)) );
}



void __fastcall TfrmProgressDialogDemo::btnExecuteClick(TObject *Sender)
{
  FCancelPosition = -1;
  randomize();
  // set everything up according to user choice
  if( pd == NULL )
  {
    pd = new TJvProgressDialog(this);
  }
  pd->Caption = edCaption->Text;
  pd->Text = edText->Text;
  pd->ShowCancel = chkShowCancel->Checked;
  pd->Interval = 100 + random(1000);
  if( chkShowLogo->Checked )
  {
    pd->Image = Image1->Picture;
    pd->Transparent = chkTransparent->Checked;
  }
  else
  {
    pd->Image = NULL;
  }
  // set up events
  pd->OnCancel = DoDialogCancel;
  pd->OnShow   = DoDialogShow;
  pd->OnClose  = DoDialogClose;
  if( chkShowModal->Checked )
  {
    DoModalShow();
  }
  else
  {
    DoNonModalShow();
  }
  // Here's an example on how to determine *when* the user cancelled the dialog
  // The FCancelPosition value is updated in DoDialogCancel below:
  if( FCancelPosition >= 0 )
  {
    ShowMessageFmt("The user cancelled at position %d",  OPENARRAY(TVarRec,(FCancelPosition)) );
  }

}
//---------------------------------------------------------------------------

void __fastcall TfrmProgressDialogDemo::btnSelectImageClick(
      TObject *Sender)
{
  if( OpenPictureDialog1->Execute() )
  {
    Image1->Picture->LoadFromFile(OpenPictureDialog1->FileName);
  }
}
//---------------------------------------------------------------------------

