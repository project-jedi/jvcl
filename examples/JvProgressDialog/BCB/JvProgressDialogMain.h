//---------------------------------------------------------------------------

#ifndef JvProgressDialogMainH
#define JvProgressDialogMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <ExtDlgs.hpp>
#include <Graphics.hpp>
#include "JvProgressDialog.hpp"
//---------------------------------------------------------------------------
class TfrmProgressDialogDemo : public TForm
{
__published:	// IDE-managed Components
        TImage *Image1;
        TLabel *Label1;
        TLabel *Label2;
        TButton *btnExecute;
        TCheckBox *chkShowLogo;
        TCheckBox *chkShowCancel;
        TEdit *edCaption;
        TEdit *edText;
        TCheckBox *chkShowEvents;
        TButton *btnSelectImage;
        TCheckBox *chkTransparent;
        TCheckBox *chkShowModal;
        TOpenPictureDialog *OpenPictureDialog1;
        void __fastcall btnExecuteClick(TObject *Sender);
        void __fastcall btnSelectImageClick(TObject *Sender);
private:        // User declarations
        int  FCancelPosition;
        void DoModalShow(void);
        void __fastcall DoDialogProgress(TObject* Sender, bool &AContinue);
        void __fastcall DoDialogCancel(TObject *Sender);
        void __fastcall DoDialogClose(TObject * Sender);
        void __fastcall DoDialogShow(TObject * Sender);
        void DoNonModalShow(void);
protected:

public:         // User declarations
        __fastcall TfrmProgressDialogDemo(TComponent* Owner);
    TJvProgressDialog *pd;
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmProgressDialogDemo *frmProgressDialogDemo;
//---------------------------------------------------------------------------
#endif
