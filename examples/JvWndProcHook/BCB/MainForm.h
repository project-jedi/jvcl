//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvCaptionButton.hpp"
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
        TLabel *Label1;
        TButton *btnAdd;
        TButton *btnDelete;
        TButton *btnRecreateWnd;
        TListBox *lbButtons;
        void __fastcall btnAddClick(TObject *Sender);
        void __fastcall btnDeleteClick(TObject *Sender);
        void __fastcall btnRecreateWndClick(TObject *Sender);
private:	// User declarations
        int FButtonCount;
        void __fastcall DoButtonClick(TObject * Sender);
public:		// User declarations
        __fastcall TfrmMain(TComponent* Owner);
protected:
        AnsiString __fastcall UniqueName(const AnsiString BaseName);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
