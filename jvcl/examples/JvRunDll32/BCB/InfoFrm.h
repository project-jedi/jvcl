//---------------------------------------------------------------------------

#ifndef InfoFrmH
#define InfoFrmH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
//---------------------------------------------------------------------------


class TfrmInfo : public TForm
{
__published:	// IDE-managed Components
        TRichEdit *reInfo;
        void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
private: // User declarations
        void LoadFile(const AnsiString FileName);
public: // User declarations
        __fastcall TfrmInfo(TComponent* Owner);

friend void TfrmInfo_View(const String FileName,const String Title);

};

void TfrmInfo_View(const String FileName,const String Title);

//---------------------------------------------------------------------------
//extern PACKAGE TfrmInfo *frmInfo;
//---------------------------------------------------------------------------
#endif
