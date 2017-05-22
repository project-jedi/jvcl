//---------------------------------------------------------------------------

#ifndef JvNTEventLogMainFormUH
#define JvNTEventLogMainFormUH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvComponentBase.hpp"
#include "JvNTEventLog.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------

#define CM_CHECKOSVERSION  (WM_USER + 1)

class TJvNTEventLogMainForm : public TForm
{
__published:	// IDE-managed Components
        TPanel *ButtonsPanel;
        TButton *btnRefresh;
        TJvNTEventLog *JvNTEventLog1;
        TListBox *ListBox1;
        TListView *ListView1;
        TSplitter *Splitter1;
        void __fastcall ListBox1Click(TObject *Sender);
        void __fastcall btnRefreshClick(TObject *Sender);
private:
        void ReadEvents(void);        // User declarations
public:         // User declarations
        __fastcall TJvNTEventLogMainForm(TComponent* Owner);
protected:
        void __fastcall CMCHECKOSVERSION(TMessage & Msg);
        BEGIN_MESSAGE_MAP
                VCL_MESSAGE_HANDLER(CM_CHECKOSVERSION, TMessage, CMCHECKOSVERSION)
        END_MESSAGE_MAP(TForm)
};
//---------------------------------------------------------------------------
extern PACKAGE TJvNTEventLogMainForm *JvNTEventLogMainForm;
//---------------------------------------------------------------------------
#endif
