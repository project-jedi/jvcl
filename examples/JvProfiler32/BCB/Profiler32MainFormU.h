//---------------------------------------------------------------------------

#ifndef Profiler32MainFormUH
#define Profiler32MainFormUH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvComponentBase.hpp"
#include "JvProfilerForm.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------

#define SET_DECIMAL_SEPARATOR (1)

class TProfiler32MainForm : public TForm
{
__published:    // IDE-managed Components
        TPanel *Panel1;
        TLabel *Label1;
        TButton *UseIdBtn;
        TButton *UseNameBtn;
        TButton *ResultBtn;
        TListBox *ListBox1;
        TProgressBar *Progress;
        TJvProfiler *P;
        void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
        void __fastcall FormKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
        void __fastcall UseIdBtnClick(TObject *Sender);
        void __fastcall UseNameBtnClick(TObject *Sender);
        void __fastcall ResultBtnClick(TObject *Sender);
private:        // User declarations
        bool FTerminated;
        AnsiString DefCaption;
public:         // User declarations
        __fastcall TProfiler32MainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TProfiler32MainForm *Profiler32MainForm;
//---------------------------------------------------------------------------
#endif
