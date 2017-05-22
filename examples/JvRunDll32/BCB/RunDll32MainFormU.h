//---------------------------------------------------------------------------

#ifndef RunDll32MainFormUH
#define RunDll32MainFormUH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvJCLUtils.hpp"
//---------------------------------------------------------------------------
class TRunDll32MainForm : public TForm
{
__published:	// IDE-managed Components
        TLabel *Label1;
        TLabel *lblModule;
        TLabel *lblFunc;
        TLabel *lblCmdLine;
        TEdit *edModule;
        TEdit *edFunc;
        TEdit *edCmdLine;
        TCheckBox *chkWait;
        TButton *btnBrowse;
        TButton *btnRun;
        TButton *btnInfo;
        TButton *btnInternal;
        void __fastcall Label1Click(TObject *Sender);
        void __fastcall btnRunClick(TObject *Sender);
        void __fastcall edModuleChange(TObject *Sender);
        void __fastcall btnInternalClick(TObject *Sender);
        void __fastcall btnBrowseClick(TObject *Sender);
        void __fastcall btnInfoClick(TObject *Sender);
private: // User declarations
        AnsiString GetWinSysDir(void);
        AnsiString GetExpandedSysFilePath(const AnsiString S);
        bool PipeToFile(AnsiString FileName, AnsiString Cmd, bool Append);
public: // User declarations
        __fastcall TRunDll32MainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TRunDll32MainForm *RunDll32MainForm;
//---------------------------------------------------------------------------
#endif
