//---------------------------------------------------------------------------

#ifndef JvSearchFileMainFormUH
#define JvSearchFileMainFormUH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvAppIniStorage.hpp"
#include "JvAppStorage.hpp"
#include "JvComponent.hpp"
#include "JvExMask.hpp"
#include "JvFormPlacement.hpp"
#include "JvSearchFiles.hpp"
#include "JvToolEdit.hpp"
#include <ComCtrls.hpp>
#include <Mask.hpp>
//---------------------------------------------------------------------------
static bool ContainsText(const AnsiString Filename,AnsiString AText);

class TJvSearchFileMainForm : public TForm
{
__published:	// IDE-managed Components
        TGroupBox *GroupBox2;
        TRichEdit *reFoundFiles;
        TGroupBox *GroupBox1;
        TLabel *Label1;
        TLabel *Label2;
        TJvDirectoryEdit *JvDirectoryBox1;
        TCheckBox *chkRecursive;
        TEdit *edFileMask;
        TComboBox *cbContainText;
        TRadioButton *rbInclude;
        TRadioButton *rbExclude;
        TCheckBox *chkClearList;
        TCheckBox *chkNoDupes;
        TButton *btnSearch;
        TButton *btnCancel;
        TStatusBar *StatusBar1;
        TJvFormStorage *JvFormStorage1;
        TJvAppIniFileStorage *JvAppIniFileStorage1;
        TJvSearchFiles *JvSearchFile1;
        void __fastcall OptionsChange(TObject * Sender);
        void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
        void __fastcall btnCancelClick(TObject *Sender);
        void __fastcall btnSearchClick(TObject *Sender);
        void __fastcall JvSearchFile1BeginScanDir(TObject *Sender,
          const AnsiString AName);
        void __fastcall JvSearchFile1FindFile(TObject *Sender,
          const AnsiString AName);
        void __fastcall JvSearchFile1Progress(TObject *Sender);
private:
        void AddSearchTextToComboBox(void);	// User declarations
public:		// User declarations
        __fastcall TJvSearchFileMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TJvSearchFileMainForm *JvSearchFileMainForm;
//---------------------------------------------------------------------------
#endif
