//---------------------------------------------------------------------------

#ifndef JvPanelDemoFrmH
#define JvPanelDemoFrmH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvComponentBase.hpp"
#include "JvExExtCtrls.hpp"
#include "JvExMask.hpp"
#include "JvExtComponent.hpp"
#include "JvFormPlacement.hpp"
#include "JvPanel.hpp"
#include "JvToolEdit.hpp"
#include <ExtCtrls.hpp>
#include <Mask.hpp>
//---------------------------------------------------------------------------
class TJvPanelDemoMainFrm : public TForm
{
__published:	// IDE-managed Components
        TJvPanel *JvPanel1;
        TLabel *Label1;
        TEdit *Edit1;
        TJvFilenameEdit *JvFilenameEdit1;
        TCheckBox *CheckBox2;
        TCheckBox *CheckBox1;
        TJvFormStorage *JvFormStorage1;
        void __fastcall CheckBox2Click(TObject *Sender);
        void __fastcall CheckBox1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
        __fastcall TJvPanelDemoMainFrm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TJvPanelDemoMainFrm *JvPanelDemoMainFrm;
//---------------------------------------------------------------------------
#endif
