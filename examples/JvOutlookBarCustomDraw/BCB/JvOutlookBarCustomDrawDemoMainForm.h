//---------------------------------------------------------------------------

#ifndef JvOutlookBarCustomDrawDemoMainFormH
#define JvOutlookBarCustomDrawDemoMainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <SysUtils.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvJVCLUtils.hpp"
#include "JvComponentBase.hpp"
#include "JvExControls.hpp"
#include "JvNavigationPane.hpp"
#include "JvOutlookBar.hpp"
#include <ImgList.hpp>
#include <Controls.hpp>
//---------------------------------------------------------------------------
class TJvOutlookBarCustomDrawDemoMainFrm : public TForm
{
__published:	// IDE-managed Components
        TJvOutlookBar *JvOutlookBar1;
        TComboBox *ComboBox1;
        TComboBox *ComboBox2;
        TLabel *Label2;
        TLabel *Label1;
        TImageList *ImageList1;
        TImageList *ImageList2;
        TJvNavPaneStyleManager *JvNavPaneStyleManager1;
        void __fastcall ComboBox1Change(TObject *Sender);
        void __fastcall ComboBox2Change(TObject *Sender);
        void __fastcall DoCustomDraw(TObject *Sender, TCanvas *ACanvas, TRect &ARect,
                                      TJvOutlookBarCustomDrawStage AStage,
                                      int AIndex, bool ADown, bool AInside, bool &DefaultDraw);
private:	// User declarations
public:		// User declarations
        __fastcall TJvOutlookBarCustomDrawDemoMainFrm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TJvOutlookBarCustomDrawDemoMainFrm *JvOutlookBarCustomDrawDemoMainFrm;
//---------------------------------------------------------------------------
#endif
