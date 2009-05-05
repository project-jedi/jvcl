//---------------------------------------------------------------------------

#ifndef OLBarMaonFormUH
#define OLBarMaonFormUH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvExControls.hpp"
#include "JvOutlookBar.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TOLBarMainForm : public TForm
{
__published:	// IDE-managed Components
        TPanel *Panel1;
        TPanel *Panel2;
        TButton *Button1;
        TCheckBox *chkSmallImages;
        TButton *Button2;
        TButton *Button3;
        TCheckBox *chkButtonFont;
        TCheckBox *chkFlat;
        TRichEdit *RichEdit1;
        TJvOutlookBar *JvOutlookBar1;
        TSplitter *Splitter1;
        TImageList *ImageList1;
        TPopupMenu *popButton;
        TMenuItem *Editbuttoncaption1;
        TPopupMenu *popPage;
        TMenuItem *Editpagecaption1;
        TMenuItem *Smallbuttons1;
        TImageList *ImageList2;
        TActionList *ActionList1;
        TAction *acSmallButtons;
        TAction *acEditButtonCaption;
        TAction *acEditPageCaption;
        TPopupMenu *popOL;
        TMenuItem *Defaultpopupmenu1;
        TMenuItem *Smallbuttons2;
        TStatusBar *StatusBar1;
        void __fastcall Button1Click(TObject *Sender);
        void __fastcall Button2Click(TObject *Sender);
        void __fastcall Button3Click(TObject *Sender);
        void __fastcall JvOutlookBar1ButtonClick(TObject *Sender,
          int Index);
        void __fastcall JvOutlookBar1PageChange(TObject *Sender,
          int Index);
        void __fastcall JvOutlookBar1PageChanging(TObject *Sender,
          int Index, bool &AllowChange);
        void __fastcall JvOutlookBar1ContextPopup(TObject *Sender,
          TPoint &MousePos, bool &Handled);
        void __fastcall acSmallButtonsExecute(TObject *Sender);
        void __fastcall chkFlatClick(TObject *Sender);
        void __fastcall acEditButtonCaptionExecute(TObject *Sender);
        void __fastcall acEditPageCaptionExecute(TObject *Sender);
private:	// User declarations
public:		// User declarations
        __fastcall TOLBarMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TOLBarMainForm *OLBarMainForm;
//---------------------------------------------------------------------------
#endif
