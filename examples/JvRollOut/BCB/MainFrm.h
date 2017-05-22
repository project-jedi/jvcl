//---------------------------------------------------------------------------

#ifndef MainFrmH
#define MainFrmH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvComponent.hpp"
#include "JvExExtCtrls.hpp"
#include "JvRollOut.hpp"
#include <ActnList.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
        TActionList *ActionList1;
        TJvRollOutAction *JvRollOutAction1;
        TJvRollOutAction *JvRollOutAction2;
        TImageList *ImageList1;
        TMainMenu *MainMenu1;
        TMenuItem *Actions1;
        TMenuItem *Action1Ctrl11;
        TMenuItem *Action2Ctrl21;
        TSplitter *Splitter1;
        TPanel *Panel4;
        TCheckBox *chkShowFocus;
        TCheckBox *chkTabStop;
        TCheckBox *chkToggleAnywhere;
        TCheckBox *chkGroupIndex;
        TCheckBox *chkHideButton;
        TCheckBox *chkHideFrame;
        TCheckBox *chkImages;
        TPanel *Panel3;
        TSplitter *Splitter3;
        TPanel *pnlRightAlign;
        TLabel *Label3;
        TJvRollOut *RO40;
        TJvRollOut *RO39;
        TJvRollOut *RO38;
        TJvRollOut *RO37;
        TJvRollOut *RO36;
        TJvRollOut *RO35;
        TJvRollOut *RO34;
        TJvRollOut *RO33;
        TJvRollOut *RO32;
        TJvRollOut *RO31;
        TPanel *pnlLeftAlign;
        TLabel *Label4;
        TJvRollOut *RO30;
        TJvRollOut *RO29;
        TJvRollOut *RO28;
        TJvRollOut *RO27;
        TJvRollOut *RO26;
        TJvRollOut *RO25;
        TJvRollOut *RO24;
        TJvRollOut *RO23;
        TJvRollOut *RO22;
        TJvRollOut *RO21;
        TPanel *Panel1;
        TSplitter *Splitter2;
        TPanel *pnlTopAlign;
        TLabel *Label1;
        TJvRollOut *RO1;
        TEdit *Edit1;
        TEdit *Edit2;
        TEdit *Edit3;
        TEdit *Edit4;
        TJvRollOut *RO2;
        TEdit *Edit5;
        TEdit *Edit6;
        TEdit *Edit7;
        TEdit *Edit8;
        TJvRollOut *RO3;
        TEdit *Edit9;
        TEdit *Edit10;
        TEdit *Edit11;
        TEdit *Edit12;
        TJvRollOut *RO4;
        TJvRollOut *RO5;
        TJvRollOut *RO6;
        TJvRollOut *RO7;
        TJvRollOut *RO8;
        TJvRollOut *RO9;
        TJvRollOut *RO10;
        TPanel *pnlBottomAlign;
        TLabel *Label2;
        TJvRollOut *RO20;
        TJvRollOut *RO19;
        TJvRollOut *RO18;
        TJvRollOut *RO17;
        TJvRollOut *RO16;
        TJvRollOut *RO15;
        TJvRollOut *RO14;
        TJvRollOut *RO13;
        TJvRollOut *RO12;
        TJvRollOut *RO11;
        void __fastcall JvRollOutAction1Execute(TObject *Sender);
        void __fastcall JvRollOutAction2Execute(TObject *Sender);
        void __fastcall chkShowFocusClick(TObject *Sender);
        void __fastcall chkTabStopClick(TObject *Sender);
        void __fastcall chkGroupIndexClick(TObject *Sender);
        void __fastcall chkToggleAnywhereClick(TObject *Sender);
        void __fastcall chkHideButtonClick(TObject *Sender);
        void __fastcall chkHideFrameClick(TObject *Sender);
        void __fastcall chkImagesClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
        __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
