//---------------------------------------------------------------------------

#ifndef JvPageListTreeViewMainFormH
#define JvPageListTreeViewMainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvButton.hpp"
#include "JvColorCombo.hpp"
#include "JvCombobox.hpp"
#include "JvCtrls.hpp"
#include "JvExComCtrls.hpp"
#include "JvExControls.hpp"
#include "JvExExtCtrls.hpp"
#include "JvExStdCtrls.hpp"
#include "JvExtComponent.hpp"
#include "JvFooter.hpp"
#include "JvGroupHeader.hpp"
#include "JvPageList.hpp"
#include "JvPageListTreeView.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TJvPageListTreeViewMainFrm : public TForm
{
__published:	// IDE-managed Components
        TActionList *ActionList1;
        TImageList *ImageList1;
        TImageList *ImageList2;
        TSplitter *Splitter1;
        TPanel *Panel1;
        TJvSettingsTreeView *JvPagedTreeView1;
        TJvStandardPage *JvStandardPage2;
        TJvStandardPage *JvStandardPage4;
        TJvStandardPage *JvStandardPage5;
        TJvStandardPage *JvStandardPage1;
        TJvFooter *JvFooter1;
        TJvFooterBtn *JvFooterBtn2;
        TJvFooterBtn *JvFooterBtn3;
        TJvFooterBtn *JvFooterBtn1;
        TJvPageList *JvPageList1;
        TJvStandardPage *pgEnvironmentGeneral;
        TLabel *Label1;
        TLabel *Label2;
        TLabel *Label3;
        TLabel *Label4;
        TLabel *Label5;
        TLabel *Label6;
        TJvGroupHeader *JvGroupHeader3;
        TRadioButton *RadioButton1;
        TRadioButton *RadioButton2;
        TButton *Button1;
        TComboBox *ComboBox1;
        TCheckBox *CheckBox1;
        TCheckBox *CheckBox2;
        TTrackBar *TrackBar1;
        TCheckBox *CheckBox3;
        TEdit *Edit1;
        TEdit *Edit2;
        TCheckBox *CheckBox4;
        TCheckBox *CheckBox5;
        TJvStandardPage *pgDocuments;
        TLabel *Label7;
        TLabel *Label8;
        TJvGroupHeader *JvGroupHeader4;
        TCheckBox *CheckBox6;
        TCheckBox *CheckBox7;
        TCheckBox *CheckBox8;
        TCheckBox *CheckBox9;
        TCheckBox *CheckBox10;
        TCheckBox *CheckBox11;
        TEdit *Edit3;
        TCheckBox *CheckBox12;
        TCheckBox *CheckBox13;
        TJvStandardPage *pgDynamicHelp;
        TLabel *Label9;
        TLabel *Label10;
        TLabel *Label11;
        TListView *ListView1;
        TListView *ListView2;
        TButton *Button2;
        TButton *Button3;
        TRadioButton *RadioButton3;
        TRadioButton *RadioButton4;
        TRadioButton *RadioButton5;
        TCheckBox *CheckBox14;
        TEdit *Edit4;
        TJvStandardPage *pgFontsColors;
        TLabel *Label12;
        TLabel *Label13;
        TLabel *Label14;
        TLabel *Label15;
        TLabel *Label16;
        TLabel *Label17;
        TLabel *Label18;
        TShape *Shape1;
        TLabel *Label19;
        TComboBox *ComboBox2;
        TButton *Button4;
        TComboBox *ComboBox3;
        TComboBox *ComboBox4;
        TListBox *ListBox1;
        TJvColorComboBox *JvColorComboBox1;
        TJvColorComboBox *JvColorComboBox2;
        TCheckBox *CheckBox15;
        TButton *Button5;
        TButton *Button6;
        TJvStandardPage *pgHelp;
        TJvStandardPage *pgInternational;
        TJvStandardPage *pgKeyboard;
        TJvStandardPage *pgProjSolutions;
        TJvStandardPage *pgTaskList;
        TJvStandardPage *pgWebBrowser;
        TJvGroupHeader *JvGroupHeader1;
        TJvGroupHeader *JvGroupHeader2;
        TStatusBar *StatusBar1;
        void __fastcall JvFooterBtn2Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
        __fastcall TJvPageListTreeViewMainFrm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TJvPageListTreeViewMainFrm *JvPageListTreeViewMainFrm;
//---------------------------------------------------------------------------
#endif
