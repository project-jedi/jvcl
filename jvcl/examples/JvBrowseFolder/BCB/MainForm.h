//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvBaseDlg.hpp"
#include "JvBrowseFolder.hpp"
#include "JvComponent.hpp"
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TJvBrowseForFolderDialog *JvBrowseFolder1;
  TGroupBox *GroupBox2;
  TLabel *Label4;
  TLabel *Label5;
  TEdit *Edit4;
  TCheckBox *CheckBox1;
  TCheckBox *CheckBox2;
  TCheckBox *CheckBox3;
  TCheckBox *CheckBox4;
  TCheckBox *CheckBox5;
  TCheckBox *CheckBox6;
  TCheckBox *CheckBox7;
  TCheckBox *CheckBox8;
  TComboBox *ComboBox1;
  TGroupBox *GroupBox1;
  TLabel *Label1;
  TLabel *Label2;
  TLabel *Label3;
  TEdit *Edit1;
  TEdit *Edit2;
  TEdit *Edit3;
  TButton *Button1;
  void __fastcall Button1Click(TObject *Sender);
  void __fastcall JvBrowseFolder1Change(TObject *Sender,
          const AnsiString Directory);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
