//---------------------------------------------------------------------------

#ifndef ChangeDirDlgH
#define ChangeDirDlgH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <JvChangeNotify.hpp>
//---------------------------------------------------------------------------
class TfrmChangeNotificationDirDlg : public TForm
{
__published:	// IDE-managed Components
  TLabel *Label1;
  TGroupBox *GroupBox1;
  TCheckBox *cbAttributes;
  TCheckBox *cbDirNames;
  TCheckBox *cbFileNames;
  TCheckBox *cbSize;
  TCheckBox *cbWrite;
  TCheckBox *cbSubTrees;
  TEdit *Edit1;
  TButton *Button3;
  TButton *Button1;
  TButton *btnOK;
  void __fastcall Button1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmChangeNotificationDirDlg(TComponent* Owner);
  static bool Execute(AnsiString& Directory, TJvChangeActions& Options, bool& IncludeSubDirs);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmChangeNotificationDirDlg *frmChangeNotificationDirDlg;
//---------------------------------------------------------------------------
#endif
