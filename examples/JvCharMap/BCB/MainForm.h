//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvColorCombo.hpp"
#include "JvCombobox.hpp"
#include "JvExStdCtrls.hpp"
#include "JvCharMap.hpp"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TPopupMenu *PopupMenu1;
  TMenuItem *Copy1;
  TPanel *Panel1;
  TLabel *Label1;
  TLabel *Label2;
  TLabel *Label3;
  TLabel *lblFilter;
  TLabel *Label4;
  TButton *btnFont;
  TCheckBox *chkZoomPanel;
  TEdit *edStart;
  TUpDown *udStart;
  TEdit *edEnd;
  TUpDown *udEnd;
  TEdit *edCols;
  TUpDown *udColumns;
  TJvColorComboBox *cbColor;
  TJvFontComboBox *cbFont;
  TCheckBox *chkUnicode;
  TRichEdit *reInfo;
  TButton *btnSelect;
  TComboBox *cbFilter;
  TComboBox *cbLocales;
  TCheckBox *chkShadow;
  TCheckBox *chkDisplayAll;
  TLabel *lblChars;
  TFontDialog *FontDialog1;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall btnFontClick(TObject *Sender);
  void __fastcall chkZoomPanelClick(TObject *Sender);
  void __fastcall chkUnicodeClick(TObject *Sender);
  void __fastcall Copy1Click(TObject *Sender);
  void __fastcall btnSelectClick(TObject *Sender);
  void __fastcall cbFilterClick(TObject *Sender);
  void __fastcall cbLocalesClick(TObject *Sender);
  void __fastcall chkShadowClick(TObject *Sender);
  void __fastcall chkDisplayAllClick(TObject *Sender);
  void __fastcall edColsChange(TObject *Sender);
  void __fastcall edEndChange(TObject *Sender);
  void __fastcall edStartChange(TObject *Sender);
private:	// User declarations
  TEdit *edCharacter;

  void __fastcall cbColorChange(TObject *Sender);
  void __fastcall cbFontChange(TObject *Sender);

  void __fastcall FillFilter();
  void __fastcall FillLocales();
  void __fastcall DoJMSelectChar(TObject *Sender, WideChar AChar);
  void __fastcall DoJMResize(TObject *Sender);
  void __fastcall DoJMValidateChar(TObject *Sender, WideChar AChar, bool &Valid);
  void __fastcall DisplayInfo(WideChar AChar);
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
  TJvCharMap *JM;
  bool Changing;
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
