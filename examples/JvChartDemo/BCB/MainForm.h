//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <Controls.hpp>
#include <Classes.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvChart.hpp"
#include "JvComponent.hpp"
#include "JvExControls.hpp"
#include <Buttons.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TPanel *Panel2;
  TCheckBox *CheckBox1;
  TPanel *Panel1;
  TSpeedButton *SpeedButton1;
  TSpeedButton *SpeedButton2;
  TSpeedButton *SpeedButton3;
  TSpeedButton *SpeedButton4;
  TSpeedButton *SpeedButton5;
  TSpeedButton *SpeedButton6;
  TSpeedButton *SpeedButton7;
  TSpeedButton *SpeedButton8;
  TSpeedButton *SpeedButton9;
  TSpeedButton *SpeedButton10;
  TSpeedButton *SpeedButton11;
  TSpeedButton *SpeedButton12;
  TSpeedButton *SpeedButton14;
  TButton *ButtonNewValues;
  TFontDialog *FontDialog1;
  TColorDialog *ColorDialog1;
  TJvChart *Chart;
  void __fastcall ButtonNewValuesClick(TObject *Sender);
  void __fastcall SpeedButton1Click(TObject *Sender);
  void __fastcall FormResize(TObject *Sender);
  void __fastcall SpeedButton2Click(TObject *Sender);
  void __fastcall SpeedButton3Click(TObject *Sender);
  void __fastcall SpeedButton4Click(TObject *Sender);
  void __fastcall SpeedButton5Click(TObject *Sender);
  void __fastcall SpeedButton6Click(TObject *Sender);
  void __fastcall SpeedButton7Click(TObject *Sender);
  void __fastcall SpeedButton8Click(TObject *Sender);
  void __fastcall SpeedButton10Click(TObject *Sender);
  void __fastcall SpeedButton12Click(TObject *Sender);
  void __fastcall SpeedButton11Click(TObject *Sender);
  void __fastcall SpeedButton14Click(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
