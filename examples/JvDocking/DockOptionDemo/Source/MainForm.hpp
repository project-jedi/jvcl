// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'MainForm.pas' rev: 5.00

#ifndef MainFormHPP
#define MainFormHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ImgList.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <Spin.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <JvDockVIDStyle.hpp>	// Pascal unit
#include <JvDockControlForm.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Mainform
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TMain_Form;
class PASCALIMPLEMENTATION TMain_Form : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	JvDockControlForm::TJvDockServer* lbDockServer1;
	JvDockVIDStyle::TJvDockVIDStyle* JvDockVIDStyle1;
	Comctrls::TPageControl* PageControl1;
	Comctrls::TTabSheet* TabSheet1;
	Comctrls::TTabSheet* TabSheet2;
	Stdctrls::TLabel* Label1;
	Stdctrls::TLabel* Label2;
	Stdctrls::TLabel* Label3;
	Stdctrls::TGroupBox* GroupBox2;
	Stdctrls::TButton* ActivePanelFont_Button;
	Stdctrls::TButton* ActivePanelStartColor_Button;
	Stdctrls::TButton* ActivePanelEndColor_Button;
	Stdctrls::TGroupBox* GroupBox3;
	Stdctrls::TButton* InactivePanelFont_Button;
	Stdctrls::TButton* InactivePanelStartColor_Button;
	Stdctrls::TButton* InactivePanelEndColor_Button;
	Spin::TSpinEdit* GrabbersSize_SpinEdit;
	Spin::TSpinEdit* SplitterWidth_SpinEdit;
	Stdctrls::TComboBox* TextAlignment_ComboBox;
	Stdctrls::TCheckBox* SystemInfo_CheckBox;
	Stdctrls::TCheckBox* TextEllipsis_CheckBox;
	Stdctrls::TLabel* Label6;
	Stdctrls::TGroupBox* GroupBox5;
	Stdctrls::TButton* ActiveTabFont_Button;
	Stdctrls::TButton* ActiveTabColor_Button;
	Stdctrls::TGroupBox* GroupBox6;
	Stdctrls::TButton* InactiveTabFont_Button;
	Stdctrls::TButton* InctiveTabColor_Button;
	Stdctrls::TComboBox* TabPosition_ComboBox;
	Stdctrls::TCheckBox* HotTrack_CheckBox;
	Stdctrls::TCheckBox* ShowIcon_CheckBox;
	Stdctrls::TButton* TrackColor_Button;
	Dialogs::TFontDialog* FontDialog1;
	Dialogs::TColorDialog* ColorDialog1;
	Controls::TImageList* ImageList1;
	void __fastcall FormClose(System::TObject* Sender, Forms::TCloseAction &Action);
	void __fastcall ActivePanelFont_ButtonClick(System::TObject* Sender);
	void __fastcall ActivePanelStartColor_ButtonClick(System::TObject* Sender);
	void __fastcall ActivePanelEndColor_ButtonClick(System::TObject* Sender);
	void __fastcall InactivePanelFont_ButtonClick(System::TObject* Sender);
	void __fastcall InactivePanelStartColor_ButtonClick(System::TObject* Sender);
	void __fastcall InactivePanelEndColor_ButtonClick(System::TObject* Sender);
	void __fastcall GrabbersSize_SpinEditChange(System::TObject* Sender);
	void __fastcall SplitterWidth_SpinEditChange(System::TObject* Sender);
	void __fastcall TextAlignment_ComboBoxChange(System::TObject* Sender);
	void __fastcall SystemInfo_CheckBoxClick(System::TObject* Sender);
	void __fastcall TextEllipsis_CheckBoxClick(System::TObject* Sender);
	void __fastcall ActiveTabFont_ButtonClick(System::TObject* Sender);
	void __fastcall InactiveTabFont_ButtonClick(System::TObject* Sender);
	void __fastcall ActiveTabColor_ButtonClick(System::TObject* Sender);
	void __fastcall InctiveTabColor_ButtonClick(System::TObject* Sender);
	void __fastcall HotTrack_CheckBoxClick(System::TObject* Sender);
	void __fastcall ShowIcon_CheckBoxClick(System::TObject* Sender);
	void __fastcall TabPosition_ComboBoxChange(System::TObject* Sender);
	void __fastcall JvDockVIDStyle1SystemInfoChange(bool Value);
	void __fastcall TrackColor_ButtonClick(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	
private:
	void __fastcall DoReadOption(void);
	void __fastcall DoReadConjoinOption(void);
	void __fastcall DoReadTabOption(void);
	
public:
	void __fastcall CreateDockWidnow(void);
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TMain_Form(Classes::TComponent* AOwner) : Forms::TForm(
		AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TMain_Form(Classes::TComponent* AOwner, int Dummy
		) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TMain_Form(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TMain_Form(HWND ParentWindow) : Forms::TForm(ParentWindow
		) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TMain_Form* Main_Form;

}	/* namespace Mainform */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Mainform;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MainForm
