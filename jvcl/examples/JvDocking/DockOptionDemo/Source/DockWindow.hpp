// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DockWindow.pas' rev: 5.00

#ifndef DockWindowHPP
#define DockWindowHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <StdCtrls.hpp>	// Pascal unit
#include <ImgList.hpp>	// Pascal unit
#include <JvDockControlForm.hpp>	// Pascal unit
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

namespace Dockwindow
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TDockWindow_Form;
class PASCALIMPLEMENTATION TDockWindow_Form : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	JvDockControlForm::TJvDockClient* lbDockClient1;
	Stdctrls::TMemo* Memo1;
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TDockWindow_Form(Classes::TComponent* AOwner) : 
		Forms::TForm(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TDockWindow_Form(Classes::TComponent* AOwner, 
		int Dummy) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TDockWindow_Form(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TDockWindow_Form(HWND ParentWindow) : Forms::TForm(
		ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TDockWindow_Form* DockWindow_Form;

}	/* namespace Dockwindow */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dockwindow;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DockWindow
