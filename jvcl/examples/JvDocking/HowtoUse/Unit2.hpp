// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Unit2.pas' rev: 5.00

#ifndef Unit2HPP
#define Unit2HPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
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

namespace Unit2
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TForm2;
class PASCALIMPLEMENTATION TForm2 : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	JvDockControlForm::TJvDockClient* lbDockClient1;
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TForm2(Classes::TComponent* AOwner) : Forms::TForm(
		AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TForm2(Classes::TComponent* AOwner, int Dummy
		) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TForm2(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TForm2(HWND ParentWindow) : Forms::TForm(ParentWindow
		) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TForm2* Form2;

}	/* namespace Unit2 */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Unit2;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Unit2
