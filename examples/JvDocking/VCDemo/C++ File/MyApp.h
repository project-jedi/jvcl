// MyApp.h : main header file for the MYAPP application
//

#if !defined(AFX_MYAPP_H__9B6A2A3A_4D6E_42BD_8CCF_156992083A79__INCLUDED_)
#define AFX_MYAPP_H__9B6A2A3A_4D6E_42BD_8CCF_156992083A79__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"       // main symbols

/////////////////////////////////////////////////////////////////////////////
// CMyAppApp:
// See MyApp.cpp for the implementation of this class
//

class CMyAppApp : public CWinApp
{
public:
	CMyAppApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CMyAppApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation
	//{{AFX_MSG(CMyAppApp)
	afx_msg void OnAppAbout();
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_MYAPP_H__9B6A2A3A_4D6E_42BD_8CCF_156992083A79__INCLUDED_)
