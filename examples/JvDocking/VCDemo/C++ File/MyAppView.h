// MyAppView.h : interface of the CMyAppView class
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_MYAPPVIEW_H__E5F347B0_55C2_460C_B559_9BFA3C6F9E47__INCLUDED_)
#define AFX_MYAPPVIEW_H__E5F347B0_55C2_460C_B559_9BFA3C6F9E47__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000


class CMyAppView : public CView
{
protected: // create from serialization only
	CMyAppView();
	DECLARE_DYNCREATE(CMyAppView)

// Attributes
public:
	CMyAppDoc* GetDocument();

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CMyAppView)
	public:
	virtual void OnDraw(CDC* pDC);  // overridden to draw this view
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	protected:
	virtual BOOL OnPreparePrinting(CPrintInfo* pInfo);
	virtual void OnBeginPrinting(CDC* pDC, CPrintInfo* pInfo);
	virtual void OnEndPrinting(CDC* pDC, CPrintInfo* pInfo);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CMyAppView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:

// Generated message map functions
protected:
	//{{AFX_MSG(CMyAppView)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  // debug version in MyAppView.cpp
inline CMyAppDoc* CMyAppView::GetDocument()
   { return (CMyAppDoc*)m_pDocument; }
#endif

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_MYAPPVIEW_H__E5F347B0_55C2_460C_B559_9BFA3C6F9E47__INCLUDED_)
