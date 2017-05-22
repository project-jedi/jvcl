// MyAppView.cpp : implementation of the CMyAppView class
//

#include "stdafx.h"
#include "MyApp.h"

#include "MyAppDoc.h"
#include "MyAppView.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CMyAppView

IMPLEMENT_DYNCREATE(CMyAppView, CView)

BEGIN_MESSAGE_MAP(CMyAppView, CView)
	//{{AFX_MSG_MAP(CMyAppView)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG_MAP
	// Standard printing commands
	ON_COMMAND(ID_FILE_PRINT, CView::OnFilePrint)
	ON_COMMAND(ID_FILE_PRINT_DIRECT, CView::OnFilePrint)
	ON_COMMAND(ID_FILE_PRINT_PREVIEW, CView::OnFilePrintPreview)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CMyAppView construction/destruction

CMyAppView::CMyAppView()
{
	// TODO: add construction code here

}

CMyAppView::~CMyAppView()
{
}

BOOL CMyAppView::PreCreateWindow(CREATESTRUCT& cs)
{
	// TODO: Modify the Window class or styles here by modifying
	//  the CREATESTRUCT cs

	return CView::PreCreateWindow(cs);
}

/////////////////////////////////////////////////////////////////////////////
// CMyAppView drawing

void CMyAppView::OnDraw(CDC* pDC)
{
	CMyAppDoc* pDoc = GetDocument();
	ASSERT_VALID(pDoc);
	// TODO: add draw code for native data here
}

/////////////////////////////////////////////////////////////////////////////
// CMyAppView printing

BOOL CMyAppView::OnPreparePrinting(CPrintInfo* pInfo)
{
	// default preparation
	return DoPreparePrinting(pInfo);
}

void CMyAppView::OnBeginPrinting(CDC* /*pDC*/, CPrintInfo* /*pInfo*/)
{
	// TODO: add extra initialization before printing
}

void CMyAppView::OnEndPrinting(CDC* /*pDC*/, CPrintInfo* /*pInfo*/)
{
	// TODO: add cleanup after printing
}

/////////////////////////////////////////////////////////////////////////////
// CMyAppView diagnostics

#ifdef _DEBUG
void CMyAppView::AssertValid() const
{
	CView::AssertValid();
}

void CMyAppView::Dump(CDumpContext& dc) const
{
	CView::Dump(dc);
}

CMyAppDoc* CMyAppView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CMyAppDoc)));
	return (CMyAppDoc*)m_pDocument;
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CMyAppView message handlers
