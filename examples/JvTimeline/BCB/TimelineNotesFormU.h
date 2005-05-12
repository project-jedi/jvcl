//---------------------------------------------------------------------------

#ifndef TimelineNotesFormUH
#define TimelineNotesFormUH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
class TTimelineNotesForm : public TForm
{
__published:	// IDE-managed Components
        TMemo *Memo1;
private:	// User declarations
public:		// User declarations
        __fastcall TTimelineNotesForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TTimelineNotesForm *TimelineNotesForm;
//---------------------------------------------------------------------------
#endif
