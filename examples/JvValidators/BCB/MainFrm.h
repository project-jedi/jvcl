//---------------------------------------------------------------------------

#ifndef MainFrmH
#define MainFrmH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvComponent.hpp"
#include "JvErrorIndicator.hpp"
#include "JvValidators.hpp"
#include <ComCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
        TLabel *Label1;
        TLabel *Label2;
        TLabel *Label3;
        TLabel *Label4;
        TLabel *Label5;
        TEdit *edRequired;
        TEdit *edRequired10Chars;
        TEdit *edRegExpr;
        TEdit *edRange0to100;
        TUpDown *udRange0to100;
        TButton *btnCheck;
        TButton *btnProviderCheck;
        TRichEdit *reResults;
        TButton *btnValSum;
        TJvValidators *JvValidators1;
        TJvRequiredFieldValidator *JvRequiredFieldValidator1;
        TJvCustomValidator *JvCustomValidator1;
        TJvRegularExpressionValidator *JvRegularExpressionValidator1;
        TJvRangeValidator *JvRangeValidator1;
        TJvErrorIndicator *JvErrorIndicator1;
        TJvValidationSummary *JvValidationSummary1;
        void __fastcall reResultsEnter(TObject *Sender);
        void __fastcall JvValidators1ValidateFailed(TObject *Sender,
                                 TJvBaseValidator *BaseValidator, bool &Continue);
        void __fastcall JvValidationSummary1Change(TObject *Sender);
        void __fastcall btnProviderCheckClick(TObject *Sender);
        void __fastcall btnValSumClick(TObject *Sender);
        void __fastcall btnCheckClick(TObject *Sender);
        void __fastcall JvCustomValidator1Validate(TObject *Sender,
          Variant &ValueToValidate, bool &Valid);
private:
        void __fastcall ProviderErrorValidateFailed(TObject * Sender, TJvBaseValidator * BaseValidator, bool & Continue);	// User declarations
public:		// User declarations
        __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
