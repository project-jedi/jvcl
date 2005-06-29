//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MainFrm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvComponent"
#pragma link "JvErrorIndicator"
#pragma link "JvValidators"
#pragma resource "*.dfm"
TForm1 *Form1;


//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
        : TForm(Owner)
{
  reResults->WordWrap = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::reResultsEnter(TObject *Sender)
{
  SelectNext(reResults,true,true);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::JvValidators1ValidateFailed(TObject *Sender,
      TJvBaseValidator *BaseValidator, bool &Continue)
{
  // using the OnValidateFailed event
  reResults->Lines->Add(Format("FAILED: %s",OPENARRAY(TVarRec,(BaseValidator->ErrorMessage)) ) );

}
//---------------------------------------------------------------------------
void __fastcall TForm1::JvValidationSummary1Change(TObject *Sender)
{
 int i;
 TJvValidationSummary *pJVS;
  pJVS = dynamic_cast<TJvValidationSummary *>(Sender);
  if(pJVS!=NULL)
  {
   reResults->Lines->Text = pJVS->Summaries->Text;
   for(i = 0;i<reResults->Lines->Count;++i)
   {
     reResults->Lines->Strings[i] = "SUMMARY: " + reResults->Lines->Strings[i];
   }
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ProviderErrorValidateFailed(TObject * Sender,
                             TJvBaseValidator * BaseValidator, bool & Continue)
{
  JvErrorIndicator1->Error[BaseValidator->ControlToValidate] = BaseValidator->ErrorMessage;
  reResults->Lines->Add(Format("PROVIDER: %s",OPENARRAY(TVarRec,(BaseValidator->ErrorMessage)) ));
}
//---------------------------------------------------------------------------

void __fastcall TForm1::JvCustomValidator1Validate(TObject *Sender,
      Variant &ValueToValidate, bool &Valid)
{
  // custom validation
  AnsiString stText = ValueToValidate;

  Valid = (stText==""?false:static_cast<bool >(stText.Length() >= 10));

}
//---------------------------------------------------------------------------

void __fastcall TForm1::btnValSumClick(TObject *Sender)
{
  reResults->Lines->Clear();
  reResults->WordWrap = false;
  JvErrorIndicator1->ClearErrors();
  JvValidators1->OnValidateFailed = NULL;
  JvValidators1->ErrorIndicator = NULL;
  // Setting the ValidationSummary for TJvValidators will delay
  // triggering the OnChange event until after Validate has completed
  JvValidationSummary1->Summaries->Clear();

#if __BORLANDC__ < 0x0560
  JvValidators1->ValidationSummary = JvValidationSummary1;
#else
  JvValidators1->ValidationSummary = JvValidationSummary1->operator IJvValidationSummary *();
#endif

  JvValidators1->Validate();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::btnCheckClick(TObject *Sender)
{
  reResults->Lines->Clear();
  reResults->WordWrap = false;
  JvErrorIndicator1->ClearErrors();
  JvValidators1->ValidationSummary = NULL;
  JvValidators1->ErrorIndicator = NULL;
  JvValidators1->OnValidateFailed = JvValidators1ValidateFailed;
  JvValidators1->Validate();

}
//---------------------------------------------------------------------------

void __fastcall TForm1::btnProviderCheckClick(TObject *Sender)
{
  reResults->Lines->Clear();
  reResults->WordWrap = false;
  // calling BeginUpdate/EndUpdate delays the error reporting until all controls have been validated
  JvErrorIndicator1->BeginUpdate();
  try
  {
    JvErrorIndicator1->ClearErrors();
    JvValidators1->ValidationSummary = NULL;
    // custom error messages for this type of check
    JvValidators1->OnValidateFailed = ProviderErrorValidateFailed;
    JvValidators1->Validate();
  }
  __finally
  {
    JvErrorIndicator1->EndUpdate();
  }

}
//---------------------------------------------------------------------------


