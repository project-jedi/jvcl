//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvComponent.hpp"
#include "JvExControls.hpp"
#include "JvInspector.hpp"
#include <JvInspExtraEditors.hpp>
#include "IniFiles.hpp"
#include <ImgList.hpp>

#pragma message "put me in JvInspector.pas"
#define TJvCustomInspectorItem Jvinspector::TJvCustomInspectorItem

//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TImageList *TestImageList;
  TJvInspectorDotNETPainter *JvInspectorDotNETPainter1;
  TJvInspectorBorlandPainter *JvInspectorBorlandPainter1;
  TJvInspector *JvInspector1;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall JvInspector1AfterItemCreate(TObject *Sender,
          const TJvCustomInspectorItem *Item);
private:	// User declarations
  bool BoolsAsChecks;
  TIniFile* INI;
  void __fastcall AddInspectorSettings();
  void __fastcall AddGlobalSettings();
  void __fastcall AddFormAndControls();
  void __fastcall AddCompoundTest();
  void __fastcall AddCtrl(const TJvCustomInspectorItem* Parent, const TControl* Ctrl);
  void __fastcall AddINIFile();
  void __fastcall AddVarious();
  void __fastcall ChangeChkState(const TJvCustomInspectorItem* Item);
  void __fastcall Edit1Change2(TObject* Sender);
  void __fastcall GetBoolsAsChecks(TJvInspectorEventData* Sender, __int64& Value);
  void __fastcall SetBoolsAsChecks(TJvInspectorEventData* Sender, __int64& Value);
  void __fastcall OnINISection(AnsiString& SectionName, bool& Parse);
  void __fastcall OnINIKey(const AnsiString SectionName, AnsiString& ItemName, PTypeInfo& ATypeInfo, bool& Allow);
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
