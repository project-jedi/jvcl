//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MainForm.h"
#include <typeinfo>
#include "TestForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvComponent"
#pragma link "JvExControls"
#pragma link "JvInspector"
#pragma link "JvInspExtraEditors"
#pragma resource "*.dfm"
TfrmMain *frmMain;

typedef enum
{
  toOption1,
  toOption2,
  toOption3,
  toOption4,
  toOption5,
  toOption6,
  toOption7,
  toOption8
} TTestOption;

typedef Set<TTestOption, toOption1, toOption8> TTestOptions;

typedef Shortint TTestRange;

typedef enum
{
  teNone,
  teAtLeastOne,
  teAtMostTwo,
  teUnlimited
} TTestEnum ;

// We need the type info for an AnsiString.
// In Delphi, we would have done TypeInfo(string). But with
// C++ Builder, the TypeInfo function doesn't exist.
// So, as recommended by the help, we get the value we need by
// calling GetPropInfo on an AnsiString publishing property
// of an existing object. As such, Name from frmMain will do.
// But we need a TTypeInfo pointer, so we access the PropType
// member of the PPropInfo returned by GetPropInfo.
// Finally, because we will need this stuff quite a lot, we
// just define it with #define. It may be worth having this
// directly in JvInspector.hpp, but for now, this will do.
// Please see RegisterPropertyEditor in C++ Builder help for
// the example that inspired this bizarre construct
#define TypeInfo_AnsiString  *(GetPropInfo(frmMain, "Name")->PropType)

// some other well know types are defined here too
#define TypeInfo_bool *(GetPropInfo(frmMain, "ReadOnly")->PropType)
#define TypeInfo_int *(GetPropInfo(frmMain, "Height")->PropType)
#define TypeInfo_TImageIndex TypeInfo_int

// This is a temporary class and object used to have the type info
// for various types
class tmpTypeClass: public TObject
{
private:
  TDateTime FDT;
  double Fd;
  signed char Fsc;
  TTestOptions FTO;
  TTestEnum FTE;

__published:
	__property TDateTime DT = {read=FDT};
	__property double d = {read=Fd};
  __property signed char sc = {read=Fsc};
  __property TTestOptions TO = {read=FTO};
  __property TTestEnum TE = {read=FTE};
};
tmpTypeClass *gTmpTypeClass = new tmpTypeClass();
#define TypeInfo_TDateTime *(GetPropInfo(gTmpTypeClass, "DT")->PropType)
#define TypeInfo_double *(GetPropInfo(gTmpTypeClass, "d")->PropType)
#define TypeInfo_signed_char *(GetPropInfo(gTmpTypeClass, "sc")->PropType)
#define TypeInfo_TTestOptions *(GetPropInfo(gTmpTypeClass, "TO")->PropType)
#define TypeInfo_TTestEnum *(GetPropInfo(gTmpTypeClass, "TE")->PropType)

#define TypeInfo_ShortInt TypeInfo_signed_char
#define TypeInfo_TTestRange TypeInfo_ShortInt

PTypeInfo GeneratedTestEnum;
AnsiString FirstName = "Marcel";
AnsiString Initial;
AnsiString LastName = "Bestebroer";
AnsiString VerInfoStr = JVCL_VERSIONSTRING;
TDateTime ADate;
TImageIndex ImgIdx;

void __fastcall TfrmMain::AddInspectorSettings()
{
  TJvInspectorCustomCategoryItem* InspCat;
  int I;

  const AnsiString PropArray[3][2] = {
    {"UseBands", "Use bands"},
    {"WantTabs", "TAB navigates"},
    {"Painter", "Paint style"}
    };

  InspCat = new TJvInspectorCustomCategoryItem(JvInspector1->Root, NULL);
  InspCat->DisplayName = "JvInspector Settings";
  for (I = 0; I < 3; I++)
    TJvInspectorPropData::New(NULL, InspCat, JvInspector1, GetPropInfo(JvInspector1, PropArray[I][0]))->DisplayName = PropArray[I][1];

  TJvInspectorVarData::New(NULL, InspCat, "AboutJVCL", TypeInfo_AnsiString, &VerInfoStr)->DisplayName = "About JVCL";
  InspCat->Expanded = True;
}

void __fastcall TfrmMain::AddGlobalSettings()
{
  TJvInspectorCustomCategoryItem* InspCat;

  InspCat = new TJvInspectorCustomCategoryItem(JvInspector1->Root, NULL);
  InspCat->DisplayName = "Global Settings (event based data)";

  // We use ReadOnly, because we need a boolean property. See above for details.
  TJvInspectorEventData* data = dynamic_cast<TJvInspectorEventData*>(TJvInspectorEventData::New(NULL, InspCat, "Use check marks", TypeInfo_bool)->Data);
  data->OnGetAsOrdinal = GetBoolsAsChecks;
  data->OnSetAsOrdinal = SetBoolsAsChecks;
  InspCat->Expanded = True;
}

void __fastcall TfrmMain::AddFormAndControls()
{
  TJvInspectorCustomCategoryItem* InspCat;

  frmTest = new TfrmTest(this);
  InspCat = new TJvInspectorCustomCategoryItem(JvInspector1->Root, NULL);
  InspCat->DisplayName = "Form and controls (published property data).";
  InspCat->SortKind = iskNone;
  AddCtrl(InspCat, frmTest);
  AddCtrl(InspCat, frmTest->PanelForLabel);
  AddCtrl(InspCat, frmTest->lblTest);
  AddCtrl(InspCat, frmTest->Edit1);
  InspCat->Expanded = true;
}

void __fastcall TfrmMain::AddCompoundTest()
{
  TJvInspectorCustomCategoryItem* InspCat;
  TJvInspectorCompoundItem* CompItem;

  InspCat = new TJvInspectorCustomCategoryItem(JvInspector1->Root, NULL);
  InspCat->DisplayName = "Compound items test (variable or heap data).";
  CompItem = new TJvInspectorCompoundItem(InspCat, NULL);
  CompItem->AddColumn(TJvInspectorVarData::New(NULL, CompItem, "First", TypeInfo_AnsiString, &FirstName));
  CompItem->AddColumn(TJvInspectorVarData::New(NULL, CompItem, "Initial", TypeInfo_AnsiString, &Initial));
  CompItem->AddColumn(TJvInspectorVarData::New(NULL, CompItem, "Last", TypeInfo_AnsiString, &LastName));
  CompItem->Columns[0]->Width = 25;
  CompItem->Columns[1]->Width = 15;
}

void __fastcall TfrmMain::AddCtrl(const TJvCustomInspectorItem* Parent, const TControl* Ctrl)
{
  TJvInspectorCustomCategoryItem* InspCat;
//  TNotifyEvent M;

  InspCat = new TJvInspectorCustomCategoryItem(Parent, NULL);
  InspCat->DisplayName = Ctrl->Name + ": " + const_cast<TControl *>(Ctrl)->ClassName(); // The cast to avoid a warning because ClassName is not a const method
  if (Ctrl == frmTest->Edit1)
  {
    TJvInspectorTMethodItem* tmi = dynamic_cast<TJvInspectorTMethodItem*>(TJvInspectorPropData::New(NULL, InspCat, Ctrl, "OnChange"));
    tmi->AddInstance(frmTest, "frmTest");
    tmi->AddInstance(this, "frmInspector");

    // There is no way to directly convert from a TNotifyEvent to a TMethod
    // So we need to do the job ourselvs by getting the address using the method name
    // Then we set the Data member to the instance to which the method is to be applied 
//    M = frmTest->Edit1Change1;
    TMethod tmpMethod;
    tmpMethod.Code = frmTest->MethodAddress("Edit1Change1");
    tmpMethod.Data = frmTest;
    tmi->AddMethod(tmpMethod, "Edit1Change1");

//    M = Edit1Change2;
    tmpMethod.Code = MethodAddress("Edit1Change2");
    tmpMethod.Data = this;
    tmi->AddMethod(tmpMethod, "Edit1Change2");
  }
  else
    TJvInspectorPropData::New(NULL, InspCat, Ctrl);
}

void __fastcall TfrmMain::AddINIFile()
{
  TJvInspectorCustomCategoryItem* InspCat;

  INI = new TIniFile(ExtractFilePath(Application->ExeName) + "demo.ini");
  InspCat = new TJvInspectorCustomCategoryItem(JvInspector1->Root, NULL);
  InspCat->DisplayName = "INI files (INI-file data layer).";
  TJvInspectorINIFileData::New(NULL, InspCat, INI, &OnINISection, &OnINIKey);
}

void __fastcall TfrmMain::AddVarious()
{
  TJvInspectorCustomCategoryItem* InspCat;
  TJvCustomInspectorItem* NewItem;

  InspCat = new TJvInspectorCustomCategoryItem(JvInspector1->Root, NULL);
  InspCat->DisplayName = "Various tests.";
  TJvInspectorVarData::New(NULL, InspCat, "Date/time", TypeInfo_TDateTime, &ADate);
  /* Duplicate the columns of the compound items test. This will return the same data instance as
    used by the columns; a new item will be added to it. We change the name of this item, but the
    name of the data instance and the columns remain what they were. If we would change the Name
    of the data instance, this will result in all items that have the same name as the data instance
    to be renamed as well. */
  TJvInspectorVarData::New(NULL, InspCat, "First", TypeInfo_AnsiString, &FirstName)->DisplayName = "Copy of first name";
  TJvInspectorVarData::New(NULL, InspCat, "Initial", TypeInfo_AnsiString, &Initial)->DisplayName = "Copy of initial";
  TJvInspectorVarData::New(NULL, InspCat, "Last", TypeInfo_AnsiString, &LastName)->DisplayName = "Copy of last name";

  // Add an ImageIndex test item
  NewItem = TJvInspectorVarData::New(NULL, InspCat, "ImageIndex", TypeInfo_TImageIndex, &ImgIdx);
  if (dynamic_cast<TJvInspectorTImageIndexItem*>(NewItem))
    dynamic_cast<TJvInspectorTImageIndexItem*>(NewItem)->Images = TestImageList;
//  NewItem.RowSizing.MinHeight := 24;

  InspCat->Expanded = True;
}

void __fastcall TfrmMain::ChangeChkState(const TJvCustomInspectorItem* Item)
{
  if (dynamic_cast<const TJvInspectorBooleanItem*>(Item))
    const_cast<TJvInspectorBooleanItem*>(dynamic_cast<const TJvInspectorBooleanItem*>(Item))->ShowAsCheckbox = BoolsAsChecks;
  for(int I = 0; I < const_cast<TJvCustomInspectorItem*>(Item)->Count; I++)
    ChangeChkState(&(Item[I]));
}

void __fastcall TfrmMain::Edit1Change2(TObject* Sender)
{
  frmTest->mmChanges->Lines->Add("Edit1Change2 event");
}

void __fastcall TfrmMain::GetBoolsAsChecks(TJvInspectorEventData* Sender, __int64& Value)
{
  Value = static_cast<__int64>(BoolsAsChecks);
}

void __fastcall TfrmMain::SetBoolsAsChecks(TJvInspectorEventData* Sender, __int64& Value)
{
  if ((Value != 0) != BoolsAsChecks)
  {
    BoolsAsChecks = Value != 0;
    JvInspector1->BeginUpdate();
    try
    {
      ChangeChkState(JvInspector1->Root);
    }
    __finally
    {
      JvInspector1->EndUpdate();
    }
  }
}

int TextIndex(const AnsiString S, const char * List, int Size)
{
  int Result = Size-1;
  while ((Result >= 0) && !AnsiSameText(S, List[Result]))
    Result--;
  return Result;
}


void __fastcall TfrmMain::OnINISection(AnsiString& SectionName, bool& Parse)
{
  switch(TextIndex(SectionName, ("", "Global", "Section2", "TypedKeys", "EmptySection"), 5))
  {
    case 0:
      SectionName = "(no section)";
      break;
    case 1:
      SectionName = "First section";
      break;
    case 2:
      SectionName = "Second section";
      break;
    case 3:
      SectionName = "Typed elements";
      break;
    case 4:
      SectionName = "Empty section, has no keys";
      break;
    default:
      Parse = False;
  }
}

void __fastcall TfrmMain::OnINIKey(const AnsiString SectionName, AnsiString& ItemName, PTypeInfo& ATypeInfo, bool& Allow)
{
  switch(TextIndex(SectionName, ("", "Global", "Section2", "TypedKeys", "EmptySection"),5))
  {
    case 0:
      switch(TextIndex(ItemName, ("Key1WithoutSection", "Key2WithoutSection"), 2))
      {
        case 0:
          ItemName = "First key";
          break;
        case 1:
          ItemName = "Second key";
          break;
        default:
          Allow = False;
      }
    case 1:
      switch(TextIndex(ItemName, ("Key1", "Key2"), 2))
      {
        case 0:
          ItemName = "First key";
          break;
        case 1:
          ItemName = "Second key";
          break;
        default:
          Allow = False;
      }
    case 2:
      switch(TextIndex(ItemName, ("Key1", "Key2"),2))
      {
        case 0:
          ItemName = "First key";
          break;
        case 1:
          ItemName = "Second key";
          break;
        default:
          Allow = False;
      }
    case 3:
      switch(TextIndex(ItemName, ("Options", "IsValid", "Float", "Range_Int", "SomeEnum", "NewEnum"),6))
      {
        case 0:
          ATypeInfo = TypeInfo_TTestOptions;
          break;
        case 1:
          ItemName = "Valid info";
          ATypeInfo = TypeInfo_bool;
          break;
        case 2:
          ItemName = "Test float value";
          ATypeInfo = TypeInfo_double;
        case 3:
          ItemName = "Last digit";
          ATypeInfo = TypeInfo_TTestRange;
        case 4:
          ItemName = "Required number of items";
          ATypeInfo = TypeInfo_TTestEnum;
        case 5:
          ItemName = "Who or what is cool?";
          ATypeInfo = GeneratedTestEnum;
        default:
          Allow = false;
      }
    default:
      Allow = False;
  }
}

//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormShow(TObject *Sender)
{
  AddFormAndControls();
  JvInspector1->EndUpdate();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  BoolsAsChecks = True;
  JvInspector1->BeginUpdate();
  JvInspector1->Root->SortKind = iskNone;
  AddGlobalSettings();
  AddInspectorSettings();
  AddCompoundTest();
  AddINIFile();
  AddVarious();
  JvInspector1->SelectedIndex = 0;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::JvInspector1AfterItemCreate(TObject *Sender,
      const TJvCustomInspectorItem *Item)
{
  if (dynamic_cast<const TJvInspectorBooleanItem*>(Item))
    dynamic_cast<const TJvInspectorBooleanItem*>(Item)->ShowAsCheckbox = True;
  if ((Item->Data != NULL) && (CompareText(Item->Data->Name, "AboutJVCL") == 0))
    Item->ReadOnly = true;
  if ((Item->Data != NULL) && (CompareText(Item->Data->Name, "Painter") == 0))
    dynamic_cast<const TJvInspectorComponentItem*>(Item)->AddOwner(this);
}
//---------------------------------------------------------------------------
