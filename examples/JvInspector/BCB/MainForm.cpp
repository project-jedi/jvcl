//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MainForm.h"
#include <typeinfo>
#include "TestForm.h"
#include <JclRTTI.hpp>
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

// JvInspector.hpp provides a TypeInfo helper class for most common
// types but the types that we defined locally are of course not taken
// into account. So we declare a class that will be used to gather
// the type info for various types defined in this file
class TLocalTypeInfoHelper: public TJvTypeInfoHelper
{
private:
  TTestOptions FTTestOptionsProp;
  TTestEnum FTTestEnumProp;
  TTestRange FTTestRangeProp;

__published:
  __property TTestOptions TTestOptionsProp = {read=FTTestOptionsProp};
  __property TTestEnum TTestEnumProp = {read=FTTestEnumProp};
  __property TTestRange TTestRangeProp = {read=FTTestRangeProp};
};

// Some global variables. They may as well be static members
// of TfrmMain, but to remain consistent with the Delphi example,
// we left them here
static PTypeInfo GeneratedTestEnum = JclGenerateEnumType("TestEnum",
    OPENARRAY(AnsiString,("me, myself and I", "Marcel Bestebroer", "Project JEDI", "JEDI-VCL Inspector")));;
static AnsiString FirstName = "Marcel";
static AnsiString Initial;
static AnsiString LastName = "Bestebroer";
static AnsiString VerInfoStr = JVCL_VERSIONSTRING;
static TDateTime ADate = Now();
static TImageIndex ImgIdx;

// We need to emulate the initialization section of Delphi units so
// that we can register some specialized type inspectors
// The only way is to declare a dummy class with the initialization
// code in its constructor and the finalization code in its destructor.
// Then we declare a static variable instance of that class and that
// will do a work similar to the initialization and finalization
// sections of a Delphi unit.
// Please note that this is a construct that should be avoided as
// much as possible as it keeps a useless object in memory during the
// whole life of the project
class TInitializer
{
public:
  TInitializer()
  {
    // Register the item inspectors from the Extra Inspector unit
    TJvInspectorAlignItem::RegisterAsDefaultItem(__classid(TJvInspectorAlignItem));
    TJvInspectorAnchorsItem::RegisterAsDefaultItem(__classid(TJvInspectorAnchorsItem));
    TJvInspectorColorItem::RegisterAsDefaultItem(__classid(TJvInspectorColorItem));
    TJvInspectorTImageIndexItem::RegisterAsDefaultItem(__classid(TJvInspectorTImageIndexItem));

    // Register our TypeInfo helper class
    RegisterTypeInfoHelper(__classid(TLocalTypeInfoHelper));
  };

  ~TInitializer()
  {
    // You need the very latest CVS version for this not to provoke
    // a linker error. (QC 7224)
    RemoveTypeInfo(GeneratedTestEnum);
  };
};
static TInitializer Initializer;

//--------------------------------------------------------------------
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
  // The last argument to GetPropInfo is required by BCB5 and set to the same default value as found in BCB6
  for (I = 0; I < 3; I++)
    TJvInspectorPropData::New(__classid(TJvInspectorPropData), InspCat, JvInspector1, GetPropInfo(JvInspector1, PropArray[I][0], System::Set<Typinfo::TTypeKind, tkUnknown, tkDynArray>()))->DisplayName = PropArray[I][1];

  TJvInspectorVarData::New(__classid(TJvInspectorVarData), InspCat, "AboutJVCL", TypeInfo(AnsiString), &VerInfoStr)->DisplayName = "About JVCL";
  InspCat->Expanded = True;
}

void __fastcall TfrmMain::AddGlobalSettings()
{
  TJvInspectorCustomCategoryItem* InspCat;

  InspCat = new TJvInspectorCustomCategoryItem(JvInspector1->Root, NULL);
  InspCat->DisplayName = "Global Settings (event based data)";

  TJvInspectorEventData* data = dynamic_cast<TJvInspectorEventData*>(TJvInspectorEventData::New(__classid(TJvInspectorEventData), InspCat, "Use check marks", TypeInfo(bool))->Data);
  data->OnGetAsOrdinal = GetBoolsAsChecks;
  data->OnSetAsOrdinal = SetBoolsAsChecks;
  InspCat->Expanded = True;
}

void __fastcall TfrmMain::AddFormAndControls()
{
  TJvInspectorCustomCategoryItem* InspCat;

  frmTest = new TfrmTest(this);
  frmTest->Show();
  Show();
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
  CompItem->AddColumn(TJvInspectorVarData::New(__classid(TJvInspectorVarData), CompItem, "First", TypeInfo(AnsiString), &FirstName));
  CompItem->AddColumn(TJvInspectorVarData::New(__classid(TJvInspectorVarData), CompItem, "Initial", TypeInfo(AnsiString), &Initial));
  CompItem->AddColumn(TJvInspectorVarData::New(__classid(TJvInspectorVarData), CompItem, "Last", TypeInfo(AnsiString), &LastName));
  CompItem->Columns[0]->Width = 25;
  CompItem->Columns[1]->Width = 15;
}

void __fastcall TfrmMain::AddCtrl(TJvCustomInspectorItem* Parent, TControl* Ctrl)
{
  TJvInspectorCustomCategoryItem* InspCat;
//  TNotifyEvent M;

  InspCat = new TJvInspectorCustomCategoryItem(Parent, NULL);
  InspCat->DisplayName = Ctrl->Name + ": " + Ctrl->ClassName(); // The cast to avoid a warning because ClassName is not a const method
  if (Ctrl == frmTest->Edit1)
  {
    TJvInspectorTMethodItem* tmi = dynamic_cast<TJvInspectorTMethodItem*>(TJvInspectorPropData::New(__classid(TJvInspectorPropData), InspCat, Ctrl, "OnChange"));
    tmi->AddInstance(frmTest, "frmTest");
    tmi->AddInstance(this, "frmInspector");

    // There is no way to directly convert from a TNotifyEvent
    // to a TMethod
    // So we need to do the job ourselves by getting the address
    // using the method name.
    // Then we set the Data member to the instance to which the
    // method is to be applied
    // Note 1: the method which name is to be retrieved MUST be
    // published or MethodAddress will return NULL
    // Note 2: the pointer used for Data MUST be point to a valid
    // instance before being used here
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
    // The last argument is required by BCB5 but is not by BCB6
    TJvInspectorPropData::New(__classid(TJvInspectorPropData), InspCat, Ctrl, System::Set<Typinfo::TTypeKind, tkUnknown, tkDynArray> ());
}

void __fastcall TfrmMain::AddINIFile()
{
  TJvInspectorCustomCategoryItem* InspCat;

  AnsiString IniFileName = ExtractFilePath(Application->ExeName);
  IniFileName += "demo.ini";
  INI = new TIniFile(IniFileName);
  InspCat = new TJvInspectorCustomCategoryItem(JvInspector1->Root, NULL);
  InspCat->DisplayName = "INI files (INI-file data layer).";
  TJvInspectorINIFileData::New(__classid(TJvInspectorINIFileData), InspCat, INI, &OnINISection, &OnINIKey);
}

void __fastcall TfrmMain::AddVarious()
{
  TJvInspectorCustomCategoryItem* InspCat;
  TJvCustomInspectorItem* NewItem;

  InspCat = new TJvInspectorCustomCategoryItem(JvInspector1->Root, NULL);
  InspCat->DisplayName = "Various tests.";
  TJvInspectorVarData::New(__classid(TJvInspectorVarData), InspCat, "Date/time", TypeInfo(TDateTime), &ADate);
  /* Duplicate the columns of the compound items test. This will return the same data instance as
    used by the columns; a new item will be added to it. We change the name of this item, but the
    name of the data instance and the columns remain what they were. If we would change the Name
    of the data instance, this will result in all items that have the same name as the data instance
    to be renamed as well. */
  TJvInspectorVarData::New(__classid(TJvInspectorVarData), InspCat, "First", TypeInfo(AnsiString), &FirstName)->DisplayName = "Copy of first name";
  TJvInspectorVarData::New(__classid(TJvInspectorVarData), InspCat, "Initial", TypeInfo(AnsiString), &Initial)->DisplayName = "Copy of initial";
  TJvInspectorVarData::New(__classid(TJvInspectorVarData), InspCat, "Last", TypeInfo(AnsiString), &LastName)->DisplayName = "Copy of last name";

  // Add an ImageIndex test item
  NewItem = TJvInspectorVarData::New(__classid(TJvInspectorVarData), InspCat, "ImageIndex", TypeInfo(TImageIndex), &ImgIdx);
  if (dynamic_cast<TJvInspectorTImageIndexItem*>(NewItem))
    dynamic_cast<TJvInspectorTImageIndexItem*>(NewItem)->Images = TestImageList;
//  NewItem.RowSizing.MinHeight := 24;

  InspCat->Expanded = True;
}

void __fastcall TfrmMain::ChangeChkState(TJvCustomInspectorItem* Item)
{
  if (dynamic_cast<TJvInspectorBooleanItem*>(Item))
    dynamic_cast<TJvInspectorBooleanItem*>(Item)->ShowAsCheckbox = BoolsAsChecks;
  for(int I = 0; I < Item->Count; I++)
    ChangeChkState(Item->Items[I]); // Do not use Item[I], as Item is a pointer itself
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

int TextIndex(const AnsiString S, const AnsiString * List, int LastIndex)
{
  int Result = LastIndex;
  while ((Result >= 0) && !AnsiSameText(S, List[Result]))
    Result--;
  return Result;
}


void __fastcall TfrmMain::OnINISection(AnsiString& SectionName, bool& Parse)
{
  switch(TextIndex(SectionName, OPENARRAY(AnsiString, ("", "Global", "Section2", "TypedKeys", "EmptySection"))))
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
  switch(TextIndex(SectionName, OPENARRAY(AnsiString, ("", "Global", "Section2", "TypedKeys", "EmptySection"))))
  {
    case 0:
      switch(TextIndex(ItemName, OPENARRAY(AnsiString, ("Key1WithoutSection", "Key2WithoutSection"))))
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
      break;
    case 1:
      switch(TextIndex(ItemName, OPENARRAY(AnsiString, ("Key1", "Key2"))))
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
      break;
    case 2:
      switch(TextIndex(ItemName, OPENARRAY(AnsiString, ("Key1", "Key2"))))
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
      break;
    case 3:
      switch(TextIndex(ItemName, OPENARRAY(AnsiString, ("Options", "IsValid", "Float", "Range_Int", "SomeEnum", "NewEnum"))))
      {
        case 0:
          ATypeInfo = TypeInfo(TTestOptions);
          break;
        case 1:
          ItemName = "Valid info";
          ATypeInfo = TypeInfo(bool);
          break;
        case 2:
          ItemName = "Test float value";
          ATypeInfo = TypeInfo(double);
          break;
        case 3:
          ItemName = "Last digit";
          ATypeInfo = TypeInfo(TTestRange);
          break;
        case 4:
          ItemName = "Required number of items";
          ATypeInfo = TypeInfo(TTestEnum);
          break;
        case 5:
          ItemName = "Who or what is cool?";
          ATypeInfo = GeneratedTestEnum;
          break;
        default:
          Allow = false;
      }
      break;
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
      TJvCustomInspectorItem *Item)
{
  if (dynamic_cast<TJvInspectorBooleanItem*>(Item))
    dynamic_cast<TJvInspectorBooleanItem*>(Item)->ShowAsCheckbox = True;
  if ((Item->Data != NULL) && (CompareText(Item->Data->Name, "AboutJVCL") == 0))
    Item->ReadOnly = true;
  if ((Item->Data != NULL) && (CompareText(Item->Data->Name, "Painter") == 0))
    dynamic_cast<TJvInspectorComponentItem*>(Item)->AddOwner(this);
}
//---------------------------------------------------------------------------


