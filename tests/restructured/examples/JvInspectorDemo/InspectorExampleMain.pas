unit InspectorExampleMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvInspector, IniFiles, TypInfo;

type
  TfrmInspector = class(TForm)
    JvInspector1: TJvInspector;
    JvInspectorBorlandPainter1: TJvInspectorBorlandPainter;
    JvInspectorDotNETPainter1: TJvInspectorDotNETPainter;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure JvInspector1AfterItemCreate(Sender: TObject; const Item: TJvCustomInspectorItem);
    procedure GetBoolsAsChecks(Sender: TJvInspectorEventData; var Value: Int64);
    procedure SetBoolsAsChecks(Sender: TJvInspectorEventData; var Value: Int64);
    procedure OnINISection(var SectionName: string; var Parse: Boolean);
    procedure OnINIKey(const SectionName: string; var ItemName: string; var ATypeInfo: PTypeInfo;
      var Allow: Boolean);
  private
    { Private declarations }
    BoolsAsChecks: Boolean;
    INI: TIniFile;
    procedure AddInspectorSettings;
    procedure AddGlobalSettings;
    procedure AddFormAndControls;
    procedure AddCompoundTest;
    procedure AddCtrl(const Parent: TJvCustomInspectorItem; const Ctrl: TControl);
    procedure AddINIFile;
    procedure ChangeChkState(const Item: TJvCustomInspectorItem);
  public
    { Public declarations }
  end;

var
  frmInspector: TfrmInspector;

implementation

{$R *.DFM}

uses
  InspectorExampleTestForm,
  JclRTTI,
  JVCLVer;

type
  TTestOption = (toOption1, toOption2, toOption3, toOption4, toOption5, toOption6, toOption7, toOption8);
  TTestOptions = set of TTestOption;
  TTestRange = 0..9;
  TTestEnum = (teNone, teAtLeastOne, teAtMostTwo, teUnlimited);

var
  GeneratedTestEnum: PTypeInfo;
  FirstName: string = 'Marcel';
  Initial: string;
  LastName: string = 'Bestebroer';
  VerInfoStr: string = JVCL_VERSIONSTRING;

{ TfrmInspector }

procedure TfrmInspector.AddInspectorSettings;
var
  InspCat: TJvInspectorCustomCategoryItem;
  I: Integer;
const
  PropArray: array[0..2, 0..1] of string = (
    ('UseBands', 'Use bands'),
    ('WantTabs', 'TAB navigates'),
    ('Painter', 'Paint style')
    );
begin
  InspCat := TJvInspectorCustomCategoryItem.Create(JvInspector1.Root, nil);
  InspCat.DisplayName := 'JvInspector Settings';
  for I := Low(PropArray) to High(PropArray) do
    TJvInspectorPropData.Create(InspCat, JvInspector1, GetPropInfo(JvInspector1, PropArray[I, 0])).Name := PropArray[I, 1];
  TJvInspectorVarData.Create('AboutJVCL', TypeInfo(string), InspCat, VerInfoStr).Name := 'About JVCL';
  InspCat.Expanded := True;
end;

procedure TfrmInspector.AddGlobalSettings;
var
  InspCat: TJvInspectorCustomCategoryItem;
begin
  InspCat := TJvInspectorCustomCategoryItem.Create(JvInspector1.Root, nil);
  InspCat.DisplayName := 'Global Settings (event based data)';
  with TJvInspectorEventData.Create(InspCat, 'Use check marks', System.TypeInfo(Boolean)) do
  begin
    OnGetAsOrdinal := GetBoolsAsChecks;
    OnSetAsOrdinal := SetBoolsAsChecks;
  end;
  InspCat.Expanded := True;
end;

procedure TfrmInspector.AddFormAndControls;
var
  InspCat: TJvInspectorCustomCategoryItem;
begin
  Application.CreateForm(TfrmTest, frmTest);
  InspCat := TJvInspectorCustomCategoryItem.Create(JvInspector1.Root, nil);
  InspCat.DisplayName := 'Form and controls (published property data).';
  InspCat.SortKind := iskNone;
  AddCtrl(InspCat, frmTest);
  AddCtrl(InspCat, frmTest.PanelForLabel);
  AddCtrl(InspCat, frmTest.lblTest);
  InspCat.Expanded := True;
end;

procedure TfrmInspector.AddCompoundTest;
var
  InspCat: TJvInspectorCustomCategoryItem;
  CompItem: TJvInspectorCompoundItem;
begin
  InspCat := TJvInspectorCustomCategoryItem.Create(JvInspector1.Root, nil);
  InspCat.DisplayName := 'Compound items test (variable or heap data).';
  CompItem := TJvInspectorCompoundItem.Create(InspCat, nil);
  CompItem.AddColumn(TJvInspectorVarData.Create('First', TypeInfo(string), CompItem, FirstName).Item);
  CompItem.AddColumn(TJvInspectorVarData.Create('Initial', TypeInfo(string), CompItem, Initial).Item);
  CompItem.AddColumn(TJvInspectorVarData.Create('Last', TypeInfo(string), CompItem, LastName).Item);
  CompItem.Columns[0].Width := 25;
  CompItem.Columns[1].Width := 15;
end;

procedure TfrmInspector.AddCtrl(const Parent: TJvCustomInspectorItem; const Ctrl: TControl);
var
  InspCat: TJvInspectorCustomCategoryItem;
begin
  InspCat := TJvInspectorCustomCategoryItem.Create(Parent, nil);
  InspCat.DisplayName := Ctrl.Name + ': ' + Ctrl.ClassName;
  TJvInspectorPropData.Create(InspCat, Ctrl);
end;

procedure TfrmInspector.AddINIFile;
var
  InspCat: TJvInspectorCustomCategoryItem;
begin
  INI := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'demo.ini');
  InspCat := TJvInspectorCustomCategoryItem.Create(JvInspector1.Root, nil);
  InspCat.DisplayName := 'INI files (INI-file data layer).';
  TJvInspectorINIFileData.Create(InspCat, INI, OnINISection, OnINIKey);
end;

procedure TfrmInspector.ChangeChkState(const Item: TJvCustomInspectorItem);
var
  I: Integer;
begin
  if Item is TJvInspectorBooleanItem then
    TJvInspectorBooleanItem(Item).ShowAsCheckbox := BoolsAsChecks;
  for I := 0 to Item.Count - 1 do
    ChangeChkState(Item[I]);
end;

type
  THackInsp = class(TJvCustomInspector);

procedure TfrmInspector.FormCreate(Sender: TObject);
begin
  BoolsAsChecks := True;
  JvInspector1.BeginUpdate;
  JvInspector1.Root.SortKind := iskNone;
  AddGlobalSettings;
  AddInspectorSettings;
  AddCompoundTest;
  AddINIFile;
  JvInspector1.SelectedIndex := 0;
end;

procedure TfrmInspector.JvInspector1AfterItemCreate(Sender: TObject; const Item: TJvCustomInspectorItem);
begin
  if Item is TJvInspectorBooleanItem then
    TJvInspectorBooleanItem(Item).ShowAsCheckbox := True;
  if (Item.Data <> nil) and (CompareText(Item.Data.Name, 'AboutJVCL') = 0) then
    Item.Readonly := True;
  if (Item.Data <> nil) and (CompareText(Item.Data.Name, 'Painter') = 0) then
    with Item as TJvInspectorComponentItem do
    begin
      AddOwner(Self);
    end;
end;

procedure TfrmInspector.GetBoolsAsChecks(Sender: TJvInspectorEventData; var Value: Int64);
begin
  Value := Ord(BoolsAsChecks);
end;

procedure TfrmInspector.SetBoolsAsChecks(Sender: TJvInspectorEventData; var Value: Int64);
begin
  if (Value <> 0) <> BoolsAsChecks then
  begin
    BoolsAsChecks := Value <> 0;
    JvInspector1.BeginUpdate;
    try
      ChangeChkState(JvInspector1.Root);
    finally
      JvInspector1.EndUpdate;
    end;
  end;
end;

function TextIndex(const S: string; const List: array of string): Integer;
begin
  Result := High(List);
  while (Result >= 0) and not AnsiSameText(S, List[Result]) do
    Dec(Result);
end;

procedure TfrmInspector.OnINISection(var SectionName: string; var Parse: Boolean);
begin
  case TextIndex(SectionName, ['', 'Global', 'Section2', 'TypedKeys', 'EmptySection']) of
    0:
      SectionName := '(no section)';
    1:
      SectionName := 'First section';
    2:
      SectionName := 'Second section';
    3:
      SectionName := 'Typed elements';
    4:
      SectionName := 'Empty section, has no keys';
  else
    Parse := False;
  end;
end;

procedure TfrmInspector.OnINIKey(const SectionName: string; var ItemName: string; var ATypeInfo: PTypeInfo;
  var Allow: Boolean);
begin
  case TextIndex(SectionName, ['', 'Global', 'Section2', 'TypedKeys', 'EmptySection']) of
    0:
      case TextIndex(ItemName, ['Key1WithoutSection', 'Key2WithoutSection']) of
        0:
          ItemName := 'First key';
        1:
          ItemName := 'Second key';
      else
        Allow := False;
      end;
    1:
      case TextIndex(ItemName, ['Key1', 'Key2']) of
        0:
          ItemName := 'First key';
        1:
          ItemName := 'Second key';
      else
        Allow := False;
      end;
    2:
      case TextIndex(ItemName, ['Key1', 'Key2']) of
        0:
          ItemName := 'First key';
        1:
          ItemName := 'Second key';
      else
        Allow := False;
      end;
    3:
      case TextIndex(ItemName, ['Options', 'IsValid', 'Float', 'Range_Int', 'SomeEnum', 'NewEnum']) of
        0:
          ATypeInfo := TypeInfo(TTestOptions);
        1:
          begin
            ItemName := 'Valid info';
            ATypeInfo := TypeInfo(Boolean);
          end;
        2:
          begin
            ItemName := 'Test float value';
            ATypeInfo := TypeInfo(Double);
          end;
        3:
          begin
            ItemName := 'Last digit';
            ATypeInfo := TypeInfo(TTestRange);
          end;
        4:
          begin
            ItemName := 'Required number of items';
            ATypeInfo := TypeInfo(TTestEnum);
          end;
        5:
          begin
            ItemName := 'Who or what is cool?';
            ATypeInfo := GeneratedTestEnum;
          end;
      else
        Allow := False;
      end;
  else
    Allow := False;
  end;
end;

procedure TfrmInspector.FormShow(Sender: TObject);
begin
  AddFormAndControls;
  JvInspector1.EndUpdate;
end;

initialization
  GeneratedTestEnum := JclGenerateEnumType('TestEnum',
    ['me, myself and I', 'Marcel Bestebroer', 'Project JEDI', 'JEDI-VCL Inspector']);

finalization
  RemoveTypeInfo(GeneratedTestEnum);
end.

