{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit InspectorExampleMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvInspector, IniFiles, TypInfo, JvComponent, ImgList, JvExControls;

type
  TfrmInspector = class(TForm)
    JvInspector1: TJvInspector;
    JvInspectorBorlandPainter1: TJvInspectorBorlandPainter;
    JvInspectorDotNETPainter1: TJvInspectorDotNETPainter;
    TestImageList: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure JvInspector1AfterItemCreate(Sender: TObject; const Item: TJvCustomInspectorItem);
    procedure GetBoolsAsChecks(Sender: TJvInspectorEventData; var Value: Int64);
    procedure SetBoolsAsChecks(Sender: TJvInspectorEventData; var Value: Int64);
    procedure OnINISection(var SectionName: string; var Parse: Boolean);
    procedure OnINIKey(const SectionName: string; var ItemName: string; var ATypeInfo: PTypeInfo;
      var Allow: Boolean);
  private
    BoolsAsChecks: Boolean;
    INI: TIniFile;
    procedure AddInspectorSettings;
    procedure AddGlobalSettings;
    procedure AddFormAndControls;
    procedure AddCompoundTest;
    procedure AddCtrl(const Parent: TJvCustomInspectorItem; const Ctrl: TControl);
    procedure AddINIFile;
    procedure AddVarious;
    procedure ChangeChkState(const Item: TJvCustomInspectorItem);
    procedure Edit1Change2(Sender: TObject);
  public
  end;

var
  frmInspector: TfrmInspector;

implementation

{$R *.dfm}

uses
  InspectorExampleTestForm,
  JclRTTI,
  JVCLVer, JvInspExtraEditors;

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
  ADate: TDateTime;
  ImgIdx: TImageIndex;

procedure TfrmInspector.AddInspectorSettings;
var
  InspCat: TJvInspectorCustomCategoryItem;
  I: Integer;
const
  PropArray: array [0..2, 0..1] of string =
   (
    ('UseBands', 'Use bands'),
    ('WantTabs', 'TAB navigates'),
    ('Painter', 'Paint style')
   );
begin
  InspCat := TJvInspectorCustomCategoryItem.Create(JvInspector1.Root, nil);
  InspCat.DisplayName := 'JvInspector Settings';
  for I := Low(PropArray) to High(PropArray) do
    TJvInspectorPropData.New(InspCat, JvInspector1, GetPropInfo(JvInspector1, PropArray[I, 0])).DisplayName := PropArray[I, 1];
  TJvInspectorVarData.New(InspCat, 'AboutJVCL', TypeInfo(string), @VerInfoStr).DisplayName := 'About JVCL';
  InspCat.Expanded := True;
end;

procedure TfrmInspector.AddGlobalSettings;
var
  InspCat: TJvInspectorCustomCategoryItem;
begin
  InspCat := TJvInspectorCustomCategoryItem.Create(JvInspector1.Root, nil);
  InspCat.DisplayName := 'Global Settings (event based data)';
  with TJvInspectorEventData(TJvInspectorEventData.New(InspCat, 'Use check marks', System.TypeInfo(Boolean)).Data) do
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
  frmTest := TfrmTest.Create(Self);
  InspCat := TJvInspectorCustomCategoryItem.Create(JvInspector1.Root, nil);
  InspCat.DisplayName := 'Form and controls (published property data).';
  InspCat.SortKind := iskNone;
  AddCtrl(InspCat, frmTest); 
  AddCtrl(InspCat, frmTest.PanelForLabel);
  AddCtrl(InspCat, frmTest.lblTest);
  AddCtrl(InspCat, frmTest.Edit1);
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
  CompItem.AddColumn(TJvInspectorVarData.New(CompItem, 'First', TypeInfo(string), @FirstName));
  CompItem.AddColumn(TJvInspectorVarData.New(CompItem, 'Initial', TypeInfo(string), @Initial));
  CompItem.AddColumn(TJvInspectorVarData.New(CompItem, 'Last', TypeInfo(string), @LastName));
  CompItem.Columns[0].Width := 25;
  CompItem.Columns[1].Width := 15;
end;

procedure TfrmInspector.AddCtrl(const Parent: TJvCustomInspectorItem; const Ctrl: TControl);
var
  InspCat: TJvInspectorCustomCategoryItem;
  M: TNotifyEvent;
begin
  InspCat := TJvInspectorCustomCategoryItem.Create(Parent, nil);
  InspCat.DisplayName := Ctrl.Name + ': ' + Ctrl.ClassName;
  if Ctrl = frmTest.Edit1 then
  begin
    with TJvInspectorPropData.New(InspCat, Ctrl, 'OnChange') as TJvInspectorTMethodItem do
    begin
      AddInstance(frmTest, 'frmTest');
      AddInstance(Self, 'frmInspector');
      M := frmTest.Edit1Change1;
      AddMethod(TMethod(M), 'Edit1Change1');
      M := Edit1Change2;
      AddMethod(TMethod(M), 'Edit1Change2');
    end;
  end
  else
    TJvInspectorPropData.New(InspCat, Ctrl);
end;

procedure TfrmInspector.AddINIFile;
var
  InspCat: TJvInspectorCustomCategoryItem;
begin
  INI := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'demo.ini');
  InspCat := TJvInspectorCustomCategoryItem.Create(JvInspector1.Root, nil);
  InspCat.DisplayName := 'INI files (INI-file data layer).';
  TJvInspectorINIFileData.New(InspCat, INI, OnINISection, OnINIKey);
end;

procedure TfrmInspector.AddVarious;
var
  InspCat: TJvInspectorCustomCategoryItem;
  NewItem: TJvCustomInspectorItem;
begin
  InspCat := TJvInspectorCustomCategoryItem.Create(JvInspector1.Root, nil);
  InspCat.DisplayName := 'Various tests.';
  TJvInspectorVarData.New(InspCat, 'Date/time', TypeInfo(TDateTime), @ADate);
  { Duplicate the columns of the compound items test. This will return the same data instance as
    used by the columns; a new item will be added to it. We change the name of this item, but the
    name of the data instance and the columns remain what they were. If we would change the Name
    of the data instance, this will result in all items that have the same name as the data instance
    to be renamed as well. }
  TJvInspectorVarData.New(InspCat, 'First', TypeInfo(string), @FirstName).DisplayName := 'Copy of first name';
  TJvInspectorVarData.New(InspCat, 'Initial', TypeInfo(string), @Initial).DisplayName := 'Copy of initial';
  TJvInspectorVarData.New(InspCat, 'Last', TypeInfo(string), @LastName).DisplayName := 'Copy of last name';

  { Add an ImageIndex test item }
  NewItem := TJvInspectorVarData.New(InspCat, 'ImageIndex', TypeInfo(TImageIndex), @ImgIdx);
  if NewItem is TJvInspectorTImageIndexItem then
    (NewItem as TJvInspectorTImageIndexItem).Images := TestImageList;
//  NewItem.RowSizing.MinHeight := 24;

  InspCat.Expanded := True;
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

procedure TfrmInspector.Edit1Change2(Sender: TObject);
begin
  frmTest.mmChanges.Lines.Add('Edit1Change2 event');
end;

procedure TfrmInspector.FormCreate(Sender: TObject);
begin
  BoolsAsChecks := True;
  JvInspector1.BeginUpdate;
  JvInspector1.Root.SortKind := iskNone;
  AddGlobalSettings;
  AddInspectorSettings;
  AddCompoundTest;
  AddINIFile;
  AddVarious;
  JvInspector1.SelectedIndex := 0;
end;

procedure TfrmInspector.JvInspector1AfterItemCreate(Sender: TObject; const Item: TJvCustomInspectorItem);
begin
  if Item is TJvInspectorBooleanItem then
    TJvInspectorBooleanItem(Item).ShowAsCheckbox := True;
  if (Item.Data <> nil) and (CompareText(Item.Data.Name, 'AboutJVCL') = 0) then
    Item.ReadOnly := True;
  if (Item.Data <> nil) and (CompareText(Item.Data.Name, 'Painter') = 0) then
    with Item as TJvInspectorComponentItem do
    begin
      AddOwner(Self);
    end
  else if Item is TJvInspectorComponentItem then
    with Item as TJvInspectorComponentItem do
    begin
      AddOwner(frmTest);
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
    ['Me, myself and I', 'Marcel Bestebroer', 'Project JEDI', 'JEDI-VCL Inspector']);
  ADate := Now;

  TJvInspectorAlignItem.RegisterAsDefaultItem;
  TJvInspectorAnchorsItem.RegisterAsDefaultItem;
  TJvInspectorColorItem.RegisterAsDefaultItem;
  TJvInspectorTImageIndexItem.RegisterAsDefaultItem;

finalization
  RemoveTypeInfo(GeneratedTestEnum);

end.

