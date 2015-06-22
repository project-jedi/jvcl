{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.delphi-jedi.org

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

unit JvParameterListMainForm;

{$I jvcl.inc}

interface

{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}
{$DEFINE INCLUDE_DEVEXP_CX}
{$ENDIF}

uses
  JclUnitVersioning,
  Windows, Messages, SysUtils, {Variants, } Classes, Graphics, Controls, Forms,
  Dialogs, JvDsaDialogs, JvParameterList, StdCtrls, JvParameterListParameter, Mask,
  JvToolEdit, JvPanel,
  {$IFDEF INCLUDE_DEVEXP_CX}
  cxButtons, cxListBox, cxRadioGroup, cxLookAndFeelPainters,
  cxDropDownEdit, cxCalendar, cxSpinEdit, cxTimeEdit, cxCheckBox, cxMemo,
  cxTextEdit, cxMaskEdit, cxControls, cxContainer, cxEdit, cxLabel,
  cxImage, cxCheckListBox,
  cxGroupBox, cxButtonEdit,
  {$ENDIF INCLUDE_DEVEXP_CX}
  ExtCtrls, JvFormPlacement, JvComponent, JvAppStorage,
  JvAppRegistryStorage, JvDynControlEngine, ComCtrls, Buttons, JvBitBtn,
  JvCombobox, CheckLst, ShlObj, ExtDlgs, JvImage,
  JvMaskEdit, JvSpin, JvBaseEdits, JvGroupBox, JvExStdCtrls,
  JvExExtCtrls, JvAppXMLStorage, JvCipher, JvComponentBase, JvExtComponent;

type

  TJvParameterListDemoMainFrm = class(TForm)
    GroupBox1: TGroupBox;
    AutoWidthCheckBox: TCheckBox;
    AutoHeightCheckBox: TCheckBox;
    LoadFromCheckBox: TCheckBox;
    StoreToCheckBox: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    MaxWidthEdit: TMaskEdit;
    MaxHeightEdit: TMaskEdit;
    Label3: TLabel;
    WidthEdit: TMaskEdit;
    Label4: TLabel;
    HeightEdit: TMaskEdit;
    JvAppRegistryStorage: TJvAppRegistryStorage;
    JvFormStorage1: TJvFormStorage;
    JvFormStorage2: TJvFormStorage;
    HistoryEnabledCheckBox: TCheckBox;
    Image1: TImage;
    GroupBox2: TGroupBox;
    DevExpCxLookAndFeelRadioGroup: TRadioGroup;
    DevExpCxStyleGroupBox: TGroupBox;
    ShadowCheckBox: TCheckBox;
    ThickLinesCheckBox: TCheckBox;
    GroupBox3: TGroupBox;
    VCLRadioButton: TRadioButton;
    JVCLRadioButton: TRadioButton;
    CxRadioButton: TRadioButton;
    VclRedRadioButton: TRadioButton;
    GroupBox4: TGroupBox;
    JvPanel1: TJvPanel;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    JvGroupBox1: TJvGroupBox;
    JvGroupBox2: TJvGroupBox;
    JvPanel3: TJvPanel;
    Button13: TButton;
    JvGroupBox3: TJvGroupBox;
    JvPanel4: TJvPanel;
    Button12: TButton;
    JvGroupBoxAllControls: TJvGroupBox;
    JvPanelAllControls: TJvPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button11: TButton;
    GroupBox5: TGroupBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label5: TLabel;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    JvAppXMLStorage: TJvAppXMLFileStorage;
    Cipher: TJvVigenereCipher;
    GroupBox6: TGroupBox;
    StaticText5: TStaticText;
    Button14: TButton;
    GroupBox7: TGroupBox;
    DefaultParameterWidthEdit: TMaskEdit;
    Label6: TLabel;
    Label7: TLabel;
    DefaultParameterLabelWidthEdit: TMaskEdit;
    AssignWidthHeightCheckBox: TCheckBox;
    JvPanel2: TJvPanel;
    Button16: TButton;
    Button15: TButton;
    Button17: TButton;
    Button18: TButton;
    Button19: TButton;
    Button20: TButton;
    NativeCheckBox: TCheckBox;
    Button21: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure DevExpCxLookAndFeelRadioGroupClick(Sender: TObject);
    procedure VCLRadioButtonClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure JvPanelAllControlsResize(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure JvAppRegistryStorageDecryptPropertyValue(var Value: string);
    procedure JvAppRegistryStorageEncryptPropertyValue(var Value: string);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure AssignWidthHeightCheckBoxClick(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Button19Click(Sender: TObject);
    procedure Button20Click(Sender: TObject);
    procedure Button21Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
  private
    procedure ShowTest3ButttonClick(const ParameterList: TJvParameterList; const Parameter: TJvBaseParameter);
    function DefaultStorage: TJvCustomAppStorage;
  {$IFDEF INCLUDE_DEVEXP_CX}
    procedure SetDevExpressDynControlEngineProperties(ParameterList:
        TJvParameterList);
  {$ENDIF INCLUDE_DEVEXP_CX}

  public
    { Public-Deklarationen }
    procedure ShowTest1(const aDynControlEngine: tJvDynControlEngine);
    procedure ShowTest2(const aDynControlEngine: tJvDynControlEngine);
    procedure ShowTest3(const aDynControlEngine: tJvDynControlEngine);
    procedure ShowTestCrypt(const aDynControlEngine: tJvDynControlEngine);
  end;

var
  JvParameterListDemoMainFrm: TJvParameterListDemoMainFrm;

const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\examples\JvParameterList'
    );

implementation

{$R *.dfm}

uses JvDynControlEngineVCL,
  JvDynControlEngineJVCL,
  JclBase, JclFileUtils,
  JvFormPlacementSelectList,
  JvDynControlEngineVCLRed,
  {$IFDEF INCLUDE_DEVEXP_CX}
  JvDynControlEngineDevExpCx,
  cxLookAndFeels,
  {$ENDIF INCLUDE_DEVEXP_CX}
  JvDynControlEngineIntf, JclStrings, JvJCLUtils, JvJclUnitVersioningBrowser;

procedure TJvParameterListDemoMainFrm.Button1Click(Sender: TObject);
begin
  ShowTest1(nil);
end;

procedure TJvParameterListDemoMainFrm.Button2Click(Sender: TObject);
begin
  ShowTest1(DynControlEngineVCL);
end;

procedure TJvParameterListDemoMainFrm.Button3Click(Sender: TObject);
begin
  ShowTest1(DynControlEngineJVCL);
end;

procedure TJvParameterListDemoMainFrm.Button4Click(Sender: TObject);
begin
  {$IFDEF INCLUDE_DEVEXP_CX}
  ShowTest1(DynControlEngineDevExpCx);
  {$ENDIF INCLUDE_DEVEXP_CX}
end;

procedure TJvParameterListDemoMainFrm.ShowTest1(const aDynControlEngine: tJvDynControlEngine);
var
  ParameterList: TJvParameterList;
  Parameter: TJvBaseParameter;
begin
  ParameterList := TJvParameterList.Create(self);
  try
    if Assigned(aDynControlEngine) then
      ParameterList.DynControlEngine := aDynControlEngine;
    {$IFDEF INCLUDE_DEVEXP_CX}
    SetDevExpressDynControlEngineProperties(ParameterList);
    {$ENDIF INCLUDE_DEVEXP_CX}
    if AutoHeightCheckBox.Checked then
      if AutoWidthCheckBox.Checked then
        ParameterList.ArrangeSettings.AutoSize := JvPanel.asBoth
      else
        ParameterList.ArrangeSettings.AutoSize := JvPanel.asHeight
    else
    if AutoWidthCheckBox.Checked then
      ParameterList.ArrangeSettings.AutoSize := JvPanel.asWidth
    else
      ParameterList.ArrangeSettings.AutoSize := JvPanel.asNone;
    Parameter := tjvTimeParameter.Create(ParameterList);
    with tjvTimeParameter(Parameter) do
    begin
      SearchName := 'TimeTest';
      Caption := 'TimeTest';
      Format := 'HH:mm:ss';
//      Width      := 100;
      AsDate := Now;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvDateParameter.Create(ParameterList);
    with tjvDateParameter(Parameter) do
    begin
      SearchName := 'DateTest';
      Caption := 'DateTest';
//      Width      := 80;
      AsDate := Now - 10;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvDateTimeParameter.Create(ParameterList);
    with tjvDateTimeParameter(Parameter) do
    begin
      SearchName := 'DateTimeTest';
      Caption := 'DateTimeTest';
      AsDate := Now;
      Width := 365;
      EditWidth := 200;
//      Width      := 200;
//      Height     := 50;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvEditParameter.Create(ParameterList);
    with tjvEditParameter(Parameter) do
    begin
      SearchName := 'EditTest';
      Caption := 'EditTest';
//      AsString   := Edit1.Text;
      EditMask := '09999';
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvEditParameter.Create(ParameterList);
    with tjvEditParameter(Parameter) do
    begin
      SearchName := 'PasswordTest';
      Caption := 'PasswordTest';
//      AsString   := Edit1.Text;
      PasswordChar := '*';
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvCheckboxParameter.Create(ParameterList);
    with Parameter do
    begin
      SearchName := 'CheckboxTest';
      Caption := '&Checkbox';
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvIntegerEditParameter.Create(ParameterList);
    with Parameter do
    begin
      SearchName := 'IntegerTest';
      Caption := 'IntegerTest';
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvIntegerEditParameter.Create(ParameterList);
    with tjvIntegerEditParameter(Parameter) do
    begin
      SearchName := 'IntegerTestCalc';
      Caption := 'IntegerTest Calc';
      EditorType := netCalculate;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvIntegerEditParameter.Create(ParameterList);
    with tjvIntegerEditParameter(Parameter) do
    begin
      SearchName := 'IntegerTestSpin';
      Caption := 'IntegerTest Spin';
      EditorType := netSpin;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvButtonEditParameter.Create(ParameterList);
    with tjvButtonEditParameter(Parameter) do
    begin
      SearchName := 'ButtonEditTest';
      Caption := 'ButtonEditTest';
      OnClick := Button5Click;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvFileNameParameter.Create(ParameterList);
    with TJvFileNameParameter(Parameter) do
    begin
      SearchName := 'FileNameTest';
      Caption := 'FileNameTest';
      labelArrangeMode := lamAbove;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvDirectoryParameter.Create(ParameterList);
    with TJvDirectoryParameter(Parameter) do
    begin
      SearchName := 'DirectoryTest';
      Caption := 'DirectoryTest';
      labelArrangeMode := lamAbove;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvMemoParameter.Create(ParameterList);
    with tjvMemoParameter(Parameter) do
    begin
      SearchName := 'MemoTest';
      Caption := 'MemoTest';
      //Height     := 60;
      Scrollbars := ssBoth;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvRichEditParameter.Create(ParameterList);
    with tjvRichEditParameter(Parameter) do
    begin
      SearchName := 'RichEditTest';
      Caption := 'RichEditTest';
      //Height     := 60;
      Scrollbars := ssBoth;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvRadioGroupParameter.Create(ParameterList);
    with tjvRadioGroupParameter(Parameter) do
    begin
      SearchName := 'RadioGroupTest';
      Caption := '&RadioGroupTest';
      ItemList.Add('Test&1');
      ItemList.Add('Test&2');
      ItemList.Add('Test&3');
      ItemList.Add('Test&4');
      ItemList.Add('Test&5');
      ReadOnly := True;
      ItemIndex := 2;
      Columns := 2;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvListBoxParameter.Create(ParameterList);
    with tjvListBoxParameter(Parameter) do
    begin
      SearchName := 'ListBoxTest';
      Caption := '&ListBoxTest';
      ItemList.Add('Listbox Test&1');
      ItemList.Add('Listbox Test&2');
      ItemList.Add('Listbox Test&3');
      Height := 80;
      Width := 80;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvCheckListBoxParameter.Create(ParameterList);
    with tjvCheckListBoxParameter(Parameter) do
    begin
      SearchName := 'CheckListBoxTest';
      Caption := '&CheckListBoxTest';
      ItemList.Add('CheckListBox Test&1');
      ItemList.Add('CheckListBox Test&2');
      ItemList.Add('CheckListBox Test&3');
      AddCheckListBoxItem('CheckListBox Header&1', cbUnchecked, False, True);
      AddCheckListBoxItem('CheckListBox Test&4', cbUnchecked);
      Height := 80;
      Width := 180;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvImageParameter.Create(ParameterList);
    with tjvImageParameter(Parameter) do
    begin
      Picture := Image1.Picture;
      SearchName := 'PictureTest';
      Caption := 'PictureTest';
//      AutoSize := True;
      Height := 280;
      Width := 340;
    end;
    ParameterList.AddParameter(Parameter);

    ParameterList.DefaultParameterWidth := 180;
    ParameterList.DefaultParameterLabelWidth := 80;
    ParameterList.MaxHeight := StrToInt(MaxHeightEdit.Text);
    ParameterList.MaxWidth := StrToInt(MaxWidthEdit.Text);
    if AssignWidthHeightCheckBox.Checked then
    begin
      ParameterList.Height := StrToInt(HeightEdit.Text);
      ParameterList.Width := StrToInt(WidthEdit.Text);
    end;
    ParameterList.HistoryEnabled := HistoryEnabledCheckBox.Checked;
    ParameterList.DefaultParameterWidth := StrToInt(DefaultParameterWidthEdit.Text);
    ParameterList.DefaultParameterLabelWidth := StrToInt(DefaultParameterLabelWidthEdit.Text);
    ParameterList.AppStorage := DefaultStorage;
    ParameterList.AppStoragePath := 'Dialog 1';
    if LoadFromCheckBox.Checked then
      ParameterList.LoadData;
    if ParameterList.ShowParameterDialog then
      if StoreToCheckBox.Checked then
        ParameterList.storeData;
//    Edit1.text := ParameterList.parameterByName('RadioGroupTest').AsString;
  finally
    ParameterList.Free;
  end;
end;

procedure TJvParameterListDemoMainFrm.ShowTest2(const aDynControlEngine: tJvDynControlEngine);
var
  ParameterList: TJvParameterList;
  Parameter: TJvBaseParameter;
begin
  ParameterList := TJvParameterList.Create(self);
  try
    if Assigned(aDynControlEngine) then
      ParameterList.DynControlEngine := aDynControlEngine;
    {$IFDEF INCLUDE_DEVEXP_CX}
    SetDevExpressDynControlEngineProperties(ParameterList);
    {$ENDIF INCLUDE_DEVEXP_CX}
    Parameter := tjvRadioGroupParameter.Create(ParameterList);
    with tjvRadioGroupParameter(Parameter) do
    begin
      SearchName := 'RadioGroup';
      Caption := '&Enabled';
      ItemList.Add('1 Enabled');
      ItemList.Add('2 Enabled');
      ItemList.Add('Both Enabled');
      ItemList.Add('Both Disabled');
      ItemIndex := 2;
      Columns := 2;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvCheckboxParameter.Create(ParameterList);
    with Parameter do
    begin
      SearchName := 'Checkbox';
      Caption := '2 Enabled';
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvEditParameter.Create(ParameterList);
    with Parameter do
    begin
      SearchName := 'Edit1';
      Caption := 'Edit Test 1';
//      Width      := 80;
      AsDate := Now;
      DisableReasons.AddReason('RadioGroup', '2 Enabled');
      DisableReasons.AddReason('RadioGroup', 'Both Disabled');
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvEditParameter.Create(ParameterList);
    with Parameter do
    begin
      SearchName := 'Edit2';
      Caption := 'Edit Test 2';
//      Width      := 80;
      AsDate := Now;
      DisableReasons.AddReason('RadioGroup', '1 Enabled');
      DisableReasons.AddReason('RadioGroup', 'Both Disabled');
      DisableReasons.AddReason('Checkbox', False);
    end;
    ParameterList.AddParameter(Parameter);
    if AutoHeightCheckBox.Checked then
      if AutoWidthCheckBox.Checked then
        ParameterList.ArrangeSettings.AutoSize := JvPanel.asBoth
      else
        ParameterList.ArrangeSettings.AutoSize := JvPanel.asHeight
    else
    if AutoWidthCheckBox.Checked then
      ParameterList.ArrangeSettings.AutoSize := JvPanel.asWidth
    else
      ParameterList.ArrangeSettings.AutoSize := JvPanel.asNone;
    ParameterList.OkButtonDisableReasons.AddReason('CheckBox', True);
    ParameterList.MaxHeight := StrToInt(MaxHeightEdit.Text);
    ParameterList.MaxWidth := StrToInt(MaxWidthEdit.Text);
    if AssignWidthHeightCheckBox.Checked then
    begin
      ParameterList.Height := StrToInt(HeightEdit.Text);
      ParameterList.Width := StrToInt(WidthEdit.Text);
    end;
    ParameterList.DefaultParameterWidth := StrToInt(DefaultParameterWidthEdit.Text);
    ParameterList.DefaultParameterLabelWidth := StrToInt(DefaultParameterLabelWidthEdit.Text);
    ParameterList.HistoryEnabled := HistoryEnabledCheckBox.Checked;
    ParameterList.AppStorage := DefaultStorage;
    ParameterList.AppStoragePath := 'Dialog 2';
    if LoadFromCheckBox.Checked then
      ParameterList.LoadData;
    if ParameterList.ShowParameterDialog then
      if StoreToCheckBox.Checked then
        ParameterList.storeData;
//    Edit1.text := ParameterList.parameterByName('RadioGroupTest').AsString;
  finally
    ParameterList.Free;
  end;
end;

procedure TJvParameterListDemoMainFrm.ShowTest3ButttonClick(const ParameterList: TJvParameterList; const Parameter:
  TJvBaseParameter);
begin
  if Assigned(Parameter) then
    if Parameter.SearchName = 'ButtonA' then
      Button5Click(nil)
    else
    if Parameter.SearchName = 'ButtonB' then
      Button6Click(nil)
    else
    if Parameter.SearchName = 'ButtonC' then
      Button7Click(nil)
    else
    if Parameter.SearchName = 'ButtonD' then
      Button8Click(nil)
    else
    if Parameter.SearchName = 'ButtonE' then
      if Assigned(ParameterList) then
        // (p3) type cast variant to string (for D5)
        MessageDlg('Edit 1 : ', string(ParameterList.ParameterByName('Edit1').WinControlData), mtWarning, [mbOK], 0);
end;

procedure TJvParameterListDemoMainFrm.ShowTest3(const aDynControlEngine: tJvDynControlEngine);
var
  ParameterList: TJvParameterList;
  Parameter: TJvBaseParameter;
begin
  ParameterList := TJvParameterList.Create(self);
  try
    if Assigned(aDynControlEngine) then
      ParameterList.DynControlEngine := aDynControlEngine;
    {$IFDEF INCLUDE_DEVEXP_CX}
    SetDevExpressDynControlEngineProperties(ParameterList);
    {$ENDIF INCLUDE_DEVEXP_CX}
    Parameter := tjvButtonParameter.Create(ParameterList);
    with tjvButtonParameter(Parameter) do
    begin
      SearchName := 'ButtonA';
      Caption := 'Message Dlg &A';
      OnClick := ShowTest3ButttonClick;
      Width := 90;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvButtonParameter.Create(ParameterList);
    with tjvButtonParameter(Parameter) do
    begin
      SearchName := 'ButtonB';
      Caption := 'Message Dlg &B';
      OnClick := ShowTest3ButttonClick;
      Width := 90;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvButtonParameter.Create(ParameterList);
    with tjvButtonParameter(Parameter) do
    begin
      SearchName := 'ButtonC';
      Caption := 'Message Dlg &C';
      OnClick := ShowTest3ButttonClick;
      Width := 90;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvButtonParameter.Create(ParameterList);
    with tjvButtonParameter(Parameter) do
    begin
      SearchName := 'ButtonD';
      Caption := 'Message Dlg &D';
      OnClick := ShowTest3ButttonClick;
      Width := 90;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvEditParameter.Create(ParameterList);
    with Parameter do
    begin
      SearchName := 'Edit1';
      Caption := 'Edit Test 1';
      Width := 150;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvButtonParameter.Create(ParameterList);
    with tjvButtonParameter(Parameter) do
    begin
      SearchName := 'ButtonE';
      Caption := 'Message Dlg &E';
      OnClick := ShowTest3ButttonClick;
      Width := 90;
      DisableReasons.AddReasonIsEmpty('Edit1');
    end;
    ParameterList.AddParameter(Parameter);
    ParameterList.ArrangeSettings.AutoSize := asBoth;
 //  ParameterList.MaxHeight := StrToInt(MaxHeightEdit.Text);
 //  ParameterList.MaxWidth  := StrToInt(MaxWidthEdit.Text);
 //  ParameterList.Height    := StrToInt(HeightEdit.Text);
    ParameterList.MaxWidth := 300;
    ParameterList.HistoryEnabled := HistoryEnabledCheckBox.Checked;
    ParameterList.AppStorage := DefaultStorage;
    ParameterList.AppStoragePath := 'Dialog 3';
    if LoadFromCheckBox.Checked then
      ParameterList.LoadData;
    if ParameterList.ShowParameterDialog then
      if StoreToCheckBox.Checked then
        ParameterList.storeData;
//    Edit1.text := ParameterList.parameterByName('RadioGroupTest').AsString;
  finally
    ParameterList.Free;
  end;
end;

procedure TJvParameterListDemoMainFrm.ShowTestCrypt(const aDynControlEngine: tJvDynControlEngine);
var
  ParameterList: TJvParameterList;
  Parameter: TJvBaseParameter;
begin
  ParameterList := TJvParameterList.Create(self);
  try
    if Assigned(aDynControlEngine) then
      ParameterList.DynControlEngine := aDynControlEngine;
    {$IFDEF INCLUDE_DEVEXP_CX}
    SetDevExpressDynControlEngineProperties(ParameterList);
    {$ENDIF INCLUDE_DEVEXP_CX}
    Parameter := tjvEditParameter.Create(ParameterList);
    with tjvEditParameter(Parameter) do
    begin
      SearchName := 'EditTest';
      Caption := 'EditTest';
//      AsString   := Edit1.Text;

    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvEditParameter.Create(ParameterList);
    with tjvEditParameter(Parameter) do
    begin
      SearchName := 'PasswordTest';
      Caption := 'PasswordTest';
//      AsString   := Edit1.Text;
//      PasswordChar := '*';
      StoreValueCrypted := True;
    end;
    ParameterList.AddParameter(Parameter);
    ParameterList.HistoryEnabled := HistoryEnabledCheckBox.Checked;
    ParameterList.AppStorage := DefaultStorage;
    ParameterList.AppStoragePath := 'Dialog Crypt';
    ParameterList.LoadData;
    if ParameterList.ShowParameterDialog then
      ParameterList.storeData;
//    Edit1.text := ParameterList.parameterByName('RadioGroupTest').AsString;
  finally
    ParameterList.Free;
  end;
end;

procedure TJvParameterListDemoMainFrm.FormShow(Sender: TObject);
begin
  {$IFNDEF INCLUDE_DEVEXP_CX}
  Button4.Enabled := False;
  DevExpCxLookAndFeelRadioGroup.Enabled := False;
  DevExpCxStyleGroupBox.Enabled := False;
  ShadowCheckBox.Enabled := False;
  ThickLinesCheckBox.Enabled := False;
  NativeCheckBox.Enabled := False;
  CxRadioButton.Enabled := False;
  if CxRadioButton.Checked then
    VCLRadioButton.Checked := True;
  {$ENDIF INCLUDE_DEVEXP_CX}
  VCLRadioButtonClick(nil);
  DevExpCxLookAndFeelRadioGroupClick(nil);
  AssignWidthHeightCheckBoxClick(nil);
end;

procedure TJvParameterListDemoMainFrm.BitBtn1Click(Sender: TObject);
var
  FormStorageSelectList: tJvFormStorageSelectList;
begin
//  SetDynControlEngineDevExpCxDefault;
  FormStorageSelectList := tJvFormStorageSelectList.Create(Self);
  try
    FormStorageSelectList.FormStorage := JvFormStorage2;
    FormStorageSelectList.SelectPath := 'SelectTest';
//    MessageDlg(FormStorageSelectList.GetSelectPath(sloLoad), mtWarning, [mbOK], 0);
    FormStorageSelectList.RestoreFormStorage;
  finally
    FormStorageSelectList.Free;
  end;
end;

procedure TJvParameterListDemoMainFrm.BitBtn2Click(Sender: TObject);
var
  FormStorageSelectList: tJvFormStorageSelectList;
begin
  FormStorageSelectList := tJvFormStorageSelectList.Create(Self);
  try
    FormStorageSelectList.FormStorage := JvFormStorage2;
    FormStorageSelectList.SelectPath := 'SelectTest';
//    MessageDlg(FormStorageSelectList.GetSelectPath(sloStore), mtWarning, [mbOK], 0);
    FormStorageSelectList.SaveFormStorage;
  finally
    FormStorageSelectList.Free;
  end;
end;

procedure TJvParameterListDemoMainFrm.DevExpCxLookAndFeelRadioGroupClick(Sender: TObject);
begin
  {$IFDEF INCLUDE_DEVEXP_CX}
  with DynControlEngineDevExpCx do
  begin
    case DevExpCxLookAndFeelRadioGroup.ItemIndex of
      1: cxProperties.LookAndFeel.Kind := lfFlat;
      2: cxProperties.LookAndFeel.Kind := lfUltraFlat;
    else
      cxProperties.LookAndFeel.Kind := lfStandard;
    end;
    cxProperties.LookAndFeel.NativeStyle := NativeCheckBox.Checked;
    if Assigned(CxProperties.StyleController) then
    begin
      CxProperties.StyleController.Style.Shadow := ShadowCheckBox.Checked;
      if ThickLinesCheckBox.Checked then
        CxProperties.StyleController.Style.BorderStyle := ebsThick
      else
        CxProperties.StyleController.Style.BorderStyle := ebsSingle;
    end;
  end;
  {$ENDIF INCLUDE_DEVEXP_CX}
end;

procedure TJvParameterListDemoMainFrm.VCLRadioButtonClick(Sender: TObject);
begin
  {$IFDEF INCLUDE_DEVEXP_CX}
  if CxRadioButton.Checked then
    SetDefaultDynControlEngine(DynControlEngineDevExpCx)
  else
  {$ENDIF INCLUDE_DEVEXP_CX}
  if JVCLRadioButton.Checked then
    SetDefaultDynControlEngine(DynControlEngineJVCL)
  else
  if VCLRedRadioButton.Checked then
    SetDefaultDynControlEngine(DynControlEngineVCLRed)
  else
    SetDefaultDynControlEngine(DynControlEngineVCL);
end;

procedure TJvParameterListDemoMainFrm.Button5Click(Sender: TObject);
begin
  JVDSADialogs.MessageDlg('Simple warning box, standard title, VCL buttons and image.',
    mtWarning, [mbOK], 0);
end;

procedure TJvParameterListDemoMainFrm.Button6Click(Sender: TObject);
begin
  JVDSADialogs.MessageDlg('Simple confirmation box, standard title, VCL buttons and image.',
    mtConfirmation, [mbYes, mbNo], 0);
end;

procedure TJvParameterListDemoMainFrm.Button7Click(Sender: TObject);
var
  Pic: TPicture;
  BtnCap: TDynStringArray;
begin
  Pic := TPicture.Create;
  try
    Pic.Icon.Assign(Application.Icon);
    SetLength(BtnCap, 2);
    BtnCap[0] := 'Sure';
    BtnCap[1] := 'No way';
    MessageDlgEx(
      'Test warning',
      'Extended confirmation box, custom title, buttons and image.',
      Pic.Graphic,
      BtnCap,
      [10, 20],
      0);
  finally
    Pic.Free;
  end;
end;

procedure TJvParameterListDemoMainFrm.Button8Click(Sender: TObject);
begin
  ShowMessage('Test ShowMessage with custom checkmark text.');
end;

procedure TJvParameterListDemoMainFrm.Button11Click(Sender: TObject);
begin
  ShowTest1(DynControlEngineVCLRed);
end;

procedure TJvParameterListDemoMainFrm.Button12Click(Sender: TObject);
begin
  ShowTest2(nil);
end;

procedure TJvParameterListDemoMainFrm.JvPanelAllControlsResize(Sender: TObject);
begin
 //  JvGroupBoxAllControls.Width := JvPanelAllControls.Width+2;
 //  JvGroupBoxAllControls.Height := JvPanelAllControls.Height+20;
end;

procedure TJvParameterListDemoMainFrm.Button13Click(Sender: TObject);
begin
  ShowTest3(nil);
end;

function TJvParameterListDemoMainFrm.DefaultStorage: TJvCustomAppStorage;
begin
  Result := JvAppRegistryStorage;
end;

procedure TJvParameterListDemoMainFrm.JvAppRegistryStorageDecryptPropertyValue(
  var Value: string);
begin
//  Cipher.Encoded := Value;
//  Value := Cipher.Decoded;
//  Value := Copy(Value, 3, Length(Value)-4);
end;

procedure TJvParameterListDemoMainFrm.JvAppRegistryStorageEncryptPropertyValue(
  var Value: string);
begin
//  Cipher.Decoded := Value;
//  Value := Cipher.Encoded;
//  Value := '##'+Value+'##';
end;

procedure TJvParameterListDemoMainFrm.Button14Click(Sender: TObject);
begin
  ShowTestCrypt(nil);
end;

procedure TJvParameterListDemoMainFrm.Button15Click(Sender: TObject);
var
  ParameterList: TJvParameterList;
  Gparameter,
    Parameter: TJvBaseParameter;
begin
  ParameterList := TJvParameterList.Create(Self);
  {$IFDEF INCLUDE_DEVEXP_CX}
  SetDevExpressDynControlEngineProperties(ParameterList);
  {$ENDIF INCLUDE_DEVEXP_CX}
  try
    ParameterList.AppStoragePath := 'Analyze Table';
    ParameterList.AppStorage := DefaultStorage;
    ParameterList.MaxWidth := 400;
    ParameterList.ArrangeSettings.AutoSize := asBoth;
    Parameter := TJvRadioGroupParameter.Create(ParameterList);
    with TJvRadioGroupParameter(Parameter) do
    begin
      SearchName := 'AnalyzeType';
      Caption := '&Analyze Type';
      ItemList.Add('COMPUTE STATISTICS');
      ItemList.Add('ESTIMATE STATISTICS');
      ItemList.Add('DELETE STATISTICS');
      ItemIndex := 0;
      Width := 160;
      Height := 80;
      VariantAsItemIndex := True;
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Gparameter := TJvGroupBoxParameter.Create(ParameterList);
    with TJvGroupBoxParameter(Gparameter) do
    begin
      SearchName := 'AnalyzeAddType';
      Caption := 'Additional &Parameter';
      Width := 160;
      Height := 30;
      ArrangeSettings.AutoSize := asHeight;
      //BorderLeft := 5;
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Gparameter);
    Parameter := TJvCheckBoxParameter.Create(ParameterList);
    with TJvCheckBoxParameter(Parameter) do
    begin
      ParentParameterName := 'AnalyzeAddType';
      Width := 150;
      AsBoolean := True;
      SearchName := 'AnalyzeAddTypeTable';
      Caption := 'TABLE';
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvCheckBoxParameter.Create(ParameterList);
    with TJvCheckBoxParameter(Parameter) do
    begin
      ParentParameterName := 'AnalyzeAddType';
      Width := 150;
      AsBoolean := True;
      SearchName := 'AnalyzeAddTypeALLCOLUMNS';
      Caption := 'ALL COLUMNS';
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvCheckBoxParameter.Create(ParameterList);
    with TJvCheckBoxParameter(Parameter) do
    begin
      ParentParameterName := 'AnalyzeAddType';
      Width := 150;
      AsBoolean := True;
      SearchName := 'AnalyzeAddTypeALLINDEXEDCOLUMNS';
      Caption := 'ALL INDEXED COLUMNS';
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvCheckBoxParameter.Create(ParameterList);
    with TJvCheckBoxParameter(Parameter) do
    begin
      ParentParameterName := 'AnalyzeAddType';
      Width := 150;
      AsBoolean := True;
      SearchName := 'AnalyzeAddTypeAllIndexes';
      Caption := 'ALL INDEXES';
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvIntegerEditParameter.Create(ParameterList));
    with TJvIntegerEditParameter(Parameter) do
    begin
      EnableReasons.AddReason('AnalyzeType', 1);
      SearchName := 'EstimatePercent';
      Caption := '&Estimate Percent';
      if AsInteger > 100 then
        AsInteger := 100;
      MInValue := 0;
      MaxValue := 100;
      LabelArrangeMode := lamBefore;
      Width := 280;
      EditWidth := 40;
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvCheckBoxParameter.Create(ParameterList);
    with Parameter do
    begin
      SearchName := 'UseDBMSStats';
      Caption := '&Use DBMS_STATS';
      Width := 160;
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvCheckBoxParameter.Create(ParameterList);
    with TJvCheckBoxParameter(Parameter) do
    begin
      Width := 160;
      AsBoolean := False;
      SearchName := 'ExecuteAsJob';
      Caption := 'Execute as DBMS-Job';
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvGroupBoxParameter.Create(ParameterList));
    with TJvGroupBoxParameter(Parameter) do
    begin
      Caption := 'DBMS_JOB Parameter';
      SearchName := 'DBMS_JOB_Parameter';
      Width := 325;
      Height := 105;
      //BorderLeft := 5;
      DisableReasons.AddReason('ExecuteAsJob', False);
//      ArrangeSettings.AutoSize := asHeight;
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(tJvDateTimeParameter.Create(ParameterList));
    with tJvDateTimeParameter(Parameter) do
    begin
      ParentParameterName := 'DBMS_JOB_Parameter';
      LabelArrangeMode := lamBefore;
      Caption := 'First &Date';
      SearchName := 'Date';
      asDate := Now;
      StoreValueToAppStorage := False;
      Width := 220;
      LabelWidth := 50;
      DisableReasons.AddReason('ExecuteAsJob', False);
    end; {*** WITH tJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(tJvEditParameter.Create(ParameterList));
    with tJvEditParameter(Parameter) do
    begin
      ParentParameterName := 'DBMS_JOB_Parameter';
      Caption := '&Interval';
      SearchName := 'Interval';
      AsString := '';
      Width := 310;
      LabelWidth := 50;
      DisableReasons.AddReason('ExecuteAsJob', False);
    end; {*** WITH tJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    ParameterList.Messages.Caption := 'Select Type of Analyze';
    ParameterList.LoadData;
    ParameterList.ShowParameterDialog;
    ParameterList.StoreData;
  finally
    ParameterList.Free;
  end;
end;

procedure TJvParameterListDemoMainFrm.AssignWidthHeightCheckBoxClick(Sender: TObject);
begin
  WidthEdit.Enabled := AssignWidthHeightCheckBox.Checked;
  HeightEdit.Enabled := AssignWidthHeightCheckBox.Checked;
end;

procedure TJvParameterListDemoMainFrm.Button16Click(Sender: TObject);
var
  ParameterList: TJvParameterList;
  Parameter: TJvBaseParameter;
begin
  ParameterList := TJvParameterList.Create(self);
  {$IFDEF INCLUDE_DEVEXP_CX}
  SetDevExpressDynControlEngineProperties(ParameterList);
  {$ENDIF INCLUDE_DEVEXP_CX}
  try
    Parameter := TJvBaseParameter(tJvMemoParameter.Create(ParameterList));
    with tJvMemoParameter(Parameter) do
    begin
      SearchName := 'Memo';
      Caption := 'Simple Memo';
      AsString := 'Memo Contents';
      Width := 420;
      Height := 200;
      WordWrap := False;
      WantTabs := False;
      WantReturns := False;
      Scrollbars := ssBoth;
      ReadOnly := TRUE;
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    ParameterList.Messages.OkButton := 'C&opy';
    ParameterList.Messages.CancelButton := '&Cancel';
    if ParameterList.ShowParameterDialog then
      JVDSADialogs.MessageDlg(ParameterList.ParameterByName('Memo').ASString, mtInformation, [mbok], 0);
  finally
    ParameterList.Free;
  end;
end;

procedure TJvParameterListDemoMainFrm.Button17Click(Sender: TObject);
begin
  ShowUnitVersioning(nil);
end;

procedure TJvParameterListDemoMainFrm.Button18Click(Sender: TObject);
var
  ParameterList: TJvParameterList;
  Parameter: TJvBaseParameter;
begin
  ParameterList := TJvParameterList.Create(Self);
  {$IFDEF INCLUDE_DEVEXP_CX}
  SetDevExpressDynControlEngineProperties(ParameterList);
  {$ENDIF INCLUDE_DEVEXP_CX}
  try
    Parameter := TJvBaseParameter(TJvEditParameter.Create(ParameterList));
    with TJvEditParameter(Parameter) do
    begin
      Caption := '&Job';
      SearchName := 'Job';
      AsString := '999';
      ReadOnly := True;
      Width := 250;
    end; {*** WITH TJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvMemoParameter.Create(ParameterList));
    with TJvMemoParameter(Parameter) do
    begin
      Caption := '&What';
      SearchName := 'What';
      AsString := 'Do Anything';
      Required := True;
      Width := 450;
      Height := 250;
      Scrollbars := ssBoth;
      WantTabs := False;
      WantReturns := True;
      WordWrap := False;
      FontName := 'Courier New';
    end; {*** WITH TJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(tJvDateParameter.Create(ParameterList));
    with tJvDateParameter(Parameter) do
    begin
      Caption := '&Date';
      SearchName := 'Date';
      asDate := Trunc(now);
      Width := 120;
    end; {*** WITH TJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(tJvTimeParameter.Create(ParameterList));
    with tJvTimeParameter(Parameter) do
    begin
      Caption := '&Time';
      SearchName := 'Time';
      asDate := now;
      Width := 120;
    end; {*** WITH TJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvEditParameter.Create(ParameterList));
    with TJvEditParameter(Parameter) do
    begin
      Caption := '&Interval';
      SearchName := 'Interval';
      AsString := 'trunc(sysdate)+1';
      Width := 450;
    end; {*** WITH TJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      Caption := '&Broken Job';
      SearchName := 'Broken';
      AsBoolean := FALSE;
      Width := 350;
    end; {*** WITH TJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    ParameterList.Messages.Caption := 'Change Job';
    ParameterList.Messages.OkButton := '&Ok';
    ParameterList.Messages.CancelButton := '&Cancel';
    ParameterList.Width := 510;
    ParameterList.ShowParameterDialog;
  finally
    FreeAndNil(ParameterList);
  end;
end;

procedure TJvParameterListDemoMainFrm.Button19Click(Sender: TObject);
var
  ParameterList: TJvParameterList;
  Parameter: TJvBaseParameter;
begin
  ParameterList := TJvParameterList.Create(Self);
  {$IFDEF INCLUDE_DEVEXP_CX}
  SetDevExpressDynControlEngineProperties(ParameterList);
  {$ENDIF INCLUDE_DEVEXP_CX}
  try
    Parameter := TJvBaseParameter(TJvEditParameter.Create(ParameterList));
    with TJvEditParameter(Parameter) do
    begin
      LabelArrangeMode := lamGroupbox;
      Caption := '&Groupbox';
      SearchName := 'Groupbox';
      ReadOnly := True;
      Width := 250;
    end; {*** WITH TJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvEditParameter.Create(ParameterList));
    with TJvEditParameter(Parameter) do
    begin
      LabelArrangeMode := lamAbove;
      Caption := '&Above';
      SearchName := 'Above';
      ReadOnly := True;
      Width := 250;
    end; {*** WITH TJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvEditParameter.Create(ParameterList));
    with TJvEditParameter(Parameter) do
    begin
      LabelArrangeMode := lamBefore;
      Caption := '&Before';
      SearchName := 'Before';
      ReadOnly := True;
      Width := 250;
    end; {*** WITH TJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    ParameterList.Messages.OkButton := '&Ok';
    ParameterList.Messages.CancelButton := '&Cancel';
    ParameterList.MaxWidth := 310;
    ParameterList.ShowParameterDialog;
  finally
    FreeAndNil(ParameterList);
  end;
end;

procedure TJvParameterListDemoMainFrm.Button20Click(Sender: TObject);
var
  ParameterList: TJvParameterList;
  Parameter: TJvBaseParameter;
begin
  ParameterList := TJvParameterList.Create(Self);
  {$IFDEF INCLUDE_DEVEXP_CX}
  SetDevExpressDynControlEngineProperties(ParameterList);
  {$ENDIF INCLUDE_DEVEXP_CX}
  try
    Parameter := TJvBaseParameter(TJvEditParameter.Create(ParameterList));
    with TJvEditParameter(Parameter) do
    begin
      LabelArrangeMode := lamGroupbox;
      Caption := '&Groupbox';
      SearchName := 'Groupbox';
      Width := 250;
      BeforeParameterName := 'Before';
      AfterParameterName := 'After';
    end; {*** WITH TJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvEditParameter.Create(ParameterList));
    with TJvEditParameter(Parameter) do
    begin
      LabelArrangeMode := lamGroupbox;
      Caption := '&Groupbox';
      SearchName := 'Groupbox2';
      Width := 250;
      BeforeParameterName := 'Before2';
    end; {*** WITH TJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvEditParameter.Create(ParameterList));
    with TJvEditParameter(Parameter) do
    begin
      LabelArrangeMode := lamGroupbox;
      Caption := '&Groupbox';
      SearchName := 'Groupbox3';
      Width := 250;
      AfterParameterName := 'After2';
    end; {*** WITH TJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvEditParameter.Create(ParameterList));
    with TJvEditParameter(Parameter) do
    begin
      LabelArrangeMode := lamNone;
      Caption := '&After';
      SearchName := 'After';
      AsString := 'A';
      Width := 20;
    end; {*** WITH TJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvEditParameter.Create(ParameterList));
    with TJvEditParameter(Parameter) do
    begin
      LabelArrangeMode := lamNone;
      SearchName := 'Before';
      AsString := 'B';
      Width := 40;
    end; {*** WITH TJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvEditParameter.Create(ParameterList));
    with TJvEditParameter(Parameter) do
    begin
      LabelArrangeMode := lamNone;
      Caption := '&After';
      SearchName := 'After2';
      AsString := 'A2';
      Width := 20;
    end; {*** WITH TJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvEditParameter.Create(ParameterList));
    with TJvEditParameter(Parameter) do
    begin
      LabelArrangeMode := lamNone;
      SearchName := 'Before2';
      AsString := 'B2';
      Width := 22;
    end; {*** WITH TJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    ParameterList.Messages.OkButton := '&Ok';
    ParameterList.Messages.CancelButton := '&Cancel';
    ParameterList.MaxWidth := 310;
    ParameterList.ShowParameterDialog;
  finally
    FreeAndNil(ParameterList);
  end;
end;

procedure TJvParameterListDemoMainFrm.Button21Click(Sender: TObject);
var
  ParameterList: TJvParameterList;
  Parameter: TJvBaseParameter;
begin
  ParameterList := TJvParameterList.Create(Self);
  {$IFDEF INCLUDE_DEVEXP_CX}
  SetDevExpressDynControlEngineProperties(ParameterList);
  {$ENDIF INCLUDE_DEVEXP_CX}
  try
    Parameter := TJvBaseParameter(TJvPageControlParameter.Create(ParameterList));
    Parameter.SearchName := 'PageControl';
    TJvPageControlParameter(Parameter).Pages.Add('Parameter');
    TJvPageControlParameter(Parameter).Pages.Add('Script Header/Footer');
    TJvPageControlParameter(Parameter).ArrangeSettings.AutoSize := asHeight;
    Parameter.Width := 530;
    ParameterList.AddParameter(Parameter);

    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    Parameter.Caption := 'Include Script Header';
    Parameter.SearchName := 'IncludeScriptHeader';
    Parameter.ParentParameterName := 'PageControl.Script Header/Footer';
    Parameter.Width := 160;
    ParameterList.AddParameter(Parameter);

    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    Parameter.Caption := 'Include Script Footer';
    Parameter.SearchName := 'IncludeScriptFooter';
    Parameter.ParentParameterName := 'PageControl.Script Header/Footer';
    Parameter.Width := 160;
    ParameterList.AddParameter(Parameter);

    Parameter := TJvBaseParameter(TJvMemoParameter.Create(ParameterList));
    Parameter.Caption := 'Script Header';
    Parameter.SearchName := 'ScriptHeader';
    Parameter.ParentParameterName := 'PageControl.Script Header/Footer';
    Parameter.Width := 520;
    Parameter.Height := 80;
    Parameter.DisableReasons.AddReason('IncludeScriptHeader', False);
    ParameterList.AddParameter(Parameter);

    Parameter := TJvBaseParameter(TJvMemoParameter.Create(ParameterList));
    Parameter.Caption := 'Script Footer';
    Parameter.SearchName := 'ScriptFooter';
    Parameter.ParentParameterName := 'PageControl.Script Header/Footer';
    Parameter.Width := 520;
    Parameter.Height := 80;
    Parameter.DisableReasons.AddReason('IncludeScriptFooter', False);
    ParameterList.AddParameter(Parameter);

    Parameter := TJvBaseParameter(TJvGroupBoxParameter.Create(ParameterList));
    with TJvGroupBoxParameter(Parameter) do
    begin
      Caption := 'Destination';
      SearchName := 'Destination';
      Width := 510;
      Height := 40;
      Height := Height + 25;
      ParentParameterName := 'PageControl.Parameter';
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      SearchName := 'CopyToClipboard';
      Caption := 'Clipboard';
      ParentParameterName := 'Destination';
      Width := 80;
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      Caption := 'File';
      SearchName := 'SaveToFile';
      ParentParameterName := 'Destination';
      Width := 50;
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      Caption := 'Append';
      SearchName := 'AppendToFile';
      ParentParameterName := 'Destination';
      DisableReasons.AddReason('SaveToFile', False);
      Width := 60;
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      Caption := 'Add Spool';
      SearchName := 'AddSpool';
      ParentParameterName := 'Destination';
      Width := 75;
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      Caption := 'Add Echo Off';
      SearchName := 'AddEchoOff';
      ParentParameterName := 'Destination';
      Width := 90;
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      Caption := 'Add Define Off';
      SearchName := 'AddDefineOff';
      ParentParameterName := 'Destination';
      Width := 95;
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(tJvFileNameParameter.Create(ParameterList));
    with tJvFileNameParameter(Parameter) do
    begin
      Caption := 'Filename:';
      SearchName := 'Filename';
      ParentParameterName := 'Destination';
      AsString := FileName;
      DialogKind := jdkSave;
      DefaultExt := 'sql';
      Filter := 'SQL-Files (*.sql)|*.sql|All Files (*.*)|*.*';
      FilterIndex := 0;
      DisableReasons.AddReason('SaveToFile', False);
      DialogOptions := [ofNoChangeDir, ofNoReadOnlyReturn, ofNoTestFileCreate,
        ofPathMustExist, ofHideReadOnly, ofOverwritePrompt];
      Width := 495;
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with (Parameter) do
    begin
      Caption := 'Include DBMS_APPLICATION_INFO';
      SearchName := 'IncludeDBMSApplicationInfo';
      Width := 230;                 
      ParentParameterName := 'PageControl.Parameter';
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with (Parameter) do
    begin
      Caption := 'Include PROMPT';
      SearchName := 'IncludePrompt';
      Width := 220;           
      ParentParameterName := 'PageControl.Parameter';
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with (Parameter) do
    begin
      Caption := 'Oracle 7 Compatible Mode';
      SearchName := 'Oracle7Compatible';
      Width := 450;        
      ParentParameterName := 'PageControl.Parameter';
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);

    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      SearchName := 'NotNullFieldsFirst';
      Caption := 'Not Null Fields First';
      DisableReasons.AddReason('IncludeFieldList', False);
      Width := 160;       
      ParentParameterName := 'PageControl.Parameter';
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      SearchName := 'IncludeTableSpace';
      Caption := 'Include Tablespace';
      Width := 160;             
      ParentParameterName := 'PageControl.Parameter';
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      SearchName := 'IncludeStorage';
      Caption := 'Include Storage Clause';
      Width := 160;            
      ParentParameterName := 'PageControl.Parameter';
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      SearchName := 'IncludePartitions';
      Caption := 'Include Partitions';
      Width := 160;           
      ParentParameterName := 'PageControl.Parameter';
      DisableReasons.AddReason('Oracle7Compatible', True);
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      SearchName := 'IncludeParallel';
      Caption := 'Include Parallel Clause';
      Width := 160;               
      ParentParameterName := 'PageControl.Parameter';
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      SearchName := 'IncludeLogging';
      Caption := 'Include Logging Clause';
      Width := 160;          
      ParentParameterName := 'PageControl.Parameter';
      DisableReasons.AddReason('Oracle7Compatible', True);
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      SearchName := 'IncludeCompress';
      Caption := 'Include Compress';
      Width := 160;               
      ParentParameterName := 'PageControl.Parameter';
      DisableReasons.AddReason('Oracle7Compatible', True);
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      SearchName := 'IncludeMonitoring';
      Caption := 'Include Monitoring Clause';
      Width := 160;               
      ParentParameterName := 'PageControl.Parameter';
      DisableReasons.AddReason('Oracle7Compatible', True);
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      SearchName := 'IncludeLobSegments';
      Caption := 'Include Lob Segments';
      Width := 160;                
      ParentParameterName := 'PageControl.Parameter';
      DisableReasons.AddReason('Oracle7Compatible', True);
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      SearchName := 'IncludeIndexes';
      Caption := 'Include Indexes';
      Width := 160;              
      ParentParameterName := 'PageControl.Parameter';
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      SearchName := 'IncludeConstraints';
      Caption := 'Include Constraints';
      Width := 160;             
      ParentParameterName := 'PageControl.Parameter';
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      SearchName := 'IncludeConstraintIndexes';
      Caption := 'Include Constraint Indexes';
      DisableReasons.AddReason('IncludeConstraints', False);
      Width := 160;                
      ParentParameterName := 'PageControl.Parameter';
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      SearchName := 'IncludeRefConstraints';
      Caption := 'Include Ref. Constraints';
      DisableReasons.AddReason('IncludeConstraints', False);
      Width := 160;         
      ParentParameterName := 'PageControl.Parameter';
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      SearchName := 'IncludeTriggers';
      Caption := 'Include Triggers';
      Width := 160;          
      ParentParameterName := 'PageControl.Parameter';
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      SearchName := 'IncludeSequences';
      Caption := 'Include Sequences';
      Width := 160;             
      ParentParameterName := 'PageControl.Parameter';
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      SearchName := 'IncludeSnapshotLogs';
      Caption := 'Include Snapshot Logs';
      Width := 160;           
      ParentParameterName := 'PageControl.Parameter';
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      SearchName := 'IncludeTableComments';
      Caption := 'Include Table Comments';
      Width := 160;           
      ParentParameterName := 'PageControl.Parameter';
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    Parameter.SearchName := 'StorageOptimize';
    Parameter.Caption := 'Optimize Storage Parameter';
    Parameter.DisableReasons.AddReason('IncludeStorage', False);
    Parameter.ParentParameterName := 'PageControl.Parameter';
    Parameter.Width := 160;
    ParameterList.AddParameter(Parameter);

    Parameter := ParameterList.ParameterByName('PageControl');
    if Assigned(Parameter) then
      TJvPageControlParameter(Parameter).Pages.Insert(1,'Storage Optimization');
    Parameter := TJvBaseParameter(TJvIntegerEditParameter.Create(ParameterList));
    with TJvIntegerEditParameter(Parameter) do
    begin
      SearchName := 'StorageNoOfBaseExtents';
      Caption := 'No Of Extents: ';
      ParentParameterName := 'PageControl.Storage Optimization';
      DisableReasons.AddReason('IncludeStorage', False);
      DisableReasons.AddReason('StorageOptimize', False);
      LabelArrangeMode := lamBefore;
      MinValue := 1;
      Width := 160;
      LabelWidth := 95;
      EditWidth := 30;
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvDoubleEditParameter.Create(ParameterList));
    with TJvDoubleEditParameter(Parameter) do
    begin
      SearchName := 'StorageSizeFactor';
      Caption := 'Size Factor: ';
      ParentParameterName := 'PageControl.Storage Optimization';
      DisableReasons.AddReason('IncludeStorage', False);
      DisableReasons.AddReason('StorageOptimize', False);
      LabelArrangeMode := lamBefore;
      Width := 160;
      LabelWidth := 95;
      EditWidth := 30;
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvDoubleEditParameter.Create(ParameterList));
    with TJvIntegerEditParameter(Parameter) do
    begin
      SearchName := 'StorageNextExtentFactor';
      Caption := 'Next Extent Factor: ';
      ParentParameterName := 'PageControl.Storage Optimization';
      DisableReasons.AddReason('IncludeStorage', False);
      DisableReasons.AddReason('StorageOptimize', False);
      LabelArrangeMode := lamBefore;
      MinValue := 0;
      Width := 160;
      LabelWidth := 95;
      EditWidth := 30;
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvIntegerEditParameter.Create(ParameterList));
    with TJvIntegerEditParameter(Parameter) do
    begin
      SearchName := 'StorageBaseExtentFactor';
      Caption := 'Base Extent Factor (K): ';
      ParentParameterName := 'PageControl.Storage Optimization';
      DisableReasons.AddReason('IncludeStorage', False);
      DisableReasons.AddReason('StorageOptimize', False);
      LabelArrangeMode := lamBefore;
      MinValue := 1;
      Width := 160;
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvIntegerEditParameter.Create(ParameterList));
    with TJvIntegerEditParameter(Parameter) do
    begin
      SearchName := 'StorageMinExtentSize';
      Caption := 'Min Extent Size (K): ';
      DisableReasons.AddReason('StorageOptimize', False);
      DisableReasons.AddReason('IncludeStorage', False);
      ParentParameterName := 'PageControl.Storage Optimization';
      LabelArrangeMode := lamBefore;
      MinValue := 1;
      Width := 160;
      LabelWidth := 95;
      EditWidth := 60;
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvIntegerEditParameter.Create(ParameterList));
    with TJvIntegerEditParameter(Parameter) do
    begin
      SearchName := 'StorageMaxExtentSize';
      Caption := 'Max Extent Size (K): ';
      DisableReasons.AddReason('StorageOptimize', False);
      DisableReasons.AddReason('IncludeStorage', False);
      ParentParameterName := 'PageControl.Storage Optimization';
      LabelArrangeMode := lamBefore;
      MinValue := 0;
      Width := 160;
      LabelWidth := 95;
      EditWidth := 60;
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      Caption := 'Remove PCT Increase';
      SearchName := 'StorageRemovePCTIncrease';
      DisableReasons.AddReason('StorageOptimize', False);
      DisableReasons.AddReason('IncludeStorage', False);
      ParentParameterName := 'PageControl.Storage Optimization';
      Width := 160;
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    with Parameter do
    begin
      Caption := 'Size 2^X K Based';
      SearchName := 'StorageOptimize2xBased';
      DisableReasons.AddReason('IncludeStorage', False);
      DisableReasons.AddReason('StorageOptimize', False);
      ParentParameterName := 'PageControl.Storage Optimization';
      Width := 160;
    end; {*** WITH Parameter DO ***}
    ParameterList.AddParameter(Parameter);
    
    ParameterList.Messages.OkButton := '&Ok';
    ParameterList.Messages.CancelButton := '&Cancel';
    ParameterList.MaxWidth := 610;
    ParameterList.ShowParameterDialog;
  finally
    FreeAndNil(ParameterList);
  end;
end;

procedure TJvParameterListDemoMainFrm.Button9Click(Sender: TObject);
begin
  JVDSADialogs.MessageDlg('Simple confirmation box, standard title, VCL buttons and image. Multi Lines'#13#10+
  'Line2'#13#10+
  'Line3'#13#10+
  'Line4'#13#10+
  'Line5'#13#10+
  'Line6'#13#10+
  'Line7'#13#10+
  'Line8'#13#10+
  'Line9'#13#10,
    mtConfirmation, [mbYes, mbNo], 0);
end;

{$IFDEF INCLUDE_DEVEXP_CX}
procedure TJvParameterListDemoMainFrm.SetDevExpressDynControlEngineProperties(
    ParameterList: TJvParameterList);
begin
  if ParameterList.DynControlEngine is tJvDynControlEngineDevExpCx then
    with tJvDynControlEngineDevExpCx(ParameterList.DynControlEngine) do
    begin
      case DevExpCxLookAndFeelRadioGroup.ItemIndex of
        1: cxProperties.LookAndFeel.Kind := lfFlat;
        2: cxProperties.LookAndFeel.Kind := lfUltraFlat;
      else
        cxProperties.LookAndFeel.Kind := lfStandard;
      end;
      if Assigned(CxProperties.StyleController) then
      begin
        CxProperties.StyleController.Style.Shadow := ShadowCheckBox.Checked;
        if ThickLinesCheckBox.Checked then
          CxProperties.StyleController.Style.BorderStyle := ebsThick
        else
          CxProperties.StyleController.Style.BorderStyle := ebsNone;
      end;
    end;
end;
{$ENDIF INCLUDE_DEVEXP_CX}


initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);

end.
