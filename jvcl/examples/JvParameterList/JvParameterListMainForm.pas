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

unit JvParameterListMainForm;

interface

{$DEFINE INCLUDE_DEVEXP_CX}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, SysUtils, {Variants, }Classes, Graphics, Controls, Forms,
  Dialogs, JvDsaDialogs, JvParameterList, StdCtrls, JvParameterListParameter, Mask,
  JvToolEdit, JvPanel,
  {$IFDEF INCLUDE_DEVEXP_CX}
  cxButtons, cxListBox, cxRadioGroup, cxLookAndFeelPainters,
  cxDropDownEdit, cxCalendar, cxSpinEdit, cxTimeEdit, cxCheckBox, cxMemo,
  cxTextEdit, cxMaskEdit, cxControls, cxContainer, cxEdit, cxLabel,
  cxImage, cxCheckListBox,
  cxGroupBox, cxButtonEdit,
  {$ENDIF}
  ExtCtrls, JvFormPlacement, JvComponent, JvAppStorage,
  JvAppRegistryStorage, JvDynControlEngine, ComCtrls, Buttons, JvBitBtn,
  JvCombobox, CheckLst, ShlObj, ExtDlgs, JvImage,
  JvMaskEdit, JvSpin, JvBaseEdits, JvGroupBox, JvExStdCtrls,
  JvExExtCtrls, JvAppXMLStorage, JvCipher;

type
  TForm1 = class (TForm)
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
    procedure JvAppRegistryStorageDecryptPropertyValue(var Value: String);
    procedure JvAppRegistryStorageEncryptPropertyValue(var Value: String);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure AssignWidthHeightCheckBoxClick(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
  private
    { Private-Deklarationen }
    UnitVersionForm : TCustomForm;
    RCSFilePanel,
    RevisionPanel,
    DatePanel,
    ExtraPanel,
    PathPanel,
    LabelControl,
    RCSFileEdit,
    RevisionEdit,
    DateEdit,
    ExtraEdit,
    PathEdit : TWinControl;
    procedure ShowTest3ButttonClick(const ParameterList: TJvParameterList; const Parameter: TJvBaseParameter);
    function DefaultStorage : TJvCustomAppStorage;
    procedure TreeViewOnChange(Sender: TObject; Node: TTreeNode);
    procedure CloseButtonOnClick(Sender : TObject);
  public
    { Public-Deklarationen }
    procedure ShowTest1(const aDynControlEngine: tJvDynControlEngine);
    procedure ShowTest2(const aDynControlEngine: tJvDynControlEngine);
    procedure ShowTest3(const aDynControlEngine: tJvDynControlEngine);
    procedure ShowTestCrypt(const aDynControlEngine: tJvDynControlEngine);
    procedure ShowUnitVersioning(const aDynControlEngine: tJvDynControlEngine);
  end;

var
  Form1: TForm1;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\examples\JvParameterList'
  );
{$ENDIF UNITVERSIONING}


implementation

{$R *.dfm}

uses JvDynControlEngineVCL,
  JvDynControlEngineJVCL,
  JclBase, JclFileUtils,
  JvFormPlacementSelectList,
  JvDynControlEngineVCLRed
  {$IFDEF INCLUDE_DEVEXP_CX}
  , JvDynControlEngineDevExpCx
  , cxLookAndFeels
  {$ENDIF}
  , JvDynControlEngineIntf, JclUnitVersioning, JclStrings;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowTest1(nil);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ShowTest1(DynControlEngineVCL);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ShowTest1(DynControlEngineJVCL);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  {$IFDEF INCLUDE_DEVEXP_CX}
  ShowTest1(DynControlEngineDevExpCx);
  {$ENDIF}
end;

procedure TForm1.ShowTest1(const aDynControlEngine: tJvDynControlEngine);
var
  ParameterList: TJvParameterList;
  Parameter:     TJvBaseParameter;
begin
  ParameterList := TJvParameterList.Create(self);
  try
    if Assigned(aDynControlEngine) then
      ParameterList.DynControlEngine := aDynControlEngine;
    {$IFDEF INCLUDE_DEVEXP_CX}
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
    {$ENDIF}
    Parameter := tjvTimeParameter.Create(ParameterList);
    with tjvTimeParameter(Parameter) do
    begin
      SearchName := 'TimeTest';
      Caption    := 'TimeTest';
      Format     := 'HH:mm:ss';
//      Width      := 100;
      AsDate     := Now;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvDateParameter.Create(ParameterList);
    with tjvDateParameter(Parameter) do
    begin
      SearchName := 'DateTest';
      Caption    := 'DateTest';
//      Width      := 80;
      AsDate     := Now-10;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvDateTimeParameter.Create(ParameterList);
    with tjvDateTimeParameter(Parameter) do
    begin
      SearchName := 'DateTimeTest';
      Caption    := 'DateTimeTest';
      AsDate     := Now;
      Width      := 365;
      RightSpace := 100;
//      Width      := 200;
//      Height     := 50;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvEditParameter.Create(ParameterList);
    with tjvEditParameter(Parameter) do
    begin
      SearchName := 'EditTest';
      Caption    := 'EditTest';
//      AsString   := Edit1.Text;
      EditMask   := '09999';
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvEditParameter.Create(ParameterList);
    with tjvEditParameter(Parameter) do
    begin
      SearchName := 'PasswordTest';
      Caption    := 'PasswordTest';
//      AsString   := Edit1.Text;
      PasswordChar := '*';
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvCheckboxParameter.Create(ParameterList);
    with Parameter do
    begin
      SearchName := 'CheckboxTest';
      Caption    := '&Checkbox';
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvIntegerEditParameter.Create(ParameterList);
    with Parameter do
    begin
      SearchName := 'IntegerTest';
      Caption    := 'IntegerTest';
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvIntegerEditParameter.Create(ParameterList);
    with tjvIntegerEditParameter(Parameter) do
    begin
      SearchName := 'IntegerTestCalc';
      Caption    := 'IntegerTest Calc';
      EditorType := netCalculate;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvIntegerEditParameter.Create(ParameterList);
    with tjvIntegerEditParameter(Parameter) do
    begin
      SearchName := 'IntegerTestSpin';
      Caption    := 'IntegerTest Spin';
      EditorType := netSpin;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvButtonEditParameter.Create(ParameterList);
    with tjvButtonEditParameter(Parameter) do
    begin
      SearchName := 'ButtonEditTest';
      Caption    := 'ButtonEditTest';
      OnClick := Button5Click;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvFileNameParameter.Create(ParameterList);
    with TJvFileNameParameter(Parameter) do
    begin
      SearchName := 'FileNameTest';
      Caption    := 'FileNameTest';
      labelArrangeMode := lamAbove;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvDirectoryParameter.Create(ParameterList);
    with TJvDirectoryParameter(Parameter) do
    begin
      SearchName := 'DirectoryTest';
      Caption    := 'DirectoryTest';
      labelArrangeMode := lamAbove;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvMemoParameter.Create(ParameterList);
    with tjvMemoParameter(Parameter) do
    begin
      SearchName := 'MemoTest';
      Caption    := 'MemoTest';
      //Height     := 60;
      Scrollbars := ssBoth;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvRichEditParameter.Create(ParameterList);
    with tjvRichEditParameter(Parameter) do
    begin
      SearchName := 'RichEditTest';
      Caption    := 'RichEditTest';
      //Height     := 60;
      Scrollbars := ssBoth;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvRadioGroupParameter.Create(ParameterList);
    with tjvRadioGroupParameter(Parameter) do
    begin
      SearchName := 'RadioGroupTest';
      Caption    := '&RadioGroupTest';
      ItemList.Add('Test&1');
      ItemList.Add('Test&2');
      ItemList.Add('Test&3');
      ItemList.Add('Test&4');
      ItemList.Add('Test&5');
      ReadOnly  := true;
      ItemIndex := 2;
      Columns   := 2;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvListBoxParameter.Create(ParameterList);
    with tjvListBoxParameter(Parameter) do
    begin
      SearchName := 'ListBoxTest';
      Caption    := '&ListBoxTest';
      ItemList.Add('Listbox Test&1');
      ItemList.Add('Listbox Test&2');
      ItemList.Add('Listbox Test&3');
      Height := 80;
      Width  := 80;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvCheckListBoxParameter.Create(ParameterList);
    with tjvCheckListBoxParameter(Parameter) do
    begin
      SearchName := 'CheckListBoxTest';
      Caption    := '&CheckListBoxTest';
      ItemList.Add('CheckListBox Test&1');
      ItemList.Add('CheckListBox Test&2');
      ItemList.Add('CheckListBox Test&3');
      AddCheckListBoxItem('CheckListBox Header&1', cbUnchecked, False, True);
      AddCheckListBoxItem('CheckListBox Test&4', cbUnchecked);
      Height := 80;
      Width  := 180;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvImageParameter.Create(ParameterList);
    with tjvImageParameter(Parameter) do
    begin
      Picture    := Image1.Picture;
      SearchName := 'PictureTest';
      Caption    := 'PictureTest';
//      AutoSize := True;
      Height     := 280;
      Width      := 340;
    end;
    ParameterList.AddParameter(Parameter);
    if AutoHeightCheckBox.Checked then
      if AutoWidthCheckBox.Checked then
        ParameterList.ArrangeSettings.AutoSize := asBoth
      else
        ParameterList.ArrangeSettings.AutoSize := asHeight
    else if AutoWidthCheckBox.Checked then
      ParameterList.ArrangeSettings.AutoSize := asWidth
    else
      ParameterList.ArrangeSettings.AutoSize := asNone;

    ParameterList.DefaultParameterWidth  := 180;
    ParameterList.DefaultParameterLabelWidth := 80;
    ParameterList.MaxHeight := StrToInt(MaxHeightEdit.Text);
    ParameterList.MaxWidth  := StrToInt(MaxWidthEdit.Text);
    if AssignWidthHeightCheckBox.Checked then
    begin
      ParameterList.Height    := StrToInt(HeightEdit.Text);
      ParameterList.Width     := StrToInt(WidthEdit.Text);
    end;
    ParameterList.HistoryEnabled := HistoryEnabledCheckBox.Checked;
    ParameterList.DefaultParameterWidth  := StrToInt(DefaultParameterWidthEdit.Text);
    ParameterList.DefaultParameterLabelWidth  := StrToInt(DefaultParameterLabelWidthEdit.Text);
    ParameterList.AppStorage  := DefaultStorage;
    ParameterList.AppStoragePath      := 'Dialog 1';
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

procedure TForm1.ShowTest2(const aDynControlEngine: tJvDynControlEngine);
var
  ParameterList: TJvParameterList;
  Parameter:     TJvBaseParameter;
begin
  ParameterList := TJvParameterList.Create(self);
  try
    if Assigned(aDynControlEngine) then
      ParameterList.DynControlEngine := aDynControlEngine;
    {$IFDEF INCLUDE_DEVEXP_CX}
    if ParameterList.DynControlEngine is tJvDynControlEngineDevExpCx then
      with tJvDynControlEngineDevExpCx(ParameterList.DynControlEngine) do
      begin
        case DevExpCxLookAndFeelRadioGroup.ItemIndex of
          1: cxProperties.LookAndFeel.Kind := lfFlat;
          2: cxProperties.LookAndFeel.Kind := lfUltraFlat;
          else
            cxProperties.LookAndFeel.Kind := lfStandard;
        end;
        CxProperties.StyleController.Style.Shadow := ShadowCheckBox.Checked;
        if ThickLinesCheckBox.Checked then
          CxProperties.StyleController.Style.BorderStyle := ebsThick
        else
          CxProperties.StyleController.Style.BorderStyle := ebsNone;
      end;
    {$ENDIF}
    Parameter := tjvRadioGroupParameter.Create(ParameterList);
    with tjvRadioGroupParameter(Parameter) do
    begin
      SearchName := 'RadioGroup';
      Caption    := '&Enabled';
      ItemList.Add('1 Enabled');
      ItemList.Add('2 Enabled');
      ItemList.Add('Both Enabled');
      ItemList.Add('Both Disabled');
      ItemIndex := 2;
      Columns   := 2;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvCheckboxParameter.Create(ParameterList);
    with Parameter do
    begin
      SearchName := 'Checkbox';
      Caption    := '2 Enabled';
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvEditParameter.Create(ParameterList);
    with Parameter do
    begin
      SearchName := 'Edit1';
      Caption    := 'Edit Test 1';
//      Width      := 80;
      AsDate     := Now;
      DisableReasons.AddReason('RadioGroup', '2 Enabled');
      DisableReasons.AddReason('RadioGroup', 'Both Disabled');
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvEditParameter.Create(ParameterList);
    with Parameter do
    begin
      SearchName := 'Edit2';
      Caption    := 'Edit Test 2';
//      Width      := 80;
      AsDate     := Now;
      DisableReasons.AddReason('RadioGroup', '1 Enabled');
      DisableReasons.AddReason('RadioGroup', 'Both Disabled');
      DisableReasons.AddReason('Checkbox', false);
    end;
    ParameterList.AddParameter(Parameter);
    if AutoHeightCheckBox.Checked then
      if AutoWidthCheckBox.Checked then
        ParameterList.ArrangeSettings.AutoSize := asBoth
      else
        ParameterList.ArrangeSettings.AutoSize := asHeight
    else if AutoWidthCheckBox.Checked then
      ParameterList.ArrangeSettings.AutoSize := asWidth
    else
      ParameterList.ArrangeSettings.AutoSize := asNone;
    ParameterList.OkButtonDisableReasons.AddReason('CheckBox', true);
    ParameterList.MaxHeight := StrToInt(MaxHeightEdit.Text);
    ParameterList.MaxWidth  := StrToInt(MaxWidthEdit.Text);
    if AssignWidthHeightCheckBox.Checked then
    begin
      ParameterList.Height    := StrToInt(HeightEdit.Text);
      ParameterList.Width     := StrToInt(WidthEdit.Text);
    end;
    ParameterList.DefaultParameterWidth  := StrToInt(DefaultParameterWidthEdit.Text);
    ParameterList.DefaultParameterLabelWidth  := StrToInt(DefaultParameterLabelWidthEdit.Text);
    ParameterList.HistoryEnabled := HistoryEnabledCheckBox.Checked;
    ParameterList.AppStorage  := DefaultStorage;
    ParameterList.AppStoragePath      := 'Dialog 2';
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

procedure TForm1.ShowTest3ButttonClick(const ParameterList: TJvParameterList; const Parameter: TJvBaseParameter);
begin
  if Assigned(Parameter) then
    if Parameter.SearchName = 'ButtonA' then
      Button5Click(nil)
    else if Parameter.SearchName = 'ButtonB' then
      Button6Click(nil)
    else if Parameter.SearchName = 'ButtonC' then
      Button7Click(nil)
    else if Parameter.SearchName = 'ButtonD' then
      Button8Click(nil)
    else if Parameter.SearchName = 'ButtonE' then
      if Assigned(ParameterList) then
        // (p3) type cast variant to string (for D5)
        MessageDlg('Edit 1 : ' , string(ParameterList.ParameterByName('Edit1').WinControlData), mtWarning, [mbOK], 0);
  ;
end;

procedure TForm1.ShowTest3(const aDynControlEngine: tJvDynControlEngine);
var
  ParameterList: TJvParameterList;
  Parameter:     TJvBaseParameter;
begin
  ParameterList := TJvParameterList.Create(self);
  try
    if Assigned(aDynControlEngine) then
      ParameterList.DynControlEngine := aDynControlEngine;
    {$IFDEF INCLUDE_DEVEXP_CX}
    if ParameterList.DynControlEngine is tJvDynControlEngineDevExpCx then
      with tJvDynControlEngineDevExpCx(ParameterList.DynControlEngine) do
      begin
        case DevExpCxLookAndFeelRadioGroup.ItemIndex of
          1: cxProperties.LookAndFeel.Kind := lfFlat;
          2: cxProperties.LookAndFeel.Kind := lfUltraFlat;
          else
            cxProperties.LookAndFeel.Kind := lfStandard;
        end;
        CxProperties.StyleController.Style.Shadow := ShadowCheckBox.Checked;
        if ThickLinesCheckBox.Checked then
          CxProperties.StyleController.Style.BorderStyle := ebsThick
        else
          CxProperties.StyleController.Style.BorderStyle := ebsNone;
      end;
    {$ENDIF}
    Parameter := tjvButtonParameter.Create(ParameterList);
    with tjvButtonParameter(Parameter) do
    begin
      SearchName := 'ButtonA';
      Caption    := 'Message Dlg &A';
      OnClick := ShowTest3ButttonClick;
      Width      := 90;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvButtonParameter.Create(ParameterList);
    with tjvButtonParameter(Parameter) do
    begin
      SearchName := 'ButtonB';
      Caption    := 'Message Dlg &B';
      OnClick := ShowTest3ButttonClick;
      Width      := 90;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvButtonParameter.Create(ParameterList);
    with tjvButtonParameter(Parameter) do
    begin
      SearchName := 'ButtonC';
      Caption    := 'Message Dlg &C';
      OnClick := ShowTest3ButttonClick;
      Width      := 90;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvButtonParameter.Create(ParameterList);
    with tjvButtonParameter(Parameter) do
    begin
      SearchName := 'ButtonD';
      Caption    := 'Message Dlg &D';
      OnClick := ShowTest3ButttonClick;
      Width      := 90;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvEditParameter.Create(ParameterList);
    with Parameter do
    begin
      SearchName := 'Edit1';
      Caption    := 'Edit Test 1';
      Width      := 150;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvButtonParameter.Create(ParameterList);
    with tjvButtonParameter(Parameter) do
    begin
      SearchName := 'ButtonE';
      Caption    := 'Message Dlg &E';
      OnClick := ShowTest3ButttonClick;
      Width      := 90;
      DisableReasons.AddReasonIsEmpty('Edit1');
    end;
    ParameterList.AddParameter(Parameter);
    ParameterList.ArrangeSettings.AutoSize := asBoth;
 //  ParameterList.MaxHeight := StrToInt(MaxHeightEdit.Text);
 //  ParameterList.MaxWidth  := StrToInt(MaxWidthEdit.Text);
 //  ParameterList.Height    := StrToInt(HeightEdit.Text);
    ParameterList.MaxWidth    := 300;
    ParameterList.HistoryEnabled := HistoryEnabledCheckBox.Checked;
    ParameterList.AppStorage := DefaultStorage;
    ParameterList.AppStoragePath     := 'Dialog 3';
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


procedure TForm1.ShowTestCrypt(const aDynControlEngine: tJvDynControlEngine);
var
  ParameterList: TJvParameterList;
  Parameter:     TJvBaseParameter;
begin
  ParameterList := TJvParameterList.Create(self);
  try
    if Assigned(aDynControlEngine) then
      ParameterList.DynControlEngine := aDynControlEngine;
    {$IFDEF INCLUDE_DEVEXP_CX}
    if ParameterList.DynControlEngine is tJvDynControlEngineDevExpCx then
      with tJvDynControlEngineDevExpCx(ParameterList.DynControlEngine) do
      begin
        case DevExpCxLookAndFeelRadioGroup.ItemIndex of
          1: cxProperties.LookAndFeel.Kind := lfFlat;
          2: cxProperties.LookAndFeel.Kind := lfUltraFlat;
          else
            cxProperties.LookAndFeel.Kind := lfStandard;
        end;
        CxProperties.StyleController.Style.Shadow := ShadowCheckBox.Checked;
        if ThickLinesCheckBox.Checked then
          CxProperties.StyleController.Style.BorderStyle := ebsThick
        else
          CxProperties.StyleController.Style.BorderStyle := ebsNone;
      end;
    {$ENDIF}
    Parameter := tjvEditParameter.Create(ParameterList);
    with tjvEditParameter(Parameter) do
    begin
      SearchName := 'EditTest';
      Caption    := 'EditTest';
//      AsString   := Edit1.Text;

    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvEditParameter.Create(ParameterList);
    with tjvEditParameter(Parameter) do
    begin
      SearchName := 'PasswordTest';
      Caption    := 'PasswordTest';
//      AsString   := Edit1.Text;
//      PasswordChar := '*';
      StoreValueCrypted := True;
    end;
    ParameterList.AddParameter(Parameter);
    ParameterList.HistoryEnabled := HistoryEnabledCheckBox.Checked;
    ParameterList.AppStorage  := DefaultStorage;
    ParameterList.AppStoragePath      := 'Dialog Crypt';
    ParameterList.LoadData;
    if ParameterList.ShowParameterDialog then
        ParameterList.storeData;
//    Edit1.text := ParameterList.parameterByName('RadioGroupTest').AsString;
  finally
    ParameterList.Free;
  end;
end;



procedure TForm1.FormShow(Sender: TObject);
begin
  {$IFNDEF INCLUDE_DEVEXP_CX}
  Button4.Enabled := false;
  DevExpCxLookAndFeelRadioGroup.Enabled := false;
  DevExpCxStyleGroupBox.Enabled := false;
  ShadowCheckBox.Enabled := false;
  ThickLinesCheckBox.Enabled := false;
  CxRadioButton.Enabled := false;
  if CxRadioButton.Checked then
    VCLRadioButton.Checked := true;
  {$ENDIF}
  VCLRadioButtonClick(nil);
  DevExpCxLookAndFeelRadioGroupClick(nil);
  AssignWidthHeightCheckBoxClick(nil);
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
var
  FormStorageSelectList: tJvFormStorageSelectList;
begin
//  SetDynControlEngineDevExpCxDefault;
  FormStorageSelectList := tJvFormStorageSelectList.Create(Self);
  try
    FormStorageSelectList.FormStorage := JvFormStorage2;
    FormStorageSelectList.SelectPath  := 'SelectTest';
//    MessageDlg(FormStorageSelectList.GetSelectPath(sloLoad), mtWarning, [mbOK], 0);
    FormStorageSelectList.RestoreFormStorage;
  finally
    FormStorageSelectList.Free;
  end;
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
var
  FormStorageSelectList: tJvFormStorageSelectList;
begin
  FormStorageSelectList := tJvFormStorageSelectList.Create(Self);
  try
    FormStorageSelectList.FormStorage := JvFormStorage2;
    FormStorageSelectList.SelectPath  := 'SelectTest';
//    MessageDlg(FormStorageSelectList.GetSelectPath(sloStore), mtWarning, [mbOK], 0);
    FormStorageSelectList.SaveFormStorage;
  finally
    FormStorageSelectList.Free;
  end;
end;


procedure TForm1.DevExpCxLookAndFeelRadioGroupClick(Sender: TObject);
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
    if Assigned(CxProperties.StyleController) then
    begin
      CxProperties.StyleController.Style.Shadow := ShadowCheckBox.Checked;
      if ThickLinesCheckBox.Checked then
        CxProperties.StyleController.Style.BorderStyle := ebsThick
      else
        CxProperties.StyleController.Style.BorderStyle := ebsSingle;
    end;
  end;
  {$ENDIF}
end;

procedure TForm1.VCLRadioButtonClick(Sender: TObject);
begin
  {$IFDEF INCLUDE_DEVEXP_CX}
  if CxRadioButton.Checked then
    SetDefaultDynControlEngine(DynControlEngineDevExpCx)
  else  {$ENDIF}
  if JVCLRadioButton.Checked then
    SetDefaultDynControlEngine(DynControlEngineJVCL)
  else if VCLRedRadioButton.Checked then
    SetDefaultDynControlEngine(DynControlEngineVCLRed)
  else
    SetDefaultDynControlEngine(DynControlEngineVCL);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  MessageDlg(
    'Simple warning box, standard title, VCL buttons and image.',
    mtWarning,
    [mbOK],
    0);
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  MessageDlg(
    'Simple confirmation box, standard title, VCL buttons and image.',
    mtConfirmation,
    [mbYes, mbNo],
    0);
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  Pic:    TPicture;
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

procedure TForm1.Button8Click(Sender: TObject);
begin
  ShowMessage('Test ShowMessage with custom checkmark text.');
end;

procedure TForm1.Button11Click(Sender: TObject);
begin
  ShowTest1(DynControlEngineVCLRed);
end;

procedure TForm1.Button12Click(Sender: TObject);
begin
  ShowTest2(nil);
end;

procedure TForm1.JvPanelAllControlsResize(Sender: TObject);
begin
 //  JvGroupBoxAllControls.Width := JvPanelAllControls.Width+2;
 //  JvGroupBoxAllControls.Height := JvPanelAllControls.Height+20;
end;

procedure TForm1.Button13Click(Sender: TObject);
begin
  ShowTest3(nil);
end;

function TForm1.DefaultStorage : TJvCustomAppStorage;
begin
  Result := JvAppRegistryStorage;
end;

procedure TForm1.JvAppRegistryStorageDecryptPropertyValue(
  var Value: String);
begin
//  Cipher.Encoded := Value;
//  Value := Cipher.Decoded;
//  Value := Copy(Value, 3, Length(Value)-4);
end;

procedure TForm1.JvAppRegistryStorageEncryptPropertyValue(
  var Value: String);
begin
//  Cipher.Decoded := Value;
//  Value := Cipher.Encoded;
//  Value := '##'+Value+'##';
end;

procedure TForm1.Button14Click(Sender: TObject);
begin
  ShowTestCrypt(nil);
end;

procedure TForm1.Button15Click(Sender: TObject);
VAR
  ParameterList     : TJvParameterList;
  Gparameter,
    Parameter       : TJvBaseParameter;
BEGIN
  ParameterList := TJvParameterList.Create(Self);
  TRY
    ParameterList.AppStoragePath := 'Analyze Table';
    ParameterList.AppStorage := DefaultStorage;
    ParameterList.MaxWidth := 400;
    ParameterList.ArrangeSettings.AutoSize := asBoth;
    Parameter := TJvRadioGroupParameter.Create(ParameterList);
    WITH TJvRadioGroupParameter(Parameter) DO
    BEGIN
      SearchName := 'AnalyzeType';
      Caption := '&Analyze Type';
      ItemList.Add('COMPUTE STATISTICS');
      ItemList.Add('ESTIMATE STATISTICS');
      ItemList.Add('DELETE STATISTICS');
      ItemIndex := 0;
      Width := 160;
      Height := 80;
      VariantAsItemIndex := True;
    END;                                {*** WITH BaseParameter DO ***}
    ParameterList.AddParameter(Parameter);
    Gparameter := TJvGroupBoxParameter.Create(ParameterList);
    WITH TJvGroupBoxParameter(Gparameter) DO
    BEGIN
      SearchName := 'AnalyzeAddType';
      Caption := 'Additional &Parameter';
      Width := 160;
      Height := 30;
      ArrangeSettings.AutoSize := asHeight;
      //BorderLeft := 5;
    END;                              {*** WITH BaseParameter DO ***}
    ParameterList.AddParameter(Gparameter);
    Parameter := TJvCheckBoxParameter.Create(ParameterList);
    WITH TJvCheckBoxParameter(Parameter) DO
    BEGIN
      ParentParameterName := 'AnalyzeAddType';
      Width := 150;
      AsBoolean := True;
      SearchName := 'AnalyzeAddTypeTable';
      Caption := 'TABLE';
    END;                          {*** WITH BaseParameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvCheckBoxParameter.Create(ParameterList);
    WITH TJvCheckBoxParameter(Parameter) DO
    BEGIN
      ParentParameterName := 'AnalyzeAddType';
      Width := 150;
      AsBoolean := True;
      SearchName := 'AnalyzeAddTypeALLCOLUMNS';
      Caption := 'ALL COLUMNS';
    END;                          {*** WITH BaseParameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvCheckBoxParameter.Create(ParameterList);
    WITH TJvCheckBoxParameter(Parameter) DO
    BEGIN
      ParentParameterName := 'AnalyzeAddType';
      Width := 150;
      AsBoolean := True;
      SearchName := 'AnalyzeAddTypeALLINDEXEDCOLUMNS';
      Caption := 'ALL INDEXED COLUMNS';
    END;                          {*** WITH BaseParameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvCheckBoxParameter.Create(ParameterList);
    WITH TJvCheckBoxParameter(Parameter) DO
    BEGIN
      ParentParameterName := 'AnalyzeAddType';
      Width := 150;
      AsBoolean := True;
      SearchName := 'AnalyzeAddTypeAllIndexes';
      Caption := 'ALL INDEXES';
    END;                          {*** WITH BaseParameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvIntegerEditParameter.Create(ParameterList));
    WITH TJvIntegerEditParameter(Parameter) DO
    BEGIN
      EnableReasons.AddReason('AnalyzeType', 1);
      SearchName := 'EstimatePercent';
      Caption := '&Estimate Percent';
      IF AsInteger > 100 THEN
        AsInteger := 100;
      MInValue := 0;
      MaxValue := 100;
      LabelArrangeMode := lamBefore;
      Width := 280;
      EditWidth := 40;
    END;                                  {*** WITH BaseParameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvCheckBoxParameter.Create(ParameterList);
    WITH Parameter DO
    BEGIN
      SearchName := 'UseDBMSStats';
      Caption := '&Use DBMS_STATS';
      Width := 160;
    END;                                  {*** WITH BaseParameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvCheckBoxParameter.Create(ParameterList);
    WITH TJvCheckBoxParameter(Parameter) DO
    BEGIN
      Width := 160;
      AsBoolean := False;
      SearchName := 'ExecuteAsJob';
      Caption := 'Execute as DBMS-Job';
    END;                                {*** WITH BaseParameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvGroupBoxParameter.Create(ParameterList));
    WITH TJvGroupBoxParameter(Parameter) DO
    BEGIN
      Caption := 'DBMS_JOB Parameter';
      SearchName := 'DBMS_JOB_Parameter';
      Width := 325;
      Height := 105;
      //BorderLeft := 5;
      DisableReasons.AddReason('ExecuteAsJob', False);
//      ArrangeSettings.AutoSize := asHeight;
    END;                                  {*** WITH BaseParameter DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(tJvDateTimeParameter.Create(ParameterList));
    WITH tJvDateTimeParameter(Parameter) DO
    BEGIN
      ParentParameterName := 'DBMS_JOB_Parameter';
      LabelArrangeMode := lamBefore;
      Caption := 'First &Date';
      SearchName := 'Date';
      asDate := Now;
      StoreValueToAppStorage := False;
      Width := 220;
      LabelWidth := 50;
      DisableReasons.AddReason('ExecuteAsJob', False);
    END; {*** WITH tJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(tJvEditParameter.Create(ParameterList));
    WITH tJvEditParameter(Parameter) DO
    BEGIN
      ParentParameterName := 'DBMS_JOB_Parameter';
      Caption := '&Interval';
      SearchName := 'Interval';
      AsString := '';
      Width := 310;
      LabelWidth := 50;
      DisableReasons.AddReason('ExecuteAsJob', False);
    END; {*** WITH tJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    ParameterList.Messages.Caption := 'Select Type of Analyze';
    ParameterList.LoadData;
    ParameterList.ShowParameterDialog;
    ParameterList.StoreData;
  FINALLY
    ParameterList.Free;
  END;
end;

procedure TForm1.AssignWidthHeightCheckBoxClick(Sender: TObject);
begin
  WidthEdit.Enabled := AssignWidthHeightCheckBox.Checked;
  HeightEdit.Enabled := AssignWidthHeightCheckBox.Checked;
end;

procedure TForm1.Button16Click(Sender: TObject);
VAR
  ParameterList     : TJvParameterList;
  BaseParameter     : TJvBaseParameter;
begin
  ParameterList := TJvParameterList.Create(self);
  try
    BaseParameter:= TJvBaseParameter(tJvMemoParameter.Create(ParameterList));
    WITH tJvMemoParameter(BaseParameter) DO
    BEGIN
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
    END;   {*** WITH BaseParameter DO ***}
    ParameterList.AddParameter(BaseParameter);
    ParameterList.Messages.OkButton := 'C&opy';
    ParameterList.Messages.CancelButton := '&Cancel';
    IF ParameterList.ShowParameterDialog THEN
      MessageDlg (ParameterList.ParameterByName('Memo').ASString, mtInformation, [mbok],0);
  finally
    ParameterList.Free;
  end;
end;

procedure TForm1.ShowUnitVersioning(const aDynControlEngine: tJvDynControlEngine);
var
  DynEngine : TJvDynControlEngine;
  MainPanel,
  ButtonPanel : TWinControl;
  TopBox,
  BottomBox : TWinControl;
  Button : TButton;
  TreeView : TWinControl;
  IJvReadOnly: IJvDynControlReadOnly;
  IJvTreeView: IJvDynControlTreeView;
  i: Integer;
  MainNode,
  Node: TTreeNode;
  Nodes: TTreeNodes;

  function FindMasterNode (iNodes : TTreeNodes; iNode : TTreeNode; const iPath : String) : tTreeNode;
  var
    Part1, Part2 : String;
    I : Integer;
  begin
    Result := nil;
    Part2 := iPath;
    Part1 := '';
    while (Part1 = '') and (Part2 <> '') do
    begin
      Part1 := trim(StrBefore('\', Part2));
      Part2 := trim(StrAfter('\', Part2));
    end;
    if Part1 <> '' then
    begin
      for i := 0 to iNode.Count-1 do
      begin
        if iNode.Item[i].Text = Part1 then
        begin
          Result := FindMasterNode (iNodes, iNode.Item[i], Part2);
          break;
        end;
      end;
      if Result = nil then
      begin
        Result := iNodes.AddChild(iNode, Part1);
        Result := FindMasterNode (iNodes, Result, Part2);
      end;
    end
    else
      Result := iNode;
  end;

begin
  if Assigned(aDynControlEngine) then
    DynEngine := aDynControlEngine
  else
    DynEngine := DefaultDynControlEngine;
  UnitVersionForm := DynEngine.CreateForm ('Unit Versioning Sample', '');
  try
    if UnitVersionForm is TForm then
      TForm(UnitVersionForm).Position := poDesktopCenter;
    UnitVersionForm.Width := 500;
    UnitVersionForm.Height := 500;
    ButtonPanel := DynEngine.CreatePanelControl(UnitVersionForm, UnitVersionForm, 'ButtonPanel', '', alBottom);
    MainPanel := DynEngine.CreatePanelControl(UnitVersionForm, UnitVersionForm, 'MainPanel', '', alClient);
    if MainPanel is TPanel then
      TPanel(MainPanel).borderWidth := 3;
    Button:= DynEngine.CreateButton(UnitVersionForm, ButtonPanel,'CloseBtn', 'Close', '', CloseButtonOnClick, True, True);
    Button.Left := Round((UnitVersionForm.Width-Button.Width)/2);
    Button.Top := Round((ButtonPanel.Height-Button.Height)/2);
    BottomBox := DynEngine.CreateGroupBoxControl(UnitVersionForm, MainPanel, 'BottomBox', 'Details');
    BottomBox.Align := alBottom;
    TopBox := DynEngine.CreateGroupBoxControl(UnitVersionForm, MainPanel, 'TopBox', 'Unit Versions');
    TopBox.Align := alClient;
    TreeView := DynEngine.CreateTreeViewControl(UnitVersionForm, TopBox, 'TreeView');
    TreeView.Align := alClient;
    RCSFilePanel := DynEngine.CreatePanelControl(UnitVersionForm, BottomBox, 'RCSFilePanel', '', alTop);
    RCSFilePanel.Align := alTop;
    RCSFileEdit := DynEngine.CreateEditControl(UnitVersionForm, RCSFilePanel, 'RCSFileEdit');
    RCSFileEdit.Width := 340;
    if Supports(RCSFileEdit, IJvDynControlReadOnly, IJvReadOnly) then
      IJvReadOnly.ControlSetReadOnly(True);
    LabelControl := DynEngine.CreateLabelControlPanel(UnitVersionForm, RCSFilePanel,'RCSFileLabel', 'RCS File', RCSFileEdit, False, 80);
    RCSFilePanel.Height := RCSFileEdit.Height+1;
    RevisionPanel := DynEngine.CreatePanelControl(UnitVersionForm, BottomBox, 'RevisionPanel', '', alTop);
    RevisionPanel.Align := alTop;
    RevisionEdit := DynEngine.CreateEditControl(UnitVersionForm, RevisionPanel, 'RevisionEdit');
    RevisionEdit.Width := 340;
    LabelControl := DynEngine.CreateLabelControlPanel(UnitVersionForm, RevisionPanel,'RevisionLabel', 'Revision', RevisionEdit, False, 80);
    if Supports(RevisionEdit, IJvDynControlReadOnly, IJvReadOnly) then
      IJvReadOnly.ControlSetReadOnly(True);
    RevisionPanel.Height := RevisionEdit.Height+1;
    DatePanel := DynEngine.CreatePanelControl(UnitVersionForm, BottomBox, 'DatePanel', '', alTop);
    DatePanel.Align := alTop;
    DateEdit := DynEngine.CreateEditControl(UnitVersionForm, DatePanel, 'DateEdit');
    DateEdit.Width := 340;
    LabelControl := DynEngine.CreateLabelControlPanel(UnitVersionForm, DatePanel,'DateLabel', 'Date', DateEdit, False, 80);
    if Supports(DateEdit, IJvDynControlReadOnly, IJvReadOnly) then
      IJvReadOnly.ControlSetReadOnly(True);
    DatePanel.Height := DateEdit.Height+1;
    PathPanel := DynEngine.CreatePanelControl(UnitVersionForm, BottomBox, 'PathPanel', '', alTop);
    PathPanel.Align := alTop;
    PathEdit := DynEngine.CreateEditControl(UnitVersionForm, PathPanel, 'PathEdit');
    PathEdit.Width := 340;
    LabelControl := DynEngine.CreateLabelControlPanel(UnitVersionForm, PathPanel,'PathLabel', 'Path', PathEdit, False, 80);
    if Supports(PathEdit, IJvDynControlReadOnly, IJvReadOnly) then
      IJvReadOnly.ControlSetReadOnly(True);
    PathPanel.Height := PathEdit.Height+1;
    ExtraPanel := DynEngine.CreatePanelControl(UnitVersionForm, BottomBox, 'ExtraPanel', '', alTop);
    ExtraPanel.Align := alTop;
    ExtraEdit := DynEngine.CreateMemoControl(UnitVersionForm, ExtraPanel, 'ExtraEdit');
//    if Supports(ExtraEdit, IJvDynControlReadOnly, IJvReadOnly) then
//      IJvReadOnly.ControlSetReadOnly(True);
    ExtraEdit.Width := 400;
    LabelControl := DynEngine.CreateLabelControlPanel(UnitVersionForm, ExtraPanel,'ExtraLabel', 'Extra', ExtraEdit, True, 80);
    LabelControl.Width := 80 + PathEdit.Width;
    ExtraPanel.Height := LabelControl.Height;

    BottomBox.Height := DatePanel.Height*4+10+ExtraPanel.Height;

    if Supports(TreeView, IJvDynControlReadOnly, IJvReadOnly) then
      IJvReadOnly.ControlSetReadOnly(True);

    if Supports(TreeView, IJvDynControlTreeView, IJvTreeView) then
    begin
      Nodes := IJvTreeView.ControlGetItems;
      Nodes.Clear;
      IJvTreeView.ControlSetOnChange(TreeViewOnChange);
      MainNode := Nodes.AddChild(nil,extractfilename(ParamStr(0))+' '+VersionFixedFileInfoString( ParamStr(0)));
      with GetUnitVersioning do
        for i := 0 to count-1 do
          node := Nodes.AddChildObject(FindMasterNode (Nodes, MainNode, Items[i].LogPath), Items[i].RCSfile+' - '+Items[i].Revision, Items[i]);
      IJvTreeView.ControlSetSortType (stText);
      if TreeView is TTreeView then
        TTreeView(TreeView).FullExpand;
      MainNode.Selected := True;
    end;
    TreeViewOnChange(nil, nil);
    UnitVersionForm.ShowModal;
  finally
    UnitVersionForm.Free;
  end;
end;

procedure TForm1.CloseButtonOnClick(Sender : TObject);
begin
  if Assigned(UnitVersionForm) then
    UnitVersionForm.ModalResult := mrOk;
end;

procedure TForm1.TreeViewOnChange(Sender: TObject; Node: TTreeNode);
var
  IJvData : IJvDynControlData;
begin
  if Assigned(Node) and Assigned(Node.Data) and
    (TObject(Node.Data) is TUnitVersion) then
  begin
    if Supports(RCSFileEdit, IJvDynControlData, IJvData) then
    begin
      IJvData.ControlValue := TUnitVersion(Node.Data).RCSFile;
      RCSFilePanel.Visible := True;
    end
    else
      RCSFilePanel.Visible := False;
    if Supports(RevisionEdit, IJvDynControlData, IJvData) then
    begin
      IJvData.ControlValue := TUnitVersion(Node.Data).Revision;
      RevisionPanel.Visible := True;
    end
    else
      RevisionPanel.Visible := False;
    if Supports(DateEdit, IJvDynControlData, IJvData) then
    begin
      IJvData.ControlValue := TUnitVersion(Node.Data).Date;
      DatePanel.Visible := True;
    end
    else
      DatePanel.Visible := False;
    if Supports(ExtraEdit, IJvDynControlData, IJvData) and
      (TUnitVersion(Node.Data).Extra <> '') then
    begin
      IJvData.ControlValue := TUnitVersion(Node.Data).Extra;
      ExtraPanel.Visible := True;
    end
    else
      ExtraPanel.Visible := False;
    if Supports(PathEdit, IJvDynControlData, IJvData) then
    begin
      IJvData.ControlValue := TUnitVersion(Node.Data).LogPath;
      PathPanel.Visible := True;
    end
    else
      PathPanel.Visible := False;
  end
  else
  begin
    RCSFilePanel.Visible := False;
    RevisionPanel.Visible := False;
    DatePanel.Visible := False;
    ExtraPanel.Visible := False;
    PathPanel.Visible := False;
  end;
end;

procedure TForm1.Button17Click(Sender: TObject);
begin
  ShowUnitVersioning(nil);
end; 

procedure TForm1.Button18Click(Sender: TObject);
VAR
  ParameterList         : TJvParameterList;
  Parameter         : TJvBaseParameter;
begin
  ParameterList := TJvParameterList.Create(Self);
  TRY
    Parameter := TJvBaseParameter(TJvEditParameter.Create(ParameterList));
    WITH TJvEditParameter(Parameter) DO
    BEGIN
      Caption := '&Job';
      SearchName := 'Job';
      AsString := '999';
      ReadOnly := True;
      Width := 250;
    END; {*** WITH TJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvMemoParameter.Create(ParameterList));
    WITH TJvMemoParameter(Parameter) DO
    BEGIN
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
    END; {*** WITH TJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(tJvDateParameter.Create(ParameterList));
    WITH tJvDateParameter(Parameter) DO
    BEGIN
      Caption := '&Date';
      SearchName := 'Date';
      asDate := Trunc(now);
      Width := 120;
    END; {*** WITH TJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(tJvTimeParameter.Create(ParameterList));
    WITH tJvTimeParameter(Parameter) DO
    BEGIN
      Caption := '&Time';
      SearchName := 'Time';
      asDate := now;
      Width := 120;
    END; {*** WITH TJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvEditParameter.Create(ParameterList));
    WITH TJvEditParameter(Parameter) DO
    BEGIN
      Caption := '&Interval';
      SearchName := 'Interval';
      AsString := 'trunc(sysdate)+1';
      Width := 450;
    END; {*** WITH TJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvCheckBoxParameter.Create(ParameterList));
    WITH Parameter DO
    BEGIN
      Caption := '&Broken Job';
      SearchName := 'Broken';
      AsBoolean := FALSE;
      Width := 350;
    END; {*** WITH TJvEditParameter(Parameter) DO ***}
    ParameterList.AddParameter(Parameter);
    ParameterList.Messages.Caption := 'Change Job';
    ParameterList.Messages.OkButton := '&Ok';
    ParameterList.Messages.CancelButton := '&Cancel';
    ParameterList.Width := 510;
    ParameterList.ShowParameterDialog;
  FINALLY
    FreeAndNil(ParameterList);
  END;
end;

end.
