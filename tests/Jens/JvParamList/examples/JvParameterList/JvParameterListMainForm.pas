unit JvParameterListMainForm;

interface

{$DEFINE INCLUDE_DEVEXP_CX}

uses
  Windows, Messages, SysUtils, {Variants, }Classes, Graphics, Controls, Forms,
  Dialogs, JvDsaDialogs, jvParameterList, StdCtrls, jvParameterList_Parameter, Mask,
  JvToolEdit, jvPanel,
  {$IFDEF INCLUDE_DEVEXP_CX}
  cxButtons, cxListBox, cxRadioGroup, cxLookAndFeelPainters,
  cxDropDownEdit, cxCalendar, cxSpinEdit, cxTimeEdit, cxCheckBox, cxMemo,
  cxTextEdit, cxMaskEdit, cxControls, cxContainer, cxEdit, cxLabel,
  cxImage, cxCheckListBox,
  cxGroupBox,
  {$ENDIF}
  ExtCtrls, JvFormPlacement, JvComponent, JvAppStore,
  JvAppRegistryStore, JvDynControlEngine, ComCtrls, Buttons, JvBitBtn,
  JvCombobox, CheckLst, ShlObj, ExtDlgs, JvImage,
  JvMaskEdit, JvSpin, JvBaseEdits, JvGroupBox;

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
    JvAppRegistryStore: TJvAppRegistryStore;
    JvFormStorage1: TJvFormStorage;
    JvFormStorage2: TJvFormStorage;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
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
    JvPanel2: TJvPanel;
    JvGroupBoxAllControls: TJvGroupBox;
    JvGroupBox3: TJvGroupBox;
    JvPanelAllControls: TJvPanel;
    JvPanel4: TJvPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button11: TButton;
    Button12: TButton;
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
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    procedure ShowTest1(const aDynControlEngine: tJvDynControlEngine);
    procedure ShowTest2(const aDynControlEngine: tJvDynControlEngine);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses JvDynControlEngine_VCL,
  JvDynControlEngine_JVCL,
  JclBase,
  JvFormPlacementSelectList,
  JvDynControlEngine_VCLRed
     {$IFDEF INCLUDE_DEVEXP_CX}
  , JvDynControlEngine_DevExpCx
  , cxLookAndFeels
{$ENDIF}
  ;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowTest1(nil);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ShowTest1(DynControlEngine_VCL);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ShowTest1(DynControlEngine_JVCL);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  {$IFDEF INCLUDE_DEVEXP_CX}
  ShowTest1(DynControlEngine_DevExpCx);
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
    if ParameterList.DynControlEngine is tJvDynControlEngine_DevExpCx then
      with tJvDynControlEngine_DevExpCx(ParameterList.DynControlEngine) do
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
    Parameter := tjvTimeParameter.Create(ParameterList);
    with tjvTimeParameter(Parameter) do
    begin
      SearchName := 'TimeTest';
      Caption    := 'TimeTest';
      Format     := 'HH:mm:ss';
///      Width      := 100;
      AsDate     := Now;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvDateParameter.Create(ParameterList);
    with tjvDateParameter(Parameter) do
    begin
      SearchName := 'DateTest';
      Caption    := 'DateTest';
//      Width      := 80;
      AsDate     := Now;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvDateTimeParameter.Create(ParameterList);
    with tjvDateTimeParameter(Parameter) do
    begin
      SearchName := 'DateTimeTest';
      Caption    := 'DateTimeTest';
      AsDate     := Now;
//      Width      := 200;
      Height     := 50;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvEditParameter.Create(ParameterList);
    with Parameter do
    begin
      SearchName := 'EditTest';
      Caption    := 'EditTest';
//      AsString   := Edit1.Text;
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
    Parameter := tjvFileNameParameter.Create(ParameterList);
    with Parameter do
    begin
      SearchName := 'FileNameTest';
      Caption    := 'FileNameTest';
      ReadOnly   := true;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvDirectoryParameter.Create(ParameterList);
    with Parameter do
    begin
      SearchName := 'DirectoryTest';
      Caption    := 'DirectoryTest';
      ReadOnly   := true;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvMemoParameter.Create(ParameterList);
    with tjvMemoParameter(Parameter) do
    begin
      SearchName := 'MemoTest';
      Caption    := 'MemoTest';
      Height     := 60;
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
    Parameter := tjvImageParameter.Create(ParameterList);
    with tjvImageParameter(Parameter) do
    begin
      Picture    := Image1.Picture;
      SearchName := 'PictureTest';
      Caption    := 'PictureTest';
//      AutoSize := True;
      Height     := 180;
      Width      := 240;
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

    ParameterList.MaxHeight := StrToInt(MaxHeightEdit.Text);
    ParameterList.MaxWidth  := StrToInt(MaxWidthEdit.Text);
    ParameterList.Height    := StrToInt(HeightEdit.Text);
    ParameterList.Width     := StrToInt(WidthEdit.Text);
    ParameterList.HistoryEnabled := HistoryEnabledCheckBox.Checked;
    ParameterList.AppStore  := JvAppRegistryStore;
    ParameterList.Path      := 'Dialog 1';
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
    if ParameterList.DynControlEngine is tJvDynControlEngine_DevExpCx then
      with tJvDynControlEngine_DevExpCx(ParameterList.DynControlEngine) do
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
    ParameterList.Height    := StrToInt(HeightEdit.Text);
    ParameterList.Width     := StrToInt(WidthEdit.Text);
    ParameterList.HistoryEnabled := HistoryEnabledCheckBox.Checked;
    ParameterList.AppStore  := JvAppRegistryStore;
    ParameterList.Path      := 'Dialog 2';
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
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
var
  FormStorageSelectList: tJvFormStorageSelectList;
begin
//  SetDynControlEngine_DevExpCxDefault;
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
  with DynControlEngine_DevExpCx do
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
      CxProperties.StyleController.Style.BorderStyle := ebsSingle;
  end;
  {$ENDIF}
end;

procedure TForm1.VCLRadioButtonClick(Sender: TObject);
begin
  {$IFDEF INCLUDE_DEVEXP_CX}
  if CxRadioButton.Checked then
    SetDefaultDynControlEngine(DynControlEngine_DevExpCx)
  else  {$ENDIF}
  if JVCLRadioButton.Checked then
    SetDefaultDynControlEngine(DynControlEngine_JVCL)
  else if VCLRedRadioButton.Checked then
    SetDefaultDynControlEngine(DynControlEngine_VCLRed)
  else
    SetDefaultDynControlEngine(DynControlEngine_VCL);
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
  ShowTest1(DynControlEngine_VCLRed);
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

end.
