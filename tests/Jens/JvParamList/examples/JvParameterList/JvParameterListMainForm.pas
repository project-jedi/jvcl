unit JvParameterListMainForm;

interface

{.$DEFINE INCLUDE_DEVEXP_CX}

uses
  Windows, Messages, SysUtils, {Variants, }Classes, Graphics, Controls, Forms,
  Dialogs, jvParameterList, StdCtrls, jvParameterList_Parameter, Mask,
  JvToolEdit,
  {$IFDEF INCLUDE_DEVEXP_CX}
  cxButtons, cxListBox, cxRadioGroup, cxLookAndFeelPainters,
  cxDropDownEdit, cxCalendar, cxSpinEdit, cxTimeEdit, cxCheckBox, cxMemo,
  cxTextEdit, cxMaskEdit, cxControls, cxContainer, cxEdit, cxLabel,
  cxImage,
  cxGroupBox,
  {$ENDIF}
  ExtCtrls, JvFormPlacement, JvComponent, JvAppStore,
  JvAppRegistryStore, JvDynControlEngine, ComCtrls, Buttons, JvBitBtn;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
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
    DevExpCxLookAndFeelRadioGroup: TRadioGroup;
    JvAppRegistryStore: TJvAppRegistryStore;
    JvFormStorage1: TJvFormStorage;
    JvFormStorage2: TJvFormStorage;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    HistoryEnabledCheckBox: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure DevExpCxLookAndFeelRadioGroupClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    procedure ShowTest1 (CONST aDynControlEngine : tJvDynControlEngine);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

Uses JvDynControlEngine_VCL,
     JvDynControlEngine_JVCL,
     JvFormPlacementSelectList
     {$IFDEF INCLUDE_DEVEXP_CX}
     ,JvDynControlEngine_DevExpCx
     ,cxLookAndFeels
     {$ENDIF}
     ;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowTest1 (nil);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ShowTest1 (DynControlEngine_VCL);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ShowTest1 (DynControlEngine_JVCL);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  {$IFDEF INCLUDE_DEVEXP_CX}
  ShowTest1 (DynControlEngine_DevExpCx);
  {$ENDIF}
end;

procedure TForm1.ShowTest1 (CONST aDynControlEngine : tJvDynControlEngine);
Var ParameterList : TJvParameterList;
    Parameter     : TJvBaseParameter;
begin
  ParameterList := TJvParameterList.Create(self);
  try
    IF Assigned (aDynControlEngine) THEN
      ParameterList.DynControlEngine := aDynControlEngine;
    {$IFDEF INCLUDE_DEVEXP_CX}
    IF ParameterList.DynControlEngine IS tJvDynControlEngine_DevExpCx THEN
      WITH tJvDynControlEngine_DevExpCx(ParameterList.DynControlEngine) DO
      Begin
        Case DevExpCxLookAndFeelRadioGroup.ItemIndex OF
          1 : cxProperties.LookAndFeel.Kind := lfFlat;
          2 : cxProperties.LookAndFeel.Kind := lfUltraFlat;
        Else
          cxProperties.LookAndFeel.Kind := lfStandard;
        End;
      End;
    {$ENDIF}
    Parameter := tjvEditParameter.Create(ParameterList);
    With Parameter Do
    Begin
      SearchName := 'EditTest';
      Caption := 'EditTest';
      AsString := Edit1.text;
    End;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvCheckboxParameter.Create(ParameterList);
    With Parameter Do
    Begin
      SearchName := 'CheckboxTest';
      Caption := '&Checkbox';
    End;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvIntegerEditParameter.Create(ParameterList);
    With Parameter Do
    Begin
      SearchName := 'IntegerTest';
      Caption := 'IntegerTest';
    End;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvFileNameParameter.Create(ParameterList);
    With Parameter Do
    Begin
      SearchName := 'FileNameTest';
      Caption := 'FileNameTest';
    End;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvDirectoryParameter.Create(ParameterList);
    With Parameter Do
    Begin
      SearchName := 'DirectoryTest';
      Caption := 'DirectoryTest';
    End;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvMemoParameter.Create(ParameterList);
    With tjvMemoParameter(Parameter) Do
    Begin
      SearchName := 'MemoTest';
      Caption := 'MemoTest';
      Height := 60;
      Scrollbars := ssBoth	;
    End;
    ParameterList.AddParameter(Parameter);
    Parameter := tjvRadioGroupParameter.Create(ParameterList);
    With tjvRadioGroupParameter(Parameter) Do
    Begin
      SearchName := 'RadioGroupTest';
      Caption := '&RadioGroupTest';
      ItemList.Add ('Test&1');
      ItemList.Add ('Test&2');
      ItemList.Add ('Test&3');
    End;
    ParameterList.AddParameter(Parameter);
    ParameterList.Width := 400;
    //ParameterList.Height := 300;
    ParameterList.AutoHeight := AutoHeightCheckBox.Checked;
    ParameterList.AutoWidth := AutoWidthCheckBox.Checked;
    ParameterList.MaxHeight := strtoint (MaxHeightEdit.Text);
    ParameterList.MaxWidth := strtoint (MaxWidthEdit.Text);
    ParameterList.Height := strtoint (HeightEdit.Text);
    ParameterList.Width := strtoint (WidthEdit.Text);
    ParameterList.HistoryEnabled := HistoryEnabledCheckBox.Checked;
    ParameterList.AppStore := JvAppRegistryStore;
    ParameterList.Path := 'Dialog 1';
    IF LoadFromCheckBox.Checked THEN
      ParameterList.LoadData;
    IF ParameterList.ShowParameterDialog THEN
      IF StoreToCheckBox.Checked THEN
        ParameterList.storeData;
//    Edit1.text := ParameterList.parameterByName('RadioGroupTest').AsString;
  finally
    ParameterList.Free;
  end;
end;



procedure TForm1.FormShow(Sender: TObject);
begin
  {$IFNDEF INCLUDE_DEVEXP_CX}
  Button4.Enabled := False;
  DevExpCxLookAndFeelRadioGroup.Enabled := FALSE;
  {$ENDIF}
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
VAR FormStorageSelectList : tJvFormStorageSelectList;
begin
//  SetDynControlEngine_DevExpCxDefault;
  FormStorageSelectList := tJvFormStorageSelectList.Create (Self);
  try
    FormStorageSelectList.FormStorage := JvFormStorage2;
    FormStorageSelectList.SelectPath := 'SelectTest';
//    MessageDlg(FormStorageSelectList.GetSelectPath(sloLoad), mtWarning, [mbOK], 0);
    FormStorageSelectList.RestoreFormStorage;
  finally
    FormStorageSelectList.Free;
  end;
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
VAR FormStorageSelectList : tJvFormStorageSelectList;
begin
  FormStorageSelectList := tJvFormStorageSelectList.Create (Self);
  try
    FormStorageSelectList.FormStorage := JvFormStorage2;
    FormStorageSelectList.SelectPath := 'SelectTest';
//    MessageDlg(FormStorageSelectList.GetSelectPath(sloStore), mtWarning, [mbOK], 0);
    FormStorageSelectList.SaveFormStorage;
  finally
    FormStorageSelectList.Free;
  end;
end;


procedure TForm1.DevExpCxLookAndFeelRadioGroupClick(Sender: TObject);
begin
  {$IFDEF INCLUDE_DEVEXP_CX}
  WITH DynControlEngine_DevExpCx DO
  Begin
    Case DevExpCxLookAndFeelRadioGroup.ItemIndex OF
      1 : cxProperties.LookAndFeel.Kind := lfFlat;
      2 : cxProperties.LookAndFeel.Kind := lfUltraFlat;
    Else
      cxProperties.LookAndFeel.Kind := lfStandard;
    End;
  End;
  {$ENDIF}
end;

end.
