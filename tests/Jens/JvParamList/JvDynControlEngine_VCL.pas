unit JvDynControlEngine_VCL;

interface

uses Classes, Controls,
  StdCtrls, ExtCtrls, ComCtrls, Mask, Forms, Graphics, Buttons,
  JvDynControlEngine, JvDynControlEngine_Interface;


type
  TJvDynControlVCLMaskEdit = class (TMaskEdit, IUnknown, IJvDynControl, IJvDynControlData)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value : boolean);
    procedure ControlSetCaption(Value : string);
    procedure ControlSetTabOrder(Value : integer);

    procedure ControlSetOnEnter(Value : TNotifyEvent);
    procedure ControlSetOnExit(Value : TNotifyEvent);
    procedure ControlSetOnChange(Value : TNotifyEvent);
    procedure ControlSetOnClick(Value : TNotifyEvent);

    function ControlValidateData(var Value : variant; var ErrorMessage : string) : boolean;

    procedure ControlSetValue(Value : variant);
    function ControlGetValue : variant;
  end;

  TJvDynControlVCLDateTimeEdit = class (TDateTimePicker, IUnknown, IJvDynControl, IJvDynControlData)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value : boolean);
    procedure ControlSetCaption(Value : string);
    procedure ControlSetTabOrder(Value : integer);

    procedure ControlSetOnEnter(Value : TNotifyEvent);
    procedure ControlSetOnExit(Value : TNotifyEvent);
    procedure ControlSetOnChange(Value : TNotifyEvent);
    procedure ControlSetOnClick(Value : TNotifyEvent);

    function ControlValidateData(var Value : variant; var ErrorMessage : string) : boolean;

    procedure ControlSetValue(Value : variant);
    function ControlGetValue : variant;
  end;

  TJvDynControlVCLDateEdit = class (TJvDynControlVCLDateTimeEdit)
    procedure ControlSetDefaultProperties;
  end;

  TJvDynControlVCLTimeEdit = class (TJvDynControlVCLDateTimeEdit)
    procedure ControlSetDefaultProperties;
  end;

  TJvDynControlVCLCheckbox = class (TCheckbox, IUnknown, IJvDynControl, IJvDynControlData)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value : boolean);
    procedure ControlSetCaption(Value : string);
    procedure ControlSetTabOrder(Value : integer);

    procedure ControlSetOnEnter(Value : TNotifyEvent);
    procedure ControlSetOnExit(Value : TNotifyEvent);
    procedure ControlSetOnChange(Value : TNotifyEvent);
    procedure ControlSetOnClick(Value : TNotifyEvent);

    function ControlValidateData(var Value : variant; var ErrorMessage : string) : boolean;

    procedure ControlSetValue(Value : variant);
    function ControlGetValue : variant;
  end;

  TJvDynControlVCLMemo = class (TMemo, IUnknown, IJvDynControl, IJvDynControlData, IJvDynControlItems, IJvDynControlMemo)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value : boolean);
    procedure ControlSetCaption(Value : string);
    procedure ControlSetTabOrder(Value : integer);

    procedure ControlSetOnEnter(Value : TNotifyEvent);
    procedure ControlSetOnExit(Value : TNotifyEvent);
    procedure ControlSetOnChange(Value : TNotifyEvent);
    procedure ControlSetOnClick(Value : TNotifyEvent);

    function ControlValidateData(var Value : variant; var ErrorMessage : string) : boolean;

    procedure ControlSetValue(Value : variant);
    function ControlGetValue : variant;
    procedure ControlSetItems(Value : TStrings);
    function ControlGetItems : TStrings;

    procedure ControlSetWantTabs(Value : boolean);
    procedure ControlSetWantReturns(Value : boolean);
    procedure ControlSetWordWrap(Value : boolean);
    procedure ControlSetScrollbars(Value : TScrollStyle);
  end;

  TJvDynControlVCLRadioGroup = class (TRadioGroup, IUnknown, IJvDynControl, IJvDynControlData, IJvDynControlItems)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value : boolean);
    procedure ControlSetCaption(Value : string);
    procedure ControlSetTabOrder(Value : integer);

    procedure ControlSetOnEnter(Value : TNotifyEvent);
    procedure ControlSetOnExit(Value : TNotifyEvent);
    procedure ControlSetOnChange(Value : TNotifyEvent);
    procedure ControlSetOnClick(Value : TNotifyEvent);

    function ControlValidateData(var Value : variant; var ErrorMessage : string) : boolean;

    procedure ControlSetValue(Value : variant);
    function ControlGetValue : variant;
    procedure ControlSetItems(Value : TStrings);
    function ControlGetItems : TStrings;
  end;

  TJvDynControlVCLListBox = class (TListBox, IUnknown, IJvDynControl, IJvDynControlData, IJvDynControlItems, IJvDynControlDblClick)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value : boolean);
    procedure ControlSetCaption(Value : string);
    procedure ControlSetTabOrder(Value : integer);

    procedure ControlSetOnEnter(Value : TNotifyEvent);
    procedure ControlSetOnExit(Value : TNotifyEvent);
    procedure ControlSetOnChange(Value : TNotifyEvent);
    procedure ControlSetOnClick(Value : TNotifyEvent);

    function ControlValidateData(var Value : variant; var ErrorMessage : string) : boolean;

    procedure ControlSetValue(Value : variant);
    function ControlGetValue : variant;
    procedure ControlSetItems(Value : TStrings);
    function ControlGetItems : TStrings;

    procedure ControlSetOnDblClick(Value : TNotifyEvent);
  end;

  TJvDynControlVCLComboBox = class (TComboBox, IUnknown, IJvDynControl, IJvDynControlData, IJvDynControlItems)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value : boolean);
    procedure ControlSetCaption(Value : string);
    procedure ControlSetTabOrder(Value : integer);

    procedure ControlSetOnEnter(Value : TNotifyEvent);
    procedure ControlSetOnExit(Value : TNotifyEvent);
    procedure ControlSetOnChange(Value : TNotifyEvent);
    procedure ControlSetOnClick(Value : TNotifyEvent);

    function ControlValidateData(var Value : variant; var ErrorMessage : string) : boolean;

    procedure ControlSetValue(Value : variant);
    function ControlGetValue : variant;
    procedure ControlSetItems(Value : TStrings);
    function ControlGetItems : TStrings;
  end;

  TJvDynControlVCLPanel = class (TPanel, IUnknown, IJvDynControl, IJvDynControlPanel)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value : string);
    procedure ControlSetTabOrder(Value : integer);

    procedure ControlSetOnEnter(Value : TNotifyEvent);
    procedure ControlSetOnExit(Value : TNotifyEvent);
    procedure ControlSetOnClick(Value : TNotifyEvent);

    procedure ControlSetBorderWidth(Value : integer);
  end;

  TJvDynControlVCLScrollbox = class (TScrollbox, IUnknown, IJvDynControl)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value : string);
    procedure ControlSetTabOrder(Value : integer);

    procedure ControlSetOnEnter(Value : TNotifyEvent);
    procedure ControlSetOnExit(Value : TNotifyEvent);
    procedure ControlSetOnClick(Value : TNotifyEvent);

  end;

  TJvDynControlVCLLabel = class (TLabel, IUnknown, IJvDynControl, IJvDynControlLabel)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value : string);
    procedure ControlSetTabOrder(Value : integer);

    procedure ControlSetOnEnter(Value : TNotifyEvent);
    procedure ControlSetOnExit(Value : TNotifyEvent);
    procedure ControlSetOnClick(Value : TNotifyEvent);

    procedure ControlSetFocusControl(Value : tWinControl);
  end;

  TJvDynControlVCLStaticText = class (TStaticText, IUnknown, IJvDynControl)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value : string);
    procedure ControlSetTabOrder(Value : integer);

    procedure ControlSetOnEnter(Value : TNotifyEvent);
    procedure ControlSetOnExit(Value : TNotifyEvent);
    procedure ControlSetOnClick(Value : TNotifyEvent);
  end;

  TJvDynControlVCLButton = class (TBitBtn, IUnknown, IJvDynControl, IJvDynControlButton)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value : string);
    procedure ControlSetTabOrder(Value : integer);

    procedure ControlSetOnEnter(Value : TNotifyEvent);
    procedure ControlSetOnExit(Value : TNotifyEvent);
    procedure ControlSetOnClick(Value : TNotifyEvent);

    procedure ControlSetGlyph(Value : tBitmap);
    procedure ControlSetNumGlyphs(Value : integer);
    procedure ControlSetLayout(Value : TButtonLayout);
  end;

function DynControlEngine_VCL : tJvDynControlEngine;

implementation

uses SysUtils{, Variants};

var
  IntDynControlEngine_VCL : tJvDynControlEngine;

 //****************************************************************************
 // TJvDynControlVCLMaskEdit
 //****************************************************************************

procedure TJvDynControlVCLMaskEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLMaskEdit.ControlSetReadOnly(Value : boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlVCLMaskEdit.ControlSetCaption(Value : string);
begin
end;

procedure TJvDynControlVCLMaskEdit.ControlSetTabOrder(Value : integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLMaskEdit.ControlSetOnEnter(Value : TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLMaskEdit.ControlSetOnExit(Value : TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLMaskEdit.ControlSetOnChange(Value : TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlVCLMaskEdit.ControlSetOnClick(Value : TNotifyEvent);
begin

end;

procedure TJvDynControlVCLMaskEdit.ControlSetValue(Value : variant);
begin
  Text := Value;
end;

function TJvDynControlVCLMaskEdit.ControlGetValue : variant;
begin
  Result := Text;
end;

function TJvDynControlVCLMaskEdit.ControlValidateData(var Value : variant; var ErrorMessage : string) : boolean;
begin
  Result := true;
end;

 //****************************************************************************
 // TJvDynControlVCLDateTimeEdit                      
 //****************************************************************************


procedure TJvDynControlVCLDateTimeEdit.ControlSetDefaultProperties;
begin
  DateFormat := dfShort;
  DateMode := dmComboBox;
  Kind := dtkDate;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetReadOnly(Value : boolean);
begin
//  ReadOnly := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetCaption(Value : string);
begin
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetTabOrder(Value : integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetOnEnter(Value : TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetOnExit(Value : TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetOnChange(Value : TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetOnClick(Value : TNotifyEvent);
begin

end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetValue(Value : variant);
begin
  Text := Value;
end;

function TJvDynControlVCLDateTimeEdit.ControlGetValue : variant;
begin
  Result := Text;
end;

function TJvDynControlVCLDateTimeEdit.ControlValidateData(var Value : variant; var ErrorMessage : string) : boolean;
begin
  Result := true;
end;

procedure TJvDynControlVCLDateEdit.ControlSetDefaultProperties;
begin
  DateFormat := dfShort;
  DateMode := dmComboBox;
  Kind := dtkDate;
end;

procedure TJvDynControlVCLTimeEdit.ControlSetDefaultProperties;
begin
  DateFormat := dfShort;
  DateMode := dmUpDown;
  Kind := dtkTime;
end;

 //****************************************************************************
 // TJvDynControlVCLCheckbox                      
 //****************************************************************************

procedure TJvDynControlVCLCheckBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLCheckbox.ControlSetReadOnly(Value : boolean);
begin
end;

procedure TJvDynControlVCLCheckbox.ControlSetCaption(Value : string);
begin
  Caption := Value;
end;

procedure TJvDynControlVCLCheckbox.ControlSetTabOrder(Value : integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLCheckbox.ControlSetOnEnter(Value : TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLCheckbox.ControlSetOnExit(Value : TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLCheckbox.ControlSetOnChange(Value : TNotifyEvent);
begin
//  OnChange := Value;
end;

procedure TJvDynControlVCLCheckbox.ControlSetOnClick(Value : TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLCheckbox.ControlSetValue(Value : variant);
begin
  if VarType(Value) = varBoolean then
    Checked := Value
  else
    Checked := Uppercase(Value) = 'TRUE';
end;

function TJvDynControlVCLCheckbox.ControlGetValue : variant;
begin
  Result := Checked;
end;

function TJvDynControlVCLCheckbox.ControlValidateData(var Value : variant; var ErrorMessage : string) : boolean;
begin
  Result := true;
end;

 //****************************************************************************
 // TJvDynControlVCLMemo                      
 //****************************************************************************

procedure TJvDynControlVCLMemo.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLMemo.ControlSetReadOnly(Value : boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetCaption(Value : string);
begin
end;

procedure TJvDynControlVCLMemo.ControlSetTabOrder(Value : integer);
begin
  TabOrder := Value;
end;


procedure TJvDynControlVCLMemo.ControlSetOnEnter(Value : TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetOnExit(Value : TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetOnChange(Value : TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetOnClick(Value : TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetValue(Value : variant);
begin
  Text := Value;
end;

function TJvDynControlVCLMemo.ControlGetValue : variant;
begin
  Result := Text;
end;

procedure TJvDynControlVCLMemo.ControlSetItems(Value : TStrings);
begin
  Lines.Assign(Value);
end;

function TJvDynControlVCLMemo.ControlGetItems : TStrings;
begin
  Result := Lines;
end;

function TJvDynControlVCLMemo.ControlValidateData(var Value : variant; var ErrorMessage : string) : boolean;
begin
  Result := true;
end;

procedure TJvDynControlVCLMemo.ControlSetWantTabs(Value : boolean);
begin
  WantTabs := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetWantReturns(Value : boolean);
begin
  WantReturns := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetWordWrap(Value : boolean);
begin
  WordWrap := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetScrollbars(Value : TScrollStyle);
begin
  Scrollbars := Value;
end;


 //****************************************************************************
 // TJvDynControlVCLRadioGroup                      
 //****************************************************************************

procedure TJvDynControlVCLRadioGroup.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLRadioGroup.ControlSetReadOnly(Value : boolean);
begin
//  ReadOnly := Value;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetCaption(Value : string);
begin
end;

procedure TJvDynControlVCLRadioGroup.ControlSetTabOrder(Value : integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetOnEnter(Value : TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetOnExit(Value : TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetOnChange(Value : TNotifyEvent);
begin
//  OnChange := Value;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetOnClick(Value : TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetValue(Value : variant);
begin
  if VarType(Value) in [varSmallInt, varInteger] then
    ItemIndex := Value
  else
    try
      ItemIndex := Value
    except
      on e : Exception do
        ItemIndex := Items.IndexOf(Value);
    end;
end;

function TJvDynControlVCLRadioGroup.ControlGetValue : variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetItems(Value : TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlVCLRadioGroup.ControlGetItems : TStrings;
begin
  Result := Items;
end;

function TJvDynControlVCLRadioGroup.ControlValidateData(var Value : variant; var ErrorMessage : string) : boolean;
begin
  Result := true;
end;

 //****************************************************************************
 // TJvDynControlVCLListBox                      
 //****************************************************************************

procedure TJvDynControlVCLListBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLListBox.ControlSetReadOnly(Value : boolean);
begin
//  ReadOnly := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetCaption(Value : string);
begin
end;

procedure TJvDynControlVCLListBox.ControlSetTabOrder(Value : integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetOnEnter(Value : TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetOnExit(Value : TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetOnChange(Value : TNotifyEvent);
begin
//  OnChange := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetOnClick(Value : TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetValue(Value : variant);
begin
  if VarType(Value) in [varSmallInt, varInteger] then
    ItemIndex := Value
  else
    try
      ItemIndex := Value
    except
      on e : Exception do
        ItemIndex := Items.IndexOf(Value);
    end;
end;

function TJvDynControlVCLListBox.ControlGetValue : variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlVCLListBox.ControlSetItems(Value : TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlVCLListBox.ControlGetItems : TStrings;
begin
  Result := Items;
end;

function TJvDynControlVCLListBox.ControlValidateData(var Value : variant; var ErrorMessage : string) : boolean;
begin
  Result := true;
end;

procedure TJvDynControlVCLListBox.ControlSetOnDblClick(Value : TNotifyEvent);
begin
  OnDblClick := Value;
end;


 //****************************************************************************
 // TJvDynControlVCLComboBox                      
 //****************************************************************************

procedure TJvDynControlVCLComboBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLComboBox.ControlSetReadOnly(Value : boolean);
begin
//  ReadOnly := Value;
end;

procedure TJvDynControlVCLComboBox.ControlSetCaption(Value : string);
begin
end;

procedure TJvDynControlVCLComboBox.ControlSetTabOrder(Value : integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLComboBox.ControlSetOnEnter(Value : TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLComboBox.ControlSetOnExit(Value : TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLComboBox.ControlSetOnChange(Value : TNotifyEvent);
begin
//  OnChange := Value;
end;

procedure TJvDynControlVCLComboBox.ControlSetOnClick(Value : TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLComboBox.ControlSetValue(Value : variant);
begin
  Text := Value;
end;

function TJvDynControlVCLComboBox.ControlGetValue : variant;
begin
  Result := Text;
end;

procedure TJvDynControlVCLComboBox.ControlSetItems(Value : TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlVCLComboBox.ControlGetItems : TStrings;
begin
  Result := Items;
end;

function TJvDynControlVCLComboBox.ControlValidateData(var Value : variant; var ErrorMessage : string) : boolean;
begin
  Result := true;
end;



 //****************************************************************************
 // TJvDynControlVCLPanel                      
 //****************************************************************************

procedure TJvDynControlVCLPanel.ControlSetDefaultProperties;
begin
  BevelInner := bvNone;
  BevelOuter := bvNone;
end;

procedure TJvDynControlVCLPanel.ControlSetCaption(Value : string);
begin
  Caption := Value;
end;

procedure TJvDynControlVCLPanel.ControlSetTabOrder(Value : integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLPanel.ControlSetOnEnter(Value : TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLPanel.ControlSetOnExit(Value : TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLPanel.ControlSetOnClick(Value : TNotifyEvent);
begin
end;

procedure TJvDynControlVCLPanel.ControlSetBorderWidth(Value : integer);
begin
  BorderWidth := Value;
end;

 //****************************************************************************
 // TJvDynControlVCLScrollbox                      
 //****************************************************************************

procedure TJvDynControlVCLScrollbox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLScrollbox.ControlSetCaption(Value : string);
begin
  Caption := Value;
end;

procedure TJvDynControlVCLScrollbox.ControlSetTabOrder(Value : integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLScrollbox.ControlSetOnEnter(Value : TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLScrollbox.ControlSetOnExit(Value : TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLScrollbox.ControlSetOnClick(Value : TNotifyEvent);
begin
end;

 //****************************************************************************
 // TJvDynControlVCLLabel                      
 //****************************************************************************

procedure TJvDynControlVCLLabel.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLLabel.ControlSetCaption(Value : string);
begin
  Caption := Value;
end;

procedure TJvDynControlVCLLabel.ControlSetTabOrder(Value : integer);
begin
end;

procedure TJvDynControlVCLLabel.ControlSetOnEnter(Value : TNotifyEvent);
begin
end;

procedure TJvDynControlVCLLabel.ControlSetOnExit(Value : TNotifyEvent);
begin
end;

procedure TJvDynControlVCLLabel.ControlSetOnClick(Value : TNotifyEvent);
begin
end;

procedure TJvDynControlVCLLabel.ControlSetFocusControl(Value : tWinControl);
begin
  FocusControl := Value;
end;

 //****************************************************************************
 // TJvDynControlVCLStaticText                      
 //****************************************************************************

procedure TJvDynControlVCLStaticText.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLStaticText.ControlSetCaption(Value : string);
begin
  Caption := Value;
end;

procedure TJvDynControlVCLStaticText.ControlSetTabOrder(Value : integer);
begin
end;

procedure TJvDynControlVCLStaticText.ControlSetOnEnter(Value : TNotifyEvent);
begin
end;

procedure TJvDynControlVCLStaticText.ControlSetOnExit(Value : TNotifyEvent);
begin
end;

procedure TJvDynControlVCLStaticText.ControlSetOnClick(Value : TNotifyEvent);
begin
end;

 //****************************************************************************
 // TJvDynControlVCLButton                      
 //****************************************************************************

procedure TJvDynControlVCLButton.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLButton.ControlSetCaption(Value : string);
begin
  Caption := Value;
end;

procedure TJvDynControlVCLButton.ControlSetTabOrder(Value : integer);
begin
end;

procedure TJvDynControlVCLButton.ControlSetOnEnter(Value : TNotifyEvent);
begin
end;

procedure TJvDynControlVCLButton.ControlSetOnExit(Value : TNotifyEvent);
begin
end;

procedure TJvDynControlVCLButton.ControlSetOnClick(Value : TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLButton.ControlSetGlyph(Value : tBitmap);
begin
  Glyph.Assign(Value);
end;

procedure TJvDynControlVCLButton.ControlSetNumGlyphs(Value : integer);
begin
  NumGlyphs := Value;
end;

procedure TJvDynControlVCLButton.ControlSetLayout(Value : TButtonLayout);
begin
  Layout := Value;
end;


function DynControlEngine_VCL : tJvDynControlEngine;
begin
  Result := IntDynControlEngine_VCL;
end;

initialization
  IntDynControlEngine_VCL := tJvDynControlEngine.Create;
  IntDynControlEngine_VCL.RegisterControl(jctLabel, TJvDynControlVCLLabel);
  IntDynControlEngine_VCL.RegisterControl(jctStaticText, TJvDynControlVCLStaticText);
  IntDynControlEngine_VCL.RegisterControl(jctButton, TJvDynControlVCLButton);
  IntDynControlEngine_VCL.RegisterControl(jctScrollbox, TJvDynControlVCLScrollbox);
  IntDynControlEngine_VCL.RegisterControl(jctPanel, TJvDynControlVCLPanel);
  IntDynControlEngine_VCL.RegisterControl(jctCheckBox, TJvDynControlVCLCheckBox);
  IntDynControlEngine_VCL.RegisterControl(jctComboBox, TJvDynControlVCLComboBox);
  IntDynControlEngine_VCL.RegisterControl(jctListBox, TJvDynControlVCLListBox);
  IntDynControlEngine_VCL.RegisterControl(jctRadioGroup, TJvDynControlVCLRadioGroup);
  IntDynControlEngine_VCL.RegisterControl(jctDateTimeEdit, TJvDynControlVCLDateTimeEdit);
  IntDynControlEngine_VCL.RegisterControl(jctTimeEdit, TJvDynControlVCLTimeEdit);
  IntDynControlEngine_VCL.RegisterControl(jctDateEdit, TJvDynControlVCLDateEdit);
  IntDynControlEngine_VCL.RegisterControl(jctEdit, TJvDynControlVCLMaskEdit);
  IntDynControlEngine_VCL.RegisterControl(jctIntegerEdit, TJvDynControlVCLMaskEdit);
  IntDynControlEngine_VCL.RegisterControl(jctDoubleEdit, TJvDynControlVCLMaskEdit);
  IntDynControlEngine_VCL.RegisterControl(jctDirectoryEdit, TJvDynControlVCLMaskEdit);
  IntDynControlEngine_VCL.RegisterControl(jctFileNameEdit, TJvDynControlVCLMaskEdit);
  IntDynControlEngine_VCL.RegisterControl(jctMemo, TJvDynControlVCLMemo);
  SetDefaultDynControlEngine(IntDynControlEngine_VCL);

finalization
  FreeAndNil(IntDynControlEngine_VCL);
end.
