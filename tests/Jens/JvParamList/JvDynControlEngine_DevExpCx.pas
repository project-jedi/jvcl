{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPlacemnt.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDynControlEngine_DevExpCx;

interface

uses Classes, Controls,
  StdCtrls, ExtCtrls, ComCtrls, Mask, Forms, Graphics, Buttons,
  cxLookAndFeels, cxMaskEdit, cxLabel, cxButtons, cxListBox, cxDropDownEdit,
  cxCalendar, cxCheckBox, cxMemo, cxRadioGroup,
  JvDynControlEngine, JvDynControlEngine_Interface;

type
  TcxDynControlWrapper = class (TPersistent)
  private
    fLookAndFeel : tcxLookAndFeel;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property LookAndFeel : tcxLookAndFeel Read fLookAndFeel Write fLookAndFeel;
  end;

  IJvDynControlDevExpCx = interface
    ['{247D29CD-ABA4-4F87-A25D-4987BD950F0C}']
    procedure ControlSetcxProperties(Value : TcxDynControlWrapper);
  end;

  TJvDynControlCxMaskEdit = class (TcxMaskEdit, IJvDynControl, IJvDynControlData, IJvDynControlDevExpCx)
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

    procedure ControlSetcxProperties(Value : TcxDynControlWrapper);
  end;

  TJvDynControlCxDateTimeEdit = class (TcxDateEdit, IJvDynControl, IJvDynControlData, IJvDynControlDevExpCx)
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

    procedure ControlSetcxProperties(Value : TcxDynControlWrapper);
  end;

  TJvDynControlCxDateEdit = class (TJvDynControlCxDateTimeEdit)
    procedure ControlSetDefaultProperties;
  end;

  TJvDynControlCxTimeEdit = class (TJvDynControlCxDateTimeEdit)
    procedure ControlSetDefaultProperties;
  end;

  TJvDynControlCxCheckbox = class (TcxCheckbox, IJvDynControl, IJvDynControlData, IJvDynControlDevExpCx)
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

    procedure ControlSetcxProperties(Value : TcxDynControlWrapper);
  end;

  TJvDynControlCxMemo = class (TcxMemo, IJvDynControl, IJvDynControlData, IJvDynControlItems, IJvDynControlMemo, IJvDynControlDevExpCx)
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

    procedure ControlSetcxProperties(Value : TcxDynControlWrapper);
  end;

  TJvDynControlCxRadioGroup = class (TcxRadioGroup, IJvDynControl, IJvDynControlData, IJvDynControlItems, IJvDynControlDevExpCx)
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

    procedure ControlSetcxProperties(Value : TcxDynControlWrapper);
  end;

  TJvDynControlCxListBox = class (TcxListBox, IJvDynControl, IJvDynControlData, IJvDynControlItems, IJvDynControlDblClick, IJvDynControlDevExpCx)
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

    procedure ControlSetcxProperties(Value : TcxDynControlWrapper);
  end;

  TJvDynControlCxComboBox = class (TcxComboBox, IJvDynControl, IJvDynControlData, IJvDynControlItems, IJvDynControlDevExpCx)
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

    procedure ControlSetcxProperties(Value : TcxDynControlWrapper);
  end;

  TJvDynControlCxPanel = class (TPanel, IJvDynControl, IJvDynControlPanel)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value : string);
    procedure ControlSetTabOrder(Value : integer);

    procedure ControlSetOnEnter(Value : TNotifyEvent);
    procedure ControlSetOnExit(Value : TNotifyEvent);
    procedure ControlSetOnClick(Value : TNotifyEvent);

    procedure ControlSetBorderWidth(Value : integer);
  end;

  TJvDynControlCxScrollbox = class (TScrollbox, IJvDynControl)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value : string);
    procedure ControlSetTabOrder(Value : integer);

    procedure ControlSetOnEnter(Value : TNotifyEvent);
    procedure ControlSetOnExit(Value : TNotifyEvent);
    procedure ControlSetOnClick(Value : TNotifyEvent);

  end;

  TJvDynControlCxLabel = class (TLabel, IJvDynControl, IJvDynControlLabel, IJvDynControlDevExpCx)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value : string);
    procedure ControlSetTabOrder(Value : integer);

    procedure ControlSetOnEnter(Value : TNotifyEvent);
    procedure ControlSetOnExit(Value : TNotifyEvent);
    procedure ControlSetOnClick(Value : TNotifyEvent);

    procedure ControlSetFocusControl(Value : tWinControl);

    procedure ControlSetcxProperties(Value : TcxDynControlWrapper);
  end;

  TJvDynControlCxStaticText = class (TcxLabel, IJvDynControl, IJvDynControlDevExpCx)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value : string);
    procedure ControlSetTabOrder(Value : integer);

    procedure ControlSetOnEnter(Value : TNotifyEvent);
    procedure ControlSetOnExit(Value : TNotifyEvent);
    procedure ControlSetOnClick(Value : TNotifyEvent);

    procedure ControlSetcxProperties(Value : TcxDynControlWrapper);
  end;

  TJvDynControlCxButton = class (TcxButton, IJvDynControl, IJvDynControlButton, IJvDynControlDevExpCx)
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

    procedure ControlSetcxProperties(Value : TcxDynControlWrapper);
  end;

  tJvDynControlEngine_DevExpCx = class (tJvDynControlEngine)
  private
    fcxProperties : TcxDynControlWrapper;
  protected
    procedure SetcxProperties(Value : TcxDynControlWrapper);
  public
    constructor Create; override;
    destructor Destroy; override;
    function CreateControlClass(aControlClass : tControlClass; aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tControl; override;
  published
    property cxProperties : TcxDynControlWrapper Read fcxProperties Write fcxProperties;
  end;


function DynControlEngine_DevExpCx : tJvDynControlEngine_DevExpCx;
procedure SetDynControlEngine_DevExpCxDefault;

implementation

uses SysUtils, Variants, cxControls;

var
  IntDynControlEngine_DevExpCx : tJvDynControlEngine_DevExpCx;

 //****************************************************************************
 // TcxDynControlWrapper
 //****************************************************************************

constructor TcxDynControlWrapper.Create;
begin
  inherited Create;
  fLookAndFeel := tcxLookAndFeel.Create(nil);
end;

destructor TcxDynControlWrapper.Destroy;
begin
  FreeAndNil(fLookAndFeel);
  inherited Destroy;
end;

 //****************************************************************************
 // TJvDynControlCxMaskEdit
 //****************************************************************************

procedure TJvDynControlCxMaskEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxMaskEdit.ControlSetReadOnly(Value : boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxMaskEdit.ControlSetCaption(Value : string);
begin
end;

procedure TJvDynControlCxMaskEdit.ControlSetTabOrder(Value : integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxMaskEdit.ControlSetOnEnter(Value : TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxMaskEdit.ControlSetOnExit(Value : TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxMaskEdit.ControlSetOnChange(Value : TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxMaskEdit.ControlSetOnClick(Value : TNotifyEvent);
begin

end;

procedure TJvDynControlCxMaskEdit.ControlSetValue(Value : variant);
begin
  Text := Value;
end;

function TJvDynControlCxMaskEdit.ControlGetValue : variant;
begin
  Result := Text;
end;

function TJvDynControlCxMaskEdit.ControlValidateData(var Value : variant; var ErrorMessage : string) : boolean;
begin
  Result := true;
end;

procedure TJvDynControlCxMaskEdit.ControlSetcxProperties(Value : TcxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
end;

 //****************************************************************************
 // TJvDynControlCxDateTimeEdit
 //****************************************************************************


procedure TJvDynControlCxDateTimeEdit.ControlSetDefaultProperties;
begin
  Properties.ShowTime  := true;
  Properties.SaveTime  := true;
  Properties.InputKind := ikStandard;
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetReadOnly(Value : boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetCaption(Value : string);
begin
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetTabOrder(Value : integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetOnEnter(Value : TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetOnExit(Value : TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetOnChange(Value : TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetOnClick(Value : TNotifyEvent);
begin

end;

procedure TJvDynControlCxDateTimeEdit.ControlSetValue(Value : variant);
begin
  Text := Value;
end;

function TJvDynControlCxDateTimeEdit.ControlGetValue : variant;
begin
  Result := Text;
end;

function TJvDynControlCxDateTimeEdit.ControlValidateData(var Value : variant; var ErrorMessage : string) : boolean;
begin
  Result := true;
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetcxProperties(Value : TcxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
end;

procedure TJvDynControlCxDateEdit.ControlSetDefaultProperties;
begin
 //  DateFormat := dfShort;
 //  DateMode := dmComboBox;
 //  Kind := dtkDate;
end;

procedure TJvDynControlCxTimeEdit.ControlSetDefaultProperties;
begin
 //  DateFormat := dfShort;
 //  DateMode := dmUpDown;
 //  Kind := dtkTime;
end;

 //****************************************************************************
 // TJvDynControlCxCheckbox                      
 //****************************************************************************

procedure TJvDynControlCxCheckBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxCheckbox.ControlSetReadOnly(Value : boolean);
begin
end;

procedure TJvDynControlCxCheckbox.ControlSetCaption(Value : string);
begin
  Caption := Value;
end;

procedure TJvDynControlCxCheckbox.ControlSetTabOrder(Value : integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxCheckbox.ControlSetOnEnter(Value : TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxCheckbox.ControlSetOnExit(Value : TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxCheckbox.ControlSetOnChange(Value : TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxCheckbox.ControlSetOnClick(Value : TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxCheckbox.ControlSetValue(Value : variant);
begin
  if VarType(Value) = varBoolean then
    Checked := Value
  else
    Checked := Uppercase(Value) = 'TRUE';
end;

function TJvDynControlCxCheckbox.ControlGetValue : variant;
begin
  Result := Checked;
end;

function TJvDynControlCxCheckbox.ControlValidateData(var Value : variant; var ErrorMessage : string) : boolean;
begin
  Result := true;
end;

procedure TJvDynControlCxCheckbox.ControlSetcxProperties(Value : TcxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
end;


 //****************************************************************************
 // TJvDynControlCxMemo                      
 //****************************************************************************

procedure TJvDynControlCxMemo.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxMemo.ControlSetReadOnly(Value : boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxMemo.ControlSetCaption(Value : string);
begin
end;

procedure TJvDynControlCxMemo.ControlSetTabOrder(Value : integer);
begin
  TabOrder := Value;
end;


procedure TJvDynControlCxMemo.ControlSetOnEnter(Value : TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxMemo.ControlSetOnExit(Value : TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxMemo.ControlSetOnChange(Value : TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxMemo.ControlSetOnClick(Value : TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxMemo.ControlSetValue(Value : variant);
begin
  Text := Value;
end;

function TJvDynControlCxMemo.ControlGetValue : variant;
begin
  Result := Text;
end;

procedure TJvDynControlCxMemo.ControlSetItems(Value : TStrings);
begin
  Lines.Assign(Value);
end;

function TJvDynControlCxMemo.ControlGetItems : TStrings;
begin
  Result := Lines;
end;

function TJvDynControlCxMemo.ControlValidateData(var Value : variant; var ErrorMessage : string) : boolean;
begin
  Result := true;
end;

procedure TJvDynControlCxMemo.ControlSetWantTabs(Value : boolean);
begin
  Properties.WantTabs := Value;
end;

procedure TJvDynControlCxMemo.ControlSetWantReturns(Value : boolean);
begin
  Properties.WantReturns := Value;
end;

procedure TJvDynControlCxMemo.ControlSetWordWrap(Value : boolean);
begin
  Properties.WordWrap := Value;
end;

procedure TJvDynControlCxMemo.ControlSetScrollbars(Value : TScrollStyle);
begin
  Properties.Scrollbars := Value;
end;

procedure TJvDynControlCxMemo.ControlSetcxProperties(Value : TcxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
end;

 //****************************************************************************
 // TJvDynControlCxRadioGroup
 //****************************************************************************

procedure TJvDynControlCxRadioGroup.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxRadioGroup.ControlSetReadOnly(Value : boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxRadioGroup.ControlSetCaption(Value : string);
begin
end;

procedure TJvDynControlCxRadioGroup.ControlSetTabOrder(Value : integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxRadioGroup.ControlSetOnEnter(Value : TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxRadioGroup.ControlSetOnExit(Value : TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxRadioGroup.ControlSetOnChange(Value : TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxRadioGroup.ControlSetOnClick(Value : TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxRadioGroup.ControlSetValue(Value : variant);
begin
  if VarType(Value) in [varSmallInt, varInteger] then
    ItemIndex := Value
  else
    try
      ItemIndex := Value
    except
      on e : Exception do
//        ItemIndex := Properties.Items. IndexOf(Value);
    end;
end;

function TJvDynControlCxRadioGroup.ControlGetValue : variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlCxRadioGroup.ControlSetItems(Value : TStrings);
begin
//  Items.Assign(Value);
end;

function TJvDynControlCxRadioGroup.ControlGetItems : TStrings;
begin
//  Result := Items;
end;

function TJvDynControlCxRadioGroup.ControlValidateData(var Value : variant; var ErrorMessage : string) : boolean;
begin
  Result := true;
end;

procedure TJvDynControlCxRadioGroup.ControlSetcxProperties(Value : TcxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
end;


 //****************************************************************************
 // TJvDynControlCxListBox
 //****************************************************************************

procedure TJvDynControlCxListBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxListBox.ControlSetReadOnly(Value : boolean);
begin
//  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxListBox.ControlSetCaption(Value : string);
begin
end;

procedure TJvDynControlCxListBox.ControlSetTabOrder(Value : integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxListBox.ControlSetOnEnter(Value : TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxListBox.ControlSetOnExit(Value : TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxListBox.ControlSetOnChange(Value : TNotifyEvent);
begin
end;

procedure TJvDynControlCxListBox.ControlSetOnClick(Value : TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxListBox.ControlSetValue(Value : variant);
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

function TJvDynControlCxListBox.ControlGetValue : variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlCxListBox.ControlSetItems(Value : TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlCxListBox.ControlGetItems : TStrings;
begin
  Result := Items;
end;

function TJvDynControlCxListBox.ControlValidateData(var Value : variant; var ErrorMessage : string) : boolean;
begin
  Result := true;
end;

procedure TJvDynControlCxListBox.ControlSetOnDblClick(Value : TNotifyEvent);
begin
  OnDblClick := Value;
end;

procedure TJvDynControlCxListBox.ControlSetcxProperties(Value : TcxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
end;


 //****************************************************************************
 // TJvDynControlCxComboBox
 //****************************************************************************

procedure TJvDynControlCxComboBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxComboBox.ControlSetReadOnly(Value : boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxComboBox.ControlSetCaption(Value : string);
begin
end;

procedure TJvDynControlCxComboBox.ControlSetTabOrder(Value : integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxComboBox.ControlSetOnEnter(Value : TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxComboBox.ControlSetOnExit(Value : TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxComboBox.ControlSetOnChange(Value : TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxComboBox.ControlSetOnClick(Value : TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxComboBox.ControlSetValue(Value : variant);
begin
  Text := Value;
end;

function TJvDynControlCxComboBox.ControlGetValue : variant;
begin
  Result := Text;
end;

procedure TJvDynControlCxComboBox.ControlSetItems(Value : TStrings);
begin
  Properties.Items.Assign(Value);
end;

function TJvDynControlCxComboBox.ControlGetItems : TStrings;
begin
  Result := Properties.Items;
end;

function TJvDynControlCxComboBox.ControlValidateData(var Value : variant; var ErrorMessage : string) : boolean;
begin
  Result := true;
end;

procedure TJvDynControlCxComboBox.ControlSetcxProperties(Value : TcxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
end;



 //****************************************************************************
 // TJvDynControlCxPanel
 //****************************************************************************

procedure TJvDynControlCxPanel.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxPanel.ControlSetCaption(Value : string);
begin
  Caption := Value;
end;

procedure TJvDynControlCxPanel.ControlSetTabOrder(Value : integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxPanel.ControlSetOnEnter(Value : TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxPanel.ControlSetOnExit(Value : TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxPanel.ControlSetOnClick(Value : TNotifyEvent);
begin
end;

procedure TJvDynControlCxPanel.ControlSetBorderWidth(Value : integer);
begin
  BorderWidth := Value;
end;

 //****************************************************************************
 // TJvDynControlCxScrollbox
 //****************************************************************************

procedure TJvDynControlCxScrollbox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxScrollbox.ControlSetCaption(Value : string);
begin
  Caption := Value;
end;

procedure TJvDynControlCxScrollbox.ControlSetTabOrder(Value : integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxScrollbox.ControlSetOnEnter(Value : TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxScrollbox.ControlSetOnExit(Value : TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxScrollbox.ControlSetOnClick(Value : TNotifyEvent);
begin
end;

 //****************************************************************************
 // TJvDynControlCxLabel                      
 //****************************************************************************

procedure TJvDynControlCxLabel.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxLabel.ControlSetCaption(Value : string);
begin
  Caption := Value;
end;

procedure TJvDynControlCxLabel.ControlSetTabOrder(Value : integer);
begin
end;

procedure TJvDynControlCxLabel.ControlSetOnEnter(Value : TNotifyEvent);
begin
end;

procedure TJvDynControlCxLabel.ControlSetOnExit(Value : TNotifyEvent);
begin
end;

procedure TJvDynControlCxLabel.ControlSetOnClick(Value : TNotifyEvent);
begin
end;

procedure TJvDynControlCxLabel.ControlSetFocusControl(Value : tWinControl);
begin
  FocusControl := Value;
end;

procedure TJvDynControlCxLabel.ControlSetcxProperties(Value : TcxDynControlWrapper);
begin
//  LookAndFeel.Assign (Value.LookandFeel);
end;

 //****************************************************************************
 // TJvDynControlCxStaticText
 //****************************************************************************

procedure TJvDynControlCxStaticText.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxStaticText.ControlSetCaption(Value : string);
begin
  Caption := Value;
end;

procedure TJvDynControlCxStaticText.ControlSetTabOrder(Value : integer);
begin
end;

procedure TJvDynControlCxStaticText.ControlSetOnEnter(Value : TNotifyEvent);
begin
end;

procedure TJvDynControlCxStaticText.ControlSetOnExit(Value : TNotifyEvent);
begin
end;

procedure TJvDynControlCxStaticText.ControlSetOnClick(Value : TNotifyEvent);
begin
end;


procedure TJvDynControlCxStaticText.ControlSetcxProperties(Value : TcxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
end;

 //****************************************************************************
 // TJvDynControlCxButton                      
 //****************************************************************************

procedure TJvDynControlCxButton.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxButton.ControlSetCaption(Value : string);
begin
  Caption := Value;
end;

procedure TJvDynControlCxButton.ControlSetTabOrder(Value : integer);
begin
end;

procedure TJvDynControlCxButton.ControlSetOnEnter(Value : TNotifyEvent);
begin
end;

procedure TJvDynControlCxButton.ControlSetOnExit(Value : TNotifyEvent);
begin
end;

procedure TJvDynControlCxButton.ControlSetOnClick(Value : TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxButton.ControlSetGlyph(Value : tBitmap);
begin
  Glyph.Assign(Value);
end;

procedure TJvDynControlCxButton.ControlSetNumGlyphs(Value : integer);
begin
  NumGlyphs := Value;
end;

procedure TJvDynControlCxButton.ControlSetLayout(Value : TButtonLayout);
begin
  Layout := Value;
end;

procedure TJvDynControlCxButton.ControlSetcxProperties(Value : TcxDynControlWrapper);
begin
  LookAndFeel.Assign(Value.LookAndFeel);
end;

 //****************************************************************************
 // tJvDynControlEngine_DevExpCx
 //****************************************************************************

constructor tJvDynControlEngine_DevExpCx.Create;
begin
  inherited Create;
  fcxProperties := TcxDynControlWrapper.Create;
end;

destructor tJvDynControlEngine_DevExpCx.Destroy;
begin
  FreeAndNil(fcxProperties);
  inherited Destroy;
end;

procedure tJvDynControlEngine_DevExpCx.SetcxProperties(Value : TcxDynControlWrapper);
begin
  if not (Value is TcxDynControlWrapper) then
    Exit;
  fcxProperties.LookAndFeel.Assign(Value.LookAndFeel);
end;

function tJvDynControlEngine_DevExpCx.CreateControlClass(aControlClass : tControlClass; aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tControl;
var
  cx : IJvDynControlDevExpCx;
  c :  TControl;
begin
  c := inherited CreateControlClass(aControlClass, aOwner, aParentControl, aControlName);
  if Supports(c, IJvDynControlDevExpCx) then
    with c as IJvDynControlDevExpCx do
      ControlSetcxProperties(cxProperties);
  Result := c;
//    cx.ControlSetLookAndFeel(LookAndFeel);
end;

 //****************************************************************************
 // DynControlEngine_DevExpCx
 //****************************************************************************


function DynControlEngine_DevExpCx : tJvDynControlEngine_DevExpCx;
begin
  Result := IntDynControlEngine_DevExpCx;
end;

procedure SetDynControlEngine_DevExpCxDefault;
begin
  SetDefaultDynControlEngine(IntDynControlEngine_DevExpCx);
end;

initialization
  IntDynControlEngine_DevExpCx := tJvDynControlEngine_DevExpCx.Create;
  IntDynControlEngine_DevExpCx.RegisterControl(jctLabel, TJvDynControlCxLabel);
  IntDynControlEngine_DevExpCx.RegisterControl(jctStaticText, TJvDynControlCxStaticText);
  IntDynControlEngine_DevExpCx.RegisterControl(jctButton, TJvDynControlCxButton);
  IntDynControlEngine_DevExpCx.RegisterControl(jctScrollbox, TJvDynControlCxScrollbox);
  IntDynControlEngine_DevExpCx.RegisterControl(jctPanel, TJvDynControlCxPanel);
  IntDynControlEngine_DevExpCx.RegisterControl(jctCheckBox, TJvDynControlCxCheckBox);
  IntDynControlEngine_DevExpCx.RegisterControl(jctComboBox, TJvDynControlCxComboBox);
  IntDynControlEngine_DevExpCx.RegisterControl(jctListBox, TJvDynControlCxListBox);
  IntDynControlEngine_DevExpCx.RegisterControl(jctRadioGroup, TJvDynControlCxRadioGroup);
  IntDynControlEngine_DevExpCx.RegisterControl(jctDateTimeEdit, TJvDynControlCxDateTimeEdit);
  IntDynControlEngine_DevExpCx.RegisterControl(jctTimeEdit, TJvDynControlCxTimeEdit);
  IntDynControlEngine_DevExpCx.RegisterControl(jctDateEdit, TJvDynControlCxDateEdit);
  IntDynControlEngine_DevExpCx.RegisterControl(jctEdit, TJvDynControlCxMaskEdit);
  IntDynControlEngine_DevExpCx.RegisterControl(jctIntegerEdit, TJvDynControlCxMaskEdit);
  IntDynControlEngine_DevExpCx.RegisterControl(jctDoubleEdit, TJvDynControlCxMaskEdit);
  IntDynControlEngine_DevExpCx.RegisterControl(jctDirectoryEdit, TJvDynControlCxMaskEdit);
  IntDynControlEngine_DevExpCx.RegisterControl(jctFileNameEdit, TJvDynControlCxMaskEdit);
  IntDynControlEngine_DevExpCx.RegisterControl(jctMemo, TJvDynControlCxMemo);
  SetDefaultDynControlEngine(IntDynControlEngine_DevExpCx);

finalization
  FreeAndNil(IntDynControlEngine_DevExpCx);
end.
