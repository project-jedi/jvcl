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

uses
  Classes, Controls, StdCtrls, ExtCtrls, ComCtrls, Mask, Forms, Graphics,
  Buttons, Dialogs,
  cxLookAndFeels, cxMaskEdit, cxLabel, cxButtons, cxListBox, cxDropDownEdit,
  cxButtonEdit, cxCalendar, cxCheckBox, cxMemo, cxRadioGroup,
  JvDynControlEngine, JvDynControlEngine_Interface;

type
  TcxDynControlWrapper = class(TPersistent)
  private
    FLookAndFeel: TcxLookAndFeel;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property LookAndFeel: TcxLookAndFeel read FLookAndFeel write FLookAndFeel;
  end;

  IJvDynControlDevExpCx = interface
    ['{247D29CD-ABA4-4F87-A25D-4987BD950F0C}']
    procedure ControlSetCxProperties(Value: TcxDynControlWrapper);
  end;

  TJvDynControlCxMaskEdit = class(TcxMaskEdit, IJvDynControl, IJvDynControlData, IJvDynControlDevExpCx)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    function ControlValidateData(var Value: Variant; var ErrorMessage: string): Boolean;

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetCxProperties(Value: TcxDynControlWrapper);
  end;

  TJvDynControlCxFileNameEdit = class(TcxButtonEdit, IJvDynControl,
    IJvDynControlData, IJvDynControlDevExpCx)
  public
    procedure OnButtonClick(Sender: TObject; AButtonIndex: Integer);

    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    function ControlValidateData(var Value: Variant; var ErrorMessage: string): Boolean;

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetCxProperties(Value: TcxDynControlWrapper);
  end;

  TJvDynControlCxDirectoryEdit = class(TcxButtonEdit, IJvDynControl,
    IJvDynControlData, IJvDynControlDevExpCx)
  public
    procedure OnButtonClick(Sender: TObject; AButtonIndex: Integer);

    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    function ControlValidateData(var Value: Variant; var ErrorMessage: string): Boolean;

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetCxProperties(Value: TcxDynControlWrapper);
  end;

  TJvDynControlCxDateTimeEdit = class(TcxDateEdit, IJvDynControl,
    IJvDynControlData, IJvDynControlDevExpCx)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    function ControlValidateData(var Value: Variant; var ErrorMessage: string): Boolean;

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetCxProperties(Value: TcxDynControlWrapper);
  end;

  TJvDynControlCxDateEdit = class(TJvDynControlCxDateTimeEdit)
  public
    procedure ControlSetDefaultProperties;
  end;

  TJvDynControlCxTimeEdit = class(TJvDynControlCxDateTimeEdit)
  public
    procedure ControlSetDefaultProperties;
  end;

  TJvDynControlCxCheckbox = class(TcxCheckBox, IJvDynControl,
    IJvDynControlData, IJvDynControlDevExpCx)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    function ControlValidateData(var Value: Variant; var ErrorMessage: string): Boolean;

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetCxProperties(Value: TcxDynControlWrapper);
  end;

  TJvDynControlCxMemo = class(TcxMemo, IJvDynControl, IJvDynControlData,
    IJvDynControlItems, IJvDynControlMemo, IJvDynControlDevExpCx)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    function ControlValidateData(var Value: Variant; var ErrorMessage: string): Boolean;

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetWantTabs(Value: Boolean);
    procedure ControlSetWantReturns(Value: Boolean);
    procedure ControlSetWordWrap(Value: Boolean);
    procedure ControlSetScrollBars(Value: TScrollStyle);

    procedure ControlSetCxProperties(Value: TcxDynControlWrapper);
  end;

  TJvDynControlCxRadioGroup = class(TcxRadioGroup, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlDevExpCx,
    IJvDynControlRadioGroup)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    function ControlValidateData(var Value: Variant; var ErrorMessage: string): Boolean;

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetCxProperties(Value: TcxDynControlWrapper);

    procedure ControlSetColumns(Value: Integer);
  end;

  TJvDynControlCxListBox = class(TcxListBox, IJvDynControl, IJvDynControlData,
    IJvDynControlItems, IJvDynControlDblClick, IJvDynControlDevExpCx)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    function ControlValidateData(var Value: Variant; var ErrorMessage: string): Boolean;

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetOnDblClick(Value: TNotifyEvent);

    procedure ControlSetCxProperties(Value: TcxDynControlWrapper);
  end;

  TJvDynControlCxComboBox = class(TcxComboBox, IJvDynControl, IJvDynControlData,
    IJvDynControlItems, IJvDynControlDevExpCx, IJvDynControlComboBox)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    function ControlValidateData(var Value: Variant; var ErrorMessage: string): Boolean;

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetCxProperties(Value: TcxDynControlWrapper);

    procedure ControlSetNewEntriesAllowed(Value: Boolean);
  end;

  TJvDynControlCxPanel = class(TPanel, IJvDynControl, IJvDynControlPanel)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetBorder(ABevelInner: TPanelBevel; ABevelOuter: TPanelBevel;
      ABevelWidth: Integer; ABorderStyle: TBorderStyle; ABorderWidth: Integer);
  end;

  TJvDynControlCxScrollBox = class(TScrollbox, IJvDynControl)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

  end;

  TJvDynControlCxLabel = class(TLabel, IJvDynControl, IJvDynControlLabel,
    IJvDynControlDevExpCx)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetFocusControl(Value: TWinControl);

    procedure ControlSetCxProperties(Value: TcxDynControlWrapper);
  end;

  TJvDynControlCxStaticText = class(TcxLabel, IJvDynControl, IJvDynControlDevExpCx)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetCxProperties(Value: TcxDynControlWrapper);
  end;

  TJvDynControlCxButton = class(TcxButton, IJvDynControl, IJvDynControlButton,
    IJvDynControlDevExpCx)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetGlyph(Value: TBitmap);
    procedure ControlSetNumGlyphs(Value: Integer);
    procedure ControlSetLayout(Value: TButtonLayout);

    procedure ControlSetCxProperties(Value: TcxDynControlWrapper);
  end;

  TJvDynControlEngine_DevExpCx = class(TJvDynControlEngine)
  private
    FcxProperties: TcxDynControlWrapper;
  protected
    procedure SetcxProperties(Value: TcxDynControlWrapper);
  public
    constructor Create; override;
    destructor Destroy; override;
    function CreateControlClass(AControlClass: TControlClass; AOwner: TComponent;
      AParentControl: TWinControl; AControlName: string): TControl; override;
  published
    property cxProperties: TcxDynControlWrapper read FcxProperties write FcxProperties;
  end;

function DynControlEngine_DevExpCx: TJvDynControlEngine_DevExpCx;
procedure SetDynControlEngine_DevExpCxDefault;

implementation

uses
  SysUtils,
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  cxControls;

var
  IntDynControlEngine_DevExpCx: TJvDynControlEngine_DevExpCx = nil;

//=== TcxDynControlWrapper ===================================================

constructor TcxDynControlWrapper.Create;
begin
  inherited Create;
  FLookAndFeel := TcxLookAndFeel.Create(nil);
end;

destructor TcxDynControlWrapper.Destroy;
begin
  FreeAndNil(FLookAndFeel);
  inherited Destroy;
end;

//=== TJvDynControlCxMaskEdit ================================================

procedure TJvDynControlCxMaskEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxMaskEdit.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxMaskEdit.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlCxMaskEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxMaskEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxMaskEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxMaskEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxMaskEdit.ControlSetOnClick(Value: TNotifyEvent);
begin

end;

procedure TJvDynControlCxMaskEdit.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlCxMaskEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

function TJvDynControlCxMaskEdit.ControlValidateData(var Value: Variant;
  var ErrorMessage: string): Boolean;
begin
  Result := True;
end;

procedure TJvDynControlCxMaskEdit.ControlSetCxProperties(Value: TcxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
end;

//=== TJvDynControlCxFileNameEdit ============================================

procedure TJvDynControlCxFileNameEdit.OnButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  with TOpenDialog.Create(Self) do
    try
      FileName := ControlGetValue;
      if Execute then
        ControlSetValue(FileName);
    finally
      Free;
    end;
end;

procedure TJvDynControlCxFileNameEdit.ControlSetDefaultProperties;
begin
  Properties.OnButtonClick := OnButtonClick;
end;

procedure TJvDynControlCxFileNameEdit.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxFileNameEdit.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlCxFileNameEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxFileNameEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxFileNameEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxFileNameEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxFileNameEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxFileNameEdit.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlCxFileNameEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

function TJvDynControlCxFileNameEdit.ControlValidateData(var Value: Variant;
  var ErrorMessage: string): Boolean;
begin
  Result := True;
end;

procedure TJvDynControlCxFileNameEdit.ControlSetCxProperties(Value: TcxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
end;

//=== TJvDynControlCxDirectoryEdit============================================

procedure TJvDynControlCxDirectoryEdit.OnButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
end;

procedure TJvDynControlCxDirectoryEdit.ControlSetDefaultProperties;
begin
  Properties.OnButtonClick := OnButtonClick;
 //  Properties.Root.BrowseFolder := bfCustomPath;
 //  Properties.ShowFullPath := True;
end;

procedure TJvDynControlCxDirectoryEdit.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxDirectoryEdit.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlCxDirectoryEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxDirectoryEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxDirectoryEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxDirectoryEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxDirectoryEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxDirectoryEdit.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlCxDirectoryEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

function TJvDynControlCxDirectoryEdit.ControlValidateData(var Value: Variant;
  var ErrorMessage: string): Boolean;
begin
  Result := True;
end;

procedure TJvDynControlCxDirectoryEdit.ControlSetCxProperties(Value: TcxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
end;

//=== TJvDynControlCxDateTimeEdit ============================================

procedure TJvDynControlCxDateTimeEdit.ControlSetDefaultProperties;
begin
  Properties.ShowTime := True;
  Properties.SaveTime := True;
  Properties.InputKind := ikStandard;
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlCxDateTimeEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

function TJvDynControlCxDateTimeEdit.ControlValidateData(var Value: Variant;
  var ErrorMessage: string): Boolean;
begin
  Result := True;
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetCxProperties(Value: TcxDynControlWrapper);
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

//=== TJvDynControlCxCheckBox ================================================

procedure TJvDynControlCxCheckBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxCheckbox.ControlSetReadOnly(Value: Boolean);
begin
end;

procedure TJvDynControlCxCheckbox.ControlSetCaption(Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlCxCheckbox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxCheckbox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxCheckbox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxCheckbox.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxCheckbox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxCheckbox.ControlSetValue(Value: Variant);
begin
  if VarType(Value) = varBoolean then
    Checked := Value
  else
    Checked := UpperCase(Value) = 'TRUE';
end;

function TJvDynControlCxCheckbox.ControlGetValue: Variant;
begin
  Result := Checked;
end;

function TJvDynControlCxCheckbox.ControlValidateData(var Value: Variant;
  var ErrorMessage: string): Boolean;
begin
  Result := True;
end;

procedure TJvDynControlCxCheckbox.ControlSetCxProperties(Value: TcxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
end;

//=== TJvDynControlCxMemo ====================================================

procedure TJvDynControlCxMemo.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxMemo.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxMemo.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlCxMemo.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxMemo.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxMemo.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxMemo.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxMemo.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxMemo.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlCxMemo.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlCxMemo.ControlSetSorted(Value: Boolean);
begin
end;

procedure TJvDynControlCxMemo.ControlSetItems(Value: TStrings);
begin
  Lines.Assign(Value);
end;

function TJvDynControlCxMemo.ControlGetItems: TStrings;
begin
  Result := Lines;
end;

function TJvDynControlCxMemo.ControlValidateData(var Value: Variant;
  var ErrorMessage: string): Boolean;
begin
  Result := True;
end;

procedure TJvDynControlCxMemo.ControlSetWantTabs(Value: Boolean);
begin
  Properties.WantTabs := Value;
end;

procedure TJvDynControlCxMemo.ControlSetWantReturns(Value: Boolean);
begin
  Properties.WantReturns := Value;
end;

procedure TJvDynControlCxMemo.ControlSetWordWrap(Value: Boolean);
begin
  Properties.WordWrap := Value;
end;

procedure TJvDynControlCxMemo.ControlSetScrollBars(Value: TScrollStyle);
begin
  Properties.ScrollBars := Value;
end;

procedure TJvDynControlCxMemo.ControlSetCxProperties(Value: TcxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
end;

//=== TJvDynControlCxRadioGroup ==============================================

procedure TJvDynControlCxRadioGroup.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxRadioGroup.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxRadioGroup.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlCxRadioGroup.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxRadioGroup.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxRadioGroup.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxRadioGroup.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxRadioGroup.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxRadioGroup.ControlSetValue(Value: Variant);
begin
  if VarType(Value) in [varSmallint, varInteger] then
    ItemIndex := Value
  else
  try
    ItemIndex := Value
  except
    // on E: Exception do
      // ItemIndex := Properties.Items. IndexOf(Value);
  end;
end;

function TJvDynControlCxRadioGroup.ControlGetValue: Variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlCxRadioGroup.ControlSetSorted(Value: Boolean);
begin
end;

procedure TJvDynControlCxRadioGroup.ControlSetItems(Value: TStrings);
var
  I: Integer;
  Item: TcxRadioGroupItem;
begin
  Properties.Items.Clear;
  for I := 0 to Value.Count - 1 do
  begin
    Item := TcxRadioGroupItem(Properties.Items.Add);
    Item.Caption := Value[I];
  end;
end;

function TJvDynControlCxRadioGroup.ControlGetItems: TStrings;
begin
//  Result := TStrings(Properties.Items);
end;

function TJvDynControlCxRadioGroup.ControlValidateData(var Value: Variant;
  var ErrorMessage: string): Boolean;
begin
  Result := True;
end;

procedure TJvDynControlCxRadioGroup.ControlSetCxProperties(Value: TcxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
end;

procedure TJvDynControlCxRadioGroup.ControlSetColumns(Value: Integer);
begin
  Properties.Columns := Value;
end;

//=== TJvDynControlCxListBox =================================================

procedure TJvDynControlCxListBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxListBox.ControlSetReadOnly(Value: Boolean);
begin
//  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxListBox.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlCxListBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxListBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxListBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxListBox.ControlSetOnChange(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxListBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxListBox.ControlSetValue(Value: Variant);
begin
  if VarType(Value) in [varSmallint, varInteger] then
    ItemIndex := Value
  else
  try
    ItemIndex := Value
  except
    on E: Exception do
      ItemIndex := Items.IndexOf(Value);
  end;
end;

function TJvDynControlCxListBox.ControlGetValue: Variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlCxListBox.ControlSetSorted(Value: Boolean);
begin
  Sorted := Value;
end;

procedure TJvDynControlCxListBox.ControlSetItems(Value: TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlCxListBox.ControlGetItems: TStrings;
begin
  Result := Items;
end;

function TJvDynControlCxListBox.ControlValidateData(var Value: Variant;
  var ErrorMessage: string): Boolean;
begin
  Result := True;
end;

procedure TJvDynControlCxListBox.ControlSetOnDblClick(Value: TNotifyEvent);
begin
  OnDblClick := Value;
end;

procedure TJvDynControlCxListBox.ControlSetCxProperties(Value: TcxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
end;

//=== TJvDynControlCxComboBox ================================================

procedure TJvDynControlCxComboBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxComboBox.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxComboBox.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlCxComboBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxComboBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxComboBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxComboBox.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxComboBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxComboBox.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlCxComboBox.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlCxComboBox.ControlSetSorted(Value: Boolean);
begin
  Properties.Sorted := Value;
end;

procedure TJvDynControlCxComboBox.ControlSetItems(Value: TStrings);
begin
  Properties.Items.Assign(Value);
end;

function TJvDynControlCxComboBox.ControlGetItems: TStrings;
begin
  Result := Properties.Items;
end;

function TJvDynControlCxComboBox.ControlValidateData(var Value: Variant;
  var ErrorMessage: string): Boolean;
begin
  Result := True;
end;

procedure TJvDynControlCxComboBox.ControlSetCxProperties(Value: TcxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
end;

procedure TJvDynControlCxComboBox.ControlSetNewEntriesAllowed(Value: Boolean);
begin
  if Value then
    Properties.DropDownListStyle := lsEditList
  else
    Properties.DropDownListStyle := lsEditFixedList;
end;

//=== TJvDynControlCxPanel ===================================================

procedure TJvDynControlCxPanel.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxPanel.ControlSetCaption(Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlCxPanel.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxPanel.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxPanel.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxPanel.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxPanel.ControlSetBorder(ABevelInner: TPanelBevel;
  ABevelOuter: TPanelBevel; ABevelWidth: Integer; ABorderStyle: TBorderStyle;
  ABorderWidth: Integer);
begin
  BorderWidth := ABorderWidth;
  BorderStyle := ABorderStyle;
  BevelInner := ABevelInner;
  BevelOuter := ABevelOuter;
  BevelWidth := ABevelWidth;
end;

//=== TJvDynControlCxScrollBox ===============================================

procedure TJvDynControlCxScrollBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxScrollBox.ControlSetCaption(Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlCxScrollBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxScrollBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxScrollBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxScrollBox.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

//=== TJvDynControlCxLabel ===================================================

procedure TJvDynControlCxLabel.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxLabel.ControlSetCaption(Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlCxLabel.ControlSetTabOrder(Value: Integer);
begin
end;

procedure TJvDynControlCxLabel.ControlSetOnEnter(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxLabel.ControlSetOnExit(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxLabel.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxLabel.ControlSetFocusControl(Value: TWinControl);
begin
  FocusControl := Value;
end;

procedure TJvDynControlCxLabel.ControlSetCxProperties(Value: TcxDynControlWrapper);
begin
//  LookAndFeel.Assign(Value.LookandFeel);
end;

//=== TJvDynControlCxStaticText ==============================================

procedure TJvDynControlCxStaticText.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxStaticText.ControlSetCaption(Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlCxStaticText.ControlSetTabOrder(Value: Integer);
begin
end;

procedure TJvDynControlCxStaticText.ControlSetOnEnter(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxStaticText.ControlSetOnExit(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxStaticText.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxStaticText.ControlSetCxProperties(Value: TcxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
end;

//=== TJvDynControlCxButton ==================================================

procedure TJvDynControlCxButton.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxButton.ControlSetCaption(Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlCxButton.ControlSetTabOrder(Value: Integer);
begin
end;

procedure TJvDynControlCxButton.ControlSetOnEnter(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxButton.ControlSetOnExit(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxButton.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxButton.ControlSetGlyph(Value: TBitmap);
begin
  Glyph.Assign(Value);
end;

procedure TJvDynControlCxButton.ControlSetNumGlyphs(Value: Integer);
begin
  NumGlyphs := Value;
end;

procedure TJvDynControlCxButton.ControlSetLayout(Value: TButtonLayout);
begin
  Layout := Value;
end;

procedure TJvDynControlCxButton.ControlSetCxProperties(Value: TcxDynControlWrapper);
begin
  LookAndFeel.Assign(Value.LookAndFeel);
end;

//=== TJvDynControlEngine_DevExpCx ===========================================

constructor TJvDynControlEngine_DevExpCx.Create;
begin
  inherited Create;
  FcxProperties := TcxDynControlWrapper.Create;
end;

destructor TJvDynControlEngine_DevExpCx.Destroy;
begin
  FreeAndNil(FcxProperties);
  inherited Destroy;
end;

procedure TJvDynControlEngine_DevExpCx.SetcxProperties(Value: TcxDynControlWrapper);
begin
  if Value is TcxDynControlWrapper then
    FcxProperties.LookAndFeel.Assign(Value.LookAndFeel);
end;

function TJvDynControlEngine_DevExpCx.CreateControlClass(AControlClass: TControlClass;
  AOwner: TComponent; AParentControl: TWinControl; AControlName: string): TControl;
var
  //cx: IJvDynControlDevExpCx;
  C: TControl;
begin
  C := inherited CreateControlClass(AControlClass, AOwner, AParentControl, AControlName);
  if Supports(C, IJvDynControlDevExpCx) then
    with C as IJvDynControlDevExpCx do
      ControlSetCxProperties(cxProperties);
  Result := C;
//    cx.ControlSetLookAndFeel(LookAndFeel);
end;

//=== DynControlEngine_DevExpCx ==============================================

function DynControlEngine_DevExpCx: TJvDynControlEngine_DevExpCx;
begin
  Result := IntDynControlEngine_DevExpCx;
end;

procedure SetDynControlEngine_DevExpCxDefault;
begin
  SetDefaultDynControlEngine(IntDynControlEngine_DevExpCx);
end;

initialization
  IntDynControlEngine_DevExpCx := TJvDynControlEngine_DevExpCx.Create;
  IntDynControlEngine_DevExpCx.RegisterControl(jctLabel, TJvDynControlCxLabel);
  IntDynControlEngine_DevExpCx.RegisterControl(jctStaticText, TJvDynControlCxStaticText);
  IntDynControlEngine_DevExpCx.RegisterControl(jctButton, TJvDynControlCxButton);
  IntDynControlEngine_DevExpCx.RegisterControl(jctScrollBox, TJvDynControlCxScrollBox);
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
  IntDynControlEngine_DevExpCx.RegisterControl(jctDirectoryEdit, TJvDynControlCxDirectoryEdit);
  IntDynControlEngine_DevExpCx.RegisterControl(jctFileNameEdit, TJvDynControlCxFileNameEdit);
  IntDynControlEngine_DevExpCx.RegisterControl(jctMemo, TJvDynControlCxMemo);
  SetDefaultDynControlEngine(IntDynControlEngine_DevExpCx);

finalization
  FreeAndNil(IntDynControlEngine_DevExpCx);

end.

