{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Jens Fudickar [jens.fudickar@oratool.de]
All Rights Reserved.

Contributor(s):
Jens Fudickar [jens.fudickar@oratool.de]

Last Modified: 2003-11-03

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDynControlEngine_VCL;

interface

uses
  Classes, Controls, StdCtrls, ExtCtrls, ComCtrls, Mask, Forms, Graphics,
  Buttons, Dialogs, FileCtrl,
  JvDynControlEngine, JvDynControlEngine_Interface;

type
  TJvDynControlVCLMaskEdit = class(TMaskEdit, IUnknown, IJvDynControl, IJvDynControlData)
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
  end;

  TJvDynControlVCLFileNameEdit = class(TPanel, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlFileName)
  private
    FEditControl: TMaskEdit;
    FButton: TBitBtn;
    FInitialDir: string;
    FFilterIndex: Integer;
    FFilter: string;
    FDialogOptions: TOpenOptions;
    FDialogKind: TJvDynControlFileNameDialogKind;
    FDialogTitle: string;
    FDefaultExt: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DefaultOnButtonClick(Sender: TObject);

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

    // IJvDynControlFileName
    procedure ControlSetInitialDir(Value: string);
    procedure ControlSetDefaultExt(Value: string);
    procedure ControlSetDialogTitle(Value: string);
    procedure ControlSetDialogOptions(Value: TOpenOptions);
    procedure ControlSetFilter(Value: string);
    procedure ControlSetFilterIndex(Value: Integer);
    procedure ControlSetDialogKind(Value: TJvDynControlFileNameDialogKind);

  end;

  TJvDynControlVCLDirectoryEdit = class(TPanel, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlDirectory)
  private
    FEditControl: TMaskEdit;
    FButton: TBitBtn;
    FInitialDir: string;
    FDialogOptions: TSelectDirOpts;
    FDialogTitle: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DefaultOnButtonClick(Sender: TObject);

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

    // IJvDynControlDirectory
    procedure ControlSetInitialDir(Value: string);
    procedure ControlSetDialogTitle(Value: string);
    procedure ControlSetDialogOptions(Value: TSelectDirOpts);
  end;

  TJvDynControlVCLDateTimeEdit = class(TDateTimePicker, IUnknown,
    IJvDynControl, IJvDynControlData)
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
  end;

  TJvDynControlVCLDateEdit = class (TJvDynControlVCLDateTimeEdit)
  public
    procedure ControlSetDefaultProperties;
  end;

  TJvDynControlVCLTimeEdit = class (TJvDynControlVCLDateTimeEdit)
  public
    procedure ControlSetDefaultProperties;
  end;

  TJvDynControlVCLCheckBox = class(TCheckBox, IUnknown, IJvDynControl,
    IJvDynControlData)
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
  end;

  TJvDynControlVCLMemo = class(TMemo, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlMemo)
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
  end;

  TJvDynControlVCLRadioGroup = class(TRadioGroup, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlRadioGroup)
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

    procedure ControlSetColumns(Value: Integer);
  end;

  TJvDynControlVCLListBox = class(TListBox, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlDblClick)
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
  end;

  TJvDynControlVCLComboBox = class(TComboBox, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlComboBox)
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

    procedure ControlSetNewEntriesAllowed(Value: Boolean);
  end;

  TJvDynControlVCLPanel = class(TPanel, IUnknown, IJvDynControl, IJvDynControlPanel)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetBorder(ABevelInner: TPanelBevel;
      ABevelOuter: TPanelBevel; ABevelWidth: Integer;
      ABorderStyle: TBorderStyle; ABorderWidth: Integer);
  end;

  TJvDynControlVCLImage = class(TImage, IUnknown, IJvDynControl, IJvDynControlImage)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetAutoSize(Value: Boolean);
    procedure ControlSetIncrementalDisplay(Value: Boolean);
    procedure ControlSetCenter(Value: Boolean);
    procedure ControlSetStretch(Value: Boolean);
    procedure ControlSetTransparent(Value: Boolean);
    procedure ControlSetPicture(Value: TPicture);
    function ControlGetPicture: TPicture;
  end;

  TJvDynControlVCLScrollBox = class(TScrollbox, IUnknown, IJvDynControl)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
  end;

  TJvDynControlVCLLabel = class(TLabel, IUnknown, IJvDynControl, IJvDynControlLabel)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetFocusControl(Value: TWinControl);
  end;

  TJvDynControlVCLStaticText = class(TStaticText, IUnknown, IJvDynControl)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
  end;

  TJvDynControlVCLButton = class(TBitBtn, IUnknown, IJvDynControl,
    IJvDynControlButton)
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
  end;

function DynControlEngine_VCL: TJvDynControlEngine;

implementation

uses
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  SysUtils, ExtDlgs;

var
  IntDynControlEngine_VCL: TJvDynControlEngine = nil;

//=== TJvDynControlVCLMaskEdit ===============================================

procedure TJvDynControlVCLMaskEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLMaskEdit.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlVCLMaskEdit.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlVCLMaskEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLMaskEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLMaskEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLMaskEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlVCLMaskEdit.ControlSetOnClick(Value: TNotifyEvent);
begin

end;

procedure TJvDynControlVCLMaskEdit.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlVCLMaskEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

function TJvDynControlVCLMaskEdit.ControlValidateData(var Value: Variant;
  var ErrorMessage: string): Boolean;
begin
  Result := True;
end;

//=== TJvDynControlVCLFileNameEdit ===========================================

constructor TJvDynControlVCLFileNameEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TBitBtn.Create(AOwner);
  FButton.Parent := Self;
  FButton.Align := alRight;
  FButton.OnClick := DefaultOnButtonClick;
  FButton.Caption := '...';
  FEditControl := TMaskEdit.Create(AOwner);
  FEditControl.Parent := Self;
  Height := FEditControl.Height;
  FButton.Width := Height;
  FEditControl.Align := alClient;
  BevelInner  := bvNone;
  BevelOuter  := bvNone;
  FDialogKind := jdkOpen;
end;

destructor TJvDynControlVCLFileNameEdit.Destroy;
begin
  FreeAndNil(FEditControl);
  FreeAndNil(FButton);
  inherited Destroy;
end;

procedure TJvDynControlVCLFileNameEdit.DefaultOnButtonClick(Sender: TObject);
begin
  case FDialogKind of
    jdkOpen:
      with TOpenDialog.Create(Self) do
        try
          Options := FDialogOptions;
          Title := FDialogTitle;
          Filter := FFilter;
          FilterIndex := FFilterIndex;
          InitialDir := FInitialDir;
          DefaultExt := FDefaultExt;
          FileName := ControlGetValue;
          if Execute then
            ControlSetValue(FileName);
        finally
          Free;
        end;
    jdkOpenPicture:
      with TOpenPictureDialog.Create(Self) do
        try
          Options := FDialogOptions;
          Title := FDialogTitle;
          Filter := FFilter;
          FilterIndex := FFilterIndex;
          InitialDir := FInitialDir;
          DefaultExt := FDefaultExt;
          FileName := ControlGetValue;
          if Execute then
            ControlSetValue(FileName);
        finally
          Free;
        end;
    jdkSave:
      with TSaveDialog.Create(Self) do
        try
          Options := FDialogOptions;
          Title := FDialogTitle;
          Filter := FFilter;
          FilterIndex := FFilterIndex;
          InitialDir := FInitialDir;
          DefaultExt := FDefaultExt;
          FileName := ControlGetValue;
          if Execute then
            ControlSetValue(FileName);
        finally
          Free;
        end;
    jdkSavePicture:
      with TSavePictureDialog.Create(Self) do
        try
          Options := FDialogOptions;
          Title := FDialogTitle;
          Filter := FFilter;
          FilterIndex := FFilterIndex;
          InitialDir := FInitialDir;
          DefaultExt := FDefaultExt;
          FileName := ControlGetValue;
          if Execute then
            ControlSetValue(FileName);
        finally
          Free;
        end;
  end;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetReadOnly(Value: Boolean);
begin
  FEditControl.ReadOnly := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  FEditControl.OnEnter := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  FEditControl.OnExit := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  FEditControl.OnChange := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetOnClick(Value: TNotifyEvent);
begin

end;

procedure TJvDynControlVCLFileNameEdit.ControlSetValue(Value: Variant);
begin
  FEditControl.Text := Value;
end;

function TJvDynControlVCLFileNameEdit.ControlGetValue: Variant;
begin
  Result := FEditControl.Text;
end;

    // IJvDynControlFileName
procedure TJvDynControlVCLFileNameEdit.ControlSetInitialDir(Value: string);
begin
  FInitialDir := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetDefaultExt(Value: string);
begin
  FDefaultExt := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetDialogTitle(Value: string);
begin
  FDialogTitle := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetDialogOptions(Value: TOpenOptions);
begin
  FDialogOptions := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetFilter(Value: string);
begin
  FFilter := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetFilterIndex(Value: Integer);
begin
  FFilterIndex := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetDialogKind(Value: TJvDynControlFileNameDialogKind);
begin
  FDialogKind := Value;
end;

function TJvDynControlVCLFileNameEdit.ControlValidateData(var Value: Variant;
  var ErrorMessage: string): Boolean;
begin
  Result := True;
end;

//=== TJvDynControlVCLDirectoryEdit ==========================================

constructor TJvDynControlVCLDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TBitBtn.Create(AOwner);
  FButton.Parent := Self;
  FButton.Align := alRight;
  FButton.OnClick := DefaultOnButtonClick;
  FButton.Caption := '...';
  FEditControl := TMaskEdit.Create(AOwner);
  FEditControl.Parent := Self;
  Height := FEditControl.Height;
  FButton.Width := Height;
  FEditControl.Align := alClient;
  BevelInner := bvNone;
  BevelOuter := bvNone;
end;

destructor TJvDynControlVCLDirectoryEdit.Destroy;
begin
  FreeAndNil(FEditControl);
  FreeAndNil(FButton);
  inherited Destroy;
end;

procedure TJvDynControlVCLDirectoryEdit.DefaultOnButtonClick(Sender: TObject);
var
  Dir: string;
  Opt: TSelectDirOpts;
begin
  Dir := ControlGetValue;
  if SelectDirectory(Dir, Opt, HelpContext) then
    ControlSetValue(Dir);
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetReadOnly(Value: Boolean);
begin
  FEditControl.ReadOnly := Value;
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  FEditControl.OnEnter := Value;
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  FEditControl.OnExit := Value;
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  FEditControl.OnChange := Value;
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetOnClick(Value: TNotifyEvent);
begin

end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetValue(Value: Variant);
begin
  FEditControl.Text := Value;
end;

function TJvDynControlVCLDirectoryEdit.ControlGetValue: Variant;
begin
  Result := FEditControl.Text;
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetInitialDir(Value: string);
begin
  FInitialDir := Value;
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetDialogTitle(Value: string);
begin
  FDialogTitle := Value;
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetDialogOptions(Value: TSelectDirOpts);
begin
  FDialogOptions := Value;
end;

function TJvDynControlVCLDirectoryEdit.ControlValidateData(var Value: Variant;
  var ErrorMessage: string): Boolean;
begin
  Result := True;
end;

//=== TJvDynControlVCLDateTimeEdit ===========================================

procedure TJvDynControlVCLDateTimeEdit.ControlSetDefaultProperties;
begin
  DateFormat := dfShort;
  DateMode := dmComboBox;
  Kind := dtkDate;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetReadOnly(Value: Boolean);
begin
//  ReadOnly := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetOnClick(Value: TNotifyEvent);
begin

end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlVCLDateTimeEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

function TJvDynControlVCLDateTimeEdit.ControlValidateData(var Value: Variant;
  var ErrorMessage: string): Boolean;
begin
  Result := True;
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

//=== TJvDynControlVCLCheckBox ===============================================

procedure TJvDynControlVCLCheckBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLCheckBox.ControlSetReadOnly(Value: Boolean);
begin
end;

procedure TJvDynControlVCLCheckBox.ControlSetCaption(Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlVCLCheckBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLCheckBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLCheckBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLCheckBox.ControlSetOnChange(Value: TNotifyEvent);
begin
//  OnChange := Value;
end;

procedure TJvDynControlVCLCheckBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLCheckBox.ControlSetValue(Value: Variant);
begin
  if VarType(Value) = varBoolean then
    Checked := Value
  else
    Checked := Uppercase(Value) = 'TRUE';
end;

function TJvDynControlVCLCheckBox.ControlGetValue: Variant;
begin
  Result := Checked;
end;

function TJvDynControlVCLCheckBox.ControlValidateData(var Value: Variant;
  var ErrorMessage: string): Boolean;
begin
  Result := True;
end;

//=== TJvDynControlVCLMemo ===================================================

procedure TJvDynControlVCLMemo.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLMemo.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlVCLMemo.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;


procedure TJvDynControlVCLMemo.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlVCLMemo.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlVCLMemo.ControlSetSorted(Value: Boolean);
begin
end;

procedure TJvDynControlVCLMemo.ControlSetItems(Value: TStrings);
begin
  Lines.Assign(Value);
end;

function TJvDynControlVCLMemo.ControlGetItems: TStrings;
begin
  Result := Lines;
end;

function TJvDynControlVCLMemo.ControlValidateData(var Value: Variant;
  var ErrorMessage: string): Boolean;
begin
  Result := True;
end;

procedure TJvDynControlVCLMemo.ControlSetWantTabs(Value: Boolean);
begin
  WantTabs := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetWantReturns(Value: Boolean);
begin
  WantReturns := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetWordWrap(Value: Boolean);
begin
  WordWrap := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetScrollBars(Value: TScrollStyle);
begin
  ScrollBars := Value;
end;


//=== TJvDynControlVCLRadioGroup =============================================

procedure TJvDynControlVCLRadioGroup.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLRadioGroup.ControlSetReadOnly(Value: Boolean);
begin
//  ReadOnly := Value;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlVCLRadioGroup.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetOnChange(Value: TNotifyEvent);
begin
//  OnChange := Value;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetValue(Value: Variant);
begin
  if VarType(Value) in [varSmallInt, varInteger] then
    ItemIndex := Value
  else
    try
      ItemIndex := Value
    except
      on E: Exception do
        ItemIndex := Items.IndexOf(Value);
    end;
end;

function TJvDynControlVCLRadioGroup.ControlGetValue: Variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetSorted(Value: Boolean);
begin
end;

procedure TJvDynControlVCLRadioGroup.ControlSetItems(Value: TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlVCLRadioGroup.ControlGetItems: TStrings;
begin
  Result := Items;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetColumns(Value: Integer);
begin
  Columns := Value;
end;

function TJvDynControlVCLRadioGroup.ControlValidateData(var Value: Variant;
  var ErrorMessage: string): Boolean;
begin
  Result := True;
end;

//=== TJvDynControlVCLListBox ================================================

procedure TJvDynControlVCLListBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLListBox.ControlSetReadOnly(Value: Boolean);
begin
//  ReadOnly := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlVCLListBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetOnChange(Value: TNotifyEvent);
begin
//  OnChange := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetValue(Value: Variant);
begin
  if VarType(Value) in [varSmallInt, varInteger] then
    ItemIndex := Value
  else
    try
      ItemIndex := Value
    except
      on E: Exception do
        ItemIndex := Items.IndexOf(Value);
    end;
end;

function TJvDynControlVCLListBox.ControlGetValue: Variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlVCLListBox.ControlSetSorted(Value: Boolean);
begin
  Sorted := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetItems(Value: TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlVCLListBox.ControlGetItems: TStrings;
begin
  Result := Items;
end;

function TJvDynControlVCLListBox.ControlValidateData(var Value: Variant;
  var ErrorMessage: string): Boolean;
begin
  Result := True;
end;

procedure TJvDynControlVCLListBox.ControlSetOnDblClick(Value: TNotifyEvent);
begin
  OnDblClick := Value;
end;


//=== TJvDynControlVCLComboBox ===============================================

procedure TJvDynControlVCLComboBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLComboBox.ControlSetReadOnly(Value: Boolean);
begin
//  ReadOnly := Value;
end;

procedure TJvDynControlVCLComboBox.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlVCLComboBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLComboBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLComboBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLComboBox.ControlSetOnChange(Value: TNotifyEvent);
begin
//  OnChange := Value;
end;

procedure TJvDynControlVCLComboBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLComboBox.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlVCLComboBox.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlVCLComboBox.ControlSetSorted(Value: Boolean);
begin
  Sorted := Value;
end;

procedure TJvDynControlVCLComboBox.ControlSetItems(Value: TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlVCLComboBox.ControlGetItems: TStrings;
begin
  Result := Items;
end;

function TJvDynControlVCLComboBox.ControlValidateData(var Value: Variant;
  var ErrorMessage: string): Boolean;
begin
  Result := True;
end;

procedure TJvDynControlVCLComboBox.ControlSetNewEntriesAllowed(Value: Boolean);
begin
  if Value then
    Style := csDropDown
  else
    Style := csDropDownList;
end;


//=== TJvDynControlVCLPanel ==================================================

procedure TJvDynControlVCLPanel.ControlSetDefaultProperties;
begin
  BevelInner := bvNone;
  BevelOuter := bvNone;
end;

procedure TJvDynControlVCLPanel.ControlSetCaption(Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlVCLPanel.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLPanel.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLPanel.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLPanel.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLPanel.ControlSetBorder(ABevelInner: TPanelBevel;
  ABevelOuter: TPanelBevel; ABevelWidth: Integer;
  ABorderStyle: TBorderStyle; ABorderWidth: Integer);
begin
  BorderWidth := ABorderWidth;
  BorderStyle := ABorderStyle;
  BevelInner  := ABevelInner;
  BevelOuter  := ABevelOuter;
  BevelWidth  := ABevelWidth;
end;

//=== TJvDynControlVCLImage ==================================================

procedure TJvDynControlVCLImage.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLImage.ControlSetCaption(Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlVCLImage.ControlSetTabOrder(Value: Integer);
begin
//  TabOrder := Value;
end;

procedure TJvDynControlVCLImage.ControlSetOnEnter(Value: TNotifyEvent);
begin
//  OnEnter := Value;
end;

procedure TJvDynControlVCLImage.ControlSetOnExit(Value: TNotifyEvent);
begin
//  OnExit := Value;
end;

procedure TJvDynControlVCLImage.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLImage.ControlSetAutoSize(Value: Boolean);
begin
  AutoSize := Value;
end;

procedure TJvDynControlVCLImage.ControlSetIncrementalDisplay(Value: Boolean);
begin
  IncrementalDisplay := Value;
end;

procedure TJvDynControlVCLImage.ControlSetCenter(Value: Boolean);
begin
  Center := Value;
end;

procedure TJvDynControlVCLImage.ControlSetStretch(Value: Boolean);
begin
  Stretch := Value;
end;

procedure TJvDynControlVCLImage.ControlSetTransparent(Value: Boolean);
begin
  Transparent := Value;
end;

procedure TJvDynControlVCLImage.ControlSetPicture(Value: TPicture);
begin
  Picture.Assign(Value);
end;

function TJvDynControlVCLImage.ControlGetPicture: TPicture;
begin
  Result := Picture;
end;

//=== TJvDynControlVCLScrollBox ==============================================

procedure TJvDynControlVCLScrollBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLScrollBox.ControlSetCaption(Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlVCLScrollBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLScrollBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLScrollBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLScrollBox.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

//=== TJvDynControlVCLLabel ==================================================

procedure TJvDynControlVCLLabel.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLLabel.ControlSetCaption(Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlVCLLabel.ControlSetTabOrder(Value: Integer);
begin
end;

procedure TJvDynControlVCLLabel.ControlSetOnEnter(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLLabel.ControlSetOnExit(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLLabel.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLLabel.ControlSetFocusControl(Value: TWinControl);
begin
  FocusControl := Value;
end;

//=== TJvDynControlVCLStaticText =============================================

procedure TJvDynControlVCLStaticText.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLStaticText.ControlSetCaption(Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlVCLStaticText.ControlSetTabOrder(Value: Integer);
begin
end;

procedure TJvDynControlVCLStaticText.ControlSetOnEnter(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLStaticText.ControlSetOnExit(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLStaticText.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

//=== TJvDynControlVCLButton =================================================

procedure TJvDynControlVCLButton.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLButton.ControlSetCaption(Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlVCLButton.ControlSetTabOrder(Value: Integer);
begin
end;

procedure TJvDynControlVCLButton.ControlSetOnEnter(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLButton.ControlSetOnExit(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLButton.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLButton.ControlSetGlyph(Value: TBitmap);
begin
  Glyph.Assign(Value);
end;

procedure TJvDynControlVCLButton.ControlSetNumGlyphs(Value: Integer);
begin
  NumGlyphs := Value;
end;

procedure TJvDynControlVCLButton.ControlSetLayout(Value: TButtonLayout);
begin
  Layout := Value;
end;


function DynControlEngine_VCL: TJvDynControlEngine;
begin
  Result := IntDynControlEngine_VCL;
end;

initialization
  IntDynControlEngine_VCL := TJvDynControlEngine.Create;
  IntDynControlEngine_VCL.RegisterControl(jctLabel, TJvDynControlVCLLabel);
  IntDynControlEngine_VCL.RegisterControl(jctStaticText, TJvDynControlVCLStaticText);
  IntDynControlEngine_VCL.RegisterControl(jctButton, TJvDynControlVCLButton);
  IntDynControlEngine_VCL.RegisterControl(jctScrollBox, TJvDynControlVCLScrollBox);
  IntDynControlEngine_VCL.RegisterControl(jctPanel, TJvDynControlVCLPanel);
  IntDynControlEngine_VCL.RegisterControl(jctImage, TJvDynControlVCLImage);
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
  IntDynControlEngine_VCL.RegisterControl(jctDirectoryEdit, TJvDynControlVCLDirectoryEdit);
  IntDynControlEngine_VCL.RegisterControl(jctFileNameEdit, TJvDynControlVCLFileNameEdit);
  IntDynControlEngine_VCL.RegisterControl(jctMemo, TJvDynControlVCLMemo);
  SetDefaultDynControlEngine(IntDynControlEngine_VCL);

finalization
  FreeAndNil(IntDynControlEngine_VCL);

end.
