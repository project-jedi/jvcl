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

unit JvDynControlEngineVCL;

interface

uses
  Classes, Controls, StdCtrls, ExtCtrls, ComCtrls, Mask, Forms, Graphics,
  Buttons, Dialogs, FileCtrl,
  JvDynControlEngine, JvDynControlEngineIntf;

type
  TJvDynControlVCLMaskEdit = class (TMaskEdit, IUnknown, IJvDynControl, IJvDynControlData, IJvDynControlReadOnly)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: boolean);
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: variant);
    function ControlGetValue: variant;
  end;

  TJvDynControlVCLFileNameEdit = class (TPanel, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlFileName, IJvDynControlReadOnly)
  private
    FEditControl: TMaskEdit;
    FButton: TBitBtn;
    FInitialDir: string;
    FFilterIndex: integer;
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
    procedure ControlSetReadOnly(Value: boolean);
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: variant);
    function ControlGetValue: variant;

    // IJvDynControlFileName
    procedure ControlSetInitialDir(Value: string);
    procedure ControlSetDefaultExt(Value: string);
    procedure ControlSetDialogTitle(Value: string);
    procedure ControlSetDialogOptions(Value: TOpenOptions);
    procedure ControlSetFilter(Value: string);
    procedure ControlSetFilterIndex(Value: integer);
    procedure ControlSetDialogKind(Value: TJvDynControlFileNameDialogKind);

  end;

  TJvDynControlVCLDirectoryEdit = class (TPanel, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlDirectory, IJvDynControlReadOnly)
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
    procedure ControlSetReadOnly(Value: boolean);
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: variant);
    function ControlGetValue: variant;

    // IJvDynControlDirectory
    procedure ControlSetInitialDir(Value: string);
    procedure ControlSetDialogTitle(Value: string);
    procedure ControlSetDialogOptions(Value: TSelectDirOpts);
  end;

  TJvDynControlVCLDateTimeEdit = class (TPanel, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlDate)
  private
    FDatePicker: TDateTimePicker;
    FTimePicker: TDateTimePicker;
  protected
    procedure ControlResize(Sender: TObject);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: variant);
    function ControlGetValue: variant;

    // IJvDynControlDate
    procedure ControlSetMinDate(Value: TDateTime);
    procedure ControlSetMaxDate(Value: TDateTime);
    procedure ControlSetFormat(Value: string);
  end;

  TJvDynControlVCLDateEdit = class (TDateTimePicker, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlDate)
  private
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: variant);
    function ControlGetValue: variant;

    // IJvDynControlDate
    procedure ControlSetMinDate(Value: TDateTime);
    procedure ControlSetMaxDate(Value: TDateTime);
    procedure ControlSetFormat(Value: string);
  end;

  TJvDynControlVCLTimeEdit = class (TDateTimePicker, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlTime)
  public
    procedure ControlSetDefaultProperties;

    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: variant);
    function ControlGetValue: variant;

    procedure ControlSetFormat(Value: string);
  end;

  TJvDynControlVCLCheckBox = class (TCheckBox, IUnknown, IJvDynControl,
    IJvDynControlData)
  private
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: variant);
    function ControlGetValue: variant;
  end;

  TJvDynControlVCLMemo = class (TMemo, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlMemo, IJvDynControlReadOnly)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: boolean);
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: variant);
    function ControlGetValue: variant;

    procedure ControlSetSorted(Value: boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetWantTabs(Value: boolean);
    procedure ControlSetWantReturns(Value: boolean);
    procedure ControlSetWordWrap(Value: boolean);
    procedure ControlSetScrollBars(Value: TScrollStyle);
  end;

  TJvDynControlVCLRadioGroup = class (TRadioGroup, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlRadioGroup)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: variant);
    function ControlGetValue: variant;

    procedure ControlSetSorted(Value: boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetColumns(Value: integer);
  end;

  TJvDynControlVCLListBox = class (TListBox, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlDblClick)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: variant);
    function ControlGetValue: variant;

    procedure ControlSetSorted(Value: boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetOnDblClick(Value: TNotifyEvent);
  end;

  TJvDynControlVCLComboBox = class (TComboBox, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlComboBox)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: variant);
    function ControlGetValue: variant;

    procedure ControlSetSorted(Value: boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetNewEntriesAllowed(Value: boolean);
  end;

  TJvDynControlVCLPanel = class (TPanel, IUnknown, IJvDynControl, IJvDynControlPanel)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetBorder(ABevelInner: TPanelBevel; ABevelOuter: TPanelBevel; ABevelWidth: integer; ABorderStyle: TBorderStyle; ABorderWidth: integer);
  end;

  TJvDynControlVCLImage = class (TImage, IUnknown, IJvDynControl, IJvDynControlImage)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetAutoSize(Value: boolean);
    procedure ControlSetIncrementalDisplay(Value: boolean);
    procedure ControlSetCenter(Value: boolean);
    procedure ControlSetProportional(Value: boolean);
    procedure ControlSetStretch(Value: boolean);
    procedure ControlSetTransparent(Value: boolean);
    procedure ControlSetPicture(Value: TPicture);
    procedure ControlSetGraphic(Value: TGraphic);
    function ControlGetPicture: TPicture;
  end;

  TJvDynControlVCLScrollBox = class (TScrollbox, IUnknown, IJvDynControl)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
  end;

  TJvDynControlVCLLabel = class (TLabel, IUnknown, IJvDynControl, IJvDynControlLabel)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetFocusControl(Value: TWinControl);
    procedure ControlSetWordWrap(Value: boolean);
  end;

  TJvDynControlVCLStaticText = class (TStaticText, IUnknown, IJvDynControl)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
  end;

  TJvDynControlVCLButton = class (TBitBtn, IUnknown, IJvDynControl,
    IJvDynControlButton)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetGlyph(Value: TBitmap);
    procedure ControlSetNumGlyphs(Value: integer);
    procedure ControlSetLayout(Value: TButtonLayout);
  end;

function DynControlEngineVCL: TJvDynControlEngine;

implementation

uses
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  SysUtils, ExtDlgs;

var
  IntDynControlEngineVCL: TJvDynControlEngine = nil;

//=== TJvDynControlVCLMaskEdit ===============================================

procedure TJvDynControlVCLMaskEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLMaskEdit.ControlSetReadOnly(Value: boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlVCLMaskEdit.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlVCLMaskEdit.ControlSetTabOrder(Value: integer);
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

procedure TJvDynControlVCLMaskEdit.ControlSetValue(Value: variant);
begin
  Text := Value;
end;

function TJvDynControlVCLMaskEdit.ControlGetValue: variant;
begin
  Result := Text;
end;

//=== TJvDynControlVCLFileNameEdit ===========================================

constructor TJvDynControlVCLFileNameEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditControl := TMaskEdit.Create(AOwner);
  FEditControl.Parent := Self;
  FButton     := TBitBtn.Create(AOwner);
  FButton.Parent := Self;
  FButton.Align := alRight;
  FButton.OnClick := DefaultOnButtonClick;
  FButton.Caption := '...';
  Height      := FEditControl.Height;
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
          Options    := FDialogOptions;
          Title      := FDialogTitle;
          Filter     := FFilter;
          FilterIndex := FFilterIndex;
          InitialDir := FInitialDir;
          DefaultExt := FDefaultExt;
          FileName   := ControlGetValue;
          if Execute then
            ControlSetValue(FileName);
        finally
          Free;
        end;
    jdkOpenPicture:
      with TOpenPictureDialog.Create(Self) do
        try
          Options    := FDialogOptions;
          Title      := FDialogTitle;
          Filter     := FFilter;
          FilterIndex := FFilterIndex;
          InitialDir := FInitialDir;
          DefaultExt := FDefaultExt;
          FileName   := ControlGetValue;
          if Execute then
            ControlSetValue(FileName);
        finally
          Free;
        end;
    jdkSave:
      with TSaveDialog.Create(Self) do
        try
          Options    := FDialogOptions;
          Title      := FDialogTitle;
          Filter     := FFilter;
          FilterIndex := FFilterIndex;
          InitialDir := FInitialDir;
          DefaultExt := FDefaultExt;
          FileName   := ControlGetValue;
          if Execute then
            ControlSetValue(FileName);
        finally
          Free;
        end;
    jdkSavePicture:
      with TSavePictureDialog.Create(Self) do
        try
          Options    := FDialogOptions;
          Title      := FDialogTitle;
          Filter     := FFilter;
          FilterIndex := FFilterIndex;
          InitialDir := FInitialDir;
          DefaultExt := FDefaultExt;
          FileName   := ControlGetValue;
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

procedure TJvDynControlVCLFileNameEdit.ControlSetReadOnly(Value: boolean);
begin
  FEditControl.ReadOnly := Value;
  FButton.Enabled := not Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetTabOrder(Value: integer);
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

procedure TJvDynControlVCLFileNameEdit.ControlSetValue(Value: variant);
begin
  FEditControl.Text := Value;
end;

function TJvDynControlVCLFileNameEdit.ControlGetValue: variant;
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

procedure TJvDynControlVCLFileNameEdit.ControlSetFilterIndex(Value: integer);
begin
  FFilterIndex := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetDialogKind(Value: TJvDynControlFileNameDialogKind);
begin
  FDialogKind := Value;
end;

//=== TJvDynControlVCLDirectoryEdit ==========================================

constructor TJvDynControlVCLDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditControl := TMaskEdit.Create(AOwner);
  FEditControl.Parent := Self;
  FButton    := TBitBtn.Create(AOwner);
  FButton.Parent := Self;
  FButton.Align := alRight;
  FButton.OnClick := DefaultOnButtonClick;
  FButton.Caption := '...';
  Height     := FEditControl.Height;
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

procedure TJvDynControlVCLDirectoryEdit.ControlSetReadOnly(Value: boolean);
begin
  FEditControl.ReadOnly := Value;
  FButton.Enabled := not Value;
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetTabOrder(Value: integer);
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

procedure TJvDynControlVCLDirectoryEdit.ControlSetValue(Value: variant);
begin
  FEditControl.Text := Value;
end;

function TJvDynControlVCLDirectoryEdit.ControlGetValue: variant;
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

//=== TJvDynControlVCLDateTimeEdit ===========================================

constructor TJvDynControlVCLDateTimeEdit.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Caption  := '';
  BorderStyle := bsNone;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  FDatePicker := TDateTimePicker.Create(self);
  FDatePicker.Parent := self;
  FDatePicker.Align := alLeft;
  FDatePicker.Top := 0;
  FDatePicker.Left := 0;
  FTimePicker := TDateTimePicker.Create(self);
  FTimePicker.Align := alClient;
  FTimePicker.Parent := self;
  FTimePicker.Top := 0;
  FTimePicker.Left := 0;
  Height   := FDatePicker.Height;
  Width    := FDatePicker.Width + FTimePicker.Width;
  OnResize := ControlResize;
  ControlResize(nil);
  FDatePicker.DateFormat := dfShort;
  FDatePicker.DateMode := dmComboBox;
  FDatePicker.Kind     := dtkDate;
  FTimePicker.DateFormat := dfShort;
  FTimePicker.DateMode := dmUpDown;
  FTimePicker.Kind     := dtkTime;
end;

destructor TJvDynControlVCLDateTimeEdit.Destroy;
begin
  FreeAndNil(FDatePicker);
  FreeAndNil(FTimePicker);
  inherited Destroy;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlResize(Sender: TObject);
begin
  FDatePicker.Height := round(Height / 2);
  FTimePicker.Height := Height;
  FDatePicker.Width  := round(Width / 2);
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetCaption(Value: string);
begin
  //Caption := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetTabOrder(Value: integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  FDatePicker.OnEnter := Value;
  FTimePicker.OnEnter := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  FDatePicker.OnExit := Value;
  FTimePicker.OnExit := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  FDatePicker.OnChange := Value;
  FTimePicker.OnChange := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetOnClick(Value: TNotifyEvent);
begin

end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetValue(Value: variant);
begin
  FDatePicker.Date := Value;
  FTimePicker.Time := Value;
end;

function TJvDynControlVCLDateTimeEdit.ControlGetValue: variant;
begin
  Result := trunc(FDatePicker.Date) + (trunc(FTimePicker.Time) - FTimePicker.Time);
end;

// IJvDynControlDate
procedure TJvDynControlVCLDateTimeEdit.ControlSetMinDate(Value: TDateTime);
begin
  FDatePicker.MinDate := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetMaxDate(Value: TDateTime);
begin
  FDatePicker.MaxDate := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetFormat(Value: string);
begin
  {$IFDEF COMPILER6_UP}
  FDatePicker.Format := Value;
  FTimePicker.Format := Value;
  {$ENDIF COMPILER6_UP}
end;

//=== TJvDynControlVCLDateEdit ===========================================

procedure TJvDynControlVCLDateEdit.ControlSetDefaultProperties;
begin
  DateFormat := dfShort;
  DateMode := dmComboBox;
  Kind := dtkDate;
end;

procedure TJvDynControlVCLDateEdit.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlVCLDateEdit.ControlSetTabOrder(Value: integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLDateEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLDateEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLDateEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlVCLDateEdit.ControlSetOnClick(Value: TNotifyEvent);
begin

end;

procedure TJvDynControlVCLDateEdit.ControlSetValue(Value: variant);
begin
  Date := Value;
end;

function TJvDynControlVCLDateEdit.ControlGetValue: variant;
begin
  Result := Date;
end;

// IJvDynControlDate
procedure TJvDynControlVCLDateEdit.ControlSetMinDate(Value: TDateTime);
begin
  MinDate := Value;
end;

procedure TJvDynControlVCLDateEdit.ControlSetMaxDate(Value: TDateTime);
begin
  MaxDate := Value;
end;

procedure TJvDynControlVCLDateEdit.ControlSetFormat(Value: string);
begin
  {$IFDEF COMPILER6_UP}
  Format := Value;
  {$ENDIF COMPILER6_UP}
end;

//=== TJvDynControlVCLTimeEdit ===========================================

procedure TJvDynControlVCLTimeEdit.ControlSetDefaultProperties;
begin
  DateFormat := dfShort;
  Kind     := dtkTime;
  DateMode := dmUpDown;
end;

procedure TJvDynControlVCLTimeEdit.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlVCLTimeEdit.ControlSetTabOrder(Value: integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLTimeEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLTimeEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLTimeEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlVCLTimeEdit.ControlSetOnClick(Value: TNotifyEvent);
begin

end;

procedure TJvDynControlVCLTimeEdit.ControlSetValue(Value: variant);
begin
  Time := Value;
end;

function TJvDynControlVCLTimeEdit.ControlGetValue: variant;
begin
  Result := Time;
end;



procedure TJvDynControlVCLTimeEdit.ControlSetFormat(Value: string);
begin
  {$IFDEF COMPILER6_UP}
  Format := Value;
  {$ENDIF COMPILER6_UP}
end;


//=== TJvDynControlVCLCheckBox ===============================================

procedure TJvDynControlVCLCheckBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLCheckBox.ControlSetCaption(Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlVCLCheckBox.ControlSetTabOrder(Value: integer);
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
  OnClick := Value;
end;

procedure TJvDynControlVCLCheckBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLCheckBox.ControlSetValue(Value: variant);
begin
  if VarType(Value) = varBoolean then
    Checked := Value
  else
    Checked := Uppercase(Value) = 'TRUE';
end;

function TJvDynControlVCLCheckBox.ControlGetValue: variant;
begin
  Result := Checked;
end;


//=== TJvDynControlVCLMemo ===================================================

procedure TJvDynControlVCLMemo.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLMemo.ControlSetReadOnly(Value: boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlVCLMemo.ControlSetTabOrder(Value: integer);
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

procedure TJvDynControlVCLMemo.ControlSetValue(Value: variant);
begin
  Text := Value;
end;

function TJvDynControlVCLMemo.ControlGetValue: variant;
begin
  Result := Text;
end;

procedure TJvDynControlVCLMemo.ControlSetSorted(Value: boolean);
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

procedure TJvDynControlVCLMemo.ControlSetWantTabs(Value: boolean);
begin
  WantTabs := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetWantReturns(Value: boolean);
begin
  WantReturns := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetWordWrap(Value: boolean);
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

procedure TJvDynControlVCLRadioGroup.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlVCLRadioGroup.ControlSetTabOrder(Value: integer);
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
  OnClick := Value;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetValue(Value: variant);
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

function TJvDynControlVCLRadioGroup.ControlGetValue: variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetSorted(Value: boolean);
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

procedure TJvDynControlVCLRadioGroup.ControlSetColumns(Value: integer);
begin
  Columns := Value;
end;

//=== TJvDynControlVCLListBox ================================================

procedure TJvDynControlVCLListBox.ControlSetDefaultProperties;
begin
end;


procedure TJvDynControlVCLListBox.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlVCLListBox.ControlSetTabOrder(Value: integer);
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

procedure TJvDynControlVCLListBox.ControlSetValue(Value: variant);
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

function TJvDynControlVCLListBox.ControlGetValue: variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlVCLListBox.ControlSetSorted(Value: boolean);
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

procedure TJvDynControlVCLListBox.ControlSetOnDblClick(Value: TNotifyEvent);
begin
  OnDblClick := Value;
end;


//=== TJvDynControlVCLComboBox ===============================================

procedure TJvDynControlVCLComboBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLComboBox.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlVCLComboBox.ControlSetTabOrder(Value: integer);
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

procedure TJvDynControlVCLComboBox.ControlSetValue(Value: variant);
begin
  Text := Value;
end;

function TJvDynControlVCLComboBox.ControlGetValue: variant;
begin
  Result := Text;
end;

procedure TJvDynControlVCLComboBox.ControlSetSorted(Value: boolean);
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

procedure TJvDynControlVCLComboBox.ControlSetNewEntriesAllowed(Value: boolean);
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

procedure TJvDynControlVCLPanel.ControlSetTabOrder(Value: integer);
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

procedure TJvDynControlVCLPanel.ControlSetBorder(ABevelInner: TPanelBevel; ABevelOuter: TPanelBevel; ABevelWidth: integer; ABorderStyle: TBorderStyle; ABorderWidth: integer);
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

procedure TJvDynControlVCLImage.ControlSetTabOrder(Value: integer);
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

procedure TJvDynControlVCLImage.ControlSetAutoSize(Value: boolean);
begin
  AutoSize := Value;
end;

procedure TJvDynControlVCLImage.ControlSetIncrementalDisplay(Value: boolean);
begin
  IncrementalDisplay := Value;
end;

procedure TJvDynControlVCLImage.ControlSetCenter(Value: boolean);
begin
  Center := Value;
end;

procedure TJvDynControlVCLImage.ControlSetProportional(Value: boolean);
begin
  {$IFDEF COMPILER6_UP}
  Proportional := Value;
  {$ENDIF COMPILER6_UP}
end;

procedure TJvDynControlVCLImage.ControlSetStretch(Value: boolean);
begin
  Stretch := Value;
end;

procedure TJvDynControlVCLImage.ControlSetTransparent(Value: boolean);
begin
  Transparent := Value;
end;

procedure TJvDynControlVCLImage.ControlSetPicture(Value: TPicture);
begin
  Picture.Assign(Value);
end;

procedure TJvDynControlVCLImage.ControlSetGraphic(Value: TGraphic);
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

procedure TJvDynControlVCLScrollBox.ControlSetTabOrder(Value: integer);
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

procedure TJvDynControlVCLLabel.ControlSetTabOrder(Value: integer);
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

procedure TJvDynControlVCLLabel.ControlSetWordWrap(Value: boolean);
begin
  WordWrap := Value;
end;

//=== TJvDynControlVCLStaticText =============================================

procedure TJvDynControlVCLStaticText.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLStaticText.ControlSetCaption(Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlVCLStaticText.ControlSetTabOrder(Value: integer);
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

procedure TJvDynControlVCLButton.ControlSetTabOrder(Value: integer);
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

procedure TJvDynControlVCLButton.ControlSetNumGlyphs(Value: integer);
begin
  NumGlyphs := Value;
end;

procedure TJvDynControlVCLButton.ControlSetLayout(Value: TButtonLayout);
begin
  Layout := Value;
end;


function DynControlEngineVCL: TJvDynControlEngine;
begin
  Result := IntDynControlEngineVCL;
end;

initialization
  IntDynControlEngineVCL := TJvDynControlEngine.Create;
  IntDynControlEngineVCL.RegisterControl(jctLabel, TJvDynControlVCLLabel);
  IntDynControlEngineVCL.RegisterControl(jctStaticText, TJvDynControlVCLStaticText);
  IntDynControlEngineVCL.RegisterControl(jctButton, TJvDynControlVCLButton);
  IntDynControlEngineVCL.RegisterControl(jctScrollBox, TJvDynControlVCLScrollBox);
  IntDynControlEngineVCL.RegisterControl(jctPanel, TJvDynControlVCLPanel);
  IntDynControlEngineVCL.RegisterControl(jctImage, TJvDynControlVCLImage);
  IntDynControlEngineVCL.RegisterControl(jctCheckBox, TJvDynControlVCLCheckBox);
  IntDynControlEngineVCL.RegisterControl(jctComboBox, TJvDynControlVCLComboBox);
  IntDynControlEngineVCL.RegisterControl(jctListBox, TJvDynControlVCLListBox);
  IntDynControlEngineVCL.RegisterControl(jctRadioGroup, TJvDynControlVCLRadioGroup);
  IntDynControlEngineVCL.RegisterControl(jctDateTimeEdit, TJvDynControlVCLDateTimeEdit);
  IntDynControlEngineVCL.RegisterControl(jctTimeEdit, TJvDynControlVCLTimeEdit);
  IntDynControlEngineVCL.RegisterControl(jctDateEdit, TJvDynControlVCLDateEdit);
  IntDynControlEngineVCL.RegisterControl(jctEdit, TJvDynControlVCLMaskEdit);
 //  IntDynControlEngineVCL.RegisterControl(jctCalculateEdit, TJvDynControlVCLMaskEdit);
 //  IntDynControlEngineVCL.RegisterControl(jctSpinEdit, TJvDynControlVCLMaskEdit);
  IntDynControlEngineVCL.RegisterControl(jctDirectoryEdit, TJvDynControlVCLDirectoryEdit);
  IntDynControlEngineVCL.RegisterControl(jctFileNameEdit, TJvDynControlVCLFileNameEdit);
  IntDynControlEngineVCL.RegisterControl(jctMemo, TJvDynControlVCLMemo);
  SetDefaultDynControlEngine(IntDynControlEngineVCL);

finalization
  FreeAndNil(IntDynControlEngineVCL);

end.
