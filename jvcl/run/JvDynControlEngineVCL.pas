{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudickar att oratool dott de]
All Rights Reserved.

Contributor(s):
Jens Fudickar [jens dott fudickar att oratool dott de]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I crossplatform.inc}

unit JvDynControlEngineVCL;

interface

uses
  Classes,
  Controls, StdCtrls, ExtCtrls, ComCtrls, Mask, Forms, Graphics,
  Buttons, Dialogs, FileCtrl, ExtDlgs, CheckLst,
  JvDynControlEngine, JvDynControlEngineIntf;

type
  TJvDynControlVCLMaskEdit = class (TMaskEdit, IUnknown, IJvDynControl, IJvDynControlData, IJvDynControlReadOnly, IJvDynControlEdit)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    //IJvDynControlEdit
    procedure ControlSetPasswordChar(Value: Char);
    procedure ControlSetEditMask(const Value: string);
  end;

  TJvDynControlVCLButtonEdit = class (TPanel, IUnknown, IJvDynControl, IJvDynControlData,
    IJvDynControlReadOnly, IJvDynControlEdit, IJvDynControlButtonEdit,
    IJvDynControlButton)
  private
    FEditControl: TMaskEdit;
    FButton: TBitBtn;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    //IJvDynControlEdit
    procedure ControlSetPasswordChar(Value: Char);
    procedure ControlSetEditMask(const Value: string);

    //IJvDynControlButtonEdit
    procedure ControlSetOnButtonClick(Value: TNotifyEvent);
    procedure ControlSetButtonCaption(const Value: string);

    //IJvDynControlButton
    procedure ControlSetGlyph(Value: TBitmap);
    procedure ControlSetNumGlyphs(Value: Integer);
    procedure ControlSetLayout(Value: TButtonLayout);
  end;


  TJvDynControlVCLFileNameEdit = class (TPanel, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlFileName, IJvDynControlReadOnly)
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
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    // IJvDynControlFileName
    procedure ControlSetInitialDir(const Value: string);
    procedure ControlSetDefaultExt(const Value: string);
    procedure ControlSetDialogTitle(const Value: string);
    procedure ControlSetDialogOptions(Value: TOpenOptions);
    procedure ControlSetFilter(const Value: string);
    procedure ControlSetFilterIndex(Value: Integer);
    procedure ControlSetDialogKind(Value: TJvDynControlFileNameDialogKind);
  end;

  TJvDynControlVCLDirectoryEdit = class (TPanel, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlDirectory, IJvDynControlReadOnly)
  private
    FEditControl: TMaskEdit;
    FButton: TBitBtn;
    FInitialDir: string;
    {$IFDEF VCL}
    FDialogOptions: TSelectDirOpts;
    {$ENDIF VCL}
    FDialogTitle: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DefaultOnButtonClick(Sender: TObject);

    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    // IJvDynControlDirectory
    procedure ControlSetInitialDir(const Value: string);
    procedure ControlSetDialogTitle(const Value: string);
    {$IFDEF VCL}
    procedure ControlSetDialogOptions(Value: TSelectDirOpts);
    {$ENDIF VCL}
  end;

  {$IFDEF VCL}
  TJvDynControlVCLDateTimeEdit = class (TPanel, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlDate)
  private
    FDatePicker: TDateTimePicker;
    FTimePicker: TDateTimePicker;
  protected
    procedure ControlResize(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    // IJvDynControlDate
    procedure ControlSetMinDate(Value: TDateTime);
    procedure ControlSetMaxDate(Value: TDateTime);
    procedure ControlSetFormat(const Value: string);
  end;

  TJvDynControlVCLDateEdit = class (TDateTimePicker, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlDate)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    // IJvDynControlDate
    procedure ControlSetMinDate(Value: TDateTime);
    procedure ControlSetMaxDate(Value: TDateTime);
    procedure ControlSetFormat(const Value: string);
  end;

  TJvDynControlVCLTimeEdit = class (TDateTimePicker, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlTime)
  public
    procedure ControlSetDefaultProperties;

    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetFormat(const Value: string);
  end;
  {$ENDIF VCL}

  TJvDynControlVCLCheckBox = class (TCheckBox, IUnknown, IJvDynControl,
    IJvDynControlData)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;
  end;

  TJvDynControlVCLMemo = class (TMemo, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlMemo, IJvDynControlReadOnly)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

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

  TJvDynControlVCLRadioGroup = class (TRadioGroup, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlRadioGroup)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetColumns(Value: Integer);
  end;

  TJvDynControlVCLListBox = class (TListBox, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlDblClick)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetOnDblClick(Value: TNotifyEvent);
  end;

  TJvDynControlVCLCheckListBox = class (TCheckListBox, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlDblClick,
    IJvDynControlCheckListBox)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetOnDblClick(Value: TNotifyEvent);

    //IJvDynControlCheckListBox = interface
    procedure ControlSetAllowGrayed(Value: Boolean);
    procedure ControlSetChecked(Index: Integer; Value: Boolean);
    procedure ControlSetItemEnabled(Index: Integer; Value: Boolean);
    procedure ControlSetState(Index: Integer; Value: TCheckBoxState);
    function ControlGetChecked(Index: Integer): Boolean;
    function ControlGetItemEnabled(Index: Integer): Boolean;
    function ControlGetState(Index: Integer): TCheckBoxState;
    procedure ControlSetHeader(Index: Integer; Value: Boolean);
    function ControlGetHeader(Index: Integer): Boolean;
  end;

  TJvDynControlVCLComboBox = class (TComboBox, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlComboBox)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetNewEntriesAllowed(Value: Boolean);
  end;

  TJvDynControlVCLGroupBox = class (TGroupBox, IUnknown, IJvDynControl)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
  end;

  TJvDynControlVCLPanel = class (TPanel, IUnknown, IJvDynControl, IJvDynControlPanel)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetBorder(ABevelInner: TPanelBevel; ABevelOuter: TPanelBevel; ABevelWidth: Integer; ABorderStyle: TBorderStyle; ABorderWidth: Integer);
  end;

  TJvDynControlVCLImage = class (TImage, IUnknown, IJvDynControl, IJvDynControlImage)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetAutoSize(Value: Boolean);
    procedure ControlSetIncrementalDisplay(Value: Boolean);
    procedure ControlSetCenter(Value: Boolean);
    {$IFDEF VCL}
    procedure ControlSetProportional(Value: Boolean);
    {$ENDIF VCL}
    procedure ControlSetStretch(Value: Boolean);
    procedure ControlSetTransparent(Value: Boolean);
    procedure ControlSetPicture(Value: TPicture);
    procedure ControlSetGraphic(Value: TGraphic);
    function ControlGetPicture: TPicture;
  end;

  TJvDynControlVCLScrollBox = class (TScrollbox, IUnknown, IJvDynControl)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
  end;

  TJvDynControlVCLLabel = class (TLabel, IUnknown, IJvDynControl, IJvDynControlLabel)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetFocusControl(Value: TWinControl);
    procedure ControlSetWordWrap(Value: Boolean);
  end;

  {$IFDEF VCL}
  TJvDynControlVCLStaticText = class (TStaticText, IUnknown, IJvDynControl)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
  end;
  {$ENDIF VCL}

  TJvDynControlVCLButton = class (TBitBtn, IUnknown, IJvDynControl,
    IJvDynControlButton)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetGlyph(Value: TBitmap);
    procedure ControlSetNumGlyphs(Value: Integer);
    procedure ControlSetLayout(Value: TButtonLayout);
  end;

function DynControlEngineVCL: TJvDynControlEngine;

implementation

uses
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  SysUtils,
  JvJCLUtils;

var
  IntDynControlEngineVCL: TJvDynControlEngine = nil;

//=== { TJvDynControlVCLMaskEdit } ===========================================

procedure TJvDynControlVCLMaskEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLMaskEdit.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlVCLMaskEdit.ControlSetCaption(const Value: string);
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

procedure TJvDynControlVCLMaskEdit.ControlSetPasswordChar(Value: Char);
begin
  {$IFDEF VCL}
  PasswordChar := Value;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  if Value = #0 then
    EchoMode := emNormal
  else
    EchoMode := emPassword;
  {$ENDIF VisualCLX}
end;

procedure TJvDynControlVCLMaskEdit.ControlSetEditMask(const Value: string);
begin
  EditMask := Value;
end;

//=== { TJvDynControlVCLButtonEdit } ============================================

constructor TJvDynControlVCLButtonEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditControl := TMaskEdit.Create(AOwner);
  FEditControl.Parent := Self;
  FButton     := TBitBtn.Create(AOwner);
  FButton.Parent := Self;
  FButton.Align := alRight;
  FButton.Caption := '...';
  Height      := FEditControl.Height;
  FButton.Width := Height;
  FEditControl.Align := alClient;
  BevelInner  := bvNone;
  BevelOuter  := bvNone;
end;

destructor TJvDynControlVCLButtonEdit.Destroy;
begin
  FreeAndNil(FEditControl);
  FreeAndNil(FButton);
  inherited Destroy;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLButtonEdit.ControlSetReadOnly(Value: Boolean);
begin
  FEditControl.ReadOnly := Value;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlVCLButtonEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  FEditControl.OnChange := Value;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
  FEditControl.OnClick := Value;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetValue(Value: Variant);
begin
  FEditControl.Text := Value;
end;

function TJvDynControlVCLButtonEdit.ControlGetValue: Variant;
begin
  Result := FEditControl.Text;
end;

{$IFDEF VisualCLX}
type
  TMaskEditAccessProtected = class(TMaskEdit);
{$ENDIF VisualCLX}

procedure TJvDynControlVCLButtonEdit.ControlSetPasswordChar(Value: Char);
begin
  {$IFDEF VCL}
  FEditControl.PasswordChar := Value;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  if Value = #0 then
    TMaskEditAccessProtected(FEditControl).EchoMode := emNormal
  else
    TMaskEditAccessProtected(FEditControl).EchoMode := emPassword;
  {$ENDIF VisualCLX}
end;

procedure TJvDynControlVCLButtonEdit.ControlSetEditMask(const Value: string);
begin
  FEditControl.EditMask := Value;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetOnButtonClick(Value: TNotifyEvent);
begin
  FButton.OnClick:= Value;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetButtonCaption(const Value: string);
begin
  FButton.Caption := Value;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetGlyph(Value: TBitmap);
begin
  FButton.Glyph.Assign(Value);
end;

procedure TJvDynControlVCLButtonEdit.ControlSetNumGlyphs(Value: Integer);
begin
  FButton.NumGlyphs := Value;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetLayout(Value: TButtonLayout);
begin
  FButton.Layout := Value;
end;

//=== { TJvDynControlVCLFileNameEdit } =======================================

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
  FButton.Enabled := not Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetCaption(const Value: string);
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
procedure TJvDynControlVCLFileNameEdit.ControlSetInitialDir(const Value: string);
begin
  FInitialDir := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetDefaultExt(const Value: string);
begin
  FDefaultExt := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetDialogTitle(const Value: string);
begin
  FDialogTitle := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetDialogOptions(Value: TOpenOptions);
begin
  FDialogOptions := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetFilter(const Value: string);
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

//=== { TJvDynControlVCLDirectoryEdit } ======================================

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
  {$IFDEF VCL}
  Opt: TSelectDirOpts;
  Dir: string;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Dir: WideString;
  {$ENDIF VisualCLX}
begin
  Dir := ControlGetValue;
  {$IFDEF VCL}
  if SelectDirectory(Dir, Opt, HelpContext) then
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  {$IFDEF MSWINDOWS}
  if SelectDirectory('', '', Dir) then
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  if SelectDirectory('', '/', Dir, False) then
  {$ENDIF LINUX}
  {$ENDIF VisualCLX}
    ControlSetValue(Dir);
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetReadOnly(Value: Boolean);
begin
  FEditControl.ReadOnly := Value;
  FButton.Enabled := not Value;
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetCaption(const Value: string);
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

procedure TJvDynControlVCLDirectoryEdit.ControlSetInitialDir(const Value: string);
begin
  FInitialDir := Value;
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetDialogTitle(const Value: string);
begin
  FDialogTitle := Value;
end;

{$IFDEF VCL}
procedure TJvDynControlVCLDirectoryEdit.ControlSetDialogOptions(Value: TSelectDirOpts);
begin
  FDialogOptions := Value;
end;
{$ENDIF VCL}

{$IFDEF VCL}
//=== { TJvDynControlVCLDateTimeEdit } =======================================

constructor TJvDynControlVCLDateTimeEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption  := '';
  BorderStyle := bsNone;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  FDatePicker := TDateTimePicker.Create(Self);
  FDatePicker.Parent := Self;
  FDatePicker.Align := alLeft;
  FDatePicker.Top := 0;
  FDatePicker.Left := 0;
  FTimePicker := TDateTimePicker.Create(Self);
  FTimePicker.Align := alClient;
  FTimePicker.Parent := Self;
  FTimePicker.Top := 0;
  FTimePicker.Left := 0;
  Height := FDatePicker.Height;
  Width := FDatePicker.Width + FTimePicker.Width;
  OnResize := ControlResize;
  ControlResize(nil);
  FDatePicker.DateFormat := dfShort;
  FDatePicker.DateMode := dmComboBox;
  FDatePicker.Kind := dtkDate;
  FTimePicker.DateFormat := dfShort;
  FTimePicker.DateMode := dmUpDown;
  FTimePicker.Kind := dtkTime;
end;

destructor TJvDynControlVCLDateTimeEdit.Destroy;
begin
  FreeAndNil(FDatePicker);
  FreeAndNil(FTimePicker);
  inherited Destroy;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlResize(Sender: TObject);
begin
  FDatePicker.Height := Round(Height / 2);
  FTimePicker.Height := Height;
  FDatePicker.Width := Round(Width / 2);
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetCaption(const Value: string);
begin
  //Caption := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetTabOrder(Value: Integer);
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

procedure TJvDynControlVCLDateTimeEdit.ControlSetValue(Value: Variant);
begin
  FDatePicker.Date := Value;
  FTimePicker.Time := Value;
end;

function TJvDynControlVCLDateTimeEdit.ControlGetValue: Variant;
begin
  Result := Trunc(FDatePicker.Date) + (Trunc(FTimePicker.Time) - FTimePicker.Time);
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

procedure TJvDynControlVCLDateTimeEdit.ControlSetFormat(const Value: string);
begin
  {$IFDEF COMPILER6_UP}
  FDatePicker.Format := Value;
  FTimePicker.Format := Value;
  {$ENDIF COMPILER6_UP}
end;

//=== { TJvDynControlVCLDateEdit } =======================================

procedure TJvDynControlVCLDateEdit.ControlSetDefaultProperties;
begin
  DateFormat := dfShort;
  DateMode := dmComboBox;
  Kind := dtkDate;
end;

procedure TJvDynControlVCLDateEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlVCLDateEdit.ControlSetTabOrder(Value: Integer);
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

procedure TJvDynControlVCLDateEdit.ControlSetValue(Value: Variant);
begin
  Date := Value;
end;

function TJvDynControlVCLDateEdit.ControlGetValue: Variant;
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

procedure TJvDynControlVCLDateEdit.ControlSetFormat(const Value: string);
begin
  {$IFDEF COMPILER6_UP}
  Format := Value;
  {$ENDIF COMPILER6_UP}
end;

//=== { TJvDynControlVCLTimeEdit } =======================================

procedure TJvDynControlVCLTimeEdit.ControlSetDefaultProperties;
begin
  DateFormat := dfShort;
  Kind := dtkTime;
  DateMode := dmUpDown;
end;

procedure TJvDynControlVCLTimeEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlVCLTimeEdit.ControlSetTabOrder(Value: Integer);
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

procedure TJvDynControlVCLTimeEdit.ControlSetValue(Value: Variant);
begin
  Time := Value;
end;

function TJvDynControlVCLTimeEdit.ControlGetValue: Variant;
begin
  Result := Time;
end;

procedure TJvDynControlVCLTimeEdit.ControlSetFormat(const Value: string);
begin
  {$IFDEF COMPILER6_UP}
  Format := Value;
  {$ENDIF COMPILER6_UP}
end;

{$ENDIF VCL}

//=== { TJvDynControlVCLCheckBox } ===========================================

procedure TJvDynControlVCLCheckBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLCheckBox.ControlSetCaption(const Value: string);
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
  OnClick := Value;
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

//=== { TJvDynControlVCLMemo } ===============================================

procedure TJvDynControlVCLMemo.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLMemo.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetCaption(const Value: string);
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

//=== { TJvDynControlVCLRadioGroup } =========================================

procedure TJvDynControlVCLRadioGroup.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLRadioGroup.ControlSetCaption(const Value: string);
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
  OnClick := Value;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetValue(Value: Variant);
begin
  if VarIsInt(Value) then
    ItemIndex := Value
  else
    ItemIndex := Items.IndexOf(Value);
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

//=== { TJvDynControlVCLListBox } ============================================

procedure TJvDynControlVCLListBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLListBox.ControlSetCaption(const Value: string);
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
  if VarIsInt(Value) then
    ItemIndex := Value
  else
    ItemIndex := Items.IndexOf(Value);
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

procedure TJvDynControlVCLListBox.ControlSetOnDblClick(Value: TNotifyEvent);
begin
  OnDblClick := Value;
end;

//=== { TJvDynControlVCLCheckListBox } ============================================

procedure TJvDynControlVCLCheckListBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLCheckListBox.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlVCLCheckListBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLCheckListBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLCheckListBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLCheckListBox.ControlSetOnChange(Value: TNotifyEvent);
begin
//  OnChange := Value;
end;

procedure TJvDynControlVCLCheckListBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLCheckListBox.ControlSetValue(Value: Variant);
begin
  if VarIsInt(Value) then
    ItemIndex := Value
  else
    ItemIndex := Items.IndexOf(Value);
end;

function TJvDynControlVCLCheckListBox.ControlGetValue: Variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlVCLCheckListBox.ControlSetSorted(Value: Boolean);
begin
  Sorted := Value;
end;

procedure TJvDynControlVCLCheckListBox.ControlSetItems(Value: TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlVCLCheckListBox.ControlGetItems: TStrings;
begin
  Result := Items;
end;

procedure TJvDynControlVCLCheckListBox.ControlSetOnDblClick(Value: TNotifyEvent);
begin
  OnDblClick := Value;
end;

//IJvDynControlCheckListBox = interface
procedure TJvDynControlVCLCheckListBox.ControlSetAllowGrayed(Value: Boolean);
begin
  AllowGrayed := Value;
end;

procedure TJvDynControlVCLCheckListBox.ControlSetChecked(Index: Integer; Value: Boolean);
begin
  Checked[Index] := Value;
end;

procedure TJvDynControlVCLCheckListBox.ControlSetItemEnabled(Index: Integer; Value: Boolean);
begin
  ItemEnabled[Index] := Value;
end;

procedure TJvDynControlVCLCheckListBox.ControlSetHeader(Index: Integer; Value: Boolean);
begin
  {$IFDEF COMPILER6_UP}
  {$IFDEF VCL}
  Header[Index] := Value;
  {$ENDIF VCL}
  {$ENDIF COMPILER6_UP}
end;

function TJvDynControlVCLCheckListBox.ControlGetHeader(Index: Integer): Boolean;
begin
  {$IFDEF COMPILER6_UP}
  {$IFDEF VCL}
  Result := Header[Index];
  {$ELSE}
  Result := False;
  {$ENDIF VCL}
  {$ELSE}
  Result := False;
  {$ENDIF COMPILER6_UP}
end;

procedure TJvDynControlVCLCheckListBox.ControlSetState(Index: Integer; Value: TCheckBoxState);
begin
  State[Index] := Value;
end;

function TJvDynControlVCLCheckListBox.ControlGetChecked(Index: Integer): Boolean;
begin
  Result := Checked[Index];
end;

function TJvDynControlVCLCheckListBox.ControlGetItemEnabled(Index: Integer): Boolean;
begin
  Result := ItemEnabled[Index];
end;


function TJvDynControlVCLCheckListBox.ControlGetState(Index: Integer): TCheckBoxState;
begin
  Result := State[Index];
end;

//=== { TJvDynControlVCLComboBox } ===========================================

procedure TJvDynControlVCLComboBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLComboBox.ControlSetCaption(const Value: string);
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
  if Style = csDropDownList then
    ItemIndex := Items.IndexOf(Value)
  else
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

procedure TJvDynControlVCLComboBox.ControlSetNewEntriesAllowed(Value: Boolean);
const
  Styles: array [Boolean] of TComboBoxStyle =
    (csDropDown, csDropDownList);
begin
  Style := Styles[Value];
end;

//=== { TJvDynControlVCLGroupBox } ==============================================

procedure TJvDynControlVCLGroupBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLGroupBox.ControlSetCaption(const Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlVCLGroupBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLGroupBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLGroupBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLGroupBox.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

//=== { TJvDynControlVCLPanel } ==============================================

procedure TJvDynControlVCLPanel.ControlSetDefaultProperties;
begin
  BevelInner := bvNone;
  BevelOuter := bvNone;
end;

procedure TJvDynControlVCLPanel.ControlSetCaption(const Value: string);
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

procedure TJvDynControlVCLPanel.ControlSetBorder(ABevelInner: TPanelBevel; ABevelOuter: TPanelBevel; ABevelWidth: Integer; ABorderStyle: TBorderStyle; ABorderWidth: Integer);
begin
  BorderWidth := ABorderWidth;
  BorderStyle := ABorderStyle;
  BevelInner  := ABevelInner;
  BevelOuter  := ABevelOuter;
  BevelWidth  := ABevelWidth;
end;

//=== { TJvDynControlVCLImage } ==============================================

procedure TJvDynControlVCLImage.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLImage.ControlSetCaption(const Value: string);
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

{$IFDEF VCL}
procedure TJvDynControlVCLImage.ControlSetProportional(Value: Boolean);
begin
  {$IFDEF COMPILER6_UP}
  Proportional := Value;
  {$ENDIF COMPILER6_UP}
end;
{$ENDIF VCL}

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

procedure TJvDynControlVCLImage.ControlSetGraphic(Value: TGraphic);
begin
  Picture.Assign(Value);
end;

function TJvDynControlVCLImage.ControlGetPicture: TPicture;
begin
  Result := Picture;
end;

//=== { TJvDynControlVCLScrollBox } ==========================================

procedure TJvDynControlVCLScrollBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLScrollBox.ControlSetCaption(const Value: string);
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

//=== { TJvDynControlVCLLabel } ==============================================

procedure TJvDynControlVCLLabel.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLLabel.ControlSetCaption(const Value: string);
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

procedure TJvDynControlVCLLabel.ControlSetWordWrap(Value: Boolean);
begin
  WordWrap := Value;
end;

{$IFDEF VCL}
//=== { TJvDynControlVCLStaticText } =========================================

procedure TJvDynControlVCLStaticText.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLStaticText.ControlSetCaption(const Value: string);
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

{$ENDIF VCL}

//=== { TJvDynControlVCLButton } =============================================

procedure TJvDynControlVCLButton.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLButton.ControlSetCaption(const Value: string);
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

function DynControlEngineVCL: TJvDynControlEngine;
begin
  Result := IntDynControlEngineVCL;
end;

type
  TJvDynControlEngineVCL = class(TJvDynControlEngine)
    procedure RegisterControls; override;
  end;

procedure TJvDynControlEngineVCL.RegisterControls;
begin
  RegisterControl(jctLabel, TJvDynControlVCLLabel);
  {$IFDEF VCL}
  RegisterControl(jctStaticText, TJvDynControlVCLStaticText);
  {$ENDIF VCL}
  RegisterControl(jctButton, TJvDynControlVCLButton);
  RegisterControl(jctScrollBox, TJvDynControlVCLScrollBox);
  RegisterControl(jctGroupBox, TJvDynControlVCLGroupBox);
  RegisterControl(jctPanel, TJvDynControlVCLPanel);
  RegisterControl(jctImage, TJvDynControlVCLImage);
  RegisterControl(jctCheckBox, TJvDynControlVCLCheckBox);
  RegisterControl(jctComboBox, TJvDynControlVCLComboBox);
  RegisterControl(jctListBox, TJvDynControlVCLListBox);
  RegisterControl(jctCheckListBox, TJvDynControlVCLCheckListBox);
  RegisterControl(jctRadioGroup, TJvDynControlVCLRadioGroup);
  {$IFDEF VCL}
  RegisterControl(jctDateTimeEdit, TJvDynControlVCLDateTimeEdit);
  RegisterControl(jctTimeEdit, TJvDynControlVCLTimeEdit);
  RegisterControl(jctDateEdit, TJvDynControlVCLDateEdit);
  {$ENDIF VCL}
  RegisterControl(jctEdit, TJvDynControlVCLMaskEdit);
//  RegisterControl(jctCalculateEdit, TJvDynControlVCLMaskEdit);
//  RegisterControl(jctSpinEdit, TJvDynControlVCLMaskEdit);
  RegisterControl(jctDirectoryEdit, TJvDynControlVCLDirectoryEdit);
  RegisterControl(jctFileNameEdit, TJvDynControlVCLFileNameEdit);
  RegisterControl(jctMemo, TJvDynControlVCLMemo);
  RegisterControl(jctButtonEdit, TJvDynControlVCLButtonEdit);
end;

initialization
  IntDynControlEngineVCL := TJvDynControlEngineVCL.Create;
  SetDefaultDynControlEngine(IntDynControlEngineVCL);

finalization
  FreeAndNil(IntDynControlEngineVCL);

end.
