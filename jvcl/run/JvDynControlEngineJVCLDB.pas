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

unit JvDynControlEngineJVCLDB;

{$I jvcl.inc}
{$I crossplatform.inc}

interface

uses
  Classes, ExtCtrls, ExtDlgs, Graphics, Buttons, Controls, Dialogs, FileCtrl, Forms,
  DBCtrls, DB, JvDBGrid, StdCtrls, JvPanel, ComCtrls,
  JvDBControls, JvDBDateTimePicker, JvDBCombobox, JvDBImage,
  JvDynControlEngine, JvDynControlEngineDB,
  JvDynControlEngineIntf, JvDynControlEngineDBIntf;

type
  TJvDynControlJVCLDBEdit = class (TJvDBMaskEdit, IUnknown, IJvDynControl, IJvDynControlData, IJvDynControlReadOnly, IJvDynControlEdit, IJvDynControlDatabase)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    //IJvDynControlEdit
    procedure ControlSetPasswordChar(Value: Char);
    procedure ControlSetEditMask(const Value: string);

    //IJvDynControlDatabase
    procedure ControlSetDataSource(Value: TDataSource);
    function ControlGetDataSource: TDataSource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;

  TJvDynControlJVCLDBButtonEdit = class (TPanel, IUnknown, IJvDynControl, IJvDynControlData,
    IJvDynControlReadOnly, IJvDynControlEdit, IJvDynControlButtonEdit,
    IJvDynControlButton, IJvDynControlDatabase)
  private
    FEditControl: TJvDBMaskEdit;
    FButton: TBitBtn;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);
    procedure ControlSetHint(const Value: string);

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

    //IJvDynControlDatabase
    procedure ControlSetDataSource(Value: TDataSource);
    function ControlGetDataSource: TDataSource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;


  TJvDynControlJVCLDBFileNameEdit = class (TPanel, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlFileName, IJvDynControlReadOnly,
    IJvDynControlDatabase)
  private
    FEditControl: TJvDBMaskEdit;
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
    procedure ControlSetHint(const Value: string);

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

    //IJvDynControlDatabase
    procedure ControlSetDataSource(Value: TDataSource);
    function ControlGetDataSource: TDataSource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;

  TJvDynControlJVCLDBDirectoryEdit = class (TPanel, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlDirectory, IJvDynControlReadOnly,
    IJvDynControlDatabase)
  private
    FEditControl: TJvDBMaskEdit;
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
    procedure ControlSetHint(const Value: string);

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

    //IJvDynControlDatabase
    procedure ControlSetDataSource(Value: TDataSource);
    function ControlGetDataSource: TDataSource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;


  TJvDynControlJVCLDBCheckBox = class (TDBCheckBox, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlDatabase)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    //IJvDynControlDatabase
    procedure ControlSetDataSource(Value: TDataSource);
    function ControlGetDataSource: TDataSource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;

  TJvDynControlJVCLDBMemo = class (TDBMemo, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlMemo, IJvDynControlReadOnly,
    IJvDynControlDatabase)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);
    procedure ControlSetHint(const Value: string);

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

    //IJvDynControlDatabase
    procedure ControlSetDataSource(Value: TDataSource);
    function ControlGetDataSource: TDataSource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;

  TJvDynControlJVCLDBDateEdit = class(TJvDBDateTimePicker, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlDate, IJvDynControlDatabase)
  public
    procedure ControlSetDefaultProperties;

    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);
    procedure ControlSetHint(const Value: string);

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

    //IJvDynControlDatabase
    procedure ControlSetDataSource(Value: TDataSource);
    function ControlGetDataSource: TDataSource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;

  TJvDynControlJVCLDBTimeEdit = class(TJvDBDateTimePicker, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlTime, IJvDynControlDatabase)
  public
    procedure ControlSetDefaultProperties;

    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetFormat(const Value: string);

    //IJvDynControlDatabase
    procedure ControlSetDataSource(Value: TDataSource);
    function ControlGetDataSource: TDataSource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;


  TJvDynControlJVCLDBRadioGroup = class (TDBRadioGroup, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlRadioGroup, IJvDynControlDatabase)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);
    procedure ControlSetHint(const Value: string);

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

    //IJvDynControlDatabase
    procedure ControlSetDataSource(Value: TDataSource);
    function ControlGetDataSource: TDataSource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;

  TJvDynControlJVCLDBListBox = class (TDBListBox, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlDblClick, IJvDynControlDatabase)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);
    procedure ControlSetHint(const Value: string);

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

    //IJvDynControlDatabase
    procedure ControlSetDataSource(Value: TDataSource);
    function ControlGetDataSource: TDataSource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;


  TJvDynControlJVCLDBComboBox = class (TJvDBComboBox, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlComboBox, IJvDynControlDatabase)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);
    procedure ControlSetHint(const Value: string);

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

    //IJvDynControlDatabase
    procedure ControlSetDataSource(Value: TDataSource);
    function ControlGetDataSource: TDataSource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;

  TJvDynControlJVCLDBImage = class (TJvDBImage, IUnknown, IJvDynControl,
      IJvDynControlImage, IJvDynControlDatabase)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);
    procedure ControlSetHint(const Value: string);

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

    //IJvDynControlDatabase
    procedure ControlSetDataSource(Value: TDataSource);
    function ControlGetDataSource: TDataSource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;

  TJvDynControlJVCLDBText = class (TDBText, IUnknown, IJvDynControl, IJvDynControlDatabase)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    //IJvDynControlDatabase
    procedure ControlSetDataSource(Value: TDataSource);
    function ControlGetDataSource: TDataSource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;


  TJvDynControlJVCLDBGrid = class (TJvDBGrid, IUnknown, IJvDynControl, IJvDynControlDatabase)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    //IJvDynControlDatabase
    procedure ControlSetDataSource(Value: TDataSource);
    function ControlGetDataSource: TDataSource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;

  TJvDynControlJVCLDBNavigator = class (TDBNavigator, IUnknown, IJvDynControl, IJvDynControlDatabase)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    //IJvDynControlDatabase
    procedure ControlSetDataSource(Value: TDataSource);
    function ControlGetDataSource: TDataSource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;


function DynControlEngineJVCLDB: TJvDynControlEngineDB;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  SysUtils,
  JvDynControlEngineJVCL,
  JvConsts, JvJCLUtils;

var
  IntDynControlEngineJVCLDB: TJvDynControlEngineDB = nil;

//=== { TJvDynControlJVCLDBEdit } ===========================================

procedure TJvDynControlJVCLDBEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLDBEdit.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlJVCLDBEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlJVCLDBEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLDBEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLDBEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLDBEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLDBEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlJVCLDBEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlJVCLDBEdit.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlJVCLDBEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlJVCLDBEdit.ControlSetPasswordChar(Value: Char);
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

procedure TJvDynControlJVCLDBEdit.ControlSetEditMask(const Value: string);
begin
  //EditMask := Value;
end;

procedure TJvDynControlJVCLDBEdit.ControlSetDataSource(Value: TDataSource);
begin
  DataSource := Value;
end;

function TJvDynControlJVCLDBEdit.ControlGetDataSource: TDataSource;
begin
  Result := DataSource;
end;

procedure TJvDynControlJVCLDBEdit.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;


function TJvDynControlJVCLDBEdit.ControlGetDataField: string;
begin
  Result := Datafield;
end;

//=== { TJvDynControlJVCLDBButtonEdit } =========================================

constructor TJvDynControlJVCLDBButtonEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditControl := TJvDBMaskEdit.Create(AOwner);
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

destructor TJvDynControlJVCLDBButtonEdit.Destroy;
begin
  FreeAndNil(FEditControl);
  FreeAndNil(FButton);
  inherited Destroy;
end;

procedure TJvDynControlJVCLDBButtonEdit.ControlSetDefaultProperties;
begin
  Self.Caption := ' ';
end;

procedure TJvDynControlJVCLDBButtonEdit.ControlSetReadOnly(Value: Boolean);
begin
  FEditControl.ReadOnly := Value;
end;

procedure TJvDynControlJVCLDBButtonEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlJVCLDBButtonEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLDBButtonEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLDBButtonEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLDBButtonEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLDBButtonEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  FEditControl.OnChange := Value;
end;

procedure TJvDynControlJVCLDBButtonEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
  FEditControl.OnClick := Value;
end;

procedure TJvDynControlJVCLDBButtonEdit.ControlSetValue(Value: Variant);
begin
  FEditControl.Text := Value;
end;

function TJvDynControlJVCLDBButtonEdit.ControlGetValue: Variant;
begin
  Result := FEditControl.Text;
end;

{$IFDEF VisualCLX}
type
  TJvDBMaskEditAccessProtected = class(TJvDBMaskEdit);
{$ENDIF VisualCLX}

procedure TJvDynControlJVCLDBButtonEdit.ControlSetPasswordChar(Value: Char);
begin
  {$IFDEF VCL}
  FEditControl.PasswordChar := Value;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  if Value = #0 then
    TJvDBMaskEditAccessProtected(FEditControl).EchoMode := emNormal
  else
    TJvDBMaskEditAccessProtected(FEditControl).EchoMode := emPassword;
  {$ENDIF VisualCLX}
end;

procedure TJvDynControlJVCLDBButtonEdit.ControlSetEditMask(const Value: string);
begin
  //FEditControl.EditMask := Value;
end;

procedure TJvDynControlJVCLDBButtonEdit.ControlSetOnButtonClick(Value: TNotifyEvent);
begin
  FButton.OnClick:= Value;
end;

procedure TJvDynControlJVCLDBButtonEdit.ControlSetButtonCaption(const Value: string);
begin
  FButton.Caption := Value;
end;

procedure TJvDynControlJVCLDBButtonEdit.ControlSetGlyph(Value: TBitmap);
begin
  FButton.Glyph.Assign(Value);
end;

procedure TJvDynControlJVCLDBButtonEdit.ControlSetNumGlyphs(Value: Integer);
begin
  FButton.NumGlyphs := Value;
end;

procedure TJvDynControlJVCLDBButtonEdit.ControlSetLayout(Value: TButtonLayout);
begin
  FButton.Layout := Value;
end;

procedure TJvDynControlJVCLDBButtonEdit.ControlSetDataSource(Value: TDataSource);
begin
  FEditControl.DataSource := Value;
end;

function TJvDynControlJVCLDBButtonEdit.ControlGetDataSource: TDataSource;
begin
  Result := FEditControl.DataSource;
end;

procedure TJvDynControlJVCLDBButtonEdit.ControlSetDataField(const Value: string);
begin
  FEditControl.DataField := Value;
end;


function TJvDynControlJVCLDBButtonEdit.ControlGetDataField: string;
begin
  Result := FEditControl.Datafield;
end;


//=== { TJvDynControlJVCLDBFileNameEdit } =======================================

constructor TJvDynControlJVCLDBFileNameEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditControl := TJvDBMaskEdit.Create(AOwner);
  FEditControl.Parent := Self;
  FButton     := TBitBtn.Create(AOwner);
  FButton.Parent := Self;
  FButton.Align := alRight;
  FButton.OnClick := DefaultOnButtonClick;
  FButton.Caption := '...';
  Height      := FEditControl.Height;
  FButton.Width := Height;
  FEditControl.Align := alClient;
  FDialogOptions := [ofHideReadOnly,ofEnableSizing];
  BevelInner  := bvNone;
  BevelOuter  := bvNone;
  FDialogKind := jdkOpen;
end;

destructor TJvDynControlJVCLDBFileNameEdit.Destroy;
begin
  FreeAndNil(FEditControl);
  FreeAndNil(FButton);
  inherited Destroy;
end;

procedure TJvDynControlJVCLDBFileNameEdit.DefaultOnButtonClick(Sender: TObject);
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
  if FEditControl.CanFocus then
    FEditControl.SetFocus;
end;

procedure TJvDynControlJVCLDBFileNameEdit.ControlSetDefaultProperties;
begin
  Caption := ' ';
end;

procedure TJvDynControlJVCLDBFileNameEdit.ControlSetReadOnly(Value: Boolean);
begin
  FEditControl.ReadOnly := Value;
  FButton.Enabled := not Value;
end;

procedure TJvDynControlJVCLDBFileNameEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlJVCLDBFileNameEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLDBFileNameEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLDBFileNameEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  FEditControl.OnEnter := Value;
end;

procedure TJvDynControlJVCLDBFileNameEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  FEditControl.OnExit := Value;
end;

procedure TJvDynControlJVCLDBFileNameEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  FEditControl.OnChange := Value;
end;

procedure TJvDynControlJVCLDBFileNameEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlJVCLDBFileNameEdit.ControlSetValue(Value: Variant);
begin
  FEditControl.Text := Value;
end;

function TJvDynControlJVCLDBFileNameEdit.ControlGetValue: Variant;
begin
  Result := FEditControl.Text;
end;

// IJvDynControlFileName
procedure TJvDynControlJVCLDBFileNameEdit.ControlSetInitialDir(const Value: string);
begin
  FInitialDir := Value;
end;

procedure TJvDynControlJVCLDBFileNameEdit.ControlSetDefaultExt(const Value: string);
begin
  FDefaultExt := Value;
end;

procedure TJvDynControlJVCLDBFileNameEdit.ControlSetDialogTitle(const Value: string);
begin
  FDialogTitle := Value;
end;

procedure TJvDynControlJVCLDBFileNameEdit.ControlSetDialogOptions(Value: TOpenOptions);
begin
  FDialogOptions := Value;
end;

procedure TJvDynControlJVCLDBFileNameEdit.ControlSetFilter(const Value: string);
begin
  FFilter := Value;
end;

procedure TJvDynControlJVCLDBFileNameEdit.ControlSetFilterIndex(Value: Integer);
begin
  FFilterIndex := Value;
end;

procedure TJvDynControlJVCLDBFileNameEdit.ControlSetDialogKind(Value: TJvDynControlFileNameDialogKind);
begin
  FDialogKind := Value;
end;


procedure TJvDynControlJVCLDBFileNameEdit.ControlSetDataSource(Value: TDataSource);
begin
  FEditControl.DataSource := Value;
end;

function TJvDynControlJVCLDBFileNameEdit.ControlGetDataSource: TDataSource;
begin
  Result := FEditControl.DataSource;
end;

procedure TJvDynControlJVCLDBFileNameEdit.ControlSetDataField(const Value: string);
begin
  FEditControl.DataField := Value;
end;

function TJvDynControlJVCLDBFileNameEdit.ControlGetDataField: string;
begin
  Result := FEditControl.Datafield;
end;


//=== { TJvDynControlJVCLDBDirectoryEdit } ======================================

constructor TJvDynControlJVCLDBDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditControl := TJvDBMaskEdit.Create(AOwner);
  FEditControl.Parent := Self;
  FButton := TBitBtn.Create(AOwner);
  FButton.Parent := Self;
  FButton.Align := alRight;
  FButton.OnClick := DefaultOnButtonClick;
  FButton.Caption := '...';
  Height := FEditControl.Height;
  FButton.Width := Height;
  FEditControl.Align := alClient;
  BevelInner := bvNone;
  BevelOuter := bvNone;
end;

destructor TJvDynControlJVCLDBDirectoryEdit.Destroy;
begin
  FreeAndNil(FEditControl);
  FreeAndNil(FButton);
  inherited Destroy;
end;

procedure TJvDynControlJVCLDBDirectoryEdit.DefaultOnButtonClick(Sender: TObject);
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
  if Dir = '' then
    if FInitialDir <> '' then
      Dir := FInitialDir
    else
      Dir := PathDelim;
  if not DirectoryExists(Dir) then
    Dir := PathDelim;
  {$IFDEF VCL}
  if SelectDirectory(Dir, Opt, HelpContext) then
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  {$IFDEF MSWINDOWS}
  if SelectDirectory('', '', Dir) then
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  if SelectDirectory('', '/', Dir, False) then
  {$ENDIF UNIX}
  {$ENDIF VisualCLX}
    ControlSetValue(Dir);
  if FEditControl.CanFocus then
    FEditControl.SetFocus;
end;

procedure TJvDynControlJVCLDBDirectoryEdit.ControlSetDefaultProperties;
begin
  Self.Caption := ' ';
end;

procedure TJvDynControlJVCLDBDirectoryEdit.ControlSetReadOnly(Value: Boolean);
begin
  FEditControl.ReadOnly := Value;
  FButton.Enabled := not Value;
end;

procedure TJvDynControlJVCLDBDirectoryEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlJVCLDBDirectoryEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLDBDirectoryEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLDBDirectoryEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  FEditControl.OnEnter := Value;
end;

procedure TJvDynControlJVCLDBDirectoryEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  FEditControl.OnExit := Value;
end;

procedure TJvDynControlJVCLDBDirectoryEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  FEditControl.OnChange := Value;
end;

procedure TJvDynControlJVCLDBDirectoryEdit.ControlSetOnClick(Value: TNotifyEvent);
begin

end;

procedure TJvDynControlJVCLDBDirectoryEdit.ControlSetValue(Value: Variant);
begin
  FEditControl.Text := Value;
end;

function TJvDynControlJVCLDBDirectoryEdit.ControlGetValue: Variant;
begin
  Result := FEditControl.Text;
end;

procedure TJvDynControlJVCLDBDirectoryEdit.ControlSetInitialDir(const Value: string);
begin
  FInitialDir := Value;
end;

procedure TJvDynControlJVCLDBDirectoryEdit.ControlSetDialogTitle(const Value: string);
begin
  FDialogTitle := Value;
end;

{$IFDEF VCL}
procedure TJvDynControlJVCLDBDirectoryEdit.ControlSetDialogOptions(Value: TSelectDirOpts);
begin
  FDialogOptions := Value;
end;
{$ENDIF VCL}


procedure TJvDynControlJVCLDBDirectoryEdit.ControlSetDataSource(Value: TDataSource);
begin
  FEditControl.DataSource := Value;
end;

function TJvDynControlJVCLDBDirectoryEdit.ControlGetDataSource: TDataSource;
begin
  Result := FEditControl.DataSource;
end;

procedure TJvDynControlJVCLDBDirectoryEdit.ControlSetDataField(const Value: string);
begin
  FEditControl.DataField := Value;
end;

function TJvDynControlJVCLDBDirectoryEdit.ControlGetDataField: string;
begin
  Result := FEditControl.Datafield;
end;



//=== { TJvDynControlJVCLDBCheckBox } ===========================================

procedure TJvDynControlJVCLDBCheckBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLDBCheckBox.ControlSetCaption(const Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlJVCLDBCheckBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLDBCheckBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLDBCheckBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLDBCheckBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLDBCheckBox.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLDBCheckBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLDBCheckBox.ControlSetValue(Value: Variant);
begin
  if VarType(Value) = varBoolean then
    Checked := Value
  else
    Checked := UpperCase(Value) = 'TRUE';
end;

function TJvDynControlJVCLDBCheckBox.ControlGetValue: Variant;
begin
  Result := Checked;
end;

procedure TJvDynControlJVCLDBCheckBox.ControlSetDataSource(Value: TDataSource);
begin
  DataSource := Value;
end;

function TJvDynControlJVCLDBCheckBox.ControlGetDataSource: TDataSource;
begin
  Result := DataSource;
end;

procedure TJvDynControlJVCLDBCheckBox.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;

function TJvDynControlJVCLDBCheckBox.ControlGetDataField: string;
begin
  Result := DataField;
end;

//=== { TJvDynControlJVCLDBMemo } ===============================================

procedure TJvDynControlJVCLDBMemo.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLDBMemo.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlJVCLDBMemo.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlJVCLDBMemo.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLDBMemo.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLDBMemo.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLDBMemo.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLDBMemo.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlJVCLDBMemo.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;               

procedure TJvDynControlJVCLDBMemo.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlJVCLDBMemo.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlJVCLDBMemo.ControlSetSorted(Value: Boolean);
begin
end;

procedure TJvDynControlJVCLDBMemo.ControlSetItems(Value: TStrings);
begin
  Lines.Assign(Value);
end;

function TJvDynControlJVCLDBMemo.ControlGetItems: TStrings;
begin
  Result := Lines;
end;

procedure TJvDynControlJVCLDBMemo.ControlSetWantTabs(Value: Boolean);
begin
  WantTabs := Value;
end;

procedure TJvDynControlJVCLDBMemo.ControlSetWantReturns(Value: Boolean);
begin
  WantReturns := Value;
end;

procedure TJvDynControlJVCLDBMemo.ControlSetWordWrap(Value: Boolean);
begin
  WordWrap := Value;
end;

procedure TJvDynControlJVCLDBMemo.ControlSetScrollBars(Value: TScrollStyle);
begin
  ScrollBars := Value;
end;

procedure TJvDynControlJVCLDBMemo.ControlSetDataSource(Value: TDataSource);
begin
  DataSource := Value;
end;

function TJvDynControlJVCLDBMemo.ControlGetDataSource: TDataSource;
begin
  Result := DataSource;
end;

procedure TJvDynControlJVCLDBMemo.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;

function TJvDynControlJVCLDBMemo.ControlGetDataField: string;
begin
  Result := DataField;
end;


//=== { TJvDynControlJVCLDBDateEdit } ==========================================

procedure TJvDynControlJVCLDBDateEdit.ControlSetDefaultProperties;
begin
  DateFormat := dfShort;
  DateMode := dmComboBox;
  Kind := dtkDate;
end;

procedure TJvDynControlJVCLDBDateEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlJVCLDBDateEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLDBDateEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLDBDateEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLDBDateEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLDBDateEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlJVCLDBDateEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlJVCLDBDateEdit.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlJVCLDBDateEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

// IJvDynControlDate

procedure TJvDynControlJVCLDBDateEdit.ControlSetMinDate(Value: TDateTime);
begin
  MinDate := Value;
end;

procedure TJvDynControlJVCLDBDateEdit.ControlSetMaxDate(Value: TDateTime);
begin
  MaxDate := Value;
end;

procedure TJvDynControlJVCLDBDateEdit.ControlSetFormat(const Value: string);
begin
  {$IFDEF COMPILER6_UP}
  Format := Value;
  {$ENDIF COMPILER6_UP}
end;

procedure TJvDynControlJVCLDBDateEdit.ControlSetDataSource(Value: TDataSource);
begin
  DataSource := Value;
end;

function TJvDynControlJVCLDBDateEdit.ControlGetDataSource: TDataSource;
begin
  Result := DataSource;
end;

procedure TJvDynControlJVCLDBDateEdit.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;

function TJvDynControlJVCLDBDateEdit.ControlGetDataField: string;
begin
  Result := DataField;
end;

//=== { TJvDynControlJVCLDBTimeEdit } ==========================================

procedure TJvDynControlJVCLDBTimeEdit.ControlSetDefaultProperties;
begin
  DateFormat := dfShort;
  Kind := dtkTime;
  DateMode := dmUpDown;
end;

procedure TJvDynControlJVCLDBTimeEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlJVCLDBTimeEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLDBTimeEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLDBTimeEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLDBTimeEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLDBTimeEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlJVCLDBTimeEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlJVCLDBTimeEdit.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlJVCLDBTimeEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlJVCLDBTimeEdit.ControlSetFormat(const Value: string);
begin
  {$IFDEF COMPILER6_UP}
  Format := Value;
  {$ENDIF COMPILER6_UP}
end;

procedure TJvDynControlJVCLDBTimeEdit.ControlSetDataSource(Value: TDataSource);
begin
  DataSource := Value;
end;

function TJvDynControlJVCLDBTimeEdit.ControlGetDataSource: TDataSource;
begin
  Result := DataSource;
end;

procedure TJvDynControlJVCLDBTimeEdit.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;

function TJvDynControlJVCLDBTimeEdit.ControlGetDataField: string;
begin
  Result := DataField;
end;


//=== { TJvDynControlJVCLDBRadioGroup } =========================================

procedure TJvDynControlJVCLDBRadioGroup.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLDBRadioGroup.ControlSetCaption(const Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlJVCLDBRadioGroup.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLDBRadioGroup.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLDBRadioGroup.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLDBRadioGroup.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLDBRadioGroup.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLDBRadioGroup.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLDBRadioGroup.ControlSetValue(Value: Variant);
begin
  if VarIsInt(Value) then
    ItemIndex := Value
  else
    ItemIndex := Items.IndexOf(Value);
end;

function TJvDynControlJVCLDBRadioGroup.ControlGetValue: Variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlJVCLDBRadioGroup.ControlSetSorted(Value: Boolean);
begin
end;

procedure TJvDynControlJVCLDBRadioGroup.ControlSetItems(Value: TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlJVCLDBRadioGroup.ControlGetItems: TStrings;
begin
  Result := Items;
end;

procedure TJvDynControlJVCLDBRadioGroup.ControlSetColumns(Value: Integer);
begin
  Columns := Value;
end;

procedure TJvDynControlJVCLDBRadioGroup.ControlSetDataSource(Value: TDataSource);
begin
  DataSource := Value;
end;

function TJvDynControlJVCLDBRadioGroup.ControlGetDataSource: TDataSource;
begin
  Result := DataSource;
end;

procedure TJvDynControlJVCLDBRadioGroup.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;

function TJvDynControlJVCLDBRadioGroup.ControlGetDataField: string;
begin
  Result := DataField;
end;


//=== { TJvDynControlJVCLDBListBox } ============================================

procedure TJvDynControlJVCLDBListBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLDBListBox.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlJVCLDBListBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLDBListBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLDBListBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLDBListBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLDBListBox.ControlSetOnChange(Value: TNotifyEvent);
begin
//  OnChange := Value;
end;

procedure TJvDynControlJVCLDBListBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLDBListBox.ControlSetValue(Value: Variant);
begin
  if VarIsInt(Value) then
    ItemIndex := Value
  else
    ItemIndex := Items.IndexOf(Value);
end;

function TJvDynControlJVCLDBListBox.ControlGetValue: Variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlJVCLDBListBox.ControlSetSorted(Value: Boolean);
begin
  Sorted := Value;
end;

procedure TJvDynControlJVCLDBListBox.ControlSetItems(Value: TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlJVCLDBListBox.ControlGetItems: TStrings;
begin
  Result := Items;
end;

procedure TJvDynControlJVCLDBListBox.ControlSetOnDblClick(Value: TNotifyEvent);
begin
  OnDblClick := Value;
end;

procedure TJvDynControlJVCLDBListBox.ControlSetDataSource(Value: TDataSource);
begin
  DataSource := Value;
end;

function TJvDynControlJVCLDBListBox.ControlGetDataSource: TDataSource;
begin
  Result := DataSource;
end;

procedure TJvDynControlJVCLDBListBox.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;

function TJvDynControlJVCLDBListBox.ControlGetDataField: string;
begin
  Result := DataField;
end;


//=== { TJvDynControlJVCLDBComboBox } ===========================================

procedure TJvDynControlJVCLDBComboBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLDBComboBox.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlJVCLDBComboBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLDBComboBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLDBComboBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLDBComboBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLDBComboBox.ControlSetOnChange(Value: TNotifyEvent);
begin
//  OnChange := Value;
end;

procedure TJvDynControlJVCLDBComboBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLDBComboBox.ControlSetValue(Value: Variant);
begin
  if Style = csDropDownList then
    ItemIndex := Items.IndexOf(Value)
  else
    Text := Value;
end;

function TJvDynControlJVCLDBComboBox.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlJVCLDBComboBox.ControlSetSorted(Value: Boolean);
begin
  Sorted := Value;
end;

procedure TJvDynControlJVCLDBComboBox.ControlSetItems(Value: TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlJVCLDBComboBox.ControlGetItems: TStrings;
begin
  Result := Items;
end;

procedure TJvDynControlJVCLDBComboBox.ControlSetNewEntriesAllowed(Value: Boolean);
const
  Styles: array [Boolean] of TComboBoxStyle =
    (csDropDownList, csDropDown);
begin
  Style := Styles[Value];
end;

procedure TJvDynControlJVCLDBComboBox.ControlSetDataSource(Value: TDataSource);
begin
  DataSource := Value;
end;

function TJvDynControlJVCLDBComboBox.ControlGetDataSource: TDataSource;
begin
  Result := DataSource;
end;

procedure TJvDynControlJVCLDBComboBox.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;

function TJvDynControlJVCLDBComboBox.ControlGetDataField: string;
begin
  Result := DataField;
end;


//=== { TJvDynControlJVCLDBImage } ==============================================

procedure TJvDynControlJVCLDBImage.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLDBImage.ControlSetCaption(const Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlJVCLDBImage.ControlSetTabOrder(Value: Integer);
begin
//  TabOrder := Value;
end;

procedure TJvDynControlJVCLDBImage.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLDBImage.ControlSetOnEnter(Value: TNotifyEvent);
begin
//  OnEnter := Value;
end;

procedure TJvDynControlJVCLDBImage.ControlSetOnExit(Value: TNotifyEvent);
begin
//  OnExit := Value;
end;

procedure TJvDynControlJVCLDBImage.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLDBImage.ControlSetAutoSize(Value: Boolean);
begin
  AutoSize := Value;
end;

procedure TJvDynControlJVCLDBImage.ControlSetIncrementalDisplay(Value: Boolean);
begin
//  IncrementalDisplay := Value;
end;

procedure TJvDynControlJVCLDBImage.ControlSetCenter(Value: Boolean);
begin
  Center := Value;
end;

{$IFDEF VCL}
procedure TJvDynControlJVCLDBImage.ControlSetProportional(Value: Boolean);
begin
  {$IFDEF COMPILER6_UP}
//  Proportional := Value;
  {$ENDIF COMPILER6_UP}
end;
{$ENDIF VCL}

procedure TJvDynControlJVCLDBImage.ControlSetStretch(Value: Boolean);
begin
  Stretch := Value;
end;

procedure TJvDynControlJVCLDBImage.ControlSetTransparent(Value: Boolean);
begin
//  Transparent := Value;
end;

procedure TJvDynControlJVCLDBImage.ControlSetPicture(Value: TPicture);
begin
  Picture.Assign(Value);
end;

procedure TJvDynControlJVCLDBImage.ControlSetGraphic(Value: TGraphic);
begin
  Picture.Assign(Value);
end;

function TJvDynControlJVCLDBImage.ControlGetPicture: TPicture;
begin
  Result := Picture;
end;

procedure TJvDynControlJVCLDBImage.ControlSetDataSource(Value: TDataSource);
begin
  DataSource := Value;
end;

function TJvDynControlJVCLDBImage.ControlGetDataSource: TDataSource;
begin
  Result := DataSource;
end;

procedure TJvDynControlJVCLDBImage.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;

function TJvDynControlJVCLDBImage.ControlGetDataField: string;
begin
  Result := DataField;
end;


//=== { TJvDynControlJVCLDBText } =========================================

procedure TJvDynControlJVCLDBText.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLDBText.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlJVCLDBText.ControlSetTabOrder(Value: Integer);
begin
end;

procedure TJvDynControlJVCLDBText.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLDBText.ControlSetOnEnter(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlJVCLDBText.ControlSetOnExit(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlJVCLDBText.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlJVCLDBText.ControlSetDataSource(Value: TDataSource);
begin
  DataSource := Value;
end;

function TJvDynControlJVCLDBText.ControlGetDataSource: TDataSource;
begin
  Result := DataSource;
end;

procedure TJvDynControlJVCLDBText.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;

function TJvDynControlJVCLDBText.ControlGetDataField: string;
begin
  Result := DataField;
end;

//=== { TJvDynControlJVCLDBGrid } ==========================================

procedure TJvDynControlJVCLDBGrid.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLDBGrid.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlJVCLDBGrid.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLDBGrid.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLDBGrid.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLDBGrid.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLDBGrid.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlJVCLDBGrid.ControlSetDataSource(Value: TDataSource);
begin
  DataSource := Value;
end;

function TJvDynControlJVCLDBGrid.ControlGetDataSource: TDataSource;
begin
  Result := DataSource;
end;

procedure TJvDynControlJVCLDBGrid.ControlSetDataField(const Value: string);
begin
end;

function TJvDynControlJVCLDBGrid.ControlGetDataField: string;
begin
  Result := '';
end;

//=== { TJvDynControlJVCLDBNavigator } ==========================================

procedure TJvDynControlJVCLDBNavigator.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLDBNavigator.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlJVCLDBNavigator.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLDBNavigator.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLDBNavigator.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLDBNavigator.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLDBNavigator.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlJVCLDBNavigator.ControlSetDataSource(Value: TDataSource);
begin
  DataSource := Value;
end;

function TJvDynControlJVCLDBNavigator.ControlGetDataSource: TDataSource;
begin
  Result := DataSource;
end;

procedure TJvDynControlJVCLDBNavigator.ControlSetDataField(const Value: string);
begin
end;

function TJvDynControlJVCLDBNavigator.ControlGetDataField: string;
begin
  Result := '';
end;

//=== { TJvDynControlEngineJVCLDB } =============================================

function DynControlEngineJVCLDB: TJvDynControlEngineDB;
begin
  Result := IntDynControlEngineJVCLDB;
end;

type
  TJvDynControlEngineJVCLDB = class(TJvDynControlEngineDB)
  public
    procedure RegisterControls; override;
  end;

procedure TJvDynControlEngineJVCLDB.RegisterControls;
begin
  RegisterControlType(jctDBText, TJvDynControlJVCLDBText);
  RegisterControlType(jcTDBEdit, TJvDynControlJVCLDBEdit);
  RegisterControlType(jctDBImage, TJvDynControlJVCLDBImage);
  RegisterControlType(jctDBCheckBox, TJvDynControlJVCLDBCheckBox);
  RegisterControlType(jctDBComboBox, TJvDynControlJVCLDBComboBox);
  RegisterControlType(jctDBListBox, TJvDynControlJVCLDBListBox);
  RegisterControlType(jctDBRadioGroup, TJvDynControlJVCLDBRadioGroup);
  RegisterControlType(jctDBDateTimeEdit, TJvDynControlJVCLDBDateEdit);
  RegisterControlType(jctDBTimeEdit, TJvDynControlJVCLDBTimeEdit);
  RegisterControlType(jctDBDateEdit, TJvDynControlJVCLDBDateEdit);
////  RegisterControlType(jctDBCalculateEdit, TJvDynControlJVCLDBEdit);
////  RegisterControlType(jctDBSpinEdit, TJvDynControlJVCLDBEdit);
  RegisterControlType(jctDBDirectoryEdit, TJvDynControlJVCLDBDirectoryEdit);
  RegisterControlType(jctDBFileNameEdit, TJvDynControlJVCLDBFileNameEdit);
  RegisterControlType(jctDBMemo, TJvDynControlJVCLDBMemo);
  RegisterControlType(jctDBButtonEdit, TJvDynControlJVCLDBButtonEdit);
  RegisterControlType(jctDBGrid, TJvDynControlJVCLDBGrid);
  RegisterControlType(jctDBNavigator, TJvDynControlJVCLDBNavigator);
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

  IntDynControlEngineJVCLDB := TJvDynControlEngineJVCLDB.Create;
  SetDefaultDynControlEngineDB(IntDynControlEngineJVCLDB);

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  FreeAndNil(IntDynControlEngineJVCLDB);

end.
