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
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDynControlEngineVCLDB;

{$I jvcl.inc}
{$I crossplatform.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes, ExtCtrls, ExtDlgs, Graphics, Buttons, Controls, Dialogs, FileCtrl,
  Forms, DBCtrls, DB, DBGrids, StdCtrls,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  JvDynControlEngine, JvDynControlEngineDB, JvDynControlEngineIntf,
  JvDynControlEngineDBIntf;

type
  TJvDynControlVCLDBEdit = class(TDBEdit, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlReadOnly, IJvDynControlEdit,
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

    //IJvDynControlEdit
    procedure ControlSetPasswordChar(Value: Char);
    procedure ControlSetEditMask(const Value: string);

    //IJvDynControlDatabase
    procedure ControlSetDataSource(Value: TDataSource);
    function ControlGetDataSource: TDataSource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
    procedure ControlSetAnchors(Value : TAnchors);
  end;

  TJvDynControlVCLDBButtonEdit = class(TPanel, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlReadOnly, IJvDynControlEdit,
    IJvDynControlButtonEdit, IJvDynControlButton, IJvDynControlDatabase)
  private
    FEditControl: TDBEdit;
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
    procedure ControlSetDefault(Value: Boolean);
    procedure ControlSetCancel(Value: Boolean);

    //IJvDynControlDatabase
    procedure ControlSetDataSource(Value: TDataSource);
    function ControlGetDataSource: TDataSource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
    procedure ControlSetAnchors(Value : TAnchors);
  end;

  TJvDynControlVCLDBFileNameEdit = class(TPanel, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlFileName,
    IJvDynControlReadOnly, IJvDynControlDatabase)
  private
    FEditControl: TDBEdit;
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
    procedure ControlSetAnchors(Value : TAnchors);
  end;

  TJvDynControlVCLDBDirectoryEdit = class(TPanel, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlDirectory,
    IJvDynControlReadOnly, IJvDynControlDatabase)
  private
    FEditControl: TDBEdit;
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
    procedure ControlSetDialogOptions(Value: TSelectDirOpts);

    //IJvDynControlDatabase
    procedure ControlSetDataSource(Value: TDataSource);
    function ControlGetDataSource: TDataSource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
    procedure ControlSetAnchors(Value : TAnchors);
  end;

  TJvDynControlVCLDBCheckBox = class(TDBCheckBox, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlDatabase,
    IJvDynControlDBCheckbox)
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
    procedure ControlSetAnchors(Value : TAnchors);

    //IJvDynControlDBCheckbox
    procedure ControlSetValueChecked(Value: Variant);
    procedure ControlSetValueUnChecked(Value: Variant);
  end;

  TJvDynControlVCLDBMemo = class(TDBMemo, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlItems, IJvDynControlMemo,
    IJvDynControlReadOnly, IJvDynControlDatabase)
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
    procedure ControlSetAnchors(Value : TAnchors);

    //IJvDynControlFont
    function ControlGetFont: TFont;
    procedure ControlSetFont(Value: TFont);
  end;

  TJvDynControlVCLDBRadioGroup = class(TDBRadioGroup, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlItems,
    IJvDynControlRadioGroup, IJvDynControlDatabase)
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
    procedure ControlSetAnchors(Value : TAnchors);
  end;

  TJvDynControlVCLDBListBox = class(TDBListBox, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlItems, IJvDynControlDblClick,
    IJvDynControlDatabase)
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
    procedure ControlSetAnchors(Value : TAnchors);
  end;

  TJvDynControlVCLDBComboBox = class(TDBComboBox, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlItems, IJvDynControlComboBox,
    IJvDynControlDatabase)
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
    procedure ControlSetAnchors(Value : TAnchors);
  end;

  TJvDynControlVCLDBImage = class(TDBImage, IUnknown,
    IJvDynControl, IJvDynControlImage, IJvDynControlDatabase)
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
    procedure ControlSetProportional(Value: Boolean);
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
    procedure ControlSetAnchors(Value : TAnchors);
  end;

  TJvDynControlVCLDBText = class(TDBText, IUnknown,
    IJvDynControl, IJvDynControlDatabase)
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
    procedure ControlSetAnchors(Value : TAnchors);
  end;

  TJvDynControlVCLDBGrid = class(TDBGrid, IUnknown,
    IJvDynControl, IJvDynControlDatabase)
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
    procedure ControlSetAnchors(Value : TAnchors);
  end;

  TJvDynControlVCLDBNavigator = class(TDBNavigator, IUnknown,
    IJvDynControl, IJvDynControlDatabase)
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
    procedure ControlSetAnchors(Value : TAnchors);
  end;

  TJvDynControlEngineVCLDB = class(TJvDynControlEngineDB)
  public
    function GetDataSourceFromDataComponent(ADataComponent: TComponent): TDataSource; override;
    procedure RegisterControls; override;
  end;

function DynControlEngineVCLDB: TJvDynControlEngineDB;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  Variants, SysUtils,
  JvDynControlEngineTools, JvJCLUtils;

var
  IntDynControlEngineVCLDB: TJvDynControlEngineDB = nil;

//=== { TJvDynControlVCLDBEdit } =============================================

procedure TJvDynControlVCLDBEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLDBEdit.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlVCLDBEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlVCLDBEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLDBEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLDBEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLDBEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLDBEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlVCLDBEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLDBEdit.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlVCLDBEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlVCLDBEdit.ControlSetPasswordChar(Value: Char);
begin
  PasswordChar := Value;
end;

procedure TJvDynControlVCLDBEdit.ControlSetEditMask(const Value: string);
begin
  //EditMask := Value;
end;

procedure TJvDynControlVCLDBEdit.ControlSetDataSource(Value: TDataSource);
begin
  DataSource := Value;
end;

function TJvDynControlVCLDBEdit.ControlGetDataSource: TDataSource;
begin
  Result := DataSource;
end;

procedure TJvDynControlVCLDBEdit.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;

function TJvDynControlVCLDBEdit.ControlGetDataField: string;
begin
  Result := DataField;
end;

procedure TJvDynControlVCLDBEdit.ControlSetAnchors(Value : TAnchors);
begin
  Anchors := Value;
end;

//=== { TJvDynControlVCLDBButtonEdit } =======================================

constructor TJvDynControlVCLDBButtonEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditControl := TDBEdit.Create(AOwner);
  FEditControl.Parent := Self;
  FButton := TBitBtn.Create(AOwner);
  FButton.Parent := Self;
  FButton.Align := alRight;
  FButton.Caption := '...';
  Height := FEditControl.Height;
  FButton.Width := Height;
  FEditControl.Align := alClient;
  BevelInner := bvNone;
  BevelOuter := bvNone;
end;

destructor TJvDynControlVCLDBButtonEdit.Destroy;
begin
  FreeAndNil(FEditControl);
  FreeAndNil(FButton);
  inherited Destroy;
end;

procedure TJvDynControlVCLDBButtonEdit.ControlSetDefaultProperties;
begin
  Self.Caption := ' ';
end;

procedure TJvDynControlVCLDBButtonEdit.ControlSetReadOnly(Value: Boolean);
begin
  FEditControl.ReadOnly := Value;
end;

procedure TJvDynControlVCLDBButtonEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlVCLDBButtonEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLDBButtonEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLDBButtonEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLDBButtonEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLDBButtonEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  FEditControl.OnChange := Value;
end;

procedure TJvDynControlVCLDBButtonEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
  FEditControl.OnClick := Value;
end;

procedure TJvDynControlVCLDBButtonEdit.ControlSetValue(Value: Variant);
begin
  FEditControl.Text := Value;
end;

function TJvDynControlVCLDBButtonEdit.ControlGetValue: Variant;
begin
  Result := FEditControl.Text;
end;



procedure TJvDynControlVCLDBButtonEdit.ControlSetPasswordChar(Value: Char);
begin
  FEditControl.PasswordChar := Value;
end;

procedure TJvDynControlVCLDBButtonEdit.ControlSetEditMask(const Value: string);
begin
  //FEditControl.EditMask := Value;
end;

procedure TJvDynControlVCLDBButtonEdit.ControlSetOnButtonClick(Value: TNotifyEvent);
begin
  FButton.OnClick := Value;
end;

procedure TJvDynControlVCLDBButtonEdit.ControlSetButtonCaption(const Value: string);
begin
  FButton.Caption := Value;
end;

procedure TJvDynControlVCLDBButtonEdit.ControlSetGlyph(Value: TBitmap);
begin
  FButton.Glyph.Assign(Value);
end;

procedure TJvDynControlVCLDBButtonEdit.ControlSetNumGlyphs(Value: Integer);
begin
  FButton.NumGlyphs := Value;
end;

procedure TJvDynControlVCLDBButtonEdit.ControlSetLayout(Value: TButtonLayout);
begin
  FButton.Layout := Value;
end;

procedure TJvDynControlVCLDBButtonEdit.ControlSetDefault(Value: Boolean);
begin
  FButton.Default := Value;
end;

procedure TJvDynControlVCLDBButtonEdit.ControlSetCancel(Value: Boolean);
begin
  FButton.Cancel := Value;
end;


procedure TJvDynControlVCLDBButtonEdit.ControlSetDataSource(Value: TDataSource);
begin
  FEditControl.DataSource := Value;
end;

function TJvDynControlVCLDBButtonEdit.ControlGetDataSource: TDataSource;
begin
  Result := FEditControl.DataSource;
end;

procedure TJvDynControlVCLDBButtonEdit.ControlSetDataField(const Value: string);
begin
  FEditControl.DataField := Value;
end;

function TJvDynControlVCLDBButtonEdit.ControlGetDataField: string;
begin
  Result := FEditControl.DataField;
end;

procedure TJvDynControlVCLDBButtonEdit.ControlSetAnchors(Value : TAnchors);
begin
  Anchors := Value;
end;

//=== { TJvDynControlVCLDBFileNameEdit } =====================================

constructor TJvDynControlVCLDBFileNameEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditControl := TDBEdit.Create(AOwner);
  FEditControl.Parent := Self;
  FButton := TBitBtn.Create(AOwner);
  FButton.Parent := Self;
  FButton.Align := alRight;
  FButton.OnClick := DefaultOnButtonClick;
  FButton.Caption := '...';
  Height := FEditControl.Height;
  FButton.Width := Height;
  FEditControl.Align := alClient;
  FDialogOptions := [ofHideReadOnly,ofEnableSizing];
  BevelInner := bvNone;
  BevelOuter := bvNone;
  FDialogKind := jdkOpen;
end;

destructor TJvDynControlVCLDBFileNameEdit.Destroy;
begin
  FreeAndNil(FEditControl);
  FreeAndNil(FButton);
  inherited Destroy;
end;

procedure TJvDynControlVCLDBFileNameEdit.DefaultOnButtonClick(Sender: TObject);
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

procedure TJvDynControlVCLDBFileNameEdit.ControlSetDefaultProperties;
begin
  Caption := ' ';
end;

procedure TJvDynControlVCLDBFileNameEdit.ControlSetReadOnly(Value: Boolean);
begin
  FEditControl.ReadOnly := Value;
  FButton.Enabled := not Value;
end;

procedure TJvDynControlVCLDBFileNameEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlVCLDBFileNameEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLDBFileNameEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLDBFileNameEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  FEditControl.OnEnter := Value;
end;

procedure TJvDynControlVCLDBFileNameEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  FEditControl.OnExit := Value;
end;

procedure TJvDynControlVCLDBFileNameEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  FEditControl.OnChange := Value;
end;

procedure TJvDynControlVCLDBFileNameEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLDBFileNameEdit.ControlSetValue(Value: Variant);
begin
  FEditControl.Text := Value;
end;

function TJvDynControlVCLDBFileNameEdit.ControlGetValue: Variant;
begin
  Result := FEditControl.Text;
end;

// IJvDynControlFileName
procedure TJvDynControlVCLDBFileNameEdit.ControlSetInitialDir(const Value: string);
begin
  FInitialDir := Value;
end;

procedure TJvDynControlVCLDBFileNameEdit.ControlSetDefaultExt(const Value: string);
begin
  FDefaultExt := Value;
end;

procedure TJvDynControlVCLDBFileNameEdit.ControlSetDialogTitle(const Value: string);
begin
  FDialogTitle := Value;
end;

procedure TJvDynControlVCLDBFileNameEdit.ControlSetDialogOptions(Value: TOpenOptions);
begin
  FDialogOptions := Value;
end;

procedure TJvDynControlVCLDBFileNameEdit.ControlSetFilter(const Value: string);
begin
  FFilter := Value;
end;

procedure TJvDynControlVCLDBFileNameEdit.ControlSetFilterIndex(Value: Integer);
begin
  FFilterIndex := Value;
end;

procedure TJvDynControlVCLDBFileNameEdit.ControlSetDialogKind(Value: TJvDynControlFileNameDialogKind);
begin
  FDialogKind := Value;
end;

procedure TJvDynControlVCLDBFileNameEdit.ControlSetDataSource(Value: TDataSource);
begin
  FEditControl.DataSource := Value;
end;

function TJvDynControlVCLDBFileNameEdit.ControlGetDataSource: TDataSource;
begin
  Result := FEditControl.DataSource;
end;

procedure TJvDynControlVCLDBFileNameEdit.ControlSetDataField(const Value: string);
begin
  FEditControl.DataField := Value;
end;

function TJvDynControlVCLDBFileNameEdit.ControlGetDataField: string;
begin
  Result := FEditControl.DataField;
end;

procedure TJvDynControlVCLDBFileNameEdit.ControlSetAnchors(Value : TAnchors);
begin
  Anchors := Value;
end;

//=== { TJvDynControlVCLDBDirectoryEdit } ====================================

constructor TJvDynControlVCLDBDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditControl := TDBEdit.Create(AOwner);
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

destructor TJvDynControlVCLDBDirectoryEdit.Destroy;
begin
  FreeAndNil(FEditControl);
  FreeAndNil(FButton);
  inherited Destroy;
end;

procedure TJvDynControlVCLDBDirectoryEdit.DefaultOnButtonClick(Sender: TObject);
var
  Opt: TSelectDirOpts;
  Dir: string;
begin
  Dir := ControlGetValue;
  if Dir = '' then
    if FInitialDir <> '' then
      Dir := FInitialDir
    else
      Dir := PathDelim;
  if not DirectoryExists(Dir) then
    Dir := PathDelim;
  if SelectDirectory(Dir, Opt, HelpContext) then
    ControlSetValue(Dir);
  if FEditControl.CanFocus then
    FEditControl.SetFocus;
end;

procedure TJvDynControlVCLDBDirectoryEdit.ControlSetDefaultProperties;
begin
  Self.Caption := ' ';
end;

procedure TJvDynControlVCLDBDirectoryEdit.ControlSetReadOnly(Value: Boolean);
begin
  FEditControl.ReadOnly := Value;
  FButton.Enabled := not Value;
end;

procedure TJvDynControlVCLDBDirectoryEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlVCLDBDirectoryEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLDBDirectoryEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLDBDirectoryEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  FEditControl.OnEnter := Value;
end;

procedure TJvDynControlVCLDBDirectoryEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  FEditControl.OnExit := Value;
end;

procedure TJvDynControlVCLDBDirectoryEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  FEditControl.OnChange := Value;
end;

procedure TJvDynControlVCLDBDirectoryEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLDBDirectoryEdit.ControlSetValue(Value: Variant);
begin
  FEditControl.Text := Value;
end;

function TJvDynControlVCLDBDirectoryEdit.ControlGetValue: Variant;
begin
  Result := FEditControl.Text;
end;

procedure TJvDynControlVCLDBDirectoryEdit.ControlSetInitialDir(const Value: string);
begin
  FInitialDir := Value;
end;

procedure TJvDynControlVCLDBDirectoryEdit.ControlSetDialogTitle(const Value: string);
begin
  FDialogTitle := Value;
end;


procedure TJvDynControlVCLDBDirectoryEdit.ControlSetDialogOptions(Value: TSelectDirOpts);
begin
  FDialogOptions := Value;
end;


procedure TJvDynControlVCLDBDirectoryEdit.ControlSetDataSource(Value: TDataSource);
begin
  FEditControl.DataSource := Value;
end;

function TJvDynControlVCLDBDirectoryEdit.ControlGetDataSource: TDataSource;
begin
  Result := FEditControl.DataSource;
end;

procedure TJvDynControlVCLDBDirectoryEdit.ControlSetDataField(const Value: string);
begin
  FEditControl.DataField := Value;
end;

function TJvDynControlVCLDBDirectoryEdit.ControlGetDataField: string;
begin
  Result := FEditControl.DataField;
end;

procedure TJvDynControlVCLDBDirectoryEdit.ControlSetAnchors(Value : TAnchors);
begin
  Anchors := Value;
end;

//=== { TJvDynControlVCLDBCheckBox } =========================================

procedure TJvDynControlVCLDBCheckBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLDBCheckBox.ControlSetCaption(const Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlVCLDBCheckBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLDBCheckBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLDBCheckBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLDBCheckBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLDBCheckBox.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLDBCheckBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLDBCheckBox.ControlSetValue(Value: Variant);
begin
  Checked := JvDynControlVariantToBoolean(Value);
end;

function TJvDynControlVCLDBCheckBox.ControlGetValue: Variant;
begin
  Result := Checked;
end;

procedure TJvDynControlVCLDBCheckBox.ControlSetDataSource(Value: TDataSource);
begin
  DataSource := Value;
end;

function TJvDynControlVCLDBCheckBox.ControlGetDataSource: TDataSource;
begin
  Result := DataSource;
end;

procedure TJvDynControlVCLDBCheckBox.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;

function TJvDynControlVCLDBCheckBox.ControlGetDataField: string;
begin
  Result := DataField;
end;

procedure TJvDynControlVCLDBCheckBox.ControlSetAnchors(Value : TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLDBCheckBox.ControlSetValueChecked(Value: Variant);
begin
  ValueChecked := Value;
end;

procedure TJvDynControlVCLDBCheckBox.ControlSetValueUnChecked(Value: Variant);
begin
  ValueUnChecked := Value;
end;


//=== { TJvDynControlVCLDBMemo } =============================================

procedure TJvDynControlVCLDBMemo.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLDBMemo.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlVCLDBMemo.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlVCLDBMemo.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLDBMemo.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLDBMemo.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLDBMemo.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLDBMemo.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlVCLDBMemo.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLDBMemo.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlVCLDBMemo.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlVCLDBMemo.ControlSetSorted(Value: Boolean);
begin
end;

procedure TJvDynControlVCLDBMemo.ControlSetItems(Value: TStrings);
begin
  Lines.Assign(Value);
end;

function TJvDynControlVCLDBMemo.ControlGetItems: TStrings;
begin
  Result := Lines;
end;

procedure TJvDynControlVCLDBMemo.ControlSetWantTabs(Value: Boolean);
begin
  WantTabs := Value;
end;

procedure TJvDynControlVCLDBMemo.ControlSetWantReturns(Value: Boolean);
begin
  WantReturns := Value;
end;

procedure TJvDynControlVCLDBMemo.ControlSetWordWrap(Value: Boolean);
begin
  WordWrap := Value;
end;

procedure TJvDynControlVCLDBMemo.ControlSetScrollBars(Value: TScrollStyle);
begin
  ScrollBars := Value;
end;

procedure TJvDynControlVCLDBMemo.ControlSetDataSource(Value: TDataSource);
begin
  DataSource := Value;
end;

function TJvDynControlVCLDBMemo.ControlGetDataSource: TDataSource;
begin
  Result := DataSource;
end;

procedure TJvDynControlVCLDBMemo.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;

function TJvDynControlVCLDBMemo.ControlGetDataField: string;
begin
  Result := DataField;
end;

function TJvDynControlVCLDBMemo.ControlGetFont: TFont;
begin
  Result := Font;
end;

procedure TJvDynControlVCLDBMemo.ControlSetAnchors(Value : TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLDBMemo.ControlSetFont(Value: TFont);
begin
  Font.Assign(Value);
end;

//=== { TJvDynControlVCLDBRadioGroup } =======================================

procedure TJvDynControlVCLDBRadioGroup.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLDBRadioGroup.ControlSetCaption(const Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlVCLDBRadioGroup.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLDBRadioGroup.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLDBRadioGroup.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLDBRadioGroup.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLDBRadioGroup.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLDBRadioGroup.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLDBRadioGroup.ControlSetValue(Value: Variant);
begin
  if VarIsInt(Value) then
    ItemIndex := Value
  else
    ItemIndex := Items.IndexOf(Value);
end;

function TJvDynControlVCLDBRadioGroup.ControlGetValue: Variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlVCLDBRadioGroup.ControlSetSorted(Value: Boolean);
begin
end;

procedure TJvDynControlVCLDBRadioGroup.ControlSetItems(Value: TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlVCLDBRadioGroup.ControlGetItems: TStrings;
begin
  Result := Items;
end;

procedure TJvDynControlVCLDBRadioGroup.ControlSetColumns(Value: Integer);
begin
  Columns := Value;
end;

procedure TJvDynControlVCLDBRadioGroup.ControlSetDataSource(Value: TDataSource);
begin
  DataSource := Value;
end;

function TJvDynControlVCLDBRadioGroup.ControlGetDataSource: TDataSource;
begin
  Result := DataSource;
end;

procedure TJvDynControlVCLDBRadioGroup.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;

function TJvDynControlVCLDBRadioGroup.ControlGetDataField: string;
begin
  Result := DataField;
end;

procedure TJvDynControlVCLDBRadioGroup.ControlSetAnchors(Value : TAnchors);
begin
  Anchors := Value;
end;

//=== { TJvDynControlVCLDBListBox } ==========================================

procedure TJvDynControlVCLDBListBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLDBListBox.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlVCLDBListBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLDBListBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLDBListBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLDBListBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLDBListBox.ControlSetOnChange(Value: TNotifyEvent);
begin
//  OnChange := Value;
end;

procedure TJvDynControlVCLDBListBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLDBListBox.ControlSetValue(Value: Variant);
begin
  if VarIsInt(Value) then
    ItemIndex := Value
  else
    ItemIndex := Items.IndexOf(Value);
end;

function TJvDynControlVCLDBListBox.ControlGetValue: Variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlVCLDBListBox.ControlSetSorted(Value: Boolean);
begin
  Sorted := Value;
end;

procedure TJvDynControlVCLDBListBox.ControlSetItems(Value: TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlVCLDBListBox.ControlGetItems: TStrings;
begin
  Result := Items;
end;

procedure TJvDynControlVCLDBListBox.ControlSetOnDblClick(Value: TNotifyEvent);
begin
  OnDblClick := Value;
end;

procedure TJvDynControlVCLDBListBox.ControlSetDataSource(Value: TDataSource);
begin
  DataSource := Value;
end;

function TJvDynControlVCLDBListBox.ControlGetDataSource: TDataSource;
begin
  Result := DataSource;
end;

procedure TJvDynControlVCLDBListBox.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;

function TJvDynControlVCLDBListBox.ControlGetDataField: string;
begin
  Result := DataField;
end;

procedure TJvDynControlVCLDBListBox.ControlSetAnchors(Value : TAnchors);
begin
  Anchors := Value;
end;

//=== { TJvDynControlVCLDBComboBox } =========================================

procedure TJvDynControlVCLDBComboBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLDBComboBox.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlVCLDBComboBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLDBComboBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLDBComboBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLDBComboBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLDBComboBox.ControlSetOnChange(Value: TNotifyEvent);
begin
//  OnChange := Value;
end;

procedure TJvDynControlVCLDBComboBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLDBComboBox.ControlSetValue(Value: Variant);
begin
  if Style = csDropDownList then
    ItemIndex := Items.IndexOf(Value)
  else
    Text := Value;
end;

function TJvDynControlVCLDBComboBox.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlVCLDBComboBox.ControlSetSorted(Value: Boolean);
begin
  Sorted := Value;
end;

procedure TJvDynControlVCLDBComboBox.ControlSetItems(Value: TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlVCLDBComboBox.ControlGetItems: TStrings;
begin
  Result := Items;
end;

procedure TJvDynControlVCLDBComboBox.ControlSetNewEntriesAllowed(Value: Boolean);
const
  Styles: array [Boolean] of TComboBoxStyle =
    (csDropDownList, csDropDown);
begin
  Style := Styles[Value];
end;

procedure TJvDynControlVCLDBComboBox.ControlSetDataSource(Value: TDataSource);
begin
  DataSource := Value;
end;

function TJvDynControlVCLDBComboBox.ControlGetDataSource: TDataSource;
begin
  Result := DataSource;
end;

procedure TJvDynControlVCLDBComboBox.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;

function TJvDynControlVCLDBComboBox.ControlGetDataField: string;
begin
  Result := DataField;
end;

procedure TJvDynControlVCLDBComboBox.ControlSetAnchors(Value : TAnchors);
begin
  Anchors := Value;
end;

//=== { TJvDynControlVCLDBImage } ============================================

procedure TJvDynControlVCLDBImage.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLDBImage.ControlSetCaption(const Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlVCLDBImage.ControlSetTabOrder(Value: Integer);
begin
//  TabOrder := Value;
end;

procedure TJvDynControlVCLDBImage.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLDBImage.ControlSetOnEnter(Value: TNotifyEvent);
begin
//  OnEnter := Value;
end;

procedure TJvDynControlVCLDBImage.ControlSetOnExit(Value: TNotifyEvent);
begin
//  OnExit := Value;
end;

procedure TJvDynControlVCLDBImage.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLDBImage.ControlSetAutoSize(Value: Boolean);
begin
  AutoSize := Value;
end;

procedure TJvDynControlVCLDBImage.ControlSetIncrementalDisplay(Value: Boolean);
begin
//  IncrementalDisplay := Value;
end;

procedure TJvDynControlVCLDBImage.ControlSetCenter(Value: Boolean);
begin
  Center := Value;
end;


procedure TJvDynControlVCLDBImage.ControlSetProportional(Value: Boolean);
begin
//  Proportional := Value;
end;


procedure TJvDynControlVCLDBImage.ControlSetStretch(Value: Boolean);
begin
  Stretch := Value;
end;

procedure TJvDynControlVCLDBImage.ControlSetTransparent(Value: Boolean);
begin
//  Transparent := Value;
end;

procedure TJvDynControlVCLDBImage.ControlSetPicture(Value: TPicture);
begin
  Picture.Assign(Value);
end;

procedure TJvDynControlVCLDBImage.ControlSetGraphic(Value: TGraphic);
begin
  Picture.Assign(Value);
end;

function TJvDynControlVCLDBImage.ControlGetPicture: TPicture;
begin
  Result := Picture;
end;

procedure TJvDynControlVCLDBImage.ControlSetDataSource(Value: TDataSource);
begin
  DataSource := Value;
end;

function TJvDynControlVCLDBImage.ControlGetDataSource: TDataSource;
begin
  Result := DataSource;
end;

procedure TJvDynControlVCLDBImage.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;

function TJvDynControlVCLDBImage.ControlGetDataField: string;
begin
  Result := DataField;
end;

procedure TJvDynControlVCLDBImage.ControlSetAnchors(Value : TAnchors);
begin
  Anchors := Value;
end;

//=== { TJvDynControlVCLDBText } =============================================

procedure TJvDynControlVCLDBText.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLDBText.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlVCLDBText.ControlSetTabOrder(Value: Integer);
begin
end;

procedure TJvDynControlVCLDBText.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLDBText.ControlSetOnEnter(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLDBText.ControlSetOnExit(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLDBText.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLDBText.ControlSetDataSource(Value: TDataSource);
begin
  DataSource := Value;
end;

function TJvDynControlVCLDBText.ControlGetDataSource: TDataSource;
begin
  Result := DataSource;
end;

procedure TJvDynControlVCLDBText.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;

function TJvDynControlVCLDBText.ControlGetDataField: string;
begin
  Result := DataField;
end;

procedure TJvDynControlVCLDBText.ControlSetAnchors(Value : TAnchors);
begin
  Anchors := Value;
end;

//=== { TJvDynControlVCLDBGrid } =============================================

procedure TJvDynControlVCLDBGrid.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLDBGrid.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlVCLDBGrid.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLDBGrid.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLDBGrid.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLDBGrid.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLDBGrid.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLDBGrid.ControlSetDataSource(Value: TDataSource);
begin
  DataSource := Value;
end;

function TJvDynControlVCLDBGrid.ControlGetDataSource: TDataSource;
begin
  Result := DataSource;
end;

procedure TJvDynControlVCLDBGrid.ControlSetDataField(const Value: string);
begin
end;

function TJvDynControlVCLDBGrid.ControlGetDataField: string;
begin
  Result := '';
end;

procedure TJvDynControlVCLDBGrid.ControlSetAnchors(Value : TAnchors);
begin
  Anchors := Value;
end;

//=== { TJvDynControlVCLDBNavigator } ========================================

procedure TJvDynControlVCLDBNavigator.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLDBNavigator.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlVCLDBNavigator.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLDBNavigator.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLDBNavigator.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLDBNavigator.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLDBNavigator.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLDBNavigator.ControlSetDataSource(Value: TDataSource);
begin
  DataSource := Value;
end;

function TJvDynControlVCLDBNavigator.ControlGetDataSource: TDataSource;
begin
  Result := DataSource;
end;

procedure TJvDynControlVCLDBNavigator.ControlSetDataField(const Value: string);
begin
end;

function TJvDynControlVCLDBNavigator.ControlGetDataField: string;
begin
  Result := '';
end;

procedure TJvDynControlVCLDBNavigator.ControlSetAnchors(Value : TAnchors);
begin
  Anchors := Value;
end;

//=== { TJvDynControlEngineVCLDB } ===========================================

function DynControlEngineVCLDB: TJvDynControlEngineDB;
begin
  Result := IntDynControlEngineVCLDB;
end;

procedure TJvDynControlEngineVCLDB.RegisterControls;
begin
  RegisterControlType(jctDBText, TJvDynControlVCLDBText);
  RegisterControlType(jctDBEdit, TJvDynControlVCLDBEdit);
  RegisterControlType(jctDBImage, TJvDynControlVCLDBImage);
  RegisterControlType(jctDBCheckBox, TJvDynControlVCLDBCheckBox);
  RegisterControlType(jctDBComboBox, TJvDynControlVCLDBComboBox);
  RegisterControlType(jctDBListBox, TJvDynControlVCLDBListBox);
  RegisterControlType(jctDBRadioGroup, TJvDynControlVCLDBRadioGroup);
  RegisterControlType(jctDBDateTimeEdit, TJvDynControlVCLDBEdit);
  RegisterControlType(jctDBTimeEdit, TJvDynControlVCLDBEdit);
  RegisterControlType(jctDBDateEdit, TJvDynControlVCLDBEdit);
////  RegisterControlType(jctDBCalculateEdit, TJvDynControlVCLDBEdit);
////  RegisterControlType(jctDBSpinEdit, TJvDynControlVCLDBEdit);
  RegisterControlType(jctDBDirectoryEdit, TJvDynControlVCLDBDirectoryEdit);
  RegisterControlType(jctDBFileNameEdit, TJvDynControlVCLDBFileNameEdit);
  RegisterControlType(jctDBMemo, TJvDynControlVCLDBMemo);
  RegisterControlType(jctDBButtonEdit, TJvDynControlVCLDBButtonEdit);
  RegisterControlType(jctDBGrid, TJvDynControlVCLDBGrid);
  RegisterControlType(jctDBNavigator, TJvDynControlVCLDBNavigator);
end;
type TAccessDBLookupControl = class(TDBLookupControl);

function TJvDynControlEngineVCLDB.GetDataSourceFromDataComponent(ADataComponent: TComponent): TDataSource;
begin
  if not Assigned(ADataComponent) then
    Result := nil
  else
  if ADataComponent is TCustomDBGrid then
    Result := TCustomDBGrid(ADataComponent).DataSource
  else
  if ADataComponent is TDBEdit then
    Result := TDBEdit(ADataComponent).DataSource
  else
  if ADataComponent is TDBNavigator then
    Result := TDBNavigator(ADataComponent).DataSource
  else
  if ADataComponent is TDBListBox then
    Result := TDBListbox(ADataComponent).DataSource
  else
  if ADataComponent is TDBLookupControl then
    Result := TAccessDBLookupControl(ADataComponent).DataSource
  else
  if ADataComponent is TDBImage then
    Result := TDBImage(ADataComponent).DataSource
  else
  if ADataComponent is TDBMemo then
    Result := TDBMemo(ADataComponent).DataSource
  else
  if ADataComponent is TDBRadioGroup then
    Result := TDBRadioGroup(ADataComponent).DataSource
  else
  if ADataComponent is TDBRichEdit then
    Result := TDBRichEdit(ADataComponent).DataSource
  else
  if ADataComponent is TDBText then
    Result := TDBText(ADataComponent).DataSource
  else
  if ADataComponent is TDBCheckBox then
    Result := TDBCheckBox(ADataComponent).DataSource
  else
    Result := inherited GetDataSourceFromDataComponent(ADataComponent);
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  IntDynControlEngineVCLDB := TJvDynControlEngineVCLDB.Create;
  SetDefaultDynControlEngineDB(IntDynControlEngineVCLDB);

finalization
  FreeAndNil(IntDynControlEngineVCLDB);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.