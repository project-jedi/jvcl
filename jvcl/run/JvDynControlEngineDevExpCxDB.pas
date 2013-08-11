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

unit JvDynControlEngineDevExpCxDB;

{$I jvcl.inc}

interface

{$IFNDEF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning, JvDynControlEngineDevExpCx, JvDynControlEngineIntf;
{$ENDIF UNITVERSIONING}

{$ELSE}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes, ExtCtrls, ExtDlgs, Graphics, Buttons, Controls, Dialogs, FileCtrl,
  Forms, DBCtrls, DB, DBGrids, StdCtrls,
  cxDBEdit, cxDBNavigator,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  JvDynControlEngine, JvDynControlEngineDB, JvDynControlEngineIntf,
  JvDynControlEngineDevExpCx, JvDynControlEngineDBIntf;

type
  TJvDynControlCxDBEdit = class(TcxDBTextEdit, IUnknown,
      IJvDynControl, IJvDynControlDevExpCx, IJvDynControlData, IJvDynControlReadOnly, IJvDynControlEdit,
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
    procedure ControlSetAnchors(Value: TAnchors);

    //IJvDynControlDevExpCx
    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);
  end;

  TJvDynControlCxDBButtonEdit = class(TcxDBButtonEdit, IUnknown,
      IJvDynControl, IJvDynControlDevExpCx, IJvDynControlData, IJvDynControlReadOnly, IJvDynControlEdit,
      IJvDynControlButtonEdit, IJvDynControlButton, IJvDynControlDatabase)
  private
    FIntOnButtonClick: TNotifyEvent;
  protected
    procedure IntOnButtonClick(Sender: TObject; AButtonIndex: Integer);
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
    procedure ControlSetAnchors(Value: TAnchors);

    //IJvDynControlDevExpCx
    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);
  end;

  TJvDynControlCxDBFileNameEdit = class(TcxDBButtonEdit, IUnknown,
      IJvDynControl, IJvDynControlDevExpCx, IJvDynControlData, IJvDynControlFileName,
      IJvDynControlReadOnly, IJvDynControlDatabase)
  private
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
    procedure ControlSetAnchors(Value: TAnchors);

    //IJvDynControlDevExpCx
    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);
  end;

  TJvDynControlCxDBDirectoryEdit = class(TcxDBButtonEdit, IUnknown,
      IJvDynControl, IJvDynControlDevExpCx, IJvDynControlData,
      IJvDynControlDirectory, IJvDynControlReadOnly, IJvDynControlDatabase)
  private
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
    procedure ControlSetAnchors(Value: TAnchors);

    //IJvDynControlDevExpCx
    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);
  end;

  TJvDynControlCxDBCheckBox = class(TcxDBCheckBox, IUnknown,
      IJvDynControl, IJvDynControlDevExpCx, IJvDynControlData, IJvDynControlDatabase,
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
    procedure ControlSetAnchors(Value: TAnchors);

    //IJvDynControlDBCheckbox
    procedure ControlSetValueChecked(Value: Variant);
    procedure ControlSetValueUnChecked(Value: Variant);

    //IJvDynControlDevExpCx
    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);
  end;

  TJvDynControlCxDBMemo = class(TcxDBMemo, IUnknown, IJvDynControl, IJvDynControlDevExpCx, IJvDynControlData,
      IJvDynControlItems, IJvDynControlMemo, IJvDynControlReadOnly, IJvDynControlDatabase, IJvDynControlFont)
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
    procedure ControlSetAnchors(Value: TAnchors);

    //IJvDynControlDevExpCx
    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);

    //IJvDynControlFont
    procedure ControlSetFont(Value: TFont);
    function ControlGetFont: TFont;

  end;

  TJvDynControlCxDBRadioGroup = class(TcxDBRadioGroup, IUnknown,
      IJvDynControl, IJvDynControlDevExpCx, IJvDynControlData, IJvDynControlItems,
      IJvDynControlRadioGroup, IJvDynControlDatabase)
  private
    FItems: TStrings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    procedure ControlSetAnchors(Value: TAnchors);

    //IJvDynControlDevExpCx
    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);
  end;

  TJvDynControlCxDBListBox = class(TcxDBListBox, IUnknown,
      IJvDynControl, IJvDynControlDevExpCx, IJvDynControlData, IJvDynControlItems,
      IJvDynControlDblClick, IJvDynControlDatabase)
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
    procedure ControlSetAnchors(Value: TAnchors);

    //IJvDynControlDevExpCx
    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);
  end;

  TJvDynControlCxDBComboBox = class(TcxDBComboBox, IUnknown,
      IJvDynControl, IJvDynControlDevExpCx, IJvDynControlData, IJvDynControlItems,
      IJvDynControlComboBox, IJvDynControlDatabase)
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
    procedure ControlSetAnchors(Value: TAnchors);

    //IJvDynControlDevExpCx
    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);
  end;

  TJvDynControlCxDBImage = class(TcxDBImage, IUnknown,
      IJvDynControl, IJvDynControlDevExpCx, IJvDynControlImage, IJvDynControlDatabase)
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
    procedure ControlSetAnchors(Value: TAnchors);

    //IJvDynControlDevExpCx
    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);
  end;

  TJvDynControlCxDBText = class(TcxDBTextEdit, IUnknown,
      IJvDynControl, IJvDynControlDevExpCx, IJvDynControlDatabase)
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
    procedure ControlSetAnchors(Value: TAnchors);

    //IJvDynControlDevExpCx
    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);
  end;

  TJvDynControlCxDBNavigator = class(TcxDBNavigator, IUnknown,
      IJvDynControl, IJvDynControlDevExpCx, IJvDynControlDatabase)
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
    procedure ControlSetAnchors(Value: TAnchors);

    //IJvDynControlDevExpCx
    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);

  end;

  TJvDynControlCxDBDateTimeEdit = class(TcxDBDateEdit, IUnknown, IJvDynControl,
      IJvDynControlData, IJvDynControlDevExpCx, IJvDynControlDate, IJvDynControlReadOnly,
      IJvDynControlDatabase)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    // IJvDynControlDate
    procedure ControlSetMinDate(Value: TDateTime);
    procedure ControlSetMaxDate(Value: TDateTime);
    procedure ControlSetFormat(const Value: string);

    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);

    //IJvDynControlDatabase
    procedure ControlSetDataSource(Value: TDataSource);
    function ControlGetDataSource: TDataSource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
    procedure ControlSetAnchors(Value: TAnchors);
  end;

  TJvDynControlCxDBDateEdit = class(TcxDBDateEdit, IUnknown, IJvDynControl,
      IJvDynControlData, IJvDynControlDevExpCx, IJvDynControlDate,
      IJvDynControlReadOnly, IJvDynControlDatabase)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    // IJvDynControlDate
    procedure ControlSetMinDate(Value: TDateTime);
    procedure ControlSetMaxDate(Value: TDateTime);
    procedure ControlSetFormat(const Value: string);

    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);
    //IJvDynControlDatabase
    procedure ControlSetDataSource(Value: TDataSource);
    function ControlGetDataSource: TDataSource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
    procedure ControlSetAnchors(Value: TAnchors);
  end;

  TJvDynControlCxDBTimeEdit = class(TcxDBTimeEdit, IUnknown, IJvDynControl,
      IJvDynControlData, IJvDynControlDevExpCx, IJvDynControlTime,
      IJvDynControlReadOnly, IJvDynControlDatabase)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);

    procedure ControlSetFormat(const Value: string);
    //IJvDynControlDatabase
    procedure ControlSetDataSource(Value: TDataSource);
    function ControlGetDataSource: TDataSource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
    procedure ControlSetAnchors(Value: TAnchors);
  end;

function DynControlEngineCxDB: TJvDynControlEngineDB;
procedure SetDefaultDynControlEngineDBDevExp;

{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}

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
  Variants, SysUtils, TypInfo,
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}
  cxTextEdit, cxMaskEdit, cxRadioGroup, cxDropDownEdit, cxDBRichEdit,
  cxEdit, cxTimeEdit, cxDBLookupComboBox, cxMemo, cxCheckbox,
  cxGridTableView, cxGridCustomView,
  cxGrid, cxGridCustomTableView, cxGridDBDataDefinitions,
  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}
  JvDynControlEngineTools, JvConsts, JvJCLUtils;

{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}

var
  IntDynControlEngineCxDB: TJvDynControlEngineDB = nil;

//=== { TJvDynControlCxDBEdit } ==============================================

procedure TJvDynControlCxDBEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxDBEdit.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxDBEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlCxDBEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxDBEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxDBEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxDBEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxDBEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxDBEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxDBEdit.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlCxDBEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlCxDBEdit.ControlSetPasswordChar(Value: Char);
begin
  if Value <> #0 then
    Properties.EchoMode := eemPassword
  else
    Properties.EchoMode := eemNormal;
end;

procedure TJvDynControlCxDBEdit.ControlSetEditMask(const Value: string);
begin
  //EditMask := Value;
end;

procedure TJvDynControlCxDBEdit.ControlSetDataSource(Value: TDataSource);
begin
  Databinding.DataSource := Value;
end;

function TJvDynControlCxDBEdit.ControlGetDataSource: TDataSource;
begin
  Result := DataBinding.DataSource;
end;

procedure TJvDynControlCxDBEdit.ControlSetDataField(const Value: string);
begin
  Databinding.DataField := Value;
end;

function TJvDynControlCxDBEdit.ControlGetDataField: string;
begin
  Result := Databinding.DataField;
end;

procedure TJvDynControlCxDBEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxDBEdit.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

//=== { TJvDynControlCxDBButtonEdit } ========================================

constructor TJvDynControlCxDBButtonEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TJvDynControlCxDBButtonEdit.Destroy;
begin
  inherited Destroy;
end;

procedure TJvDynControlCxDBButtonEdit.IntOnButtonClick(Sender: TObject;
  AButtonIndex: Integer);
begin
  if Assigned(FIntOnButtonClick) then
    FIntOnButtonClick(Sender);
end;

procedure TJvDynControlCxDBButtonEdit.ControlSetDefaultProperties;
begin
  Properties.OnButtonClick := IntOnButtonClick;
  Properties.MaskKind := emkStandard;
end;

procedure TJvDynControlCxDBButtonEdit.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxDBButtonEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlCxDBButtonEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxDBButtonEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxDBButtonEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxDBButtonEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxDBButtonEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxDBButtonEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxDBButtonEdit.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlCxDBButtonEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlCxDBButtonEdit.ControlSetPasswordChar(Value: Char);
begin
  if Value <> #0 then
    Properties.EchoMode := eemPassword
  else
    Properties.EchoMode := eemNormal;
end;

procedure TJvDynControlCxDBButtonEdit.ControlSetEditMask(const Value: string);
begin
  //FEditControl.EditMask := Value;
end;

procedure TJvDynControlCxDBButtonEdit.ControlSetOnButtonClick(Value: TNotifyEvent);
begin
  FIntOnButtonClick := Value;
end;

procedure TJvDynControlCxDBButtonEdit.ControlSetButtonCaption(const Value: string);
begin
  Properties.Buttons[0].Caption := Value;
end;

procedure TJvDynControlCxDBButtonEdit.ControlSetGlyph(Value: TBitmap);
begin
  Properties.Buttons[0].Glyph.Assign(Value);
end;

procedure TJvDynControlCxDBButtonEdit.ControlSetNumGlyphs(Value: Integer);
begin
end;

procedure TJvDynControlCxDBButtonEdit.ControlSetLayout(Value: TButtonLayout);
begin
end;

procedure TJvDynControlCxDBButtonEdit.ControlSetDefault(Value: Boolean);
begin
end;

procedure TJvDynControlCxDBButtonEdit.ControlSetCancel(Value: Boolean);
begin
end;

procedure TJvDynControlCxDBButtonEdit.ControlSetDataSource(Value: TDataSource);
begin
  Databinding.DataSource := Value;
end;

function TJvDynControlCxDBButtonEdit.ControlGetDataSource: TDataSource;
begin
  Result := Databinding.DataSource;
end;

procedure TJvDynControlCxDBButtonEdit.ControlSetDataField(const Value: string);
begin
  Databinding.DataField := Value;
end;

function TJvDynControlCxDBButtonEdit.ControlGetDataField: string;
begin
  Result := Databinding.DataField;
end;

procedure TJvDynControlCxDBButtonEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxDBButtonEdit.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

//=== { TJvDynControlCxDBFileNameEdit } ======================================

constructor TJvDynControlCxDBFileNameEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TJvDynControlCxDBFileNameEdit.Destroy;
begin
  inherited Destroy;
end;

procedure TJvDynControlCxDBFileNameEdit.DefaultOnButtonClick(Sender: TObject);
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
  if CanFocus then
    SetFocus;
end;

procedure TJvDynControlCxDBFileNameEdit.ControlSetDefaultProperties;
begin
  Caption := ' ';
end;

procedure TJvDynControlCxDBFileNameEdit.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxDBFileNameEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlCxDBFileNameEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxDBFileNameEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxDBFileNameEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxDBFileNameEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxDBFileNameEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxDBFileNameEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxDBFileNameEdit.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlCxDBFileNameEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

// IJvDynControlFileName

procedure TJvDynControlCxDBFileNameEdit.ControlSetInitialDir(const Value: string);
begin
  FInitialDir := Value;
end;

procedure TJvDynControlCxDBFileNameEdit.ControlSetDefaultExt(const Value: string);
begin
  FDefaultExt := Value;
end;

procedure TJvDynControlCxDBFileNameEdit.ControlSetDialogTitle(const Value: string);
begin
  FDialogTitle := Value;
end;

procedure TJvDynControlCxDBFileNameEdit.ControlSetDialogOptions(Value: TOpenOptions);
begin
  FDialogOptions := Value;
end;

procedure TJvDynControlCxDBFileNameEdit.ControlSetFilter(const Value: string);
begin
  FFilter := Value;
end;

procedure TJvDynControlCxDBFileNameEdit.ControlSetFilterIndex(Value: Integer);
begin
  FFilterIndex := Value;
end;

procedure TJvDynControlCxDBFileNameEdit.ControlSetDialogKind(Value: TJvDynControlFileNameDialogKind);
begin
  FDialogKind := Value;
end;

procedure TJvDynControlCxDBFileNameEdit.ControlSetDataSource(Value: TDataSource);
begin
  Databinding.DataSource := Value;
end;

function TJvDynControlCxDBFileNameEdit.ControlGetDataSource: TDataSource;
begin
  Result := Databinding.DataSource;
end;

procedure TJvDynControlCxDBFileNameEdit.ControlSetDataField(const Value: string);
begin
  Databinding.DataField := Value;
end;

function TJvDynControlCxDBFileNameEdit.ControlGetDataField: string;
begin
  Result := Databinding.DataField;
end;

procedure TJvDynControlCxDBFileNameEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxDBFileNameEdit.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

//=== { TJvDynControlCxDBDirectoryEdit } =====================================

constructor TJvDynControlCxDBDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TJvDynControlCxDBDirectoryEdit.Destroy;
begin
  inherited Destroy;
end;

procedure TJvDynControlCxDBDirectoryEdit.DefaultOnButtonClick(Sender: TObject);
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
  if CanFocus then
    SetFocus;
end;

procedure TJvDynControlCxDBDirectoryEdit.ControlSetDefaultProperties;
begin
  Self.Caption := ' ';
end;

procedure TJvDynControlCxDBDirectoryEdit.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxDBDirectoryEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlCxDBDirectoryEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxDBDirectoryEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxDBDirectoryEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxDBDirectoryEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxDBDirectoryEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxDBDirectoryEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxDBDirectoryEdit.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlCxDBDirectoryEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlCxDBDirectoryEdit.ControlSetInitialDir(const Value: string);
begin
  FInitialDir := Value;
end;

procedure TJvDynControlCxDBDirectoryEdit.ControlSetDialogTitle(const Value: string);
begin
  FDialogTitle := Value;
end;


procedure TJvDynControlCxDBDirectoryEdit.ControlSetDialogOptions(Value: TSelectDirOpts);
begin
  FDialogOptions := Value;
end;


procedure TJvDynControlCxDBDirectoryEdit.ControlSetDataSource(Value: TDataSource);
begin
  Databinding.DataSource := Value;
end;

function TJvDynControlCxDBDirectoryEdit.ControlGetDataSource: TDataSource;
begin
  Result := Databinding.DataSource;
end;

procedure TJvDynControlCxDBDirectoryEdit.ControlSetDataField(const Value: string);
begin
  Databinding.DataField := Value;
end;

function TJvDynControlCxDBDirectoryEdit.ControlGetDataField: string;
begin
  Result := Databinding.DataField;
end;

procedure TJvDynControlCxDBDirectoryEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxDBDirectoryEdit.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

//=== { TJvDynControlCxDBCheckBox } ==========================================

procedure TJvDynControlCxDBCheckBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxDBCheckBox.ControlSetCaption(const Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlCxDBCheckBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxDBCheckBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxDBCheckBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxDBCheckBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxDBCheckBox.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxDBCheckBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxDBCheckBox.ControlSetValue(Value: Variant);
begin
  Checked := JvDynControlVariantToBoolean(Value);
end;

function TJvDynControlCxDBCheckBox.ControlGetValue: Variant;
begin
  Result := Checked;
end;

procedure TJvDynControlCxDBCheckBox.ControlSetDataSource(Value: TDataSource);
begin
  Databinding.DataSource := Value;
end;

function TJvDynControlCxDBCheckBox.ControlGetDataSource: TDataSource;
begin
  Result := DataBinding.DataSource;
end;

procedure TJvDynControlCxDBCheckBox.ControlSetDataField(const Value: string);
begin
  Databinding.DataField := Value;
end;

function TJvDynControlCxDBCheckBox.ControlGetDataField: string;
begin
  Result := Databinding.DataField;
end;

procedure TJvDynControlCxDBCheckBox.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxDBCheckBox.ControlSetValueChecked(Value: Variant);
begin
  Properties.ValueChecked := Value;
end;

procedure TJvDynControlCxDBCheckBox.ControlSetValueUnChecked(Value: Variant);
begin
  Properties.ValueUnChecked := Value;
end;

procedure TJvDynControlCxDBCheckBox.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

//=== { TJvDynControlCxDBMemo } ==============================================

procedure TJvDynControlCxDBMemo.ControlSetDefaultProperties;
begin
  Properties.ScrollBars := ssBoth;
  Properties.WantReturns := True;
  Properties.WantTabs := True;
end;

procedure TJvDynControlCxDBMemo.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxDBMemo.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlCxDBMemo.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxDBMemo.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxDBMemo.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxDBMemo.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxDBMemo.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxDBMemo.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxDBMemo.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlCxDBMemo.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlCxDBMemo.ControlSetSorted(Value: Boolean);
begin
end;

procedure TJvDynControlCxDBMemo.ControlSetItems(Value: TStrings);
begin
  Lines.Assign(Value);
end;

function TJvDynControlCxDBMemo.ControlGetItems: TStrings;
begin
  Result := Lines;
end;

procedure TJvDynControlCxDBMemo.ControlSetWantTabs(Value: Boolean);
begin
  Properties.WantTabs := Value;
end;

procedure TJvDynControlCxDBMemo.ControlSetWantReturns(Value: Boolean);
begin
  Properties.WantReturns := Value;
end;

procedure TJvDynControlCxDBMemo.ControlSetWordWrap(Value: Boolean);
begin
  Properties.WordWrap := Value;
end;

procedure TJvDynControlCxDBMemo.ControlSetScrollBars(Value: TScrollStyle);
begin
  ScrollBars := Value;
end;

procedure TJvDynControlCxDBMemo.ControlSetDataSource(Value: TDataSource);
begin
  Databinding.DataSource := Value;
end;

function TJvDynControlCxDBMemo.ControlGetDataSource: TDataSource;
begin
  Result := DataBinding.DataSource;
end;

procedure TJvDynControlCxDBMemo.ControlSetDataField(const Value: string);
begin
  Databinding.DataField := Value;
end;

function TJvDynControlCxDBMemo.ControlGetDataField: string;
begin
  Result := Databinding.DataField;
end;

function TJvDynControlCxDBMemo.ControlGetFont: TFont;
begin
  Result := Font;
end;

procedure TJvDynControlCxDBMemo.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxDBMemo.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

procedure TJvDynControlCxDBMemo.ControlSetFont(Value: TFont);
begin
  Font.Assign(Value);
end;

//=== { TJvDynControlCxDBRadioGroup } ========================================

constructor TJvDynControlCxDBRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TStringList.Create;
end;

destructor TJvDynControlCxDBRadioGroup.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TJvDynControlCxDBRadioGroup.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxDBRadioGroup.ControlSetCaption(const Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlCxDBRadioGroup.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxDBRadioGroup.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxDBRadioGroup.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxDBRadioGroup.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxDBRadioGroup.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxDBRadioGroup.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxDBRadioGroup.ControlSetValue(Value: Variant);
var
  I: Integer;
begin
  if VarIsInt(Value) then
    ItemIndex := Value
  else
  begin
    ItemIndex := -1;
    for I := 0 to Properties.Items.Count - 1 do
      if TcxRadioGroupItem(Properties.Items[I]).Caption = Value then
      begin
        ItemIndex := I;
        Break;
      end;
  end;
end;

function TJvDynControlCxDBRadioGroup.ControlGetValue: Variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlCxDBRadioGroup.ControlSetSorted(Value: Boolean);
begin
end;

procedure TJvDynControlCxDBRadioGroup.ControlSetItems(Value: TStrings);
var
  I: Integer;
  Item: TcxRadioGroupItem;
begin
  FItems.Assign(Value);
  Properties.Items.Clear;
  for I := 0 to Value.Count - 1 do
  begin
    Item := TcxRadioGroupItem(Properties.Items.Add);
    Item.Caption := Value[I];
  end;
end;

function TJvDynControlCxDBRadioGroup.ControlGetItems: TStrings;
begin
  Result := FItems;
end;

procedure TJvDynControlCxDBRadioGroup.ControlSetColumns(Value: Integer);
begin
  Properties.Columns := Value;
end;

procedure TJvDynControlCxDBRadioGroup.ControlSetDataSource(Value: TDataSource);
begin
  Databinding.DataSource := Value;
end;

function TJvDynControlCxDBRadioGroup.ControlGetDataSource: TDataSource;
begin
  Result := DataBinding.DataSource;
end;

procedure TJvDynControlCxDBRadioGroup.ControlSetDataField(const Value: string);
begin
  Databinding.DataField := Value;
end;

function TJvDynControlCxDBRadioGroup.ControlGetDataField: string;
begin
  Result := Databinding.DataField;
end;

procedure TJvDynControlCxDBRadioGroup.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxDBRadioGroup.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

//=== { TJvDynControlCxDBListBox } ===========================================

procedure TJvDynControlCxDBListBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxDBListBox.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlCxDBListBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxDBListBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxDBListBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxDBListBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxDBListBox.ControlSetOnChange(Value: TNotifyEvent);
begin
  //  Properties.OnChange := Value;
end;

procedure TJvDynControlCxDBListBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxDBListBox.ControlSetValue(Value: Variant);
begin
  if VarIsInt(Value) then
    ItemIndex := Value
  else
    ItemIndex := Items.IndexOf(Value);
end;

function TJvDynControlCxDBListBox.ControlGetValue: Variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlCxDBListBox.ControlSetSorted(Value: Boolean);
begin
  Sorted := Value;
end;

procedure TJvDynControlCxDBListBox.ControlSetItems(Value: TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlCxDBListBox.ControlGetItems: TStrings;
begin
  Result := Items;
end;

procedure TJvDynControlCxDBListBox.ControlSetOnDblClick(Value: TNotifyEvent);
begin
  OnDblClick := Value;
end;

procedure TJvDynControlCxDBListBox.ControlSetDataSource(Value: TDataSource);
begin
  Databinding.DataSource := Value;
end;

function TJvDynControlCxDBListBox.ControlGetDataSource: TDataSource;
begin
  Result := DataBinding.DataSource;
end;

procedure TJvDynControlCxDBListBox.ControlSetDataField(const Value: string);
begin
  Databinding.DataField := Value;
end;

function TJvDynControlCxDBListBox.ControlGetDataField: string;
begin
  Result := Databinding.DataField;
end;

procedure TJvDynControlCxDBListBox.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxDBListBox.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

//=== { TJvDynControlCxDBComboBox } ==========================================

procedure TJvDynControlCxDBComboBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxDBComboBox.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlCxDBComboBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxDBComboBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxDBComboBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxDBComboBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxDBComboBox.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxDBComboBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxDBComboBox.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlCxDBComboBox.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlCxDBComboBox.ControlSetSorted(Value: Boolean);
begin
  Properties.Sorted := Value;
end;

procedure TJvDynControlCxDBComboBox.ControlSetItems(Value: TStrings);
begin
  Properties.Items.Assign(Value);
end;

function TJvDynControlCxDBComboBox.ControlGetItems: TStrings;
begin
  Result := Properties.Items;
end;

procedure TJvDynControlCxDBComboBox.ControlSetNewEntriesAllowed(Value: Boolean);
begin
  if Value then
    Properties.DropDownListStyle := lsEditList
  else
    Properties.DropDownListStyle := lsEditFixedList;
end;

procedure TJvDynControlCxDBComboBox.ControlSetDataSource(Value: TDataSource);
begin
  Databinding.DataSource := Value;
end;

function TJvDynControlCxDBComboBox.ControlGetDataSource: TDataSource;
begin
  Result := DataBinding.DataSource;
end;

procedure TJvDynControlCxDBComboBox.ControlSetDataField(const Value: string);
begin
  Databinding.DataField := Value;
end;

function TJvDynControlCxDBComboBox.ControlGetDataField: string;
begin
  Result := Databinding.DataField;
end;

procedure TJvDynControlCxDBComboBox.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxDBComboBox.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

//=== { TJvDynControlCxDBImage } =============================================

procedure TJvDynControlCxDBImage.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxDBImage.ControlSetCaption(const Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlCxDBImage.ControlSetTabOrder(Value: Integer);
begin
  //  TabOrder := Value;
end;

procedure TJvDynControlCxDBImage.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxDBImage.ControlSetOnEnter(Value: TNotifyEvent);
begin
  //  OnEnter := Value;
end;

procedure TJvDynControlCxDBImage.ControlSetOnExit(Value: TNotifyEvent);
begin
  //  OnExit := Value;
end;

procedure TJvDynControlCxDBImage.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxDBImage.ControlSetAutoSize(Value: Boolean);
begin
  AutoSize := Value;
end;

procedure TJvDynControlCxDBImage.ControlSetIncrementalDisplay(Value: Boolean);
begin
  //  IncrementalDisplay := Value;
end;

procedure TJvDynControlCxDBImage.ControlSetCenter(Value: Boolean);
begin
  Properties.Center := Value;
end;


procedure TJvDynControlCxDBImage.ControlSetProportional(Value: Boolean);
begin
  //  Proportional := Value;
end;


procedure TJvDynControlCxDBImage.ControlSetStretch(Value: Boolean);
begin
  Properties.Stretch := Value;
end;

procedure TJvDynControlCxDBImage.ControlSetTransparent(Value: Boolean);
begin
  //  Transparent := Value;
end;

procedure TJvDynControlCxDBImage.ControlSetPicture(Value: TPicture);
begin
  Picture.Assign(Value);
end;

procedure TJvDynControlCxDBImage.ControlSetGraphic(Value: TGraphic);
begin
  Picture.Assign(Value);
end;

function TJvDynControlCxDBImage.ControlGetPicture: TPicture;
begin
  Result := Picture;
end;

procedure TJvDynControlCxDBImage.ControlSetDataSource(Value: TDataSource);
begin
  Databinding.DataSource := Value;
end;

function TJvDynControlCxDBImage.ControlGetDataSource: TDataSource;
begin
  Result := DataBinding.DataSource;
end;

procedure TJvDynControlCxDBImage.ControlSetDataField(const Value: string);
begin
  Databinding.DataField := Value;
end;

function TJvDynControlCxDBImage.ControlGetDataField: string;
begin
  Result := Databinding.DataField;
end;

procedure TJvDynControlCxDBImage.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxDBImage.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

//=== { TJvDynControlCxDBText } ==============================================

procedure TJvDynControlCxDBText.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxDBText.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlCxDBText.ControlSetTabOrder(Value: Integer);
begin
end;

procedure TJvDynControlCxDBText.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxDBText.ControlSetOnEnter(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxDBText.ControlSetOnExit(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxDBText.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxDBText.ControlSetDataSource(Value: TDataSource);
begin
  Databinding.DataSource := Value;
end;

function TJvDynControlCxDBText.ControlGetDataSource: TDataSource;
begin
  Result := DataBinding.DataSource;
end;

procedure TJvDynControlCxDBText.ControlSetDataField(const Value: string);
begin
  Databinding.DataField := Value;
end;

function TJvDynControlCxDBText.ControlGetDataField: string;
begin
  Result := Databinding.DataField;
end;

procedure TJvDynControlCxDBText.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxDBText.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

//=== { TJvDynControlCxDBNavigator } =========================================

procedure TJvDynControlCxDBNavigator.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxDBNavigator.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlCxDBNavigator.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxDBNavigator.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxDBNavigator.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxDBNavigator.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxDBNavigator.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxDBNavigator.ControlSetDataSource(Value: TDataSource);
begin
  DataSource := Value;
end;

function TJvDynControlCxDBNavigator.ControlGetDataSource: TDataSource;
begin
  Result := DataSource;
end;

procedure TJvDynControlCxDBNavigator.ControlSetDataField(const Value: string);
begin
end;

function TJvDynControlCxDBNavigator.ControlGetDataField: string;
begin
  Result := '';
end;

procedure TJvDynControlCxDBNavigator.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxDBNavigator.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  LookAndFeel.Assign(Value.LookAndFeel);
  //Style.StyleController := Value.StyleController;
end;

//=== { TJvDynControlCxDBDateTimeEdit } ======================================

procedure TJvDynControlCxDBDateTimeEdit.ControlSetDefaultProperties;
begin
  Properties.ShowTime := True;
  Properties.SaveTime := False;
  //  Properties.InputKind := ikStandard;
end;

procedure TJvDynControlCxDBDateTimeEdit.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxDBDateTimeEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlCxDBDateTimeEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxDBDateTimeEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxDBDateTimeEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxDBDateTimeEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxDBDateTimeEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxDBDateTimeEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxDBDateTimeEdit.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlCxDBDateTimeEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

// IJvDynControlDate

procedure TJvDynControlCxDBDateTimeEdit.ControlSetMinDate(Value: TDateTime);
begin
  Properties.MinDate := Value;
end;

procedure TJvDynControlCxDBDateTimeEdit.ControlSetMaxDate(Value: TDateTime);
begin
  Properties.MaxDate := Value;
end;

procedure TJvDynControlCxDBDateTimeEdit.ControlSetFormat(const Value: string);
begin
  //  Format := Value;
end;

procedure TJvDynControlCxDBDateTimeEdit.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

procedure TJvDynControlCxDBDateTimeEdit.ControlSetDataSource(Value: TDataSource);
begin
  Databinding.DataSource := Value;
end;

function TJvDynControlCxDBDateTimeEdit.ControlGetDataSource: TDataSource;
begin
  Result := Databinding.DataSource;
end;

procedure TJvDynControlCxDBDateTimeEdit.ControlSetDataField(const Value: string);
begin
  Databinding.DataField := Value;
end;

function TJvDynControlCxDBDateTimeEdit.ControlGetDataField: string;
begin
  Result := Databinding.DataField;
end;

procedure TJvDynControlCxDBDateTimeEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

//=== { TJvDynControlCxDBDateEdit } ==========================================

procedure TJvDynControlCxDBDateEdit.ControlSetDefaultProperties;
begin
  Properties.ShowTime := False;
  Properties.SaveTime := False;
  //  Properties.InputKind := ikStandard;
end;

procedure TJvDynControlCxDBDateEdit.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxDBDateEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlCxDBDateEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxDBDateEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxDBDateEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxDBDateEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxDBDateEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxDBDateEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxDBDateEdit.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlCxDBDateEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

// IJvDynControlDate

procedure TJvDynControlCxDBDateEdit.ControlSetMinDate(Value: TDateTime);
begin
  Properties.MinDate := Value;
end;

procedure TJvDynControlCxDBDateEdit.ControlSetMaxDate(Value: TDateTime);
begin
  Properties.MaxDate := Value;
end;

procedure TJvDynControlCxDBDateEdit.ControlSetFormat(const Value: string);
begin
  //  Format := Value;
end;

procedure TJvDynControlCxDBDateEdit.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

procedure TJvDynControlCxDBDateEdit.ControlSetDataSource(Value: TDataSource);
begin
  Databinding.DataSource := Value;
end;

function TJvDynControlCxDBDateEdit.ControlGetDataSource: TDataSource;
begin
  Result := Databinding.DataSource;
end;

procedure TJvDynControlCxDBDateEdit.ControlSetDataField(const Value: string);
begin
  Databinding.DataField := Value;
end;

function TJvDynControlCxDBDateEdit.ControlGetDataField: string;
begin
  Result := Databinding.DataField;
end;

procedure TJvDynControlCxDBDateEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

//=== { TJvDynControlCxDBTimeEdit } ==========================================

procedure TJvDynControlCxDBTimeEdit.ControlSetDefaultProperties;
begin
  Properties.ShowDate := False;
  Properties.UseCtrlIncrement := True;
end;

procedure TJvDynControlCxDBTimeEdit.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxDBTimeEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlCxDBTimeEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxDBTimeEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxDBTimeEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxDBTimeEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxDBTimeEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxDBTimeEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxDBTimeEdit.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlCxDBTimeEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlCxDBTimeEdit.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

procedure TJvDynControlCxDBTimeEdit.ControlSetFormat(const Value: string);
begin
  //  Properties.Format := Value;
  Properties.Use24HourFormat := (Pos('H', Value) > 0);
  if Pos('s', Value) > 0 then
    Properties.TimeFormat := tfHourMinSec
  else
  if Pos('m', Value) > 0 then
    Properties.TimeFormat := tfHourMin
  else
    Properties.TimeFormat := tfHour;
end;

procedure TJvDynControlCxDBTimeEdit.ControlSetDataSource(Value: TDataSource);
begin
  Databinding.DataSource := Value;
end;

function TJvDynControlCxDBTimeEdit.ControlGetDataSource: TDataSource;
begin
  Result := Databinding.DataSource;
end;

procedure TJvDynControlCxDBTimeEdit.ControlSetDataField(const Value: string);
begin
  Databinding.DataField := Value;
end;

function TJvDynControlCxDBTimeEdit.ControlGetDataField: string;
begin
  Result := Databinding.DataField;
end;

procedure TJvDynControlCxDBTimeEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

//=== { TJvDynControlEngineDevExpCxDB } ======================================

function DynControlEngineCxDB: TJvDynControlEngineDB;
begin
  Result := IntDynControlEngineCxDB;
end;

procedure SetDefaultDynControlEngineDBDevExp;
begin
  SetDefaultDynControlEngineDB(DynControlEngineCxDB);
end;

type
  TJvDynControlEngineDevExpCxDB = class(TJvDynControlEngineDB)
  private
    FCxProperties: TCxDynControlWrapper;
  protected
    procedure SetcxProperties(Value: TCxDynControlWrapper);
    procedure RegisterControls; override;
    procedure TransferGridItemToControl(AGridItem: TcxCustomGridTableItem;
      ADataSource: TDataSource; AControl: TWinControl; AOptions: TJvCreateDBFieldsOnControlOptions);
  public
    constructor Create; override;
    destructor Destroy; override;
    function CreateControlClass(AControlClass: TControlClass; AOwner: TComponent; AParentControl: TWinControl;
      AControlName: string): TControl; override;
    function CreateControlsFromCxGridViewOnControl(AGridView: TcxCustomGridTableView;
      AControl: TWinControl; AOptions: TJvCreateDBFieldsOnControlOptions): Boolean;
    function CreateControlsFromDataComponentOnControl(ADataComponent: TComponent;
      AControl: TWinControl; AOptions: TJvCreateDBFieldsOnControlOptions): Boolean; override;
    function GetDataSourceFromDataComponent(ADataComponent: TComponent): TDataSource; override;
  published
    property CxProperties: TCxDynControlWrapper read FCxProperties write FCxProperties;
  end;

constructor TJvDynControlEngineDevExpCxDB.Create;
begin
  inherited Create;
  FCxProperties := TCxDynControlWrapper.Create;
end;

destructor TJvDynControlEngineDevExpCxDB.Destroy;
begin
  FreeAndNil(FCxProperties);
  inherited Destroy;
end;

procedure TJvDynControlEngineDevExpCxDB.SetcxProperties(Value: TCxDynControlWrapper);
begin
  if Value is TCxDynControlWrapper then
    FCxProperties.LookAndFeel.Assign(Value.LookAndFeel);
end;

function TJvDynControlEngineDevExpCxDB.CreateControlClass(AControlClass: TControlClass; AOwner: TComponent;
  AParentControl: TWinControl; AControlName: string): TControl;
var
  C: TControl;
begin
  C := inherited CreateControlClass(AControlClass, AOwner, AParentControl, AControlName);
  if Supports(C, IJvDynControlDevExpCx) then
    with C as IJvDynControlDevExpCx do
      ControlSetCxProperties(cxProperties);
  Result := C;
end;

procedure TJvDynControlEngineDevExpCxDB.RegisterControls;
begin
  RegisterControlType(jctDBText, TJvDynControlCxDBText);
  RegisterControlType(jctDBEdit, TJvDynControlCxDBEdit);
  RegisterControlType(jctDBImage, TJvDynControlCxDBImage);
  RegisterControlType(jctDBCheckBox, TJvDynControlCxDBCheckBox);
  RegisterControlType(jctDBComboBox, TJvDynControlCxDBComboBox);
  RegisterControlType(jctDBListBox, TJvDynControlCxDBListBox);
  RegisterControlType(jctDBRadioGroup, TJvDynControlCxDBRadioGroup);
  RegisterControlType(jctDBDateTimeEdit, TJvDynControlCxDBDateTimeEdit);
  RegisterControlType(jctDBTimeEdit, TJvDynControlCxDBTimeEdit);
  RegisterControlType(jctDBDateEdit, TJvDynControlCxDBDateEdit);
  ////  RegisterControlType(jctDBCalculateEdit, TJvDynControlCxDBEdit);
  ////  RegisterControlType(jctDBSpinEdit, TJvDynControlCxDBEdit);
  RegisterControlType(jctDBDirectoryEdit, TJvDynControlCxDBDirectoryEdit);
  RegisterControlType(jctDBFileNameEdit, TJvDynControlCxDBFileNameEdit);
  RegisterControlType(jctDBMemo, TJvDynControlCxDBMemo);
  RegisterControlType(jctDBButtonEdit, TJvDynControlCxDBButtonEdit);
  //  RegisterControlType(jctDBGrid, TJvDynControlCxDBGrid);
  RegisterControlType(jctDBNavigator, TJvDynControlCxDBNavigator);
end;

function TJvDynControlEngineDevExpCxDB.GetDataSourceFromDataComponent(ADataComponent: TComponent): TDataSource;
begin
  if not Assigned(ADataComponent) then
    Result := nil
  else
  if ADataComponent is TcxCustomGridTableView then
    if TcxCustomGridTableView(ADataComponent).DataController is TcxGridDBDataController then
      Result := TcxGridDBDataController(TcxCustomGridTableView(ADataComponent).DataController).DataSource
    else
      Result := nil
  else
  if (ADataComponent is TcxCustomGrid) and
    (TcxCustomGrid(ADataComponent).ActiveView is TcxCustomGridTableView) then
    if TcxCustomGridTableView(TcxCustomGrid(ADataComponent).ActiveView).DataController is TcxGridDBDataController
      then
      Result :=
        TcxGridDBDataController(TcxCustomGridTableView(TcxCustomGrid(ADataComponent).ActiveView).DataController).DataSource
    else
      Result := nil
  else
  if ADataComponent is TcxDBTextEdit then
    Result := TcxDBTextEdit(ADataComponent).Databinding.DataSource
  else
  if ADataComponent is TcxDBNavigator then
    Result := TcxDBNavigator(ADataComponent).DataSource
  else
  if ADataComponent is TcxDBListbox then
    Result := TcxDBListbox(ADataComponent).Databinding.DataSource
  else
  if ADataComponent is TcxDBLookupComboBox then
    Result := TcxDBLookupComboBox(ADataComponent).Databinding.DataSource
  else
  if ADataComponent is TcxDBImage then
    Result := TcxDBImage(ADataComponent).Databinding.DataSource
  else
  if ADataComponent is TcxDBMemo then
    Result := TcxDBMemo(ADataComponent).Databinding.DataSource
  else
  if ADataComponent is TcxDBRadioGroup then
    Result := TcxDBRadioGroup(ADataComponent).Databinding.DataSource
  else
  if ADataComponent is TcxDBRichEdit then
    Result := TcxDBRichEdit(ADataComponent).Databinding.DataSource
  else
  if ADataComponent is TcxDBCheckBox then
    Result := TcxDBCheckBox(ADataComponent).Databinding.DataSource
  else
    Result := inherited GetDataSourceFromDataComponent(ADataComponent);
end;

type
  TAccesscxCustomGridTableItem = class(TcxCustomGridTableItem);
  TAccesscxCustomEdit = class(TcxCustomEdit);
  TAccessCustomControl = class(TCustomControl);

procedure TJvDynControlEngineDevExpCxDB.TransferGridItemToControl(AGridItem: TcxCustomGridTableItem;
  ADataSource: TDataSource; AControl: TWinControl; AOptions: TJvCreateDBFieldsOnControlOptions);
var
  Control: TWinControl;
  LabelControl: TWinControl;
  GridDataBinding: TcxGridItemDBDataBinding;
begin
  if not (AGridItem is TcxGridColumn) or
    not (AGridItem.DataBinding is TcxGridItemDBDataBinding) then
    Exit;
  GridDataBinding := TcxGridItemDBDataBinding(AGridItem.DataBinding);
  if not Assigned(GridDataBinding.Field) then
    Exit;
  with AOptions do
  begin
    if TcxGridColumn(AGridItem).Visible or
      (TcxGridColumn(AGridItem).GroupIndex >= 0) or
      ShowInvisibleFields then
    begin
      if aGridItem.PropertiesClass = TcxMemoProperties then
      begin
        Control := TWinControl(CreateDBControl(jctDBMemo, AControl, AControl, '', aDataSource,
          GridDataBinding.Field.FieldName));
        if Supports(Control, IJvDynControlMemo) and Assigned(TcxGridColumn(aGridItem).Properties) then
          with Control as IJvDynControlMemo do
          begin
            ControlSetScrollbars(TcxMemoProperties(TcxGridColumn(aGridItem).Properties).Scrollbars);
            ControlSetWantReturns(TcxMemoProperties(TcxGridColumn(aGridItem).Properties).WantReturns);
            ControlSetWantTabs(TcxMemoProperties(TcxGridColumn(aGridItem).Properties).WantTabs);
            ControlSetWordwrap(TcxMemoProperties(TcxGridColumn(aGridItem).Properties).WordWrap);
          end;
      end
      else
        if aGridItem.PropertiesClass = TcxCheckBoxProperties then
        begin
          Control := TWinControl(CreateDBControl(jctDBCheckBox, AControl, AControl, '', aDataSource,
            GridDataBinding.Field.FieldName));
          if Supports(Control, IJvDynControlDBCheckBox) and Assigned(TcxGridColumn(aGridItem).Properties) then
            with Control as IJvDynControlDBCheckBox do
            begin
              ControlSetValueChecked(TcxCheckBoxProperties(TcxGridColumn(aGridItem).Properties).ValueChecked);
              ControlSetValueUnChecked(TcxCheckBoxProperties(TcxGridColumn(aGridItem).Properties).ValueUnChecked);
            end;
        end
        else
          if aGridItem.PropertiesClass = TcxComboBoxProperties then
          begin
            Control := TWinControl(CreateDBControl(jctDBCheckBox, AControl, AControl, '', aDataSource,
              GridDataBinding.Field.FieldName));
            if Supports(Control, IJvDynControlDBCheckBox) and Assigned(TcxGridColumn(aGridItem).Properties) then
              with Control as IJvDynControlDBCheckBox do
              begin
                ControlSetValueChecked(TcxCheckBoxProperties(TcxGridColumn(aGridItem).Properties).ValueChecked);
                ControlSetValueUnChecked(TcxCheckBoxProperties(TcxGridColumn(aGridItem).Properties).ValueUnChecked);
              end;
          end
          else
            Control := CreateDBFieldControl(GridDataBinding.Field, AControl, AControl, '', ADataSource);
      if FieldDefaultWidth > 0 then
        Control.Width := FieldDefaultWidth
      else
      begin
        if UseFieldSizeForWidth then
          if GridDataBinding.Field.Size > 0 then
            Control.Width :=
              TAccessCustomControl(AControl).Canvas.TextWidth('X') * GridDataBinding.Field.Size
          else
            if (aGridItem.PropertiesClass = TcxMemoProperties) and (FieldMaxWidth > 0) then
              Control.Width := FieldMaxWidth
            else
        else
          if GridDataBinding.Field.DisplayWidth > 0 then
            Control.Width :=
                TAccessCustomControl(AControl).Canvas.TextWidth('X') * GridDataBinding.Field.DisplayWidth;
        if (FieldMaxWidth > 0) and (Control.Width > FieldMaxWidth) then
          Control.Width := FieldMaxWidth
        else
          if (FieldMinWidth > 0) and (Control.Width < FieldMinWidth) then
            Control.Width := FieldMinWidth
      end;
      if Assigned(TcxGridColumn(aGridItem).Properties) then
        if Supports(Control, IJvDynControlReadOnly) then
          with Control as IJvDynControlReadOnly do
            ControlSetReadOnly(TcxGridColumn(AGridItem).Properties.ReadOnly);

      if UseParentColorForReadOnly then
        // Use ParentColor when the field is ReadOnly
        if not ADataSource.DataSet.CanModify or GridDataBinding.Field.ReadOnly then
          if isPublishedProp(Control, 'ParentColor') then
            SetOrdProp(Control, 'ParentColor', Ord(True));

      LabelControl := GetDynControlEngine.CreateLabelControlPanel(AControl, AControl,
        '', '&' + AGridItem.Caption, Control, LabelOnTop, LabelDefaultWidth);
      if FieldWidthStep > 0 then
        if (LabelControl.Width mod FieldWidthStep) <> 0 then
          LabelControl.Width := ((LabelControl.Width div FieldWidthStep) + 1) * FieldWidthStep;
    end;
  end;
end;

function TJvDynControlEngineDevExpCxDB.CreateControlsFromCxGridViewOnControl(AGridView: TcxCustomGridTableView;
  AControl: TWinControl; AOptions: TJvCreateDBFieldsOnControlOptions): Boolean;
var
  I: Integer;
  CreateOptions: TJvCreateDBFieldsOnControlOptions;
  GridDataController: TcxGridDBDataController;
begin
  Result := False;
  if not Assigned(AOptions) then
    CreateOptions := TJvCreateDBFieldsOnControlOptions.Create
  else
    CreateOptions := AOptions;
  try
    if tcxCustomGridView(AGridView).DataController is TcxGridDBDataController then
      GridDataController := TcxGridDBDataController(AGridView.DataController)
    else
      Exit;
    for I := 0 to AGridView.GroupedItemCount - 1 do
      TransferGridItemToControl(AGridView.GroupedItems[I], GridDataController.DataSource, AControl, CreateOptions);
    for I := 0 to AGridView.VisibleItemCount - 1 do
      TransferGridItemToControl(AGridView.VisibleItems[I], GridDataController.DataSource, AControl, CreateOptions);
  finally
    if not Assigned(AOptions) then
      CreateOptions.Free;
  end;
  Result := True;
end;

function TJvDynControlEngineDevExpCxDB.CreateControlsFromDataComponentOnControl(ADataComponent: TComponent;
  AControl: TWinControl; AOptions: TJvCreateDBFieldsOnControlOptions): Boolean;
begin
  if Assigned(ADataComponent) then
    if (ADataComponent is TcxGrid) and
      (TcxGrid(ADataComponent).ActiveView is TcxCustomGridTableView) then
      Result := CreateControlsFromcxGridViewOnControl(TcxCustomGridTableView(TcxGrid(ADataComponent).ActiveView),
        AControl, AOptions)
    else
      if ADataComponent is TcxCustomGridTableView then
        Result := CreateControlsFromcxGridViewOnControl(TcxCustomGridTableView(ADataComponent), AControl, AOptions)
      else
        Result := inherited CreateControlsFromDataComponentOnControl(ADataComponent, AControl, AOptions)
    else
      Result := False;
end;

{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}
  IntDynControlEngineCxDB := TJvDynControlEngineDevExpCxDB.Create;
  SetDefaultDynControlEngineDB(IntDynControlEngineCxDB);
  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}

finalization
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}
  FreeAndNil(IntDynControlEngineCxDB);
  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
