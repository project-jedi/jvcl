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

unit JvDynControlEngineVCLDB;

{$I jvcl.inc}
{$I crossplatform.inc}

interface

uses
  Classes, ExtCtrls, ExtDlgs, Graphics, Buttons, Controls, Dialogs, FileCtrl, Forms,
  DBCtrls, DB, DBGrids, StdCtrls,
  JvDynControlEngine, JvDynControlEngineDB,
  JvDynControlEngineIntf, JvDynControlEngineDBIntf;

type
  TJvDynControlVCLDBEdit = class (TDBEdit, IUnknown, IJvDynControl, IJvDynControlData, IJvDynControlReadOnly, IJvDynControlEdit, IJvDynControlDatabase)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(Value: string);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    //IJvDynControlEdit
    procedure ControlSetPasswordChar(Value: Char);
    procedure ControlSetEditMask(const Value: string);

    //IJvDynControlDatabase
    procedure ControlSetDatasource(Value: TDatasource);
    function ControlGetDatasource: TDatasource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;

  TJvDynControlVCLDBButtonEdit = class (TPanel, IUnknown, IJvDynControl, IJvDynControlData,
    IJvDynControlReadOnly, IJvDynControlEdit, IJvDynControlButtonEdit,
    IJvDynControlButton, IJvDynControlDatabase)
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

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(Value: string);

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
    procedure ControlSetDatasource(Value: TDatasource);
    function ControlGetDatasource: TDatasource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;


  TJvDynControlVCLDBFileNameEdit = class (TPanel, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlFileName, IJvDynControlReadOnly,
    IJvDynControlDatabase)
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

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(Value: string);

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
    procedure ControlSetDatasource(Value: TDatasource);
    function ControlGetDatasource: TDatasource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;

  TJvDynControlVCLDBDirectoryEdit = class (TPanel, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlDirectory, IJvDynControlReadOnly,
    IJvDynControlDatabase)
  private
    FEditControl: TDBEdit;
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
    procedure ControlSetHint(Value: string);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    // IJvDynControlDirectory
    procedure ControlSetInitialDir(const Value: string);
    procedure ControlSetDialogTitle(const Value: string);
    {$IFDEF VCL}
    procedure ControlSetDialogOptions(Value: TSelectDirOpts);
    {$ENDIF VCL}

    //IJvDynControlDatabase
    procedure ControlSetDatasource(Value: TDatasource);
    function ControlGetDatasource: TDatasource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;


  TJvDynControlVCLDBCheckBox = class (TDBCheckBox, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlDatabase)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(Value: string);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    //IJvDynControlDatabase
    procedure ControlSetDatasource(Value: TDatasource);
    function ControlGetDatasource: TDatasource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;

  TJvDynControlVCLDBMemo = class (TDBMemo, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlMemo, IJvDynControlReadOnly,
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
    procedure ControlSetHint(Value: string);

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
    procedure ControlSetDatasource(Value: TDatasource);
    function ControlGetDatasource: TDatasource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;

  TJvDynControlVCLDBRadioGroup = class (TDBRadioGroup, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlRadioGroup, IJvDynControlDatabase)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(Value: string);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetColumns(Value: Integer);

    //IJvDynControlDatabase
    procedure ControlSetDatasource(Value: TDatasource);
    function ControlGetDatasource: TDatasource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;

  TJvDynControlVCLDBListBox = class (TDBListBox, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlDblClick, IJvDynControlDatabase)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(Value: string);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetOnDblClick(Value: TNotifyEvent);

    //IJvDynControlDatabase
    procedure ControlSetDatasource(Value: TDatasource);
    function ControlGetDatasource: TDatasource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;


  TJvDynControlVCLDBComboBox = class (TDBComboBox, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlComboBox, IJvDynControlDatabase)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(Value: string);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetNewEntriesAllowed(Value: Boolean);

    //IJvDynControlDatabase
    procedure ControlSetDatasource(Value: TDatasource);
    function ControlGetDatasource: TDatasource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;

  TJvDynControlVCLDBImage = class (TDBImage, IUnknown, IJvDynControl,
      IJvDynControlImage, IJvDynControlDatabase)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(Value: string);

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
    procedure ControlSetDatasource(Value: TDatasource);
    function ControlGetDatasource: TDatasource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;

  TJvDynControlVCLDBText = class (TDBText, IUnknown, IJvDynControl, IJvDynControlDatabase)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(Value: string);

    //IJvDynControlDatabase
    procedure ControlSetDatasource(Value: TDatasource);
    function ControlGetDatasource: TDatasource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;


  TJvDynControlVCLDBGrid = class (TDBGrid, IUnknown, IJvDynControl, IJvDynControlDatabase)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(Value: string);

    //IJvDynControlDatabase
    procedure ControlSetDatasource(Value: TDatasource);
    function ControlGetDatasource: TDatasource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;

  TJvDynControlVCLDBNavigator = class (TDBNavigator, IUnknown, IJvDynControl, IJvDynControlDatabase)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(Value: string);

    //IJvDynControlDatabase
    procedure ControlSetDatasource(Value: TDatasource);
    function ControlGetDatasource: TDatasource;
    procedure ControlSetDataField(const Value: string);
    function ControlGetDataField: string;
  end;




function DynControlEngineVCLDB: TJvDynControlEngineDB;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  SysUtils,
  JvDynControlEngineVCL,
  JvConsts, JvJCLUtils;

var
  IntDynControlEngineVCLDB: TJvDynControlEngineDB = nil;

//=== { TJvDynControlVCLDBEdit } ===========================================

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

procedure TJvDynControlVCLDBEdit.ControlSetHint(Value: String);
begin
  Hint := Value;
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

procedure TJvDynControlVCLDBEdit.ControlSetEditMask(const Value: string);
begin
  //EditMask := Value;
end;

procedure TJvDynControlVCLDBEdit.ControlSetDatasource(Value: TDatasource);
begin
  Datasource := Value;
end;

function TJvDynControlVCLDBEdit.ControlGetDatasource: TDatasource;
begin
  Result := Datasource;
end;

procedure TJvDynControlVCLDBEdit.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;


function TJvDynControlVCLDBEdit.ControlGetDataField: string;
begin
  Result := Datafield;
end;

//=== { TJvDynControlVCLDBButtonEdit } =========================================

constructor TJvDynControlVCLDBButtonEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditControl := TDBEdit.Create(AOwner);
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

procedure TJvDynControlVCLDBButtonEdit.ControlSetHint(Value: String);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLDBButtonEdit.ControlSetValue(Value: Variant);
begin
  FEditControl.Text := Value;
end;

function TJvDynControlVCLDBButtonEdit.ControlGetValue: Variant;
begin
  Result := FEditControl.Text;
end;

{$IFDEF VisualCLX}
type
  TDBEditAccessProtected = class(TDBEdit);
{$ENDIF VisualCLX}

procedure TJvDynControlVCLDBButtonEdit.ControlSetPasswordChar(Value: Char);
begin
  {$IFDEF VCL}
  FEditControl.PasswordChar := Value;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  if Value = #0 then
    TDBEditAccessProtected(FEditControl).EchoMode := emNormal
  else
    TDBEditAccessProtected(FEditControl).EchoMode := emPassword;
  {$ENDIF VisualCLX}
end;

procedure TJvDynControlVCLDBButtonEdit.ControlSetEditMask(const Value: string);
begin
  //FEditControl.EditMask := Value;
end;

procedure TJvDynControlVCLDBButtonEdit.ControlSetOnButtonClick(Value: TNotifyEvent);
begin
  FButton.OnClick:= Value;
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

procedure TJvDynControlVCLDBButtonEdit.ControlSetDatasource(Value: TDatasource);
begin
  FEditControl.Datasource := Value;
end;

function TJvDynControlVCLDBButtonEdit.ControlGetDatasource: TDatasource;
begin
  Result := FEditControl.Datasource;
end;

procedure TJvDynControlVCLDBButtonEdit.ControlSetDataField(const Value: string);
begin
  FEditControl.DataField := Value;
end;


function TJvDynControlVCLDBButtonEdit.ControlGetDataField: string;
begin
  Result := FEditControl.Datafield;
end;


//=== { TJvDynControlVCLDBFileNameEdit } =======================================

constructor TJvDynControlVCLDBFileNameEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditControl := TDBEdit.Create(AOwner);
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

procedure TJvDynControlVCLDBFileNameEdit.ControlSetHint(Value: String);
begin
  FEditControl.Hint := Value;
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


procedure TJvDynControlVCLDBFileNameEdit.ControlSetDatasource(Value: TDatasource);
begin
  FEditControl.Datasource := Value;
end;

function TJvDynControlVCLDBFileNameEdit.ControlGetDatasource: TDatasource;
begin
  Result := FEditControl.Datasource;
end;

procedure TJvDynControlVCLDBFileNameEdit.ControlSetDataField(const Value: string);
begin
  FEditControl.DataField := Value;
end;

function TJvDynControlVCLDBFileNameEdit.ControlGetDataField: string;
begin
  Result := FEditControl.Datafield;
end;


//=== { TJvDynControlVCLDBDirectoryEdit } ======================================

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

procedure TJvDynControlVCLDBDirectoryEdit.ControlSetHint(Value: String);
begin
  FEditControl.Hint := Value;
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

{$IFDEF VCL}
procedure TJvDynControlVCLDBDirectoryEdit.ControlSetDialogOptions(Value: TSelectDirOpts);
begin
  FDialogOptions := Value;
end;
{$ENDIF VCL}


procedure TJvDynControlVCLDBDirectoryEdit.ControlSetDatasource(Value: TDatasource);
begin
  FEditControl.Datasource := Value;
end;

function TJvDynControlVCLDBDirectoryEdit.ControlGetDatasource: TDatasource;
begin
  Result := FEditControl.Datasource;
end;

procedure TJvDynControlVCLDBDirectoryEdit.ControlSetDataField(const Value: string);
begin
  FEditControl.DataField := Value;
end;

function TJvDynControlVCLDBDirectoryEdit.ControlGetDataField: string;
begin
  Result := FEditControl.Datafield;
end;



//=== { TJvDynControlVCLDBCheckBox } ===========================================

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

procedure TJvDynControlVCLDBCheckBox.ControlSetHint(Value: String);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLDBCheckBox.ControlSetValue(Value: Variant);
begin
  if VarType(Value) = varBoolean then
    Checked := Value
  else
    Checked := UpperCase(Value) = 'TRUE';
end;

function TJvDynControlVCLDBCheckBox.ControlGetValue: Variant;
begin
  Result := Checked;
end;

procedure TJvDynControlVCLDBCheckBox.ControlSetDatasource(Value: TDatasource);
begin
  Datasource := Value;
end;

function TJvDynControlVCLDBCheckBox.ControlGetDatasource: TDatasource;
begin
  Result := Datasource;
end;

procedure TJvDynControlVCLDBCheckBox.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;

function TJvDynControlVCLDBCheckBox.ControlGetDataField: string;
begin
  Result := DataField;
end;

//=== { TJvDynControlVCLDBMemo } ===============================================

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

procedure TJvDynControlVCLDBMemo.ControlSetHint(Value: String);
begin
  Hint := Value;
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

procedure TJvDynControlVCLDBMemo.ControlSetDatasource(Value: TDatasource);
begin
  Datasource := Value;
end;

function TJvDynControlVCLDBMemo.ControlGetDatasource: TDatasource;
begin
  Result := Datasource;
end;

procedure TJvDynControlVCLDBMemo.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;

function TJvDynControlVCLDBMemo.ControlGetDataField: string;
begin
  Result := DataField;
end;

//=== { TJvDynControlVCLDBRadioGroup } =========================================

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

procedure TJvDynControlVCLDBRadioGroup.ControlSetHint(Value: String);
begin
  Hint := Value;
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

procedure TJvDynControlVCLDBRadioGroup.ControlSetDatasource(Value: TDatasource);
begin
  Datasource := Value;
end;

function TJvDynControlVCLDBRadioGroup.ControlGetDatasource: TDatasource;
begin
  Result := Datasource;
end;

procedure TJvDynControlVCLDBRadioGroup.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;

function TJvDynControlVCLDBRadioGroup.ControlGetDataField: string;
begin
  Result := DataField;
end;


//=== { TJvDynControlVCLDBListBox } ============================================

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

procedure TJvDynControlVCLDBListBox.ControlSetHint(Value: String);
begin
  Hint := Value;
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

procedure TJvDynControlVCLDBListBox.ControlSetDatasource(Value: TDatasource);
begin
  Datasource := Value;
end;

function TJvDynControlVCLDBListBox.ControlGetDatasource: TDatasource;
begin
  Result := Datasource;
end;

procedure TJvDynControlVCLDBListBox.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;

function TJvDynControlVCLDBListBox.ControlGetDataField: string;
begin
  Result := DataField;
end;


//=== { TJvDynControlVCLDBComboBox } ===========================================

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

procedure TJvDynControlVCLDBComboBox.ControlSetHint(Value: String);
begin
  Hint := Value;
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

procedure TJvDynControlVCLDBComboBox.ControlSetDatasource(Value: TDatasource);
begin
  Datasource := Value;
end;

function TJvDynControlVCLDBComboBox.ControlGetDatasource: TDatasource;
begin
  Result := Datasource;
end;

procedure TJvDynControlVCLDBComboBox.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;

function TJvDynControlVCLDBComboBox.ControlGetDataField: string;
begin
  Result := DataField;
end;


//=== { TJvDynControlVCLDBImage } ==============================================

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

procedure TJvDynControlVCLDBImage.ControlSetHint(Value: String);
begin
  Hint := Value;
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

{$IFDEF VCL}
procedure TJvDynControlVCLDBImage.ControlSetProportional(Value: Boolean);
begin
  {$IFDEF COMPILER6_UP}
//  Proportional := Value;
  {$ENDIF COMPILER6_UP}
end;
{$ENDIF VCL}

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

procedure TJvDynControlVCLDBImage.ControlSetDatasource(Value: TDatasource);
begin
  Datasource := Value;
end;

function TJvDynControlVCLDBImage.ControlGetDatasource: TDatasource;
begin
  Result := Datasource;
end;

procedure TJvDynControlVCLDBImage.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;

function TJvDynControlVCLDBImage.ControlGetDataField: string;
begin
  Result := DataField;
end;


//=== { TJvDynControlVCLDBText } =========================================

procedure TJvDynControlVCLDBText.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLDBText.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlVCLDBText.ControlSetTabOrder(Value: Integer);
begin
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
    
procedure TJvDynControlVCLDBText.ControlSetHint(Value: String);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLDBText.ControlSetDatasource(Value: TDatasource);
begin
  Datasource := Value;
end;

function TJvDynControlVCLDBText.ControlGetDatasource: TDatasource;
begin
  Result := Datasource;
end;

procedure TJvDynControlVCLDBText.ControlSetDataField(const Value: string);
begin
  DataField := Value;
end;

function TJvDynControlVCLDBText.ControlGetDataField: string;
begin
  Result := DataField;
end;

//=== { TJvDynControlVCLDBGrid } ==========================================

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

procedure TJvDynControlVCLDBGrid.ControlSetHint(Value: String);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLDBGrid.ControlSetDatasource(Value: TDatasource);
begin
  Datasource := Value;
end;

function TJvDynControlVCLDBGrid.ControlGetDatasource: TDatasource;
begin
  Result := Datasource;
end;

procedure TJvDynControlVCLDBGrid.ControlSetDataField(const Value: string);
begin
end;

function TJvDynControlVCLDBGrid.ControlGetDataField: string;
begin
  Result := '';
end;

//=== { TJvDynControlVCLDBNavigator } ==========================================

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

procedure TJvDynControlVCLDBNavigator.ControlSetHint(Value: String);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLDBNavigator.ControlSetDatasource(Value: TDatasource);
begin
  Datasource := Value;
end;

function TJvDynControlVCLDBNavigator.ControlGetDatasource: TDatasource;
begin
  Result := Datasource;
end;

procedure TJvDynControlVCLDBNavigator.ControlSetDataField(const Value: string);
begin
end;

function TJvDynControlVCLDBNavigator.ControlGetDataField: string;
begin
  Result := '';
end;


//=== { TJvDynControlEngineVCLDB } =============================================

function DynControlEngineVCLDB: TJvDynControlEngineDB;
begin
  Result := IntDynControlEngineVCLDB;
end;

type
  TJvDynControlEngineVCLDB = class(TJvDynControlEngineDB)
  public
    procedure RegisterControls; override;
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

  IntDynControlEngineVCLDB := TJvDynControlEngineVCLDB.Create;
  SetDefaultDynControlEngineDB(IntDynControlEngineVCLDB);

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  FreeAndNil(IntDynControlEngineVCLDB);

end.
