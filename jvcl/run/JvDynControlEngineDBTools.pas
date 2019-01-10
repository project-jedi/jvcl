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

unit JvDynControlEngineDBTools;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Controls, DB, Classes,
  ActnList, Forms, Graphics, JvTypes,
  JvPanel, JvDynControlEngine, JvDynControlEngineDB;

type
  TJvDataSourceEditDialogCreateDataControlsEvent = procedure(ADatacomponent :
      TComponent; ADynControlEngineDB: TJvDynControlEngineDB; AParentControl:
      TWinControl; AFieldCreateOptions: TJvCreateDBFieldsOnControlOptions) of
      object;
  TJvDataSourceEditDialogOnFormShowEvent = procedure(ADatacomponent :
      TComponent; ADynControlEngineDB: TJvDynControlEngineDB) of
      object;

  TJvDynControlSizeConstraints = class(TPersistent)
  private
    FMaxHeight: TConstraintSize;
    FMaxWidth: TConstraintSize;
    FMinWidth: TConstraintSize;
    FMinHeight: TConstraintSize;
    procedure SetConstraints(Index: Integer; Value: TConstraintSize);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property MaxHeight: TConstraintSize index 0 read FMaxHeight write SetConstraints default 0;
    property MaxWidth: TConstraintSize index 1 read FMaxWidth write SetConstraints default 0;
    property MinHeight: TConstraintSize index 2 read FMinHeight write SetConstraints default 0;
    property MinWidth: TConstraintSize index 3 read FMinWidth write SetConstraints default 0;
  end;

  TJvDynControlDataSourceEditDialog = class(TJvPersistentProperty)
    procedure OnFormShow(Sender: TObject);
  private
    FForm: TCustomForm;
    FDynControlEngineDB: TJvDynControlEngineDB;
    FDataSource: TDataSource;
    FDataComponent: TComponent;
    FDialogCaption: string;
    FPostButtonCaption: string;
    FCancelButtonCaption: string;
    FCloseButtonCaption: string;
    FPostButtonGlyph: TBitmap;
    FCancelButtonGlyph: TBitmap;
    FCloseButtonGlyph: TBitmap;
    FIncludeNavigator: Boolean;
    FBorderStyle: TFormBorderStyle;
    FPosition: TPosition;
    FTop: Integer;
    FLeft: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FOnCreateDataControlsEvent: TJvDataSourceEditDialogCreateDataControlsEvent;
    FArrangeConstraints: TJvDynControlSizeConstraints;
    FArrangeSettings: TJvArrangeSettings;
    FFieldCreateOptions: TJvCreateDBFieldsOnControlOptions;
    FScrollBox: TScrollBox;
    FNavigatorPanel: TJvPanel;
    FButtonPanel: TWinControl;
    FPostAction: TCustomAction;
    FCancelAction: TCustomAction;
    FOnFormShowEvent: TJvDataSourceEditDialogOnFormShowEvent;
  protected
    function GetDynControlEngineDB: TJvDynControlEngineDB;
    procedure SetDataComponent(Value: TComponent);
    procedure OnPostButtonClick(Sender: TObject);
    procedure OnCancelButtonClick(Sender: TObject);
    procedure OnCloseButtonClick(Sender: TObject);
    function CreateDynControlDialog(var AMainPanel: TWinControl): TCustomForm;
    procedure SetArrangeSettings(Value: TJvArrangeSettings);
    procedure SetArrangeConstraints(Value: TJvDynControlSizeConstraints);
    procedure SetFieldCreateOptions(Value: TJvCreateDBFieldsOnControlOptions);
    procedure ArrangePanelChangedWidth(Sender: TObject; ChangedSize: Integer);
    procedure ArrangePanelChangedHeight(Sender: TObject; ChangedSize: Integer);
    procedure CreateDataControls(ADatacomponent : TComponent; ADynControlEngineDB:
        TJvDynControlEngineDB; AParentControl: TWinControl; AFieldCreateOptions:
        TJvCreateDBFieldsOnControlOptions); virtual;
    property DataSource: TDataSource read FDataSource;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function ShowDialog: TModalResult;
  published
    property DataComponent: TComponent read FDataComponent write SetDataComponent;
    property PostButtonCaption: string read FPostButtonCaption write FPostButtonCaption;
    property CancelButtonCaption: string read FCancelButtonCaption write FCancelButtonCaption;
    property CloseButtonCaption: string read FCloseButtonCaption write FCloseButtonCaption;
    property PostButtonGlyph: TBitmap read FPostButtonGlyph write FPostButtonGlyph;
    property CancelButtonGlyph: TBitmap read FCancelButtonGlyph write FCancelButtonGlyph;
    property CloseButtonGlyph: TBitmap read FCloseButtonGlyph write FCloseButtonGlyph;
    property DialogCaption: string read FDialogCaption write FDialogCaption;
    property DynControlEngineDB: TJvDynControlEngineDB read GetDynControlEngineDB write FDynControlEngineDB;
    property IncludeNavigator: Boolean read FIncludeNavigator write FIncludeNavigator;
    property BorderStyle: TFormBorderStyle read FBorderStyle write FBorderStyle default bsDialog;
    property Position: TPosition read FPosition write FPosition default {$IFDEF COMPILER7_UP} poOwnerFormCenter {$ELSE} poScreenCenter{$ENDIF COMPILER7_UP};
    property Top: Integer read FTop write FTop default 0;
    property Left: Integer read FLeft write FLeft default 0;
    property Width: Integer read FWidth write FWidth default 0;
    property Height: Integer read FHeight write FHeight default 0;
    property OnCreateDataControlsEvent: TJvDataSourceEditDialogCreateDataControlsEvent read FOnCreateDataControlsEvent write
      FOnCreateDataControlsEvent;
    property ArrangeConstraints: TJvDynControlSizeConstraints read FArrangeConstraints write SetArrangeConstraints;
    property ArrangeSettings: TJvArrangeSettings read FArrangeSettings write
        SetArrangeSettings;
    property FieldCreateOptions: TJvCreateDBFieldsOnControlOptions read
        FFieldCreateOptions write SetFieldCreateOptions;
    property OnFormShowEvent: TJvDataSourceEditDialogOnFormShowEvent read
        FOnFormShowEvent write FOnFormShowEvent;
  end;


function ShowDataSourceEditDialog(ADataComponent: TComponent; const
    ADialogCaption, APostButtonCaption, ACancelButtonCaption,
    ACloseButtonCaption: string; AIncludeNavigator: Boolean;
    AFieldCreateOptions: TJvCreateDBFieldsOnControlOptions = nil;
    AArrangeConstraints: TJvDynControlSizeConstraints = nil; AArrangeSettings:
    TJvArrangeSettings = nil; ADynControlEngineDB: TJvDynControlEngineDB = nil;
    ACreateDataControlsEvent: TJvDataSourceEditDialogCreateDataControlsEvent =
    nil; AOnFormShowEvent: TJvDataSourceEditDialogOnFormShowEvent = nil):
    TModalResult;


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
  StdCtrls, SysUtils,
  JvDBActions, JvDynControlEngineIntf, JvResources;

procedure TJvDynControlDataSourceEditDialog.SetDataComponent(Value: TComponent);
begin
  FDataComponent := Value;
  FDataSource := DynControlengineDB.GetDataSourceFromDataComponent(Value);
end;

function TJvDynControlDataSourceEditDialog.GetDynControlEngineDB: TJvDynControlEngineDB;
begin
  if Assigned(FDynControlEngineDB) then
    Result := FDynControlEngineDB
  else
    Result := DefaultDynControlEngineDB;
end;

procedure TJvDynControlDataSourceEditDialog.OnPostButtonClick(Sender: TObject);
begin
  if DataSource.Dataset.State in [dsInsert, dsEdit] then
  try
    DataSource.Dataset.Post;
    FForm.ModalResult := mrOk;
  except
    FForm.ModalResult := mrNone;
  end;
end;

procedure TJvDynControlDataSourceEditDialog.OnCancelButtonClick(Sender: TObject);
begin
  if DataSource.Dataset.State in [dsInsert, dsEdit] then
    DataSource.Dataset.Cancel;
  FForm.ModalResult := mrCancel;
end;

procedure TJvDynControlDataSourceEditDialog.OnCloseButtonClick(Sender: TObject);
begin
  FForm.ModalResult := mrAbort;
end;

constructor TJvDynControlDataSourceEditDialog.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  
  FDialogCaption := '';
  FPostButtonCaption := RSSRWPostButtonCaption;
  FCancelButtonCaption := RSSRWCancelButtonCaption;
  FCloseButtonCaption := RSSRWCloseButtonCaption;
  FPostButtonGlyph := nil;
  FCancelButtonGlyph := nil;
  FCloseButtonGlyph := nil;
  FBorderStyle := bsDialog;
  FTop := 0;
  FLeft := 0;
  FWidth := 0;
  FHeight := 0;
  {$IFDEF COMPILER7_UP}
  FPosition := poOwnerFormCenter;
  {$ELSE}
  FPosition := poScreenCenter;
  {$ENDIF COMPILER7_UP};  
  FDynControlEngineDB := nil;
  FDataSource := nil;
  FArrangeSettings := TJvArrangeSettings.Create(Self);
  FArrangeSettings.AutoSize := asBoth;
  FArrangeSettings.DistanceHorizontal := 3;
  FArrangeSettings.DistanceVertical := 3;
  FArrangeSettings.BorderLeft := 3;
  FArrangeSettings.BorderTop := 3;
  FArrangeSettings.WrapControls := True;
  FArrangeConstraints := TJvDynControlSizeConstraints.Create;
  FArrangeConstraints.MaxHeight := 480;
  FArrangeConstraints.MaxWidth := 640;
  FFieldCreateOptions := TJvCreateDBFieldsOnControlOptions.Create;
end;

destructor TJvDynControlDataSourceEditDialog.Destroy;
begin
  FFieldCreateOptions.Free;
  FArrangeConstraints.Free;
  FArrangeSettings.Free;
  inherited Destroy;
end;

procedure TJvDynControlDataSourceEditDialog.SetArrangeSettings(Value: TJvArrangeSettings);
begin
  FArrangeSettings.Assign(Value);
end;

procedure TJvDynControlDataSourceEditDialog.SetArrangeConstraints(Value: TJvDynControlSizeConstraints);
begin
  FArrangeConstraints.Assign(Value);
end;

procedure TJvDynControlDataSourceEditDialog.SetFieldCreateOptions(Value: TJvCreateDBFieldsOnControlOptions);
begin
  FFieldCreateOptions.Assign(Value);
end;

procedure TJvDynControlDataSourceEditDialog.ArrangePanelChangedWidth(Sender: TObject; ChangedSize: Integer);
begin
  FForm.ClientWidth := ChangedSize;
end;

procedure TJvDynControlDataSourceEditDialog.ArrangePanelChangedHeight(Sender: TObject; ChangedSize: Integer);
begin
  if Assigned(FNavigatorPanel) then
    FForm.ClientHeight := ChangedSize + FButtonPanel.Height + FNavigatorPanel.Height + 35
  else
    FForm.ClientHeight := ChangedSize + FButtonPanel.Height + 35;
end;

type
  TAccessControl = class(TControl);

procedure TJvDynControlDataSourceEditDialog.CreateDataControls(ADatacomponent :
    TComponent; ADynControlEngineDB: TJvDynControlEngineDB; AParentControl:
    TWinControl; AFieldCreateOptions: TJvCreateDBFieldsOnControlOptions);
begin
  ADynControlEngineDB.CreateControlsFromDataComponentOnControl(ADataComponent, AParentControl, AFieldCreateOptions);
end;

function TJvDynControlDataSourceEditDialog.CreateDynControlDialog(var AMainPanel: TWinControl): TCustomForm;
var
  DynControlEngine: TJvDynControlEngine;
  Form: TCustomForm;
  PostButton, CancelButton, CloseButton: TButtonControl;
  LeftPos: Integer;
  DynCtrlButton: IJvDynControlButton;
  DynCtrlAction: IJvDynControlAction;

  function CalcButtonWidth(ACaptionWidth: Integer; AGlyph: TBitmap): Integer;
  begin
    Result := 4;
    if Assigned(AGlyph) then
      Result := Result + AGlyph.Width;
    if ACaptionWidth > 0 then
    begin
      Result := Result + ACaptionWidth;
      if ACaptionWidth > 0 then
        Result := Result + 4;
    end;
  end;

begin
  DynControlEngine := DynControlEngineDB.DynControlEngine;
  Form := DynControlEngine.CreateForm(DialogCaption, '');
  TForm(Form).Position := Position;
  TForm(Form).BorderStyle := BorderStyle;
  TForm(Form).FormStyle := fsNormal;
  TForm(Form).BorderIcons := [];

  FButtonPanel := DynControlEngine.CreatePanelControl(Form, Form, '', '', alBottom);
  FButtonPanel.Width := Form.ClientWidth;
  AMainPanel := DynControlEngine.CreatePanelControl(Form, Form, '', '', alClient);
  LeftPos := FButtonPanel.Width;
  if (CloseButtonCaption <> '') or Assigned(CloseButtonGlyph) then
  begin
    CloseButton := DynControlEngine.CreateButton(Form, FButtonPanel, '', CloseButtonCaption, '', OnCloseButtonClick,
      True, False);
    FButtonPanel.Height := CloseButton.Height + 6;
    CloseButton.Top := 3;
    CloseButton.Anchors := [akTop, akRight];
    CloseButton.Width := CalcButtonWidth(Form.Canvas.TextWidth(CloseButtonCaption), CloseButtonGlyph);
    CloseButton.Left := LeftPos - CloseButton.Width - 5;
    LeftPos := CloseButton.Left;
    CloseButton.TabOrder := 0;
    if Supports(CloseButton, IJvDynControlButton, DynCtrlButton) then
    begin
      DynCtrlButton.ControlSetDefault(True);
      DynCtrlButton.ControlSetCancel(True);
      if Assigned(CloseButtonGlyph) then
        DynCtrlButton.ControlSetGlyph(CloseButtonGlyph);
    end;
  end;
  if (CancelButtonCaption <> '') or Assigned(CancelButtonGlyph) then
  begin
    CancelButton := DynControlEngine.CreateButton(Form, FButtonPanel, '', CancelButtonCaption, '', OnCancelButtonClick,
      True, False);
    if Supports(CancelButton, IJvDynControlAction, DynCtrlAction) then
    begin
      FCancelAction := TJvDatabaseCancelAction.Create (Form);
      FCancelAction.Caption := CancelButtonCaption;
      DynCtrlAction.ControlSetAction(FCancelAction);
    end
    else
      FCancelAction := nil;
    FButtonPanel.Height := CancelButton.Height + 6;
    CancelButton.Top := 3;
    CancelButton.Anchors := [akTop, akRight];
    CancelButton.Width := CalcButtonWidth(Form.Canvas.TextWidth(CancelButtonCaption), CancelButtonGlyph);
    CancelButton.Left := LeftPos - CancelButton.Width - 5;
    LeftPos := CancelButton.Left;
    CancelButton.TabOrder := 0;
    if Supports(CancelButton, IJvDynControlButton, DynCtrlButton) then
    begin
      DynCtrlButton.ControlSetDefault (False);
      DynCtrlButton.ControlSetCancel(False);
      if Assigned(CancelButtonGlyph) then
        DynCtrlButton.ControlSetGlyph(CancelButtonGlyph);
    end;
  end;
  if (PostButtonCaption <> '') or Assigned(PostButtonGlyph) then
  begin
    PostButton := DynControlEngine.CreateButton(Form, FButtonPanel, '', PostButtonCaption, '', OnPostButtonClick, True,
      False);
    FButtonPanel.Height := PostButton.Height + 6;
    if Supports(PostButton, IJvDynControlAction, DynCtrlAction) then
    begin
      FPostAction := TJvDatabasePostAction.Create (Form);
      FPostAction.Caption := PostButtonCaption;
      DynCtrlAction.ControlSetAction(FPostAction);
    end
    else
      FPostAction := nil;
    PostButton.Top := 3;
    PostButton.Anchors := [akTop, akRight];
    PostButton.Width := CalcButtonWidth(Form.Canvas.TextWidth(PostButtonCaption), PostButtonGlyph);
    PostButton.Left := LeftPos - PostButton.Width - 5;
    PostButton.TabOrder := 0;
    if Supports(PostButton, IJvDynControlButton, DynCtrlButton) then
    begin
      DynCtrlButton.ControlSetDefault (False);
      DynCtrlButton.ControlSetCancel(False);
      if Assigned(PostButtonGlyph) then
        DynCtrlButton.ControlSetGlyph(PostButtonGlyph);
    end;
  end;
  TForm(Form).Top := Top;
  TForm(Form).Left := Left;
  TForm(Form).Height := Height;
  TForm(Form).Width := Width;
  Result := Form;
end;

procedure TJvDynControlDataSourceEditDialog.OnFormShow(Sender: TObject);
begin
  if Assigned(OnFormShowEvent) then
    OnFormShowEvent(DataComponent, DynControlEngineDB);
end;

function TJvDynControlDataSourceEditDialog.ShowDialog: TModalResult;
var
  MainPanel: TWinControl;
  ArrangePanel: TJvPanel;
  Navigator: TControl;
begin
  FForm := CreateDynControlDialog(MainPanel);
  try
    FScrollBox := TScrollBox.Create(FForm);
    FScrollBox.Parent := MainPanel;
    FScrollBox.Align := alClient;
    FScrollBox.BorderStyle := bsNone;
    FScrollBox.AutoScroll := True;
    {$IFDEF COMPILER10_UP}
    FScrollBox.ParentBackground := True;
    {$ENDIF COMPILER10_UP}
    FForm.Constraints.Assign(ArrangeConstraints);
    ArrangePanel := TJvPanel.Create(FForm);
    ArrangePanel.Align := alTop;
    ArrangePanel.BevelInner := bvNone;
    ArrangePanel.BevelOuter := bvNone;
    ArrangePanel.Parent := FScrollBox;
    ArrangePanel.OnChangedWidth := ArrangePanelChangedWidth;
    ArrangePanel.OnChangedHeight := ArrangePanelChangedHeight;
    ArrangePanel.ArrangeSettings := ArrangeSettings;
    if ArrangeSettings.MaxWidth = 0 then
      ArrangePanel.ArrangeSettings.MaxWidth := ArrangeConstraints.MaxWidth;
    if ArrangeSettings.MaxWidth = 0 then
      ArrangeSettings.MaxWidth := Screen.Width;
    FNavigatorPanel := TJvPanel.Create(FForm);
    Navigator := DynControlEngineDB.CreateDBNavigatorControl(FForm, FNavigatorPanel, '', DataSource);
    Navigator.Left := 3;
    Navigator.Top := 3;
    FNavigatorPanel.Align := alBottom;
    FNavigatorPanel.BevelInner := bvNone;
    FNavigatorPanel.BevelOuter := bvNone;
    FNavigatorPanel.Parent := MainPanel;
    FNavigatorPanel.Height := Navigator.Height + 6;
    FNavigatorPanel.Visible := IncludeNavigator;
    if Assigned(OnCreateDataControlsEvent) then
      OnCreateDataControlsEvent(DataComponent, DynControlEngineDB, ArrangePanel, FieldCreateOptions)
    else
      CreateDataControls(DataComponent, DynControlEngineDB, ArrangePanel, FieldCreateOptions);
    if Assigned (FCancelAction) then
      TJvDatabaseCancelAction(FCancelAction).DataComponent := DataComponent;
    if Assigned (FPostAction) then
      TJvDatabaseCancelAction(FPostAction).DataComponent := DataComponent;
    TForm(FForm).Top := Top;
    TForm(FForm).Left := Left;
    TForm(FForm).Height := Height;
    TForm(FForm).Width := Width;
    TForm(FForm).OnShow := OnFormShow;
    ArrangePanel.ArrangeSettings.AutoArrange := True;
    MainPanel.TabOrder := 0;
    Result := FForm.ShowModal;
  finally
    FForm.Free;
  end;
end;

function ShowDataSourceEditDialog(ADataComponent: TComponent; const
    ADialogCaption, APostButtonCaption, ACancelButtonCaption,
    ACloseButtonCaption: string; AIncludeNavigator: Boolean;
    AFieldCreateOptions: TJvCreateDBFieldsOnControlOptions = nil;
    AArrangeConstraints: TJvDynControlSizeConstraints = nil; AArrangeSettings:
    TJvArrangeSettings = nil; ADynControlEngineDB: TJvDynControlEngineDB = nil;
    ACreateDataControlsEvent: TJvDataSourceEditDialogCreateDataControlsEvent =
    nil; AOnFormShowEvent: TJvDataSourceEditDialogOnFormShowEvent = nil):
    TModalResult;
var
  Dialog: TJvDynControlDataSourceEditDialog;
begin
  Dialog := TJvDynControlDataSourceEditDialog.Create(ADataComponent);
  try
    Dialog.DataComponent := ADataComponent;
    Dialog.DialogCaption := ADialogCaption;
    Dialog.PostButtonCaption := APostButtonCaption;
    Dialog.CancelButtonCaption := ACancelButtonCaption;
    Dialog.CloseButtonCaption := ACloseButtonCaption;
    Dialog.IncludeNavigator := AIncludeNavigator;
    Dialog.DynControlEngineDB := ADynControlEngineDB;
    if Assigned(AFieldCreateOptions) then
      Dialog.FieldCreateOptions := AFieldCreateOptions;
    if Assigned(AArrangeSettings) then
      Dialog.ArrangeSettings := AArrangeSettings;
    if Assigned(AArrangeConstraints) then
      Dialog.ArrangeConstraints := AArrangeConstraints;
    Dialog.OnCreateDataControlsEvent := ACreateDataControlsEvent;
    Dialog.OnFormShowEvent := AOnFormShowEvent;
    Result := Dialog.ShowDialog;
  finally
    Dialog.Free;
  end;
end;

procedure TJvDynControlSizeConstraints.Assign(Source: TPersistent);
begin
  if (Source = Self) then
    Exit;
  if Source is TJvDynControlSizeConstraints then
  begin
    TJvDynControlSizeConstraints(Source).MinWidth := MinWidth;
    TJvDynControlSizeConstraints(Source).MinHeight := MinHeight;
    TJvDynControlSizeConstraints(Source).MaxWidth := MaxWidth;
    TJvDynControlSizeConstraints(Source).MaxHeight := MaxHeight;
  end
  else
    inherited assign(Source);
end;

procedure TJvDynControlSizeConstraints.AssignTo(Dest: TPersistent);
begin
  if Dest is TSizeConstraints then
  begin
    TSizeConstraints (Dest).MinWidth := MinWidth;
    TSizeConstraints (Dest).MinHeight := MinHeight;
    TSizeConstraints (Dest).MaxWidth := MaxWidth;
    TSizeConstraints (Dest).MaxHeight := MaxHeight;
  end
  else
    inherited;
end;

procedure TJvDynControlSizeConstraints.SetConstraints(Index: Integer; Value: TConstraintSize);
begin
  case Index of
    0:
      if Value <> FMaxHeight then
      begin
        FMaxHeight := Value;
        if (Value > 0) and (Value < FMinHeight) then
          FMinHeight := Value;
      end;
    1:
      if Value <> FMaxWidth then
      begin
        FMaxWidth := Value;
        if (Value > 0) and (Value < FMinWidth) then
          FMinWidth := Value;
      end;
    2:
      if Value <> FMinHeight then
      begin
        FMinHeight := Value;
        if (FMaxHeight > 0) and (Value > FMaxHeight) then
          FMaxHeight := Value;
      end;
    3:
      if Value <> FMinWidth then
      begin
        FMinWidth := Value;
        if (FMaxWidth > 0) and (Value > FMaxWidth) then
          FMaxWidth := Value;
      end;
  end;
end;



{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization

  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

