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

unit JvDynControlEngineDBTools;

{$I jvcl.inc}

interface

uses
  Controls, DB, Forms, Classes,
  JvPanel, JvDynControlEngineTools, JvDynControlEngine, JvDynControlEngineDB;

type
  TJvCreateDataControlsEvent = procedure(ADynControlEngineDB: TJvDynControlEngineDB; AParentControl: TWinControl) of
    object;
  TJvDynControlDataSourceEditDialog = class(TObject)
  private
    FForm: TCustomForm;
    FDynControlEngineDB: TJvDynControlEngineDB;
    FDataSource: TDataSource;
    FDialogCaption: string;
    FPostButtonCaption: string;
    FCancelButtonCaption: string;
    FCloseButtonCaption: string;
    FIncludeNavigator: Boolean;
    FBorderStyle: TFormBorderStyle;
    FPosition: TPosition;
    FTop: Integer;
    FLeft: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FOnCreateDataControlsEvent: TJvCreateDataControlsEvent;
  protected
    procedure OnPostButtonClick(Sender: TObject);
    procedure OnCancelButtonClick(Sender: TObject);
    procedure OnCloseButtonClick(Sender: TObject);
    function IntDynControlEngineDB: TJvDynControlEngineDB;
    function CreateDynControlDialog(var AMainPanel: TWinControl): TCustomForm;
  public
    constructor Create;
    function ShowDialog: TModalResult;
  published
    property DataSource: TDataSource read FDataSource write FDataSource;
    property PostButtonCaption: string read FPostButtonCaption write FPostButtonCaption;
    property CancelButtonCaption: string read FCancelButtonCaption write FCancelButtonCaption;
    property CloseButtonCaption: string read FCloseButtonCaption write FCloseButtonCaption;
    property DialogCaption: string read FDialogCaption write FDialogCaption;
    property DynControlEngineDB: TJvDynControlEngineDB read FDynControlEngineDB write FDynControlEngineDB;
    property IncludeNavigator: Boolean read FIncludeNavigator write FIncludeNavigator;
    property BorderStyle: TFormBorderStyle read FBorderStyle write FBorderStyle default bsDialog;
    property Position: TPosition read FPosition write FPosition default poScreenCenter;
    property Top: Integer read FTop write FTop default 0;
    property Left: Integer read FLeft write FLeft default 0;
    property Width: Integer read FWidth write FWidth default 640;
    property Height: Integer read FHeight write FHeight default 480;
    property OnCreateDataControlsEvent: TJvCreateDataControlsEvent read FOnCreateDataControlsEvent write
      FOnCreateDataControlsEvent;
  end;

function ShowDatasourceEditDialog(ADataSource: TDataSource;
  const ADialogCaption, APostButtonCaption, ACancelButtonCaption, ACloseButtonCaption: string;
  AIncludeNavigator: Boolean;
  ADynControlEngineDB: TJvDynControlEngineDB = nil): TModalResult;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  StdCtrls, SysUtils,
  JvResources;

function TJvDynControlDataSourceEditDialog.CreateDynControlDialog(var AMainPanel: TWinControl): TCustomForm;
var
  DynControlEngine: TJvDynControlEngine;
  ButtonPanel: TWinControl;
  Form: TCustomForm;
  PostButton, CancelButton, CloseButton: TButtonControl;
  LeftPos: Integer;
begin
  if Assigned(IntDynControlEngineDB.DynControlEngine) then
    DynControlEngine := IntDynControlEngineDB.DynControlEngine
  else
    DynControlEngine := DefaultDynControlEngine;
  Form := DynControlEngine.CreateForm(DialogCaption, '');
  TForm(Form).Position := Position;
  TForm(Form).BorderStyle := BorderStyle;
  TForm(Form).Top := Top;
  TForm(Form).Left := Left;
  TForm(Form).Height := Height;
  TForm(Form).Width := Width;
  with TForm(Form) do
  begin
    FormStyle := fsNormal;
    BorderIcons := [];
  end;

  ButtonPanel := DynControlEngine.CreatePanelControl(Form, Form, '', '', alBottom);
  AMainPanel := DynControlEngine.CreatePanelControl(Form, Form, '', '', alClient);
  LeftPos := ButtonPanel.Width;
  if CloseButtonCaption <> '' then
  begin
    CloseButton := DynControlEngine.CreateButton(Form, ButtonPanel, '', CloseButtonCaption, '', OnCloseButtonClick,
      True, False);
    ButtonPanel.Height := CloseButton.Height + 6;
    CloseButton.Top := 3;
    CloseButton.Anchors := [akTop, akRight];
    CloseButton.Left := LeftPos - CloseButton.Width - 5;
    LeftPos := CloseButton.Left;
    CloseButton.TabOrder := 0;
  end;
  if CancelButtonCaption <> '' then
  begin
    CancelButton := DynControlEngine.CreateButton(Form, ButtonPanel, '', CancelButtonCaption, '', OnCancelButtonClick,
      True, False);
    ButtonPanel.Height := CancelButton.Height + 6;
    CancelButton.Top := 3;
    CancelButton.Anchors := [akTop, akRight];
    CancelButton.Left := LeftPos - CancelButton.Width - 5;
    LeftPos := CancelButton.Left;
    CancelButton.TabOrder := 0;
  end;
  if PostButtonCaption <> '' then
  begin
    PostButton := DynControlEngine.CreateButton(Form, ButtonPanel, '', PostButtonCaption, '', OnPostButtonClick, True,
      False);
    ButtonPanel.Height := PostButton.Height + 6;
    PostButton.Top := 3;
    PostButton.Anchors := [akTop, akRight];
    PostButton.Left := LeftPos - PostButton.Width - 5;
    PostButton.TabOrder := 0;
  end;
  Result := Form;
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

function TJvDynControlDataSourceEditDialog.IntDynControlEngineDB: TJvDynControlEngineDB;
begin
  if Assigned(DynControlEngineDB) then
    Result := DynControlEngineDB
  else
    Result := DefaultDynControlEngineDB;
end;

constructor TJvDynControlDataSourceEditDialog.Create;
begin
  inherited Create;
  FDialogCaption := '';
  FPostButtonCaption := RSSRWPostButtonCaption;
  FCancelButtonCaption := RSSRWCancelButtonCaption;
  FCloseButtonCaption := RSSRWCloseButtonCaption;
  FBorderStyle := bsDialog;
  FTop := 0;
  FLeft := 0;
  FWidth := 640;
  FHeight := 480;
  FPosition := poScreenCenter;
  FDynControlEngineDB := nil;
  FDatasource := nil;
end;

function TJvDynControlDataSourceEditDialog.ShowDialog: TModalResult;
var
  MainPanel: TWinControl;
  NavigatorPanel: TJvPanel;
  ArrangePanel: TJvPanel;
  ScrollBox: TScrollbox;
  Navigator: TControl;
begin
  FForm := CreateDynControlDialog(MainPanel);
  try
    ScrollBox := TScrollBox.Create(FForm);
    ScrollBox.Parent := MainPanel;
    ScrollBox.Align := alClient;
    ScrollBox.BorderStyle := bsNone;
    ScrollBox.AutoScroll := True;
    ArrangePanel := TJvPanel.Create(FForm);
    with ArrangePanel, ArrangePanel.ArrangeSettings do
    begin
      Align := alTop;
      BevelInner := bvNone;
      BevelOuter := bvNone;
      Parent := ScrollBox;
      AutoArrange := False;
      AutoSize := asHeight;
      BorderLeft := 3;
      BorderTop := 3;
      WrapControls := True;
    end;
    if IncludeNavigator then
    begin
      NavigatorPanel := TJvPanel.Create(FForm);
      Navigator := IntDynControlEngineDB.CreateDBNavigatorControl(FForm, NavigatorPanel, '', DataSource);
      Navigator.Left := 3;
      Navigator.Top := 3;
      with NavigatorPanel do
      begin
        Align := alBottom;
        BevelInner := bvNone;
        BevelOuter := bvNone;
        Parent := MainPanel;
        Height := Navigator.Height + 6;
      end;
    end
    else
      NavigatorPanel := nil;
    if Assigned(OnCreateDataControlsEvent) then
      OnCreateDataControlsEvent(IntDynControlEngineDB, ArrangePanel)
    else
      IntDynControlEngineDB.CreateControlsFromDatasourceOnControl(DataSource, ArrangePanel);
    ArrangePanel.ArrangeControls;
//    FForm.ClientWidth := 450;
    if Assigned(NavigatorPanel) then
      FForm.ClientHeight := ArrangePanel.Height + 35 + NavigatorPanel.Height
    else
      FForm.ClientHeight := ArrangePanel.Height + 35;
    ArrangePanel.ArrangeSettings.AutoArrange := True;
    Result := FForm.ShowModal;
  finally
    FForm.Free;
  end;
end;

function ShowDatasourceEditDialog(ADataSource: TDataSource;
  const ADialogCaption, APostButtonCaption, ACancelButtonCaption, ACloseButtonCaption: string;
  AIncludeNavigator: Boolean;
  ADynControlEngineDB: TJvDynControlEngineDB = nil): TModalResult;
var
  Dialog: TJvDynControlDataSourceEditDialog;
begin
  Dialog := TJvDynControlDataSourceEditDialog.Create;
  try
    Dialog.DataSource := ADataSource;
    Dialog.DialogCaption := ADialogCaption;
    Dialog.PostButtonCaption := APostButtonCaption;
    Dialog.CancelButtonCaption := ACancelButtonCaption;
    Dialog.CloseButtonCaption := ACloseButtonCaption;
    Dialog.IncludeNavigator := AIncludeNavigator;
    Dialog.DynControlEngineDB := ADynControlEngineDB;
    Result := Dialog.ShowDialog;
  finally
    Dialog.Free;
  end;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

