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

interface

uses
  Controls, DB, Forms,
  JvPanel, JvDynControlEngineTools, JvDynControlEngineDB;

type
  TJvDynControlDataSourceEditDialog = class(TObject)
  private
    FForm: TCustomForm;
    FDynControlEngineDB: TJvDynControlEngineDB;
    FDataSource: TDataSource;
    FDialogCaption: string;
    FPostCaption: string;
    FCancelCaption: string;
    FIncludeNavigator: Boolean;
  protected
    procedure OnPostButtonClick(Sender: TObject);
    procedure OnCancelButtonClick(Sender: TObject);
    function IntDynControlEngineDB: TJvDynControlEngineDB;
  public
    function ShowDialog: TModalResult;
  published
    property DataSource: TDataSource read FDataSource write FDataSource;
    property PostCaption: string read FPostCaption write FPostCaption;
    property CancelCaption: string read FCancelCaption write FCancelCaption;
    property DialogCaption: string read FDialogCaption write FDialogCaption;
    property DynControlEngineDB: TJvDynControlEngineDB read FDynControlEngineDB write FDynControlEngineDB;
    property IncludeNavigator: Boolean read FIncludeNavigator write FIncludeNavigator;
  end;

function ShowDatasourceEditDialog(ADataSource: TDataSource;
  const ADialogCaption, APostCaption, ACancelCaption: string;
  AIncludeNavigator: Boolean;
  ADynControlEngineDB: TJvDynControlEngineDB = nil): TModalResult;

implementation

uses
  SysUtils;

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

function TJvDynControlDataSourceEditDialog.IntDynControlEngineDB: TJvDynControlEngineDB;
begin
  if Assigned(DynControlEngineDB) then
    Result := DynControlEngineDB
  else
    Result := DefaultDynControlEngineDB;
end;

function TJvDynControlDataSourceEditDialog.ShowDialog: TModalResult;
var
  MainPanel: TWinControl;
  NavigatorPanel: TJvPanel;
  ArrangePanel: TJvPanel;
  ScrollBox: TScrollbox;
  Navigator: TControl;
begin
  FForm := CreateDynControlDialog(DialogCaption, PostCaption, CancelCaption,
    OnPostButtonClick, OnCancelButtonClick, MainPanel, IntDynControlEngineDB.DynControlEngine);
  try
    ScrollBox := TScrollBox.Create(FForm);
    ScrollBox.Parent := MainPanel;
    ScrollBox.Align := alClient;
    ScrollBox.BorderStyle := bsNone;
    ScrollBox.AutoScroll := True;
    ArrangePanel := TJvPanel.Create(FForm);
    with ArrangePanel, ArrangePanel.ArrangeSettings do
    begin
      Align := alClient;
      BevelInner := bvNone;
      BevelOuter := bvNone;
      Parent := ScrollBox;
      AutoArrange := True;
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
    end;
    IntDynControlEngineDB.CreateControlsFromDatasourceOnControl(DataSource, ArrangePanel);
    FForm.ClientWidth := 450;
    FForm.ClientHeight := ArrangePanel.Height + 35;
    if Assigned(NavigatorPanel) then
      FForm.ClientHeight := FForm.ClientHeight + NavigatorPanel.Height;
    Result := FForm.ShowModal;
  finally
    FForm.Free;
  end;
end;

function ShowDatasourceEditDialog(ADataSource: TDataSource;
  const ADialogCaption, APostCaption, ACancelCaption: string;
  AIncludeNavigator: Boolean;
  ADynControlEngineDB: TJvDynControlEngineDB = nil): TModalResult;
var
  Dialog: TJvDynControlDataSourceEditDialog;
begin
  Dialog := TJvDynControlDataSourceEditDialog.Create;
  try
    Dialog.DataSource := ADataSource;
    Dialog.DialogCaption := ADialogCaption;
    Dialog.PostCaption := APostCaption;
    Dialog.CancelCaption := ACancelCaption;
    Dialog.IncludeNavigator := AIncludeNavigator;
    Dialog.DynControlEngineDB := ADynControlEngineDB;
    Result := Dialog.ShowDialog;
  finally
    Dialog.Free;
  end;
end;

end.

