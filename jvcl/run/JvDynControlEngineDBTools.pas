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

Uses Controls, DB, Forms,
     JvPanel,
     JvDynControlEngineTools, JvDynControlEngineDB;

type
  tJvDynControlDatasourceEditDialog = class(tObject)
  private
    fForm : TCustomForm;
    fDynControlEngineDB: TJvDynControlEngineDB;
    fDatasource : tDatasource;
    fDialogCaption : string;
    fPostCaption   : string;
    fCancelCaption : string;
    fIncludeNavigator : Boolean;
  protected
    procedure OnPostButtonClick (Sender : TObject);
    procedure OnCancelButtonClick (Sender : TObject);
    function IntDynControlEngineDB: TJvDynControlEngineDB;
  public
    function ShowDialog : tModalResult;
  published
    property Datasource : tDatasource read fDatasource write fDatasource;
    property PostCaption   : string read fPostCaption write fPostCaption;
    property CancelCaption : string read fCancelCaption write fCancelCaption;
    property DialogCaption : string read fDialogCaption write fDialogCaption;
    property DynControlEngineDB: TJvDynControlEngineDB read fDynControlEngineDB write fDynControlEngineDB;
    property IncludeNavigator : Boolean read fIncludeNavigator write fIncludeNavigator;
  end;


function ShowDatasourceEditDialog (aDatasource : tDatasource;
                                   const aDialogCaption, aPostCaption, aCancelCaption : string;
                                   aIncludeNavigator : Boolean;
                                   aDynControlEngineDB: TJvDynControlEngineDB = NIL) : tModalResult;

implementation

Uses SysUtils;


procedure tJvDynControlDatasourceEditDialog.OnPostButtonClick (Sender : TObject);
begin
  if Datasource.Dataset.State IN [dsInsert, dsEdit] then
  try
    Datasource.Dataset.Post;
    fForm.ModalResult := mrOk;
  except
    on e:exception do
      fForm.ModalResult := mrNone;
  end;
end;

procedure tJvDynControlDatasourceEditDialog.OnCancelButtonClick (Sender : TObject);
begin
  if Datasource.Dataset.State IN [dsInsert, dsEdit] then
    Datasource.Dataset.Cancel;
  fForm.ModalResult := mrCancel;
end;

function tJvDynControlDatasourceEditDialog.IntDynControlEngineDB: TJvDynControlEngineDB;
begin
  if Assigned(DynControlEngineDB) then
    Result := DynControlEngineDB
  else
    Result := DefaultDynControlEngineDB;
end;

function tJvDynControlDatasourceEditDialog.ShowDialog : tModalResult;
Var
  MainPanel : TWinControl;
  NavigatorPanel : TJvPanel;
  ArrangePanel : TJvPanel;
  Scrollbox    : TScrollbox;
  Navigator    : TControl;
begin
  fForm := CreateDynControlDialog (DialogCaption,
                                   PostCaption, CancelCaption,
                                   OnPostButtonClick, OnCancelButtonClick,
                                   MainPanel,
                                   IntDynControlEngineDB.DynControlEngine);
  try
    ScrollBox := TScrollbox.Create(fForm);
    Scrollbox.Parent := MainPanel;
    Scrollbox.Align := alClient;
    Scrollbox.BorderStyle := bsNone;
    Scrollbox.AutoScroll := True;
    ArrangePanel := TJvPanel.Create (fForm);
    with ArrangePanel,ArrangePanel.ArrangeSettings do
    begin
      Align := alClient;
      BevelInner := bvNone;
      BevelOuter := bvNone;
      Parent := Scrollbox;
      AutoArrange := True;
      AutoSize := asHeight;
      BorderLeft := 3;
      BorderTop := 3;
      WrapControls := True;
    end;
    if IncludeNavigator then
    begin
      NavigatorPanel := TJvPanel.Create (fForm);
      Navigator := IntDynControlEngineDB.CreateDBNavigatorControl(fForm, NavigatorPanel, '', Datasource);
      Navigator.Left := 3;
      Navigator.Top := 3;
      with NavigatorPanel do
      begin
        Align := alBottom;
        BevelInner := bvNone;
        BevelOuter := bvNone;
        Parent := MainPanel;
        Height := Navigator.Height+6;
      end;
    end;
    IntDynControlEngineDB.CreateControlsFromDatasourceOnControl(
        DataSource, ArrangePanel);
    fForm.ClientWidth := 450;
    fForm.ClientHeight := ArrangePanel.Height + 35;
    if Assigned(NavigatorPanel) then
      fForm.ClientHeight := fForm.ClientHeight+NavigatorPanel.Height;
    Result := fForm.ShowModal;
  finally
    fForm.Free;
  end;
end;

function ShowDatasourceEditDialog (aDatasource : tDatasource;
                                   const aDialogCaption, aPostCaption, aCancelCaption : string;
                                   aIncludeNavigator : Boolean;
                                   aDynControlEngineDB: TJvDynControlEngineDB = NIL) : tModalResult;
var
  Dialog : tJvDynControlDatasourceEditDialog;
begin
  Dialog := tJvDynControlDatasourceEditDialog.Create;
  try
    Dialog.Datasource:= aDatasource;
    Dialog.DialogCaption := aDialogCaption;
    Dialog.PostCaption := aPostCaption;
    Dialog.CancelCaption := aCancelCaption;
    Dialog.IncludeNavigator :=aIncludeNavigator;
    Dialog.DynControlEngineDB := aDynControlEngineDB;
    Result := Dialog.ShowDialog;
  finally
    Dialog.Free;
  end;
end;

end.
