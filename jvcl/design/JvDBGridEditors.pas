{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBGridEditors.PAS, released on 2004-10-06.

The Initial Developer of the Original Code is Frédéric Leneuf-Magaud.
Portions created by Frédéric Leneuf-Magaud are Copyright (C) 2004 Frédéric Leneuf-Magaud.
All Rights Reserved.

Contributor(s): Olivier Sannier

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBGridEditors;

{$I jvcl.inc}

interface

uses
  Classes, Controls, Forms,
  DesignIntf, DesignEditors,
  TypInfo;

type
  TJvDBGridControlsProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;
  
implementation

uses
  JvDBGrid, JvDBGridControlsEditorForm,
  JvDsgnConsts, JvTypes;

procedure TJvDBGridControlsProperty.Edit;
var
  Dlg: TfrmJvDBGridControlsEditor;
  CloseDataset: Boolean;
begin
  CloseDataset := False;
  if TJvDBGrid(GetComponent(0)).DataSource = nil then
    raise EJVCLException.CreateRes(@RsEJvDBGridDataSourceNeeded);
  if TJvDBGrid(GetComponent(0)).DataSource.DataSet = nil then
    raise EJVCLException.CreateRes(@RsEJvDBGridDataSetNeeded);
  if not TJvDBGrid(GetComponent(0)).DataSource.DataSet.Active then
  try
    TJvDBGrid(GetComponent(0)).DataSource.DataSet.Open;
    CloseDataset := True;
  except
  end;

  Dlg := TfrmJvDBGridControlsEditor.Create(Application);
  try
    Dlg.JvDBGridControls.Assign(TJvDBGridControls(GetOrdValue));
    Dlg.Grid := TJvDBGrid(GetComponent(0));
    Dlg.Initialize;
    Dlg.ShowModal;
    if Dlg.ModalResult = mrOk then
    begin
      TJvDBGridControls(GetOrdValue).Assign(Dlg.JvDBGridControls);
      Modified;
    end;
  finally
    Dlg.Free;
    if CloseDataset then
      TJvDBGrid(GetComponent(0)).DataSource.DataSet.Close;
  end;
end;

function TJvDBGridControlsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TJvDBGridControlsProperty.GetValue: string;
begin
  Result := '(' + {$IFDEF RTL200_UP}UTF8ToString{$ENDIF RTL200_UP}(GetPropType^.Name) + ')';
end;

end.