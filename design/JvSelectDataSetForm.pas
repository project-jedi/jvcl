{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSelDSFrm.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvSelectDataSetForm;

interface

{$IFNDEF DelphiPersonalEdition}

uses
  Windows, SysUtils, Classes, Controls, Forms, DB, StdCtrls,
  {$IFDEF COMPILER6_UP}
  RTLConsts, DesignIntf, DesignEditors, VCLEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvDsgnTypes;

type
  TJvSelectDataSetForm = class(TForm)
    GroupBox: TGroupBox;
    DataSetList: TListBox;
    OkBtn: TButton;
    CancelBtn: TButton;
    procedure DataSetListDblClick(Sender: TObject);
    procedure DataSetListKeyPress(Sender: TObject; var Key: Char);
  private
    FDesigner: IDesigner;
    FExclude: string;
    procedure FillDataSetList(ExcludeDataSet: TDataSet);
    procedure AddDataSet(const S: string);
  end;

function SelectDataSet(ADesigner: IJvFormDesigner; const ACaption: string; ExcludeDataSet: TDataSet): TDataSet;

{$ENDIF DelphiPersonalEdition}

implementation

{$IFNDEF DelphiPersonalEdition}

uses
  TypInfo,
  DsnDbCst, DSDesign,
  JvJVCLUtils, JvConsts;

{$IFDEF VCL}
{$R *.dfm}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$R *.xfm}
{$ENDIF VisualCLX}

function SelectDataSet(ADesigner: IJvFormDesigner; const ACaption: string; ExcludeDataSet: TDataSet): TDataSet;
begin
  Result := nil;
  with TJvSelectDataSetForm.Create(Application) do
  try
    if ACaption <> '' then
      Caption := ACaption;
    FDesigner := ADesigner;
    FillDataSetList(ExcludeDataSet);
    if ShowModal = mrOk then
      if DataSetList.ItemIndex >= 0 then
      begin
        with DataSetList do
          {$IFDEF COMPILER6_UP}
          Result := FDesigner.GetComponent(Items[ItemIndex]) as TDataSet;
          {$ELSE}
          Result := FDesigner.Form.FindComponent(Items[ItemIndex]) as TDataSet;
          {$ENDIF COMPILER6_UP}
      end;
  finally
    Free;
  end;
end;

//=== { TJvSelectDataSetForm } ===============================================

procedure TJvSelectDataSetForm.AddDataSet(const S: string);
begin
  if (S <> '') and (S <> FExclude) then
    DataSetList.Items.Add(S);
end;

procedure TJvSelectDataSetForm.FillDataSetList(ExcludeDataSet: TDataSet);
{$IFNDEF COMPILER6_UP}
var
  I: Integer;
  Component: TComponent;
{$ENDIF COMPILER6_UP}
begin
  DataSetList.Items.BeginUpdate;
  try
    DataSetList.Clear;
    FExclude := '';
    if ExcludeDataSet <> nil then
      FExclude := ExcludeDataSet.Name;
    {$IFDEF COMPILER6_UP}
    FDesigner.GetComponentNames(GetTypeData(TypeInfo(TDataSet)), AddDataSet);
    {$ELSE}
    for I := 0 to FDesigner.Form.ComponentCount - 1 do
    begin
      Component := FDesigner.Form.Components[I];
      if (Component is TDataSet) and (Component <> ExcludeDataSet) then
        AddDataSet(Component.Name);
    end;
    {$ENDIF COMPILER6_UP}
    with DataSetList do
    begin
      if Items.Count > 0 then
        ItemIndex := 0;
      Enabled := Items.Count > 0;
      OkBtn.Enabled := (ItemIndex >= 0);
    end;
  finally
    DataSetList.Items.EndUpdate;
  end;
end;

procedure TJvSelectDataSetForm.DataSetListDblClick(Sender: TObject);
begin
  if DataSetList.ItemIndex >= 0 then
    ModalResult := mrOk;
end;

procedure TJvSelectDataSetForm.DataSetListKeyPress(Sender: TObject;
  var Key: Char);
begin
  if (Ord(Key) = VK_RETURN) and (DataSetList.ItemIndex >= 0) then
    ModalResult := mrOk;
end;

{$ENDIF DelphiPersonalEdition}

end.

