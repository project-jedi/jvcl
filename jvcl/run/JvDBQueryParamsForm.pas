{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvQBndDlg.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBQueryParamsForm;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, DB,
  JvComponent;

type
  TJvQueryParamsDialog = class(TJvForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    ParamValue: TEdit;
    Label2: TLabel;
    NullValue: TCheckBox;
    OkBtn: TButton;
    CancelBtn: TButton;
    Label3: TLabel;
    TypeList: TComboBox;
    ParamList: TListBox;
    HelpBtn: TButton;
    procedure ParamListChange(Sender: TObject);
    procedure TypeListChange(Sender: TObject);
    procedure ParamValueExit(Sender: TObject);
    procedure NullValueClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    InitList: TParams;
    PressedOK: Boolean;
    InValueExit: Boolean;
    InParamChange: Boolean;
    procedure CheckValue;
    procedure Edit;
    procedure Unbind;
  end;

function EditQueryParams(DataSet: TDataSet; List: TParams;
  AHelpContext: THelpContext = 0): Boolean;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  DbConsts,
  JvTypes, JvResources;

{$IFDEF VCL}
{$R *.dfm}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$R *.xfm}
{$ENDIF VisualCLX}

var
  FieldTypes: array [TFieldType] of string;

procedure FillFieldTypes;
var
  ParamString: string;
  I: Integer;
  J: TFieldType;
begin
  for J := Low(TFieldType) to High(TFieldType) do
    FieldTypes[J] := '';
  ParamString := RsDataTypes;
  J := Low(TFieldType);
  I := 1;
  while I <= Length(ParamString) do
  begin
    FieldTypes[J] := ExtractFieldName(ParamString, I);
    Inc(J);
  end;
end;

function GetFieldType(const Value: string): TFieldType;
begin
  for Result := Low(TFieldType) to High(TFieldType) do
    if (FieldTypes[Result] <> '') and (FieldTypes[Result] = Value) then
      Exit;
  Result := ftUnknown;
end;

procedure ClearFieldTypes;
var
  I: TFieldType;
begin
  for I := Low(TFieldType) to High(TFieldType) do
    //DisposeStr(FieldTypes[I]);
    FieldTypes[I] := '';
end;

procedure DoneQBind;
begin
  ClearFieldTypes;
end;

function EditQueryParams(DataSet: TDataSet; List: TParams;
  AHelpContext: THelpContext): Boolean;
begin
  with TJvQueryParamsDialog.Create(Application) do
  try
    HelpContext := AHelpContext;
    if HelpContext = 0 then
    begin
      HelpBtn.Visible := False;
      OkBtn.Left := OkBtn.Left + HelpBtn.Width div 2;
      CancelBtn.Left := CancelBtn.Left + HelpBtn.Width div 2;
    end;
    if csDesigning in DataSet.ComponentState then
      Caption := Format(RsParamEditor,
        {$IFDEF BCB}
        [DataSet.Owner.Name, '->', DataSet.Name]);
        {$ELSE}
        [DataSet.Owner.Name, '.', DataSet.Name]);
        {$ENDIF BCB}
    InitList := List;
    Edit;
    Result := PressedOK;
  finally
    Free;
  end;
end;

procedure TJvQueryParamsDialog.Edit;
var
  I: Integer;
  J: TFieldType;
begin
  for J := Low(TFieldType) to High(TFieldType) do
    if (FieldTypes[J] <> '') and (FieldTypes[J] <> '') then
      TypeList.Items.Add(FieldTypes[J]);
  if InitList.Count = 0 then
  begin
    ParamValue.Enabled := False;
    NullValue.Enabled := False;
    TypeList.Enabled := False;
    ParamList.Enabled := False;
  end
  else
  begin
    for I := 0 to InitList.Count - 1 do
      if ParamList.Items.IndexOf(InitList[I].Name) = -1 then
        ParamList.Items.Add(InitList[I].Name);
    ParamList.ItemIndex := 0;
    ParamListChange(Self);
    ActiveControl := OkBtn;
  end;
  PressedOK := ShowModal = mrOk;
end;

procedure TJvQueryParamsDialog.ParamListChange(Sender: TObject);
begin
  InParamChange := True;
  try
    with InitList.ParamByName(ParamList.Items[ParamList.ItemIndex]) do
    begin
      if (FieldTypes[DataType] <> '') and (FieldTypes[DataType] <> '') then
      begin
        with TypeList do
          ItemIndex := Items.IndexOf(FieldTypes[DataType]);
        if Bound then
          ParamValue.Text := AsString
        else
          ParamValue.Text := '';
      end
      else
      begin
        TypeList.ItemIndex := -1;
        ParamValue.Text := '';
      end;
      NullValue.Checked := IsNull;
    end;
  finally
    InParamChange := False;
  end;
end;

procedure TJvQueryParamsDialog.TypeListChange(Sender: TObject);
begin
  with InitList.ParamByName(ParamList.Items[ParamList.ItemIndex]) do
  begin
    DataType := GetFieldType(TypeList.Text);
    ParamValue.Text := '';
    NullValue.Checked := IsNull;
  end;
end;

procedure TJvQueryParamsDialog.ParamValueExit(Sender: TObject);
begin
  if InValueExit or (ActiveControl = CancelBtn) then
    Exit;
  InValueExit := True;
  try
    if ParamValue.Text <> '' then
      NullValue.Checked := False;
    if (TypeList.Text = '') and TypeList.CanFocus then
    begin
      TypeList.SetFocus;
      raise EJVCLException.CreateRes(@RsEInvalidParamFieldType);
    end;
    if ParamValue.Text = '' then
      with InitList.ParamByName(ParamList.Items[ParamList.ItemIndex]) do
      begin
        if NullValue.Checked then
          Clear
        else
          Unbind;
      end
    else
      CheckValue;
  finally
    InValueExit := False;
  end;
end;

procedure TJvQueryParamsDialog.CheckValue;
begin
  try
    with InitList.ParamByName(ParamList.Items[ParamList.ItemIndex]) do
    begin
      if (DataType in [ftDate, ftTime, ftDateTime]) and
        SameText(ParamValue.Text, 'Now') then
      begin
        case DataType of
          ftDate:
            Text := DateToStr(SysUtils.Date);
          ftTime:
            Text := TimeToStr(SysUtils.Time);
          ftDateTime:
            Text := DateTimeToStr(SysUtils.Now);
        end;
      end
      else
        Text := ParamValue.Text;
    end;
  except
    with ParamValue do
    begin
      if CanFocus then
        SetFocus;
      SelectAll;
    end;
    raise;
  end;
end;

procedure TJvQueryParamsDialog.Unbind;
begin
  with InitList.ParamByName(ParamList.Items[ParamList.ItemIndex]) do
  begin
    AsInteger := 1;
    DataType := GetFieldType(TypeList.Text);
    Bound := False;
  end;
end;

procedure TJvQueryParamsDialog.NullValueClick(Sender: TObject);
begin
  if InParamChange then
    Exit;
  if NullValue.Checked then
    with InitList.ParamByName(ParamList.Items[ParamList.ItemIndex]) do
    begin
      Clear;
      ParamValue.Text := '';
    end
  else
    Unbind;
end;

procedure TJvQueryParamsDialog.OkBtnClick(Sender: TObject);
begin
  if not TypeList.Enabled then
    Exit;
  try
    ParamValueExit(Sender);
  except
    ModalResult := mrNone;
    raise;
  end;
end;

procedure TJvQueryParamsDialog.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
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

  FillFieldTypes;

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  DoneQBind;

end.

