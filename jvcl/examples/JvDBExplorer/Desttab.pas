{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

{*******************************************************}
{                                                       }
{     Delphi VCL Extensions (RX) demo program           }
{                                                       }
{     Copyright (c) 1996 AO ROSNO                       }
{     Copyright (c) 1997 Master-Bank                    }
{                                                       }
{*******************************************************}

unit DestTab;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls,
  StdCtrls, ExtCtrls, FileCtrl, Mask, JvToolEdit, DB, DBTables,
  JvValidateEdit, JvComponent, JvFormPlacement, JvEdit, JvExStdCtrls,
  JvExMask;

type
  TDestTableDlg = class(TForm)
    OkBtn: TButton;
    CancelBtn: TButton;
    FormStorage: TJvFormStorage ;
    TypeBtn: TRadioGroup;
    TabnameEdit: TJvFilenameEdit ;
    Label1: TLabel;
    RecordCountBox: TGroupBox;
    AllRecsBtn: TRadioButton;
    FirstRecsBtn: TRadioButton;
    Label2: TLabel;
    RecordCntEdit: TJvValidateEdit;
    procedure TabnameEditAfterDialog(Sender: TObject; var Name: string;
      var Action: Boolean);
    procedure TabnameEditChange(Sender: TObject);
    procedure TypeBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure RecordCountBtnClick(Sender: TObject);
  private
    { Private declarations }
    function GetTableType: TTableType;
    procedure SetTableType(Value: TTableType);
    function GetTableName: string;
    procedure SetTableName(const Value: string);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
    property TableType: TTableType read GetTableType write SetTableType;
    property TableName: string read GetTableName write SetTableName;
  end;

function GetDestTable(var TabName: string; var TabType: TTableType;
  var RecordCount: Longint): Boolean;

implementation

{$B-}

uses
  Consts, Dialogs, JvJVCLUtils, JvJCLUtils, Main;

{$R *.DFM}

function GetDestTable(var TabName: string; var TabType: TTableType;
  var RecordCount: Longint): Boolean;
begin
  with TDestTableDlg.Create(Application) do
  try
    FormStorage.RestoreFormPlacement;
    if TabType <> ttDefault then TableType := TabType;
    if TabName <> '' then TableName := TabName;
    if RecordCount <> 0 then begin
      RecordCntEdit.AsInteger := RecordCount;
      FirstRecsBtn.Checked := True;
    end;
    Result := ShowModal = mrOk;
    if Result then begin
      TabName := TableName;
      TabType := TableType;
      RecordCount := 0;
      if FirstRecsBtn.Checked then
        RecordCount := RecordCntEdit.AsInteger;
    end;
  finally
    Free;
  end;
end;

function DefExtension(TableType: TTableType): string;
begin
  case TableType of
    ttParadox: Result := '.DB';
    ttDBase: Result := '.DBF';
    ttASCII: Result := '.TXT';
    else Result := '';
  end;
end;

{ TDestTableDlg }

procedure TDestTableDlg.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Application.MainForm <> nil then
    Params.WndParent := Application.MainForm.Handle;
end;

function TDestTableDlg.GetTableType: TTableType;
begin
  Result := TTableType(TypeBtn.ItemIndex + 1);
end;

procedure TDestTableDlg.SetTableType(Value: TTableType);
begin
  TypeBtn.ItemIndex := Integer(Value) - 1;
end;

function TDestTableDlg.GetTableName: string;
begin
  Result := TabnameEdit.FileName;
end;

procedure TDestTableDlg.SetTableName(const Value: string);
begin
  if Value <> TabnameEdit.FileName then begin
    TabnameEdit.FileName := NormalDir(TabnameEdit.InitialDir) +
      ChangeFileExt(Value, DefExtension(TableType));
  end;
end;

procedure TDestTableDlg.TabnameEditAfterDialog(Sender: TObject;
  var Name: string; var Action: Boolean);
begin
  if (CompareText(ExtractFileExt(Name), '.DB') = 0) then
    TypeBtn.ItemIndex := 0
  else if (CompareText(ExtractFileExt(Name), '.DBF') = 0) then
    TypeBtn.ItemIndex := 1
  else if (CompareText(ExtractFileExt(Name), '.TXT') = 0) then
    TypeBtn.ItemIndex := 2;
end;

procedure TDestTableDlg.TabnameEditChange(Sender: TObject);
begin
  OkBtn.Enabled := TabnameEdit.FileName <> '';
end;

procedure TDestTableDlg.TypeBtnClick(Sender: TObject);
begin
  if TabnameEdit.FileName <> '' then begin
    TabnameEdit.FileName := ChangeFileExt(TabnameEdit.FileName,
      DefExtension(TableType));
  end;
  TabnameEdit.FilterIndex := Integer(TableType);
end;

procedure TDestTableDlg.OkBtnClick(Sender: TObject);
begin
  if (not FileExists(TabnameEdit.FileName)) or
    (MessageDlg(Format('File %s already exists. Do you want to replace it?',
    [TabnameEdit.FileName]), mtWarning, [mbYes, mbNo], 0) = mrYes)
  then
    ModalResult := mrOk;
end;

procedure TDestTableDlg.RecordCountBtnClick(Sender: TObject);
begin
  RecordCntEdit.Enabled := FirstRecsBtn.Checked;
  if RecordCntEdit.Enabled then begin
    RecordCntEdit.Color := clWindow;
    RecordCntEdit.ParentFont := True;
    if TabnameEdit.Text <> '' then ActiveControl := RecordCntEdit
    else ActiveControl := TabnameEdit;
  end
  else begin
    RecordCntEdit.ParentColor := True;
    RecordCntEdit.Font.Color := RecordCntEdit.Color;
  end;
end;

end.
