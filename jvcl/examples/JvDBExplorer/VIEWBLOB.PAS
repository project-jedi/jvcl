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

unit ViewBlob;

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons, Dialogs,
  StdCtrls, ExtCtrls, DBCtrls, DB, DBTables, Menus, HexDump,
  JvComponent, JvFormPlacement;

type
  TBlobViewDlg = class(TForm)
    CancelBtn: TBitBtn;
    Bevel: TBevel;
    DataSource: TDataSource;
    DBNavigator: TDBNavigator;
    HexPanel: TPanel;
    SizeLabel: TLabel;
    ClearBtn: TBitBtn;
    AddressBox: TCheckBox;
    FormStorage: TJvFormStorage ;
    procedure DataSourceDataChange(Sender: TObject; Field: TField);
    procedure FormDestroy(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure AddressBoxClick(Sender: TObject);
  private
    FField: TField;
    FData: Pointer;
    FHexDump: THexDump;
  protected
{$IFNDEF WIN32}
    procedure CreateParams(var Params: TCreateParams); override;
{$ENDIF}
  end;

procedure BlobView(DataSet: TDataSet; const FieldName: string);

implementation

{$R *.DFM}

uses
  Messages, SysUtils, DBConsts, Math,
  JvJCLUtils;

const
  SBlobSize = 'Blob size: %s byte(s)';
  SClearMsg = 'Field %s will be emptied. All data will be lost. Continue?';

procedure BlobView(DataSet: TDataSet; const FieldName: string);
begin
  with TBlobViewDlg.Create(Application) do
  try
    FField := DataSet.FieldByName(FieldName);
    if not (FField is TBlobField) then
      raise EDatabaseError.CreateFmt(SFieldTypeMismatch, [FieldName]);
    Caption := Format('Field: %s', [FieldName]);
    FHexDump := CreateHexDump(HexPanel);
    FHexDump.RelativeAddress := True;
    AddressBoxClick(nil);
    DataSource.DataSet := DataSet;
    ShowModal;
  finally
    Free;
  end;
end;

{$IFNDEF WIN32}
procedure TBlobViewDlg.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Application.MainForm <> nil then
    Params.WndParent := Application.MainForm.Handle;
end;
{$ENDIF}

procedure TBlobViewDlg.DataSourceDataChange(Sender: TObject;
  Field: TField);
var
  BlobStream: TBlobStream;
  DataSize: Longint;
begin
  if (Field = nil) or (Field = FField) then begin
    FreeMemo(FData);
    FData := nil;
    BlobStream := TBlobStream.Create(FField as TBlobField, bmRead);
    try
      DataSize := BlobStream.Size;
      SizeLabel.Caption := Format(SBlobSize, [FormatFloat(',0', BlobStream.Size)]);
      if DataSize > 0 then begin
        FData := AllocMemo(DataSize);
        BlobStream.Read(FData^, DataSize);
      end;
      FHexDump.Address := FData;
      FHexDump.DataSize := DataSize;
      ClearBtn.Enabled := DataSize > 0;
      ActiveControl := FHexDump;
    finally
      BlobStream.Free;
    end;
  end;
end;

procedure TBlobViewDlg.FormDestroy(Sender: TObject);
begin
  DataSource.OnDataChange := nil;
  FreeMemo(FData);
  FData := nil;
end;

procedure TBlobViewDlg.ClearBtnClick(Sender: TObject);
var
  WasEdit: Boolean;
begin
  if MessageDlg(Format(SClearMsg, [FField.FieldName]), mtWarning,
    [mbYes, mbNo], 0) = mrYes then
  begin
    WasEdit := DataSource.DataSet.State in [dsEdit, dsInsert];
    if not WasEdit then DataSource.DataSet.Edit;
    try
      with TBlobStream.Create(FField as TBlobField, bmWrite) do
      try
        Truncate;
      finally
        Free;
      end;
      if not WasEdit then DataSource.DataSet.Post;
    except
      if not WasEdit then DataSource.DataSet.Cancel;
      raise;
    end;
  end;
end;

procedure TBlobViewDlg.AddressBoxClick(Sender: TObject);
begin
  if FHexDump <> nil then
    FHexDump.ShowAddress := AddressBox.Checked;
end;

end.
