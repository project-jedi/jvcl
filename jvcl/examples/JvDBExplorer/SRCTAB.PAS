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

unit SrcTab;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, Grids, StdCtrls, Mask, JvToolEdit, DB,
  DBTables, DBGrids, JvValidateEdit,
  JvBDEMemTable, JvComponent, JvFormPlacement, 
  JvEdit, JvBDELists, JvDBControls, JvDBGrid, JvExDBGrids, JvExMask,
  JvExStdCtrls;

type
  TSrcTableDlg = class(TForm)
    Expanded: TBevel;
    FormStorage: TJvFormStorage ;
    TableFields: TJvTableItems ;
    MappingsTab: TJvBDEMemoryTable;
    MappingsTabSRC_NAME: TStringField;
    MappingsTabDST_NAME: TStringField;
    dsMappings: TDataSource;
    TopPanel: TPanel;
    Label1: TLabel;
    Label4: TLabel;
    RecordCountBox: TGroupBox;
    Label2: TLabel;
    FirstRecsBtn: TRadioButton;
    AllRecsBtn: TRadioButton;
    ModeCombo: TComboBox;
    SrcNameEdit: TJvFilenameEdit ;
    OkBtn: TButton;
    CancelBtn: TButton;
    MapBtn: TButton;
    BottomPanel: TPanel;
    Label3: TLabel;
    MapGrid: TJvDBGrid;
    RecordCntEdit: TJvValidateEdit ;
    procedure FormCreate(Sender: TObject);
    procedure MapBtnClick(Sender: TObject);
    procedure SrcNameEditChange(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure AllRecsBtnClick(Sender: TObject);
    procedure MappingsTabDST_NAMEGetText(Sender: TField; var Text: string;
      DisplayText: Boolean);
    procedure MappingsTabDST_NAMESetText(Sender: TField;
      const Text: string);
  private
    { Private declarations }
    FExpanded: Boolean;
    FMappingsHeight: Integer;
    FDstTable: TTable;
    FSrcName: string;
    procedure UpdateFormView;
    procedure UpdateMapGrid;
    procedure MapTabBeforeDeleteInsert(DataSet: TDataSet);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
  end;

function GetImportParams(const DstTable: TTable; var TabName: string;
  var RecordCount: Longint; Mappings: TStrings; var Mode: TBatchMode): Boolean;

implementation

uses JvJVCLUtils;

{$R *.DFM}

function GetImportParams(const DstTable: TTable; var TabName: string;
  var RecordCount: Longint; Mappings: TStrings; var Mode: TBatchMode): Boolean;
begin
  with TSrcTableDlg.Create(Application) do begin
    try
      Caption := Format(Caption, [DstTable.TableName]);
      FDstTable := DstTable;
      Result := ShowModal = mrOk;
      if Result then begin
        TabName := SrcNameEdit.Text;
        RecordCount := 0;
        if FirstRecsBtn.Checked then
          RecordCount := RecordCntEdit.AsInteger;
        if Mappings <> nil then begin
          Mappings.Clear;
          with MappingsTab do begin
            if Active then begin
              First;
              while not EOF do begin
                if (Trim(FieldByName('SRC_NAME').AsString) <> '') and
                  (Trim(FieldByName('DST_NAME').AsString) <> '') then
                  Mappings.Add(Format('%s=%s', [FieldByName('DST_NAME').Value,
                    FieldByName('SRC_NAME').Value]));
                Next;
              end;
            end;
          end;
        end;
        Mode := TBatchMode(ModeCombo.ItemIndex);
      end;
    finally
      Free;
    end;
  end;
end;

const
  SMappings = '&Mappings';

procedure TSrcTableDlg.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Application.MainForm <> nil then
    Params.WndParent := Application.MainForm.Handle;
end;

procedure TSrcTableDlg.MapTabBeforeDeleteInsert(DataSet: TDataSet);
begin
  SysUtils.Abort;
end;

procedure TSrcTableDlg.FormCreate(Sender: TObject);
begin
  ModeCombo.ItemIndex := 0;
  FMappingsHeight := ClientHeight;
  UpdateFormView;
end;

procedure TSrcTableDlg.UpdateFormView;
begin
  DisableAlign;
  try
    if FExpanded then begin
      ClientHeight := FMappingsHeight;
      MapBtn.Caption := '<< ' + SMappings;
    end
    else begin
      ClientHeight := BottomPanel.Top;
      MapBtn.Caption := SMappings + ' >>';;
    end;
    BottomPanel.Visible := FExpanded;
    MapGrid.Enabled := FExpanded;
  finally
    EnableAlign;
  end;
end;

procedure TSrcTableDlg.MapBtnClick(Sender: TObject);
begin
  if not FExpanded then UpdateMapGrid;
  FExpanded := not FExpanded;
  UpdateFormView;
end;

procedure TSrcTableDlg.SrcNameEditChange(Sender: TObject);
begin
  OkBtn.Enabled := SrcNameEdit.Text <> EmptyStr;
  MapBtn.Enabled := FExpanded or (SrcNameEdit.Text <> EmptyStr);
end;

procedure TSrcTableDlg.OkBtnClick(Sender: TObject);
begin
  if not FileExists(SrcNameEdit.FileName) then begin
    raise Exception.Create(Format('File %s does not exist',
      [SrcNameEdit.FileName]));
  end;
  ModalResult := mrOk;
end;

procedure TSrcTableDlg.AllRecsBtnClick(Sender: TObject);
begin
  RecordCntEdit.Enabled := FirstRecsBtn.Checked;
  if RecordCntEdit.Enabled then begin
    RecordCntEdit.Color := clWindow;
    RecordCntEdit.ParentFont := True;
    if SrcNameEdit.Text <> '' then ActiveControl := RecordCntEdit
    else ActiveControl := SrcNameEdit;
  end
  else begin
    RecordCntEdit.ParentColor := True;
    RecordCntEdit.Font.Color := RecordCntEdit.Color;
  end;
end;

procedure TSrcTableDlg.UpdateMapGrid;
begin
  if (FSrcName = SrcNameEdit.FileName) and MappingsTab.Active then
    Exit;
  FSrcName := SrcNameEdit.FileName;
  MappingsTab.DisableControls;
  StartWait;
  try
    MappingsTab.Close;
    TableFields.Close;
    TableFields.SessionName := FDstTable.SessionName;
    TableFields.DatabaseName := FDstTable.DatabaseName;
    TableFields.TableName := FDstTable.TableName;
    TableFields.Open;
    try
      MapGrid.Columns[1].PickList.Clear;
      while not TableFields.EOF do begin
        MapGrid.Columns[1].PickList.Add(
          TableFields.FieldByName('NAME').AsString);
        TableFields.Next;
      end;
    finally
      TableFields.Close;
    end;
    TableFields.DatabaseName := '';
    TableFields.TableName := SrcNameEdit.FileName;
    TableFields.Open;
    try
      with MappingsTab do begin
        BeforeDelete := nil;
        BeforeInsert := nil;
        Open;
      end;
      while not TableFields.Eof do begin
        MappingsTab.Append;
        MappingsTab.FieldByName('SRC_NAME').AsString :=
          TableFields.FieldByName('NAME').AsString;
        if MapGrid.Columns[1].PickList.IndexOf(
          MappingsTab.FieldByName('SRC_NAME').AsString) >= 0 then
          MappingsTab.FieldByName('DST_NAME').AsString :=
            MappingsTab.FieldByName('SRC_NAME').AsString
        else
          MappingsTab.FieldByName('DST_NAME').AsString := ' ';
        try
          MappingsTab.Post;
        except
          MappingsTab.Cancel;
          raise;
        end;
        TableFields.Next;
      end;
      with MappingsTab do begin
        BeforeDelete := MapTabBeforeDeleteInsert;
        BeforeInsert := MapTabBeforeDeleteInsert;
      end;
    finally
      TableFields.Close;
    end;
    MappingsTab.First;
  finally
    StopWait;
    MappingsTab.EnableControls;
  end;
end;

procedure TSrcTableDlg.MappingsTabDST_NAMEGetText(Sender: TField;
  var Text: string; DisplayText: Boolean);
begin
  Text := Trim(Sender.AsString);
end;

procedure TSrcTableDlg.MappingsTabDST_NAMESetText(Sender: TField;
  const Text: string);
begin
  if Text = '' then Sender.AsString := ' '
  else Sender.AsString := Text;
end;

end.
