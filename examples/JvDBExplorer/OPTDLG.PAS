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
{     Copyright (c) 1997, 1998 Master-Bank              }
{                                                       }
{*******************************************************}

unit OptDlg;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, TabNotBk, DB, JvBDELists,
  Mask, ComCtrls, DBTables, JvToolEdit, JvValidateEdit,
  JvComponent, JvFormPlacement, JvDBLookup, JvEdit, JvExControls,
  JvExStdCtrls;

type
  TOptionsDialog = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    ApplyBtn: TButton;
    Notebook: TTabbedNotebook;
    GroupBox1: TGroupBox;
    KeepConnectBtn: TCheckBox;
    ShowSystemBtn: TCheckBox;
    AutoActivateBtn: TCheckBox;
    GroupBox2: TGroupBox;
    FloatFormatEdit: TEdit;
    DateFormatEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    TimeFormatEdit: TEdit;
    Label5: TLabel;
    DateTimeFormatEdit: TEdit;
    Image2: TImage;
    Image3: TImage;
    GroupBox3: TGroupBox;
    Image4: TImage;
    Label7: TLabel;
    Label8: TLabel;
    MaxHistoryEdit: TJvValidateEdit ;
    GroupBox5: TGroupBox;
    LiveQueryBtn: TCheckBox;
    AbortQueryBtn: TCheckBox;
    Image5: TImage;
    ShowTimeBtn: TCheckBox;
    TraceCategories: TGroupBox;
    CBPrepared: TCheckBox;
    CBExecuted: TCheckBox;
    CBVendorErr: TCheckBox;
    CBStatement: TCheckBox;
    CBConnect: TCheckBox;
    CBTransaction: TCheckBox;
    CBMisc: TCheckBox;
    CBBlob: TCheckBox;
    CBVendor: TCheckBox;
    Label9: TLabel;
    BufferSizeEdit: TJvValidateEdit ;
    QueryInThreadBtn: TCheckBox;
    SQLCountBtn: TCheckBox;
    UpDown: TUpDown;
    SQLMemo: TMemo;
    FontDialog: TFontDialog;
    SQLFontBtn: TButton;
    procedure ApplyBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OptionsChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SQLFontBtnClick(Sender: TObject);
    procedure LangDrivListAfterOpen(DataSet: TDataSet);
    procedure UpDownChanging(Sender: TObject; var AllowChange: Boolean);
  private
    { Private declarations }
    LangDrivList:TJvLangDrivList; //!!!
    procedure ApplyOptions;
    procedure SetOptions;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
  end;

implementation

uses Consts, IniFiles, JvJVCLUtils, JvDBUtils, Options, Main, DBCbRest, JvBdeUtils;

{$R *.DFM}

{ TOptionsDialog }

procedure TOptionsDialog.ApplyOptions;
begin
  AutoActivate := AutoActivateBtn.Checked;
  SystemTables := ShowSystemBtn.Checked;

//  ASCIIDelimited := DelimitedBtn.Checked;
//  ShowRestWarnings := ShowWarnBox.Checked;
  SetKeepConnections(KeepConnectBtn.Checked);
//  ASCIICharSet := CharsetEdit.Value;
  if ASCIICharSet = '' then ASCIICharSet := 'ascii';
  defFloatFormat := FloatFormatEdit.Text;
  defDateFormat := DateFormatEdit.Text;
  defTimeFormat := TimeFormatEdit.Text;
  defDateTimeFormat := DateTimeFormatEdit.Text;
  SQLHistoryCapacity := MaxHistoryEdit.AsInteger;
  LiveQueries := LiveQueryBtn.Checked;
  ShowExecTime := ShowTimeBtn.Checked;
  SQLCalcCount := SQLCountBtn.Checked;
  SQLTraceBuffer := Trunc(BufferSizeEdit.Value);
  SQLTraceFlags := [];
  if CBPrepared.Checked then Include(SQLTraceFlags, tfQPrepare);
  if CBExecuted.Checked then Include(SQLTraceFlags, tfQExecute);
  if CBVendorErr.Checked then Include(SQLTraceFlags, tfError);
  if CBStatement.Checked then Include(SQLTraceFlags, tfStmt);
  if CBConnect.Checked then Include(SQLTraceFlags, tfConnect);
  if CBTransaction.Checked then Include(SQLTraceFlags, tfTransact);
  if CBMisc.Checked then Include(SQLTraceFlags, tfMisc);
  if CBBlob.Checked then Include(SQLTraceFlags, tfBlob);
  if CBVendor.Checked then Include(SQLTraceFlags, tfVendor);
  if (Application.MainForm <> nil) then
    TDBExplorerMainForm(Application.MainForm).SQLFontContainer.Font := SQLMemo.Font;
  EnableQueryAbort := AbortQueryBtn.Checked;
  QueryInThreads := QueryInThreadBtn.Checked;
end;

procedure TOptionsDialog.SetOptions;
begin
  AutoActivateBtn.Checked := AutoActivate;
  ShowSystemBtn.Checked := SystemTables;
//  DelimitedBtn.Checked := ASCIIDelimited;
//  FixedBtn.Checked := not ASCIIDelimited;
//  ShowWarnBox.Checked := ShowRestWarnings;
  KeepConnectBtn.Checked := Session.KeepConnections;
  LiveQueryBtn.Checked := LiveQueries;
  ShowTimeBtn.Checked := ShowExecTime;
  SQLCountBtn.Checked := SQLCalcCount;
  AbortQueryBtn.Checked := EnableQueryAbort;
  QueryInThreadBtn.Checked := QueryInThreads;
//  CharsetEdit.Value := ASCIICharSet;
  FloatFormatEdit.Text := defFloatFormat;
  DateFormatEdit.Text := defDateFormat;
  TimeFormatEdit.Text := defTimeFormat;
  DateTimeFormatEdit.Text := defDateTimeFormat;
  MaxHistoryEdit.AsInteger := SQLHistoryCapacity;
  UpDown.Position := MaxHistoryEdit.AsInteger;
  BufferSizeEdit.Value := SQLTraceBuffer;
  CBPrepared.Checked := tfQPrepare in SQLTraceFlags;
  CBExecuted.Checked := tfQExecute in SQLTraceFlags;
  CBVendorErr.Checked := tfError in SQLTraceFlags;
  CBStatement.Checked := tfStmt in SQLTraceFlags;
  CBConnect.Checked := tfConnect in SQLTraceFlags;
  CBTransaction.Checked := tfTransact in SQLTraceFlags;
  CBMisc.Checked := tfMisc in SQLTraceFlags;
  CBBlob.Checked := tfBlob in SQLTraceFlags;
  CBVendor.Checked := tfVendor in SQLTraceFlags;
  if (Application.MainForm <> nil) then
    SQLMemo.Font := TDBExplorerMainForm(Application.MainForm).SQLFontContainer.Font;
end;

procedure TOptionsDialog.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Application.MainForm <> nil then
    Params.WndParent := Application.MainForm.Handle;
end;

{ TODO : Fix save and restore with new TJvAppStorage implementation }
procedure TOptionsDialog.ApplyBtnClick(Sender: TObject);
//!!! var
//!!!  Ini: TIniFile;
begin
  if ApplyBtn.Enabled then begin
    ApplyOptions;
    if (Application.MainForm <> nil) then begin
      TDBExplorerMainForm(Application.MainForm).ApplyOptions;
{!!!    Ini := TIniFile.Create(
        TDBExplorerMainForm(Application.MainForm).FormPlacement.AppStorage.Path);
      try
        SaveOptions(Ini);
      finally
        Ini.Free;
      end;
      }
    end;
    ApplyBtn.Enabled := False;
  end;
end;

procedure TOptionsDialog.FormShow(Sender: TObject);
begin
  SetOptions;
  ApplyBtn.Enabled := False;
end;

procedure TOptionsDialog.OptionsChanged(Sender: TObject);
begin
  ApplyBtn.Enabled := True;
end;

procedure TOptionsDialog.FormCreate(Sender: TObject);
begin
  LangDrivList := TJvLangDrivList.Create(self);
  StartWait;
  try
    LangDrivList.AfterOpen := LangDrivListAfterOpen;
    LangDrivList.Open;
    Notebook.PageIndex := 0;
    BufferSizeEdit.MinValue := MinSQLTraceBuffer;
  finally
    StopWait;
  end;
end;

procedure TOptionsDialog.SQLFontBtnClick(Sender: TObject);
begin
  with FontDialog do begin
    Font := SQLMemo.Font;
    if Execute then begin
      SQLMemo.Font := Font;
      OptionsChanged(Sender);
    end;
  end;
end;

procedure TOptionsDialog.LangDrivListAfterOpen(DataSet: TDataSet);
begin
  LangDrivList.FieldByName('DESCRIPTION').DisplayWidth := 22;
end;

procedure TOptionsDialog.UpDownChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  if MaxHistoryEdit.CanFocus then begin
    ActiveControl := MaxHistoryEdit;
    OptionsChanged(Sender);
  end
  else AllowChange := False;
end;
initialization
  RegisterClass(TOptionsDialog);
  
end.
