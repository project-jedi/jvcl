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

unit DevReader;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, JvHidControllerClass;

type
  TReport = packed record
    ReportID: Byte;
    Bytes:    array [0..63] of Byte;
  end;

  TMainForm = class(TForm)
    DevListBox: TListBox;
    HistoryListBox: TListBox;
    ReadBtn: TSpeedButton;
    WriteBtn: TSpeedButton;
    SaveBtn: TSpeedButton;
    SaveDialog: TSaveDialog;
    ReportID: TEdit;
    Edit1: TEdit;
    Label1: TLabel;
    HidCtl: TJvHidDeviceController;
    InfoBtn: TSpeedButton;
    procedure HidCtlDeviceChange(Sender: TObject);
    function HidCtlEnumerate(HidDev: TJvHidDevice;
      const Idx: Integer): Boolean;
    procedure ReadBtnClick(Sender: TObject);
    procedure DevListBoxClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure WriteBtnClick(Sender: TObject);
    procedure InfoBtnClick(Sender: TObject);
    procedure HidCtlDeviceDataError(HidDev: TJvHidDevice;
      Error: Cardinal);
  public
    Edits: array [0..63] of TEdit;
    CurrentDevice: TJvHidDevice;
  published
    procedure ShowRead(HidDev: TJvHidDevice; ReportID: Byte;
      const Data: Pointer; Size: Word);
  end;

var
  MainForm: TMainForm;

implementation

uses
  Info;

{$R *.DFM}

procedure TMainForm.HidCtlDeviceChange(Sender: TObject);
var
  Dev: TJvHidDevice;
  I: Integer;
begin
  ReadBtn.Down := False;
  ReadBtnClick(Self);
  if Assigned(DevListBox) then
  begin
    for I := 0 to DevListBox.Items.Count - 1 do
    begin
      Dev := TJvHidDevice(DevListBox.Items.Objects[I]);
      Dev.Free;
    end;
    DevListBox.Clear;
    HistoryListBox.Clear;
    HidCtl.Enumerate;
    if DevListBox.Items.Count > 0 then
    begin
      DevListBox.ItemIndex := 0;
      DevListBoxClick(Self);
    end;
  end;
end;

function TMainForm.HidCtlEnumerate(HidDev: TJvHidDevice;
  const Idx: Integer): Boolean;
var
  N: Integer;
  Dev: TJvHidDevice;
begin
  if Assigned(DevListBox) then
  begin
    if HidDev.ProductName <> '' then
      N := DevListBox.Items.Add(HidDev.ProductName)
    else
      N := DevListBox.Items.Add(Format('Device VID=%.4x PID=%.4x',
        [HidDev.Attributes.VendorID, HidDev.Attributes.ProductID]));
    HidCtl.CheckOutByIndex(Dev, Idx);
    DevListBox.Items.Objects[N] := Dev;
  end;
  Result := True;
end;

procedure TMainForm.DevListBoxClick(Sender: TObject);
var
  I: Integer;
  Dev: TJvHidDevice;
begin
  ReadBtn.Down := False;
  ReadBtnClick(Self);
  HistoryListBox.Clear;
  if Assigned(Edits[0]) and
    (DevListBox.Items.Count > 0) and (DevListBox.ItemIndex >= 0) then
  begin
    Dev := TJvHidDevice(DevListBox.Items.Objects[DevListBox.ItemIndex]);
    for I := Low(Edits) to High(Edits) do
      Edits[I].Visible := False;
    for I := 0 to Dev.Caps.OutputReportByteLength - 2 do
      Edits[I].Visible := True;
    WriteBtn.Enabled := Dev.Caps.OutputReportByteLength <> 0;
  end;
end;

procedure TMainForm.ShowRead(HidDev: TJvHidDevice; ReportID: Byte;
  const Data: Pointer; Size: Word);
var
  I: Integer;
  Str: string;
begin
  Str := Format('R %.2x  ', [ReportID]);
  for I := 0 to Size - 1 do
    Str := Str + Format('%.2x ', [Cardinal(PChar(Data)[I])]);
  HistoryListBox.ItemIndex := HistoryListBox.Items.Add(Str);
end;

procedure TMainForm.HidCtlDeviceDataError(HidDev: TJvHidDevice;
  Error: Cardinal);
begin
  HistoryListBox.ItemIndex := HistoryListBox.Items.Add('READ ERROR: ' + SysErrorMessage(Error));
end;

procedure TMainForm.InfoBtnClick(Sender: TObject);
begin
  if (DevListBox.Items.Count > 0) and (DevListBox.ItemIndex >= 0) then
    with TInfoForm.Create(Self) do
    begin
      Dev := TJvHidDevice(DevListBox.Items.Objects[DevListBox.ItemIndex]);
      ShowModal;
      Free;
    end;
end;

procedure TMainForm.ReadBtnClick(Sender: TObject);
begin
  CurrentDevice := nil;
  if (DevListBox.Items.Count > 0) and (DevListBox.ItemIndex >= 0) then
  begin
    CurrentDevice := TJvHidDevice(DevListBox.Items.Objects[DevListBox.ItemIndex]);
    if not CurrentDevice.HasReadWriteAccess then
      ReadBtn.Down := False;
  end;
end;

procedure TMainForm.WriteBtnClick(Sender: TObject);
var
  I: Integer;
  Buf: array [0..64] of Byte;
  Written: Cardinal;
  ToWrite: Cardinal;
  Str: string;
begin
  if Assigned(CurrentDevice) then
  begin
    Buf[0] := StrToIntDef('$' + ReportID.Text, 0);
    ReportID.Text := Format('%.2x', [Buf[0]]);
    ToWrite := CurrentDevice.Caps.OutputReportByteLength;
    for I := 1 to ToWrite-1 do
    begin
      Buf[I] := StrToIntDef('$' + Edits[I-1].Text, 0);
      Edits[I-1].Text := Format('%.2x', [Buf[I]]);
    end;
    CurrentDevice.WriteFile(Buf, ToWrite, Written);
    Str := Format('W %.2x  ', [Buf[0]]);
    for I := 1 to Written-1 do
      Str := Str + Format('%.2x ', [Buf[I]]);
    HistoryListBox.ItemIndex := HistoryListBox.Items.Add(Str);
  end;
end;

procedure TMainForm.SaveBtnClick(Sender: TObject);
begin
  ForceCurrentDirectory := True;
  if SaveDialog.Execute then
    HistoryListBox.Items.SaveToFile(SaveDialog.FileName);
end;

procedure TMainForm.FormActivate(Sender: TObject);
var
  I, J: Integer;
begin
  if Assigned(Edits[0]) then
    Exit;
  Edits[0] := Edit1;
  for I := 1 to High(Edits) do
    Edits[I] := TEdit.Create(Self);
  for J := 0 to 3 do
    for I := 0 to 15 do
      with Edits[J*16 + I] do
      begin
        Visible  := False;
        Left     := Edit1.Left + I*(Edit1.Width+2);
        Top      := Edit1.Top  + J*(Edit1.Height+2);
        Width    := Edit1.Width;
        Anchors  := Edit1.Anchors;
        if not Assigned(Parent) then
          Parent := Edit1.Parent;
        TabOrder := Edit1.TabOrder + J*16 + I;
      end;
  DevListBoxClick(Self);
end;

end.
