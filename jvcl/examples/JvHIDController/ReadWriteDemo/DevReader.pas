unit DevReader;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, JvHidControllerClass, JvComponent;

type
  TReport = packed record
    ReportID: Byte;
    Bytes:    array [0..63] of Byte;
  end;

  TDevThread = class(TThread)
  public
    Dev:  TJvHidDevice;
    NumBytesRead: Cardinal;
    Data: TReport;
    procedure Execute; override;
    procedure HandleData;
  end;

  TForm1 = class(TForm)
    DevListBox: TListBox;
    HistoryListBox: TListBox;
    Read: TSpeedButton;
    Write: TSpeedButton;
    Save: TSpeedButton;
    SaveDialog1: TSaveDialog;
    ReportID: TEdit;
    Edit1: TEdit;
    Label1: TLabel;
    HidCtl: TJvHidDeviceController;
    Info: TSpeedButton;
    procedure HidCtlDeviceChange(Sender: TObject);
    function HidCtlEnumerate(const HidDev: TJvHidDevice;
      const Idx: Integer): Boolean;
    procedure ReadClick(Sender: TObject);
    procedure DevListBoxClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure WriteClick(Sender: TObject);
    procedure InfoClick(Sender: TObject);
  public
    DevList: TList;
    Edits: array [0..63] of TEdit;
    TheDev: TJvHidDevice;
    DevThread: TDevThread;
    procedure ShowRead;
  end;

var
  Form1: TForm1;

implementation

uses
  Info;

{$R *.DFM}

procedure TForm1.HidCtlDeviceChange(Sender: TObject);
var
  Dev: TJvHidDevice;
  I: Integer;
begin
  Read.Down := False;
  if Assigned(DevListBox) then
  begin
    if DevList <> nil then
    begin
      for I := 0 to DevList.Count - 1 do
      begin
        Dev := DevList.Items[I];
        Dev.Free;
      end;
      DevList.Clear;
    end
    else
      DevList := TList.Create;
    HistoryListBox.Clear;
    DevListBox.Clear;
    HidCtl.Enumerate;
    if DevListBox.Items.Count > 0 then
    begin
      DevListBox.ItemIndex := 0;
      DevListBoxClick(Self);
    end;
  end;
end;

function TForm1.HidCtlEnumerate(const HidDev: TJvHidDevice;
  const Idx: Integer): Boolean;
var
  Dev: TJvHidDevice;
begin
  if Assigned(DevListBox) then
  begin
    if HidDev.ProductName <> '' then
      DevListBox.Items.Add(HidDev.ProductName)
    else
      DevListBox.Items.Add(Format('Device VID=%x PID=%x',
        [HidDev.Attributes.VendorID, HidDev.Attributes.ProductID]));
    HidCtl.CheckOutByIndex(Dev, Idx);
    DevList.Add(Dev);
  end;
  Result := True;
end;

procedure TForm1.DevListBoxClick(Sender: TObject);
var
  I: Integer;
begin
  Read.Down := False;
  HistoryListBox.Clear;
  if Assigned(Edits[0]) and
    (DevListBox.Items.Count > 0) and (DevListBox.ItemIndex >= 0) then
  begin
    TheDev := DevList[DevListBox.ItemIndex];
    for I := 0 to TheDev.Caps.OutputReportByteLength - 1 do
      Edits[I].Visible := True;
    for I := TheDev.Caps.OutputReportByteLength-1 to 63 do
      Edits[I].Visible := False;
    Write.Enabled := TheDev.Caps.OutputReportByteLength <> 0;
  end;
end;

procedure TForm1.ShowRead;
var
  I: Integer;
  Str: string;
begin
  Str := Format('R %.2x  ', [DevThread.Data.ReportID]);
  for I := 0 to DevThread.NumBytesRead - 2 do
    Str := Str + Format('%.2x ', [DevThread.Data.Bytes[I]]);
  HistoryListBox.ItemIndex := HistoryListBox.Items.Add(Str);
end;

procedure TDevThread.HandleData;
begin
  Synchronize(Form1.ShowRead);
end;

procedure TDevThread.Execute;
var
  SleepRet: DWORD;

  procedure Dummy(ErrorCode: DWORD; Count: DWORD; Ovl: POverlapped); stdcall;
  begin
  end;

begin
  SleepRet := WAIT_IO_COMPLETION;
  while not Terminated do
  begin
    // read data
    SleepRet := WAIT_IO_COMPLETION;
    if not Dev.ReadFileEx(Data, Dev.Caps.InputReportByteLength, @Dummy) then
      Break;
    // wait for read to complete
    repeat
      SleepRet := SleepEx(200, True);
    until Terminated or (SleepRet = WAIT_IO_COMPLETION);
    // show data read
    if not Terminated then
    begin
      NumBytesRead := Dev.HidOverlappedReadResult;
      HandleData;
    end;
  end;
  // cancel ReadFileEx call or the callback will
  // crash your program
  if SleepRet <> WAIT_IO_COMPLETION then
    Dev.CancelIO(omhRead);
end;

procedure TForm1.InfoClick(Sender: TObject);
begin
  if (DevListBox.Items.Count > 0) and (DevListBox.ItemIndex >= 0) then
    with TInfoForm.Create(Self) do
    begin
      ShowModal;
      Free;
    end;
end;

procedure TForm1.ReadClick(Sender: TObject);
begin
  if (DevListBox.Items.Count > 0) and (DevListBox.ItemIndex >= 0) then
  begin
    TheDev := DevList[DevListBox.ItemIndex];
    if Read.Down then
    begin
      DevThread := TDevThread.Create(True);
      DevThread.FreeOnTerminate := False;
      DevThread.Dev := TheDev;
      DevThread.Resume;
    end
    else
    begin
      DevThread.Terminate;
      DevThread.WaitFor;
      FreeAndNil(DevThread);
    end;
  end;
end;

procedure TForm1.WriteClick(Sender: TObject);
var
  I: Integer;
  Buf: array [0..64] of Byte;
  Written: Cardinal;
  ToWrite: Cardinal;
  Str: string;
begin
  if (DevListBox.Items.Count > 0) and (DevListBox.ItemIndex >= 0) then
  begin
    TheDev := DevList[DevListBox.ItemIndex];
    Buf[0] := StrToIntDef('$' + ReportID.Text, 0);
    ReportID.Text := Format('%.2x', [Buf[0]]);
    ToWrite := TheDev.Caps.OutputReportByteLength;
    for I := 1 to ToWrite-1 do
    begin
      Buf[I] := StrToIntDef('$' + Edits[I-1].Text, 0);
      Edits[I-1].Text := Format('%.2x', [Buf[I]]);
    end;
    TheDev.WriteFile(Buf, ToWrite, Written);
    Str := Format('W %.2x  ', [Buf[0]]);
    for I := 1 to Written-1 do
      Str := Str + Format('%.2x ', [Buf[I]]);
    HistoryListBox.ItemIndex := HistoryListBox.Items.Add(Str);
  end;
end;

procedure TForm1.SaveClick(Sender: TObject);
begin
  ForceCurrentDirectory := True;
  if SaveDialog1.Execute then
    HistoryListBox.Items.SaveToFile(SaveDialog1.FileName);
end;

procedure TForm1.FormActivate(Sender: TObject);
var
  I, J: Integer;
begin
  if Assigned(Edits[0]) then
    Exit;
  Edits[0] := Edit1;
  for I := 1 to 63 do
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
        Parent   := Edit1.Parent;
        TabOrder := 2 + J*16 + I;
      end;
  DevListBoxClick(Self);
end;

end.
