unit MouseReader;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,  JvHidControllerClass;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    HidCtl: TJvHidDeviceController;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Buttons: TLabel;
    DeltaX: TLabel;
    DeltaY: TLabel;
    procedure HidCtlDeviceChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    DevList: TList;
  end;

  // we presume the data format of a USB mouse
  TMouseReport = packed record
    ReportID: Byte;
    Buttons:  Byte;
    DeltaX:   Shortint;
    DeltaY:   Shortint;
    Dummy:    array [0..11] of Byte;
  end;

  TMouseThread = class(TThread)
  private
    Dev:      TJvHidDevice;
    DevIndex: Integer;
    Data:     TMouseReport;
  public
    procedure Execute; override;
    procedure HandleMouseData;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.HidCtlDeviceChange(Sender: TObject);
var
  I: Integer;
  Trd: TMouseThread;
  HidDev: TJvHidDevice;
begin
  // an HID device has been plugged or unplugged
  // we kill all threads and create new ones
  if DevList = nil then
    DevList := TList.Create;
  for i := 0 to DevList.Count - 1 do
  begin
    Trd := DevList.Items[I];
    HidDev := Trd.Dev;
    Trd.Terminate;
    Trd.WaitFor;
    Trd.Free;
    HidDev.Free;
  end;
  DevList.Clear;
  ListBox1.Clear;
  // get all devices we are interested in
  while HidCtl.CheckOutByClass(HidDev, cHidMouseClass) do
  begin
    // start a thread for each device
    Trd := TMouseThread.Create(True);
    with Trd do
    begin
      // initialize threads data
      FreeOnTerminate := True;
      Dev := HidDev;
      // we mark the Thread with its ListBox position
      DevIndex := DevList.Count;
      DevList.Add(Trd);
      if HidDev.ProductName = '' then
        ListBox1.Items.Add(Format('VID=%.4x PID=%.4x',
          [HidDev.Attributes.VendorID,
           HidDev.Attributes.ProductID]))
      else
        ListBox1.Items.Add(HidDev.ProductName);
      // let the thread run
      Resume;
    end;
  end;
end;

procedure TMouseThread.HandleMouseData;
begin
  // select the device the data comes from
  Form1.ListBox1.ItemIndex := DevIndex;
  // show the data read
  Form1.Buttons.Caption := Format('%.2x',[Data.Buttons]);
  Form1.DeltaX.Caption  := Format('%d',  [Data.DeltaX]);
  Form1.DeltaY.Caption  := Format('%d',  [Data.DeltaY]);
end;

procedure TMouseThread.Execute;

var
  SleepRet: DWORD;

  procedure Dummy(ErrorCode: DWORD; Count: DWORD;
    Ovl: POverlapped); stdcall;
  begin
  end;

begin
  SleepRet := 0;
  while not Terminated do
  begin
    // read mouse data
    Dev.ReadFileEx(Data, Dev.Caps.InputReportByteLength,
      @Dummy);
    // wait for read to complete
    repeat
      SleepRet := SleepEx(1000, True);
    until Terminated or (SleepRet = WAIT_IO_COMPLETION);
    // show data read
    if not Terminated then
      Synchronize(HandleMouseData);
  end;
  // cancel ReadFileEx call or the callback will
  // crash your program
  if SleepRet <> WAIT_IO_COMPLETION then
    Dev.CancelIO(omRead);
end;

end.
