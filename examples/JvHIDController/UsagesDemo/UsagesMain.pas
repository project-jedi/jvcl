unit UsagesMain;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, StdCtrls, Buttons,
  JvHidControllerClass, Hid, HidUsage;

type
  TUsagesForm = class(TForm)
    DevListBox: TListBox;
    HidCtl: TJvHidDeviceController;
    Info: TSpeedButton;
    Description: TLabel;
    procedure HidCtlDeviceChange(Sender: TObject);
    function HidCtlEnumerate(HidDev: TJvHidDevice;
      const Idx: Integer): Boolean;
    procedure InfoClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
  end;

var
  UsagesForm: TUsagesForm;

implementation

uses
  Info;

{$R *.DFM}

procedure TUsagesForm.FormShow(Sender: TObject);
begin
  // this compensates the possibility that HidCtlDeviceChange
  // may not be able to fill the listbox the first time
  HidCtlDeviceChange(Self);
end;

procedure TUsagesForm.HidCtlDeviceChange(Sender: TObject);
var
  Dev: TJvHidDevice;
  I: Integer;
begin
  // HidCtlDeviceChange happens before TUsagesForm.Create completed
  // DevListBox may have not been created yet
  if Assigned(DevListBox) then
  begin
    // hand back all the devices assigned to the list entries
    for I := 0 to DevListBox.Count-1 do
    begin
      Dev := TJvHidDevice(DevListBox.Items.Objects[I]);
      HidCtl.CheckIn(Dev);
      DevListBox.Items.Objects[I] := nil;
    end;
    DevListBox.Items.Clear;
    HidCtl.Enumerate;
  end;
end;

function TUsagesForm.HidCtlEnumerate(HidDev: TJvHidDevice;
  const Idx: Integer): Boolean;
var
  Dev: TJvHidDevice;
  N: Integer;
begin
  // add a descriptive entry to the listbox for the device
  if HidDev.ProductName <> '' then
    N := DevListBox.Items.Add(HidDev.ProductName)
  else
    N := DevListBox.Items.Add(Format('Device VID=%x PID=%x',
      [HidDev.Attributes.VendorID, HidDev.Attributes.ProductID]));
  // check out the device and assign it to the list entry
  HidCtl.CheckOutByIndex(Dev, Idx);
  DevListBox.Items.Objects[N] := Dev;
  Result := True;
end;

procedure TUsagesForm.InfoClick(Sender: TObject);
begin
  // show info about the device assigned to the list entry
  if (DevListBox.Items.Count > 0) and (DevListBox.ItemIndex >= 0) then
    with TInfoForm.Create(Self) do
    begin
      Dev := TJvHidDevice(DevListBox.Items.Objects[DevListBox.ItemIndex]);
      ShowModal;
      Free;
    end;
end;

end.
