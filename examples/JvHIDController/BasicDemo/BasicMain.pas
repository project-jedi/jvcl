unit BasicMain;

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, Forms,
  JvHidControllerClass;

type
  TMainForm = class(TForm)
    HidCtl: TJvHidDeviceController;
    DeviceList: TListBox;
    Description: TLabel;
    procedure HidCtlDeviceChange(Sender: TObject);
    function HidCtlEnumerate(HidDev: TJvHidDevice;
      const Idx: Integer): Boolean;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

procedure TMainForm.HidCtlDeviceChange(Sender: TObject);
begin
  DeviceList.Clear;
  HidCtl.Enumerate;
end;

function TMainForm.HidCtlEnumerate(HidDev: TJvHidDevice;
  const Idx: Integer): Boolean;
begin
  DeviceList.Items.Add(
    Format('%.4x/%.4x', [HidDev.Attributes.VendorID,
      HidDev.Attributes.ProductID]));
  Result := True;
end;

end.
