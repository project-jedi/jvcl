unit Unit1;

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, Forms,
  JvHidControllerClass, JvComponent;

type
  TForm1 = class(TForm)
    HidCtl: TJvHidDeviceController;
    ListBox1: TListBox;
    procedure HidCtlDeviceChange(Sender: TObject);
    function HidCtlEnumerate(const HidDev: TJvHidDevice;
      const Idx: Integer): Boolean;
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.HidCtlDeviceChange(Sender: TObject);
begin
  ListBox1.Clear;
  HidCtl.Enumerate;
end;

function TForm1.HidCtlEnumerate(const HidDev: TJvHidDevice;
  const Idx: Integer): Boolean;
begin
  ListBox1.Items.Add(
    Format('%x/%x', [HidDev.Attributes.VendorID,
      HidDev.Attributes.ProductID]));
  Result := True;
end;

end.
