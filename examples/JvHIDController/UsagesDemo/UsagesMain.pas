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

unit UsagesMain;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, StdCtrls, Buttons,
  JvHidControllerClass, Hid, HidUsage, JvComponent;

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
    for I := 0 to DevListBox.Items.Count-1 do
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
