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
