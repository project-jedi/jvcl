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

unit Info;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvHidControllerClass;

type
  TInfoForm = class(TForm)
    DevStrings: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Vid: TLabel;
    Pid: TLabel;
    Vers: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    InputLen: TLabel;
    OutputLen: TLabel;
    FeatureLen: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    VendorName: TLabel;
    ProductName: TLabel;
    Label11: TLabel;
    SerialNo: TLabel;
    LangStrings: TListBox;
    Label12: TLabel;
    procedure FormShow(Sender: TObject);
  public
    Dev: TJvHidDevice;
  end;

var
  InfoForm: TInfoForm;

implementation

{$R *.DFM}

procedure TInfoForm.FormShow(Sender: TObject);
var
  I: Integer;
begin
  VendorName.Caption := Dev.VendorName;
  ProductName.Caption := Dev.ProductName;
  SerialNo.Caption := Dev.SerialNumber;
  Vid.Caption  := IntToHex(Dev.Attributes.VendorID, 4);
  Pid.Caption  := IntToHex(Dev.Attributes.ProductID, 4);
  Vers.Caption := IntToHex(Dev.Attributes.VersionNumber, 4);
  if Dev.Caps.InputReportByteLength > 0 then
    InputLen.Caption   := IntToHex(Dev.Caps.InputReportByteLength-1, 1);
  if Dev.Caps.OutputReportByteLength > 0 then
    OutputLen.Caption  := IntToHex(Dev.Caps.OutputReportByteLength-1, 1);
  if Dev.Caps.FeatureReportByteLength > 0 then
    FeatureLen.Caption := IntToHex(Dev.Caps.FeatureReportByteLength-1, 1);
  for I := 1 to 255 do
    if Dev.DeviceStrings[I] <> '' then
      DevStrings.Items.Add(Format('%3d) %s',[I, Dev.DeviceStrings[I]]));
  for I := 0 to Dev.LanguageStrings.Count - 1 do
    LangStrings.Items.Add(Dev.LanguageStrings[I]);
end;

end.
