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
    InfoMemo: TMemo;
    Save: TButton;
    SaveDialog: TSaveDialog;
    procedure FormShow(Sender: TObject);
    procedure CollectBtnInfo(Val: WORD; Title, TitleBar: string);
    procedure CollectValueInfo(Val: WORD; Title, TitleBar: string);
    procedure SaveClick(Sender: TObject);
  public
    Dev: TJvHidDevice;
  end;

implementation

{$R *.DFM}

uses
  UsagesInfo, Hid;

function BoolToStr(B: Boolean): string;
const
  Strs: array [Boolean] of PChar =
    ('False', 'True');
begin
  Result := Strs[B];
end;

procedure TInfoForm.CollectBtnInfo(Val: WORD; Title, TitleBar: string);
var
  I: Integer;
  Ret: Integer;
  N: WORD;
  UsagePageText: string;
  UsageText: string;
  LinkUsagePageText: string;
  LinkUsageText: string;
  // more than enough Caps structures to read
  BtnCaps: array [0..255] of THIDPButtonCaps;
begin
  N := Length(BtnCaps);
  Dev.ReportTypeParam := Val;
  Ret := Dev.GetButtonCaps(@BtnCaps[0], N);
  if Ret = HIDP_STATUS_SUCCESS then
  begin
    InfoMemo.Lines.Add('');
    InfoMemo.Lines.Add(Title);
    InfoMemo.Lines.Add(TitleBar);
    for I := 0 to N-1 do
    begin
      UsageAndUsagePageText(BtnCaps[I].UsagePage,     BtnCaps[I].Usage,     UsagePageText,     UsageText);
      UsageAndUsagePageText(BtnCaps[I].LinkUsagePage, BtnCaps[I].LinkUsage, LinkUsagePageText, LinkUsageText);
      InfoMemo.Lines.Add('');
      InfoMemo.Lines.Add(Format('%u)', [I]));
      InfoMemo.Lines.Add(Format('UsagePage:         %s ($%.4x)', [UsagePageText, BtnCaps[I].UsagePage]));
      InfoMemo.Lines.Add(Format('ReportID:          %u',         [BtnCaps[I].ReportID]));
      InfoMemo.Lines.Add(Format('IsAlias:           %s',         [BoolToStr(BtnCaps[I].IsAlias)]));
      InfoMemo.Lines.Add(Format('BitField:          %u',         [BtnCaps[I].BitField]));
      InfoMemo.Lines.Add(Format('LinkCollection:    %u',         [BtnCaps[I].LinkCollection]));
      InfoMemo.Lines.Add(Format('LinkUsage:         %s ($%.4x)', [LinkUsageText, BtnCaps[I].LinkUsage]));
      InfoMemo.Lines.Add(Format('LinkUsagePage:     %s ($%.4x)', [LinkUsagePageText, BtnCaps[I].LinkUsagePage]));
      InfoMemo.Lines.Add(Format('IsRange:           %s',         [BoolToStr(BtnCaps[I].IsRange)]));
      InfoMemo.Lines.Add(Format('IsStringRange:     %s',         [BoolToStr(BtnCaps[I].IsStringRange)]));
      InfoMemo.Lines.Add(Format('IsDesignatorRange: %s',         [BoolToStr(BtnCaps[I].IsDesignatorRange)]));
      InfoMemo.Lines.Add(Format('IsAbsolute:        %s',         [BoolToStr(BtnCaps[I].IsAbsolute)]));
      if BtnCaps[I].IsRange then
      begin
        InfoMemo.Lines.Add(Format('UsageMin:      %d UsageMax:      %d', [BtnCaps[I].UsageMin,      BtnCaps[I].UsageMax]));
        InfoMemo.Lines.Add(Format('StringMin:     %d StringMax:     %d', [BtnCaps[I].StringMin,     BtnCaps[I].StringMax]));
        InfoMemo.Lines.Add(Format('DesignatorMin: %d DesignatorMax: %d', [BtnCaps[I].DesignatorMin, BtnCaps[I].DesignatorMax]));
        InfoMemo.Lines.Add(Format('DataIndexMin:  %d DataIndexMax:  %d', [BtnCaps[I].DataIndexMin,  BtnCaps[I].DataIndexMax]));
      end
      else
      begin
        InfoMemo.Lines.Add(Format('Usage:             %s ($%.4x)', [UsageText, BtnCaps[I].Usage]));
        InfoMemo.Lines.Add(Format('StringIndex:       %u',         [BtnCaps[I].StringIndex]));
        InfoMemo.Lines.Add(Format('DesignatorIndex:   %u',         [BtnCaps[I].DesignatorIndex]));
        InfoMemo.Lines.Add(Format('DataIndex:         %u',         [BtnCaps[I].DataIndex]));
      end;
    end;
  end;
end;

procedure TInfoForm.CollectValueInfo(Val: WORD; Title, TitleBar: string);
var
  I: Integer;
  Ret: Integer;
  N: WORD;
  UsagePageText: string;
  UsageText: string;
  LinkUsagePageText: string;
  LinkUsageText: string;
  // more than enough Caps structures to read
  ValCaps: array [0..255] of THIDPValueCaps;
begin
  N := Length(ValCaps);
  Dev.ReportTypeParam := Val;
  Ret := Dev.GetValueCaps(@ValCaps[0], N);
  if Ret = HIDP_STATUS_SUCCESS then
  begin
    InfoMemo.Lines.Add('');
    InfoMemo.Lines.Add(Title);
    InfoMemo.Lines.Add(TitleBar);
    for I := 0 to N-1 do
    begin
      UsageAndUsagePageText(ValCaps[I].UsagePage, ValCaps[I].Usage, UsagePageText, UsageText);
      UsageAndUsagePageText(ValCaps[I].LinkUsagePage, ValCaps[I].LinkUsage, LinkUsagePageText, LinkUsageText);
      InfoMemo.Lines.Add('');
      InfoMemo.Lines.Add(Format('%u)', [I]));
      InfoMemo.Lines.Add(Format('UsagePage:         %s ($%.4x)', [UsagePageText, ValCaps[I].UsagePage]));
      InfoMemo.Lines.Add(Format('ReportID:          %u',         [ValCaps[I].ReportID]));
      InfoMemo.Lines.Add(Format('IsAlias:           %s',         [BoolToStr(ValCaps[I].IsAlias)]));
      InfoMemo.Lines.Add(Format('BitField:          %u',         [ValCaps[I].BitField]));
      InfoMemo.Lines.Add(Format('LinkCollection:    %u',         [ValCaps[I].LinkCollection]));
      InfoMemo.Lines.Add(Format('LinkUsage:         %s ($%.4x)', [LinkUsageText, ValCaps[I].LinkUsage]));
      InfoMemo.Lines.Add(Format('LinkUsagePage:     %s ($%.4x)', [LinkUsagePageText, ValCaps[I].LinkUsagePage]));
      InfoMemo.Lines.Add(Format('IsRange:           %s',         [BoolToStr(ValCaps[I].IsRange)]));
      InfoMemo.Lines.Add(Format('IsStringRange:     %s',         [BoolToStr(ValCaps[I].IsStringRange)]));
      InfoMemo.Lines.Add(Format('IsDesignatorRange: %s',         [BoolToStr(ValCaps[I].IsDesignatorRange)]));
      InfoMemo.Lines.Add(Format('IsAbsolute:        %s',         [BoolToStr(ValCaps[I].IsAbsolute)]));
      InfoMemo.Lines.Add(Format('HasNull:           %s',         [BoolToStr(ValCaps[I].HasNull)]));
      InfoMemo.Lines.Add(Format('BitSize:           %u',         [ValCaps[I].BitSize]));
      InfoMemo.Lines.Add(Format('ReportCount:       %u',         [ValCaps[I].ReportCount]));
      InfoMemo.Lines.Add(Format('UnitsExp:          %u',         [ValCaps[I].UnitsExp]));
      InfoMemo.Lines.Add(Format('Units:             %u',         [ValCaps[I].Units]));
      InfoMemo.Lines.Add(Format('LogicalMin:        %d',         [ValCaps[I].LogicalMin]));
      InfoMemo.Lines.Add(Format('LogicalMax:        %d',         [ValCaps[I].LogicalMax]));
      InfoMemo.Lines.Add(Format('PhysicalMin:       %d',         [ValCaps[I].PhysicalMin]));
      InfoMemo.Lines.Add(Format('PhysicalMax:       %d',         [ValCaps[I].PhysicalMax]));
      if ValCaps[I].IsRange then
      begin
        InfoMemo.Lines.Add(Format('UsageMin:      %d UsageMax:      %d', [ValCaps[I].UsageMin,      ValCaps[I].UsageMax]));
        InfoMemo.Lines.Add(Format('StringMin:     %d StringMax:     %d', [ValCaps[I].StringMin,     ValCaps[I].StringMax]));
        InfoMemo.Lines.Add(Format('DesignatorMin: %d DesignatorMax: %d', [ValCaps[I].DesignatorMin, ValCaps[I].DesignatorMax]));
        InfoMemo.Lines.Add(Format('DataIndexMin:  %d DataIndexMax:  %d', [ValCaps[I].DataIndexMin,  ValCaps[I].DataIndexMax]));
      end
      else
      begin
        InfoMemo.Lines.Add(Format('Usage:             %s ($%.4x)', [UsageText, ValCaps[I].Usage]));
        InfoMemo.Lines.Add(Format('StringIndex:       %u',         [ValCaps[I].StringIndex]));
        InfoMemo.Lines.Add(Format('DesignatorIndex:   %u',         [ValCaps[I].DesignatorIndex]));
        InfoMemo.Lines.Add(Format('DataIndex:         %u',         [ValCaps[I].DataIndex]));
      end;
    end;
  end;
end;

procedure TInfoForm.FormShow(Sender: TObject);
begin
  InfoMemo.Lines.Clear;
  if Dev.VendorName <> '' then
    InfoMemo.Lines.Add(Format('"%s" "%s" VID=$%.4x PID=$%.4x',
      [Dev.VendorName, Dev.ProductName, Dev.Attributes.VendorID, Dev.Attributes.ProductID]))
  else
    InfoMemo.Lines.Add(Format('VID=$%.4x PID=$%.4x',
      [Dev.Attributes.VendorID, Dev.Attributes.ProductID]));
  if Dev.SerialNumber <> '' then
    InfoMemo.Lines.Add(Format('SerialNumber=%s', [Dev.SerialNumber]));
  if Dev.Caps.InputReportByteLength > 0 then
    InfoMemo.Lines.Add(Format('Input Report Size=%d', [Dev.Caps.InputReportByteLength-1]))
  else
    InfoMemo.Lines.Add('Input Report Size=0');
  if Dev.Caps.OutputReportByteLength > 0 then
    InfoMemo.Lines.Add(Format('Output Report Size=%d', [Dev.Caps.OutputReportByteLength-1]))
  else
    InfoMemo.Lines.Add('Output Report Size=0');
  if Dev.Caps.FeatureReportByteLength > 0 then
    InfoMemo.Lines.Add(Format('Feature Report Size=%d', [Dev.Caps.FeatureReportByteLength-1]))
  else
    InfoMemo.Lines.Add('Feature Report Size=0');
  CollectBtnInfo(HidP_Input,     'Button Input',   '============');
  CollectBtnInfo(HidP_Output,    'Button Output',  '=============');
  CollectBtnInfo(HidP_Feature,   'Button Feature', '==============');
  CollectValueInfo(HidP_Input,   'Value Input',    '===========');
  CollectValueInfo(HidP_Output,  'Value Output',   '============');
  CollectValueInfo(HidP_Feature, 'Value Feature',  '=============');
end;

procedure TInfoForm.SaveClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    InfoMemo.Lines.SaveToFile(SaveDialog.FileName);
end;

end.
