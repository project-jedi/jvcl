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

unit CollectionMain;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, ComCtrls,
  JvHidControllerClass, Dialogs, StdCtrls;

type
  TCollectionDemoForm = class(TForm)
    HidCtl: TJvHidDeviceController;
    DeviceTree: TTreeView;
    Save: TButton;
    SaveDialog: TSaveDialog;
    Description: TLabel;
    procedure FormShow(Sender: TObject);
    procedure HidCtlDeviceChange(Sender: TObject);
    function  HidCtlEnumerate(HidDev: TJvHidDevice;
      Index: Integer): Boolean;
    procedure SaveClick(Sender: TObject);
  public
    Root: TTreeNode;
    procedure EnumerateCaps(HidDev: TJvHidDevice;
      Parent: TTreeNode; Idx: Word);
    procedure EnumerateNodes(HidDev: TJvHidDevice;
      Parent: TTreeNode; Idx: Word; NumSiblings: Word);
  end;

var
  CollectionDemoForm: TCollectionDemoForm;

implementation

uses
  UsagesInfo, Hid;

{$R *.DFM}

procedure TCollectionDemoForm.FormShow(Sender: TObject);
begin
  // this compensates the possibility that HidCtlDeviceChange
  // may not be able to fill the treeview the first time
  HidCtlDeviceChange(Self);
end;

procedure TCollectionDemoForm.HidCtlDeviceChange(Sender: TObject);
begin
  // HidCtlDeviceChange happens before TCollectionDemoForm.Create completed
  // DeviceTree may have not been created yet
  if Assigned(DeviceTree) then
  begin
    // each time something happens to the HID devices the treeview is filled
    DeviceTree.Items.Clear;
    Root := DeviceTree.Items.Add(nil, 'HID-Devices');
    HidCtl.Enumerate;
  end;
end;

procedure TCollectionDemoForm.EnumerateCaps(HidDev: TJvHidDevice; Parent: TTreeNode; Idx: Word);

  procedure EnumerateButtonCaps(HidDev: TJvHidDevice; Parent: TTreeNode;
    ReportType: THIDPReportType; ReportText: string);
  var
    I: Integer;
    Ret: Integer;
    N: WORD;
    UsagePageText: string;
    UsageText: string;
    // 256 Caps entries should be always sufficient
    BtnCaps: array [0..255] of THIDPButtonCaps;
  begin
    N := 255;
    // get ALL ButtonCaps of the specified ReportType
    HidDev.ReportTypeParam := ReportType;
    Ret := HidDev.GetSpecificButtonCaps(@BtnCaps[0], N);
    if Ret = HIDP_STATUS_SUCCESS then
      for I := 0 to N-1 do
        if BtnCaps[I].LinkCollection = Idx then
        begin
          UsageAndUsagePageText(BtnCaps[I].UsagePage, BtnCaps[I].Usage, UsagePageText, UsageText);
          if BtnCaps[I].IsRange then
            DeviceTree.Items.AddChild(Parent,
              Format('Button %s Range: UsagePage=%s ($%.4x) %d..%d',
                [ReportText, UsagePageText, BtnCaps[I].UsagePage, BtnCaps[I].UsageMin, BtnCaps[I].UsageMax]))
          else
            DeviceTree.Items.AddChild(Parent,
              Format('Button %s: UsagePage=%s ($%.4x) Usage=%s ($%.4x)',
                [ReportText, UsagePageText, BtnCaps[I].UsagePage, UsageText, BtnCaps[I].Usage]));
        end;
  end;

  procedure EnumerateValueCaps(HidDev: TJvHidDevice; Parent: TTreeNode;
    ReportType: THIDPReportType; ReportText: string);
  var
    I: Integer;
    Ret: Integer;
    N: WORD;
    UsagePageText: string;
    UsageText: string;
    // 256 Caps entries should be always sufficient
    ValCaps: array [0..255] of THIDPValueCaps;
  begin
    N := 255;
    // get ALL ValueCaps of the specified ReportType
    HidDev.ReportTypeParam := ReportType;
    Ret := HidDev.GetSpecificValueCaps(@ValCaps[0], N);
    if Ret = HIDP_STATUS_SUCCESS then
      for I := 0 to N-1 do
        if ValCaps[I].LinkCollection = Idx then
        begin
          UsageAndUsagePageText(ValCaps[I].UsagePage, ValCaps[I].Usage, UsagePageText, UsageText);
          if ValCaps[I].IsRange then
            DeviceTree.Items.AddChild(Parent,
              Format('Value %s Range: UsagePage=%s ($%.4x) %d..%d',
                [ReportText, UsagePageText, ValCaps[I].UsagePage, ValCaps[I].UsageMin, ValCaps[I].UsageMax]))
          else
            DeviceTree.Items.AddChild(Parent,
              Format('Value %s: UsagePage=%s ($%.4x) Usage=%s ($%.4x)',
                [ReportText, UsagePageText, ValCaps[I].UsagePage, UsageText, ValCaps[I].Usage]));
        end;
  end;

begin
  // set the params to get ALL Caps
  HidDev.UsagePageParam      := 0;
  HidDev.UsageParam          := 0;
  HidDev.LinkCollectionParam := HIDP_LINK_COLLECTION_UNSPECIFIED;
  EnumerateButtonCaps(HidDev, Parent, HidP_Input,   'Input');
  EnumerateButtonCaps(HidDev, Parent, HidP_Output,  'Output');
  EnumerateButtonCaps(HidDev, Parent, HidP_Feature, 'Feature');
  EnumerateValueCaps (HidDev, Parent, HidP_Input,   'Input');
  EnumerateValueCaps (HidDev, Parent, HidP_Output,  'Output');
  EnumerateValueCaps (HidDev, Parent, HidP_Feature, 'Feature');
end;

procedure TCollectionDemoForm.EnumerateNodes(HidDev: TJvHidDevice;
  Parent: TTreeNode; Idx: Word; NumSiblings: Word);
var
  I: Word;
  Node: TTreeNode;
  UsagePageText: string;
  UsageText: string;
  CollectionTypeText: string;
  NodeText: string;
begin
  // add a list of sibling nodes to the device tree node Parent
  for I := 1 to NumSiblings do
  begin
    UsageAndUsagePageText(HidDev.LinkCollectionNodes[Idx].LinkUsagePage,
      HidDev.LinkCollectionNodes[Idx].LinkUsage, UsagePageText, UsageText);
    case HidDev.LinkCollectionNodes[Idx].CollectionType of
      $00:
        CollectionTypeText := 'Physical';
      $01:
        CollectionTypeText := 'Application';
      $02:
        CollectionTypeText := 'Logical';
      $03:
        CollectionTypeText := 'Report';
      $04:
        CollectionTypeText := 'Named Array';
      $05:
        CollectionTypeText := 'Usage Switch';
      $06:
        CollectionTypeText := 'Usage Modifier';
      $07..$7F:
        CollectionTypeText := Format('Reserved $%.2x',
          [Cardinal(HidDev.LinkCollectionNodes[Idx].CollectionType)]);
      $80..$FF:
        CollectionTypeText := Format('Vendor-defined $%.2x',
          [Cardinal(HidDev.LinkCollectionNodes[Idx].CollectionType)]);
    end;

    NodeText :=
      'UsagePage='      + UsagePageText      + Format(' ($%.4x)  ', [HidDev.LinkCollectionNodes[Idx].LinkUsagePage]) +
      'Usage='          + UsageText          + Format(' ($%.4x)  ', [HidDev.LinkCollectionNodes[Idx].LinkUsage]) +
      'CollectionType=' + CollectionTypeText + Format(' ($%.2x)',   [HidDev.LinkCollectionNodes[Idx].CollectionType]);
    if HidDev.LinkCollectionNodes[Idx].IsAlias <> 0 then
      NodeText := NodeText + '  IsAlias';
    Node := DeviceTree.Items.AddChild(Parent, NodeText);

    EnumerateCaps(HidDev, Node, Idx);

    // recurse to the children nodes
    if HidDev.LinkCollectionNodes[Idx].FirstChild <> 0 then
      EnumerateNodes(HidDev, Node,
        HidDev.LinkCollectionNodes[Idx].FirstChild,
        HidDev.LinkCollectionNodes[Idx].NumberOfChildren);
    // secure against buggy descriptors
    if HidDev.LinkCollectionNodes[Idx].NextSibling <> 0 then
      // follow the link to the next sibling
      Idx := HidDev.LinkCollectionNodes[Idx].NextSibling
    else
      Break;
  end;
end;

function TCollectionDemoForm.HidCtlEnumerate(HidDev: TJvHidDevice;
  Index: Integer): Boolean;
var
  Idx: Word;
  Node: TTreeNode;
  Name: string;
begin
  // give the treeview node for the device a descriptive name
  if HidDev.ProductName <> '' then
    Name := Format('"%s"  VID=$%.4x PID=$%.4x',
      [HidDev.ProductName, HidDev.Attributes.VendorID, HidDev.Attributes.ProductID])
  else
    Name := Format('VID=$%.4x PID=$%.4x',
      [HidDev.Attributes.VendorID, HidDev.Attributes.ProductID]);
  Node := DeviceTree.Items.AddChild(Root, Name);

  // there is only one root node, but we try to enumerate siblings anyway
  // in case there is a bad descriptor
  Idx := 0;
  repeat
    EnumerateNodes(HidDev, Node, Idx, 1);
    Idx := HidDev.LinkCollectionNodes[Idx].NextSibling;
  until Idx = 0;

  DeviceTree.FullExpand;
  Result := True;
end;

procedure TCollectionDemoForm.SaveClick(Sender: TObject);
begin
  // write the treeview content to a file to avoid the need
  // to send screenshots
  if SaveDialog.Execute then
    DeviceTree.SaveToFile(SaveDialog.FileName);
end;

end.
