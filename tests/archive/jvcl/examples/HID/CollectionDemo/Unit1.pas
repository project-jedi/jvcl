unit Unit1;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, ComCtrls,
  JvHidControllerClass;

type
  TForm1 = class(TForm)
    HidCtl: TJvHidDeviceController;
    TreeView1: TTreeView;
    procedure HidCtlDeviceChange(Sender: TObject);
    function  HidCtlEnumerate(HidDev: TJvHidDevice;
      Index: Integer): Boolean;
  private
    FRoot: TTreeNode;
  public
    procedure EnumerateNodes(HidDev: TJvHidDevice;
      Parent: TTreeNode; Idx, Count: WORD);
  end;

var
  Form1: TForm1;

implementation

uses
  HidUsage;

{$R *.DFM}

procedure TForm1.HidCtlDeviceChange(Sender: TObject);
begin
  TreeView1.Items.Clear;
  FRoot := TreeView1.Items.Add(nil, 'HID-Devices');
  HidCtl.Enumerate;
end;

procedure TForm1.EnumerateNodes(HidDev: TJvHidDevice;
   Parent: TTreeNode; Idx, Count: WORD);
var
  I: Integer;
  Node: TTreeNode;
  UsagePageText: string;
  UsageText: string;
  CollectionTypeText: string;
begin
  // add a list of sibling nodes to the device tree node Parent
  I := 0;
  repeat
    UsagePageText := '';
    UsageText := '';
    case HidDev.LinkCollectionNodes[Idx].LinkUsagePage of
      HID_USAGE_PAGE_GENERIC:
        begin
          UsagePageText := 'Generic Desktop';
          case HidDev.LinkCollectionNodes[Idx].LinkUsage of
            HID_USAGE_GENERIC_POINTER:
              UsageText := 'Pointing Device';
            HID_USAGE_GENERIC_MOUSE:
              UsageText := 'Mouse';
            HID_USAGE_GENERIC_JOYSTICK:
              UsageText := 'Joystick';
            HID_USAGE_GENERIC_GAMEPAD:
              UsageText := 'Gamepad';
            HID_USAGE_GENERIC_KEYBOARD:
              UsageText := 'Keyboard';
            HID_USAGE_GENERIC_KEYPAD:
              UsageText := 'Keypad';
            HID_USAGE_GENERIC_MULTIAXIS:
              UsageText := 'Multiaxis';
            HID_USAGE_GENERIC_SYSTEM_CTL:
              UsageText := 'System Control';
            HID_USAGE_GENERIC_X:
              UsageText := 'X Axis';
            HID_USAGE_GENERIC_Y:
              UsageText := 'Y Axis';
            HID_USAGE_GENERIC_Z:
              UsageText := 'Z Axis';
            HID_USAGE_GENERIC_RX:
              UsageText := 'Relative X Axis';
            HID_USAGE_GENERIC_RY:
              UsageText := 'Relative Y Axis';
            HID_USAGE_GENERIC_RZ:
              UsageText := 'Relative Z Axis';
            HID_USAGE_GENERIC_SLIDER:
              UsageText := 'Slider';
            HID_USAGE_GENERIC_DIAL:
              UsageText := 'Dial';
            HID_USAGE_GENERIC_WHEEL:
              UsageText := 'Wheel';
            HID_USAGE_GENERIC_HATSWITCH:
              UsageText := '';
            HID_USAGE_GENERIC_COUNTED_BUFFER:
              UsageText := 'Counted Buffer';
            HID_USAGE_GENERIC_BYTE_COUNT:
              UsageText := 'Byte Count';
            HID_USAGE_GENERIC_MOTION_WAKEUP:
              UsageText := 'Motion Wakeup';
            HID_USAGE_GENERIC_START:
              UsageText := 'Start';
            HID_USAGE_GENERIC_SELECT:
              UsageText := 'Select';
            HID_USAGE_GENERIC_VX:
              UsageText := 'Velocity X';
            HID_USAGE_GENERIC_VY:
              UsageText := 'Velocity Y';
            HID_USAGE_GENERIC_VZ:
              UsageText := 'Velocity Z';
            HID_USAGE_GENERIC_VBRX:
              UsageText := 'Velocity Brake X';
            HID_USAGE_GENERIC_VBRY:
              UsageText := 'Velocity Brake Y';
            HID_USAGE_GENERIC_VBRZ:
              UsageText := 'Velocity Brake Z';
            HID_USAGE_GENERIC_VNO:
              UsageText := 'VNO';
            HID_USAGE_GENERIC_SYSCTL_POWER:
              UsageText := 'System Control Power';
            HID_USAGE_GENERIC_SYSCTL_SLEEP:
              UsageText := 'System Control Sleep';
            HID_USAGE_GENERIC_SYSCTL_WAKE:
              UsageText := 'System Control Wake';
            HID_USAGE_GENERIC_SYSCTL_CONTEXT_MENU:
              UsageText := 'System Control Context Menu';
            HID_USAGE_GENERIC_SYSCTL_MAIN_MENU:
              UsageText := 'System Control Main Menu';
            HID_USAGE_GENERIC_SYSCTL_APP_MENU:
              UsageText := 'System Control App Menu';
            HID_USAGE_GENERIC_SYSCTL_HELP_MENU:
              UsageText := 'System Control Help Menu';
            HID_USAGE_GENERIC_SYSCTL_MENU_EXIT:
              UsageText := 'System Control Menu Exit';
            HID_USAGE_GENERIC_SYSCTL_MENU_SELECT:
              UsageText := 'System Control Menu Select';
            HID_USAGE_GENERIC_SYSCTL_MENU_RIGHT:
              UsageText := 'System Control Menu Right';
            HID_USAGE_GENERIC_SYSCTL_MENU_LEFT:
              UsageText := 'System Control Menu Left';
            HID_USAGE_GENERIC_SYSCTL_MENU_UP:
              UsageText := 'System Control Menu Up';
            HID_USAGE_GENERIC_SYSCTL_MENU_DOWN:
              UsageText := 'System Control Menu Down';
            HID_USAGE_GENERIC_SYSCTL_DPAD_UP:
              UsageText := 'System Control DPad Up';
            HID_USAGE_GENERIC_SYSCTL_DPAD_DOWN:
              UsageText := 'System Control DPad Down';
            HID_USAGE_GENERIC_SYSCTL_DPAD_RIGHT:
              UsageText := 'System Control DPad Right';
            HID_USAGE_GENERIC_SYSCTL_DPAD_LEFT:
              UsageText := 'System Control DPad Left';
          end;
        end;
      HID_USAGE_PAGE_SIMULATION:
        begin
          UsagePageText := 'Simulation';
          case HidDev.LinkCollectionNodes[Idx].LinkUsage of
            HID_USAGE_SIMULATION_RUDDER:
              UsageText := 'Rudder';
            HID_USAGE_SIMULATION_THROTTLE:
              UsageText := 'Throttle';
          end;
        end;
      HID_USAGE_PAGE_VR:
        UsagePageText := 'Virtual Reality';
      HID_USAGE_PAGE_SPORT:
        UsagePageText := 'Sport';
      HID_USAGE_PAGE_GAME:
        UsagePageText := 'Game';
      HID_USAGE_PAGE_KEYBOARD:
        UsagePageText := 'Keyboard';
      HID_USAGE_PAGE_LED:
        UsagePageText := 'LED';
      HID_USAGE_PAGE_BUTTON:
        UsagePageText := 'Button';
      HID_USAGE_PAGE_ORDINAL:
        UsagePageText := 'Ordinal';
      HID_USAGE_PAGE_TELEPHONY:
        begin
          UsagePageText := 'Telephony';
          case HidDev.LinkCollectionNodes[Idx].LinkUsage of
            HID_USAGE_TELEPHONY_PHONE:
              UsageText := 'Phone';
            HID_USAGE_TELEPHONY_ANSWERING_MACHINE:
              UsageText := 'Answering Machine';
            HID_USAGE_TELEPHONY_MESSAGE_CONTROLS:
              UsageText := 'Message Controls';
            HID_USAGE_TELEPHONY_HANDSET:
              UsageText := 'Handset';
            HID_USAGE_TELEPHONY_HEADSET:
              UsageText := 'Headset';
            HID_USAGE_TELEPHONY_KEYPAD:
              UsageText := 'Keypad';
            HID_USAGE_TELEPHONY_PROGRAMMABLE_BUTTON:
              UsageText := 'Programmable Button';
          end;
        end;
      HID_USAGE_PAGE_CONSUMER:
        UsagePageText := 'Consumer';
      HID_USAGE_PAGE_DIGITIZER:
        UsagePageText := 'Digitizer';
      HID_USAGE_PAGE_UNICODE:
        UsagePageText := 'Unicode';
      HID_USAGE_PAGE_ALPHANUMERIC:
        UsagePageText := 'Alphanumeric';
    end;

    if UsagePageText = '' then
      UsagePageText := Format('UsagePage=%u',
        [HidDev.LinkCollectionNodes[Idx].LinkUsagePage]);
    if UsageText = '' then
      UsageText := Format('Usage=%u',
        [HidDev.LinkCollectionNodes[Idx].LinkUsage]);

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

    Node := TreeView1.Items.AddChild(Parent,
      UsagePageText + ': ' + UsageText + ' (' + CollectionTypeText + ')');

    // recurse to the children nodes
    if HidDev.LinkCollectionNodes[Idx].FirstChild <> 0 then
      EnumerateNodes(HidDev, Node,
        Idx + HidDev.LinkCollectionNodes[Idx].FirstChild,
         HidDev.LinkCollectionNodes[Idx].NumberOfChildren);
    // follow the link to the next sibling
    Idx := HidDev.LinkCollectionNodes[Idx].NextSibling;
    Inc(I);
  until I >= Count;
end;

function TForm1.HidCtlEnumerate(HidDev: TJvHidDevice;
  Index: Integer): Boolean;
var
  Node: TTreeNode;
  Name: string;
begin
  Name := HidDev.ProductName;
  if Name = '' then
    Name := Format('VID=%.4x PID=%.4x', [HidDev.Attributes.VendorID, HidDev.Attributes.ProductID]);
  Node := TreeView1.Items.AddChild(FRoot, Name);
  EnumerateNodes(HidDev, Node, 1, HidDev.LinkCollectionNodes[1].NumberOfChildren);
  TreeView1.FullExpand;
  Result := True;
end;

end.
