unit ExampleJVSegmentedLEDDisplayMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvComponent, JvLEDDisplays, ExtCtrls, ComCtrls, JvComCtrls, StdCtrls,
  JvInspector;

type
  TfrmExampleSegmentedLEDDisplayMain = class(TForm)
    sldScroller: TJvSegmentLEDDisplay;
    tmrLEDScroller: TTimer;
    pcMain: TJvPageControl;
    ts7Seg: TTabSheet;
    sldCarOdometer1: TJvSegmentLEDDisplay;
    sldCarOdoMeter2: TJvSegmentLEDDisplay;
    pnlCarOdometerRight: TPanel;
    pnlCarOdometerLeftBottom: TPanel;
    lblCarOdometerTopKM: TLabel;
    lblCarOdometerBottomKM: TLabel;
    pnlCarOdometerExplain: TPanel;
    lblCarOdometerExplain: TLabel;
    sld7SegTester: TJvSegmentLEDDisplay;
    insp7SegTester: TJvInspector;
    idnpMain: TJvInspectorDotNETPainter;
    procedure tmrLEDScrollerTimer(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure Loaded; override;
  public
    { Public declarations }
    procedure Init7SegInspector;
  end;

var
  frmExampleSegmentedLEDDisplayMain: TfrmExampleSegmentedLEDDisplayMain;

implementation

uses
  JvInspExtraEditors;

{$R *.DFM}

var
  ScrollText: string = '                WELCOME TO THE JEDI-VCL SEGMENTED LED DISPLAYS ' +
    'EXAMPLE                THIS EXAMPLE WILL DEMONSTRATE THE VARIOUS DISPLAY TYPES THAT BECOME ' +
    'AVAILABLE THROUGH THE COMPONENT TJVSEGMENTLEDDISPLAY                THIS TEXT IS SCROLLED ' +
    'ON A COMPONENT SET TO RENDER A 14-SEGMENT DISPLAY        USING THE PAGES BELOW YOU CAN SEE ' +
    'THE OTHER AVAILABLE TYPES AND WHICH CHARACTERS THEY SUPPORT';

procedure TfrmExampleSegmentedLEDDisplayMain.Loaded;
begin
  inherited Loaded;
  TJvInspectorColorItem.RegisterAsDefaultItem;
  Init7SegInspector;
end;

procedure TfrmExampleSegmentedLEDDisplayMain.Init7SegInspector;
var
  Cat: TJvInspectorCustomCategoryItem;
  Instances: TJvInspectorItemInstances;
  I: Integer;
  Cat1: TJvInspectorCustomCategoryItem;
begin
  insp7SegTester.Root.SortKind := iskNone;
  Cat := TJvInspectorCustomCategoryItem.Create(insp7SegTester.Root, nil);
  Cat.DisplayName := 'Fixed settings';
  Instances := TJvInspectorPropData.NewByNames(Cat, sld7SegTester, ['DigitCount', 'DigitHeight', 'DigitWidth']);
  for I := High(Instances) downto 0 do
    Instances[I].ReadOnly := True;
//  Cat.Expanded := True;
  Cat := TJvInspectorCustomCategoryItem.Create(insp7SegTester.Root, nil);
  Cat.DisplayName := 'Appearance';
  Cat1 := TJvInspectorCustomCategoryItem.Create(Cat, nil);
  Cat1.DisplayName := 'Colors';
  TJvInspectorPropData.New(Cat1, sld7SegTester, 'Color').DisplayName := 'Background';
  TJvInspectorPropData.New(Cat1, sld7SegTester, 'ColorOn').DisplayName := 'Segment lit';
  TJvInspectorPropData.New(Cat1, sld7SegTester, 'ColorOff').DisplayName := 'Segment unlit';
  Cat1.Expanded;
  TJvInspectorPropData.New(Cat, sld7SegTester, 'SegmentWidth').DisplayName := 'Segment width';
  TJvInspectorPropData.New(Cat, sld7SegTester, 'Spacing').DisplayName := 'Space between segments';
  TJvInspectorPropData.New(Cat, sld7SegTester, 'SlantAngle').DisplayName := 'Slanting angle';
  TJvInspectorPropData.New(Cat, sld7SegTester, 'Margin');
  Cat.Expanded := True;
  TJvInspectorPropData.New(insp7SegTester.Root, sld7SegTester, 'Text');
end;

procedure TfrmExampleSegmentedLEDDisplayMain.tmrLEDScrollerTimer(
  Sender: TObject);
begin
  sldScroller.Text := Copy(ScrollText, 1, 16);
  ScrollText := Copy(ScrollText, 2, Length(ScrollText) - 1) + ScrollText[1];
end;

end.
