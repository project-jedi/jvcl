unit ExampleJVSegmentedLEDDisplayMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvComponent, JvLEDDisplays, ExtCtrls, ComCtrls, JvComCtrls, StdCtrls;

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
    procedure tmrLEDScrollerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmExampleSegmentedLEDDisplayMain: TfrmExampleSegmentedLEDDisplayMain;

implementation

{$R *.DFM}

var
  ScrollText: string = '                WELCOME TO THE JEDI-VCL SEGMENTED LED DISPLAYS ' +
    'EXAMPLE                THIS EXAMPLE WILL DEMONSTRATE THE VARIOUS DISPLAY TYPES THAT BECOME ' +
    'AVAILABLE THROUGH THE COMPONENT TJVSEGMENTLEDDISPLAY                THIS TEXT IS SCROLLED ' +
    'ON A COMPONENT SET TO RENDER A 14-SEGMENT DISPLAY        USING THE PAGES BELOW YOU CAN SEE ' +
    'THE OTHER AVAILABLE TYPES AND WHICH CHARACTERS THEY SUPPORT';

procedure TfrmExampleSegmentedLEDDisplayMain.tmrLEDScrollerTimer(
  Sender: TObject);
begin
  sldScroller.Text := Copy(ScrollText, 1, 16);
  ScrollText := Copy(ScrollText, 2, Length(ScrollText) - 1) + ScrollText[1];
end;

end.
