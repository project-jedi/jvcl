unit ExampleJVSegmentedLEDDisplayMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvComponent, JvLEDDisplays, ExtCtrls, ComCtrls, JvComCtrls, StdCtrls,
  ExampleJVSegmentedLEDDisplay7SegMain,
  ExampleJVSegmentedLEDDisplay14SegMain,
  ExampleJVSegmentedLEDDisplay16SegMain;

type
  TfrmExampleSegmentedLEDDisplayMain = class(TForm)
    sldScroller: TJvSegmentLEDDisplay;
    tmrUpdater: TTimer;
    pcMain: TJvPageControl;
    ts7Seg: TTabSheet;
    fme7SegExamples1: Tfme7SegExamples;
    ts14Seg: TTabSheet;
    fme14SegExamples1: Tfme14SegExamples;
    ts16Segs: TTabSheet;
    fme16SegExamples1: Tfme16SegExamples;
    procedure tmrUpdaterTimer(Sender: TObject);
  private
    { Private declarations }
    ScrollCnt: Integer;
  protected
    procedure Loaded; override;
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

procedure TfrmExampleSegmentedLEDDisplayMain.Loaded;
begin
  inherited Loaded;
  pcMain.ActivePageIndex := 0;
end;

procedure TfrmExampleSegmentedLEDDisplayMain.tmrUpdaterTimer(
  Sender: TObject);
begin
  fme7SegExamples1.TimerEvent;
  fme14SegExamples1.TimerEvent;
  fme16SegExamples1.TimerEvent;
  Inc(ScrollCnt);
  if not Odd(ScrollCnt) then
  begin
    sldScroller.Text := Copy(ScrollText, 1, 16);
    ScrollText := Copy(ScrollText, 2, Length(ScrollText) - 1) + ScrollText[1];
  end;
end;

end.
