unit ExampleJVSegmentedLEDDisplay7SegMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, JvComponent, JvLEDDisplays, Buttons;

type
  Tfme7SegExamples = class(TFrame)
    pc7Seg: TPageControl;
    tsOdometer: TTabSheet;
    sldCarOdometer1: TJvSegmentLEDDisplay;
    sldCarOdoMeter2: TJvSegmentLEDDisplay;
    pnlCarOdometerRight: TPanel;
    lblCarOdometerTopKM: TLabel;
    lblCarOdometerBottomKM: TLabel;
    pnlCarOdometerLeftBottom: TPanel;
    pnlCarOdometerExplain: TPanel;
    lblCarOdometerExplain: TLabel;
    sldHours: TJvSegmentLEDDisplay;
    sldTemp: TJvSegmentLEDDisplay;
    sldMinutes: TJvSegmentLEDDisplay;
    lblCelcius: TLabel;
    pnlDegrees: TPanel;
    sphDegrees: TShape;
    pnlColon: TPanel;
    sphTopColon: TShape;
    shpBottomColon: TShape;
    lblAM: TLabel;
    lblPM: TLabel;
    btnResetTrip: TSpeedButton;
    btn24hClock: TSpeedButton;
    lblSpeed: TLabel;
    edSpeed: TEdit;
    lblSpeedUnit1: TLabel;
    lblSpeedUnitSlash: TLabel;
    lblSpeedUnit2: TLabel;
    tsGeneralInfo: TTabSheet;
    sldCharsetMain: TJvSegmentLEDDisplay;
    lblCharsetCaption: TLabel;
    lblMainCharset: TLabel;
    lblAlternateCharset: TLabel;
    sldTest: TJvSegmentLEDDisplay;
    lblDescriptionCaption: TLabel;
    lblDescription: TLabel;
    lblTestAreaCaption: TLabel;
    sldCharsetAlt: TJvSegmentLEDDisplay;
    cxA: TCheckBox;
    cxB: TCheckBox;
    cxC: TCheckBox;
    cxD: TCheckBox;
    cxE: TCheckBox;
    cxF: TCheckBox;
    cxG: TCheckBox;
    cxDP: TCheckBox;
    lblCharacter: TLabel;
    edCharacter: TEdit;
    lblSegmentStringCaption: TLabel;
    edSegmentString: TEdit;
    lblA: TLabel;
    lblB: TLabel;
    lblC: TLabel;
    lblD: TLabel;
    lblE: TLabel;
    lblF: TLabel;
    lblG: TLabel;
    lblDP: TLabel;
    procedure cxAClick(Sender: TObject);
    procedure edCharacterChange(Sender: TObject);
    procedure edSegmentStringChange(Sender: TObject);
    procedure edSpeedChange(Sender: TObject);
    procedure btnResetTripClick(Sender: TObject);
  private
    { Private declarations }
    CheckMarkClicked: Boolean;
    CheckMarkChangedIsDP: Boolean;
    CharChanged: Boolean;
    SegmentStringChanged: Boolean;

    TotalOdo: Double;
    TripOdo: Double;
    Incs: Double;
    procedure UpdateCheckMarks;
  protected
    { Protected declarations }
    procedure Loaded; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure TimerEvent;
  end;

implementation

{$R *.DFM}

procedure Tfme7SegExamples.UpdateCheckMarks;
begin
  cxA.Checked := Pos('A', edSegmentString.Text) > 0;
  cxB.Checked := Pos('B', edSegmentString.Text) > 0;
  cxC.Checked := Pos('C', edSegmentString.Text) > 0;
  cxD.Checked := Pos('D', edSegmentString.Text) > 0;
  cxE.Checked := Pos('E', edSegmentString.Text) > 0;
  cxF.Checked := Pos('F', edSegmentString.Text) > 0;
  cxG.Checked := Pos('G', edSegmentString.Text) > 0;
  cxDP.Checked := Pos('DP', edSegmentString.Text) > 0;
end;

procedure Tfme7SegExamples.Loaded;
begin
  inherited Loaded;
  pc7Seg.ActivePageIndex := 0;
end;

constructor Tfme7SegExamples.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TotalOdo := 74221;
  TripOdo := 539.1;
  edSpeed.OnChange(edSpeed);
end;

procedure Tfme7SegExamples.TimerEvent;
var
  Hr, Mn, Sec, MSec: Word;
begin
  { Update time }
  DecodeTime(Now, Hr, Mn, Sec, MSec);
  if not btn24hClock.Down then
  begin
    if Hr > 12 then
    begin
      Dec(Hr, 12);
      lblPM.Font.Color := sldCarOdometer1.ColorOn;
      lblAM.Font.Color := sldCarOdometer1.ColorOff;
    end
    else
    begin
      lblPM.Font.Color := sldCarOdometer1.ColorOff;
      lblAM.Font.Color := sldCarOdometer1.ColorOn;
    end;
  end
  else
  begin
    lblPM.Font.Color := sldCarOdometer1.ColorOff;
    lblAM.Font.Color := sldCarOdometer1.ColorOff;
  end;
  sldHours.Text := Format('%2d', [Hr]);
  sldMinutes.Text := Format('%.2d', [Mn]);

  // Update odometers
  TotalOdo := TotalOdo + Incs;
  TripOdo := TripOdo + Incs;
  if TripOdo >= 1000 then
    TripOdo := TripOdo - 1000;
  sldCarOdometer1.Text := Format('%6d', [Trunc(TotalOdo)]);
  sldCarOdoMeter2.Text := Format('%4.2d', [Trunc(TripOdo * 10)]);
  sldCarOdoMeter2.Digits[2].SegmentState[7] := True;
end;

procedure Tfme7SegExamples.cxAClick(Sender: TObject);
var
  S: string;
  I: Integer;
begin
  if not SegmentStringChanged then
  begin
    try
      CheckMarkClicked := True;
      CheckMarkChangedIsDP := Sender = cxDP;
      S := '';
      for I := tsGeneralInfo.ControlCount - 1 downto 0 do
      begin
        if (tsGeneralInfo.Controls[I] is TCustomCheckBox) and TCheckBox(tsGeneralInfo.Controls[I]).Checked then
          S := S + TCheckBox(tsGeneralInfo.Controls[I]).Caption;
      end;
      edSegmentString.Text := S;
    finally
      CheckmarkClicked := False;
      CheckMarkChangedIsDP := False;
    end;
  end;
end;

procedure Tfme7SegExamples.edCharacterChange(Sender: TObject);
begin
  if not CheckMarkClicked and not SegmentStringChanged then
  begin
    CharChanged := True;
    try
      edSegmentString.Text := CharacterToSegmentString(edCharacter.Text[1], slk7Segments);
    finally
      CharChanged := False;
    end;
  end;
end;

procedure Tfme7SegExamples.edSegmentStringChange(Sender: TObject);
var
  S: string;
begin
  if not SegmentStringChanged then
  begin
    SegmentStringChanged := True;
    try
      S := UpperCase(edSegmentString.Text);
      if Pos('P', S) > 0 then
      begin
        if Pos('DP', S) = 0 then
          S := StringReplace(S, 'P', 'DP', [rfReplaceAll, rfIgnoreCase]);
      end;
      sldTest.Digits[0].Segments := S;
      edSegmentString.Text := sldTest.Digits[0].Segments;
      if not CheckMarkClicked then
        UpdateCheckMarks;
      if not CharChanged and (not CheckMarkClicked or not CheckMarkChangedIsDP) then
        edCharacter.Text := '';
    finally
      SegmentStringChanged := False;
    end;
  end;
end;

procedure Tfme7SegExamples.edSpeedChange(Sender: TObject);
var
  kmperhr: Double;
begin
  kmperhr := StrToFloat(edSpeed.Text);
  Incs := kmperhr / 3600 {per secs} / 5 {per .2 seconds}
end;

procedure Tfme7SegExamples.btnResetTripClick(Sender: TObject);
begin
  TripOdo := 0;
end;

end.
