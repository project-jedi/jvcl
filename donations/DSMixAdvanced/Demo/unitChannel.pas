unit unitChannel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, cbDSmixer, Gauges, XPMan;

const
  GRAPH_WIDTH = 110;

type
  TAnalysis = class
  private
    FChannelCount,
    FSamplesPerPixel,
    FSampleCount,
    FGraphWidth: Integer;
    FAddOn: Byte;
    FGraphData: array [0..1] of array of array of byte;
    FVUData: array [0..1] of Single;
    FChannel: TcbDSMixerChannel;

  public
    constructor Create(Channel: TcbDSMixerChannel; SampleCount,
      GraphWidth: Integer);
    procedure CalcGraph;
    procedure Clear;
  end;

  TFormChannel = class(TForm)
    Panel2: TPanel;
    Label14: TLabel;
    edtFileName: TEdit;
    btnBrowse: TButton;
    pnlControl: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label1: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    buttonPlay: TButton;
    buttonStop: TButton;
    trackVolume: TTrackBar;
    editFrom: TEdit;
    editTo: TEdit;
    checkLoop: TCheckBox;
    buttonFade: TButton;
    buttonPause: TButton;
    trackPan: TTrackBar;
    Panel1: TPanel;
    paintboxLeft: TPaintBox;
    paintboxRight: TPaintBox;
    Label9: TLabel;
    Label10: TLabel;
    trackFrequency: TTrackBar;
    pbPosition: TProgressBar;
    Timer1: TTimer;
    OpenDialog1: TOpenDialog;
    pbarLeft: TProgressBar;
    pbarRight: TProgressBar;
    GroupBox1: TGroupBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    Button1: TButton;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    wdmix: TScrollBar;
    Button2: TButton;
    XPManifest1: TXPManifest;
    Label15: TLabel;
    Label16: TLabel;
    Feedback: TScrollBar;
    Label17: TLabel;
    ldelay: TScrollBar;
    Rdelay: TScrollBar;
    Label18: TLabel;
    Label19: TLabel;
    pdelay: TScrollBar;
    Label20: TLabel;
    wdmixchor: TScrollBar;
    Label21: TLabel;
    ScrollBar2: TScrollBar;
    Label22: TLabel;
    ScrollBar3: TScrollBar;
    Label23: TLabel;
    ScrollBar4: TScrollBar;
    Label24: TLabel;
    ScrollBar5: TScrollBar;
    Button3: TButton;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    procedure buttonPlayClick(Sender: TObject);
    procedure buttonStopClick(Sender: TObject);
    procedure buttonFadeClick(Sender: TObject);
    procedure checkLoopClick(Sender: TObject);
    procedure trackVolumeChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure buttonPauseClick(Sender: TObject);
    procedure editFromExit(Sender: TObject);
    procedure trackPanChange(Sender: TObject);
    procedure paintboxLeftPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure paintboxRightPaint(Sender: TObject);
    procedure trackFrequencyChange(Sender: TObject);
    procedure pbPositionMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure editToExit(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure wdmixChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    FWB: TcbDSMixerChannel;
    FDSM: TcbDSMixer;
    FDSFXManager: TDSFXManager;
    FAnalysis: TAnalysis;

    procedure SetWB( Value: TcbDSMixerChannel );
  public
    constructor Create(Sender: TComponent; DSM: TcbDSMixer); reintroduce; virtual;
    procedure OnEndFade(Sender: TObject);
    procedure OnEndPlay(Sender: TObject);

    property wb: TcbDSMixerChannel read FWB write SetWB;
  end;

var
  FormChannel: TFormChannel;

implementation

uses unitDSMixerTest;

{$R *.DFM}

constructor TFormChannel.Create(Sender: TComponent; DSM: TcbDSMixer);
begin
  FDSM := DSM;
  inherited Create(Sender);
end;

procedure TFormChannel.OnEndFade(Sender: TObject);
begin
  FWB.Stop;
end;

procedure TFormChannel.OnEndPlay(Sender: TObject);
begin
  Timer1.Enabled := false;
  FAnalysis.Clear;
  paintboxLeft.Invalidate;
  paintboxRight.Invalidate;
  pbPosition.Position := 0;
end;

procedure TFormChannel.buttonPlayClick(Sender: TObject);
begin
  FWB.Play;
  trackFrequency.Position := FWB.Frequency;
  timer1.Enabled := true;
end;


procedure TFormChannel.buttonStopClick(Sender: TObject);
begin
  timer1.Enabled := false;
  FWB.Stop;
end;

procedure TFormChannel.buttonFadeClick(Sender: TObject);
begin
  FWB.Fade(5000, -5000);
end;

procedure TFormChannel.checkLoopClick(Sender: TObject);
begin
  FWB.Loop := checkLoop.Checked;
end;

procedure TFormChannel.trackVolumeChange(Sender: TObject);
begin
  FWB.Volume := -10000-trackVolume.Position;
end;

procedure TFormChannel.Timer1Timer(Sender: TObject);
begin
  if not(FWB.Status = sPlaying) then
    exit;

  FAnalysis.CalcGraph;

  paintboxLeft.Invalidate;
  paintboxRight.Invalidate;

  trackVolume.Position := -FWB.Volume-10000;
  pbarLeft.Position := Trunc(FAnalysis.FVUData[0]*100);
  pbarRight.Position := Trunc(FAnalysis.FVUData[1]*100);

  pbPosition.Position := (FWB.Position * 100) div FWB.FileDuration;
end;


procedure TFormChannel.SetWB( Value: TcbDSMixerChannel );
begin
  FWB := Value;
  Caption := FWB.FileName;
end;

procedure TFormChannel.buttonPauseClick(Sender: TObject);
begin
  FWB.Pause;
  Timer1.Enabled := False;
end;

procedure TFormChannel.editFromExit(Sender: TObject);
begin
  try
    FWB.RangeStart := StrToInt(editFrom.Text);
  except
    on Exception do;
  end;
end;

procedure TFormChannel.trackPanChange(Sender: TObject);
begin
  FWB.Pan := trackPan.Position;
end;

procedure TFormChannel.FormCreate(Sender: TObject);
begin
  if not Assigned(FWB) then
  begin
    FWB := TcbDSMixerChannel.Create(FDSM);

    FWB.OnEndFade := OnEndFade;
    FWB.OnEndPlay := OnEndPlay;
  end;
FDSFXManager:=TDSFXManager.Create(Self);
paintboxLeft.Canvas.Brush.Style:=bsclear;
paintboxright.Canvas.Brush.Style:=bsclear;
end;

procedure TFormChannel.paintboxLeftPaint(Sender: TObject);
var
  Cnt: Integer;
begin
  if Assigned(FAnalysis) then
    for Cnt := 0 to FAnalysis.FGraphWidth-1 do
    begin
      paintboxleft.Canvas.Pen.Color:=clGray;
      paintboxLeft.Canvas.MoveTo(Cnt, (255-FAnalysis.FGraphData[0, Cnt, 0]) shr 2 );
      paintboxLeft.Canvas.LineTo(Cnt, (255-FAnalysis.FGraphData[1, Cnt, 0]) shr 2 );
      paintboxleft.Canvas.Pen.Color:=clwhite;
      paintboxLeft.Canvas.MoveTo(Cnt-1, (255-FAnalysis.FGraphData[0, Cnt, 0]) shr 2-1 );
      paintboxLeft.Canvas.LineTo(Cnt-1, (255-FAnalysis.FGraphData[1, Cnt, 0]) shr 2-1 );
      paintboxleft.Canvas.Pen.Color:=clblack;
      paintboxLeft.Canvas.MoveTo(Cnt+1, (255-FAnalysis.FGraphData[0, Cnt, 0]) shr 2 +1);
      paintboxLeft.Canvas.LineTo(Cnt+1, (255-FAnalysis.FGraphData[1, Cnt, 0]) shr 2 +1);
    end;
end;

procedure TFormChannel.paintboxRightPaint(Sender: TObject);
var
  Cnt: Integer;
begin
  if Assigned(FAnalysis) and (FAnalysis.FChannelCount > 1) then
    for Cnt := 0 to FAnalysis.FGraphWidth-1 do
    begin
      paintboxRight.Canvas.Pen.Color:=clblue;
      paintboxRight.Canvas.MoveTo(Cnt-1, (255-FAnalysis.FGraphData[0, Cnt, 1]) shr 2 );
      paintboxRight.Canvas.LineTo(Cnt-1, (255-FAnalysis.FGraphData [1, Cnt, 1]) shr 2 );
      paintboxRight.Canvas.Pen.Color:=clblack;
      paintboxRight.Canvas.MoveTo(Cnt, (255-FAnalysis.FGraphData[0, Cnt, 1]) shr 2 +1);
      paintboxRight.Canvas.LineTo(Cnt, (255-FAnalysis.FGraphData [1, Cnt, 1]) shr 2 +1);
    end;
end;

procedure TFormChannel.trackFrequencyChange(Sender: TObject);
begin
  FWB.Frequency := trackFrequency.Position;
end;

procedure TFormChannel.pbPositionMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FWB.Pause;
  FWB.Position := (X * Integer(FWB.FileDuration)) div pbPosition.ClientWidth;
  pbPosition.Position := (FWB.Position * 100) div FWB.FileDuration;
  FWB.Play;
end;

procedure TFormChannel.FormDestroy(Sender: TObject);
begin
  timer1.Enabled := False;
  FreeAndNil(FAnalysis);
  FreeAndNil(FDSFXManager);
  FreeAndNil(FWB);
  formdsmixer.UpdateStatus;
end;

procedure TFormChannel.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFormChannel.editToExit(Sender: TObject);
begin
  try
    FWB.RangeEnd := StrToInt(editTo.Text);
  except
    on Exception do;
  end;
end;

procedure TFormChannel.btnBrowseClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    edtFileName.Text := '';
    pnlControl.Enabled := False;
    FWB.FileName := '';
    FreeAndNil(FAnalysis);

    FWB.FileName := OpenDialog1.FileName;
    edtFileName.Text := FWB.FileName;
    pnlControl.Enabled := True;
    FAnalysis := TAnalysis.Create(FWB, (FWB.FileSamplesPerSec *
      Timer1.Interval) div 1000, GRAPH_WIDTH);
    fwb.Play;
    fwb.Stop;
    FDSFXManager.DSBuffer:=fwb.DSBuffer;
    FDSFXManager.LoadDefaultParameters;
  end;
end;


constructor TAnalysis.Create(Channel: TcbDSMixerChannel; SampleCount,
  GraphWidth: Integer);
begin
  inherited Create;

  FChannel := Channel;
  FGraphWidth := GraphWidth;
  FSampleCount := SampleCount;

  if (FChannel.FileNumChannels > 1) then
    FChannelCount := 2
  else
    FChannelCount := 1;

  FSamplesPerPixel := SampleCount div GRAPH_WIDTH;
  if (FSamplesPerPixel = 0) then
    FSamplesPerPixel := 1;

  if (FChannel.FileBitsPerSample = 8) then
    FAddOn := 0
  else
    FAddOn := 128;

  SetLength(FGraphData[0], FGraphWidth, FChannelCount);
  SetLength(FGraphData[1], FGraphWidth, FChannelCount);
end;

procedure TAnalysis.CalcGraph;
var
  GraphPoint,
  SamplePoint: Integer;
  Size1,
  Size2,
  CntCh: DWord;
  PointUpper,
  PointLower,
  MinValue,
  MaxValue: Byte;
  Audio1Ptr,
  Audio2Ptr: Pointer;

  procedure DoBufCalc(AudioPtr: Pointer; SamplesCount, Channel: Integer);
  var
    V: Byte;
  begin
    while (GraphPoint < FGraphWidth) do
    begin
      while (SamplePoint <= FSamplesPerPixel) do
      begin
        if (SamplesCount = 0) then
          exit;

        v := ShortInt(AudioPtr^) + FAddOn;
        if v > PointUpper then PointUpper := v;
        if v < PointLower then PointLower := v;
        Inc(SamplePoint);
        Inc(PByte(AudioPtr), FChannel.FileBlockAlign);
        Dec(SamplesCount);
      end;

      if (PointUpper > MaxValue) then
        MaxValue := PointUpper;
      if (PointLower < MinValue) then
        MinValue := PointLower;

      FGraphData[0, GraphPoint, Channel] := PointUpper;
      FGraphData[1, GraphPoint, Channel] := PointLower;
      PointUpper := 0;
      PointLower := 255;
      Inc(GraphPoint);
      SamplePoint := 1;
    end;
  end;

begin
  FChannel.Lock(0, FSampleCount*Integer(FChannel.FileBlockAlign),
    Audio1Ptr, Size1, Audio2Ptr, Size2);
  try
    for CntCh := 0 to FChannelCount-1 do
    begin
      MinValue := 255;
      MaxValue := 0;
      PointUpper := 0;
      PointLower := 255;
      GraphPoint := 0;
      SamplePoint := 1;
      if (Size1 > 0) then
        DoBufCalc(Pointer(PChar(Audio1Ptr) + FChannel.FileBlockAlign - 1 -
          (FChannel.FileBitsPerSample div 8) * CntCh),
          Size1 div FChannel.FileBlockAlign,
          CntCh);
      if (Size2 > 0) then
        DoBufCalc(Pointer(PChar(Audio2Ptr) + FChannel.FileBlockAlign - 1 -
          (FChannel.FileBitsPerSample div 8) * CntCh),
          Size2 div FChannel.FileBlockAlign,
          CntCh);

      if Abs(MinValue-128) > Abs(MaxValue-128) then
        FVUData[CntCh] := Sqrt(Abs(MinValue-128) / 128)
      else
        FVUData[CntCh] := Sqrt(Abs(MaxValue-128) / 128);
    end;
  finally
    FChannel.Unlock;
  end;
end;

procedure TAnalysis.Clear;
var
  Cnt,
  CntC: Integer;
begin
  for Cnt := 0 to FGraphWidth-1 do
    for CntC := 0 to FChannelCount-1 do
    begin
      FGraphData[0, Cnt, CntC] := 128;
      FGraphData[1, Cnt, CntC] := 128;
    end;
end;

procedure TFormChannel.Button1Click(Sender: TObject);
begin
fwb.Pause;
FDSFXManager.DisableAllFX;
if checkbox1.checked then FDSFXManager.EnableEffect(eSFX_echo);
if checkbox2.checked then FDSFXManager.EnableEffect(eSFX_chorus);
if checkbox3.checked then FDSFXManager.EnableEffect(eSFX_flanger);
if checkbox4.checked then FDSFXManager.EnableEffect(eSFX_gargle);
if checkbox5.checked then FDSFXManager.EnableEffect(eSFX_reverb);
if checkbox6.checked then FDSFXManager.EnableEffect(eSFX_parameq);
if checkbox6.checked then FDSFXManager.EnableEffect(eSFX_compressor);
FDSFXManager.ActivateFX;
fwb.Play;
end;

procedure TFormChannel.wdmixChange(Sender: TObject);
begin
FDSFXManager.SetCurrentParameters(eSFX_echo);
end;

procedure TFormChannel.Button2Click(Sender: TObject);
begin
FDSFXManager.Echo.fWetDryMix:=wdmix.Position;
FDSFXManager.Echo.fFeedback:=Feedback.Position;
FDSFXManager.Echo.fLeftDelay:=ldelay.Position;
FDSFXManager.Echo.fRightDelay:=Rdelay.Position;
FDSFXManager.Echo.lPanDelay:=pdelay.Position;
FDSFXManager.SetCurrentParameters(eSFX_echo);
end;

procedure TFormChannel.Button3Click(Sender: TObject);
begin
FDSFXManager.Chorus.fWetDryMix:=wdmixchor.Position;
FDSFXManager.Chorus.fDepth:=ScrollBar2.Position;
FDSFXManager.Chorus.fFeedback:=ScrollBar3.Position;
FDSFXManager.Chorus.fFrequency:=ScrollBar4.Position;
FDSFXManager.Chorus.fDelay:=ScrollBar4.Position;
end;

end.
