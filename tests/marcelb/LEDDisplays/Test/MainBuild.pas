unit MainBuild;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvLEDDisplays, ExtCtrls;

type
  TForm4 = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    JVCLOn: Boolean;
    FLEDDisplay: TJvSegmentLEDDisplay;
    FLEDDisplay16: TJvSegmentLEDDisplay;
    FLEDDisplay16_2: TJvSegmentLEDDisplay;
    FLEDDisplay16_3: TJvSegmentLEDDisplay;
    FLEDDisplayJVCL: TJvSegmentLEDDisplay;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.DFM}

const
  clOrange = $0000A0FF;
  clOrangeOff = $0000415A;
  clLimeOff = $00004F00;
  clRedOff = $00000064;

procedure TForm4.FormCreate(Sender: TObject);
begin
  FLEDDisplay := TJvSegmentLEDDisplay.Create(Self);
  with FLEDDisplay do
  begin
    DigitHeight := 32;
    DigitWidth := 20;
    SegmentWidth := 2;
    Color := clBlack;
    ColorOn := clRed;
    ColorOff := clRedOff;
    Parent := Self;
    DisplayString('-0123456789ABCDEFc HELP'#248'''" Error', True);
    Digit[DigitCount - 1].UseDP := True;
    Digit[DigitCount - 1].SegmentState[7] := True;
  end;

  FLEDDisplay16 := TJvSegmentLEDDisplay.Create(Self);
  with FLEDDisplay16 do
  begin
    DigitHeight := 64;
    DigitWidth := 40;
    SegmentWidth := 2;
    Kind := slk16;
    Color := clBlack;
    ColorOn := clLime;
    ColorOff := clLimeOff;
    Top := FLEDDisplay.Top + FLEDDisplay.Height + 8;
    Parent := Self;
    DisplayString('0123456789 '#0#4''#248'''"', True);
  end;

  FLEDDisplay16_2 := TJvSegmentLEDDisplay.Create(Self);
  with FLEDDisplay16_2 do
  begin
    DigitHeight := 64;
    DigitWidth := 40;
    SegmentWidth := 2;
    Kind := slk16;
    Color := clBlack;
    ColorOn := clLime;
    ColorOff := clLimeOff;
    Top := FLEDDisplay16.Top + FLEDDisplay16.Height;
    Parent := Self;
    DisplayString('ABCDEFGHIJKLMNOP', True);
  end;

  FLEDDisplay16_3 := TJvSegmentLEDDisplay.Create(Self);
  with FLEDDisplay16_3 do
  begin
    DigitHeight := 64;
    DigitWidth := 40;
    SegmentWidth := 2;
    Kind := slk16;
    Color := clBlack;
    ColorOn := clLime;
    ColorOff := clLimeOff;
    Top := FLEDDisplay16_2.Top + FLEDDisplay16_2.Height;
    Parent := Self;
    DisplayString('QRSTUVWXYZ +-()*', True);
  end;

  FLEDDisplayJVCL := TJvSegmentLEDDisplay.Create(Self);
  with FLEDDisplayJVCL do
  begin
    DigitHeight := 88;
    DigitWidth := 64;
    SegmentWidth := 4;
    Kind := slk14;
    Color := clBlack;
    ColorOn := clOrange;
    ColorOff := clOrangeOff;
    Top := FLEDDisplay16_3.Top + FLEDDisplay16_3.Height + 8;
    Parent := Self;
    DisplayString(' JEDI-VCL ', True);
    SlantAngle := 10;
    JVCLOn := True;
  end;
end;

procedure TForm4.Timer1Timer(Sender: TObject);
begin
  if JVCLOn then
  begin
    FLEDDisplayJVCL.DisplayString('');
    JVCLOn := False;
  end
  else
  begin
    FLEDDisplayJVCL.DisplayString(' JEDI-VCL');
    JVCLOn := True;
  end
end;

end.
