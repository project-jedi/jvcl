unit MainBuild;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvLEDDisplays;

type
  TForm4 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
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
  clOrangeOff = $00004264;
  clLimeOff = $00006400;
  clRedOff = $00000064;
  
procedure TForm4.FormCreate(Sender: TObject);
begin
  FLEDDisplay := TJvSegmentLEDDisplay.Create(Self);
  with FLEDDisplay do
  begin
    DigitHeight := 40;
    DigitWidth := 24;
    SegmentWidth := 2;
    DigitCount := 5;
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
    DigitHeight := 88;
    DigitWidth := 48;
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
    DigitHeight := 88;
    DigitWidth := 48;
    SegmentWidth := 2;
    Kind := slk16;
    Color := clBlack;
    ColorOn := clLime;
    ColorOff := clLimeOff;
    Top := FLEDDisplay16.Top + FLEDDisplay16.Height;// + 16;
    Parent := Self;
    DisplayString('ABCDEFGHIJKLMNOP', True);
  end;

  FLEDDisplay16_3 := TJvSegmentLEDDisplay.Create(Self);
  with FLEDDisplay16_3 do
  begin
    DigitHeight := 88;
    DigitWidth := 48;
    SegmentWidth := 2;
    Kind := slk16;
    Color := clBlack;
    ColorOn := clLime;
    ColorOff := clLimeOff;
    Top := FLEDDisplay16_2.Top + FLEDDisplay16_2.Height;// + 16;
    Parent := Self;
    DisplayString('QRSTUVWXYZ +-()*'{#248'+*()/'}, True);
  end;

  FLEDDisplayJVCL := TJvSegmentLEDDisplay.Create(Self);
  with FLEDDisplayJVCL do
  begin
    DigitHeight := 196;
    DigitWidth := 96;
    SegmentWidth := 4;
    Kind := slk14;
    Color := clBlack;
    ColorOn := clOrange;
    ColorOff := clOrangeOff;
    Top := FLEDDisplay16_3.Top + FLEDDisplay16_3.Height + 8;
    Parent := Self;
    DisplayString('JEDI-VCL', True);
  end;
end;

end.
