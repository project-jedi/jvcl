unit MainBuild;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, JvLEDDisplays;

type
  TForm4 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FLEDDisplay: TJvSegmentLEDDisplay;
    FLEDDisplay16: TJvSegmentLEDDisplay;
    FLEDDisplay16_2: TJvSegmentLEDDisplay;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.DFM}

const
  clLimeOff = $00006400;
  clRedOff = $00000064;
  
procedure TForm4.FormCreate(Sender: TObject);
begin
  FLEDDisplay := TJvSegmentLEDDisplay.Create(Self);
  FLEDDisplay.DigitHeight := 80;
  FLEDDisplay.DigitWidth := 48;
  FLEDDisplay.SegmentWidth := 4;
  FLEDDisplay.DigitCount := 5;
  FLEDDisplay.Color := clBlack;
  FLEDDisplay.ColorOn := clRed;
  FLEDDisplay.ColorOff := clRedOff;
  FLEDDisplay.Parent := Self;
  FLEDDisplay.Digit[0].SetChar('E');
  FLEDDisplay.Digit[1].SetChar('r');
  FLEDDisplay.Digit[2].SetChar('r');
  FLEDDisplay.Digit[3].SetChar('o');
  FLEDDisplay.Digit[4].SetChar('r');
  FLEDDisplay.Digit[4].UseDP := True;
  FLEDDisplay.Digit[4].SegmentState[7] := True;

  FLEDDisplay16 := TJvSegmentLEDDisplay.Create(Self);
  FLEDDisplay16.DigitHeight := 88;
  FLEDDisplay16.DigitWidth := 48;
  FLEDDisplay16.SegmentWidth := 4;
  FLEDDisplay16.Kind := slk14;
  FLEDDisplay16.DigitCount := 13;
  FLEDDisplay16.Color := clBlack;
  FLEDDisplay16.ColorOn := clLime;
  FLEDDisplay16.ColorOff := clLimeOff;
  FLEDDisplay16.Top := FLEDDisplay.Top + FLEDDisplay.Height + 16;
  FLEDDisplay16.Parent := Self;
  FLEDDisplay16.Digit[0].SetChar('A');
  FLEDDisplay16.Digit[1].SetChar('B');
  FLEDDisplay16.Digit[2].SetChar('C');
  FLEDDisplay16.Digit[3].SetChar('D');
  FLEDDisplay16.Digit[4].SetChar('E');
  FLEDDisplay16.Digit[5].SetChar('F');
  FLEDDisplay16.Digit[6].SetChar('G');
  FLEDDisplay16.Digit[7].SetChar('H');
  FLEDDisplay16.Digit[8].SetChar('I');
  FLEDDisplay16.Digit[9].SetChar('J');
  FLEDDisplay16.Digit[10].SetChar('K');
  FLEDDisplay16.Digit[11].SetChar('L');
  FLEDDisplay16.Digit[12].SetChar('M');

  FLEDDisplay16_2 := TJvSegmentLEDDisplay.Create(Self);
  FLEDDisplay16_2.DigitHeight := 88;
  FLEDDisplay16_2.DigitWidth := 48;
  FLEDDisplay16_2.SegmentWidth := 4;
  FLEDDisplay16_2.Kind := slk14;
  FLEDDisplay16_2.DigitCount := 13;
  FLEDDisplay16_2.Color := clBlack;
  FLEDDisplay16_2.ColorOn := clLime;
  FLEDDisplay16_2.ColorOff := clLimeOff;
  FLEDDisplay16_2.Top := FLEDDisplay16.Top + FLEDDisplay16.Height + 16;
  FLEDDisplay16_2.Parent := Self;
  FLEDDisplay16_2.Digit[0].SetChar('N');
  FLEDDisplay16_2.Digit[1].SetChar('O');
  FLEDDisplay16_2.Digit[2].SetChar('P');
  FLEDDisplay16_2.Digit[3].SetChar('Q');
  FLEDDisplay16_2.Digit[4].SetChar('R');
  FLEDDisplay16_2.Digit[5].SetChar('S');
  FLEDDisplay16_2.Digit[6].SetChar('T');
  FLEDDisplay16_2.Digit[7].SetChar('U');
  FLEDDisplay16_2.Digit[8].SetChar('V');
  FLEDDisplay16_2.Digit[9].SetChar('W');
  FLEDDisplay16_2.Digit[10].SetChar('X');
  FLEDDisplay16_2.Digit[11].SetChar('Y');
  FLEDDisplay16_2.Digit[12].SetChar('Z');
end;

end.
