{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

unit LEDMain;

interface

uses
  QWindows, QMessages, SysUtils, Variants, Classes, Types, QGraphics, QControls, QForms,
  QDialogs, JvQExControls, JvQComponent, JvQLED, QStdCtrls;

type
  TLEDDemoMain = class(TForm)
    JvLED1: TJvLED;
    JvLED2: TJvLED;
    JvLED3: TJvLED;
    JvLED4: TJvLED;
    JvLED5: TJvLED;
    JvLED6: TJvLED;
    JvLED7: TJvLED;
    JvLED8: TJvLED;
    JvLED9: TJvLED;
    JvLED10: TJvLED;
    JvLED11: TJvLED;
    JvLED12: TJvLED;
    JvLED13: TJvLED;
    JvLED14: TJvLED;
    JvLED15: TJvLED;
    JvLED16: TJvLED;
    JvLED17: TJvLED;
    JvLED18: TJvLED;
    JvLED19: TJvLED;
    JvLED20: TJvLED;
    JvLED21: TJvLED;
    JvLED22: TJvLED;
    JvLED23: TJvLED;
    JvLED24: TJvLED;
    JvLED25: TJvLED;
    JvLED26: TJvLED;
    JvLED27: TJvLED;
    JvLED28: TJvLED;
    JvLED29: TJvLED;
    JvLED30: TJvLED;
    JvLED31: TJvLED;
    JvLED32: TJvLED;
    JvLED33: TJvLED;
    JvLED34: TJvLED;
    JvLED35: TJvLED;
    JvLED36: TJvLED;
    JvLED37: TJvLED;
    JvLED38: TJvLED;
    JvLED39: TJvLED;
    JvLED40: TJvLED;
    JvLED41: TJvLED;
    JvLED42: TJvLED;
    JvLED43: TJvLED;
    JvLED44: TJvLED;
    Data7: TJvLED;
    Data6: TJvLED;
    Data5: TJvLED;
    Data4: TJvLED;
    Data3: TJvLED;
    Data2: TJvLED;
    Data1: TJvLED;
    Data0: TJvLED;
    HexText: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure HexTextChange(Sender: TObject);
  public
    Activated: Boolean;
    Changing: Boolean;
    HexValue: Byte;
    HexLEDs: array [0..7] of TJvLED;
  end;

var
  LEDDemoMain: TLEDDemoMain;

implementation

{$R *.xfm}

procedure TLEDDemoMain.FormActivate(Sender: TObject);
var
  I: Integer;
begin
  if not Activated then
  begin
    Activated := True;
    for I := Low(HexLEDs) to High(HexLEDs) do
      HexLEDs[I] := TJvLED(FindComponent(Format('Data%d', [I])));
  end;
end;

procedure TLEDDemoMain.HexTextChange(Sender: TObject);
var
  I: Integer;
begin
  if not Changing then
  begin
    try
      Changing := True;
      HexValue := StrToInt('$' + HexText.Text);
    except
      HexText.Text := '00';
      HexValue := 0;
    end;
    for I := Low(HexLEDs) to High(HexLEDs) do
      HexLEDs[I].Status := Odd(HexValue shr I);
    Changing := False;
  end;
end;

end.
