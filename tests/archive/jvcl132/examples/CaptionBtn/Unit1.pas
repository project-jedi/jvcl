unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,JvCaptionButton, ExtCtrls, JvComponent;

type
  TForm1 = class(TForm)
    CaptionButton1: TJvCaptionButton;
    procedure CaptionButton1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CaptionButton1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClick(Sender: TObject);
    procedure CaptionButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

function BoolToString(Value:boolean):String;
const
  cBoolString:array[boolean] of shortstring = ('no','yes');
begin
  Result := cBoolString[Value];
end;


procedure TForm1.CaptionButton1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Caption := 'MouseDown: ' + BoolToString(CaptionButton1.Down);;
{  if CaptionButton1.Down then
    Screen.Cursor := crHelp
  else
    Screen.Cursor := crDefault; }
end;

procedure TForm1.CaptionButton1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Caption := 'MouseUp: ' + BoolToString(CaptionButton1.Down);;
{  if CaptionButton1.Down then
    Screen.Cursor := crHelp
  else
    Screen.Cursor := crDefault; }
end;

procedure TForm1.FormClick(Sender: TObject);
begin
  Caption := 'FormClick: ' + BoolToString(CaptionButton1.Down);
{  if CaptionButton1.Down then
    Screen.Cursor := crDefault
  else
    Screen.Cursor := crHelp; }
end;

procedure TForm1.CaptionButton1Click(Sender: TObject);
begin
  Caption  := 'Click!';
end;

end.
