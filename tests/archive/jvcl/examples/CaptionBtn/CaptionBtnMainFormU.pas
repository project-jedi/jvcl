unit CaptionBtnMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,JvCaptionButton, ExtCtrls, JvComponent;

type
  TCaptionBtnMainForm = class(TForm)
    CaptionButton1: TJvCaptionButton;
    procedure CaptionButton1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CaptionButton1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClick(Sender: TObject);
    procedure CaptionButton1Click(Sender: TObject);
  end;

var
  CaptionBtnMainForm: TCaptionBtnMainForm;

implementation

{$R *.DFM}

function BoolToString(Value:boolean):String;
const
  cBoolString:array[boolean] of shortstring = ('no','yes');
begin
  Result := cBoolString[Value];
end;

procedure TCaptionBtnMainForm.CaptionButton1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Caption := 'MouseDown: ' + BoolToString(CaptionButton1.Down);;
{  if CaptionButton1.Down then
    Screen.Cursor := crHelp
  else
    Screen.Cursor := crDefault; }
end;

procedure TCaptionBtnMainForm.CaptionButton1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Caption := 'MouseUp: ' + BoolToString(CaptionButton1.Down);;
{  if CaptionButton1.Down then
    Screen.Cursor := crHelp
  else
    Screen.Cursor := crDefault; }
end;

procedure TCaptionBtnMainForm.FormClick(Sender: TObject);
begin
  Caption := 'FormClick: ' + BoolToString(CaptionButton1.Down);
{  if CaptionButton1.Down then
    Screen.Cursor := crDefault
  else
    Screen.Cursor := crHelp; }
end;

procedure TCaptionBtnMainForm.CaptionButton1Click(Sender: TObject);
begin
  Caption  := 'Click!';
end;

end.
