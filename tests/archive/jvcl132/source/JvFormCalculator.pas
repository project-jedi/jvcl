{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFormCalculator.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvFormCalculator;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls,
  JvSpeedButton, JvEdit;

type
  TOperation = (opNothing, opPlus, opMinus, opMul, opDiv);

  TFormCalc = class(TForm)
    BUSpeedButton1: TJvSpeedButton;
    BUSpeedButton2: TJvSpeedButton;
    BUSpeedButton3: TJvSpeedButton;
    BUSpeedButton4: TJvSpeedButton;
    BUSpeedButton5: TJvSpeedButton;
    BUSpeedButton6: TJvSpeedButton;
    BUSpeedButton7: TJvSpeedButton;
    BUSpeedButton8: TJvSpeedButton;
    BUSpeedButton9: TJvSpeedButton;
    BUSpeedButton10: TJvSpeedButton;
    BUSpeedButton11: TJvSpeedButton;
    BUSpeedButton12: TJvSpeedButton;
    BUSpeedButton13: TJvSpeedButton;
    BUSpeedButton14: TJvSpeedButton;
    BUSpeedButton15: TJvSpeedButton;
    BUSpeedButton16: TJvSpeedButton;
    BUSpeedButton17: TJvSpeedButton;
    BUSpeedButton18: TJvSpeedButton;
    BUSpeedButton19: TJvSpeedButton;
    BUSpeedButton20: TJvSpeedButton;
    BUSpeedButton21: TJvSpeedButton;
    Label1: TLabel;
    BUSpeedButton22: TJvSpeedButton;
    BUSpeedButton23: TJvSpeedButton;
    BUSpeedButton24: TJvSpeedButton;
    Edit1: TJvEdit;
    procedure BUSpeedButton1Click(Sender: TObject);
    procedure BUSpeedButton11Click(Sender: TObject);
    procedure BUSpeedButton17Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BUSpeedButton14Click(Sender: TObject);
    procedure BUSpeedButton13Click(Sender: TObject);
    procedure BUSpeedButton16Click(Sender: TObject);
    procedure BUSpeedButton15Click(Sender: TObject);
    procedure BUSpeedButton12Click(Sender: TObject);
    procedure BUSpeedButton19Click(Sender: TObject);
    procedure BUSpeedButton20Click(Sender: TObject);
    procedure BUSpeedButton21Click(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    function Calc: Boolean;
    procedure BUSpeedButton22Click(Sender: TObject);
    procedure BUSpeedButton23Click(Sender: TObject);
    procedure BUSpeedButton24Click(Sender: TObject);
    function GetResult: Extended;
    procedure SetResult(Value: Extended);
  private
    FVal: Extended;
    FMval: Extended;
    FOp: TOperation;
  public
  end;

var
  FormCalc: TFormCalc;

implementation

{$R *.DFM}

{****************************************************}

function TFormCalc.GetResult: Extended;
begin
  if Edit1.Text <> '' then
    Result := StrToFloat(Edit1.Text)
  else
    Result := 0;
end;

{****************************************************}

procedure TFormCalc.SetResult(Value: Extended);
begin
  Edit1.Text := FloatToStr(Value);
end;

{****************************************************}

function TFormCalc.Calc: Boolean;
begin
  Result := True;
  if (Edit1.Text <> '') and (Edit1.Alignment = taLeftJustify) then
  begin
    case FOp of
      opPlus:
        Edit1.Text := FloatToStr(FVal + StrToFloat(Edit1.Text));
      opMinus:
        Edit1.Text := FloatToStr(FVal - StrToFloat(Edit1.Text));
      opMul:
        Edit1.Text := FloatToStr(FVal * StrToFloat(Edit1.Text));
      opDiv:
        if StrToFloat(Edit1.Text) <> 0.0 then
          Edit1.Text := FloatToStr(FVal / StrToFloat(Edit1.Text))
        else
        begin
          ShowMessage('Division by Zero!');
          Result := False;
        end;
    end;
  end
  else
    Result := False;
end;

{****************************************************}

procedure TFormCalc.BUSpeedButton1Click(Sender: TObject);
begin
  if Edit1.Alignment = taRightJustify then
  begin
    Edit1.Text := '';
    Edit1.Alignment := taLeftJustify;
  end;
  if Edit1.Text = '0' then
    Edit1.Text := '';
  Edit1.Text := Edit1.Text + (Sender as TJvSpeedButton).Caption;
end;

{****************************************************}

procedure TFormCalc.BUSpeedButton11Click(Sender: TObject);
begin
  Edit1.Clear;
  FVal := 0;
end;

{****************************************************}

procedure TFormCalc.BUSpeedButton17Click(Sender: TObject);
begin
  if Calc then
  begin
    Label1.Caption := '=';
    FVal := StrToFloat(Edit1.Text);
    FOp := opNothing;
    Edit1.Alignment := taRightJustify;
  end;
end;

{****************************************************}

procedure TFormCalc.FormCreate(Sender: TObject);
begin
  FVal := 0;
  FMVal := 0;
  FOp := opNothing;
end;

{****************************************************}

procedure TFormCalc.BUSpeedButton14Click(Sender: TObject);
begin
  if Calc then
  begin
    FVal := StrToFloat(Edit1.Text);
    Edit1.Text := FloatToStr(FVal);
    Edit1.Alignment := taRightJustify;
  end;
  FOp := opPlus;
  Label1.Caption := '+';
end;

{****************************************************}

procedure TFormCalc.BUSpeedButton13Click(Sender: TObject);
begin
  if Calc then
  begin
    Calc;
    FVal := StrToFloat(Edit1.Text);
    Edit1.Text := FloatToStr(FVal);
    Edit1.Alignment := taRightJustify;
  end;
  FOp := opMul;
  Label1.Caption := '*';
end;

{****************************************************}

procedure TFormCalc.BUSpeedButton16Click(Sender: TObject);
begin
  if Calc then
  begin
    FVal := StrToFloat(Edit1.Text);
    Edit1.Text := FloatToStr(FVal);
    Edit1.Alignment := taRightJustify;
  end;
  FOp := opMinus;
  Label1.Caption := '-';
end;

{****************************************************}

procedure TFormCalc.BUSpeedButton15Click(Sender: TObject);
begin
  if calc then
  begin
    calc;
    FVal := StrToFloat(Edit1.Text);
    Edit1.Text := FloatToStr(FVal);
    Edit1.Alignment := taRightJustify;
  end;
  FOp := opDiv;
  Label1.Caption := '/';
end;

{****************************************************}

procedure TFormCalc.BUSpeedButton12Click(Sender: TObject);
begin
  if Edit1.Alignment = taLeftJustify then
  begin
    try
      StrToFloat(Edit1.Text + ',0');
      Edit1.Text := Edit1.Text + ',';
    except
      Edit1.Text := Edit1.Text + '.';
    end;
  end
  else
    Beep;
end;

{****************************************************}

procedure TFormCalc.BUSpeedButton19Click(Sender: TObject);
begin
  try
    FMVal := StrToFloat(Edit1.Text);
  except
  end;
end;

{****************************************************}

procedure TFormCalc.BUSpeedButton20Click(Sender: TObject);
begin
  Edit1.Alignment := taLeftJustify;
  Edit1.Text := FloatToStr(FMVal);
end;

{****************************************************}

procedure TFormCalc.BUSpeedButton21Click(Sender: TObject);
begin
  Edit1.Clear;
  Edit1.Alignment := taLeftJustify;
end;

{****************************************************}

procedure TFormCalc.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    '0'..'9':
      begin
        if Edit1.Alignment = taRightJustify then
        begin
          Edit1.Text := '';
          Edit1.Alignment := taLeftJustify;
        end;
        if Edit1.Text = '0' then
          Edit1.Text := '';
        Edit1.Text := Edit1.Text + Key;
      end;
    '+':
      BUSpeedButton14click(Self);
    '-':
      BUSpeedButton16click(Self);
    '*':
      BUSpeedButton13click(Self);
    '/':
      BUSpeedButton15click(Self);
    '=', #13:
      BUSpeedButton17click(Self);
  else
    Beep;
  end;
end;

{****************************************************}

procedure TFormCalc.BUSpeedButton22Click(Sender: TObject);
begin
  if (Edit1.Text <> '') and (Edit1.Alignment = taLeftJustify) then
    Edit1.Text := FloatToStr(-StrToFloat(Edit1.Text))
  else
    Beep;
end;

{****************************************************}

procedure TFormCalc.BUSpeedButton23Click(Sender: TObject);
begin
  if Edit1.Alignment = taRightJustify then
  begin
    Edit1.Text := '';
    Edit1.Alignment := taLeftJustify;
  end;
  Edit1.Text := FloatToStr(Pi);
end;

{****************************************************}

procedure TFormCalc.BUSpeedButton24Click(Sender: TObject);
begin
  if Edit1.Alignment = taRightJustify then
  begin
    Edit1.Text := '';
    Edit1.Alignment := taLeftJustify;
  end;
  try
    Edit1.Text := IntToStr(Trunc(StrToFloat(Edit1.Text)));
  except
    Edit1.Text := '';
    Beep;
  end;
end;

end.
