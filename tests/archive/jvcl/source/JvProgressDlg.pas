{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvProgressDlg.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvProgressDlg;

interface

uses
  SysUtils, Classes, Controls, Forms,
  JvFormProgress, JvTypes, JvComponent;

type
  TJvProgressDlg = class(TJvComponent)
  private
    FPForm: TFormProg;
    FValue: Integer;
    FMaximum: Integer;
    FText: string;
    FStayOnTop: Boolean;
    FAutoTimeLeft: Boolean;
    FLast: TDateTime;
    FLastVal: TDateTime;
    procedure SetMaximum(const Value: Integer);
    procedure SetValue(const Value: Integer);
    procedure SetText(const Value: string);
    procedure SetStayOnTop(const Value: Boolean);
    procedure ApplyText(Value: string);
    function GetShowing: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Showing: boolean read GetShowing;
  published
    property Maximum: Integer read FMaximum write SetMaximum default 100;
    property Value: Integer read FValue write SetValue default 0;
    property Text: string read FText write SetText;
    property StayOnTop: Boolean read FStayOnTop write SetStayOnTop default True;
    procedure Show;
    procedure Close;
    property AutoTimeLeft: Boolean read FAutoTimeLeft write FAutoTimeLeft default False;
    procedure StartProgression;
  end;

implementation

resourcestring
  RC_ProgressText = 'Progress';
  RC_EstimatedTime = 'Estimated time left : ';
  RC_Days = ' day(s) ';

constructor TJvProgressDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPForm := TFormProg.Create(Self);
  FText := RC_ProgressText;
  FMaximum := 100;
  FValue := 0;
  FStayOnTop := True;
  FAutoTimeLeft := False;
  FLast := Now;
  FLastVal := Now;
end;

destructor TJvProgressDlg.Destroy;
begin
  FPForm.Free;
  inherited Destroy;
end;

procedure TJvProgressDlg.Close;
begin
  FPForm.Tag := 1;
  FPForm.Close;
end;

procedure TJvProgressDlg.StartProgression;
begin
  FLast := Now;
end;

procedure TJvProgressDlg.SetMaximum(const Value: Integer);
begin
  FMaximum := Value;
  if not (csDesigning in ComponentState) then
    FPForm.ProgressBar1.Max := Value;
  FLast := Now;
end;

procedure TJvProgressDlg.SetStayOnTop(const Value: Boolean);
begin
  FStayOnTop := Value;
  if Value then
    FPForm.FormStyle := fsStayOnTop
  else
    FPForm.FormStyle := fsNormal;
end;

procedure TJvProgressDlg.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    ApplyText(Value);
  end;
end;

procedure TJvProgressDlg.ApplyText(Value: string);
begin
  FPForm.Label1.Caption := Value;
end;

procedure TJvProgressDlg.SetValue(const Value: Integer);
var
  Days: Integer;
  OldDate: TDateTime;
  St: string;
begin
  if FValue <> Value then
  begin
    FValue := Value;
    if csDesigning in ComponentState then
      Exit;
    FPForm.ProgressBar1.Position := Value;
    if FAutoTimeLeft then
    begin
      if FPForm.ProgressBar1.Position = 0 then
        Exit;
      OldDate := (Now - FLast) / (FPForm.ProgressBar1.Position) * FPForm.ProgressBar1.Max;
      OldDate := OldDate - (Now - FLast);

      Days := Round(Int(OldDate));

      if FText <> '' then
        St := FText + ' (' + RC_EstimatedTime
      else
        St := RC_EstimatedTime;
      if Abs(FLastVal - OldDate) > EncodeTime(0, 0, 1, 0) then
      begin
        FLastVal := OldDate;
        if Days = 0 then
          St := St + FormatDateTime('hh:nn:ss', OldDate)
        else
          St := St + IntToStr(Days) + RC_Days + FormatDateTime('hh:nn:ss', OldDate);
        if FText <> '' then
          St := St + ')';
        ApplyText(St)
      end;
    end;
  end;
end;

procedure TJvProgressDlg.Show;
begin
  if FStayOnTop then
    FPForm.FormStyle := fsStayOnTop
  else
    FPForm.FormStyle := fsNormal;
  FPForm.Label1.Caption := FText;
  FPForm.Tag := 0;
  FPForm.Show;
  FPForm.Update;
end;

function TJvProgressDlg.GetShowing: Boolean;
begin
  Result := FPForm.Visible;
end;

end.

