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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvProgressDlg;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvFormProgress, JvBaseDlg, JvTypes, JvComponent;

type
  TJvProgressDlg = class(TJvComponent)
  private
    FPForm: TFormProg;
    FValue: Integer;
    FMaximum: Integer;
    FText: string;
    FStay: Boolean;
    FAutoTimeLeft: Boolean;
    FLast: TDateTime;
    FLastVal: TDateTime;
    procedure SetMaximum(const Value: Integer);
    procedure SetValue(const Value: Integer);
    procedure SetText(const Value: string);
    procedure SetStay(const Value: Boolean);
    procedure ApplyText(Value: string);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Maximum: Integer read FMaximum write SetMaximum default 100;
    property Value: Integer read FValue write SetValue default 0;
    property Text: string read FText write SetText;
    property StayOnTop: Boolean read FStay write SetStay default True;
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

  {**************************************************}

procedure TJvProgressDlg.Close;
begin
  FPForm.Tag := 1;
  FPForm.Close;
end;

{**************************************************}

procedure TJvProgressDlg.StartProgression;
begin
  FLast := Now;
end;

{**************************************************}

constructor TJvProgressDlg.Create(AOwner: TComponent);
begin
  inherited;
  FPForm := TFormProg.Create(Self);
  FText := RC_ProgressText;
  FMaximum := 100;
  FValue := 0;
  FStay := True;
  FAutoTimeLeft := False;
  FLast := Now;
  FLastVal := Now;
end;

{**************************************************}

destructor TJvProgressDlg.Destroy;
begin
  FPForm.Free;
  inherited;
end;

{**************************************************}

procedure TJvProgressDlg.SetMaximum(const Value: Integer);
begin
  FMaximum := Value;
  if not (csDesigning in ComponentState) then
    FPForm.ProgressBar1.Max := Value;
  FLast := Now;
end;

{**************************************************}

procedure TJvProgressDlg.SetStay(const Value: Boolean);
begin
  FStay := Value;
  if Value then
    FPForm.FormStyle := fsStayOntop
  else
    FPForm.FormStyle := fsNormal;
end;

{**************************************************}

procedure TJvProgressDlg.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    ApplyText(Value);
  end;
end;

{**************************************************}

procedure TJvProgressDlg.ApplyText(Value: string);
begin
  FPForm.Label1.Caption := Value;
end;

{**************************************************}

procedure TJvProgressDlg.SetValue(const Value: Integer);
var
  days: Integer;
  OldDate: TDateTime;
  st: string;
begin
  if FValue <> Value then
  begin
    FValue := Value;
    FPForm.ProgressBar1.Position := Value;
    if FAutoTimeLeft then
    begin
      if FPForm.ProgressBar1.Position = 0 then
        Exit;
      OldDate := (Now - FLast) / (FPForm.ProgressBar1.Position) * FPForm.ProgressBar1.Max;
      OldDate := OldDate - (Now - FLast);

      days := Round(Int(OldDate));

      if FText <> '' then
        st := FText + ' (' + RC_EstimatedTime
      else
        st := RC_EstimatedTime;
      if Abs(FLastVal - OldDate) > EncodeTime(0, 0, 1, 0) then
      begin
        FLastVal := OldDate;
        if days = 0 then
          st := st + FormatDateTime('hh:nn:ss', OldDate)
        else
          st := st + IntToStr(days) + RC_Days + FormatDateTime('hh:nn:ss', OldDate);
        if FText <> '' then
          st := st + ')';
        ApplyText(st)
      end;
    end;
  end;
end;

{**************************************************}

procedure TJvProgressDlg.Show;
begin
  if FStay then
    FPForm.FormStyle := fsStayOnTop
  else
    FPForm.Formstyle := fsNormal;
  FPForm.Label1.Caption := FText;
  FPForm.Tag := 0;
  FPForm.Show;
end;

end.
