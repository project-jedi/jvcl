{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDateTimeDlg.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 att users dott sourceforge dott net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  Property editors for TDate, TTime and TDateTime data types

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDateTimeForm;

{$I jvcl.inc}

interface

uses
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  SysUtils,
  {$IFDEF VCL}
  Controls, Forms, StdCtrls, ComCtrls, Menus,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QControls, QForms, QStdCtrls, QComCtrls, QMenus,
  {$ENDIF VisualCLX}
  Classes,
  JvComponent;

type
  { define the different types of date and time combinations supported by the dialog }
  TDateSelectType = (dstDate, dstTime, dstDateTime);

  { the edit dialog }
  TFrmSelectDateTimeDlg = class(TForm)
    dtpDate: TDateTimePicker;
    lblDate: TLabel;
    lblTime: TLabel;
    dtpTime: TDateTimePicker;
    btnOK: TButton;
    btnCancel: TButton;
    PopupMenu1: TPopupMenu;
    mnuNow: TMenuItem;
    mnuDate: TMenuItem;
    mnuTime: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure mnuNowClick(Sender: TObject);
    procedure mnuDateClick(Sender: TObject);
    procedure mnuTimeClick(Sender: TObject);
  public
    class function SelectDateTime(var ADate: TDateTime; AType: TDateSelectType): Boolean;
  end;

implementation

uses
  JvDsgnConsts;

{$IFDEF VCL}
{$R *.dfm}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$R *.xfm}
{$ENDIF VisualCLX}

class function TFrmSelectDateTimeDlg.SelectDateTime(var ADate: TDateTime;
  AType: TDateSelectType): Boolean;
var
  F: TFrmSelectDateTimeDlg;
begin
  F := Self.Create(Application);
  try
    F.dtpDate.Date := Trunc(ADate);
    F.dtpTime.Time := Frac(ADate);
    case AType of
      dstDate:
        begin
          F.mnuDate.Visible := False;
          F.mnuTime.Visible := False;
          F.Caption := RsSelectDate;
          F.lblTime.Visible := False;
          F.dtpTime.Visible := False;
          F.Height := F.Height - F.lblTime.Height - F.dtpTime.Height;
        end;
      dstTime:
        begin
          F.mnuTime.Visible := False;
          F.mnuDate.Visible := False;
          F.Caption := RsSelectTime;
          F.lblDate.Visible := False;
          F.dtpDate.Visible := False;
          F.lblTime.Top := F.lblDate.Top;
          F.dtpTime.Top := F.dtpDate.Top;
          F.Height := F.Height - F.lblTime.Height - F.dtpTime.Height;
        end;
    end;
    Result := F.ShowModal = mrOk;
    if Result then
      ADate := Trunc(F.dtpDate.Date) + Frac(F.dtpTime.Time);
  finally
    F.Free;
  end;
end;

procedure TFrmSelectDateTimeDlg.FormCreate(Sender: TObject);
begin
  Caption := RsSelectDateTime;
end;

procedure TFrmSelectDateTimeDlg.mnuNowClick(Sender: TObject);
begin
  dtpDate.Date := Now;
  dtpTime.Time := Frac(Now);
end;

procedure TFrmSelectDateTimeDlg.mnuDateClick(Sender: TObject);
begin
  dtpDate.Date := Date;
end;

procedure TFrmSelectDateTimeDlg.mnuTimeClick(Sender: TObject);
begin
  dtpTime.Time := Frac(Now);
end;

end.

