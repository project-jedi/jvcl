{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDateTimeDlg.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{ Property editors for TDate, TTime and TDateTime data types }

unit JvDateTimeDlg;

interface

uses
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF}
  SysUtils, Classes, Controls, Forms, StdCtrls, ComCtrls, Menus;

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

resourcestring
  SSelectDate = 'Select Date';
  SSelectTime = 'Select Time';
  SSelectDateTime = 'Select Date and Time';

implementation

{$R *.dfm}

{ TFrmSelectDateTimeDlg }

class function TFrmSelectDateTimeDlg.SelectDateTime(var ADate: TDateTime;
  AType: TDateSelectType): Boolean;
var
  F: TFrmSelectDateTimeDlg;
begin
  F := Self.Create(Application);
  try
    F.dtpDate.Date := trunc(ADate);
    F.dtpTime.Time := frac(ADate);
    case AType of
      dstDate:
        begin
          F.mnuDate.Visible := false;
          F.mnuTime.Visible := false;
          F.Caption := SSelectDate;
          F.lblTime.Visible := False;
          F.dtpTime.Visible := False;
          F.Height := F.Height - F.lblTime.Height - F.dtpTime.Height;
        end;
      dstTime:
        begin
          F.mnuTime.Visible := false;
          F.mnuDate.Visible := false;
          F.Caption := SSelectTime;
          F.lblDate.Visible := False;
          F.dtpDate.Visible := False;
          F.lblTime.Top := F.lblDate.Top;
          F.dtpTime.Top := F.dtpDate.Top;
          F.Height := F.Height - F.lblTime.Height - F.dtpTime.Height;
        end;
    end;
    Result := F.ShowModal = mrOK;
    if Result then
      ADate := Trunc(F.dtpDate.Date) + Frac(F.dtpTime.Time);
  finally
    F.Free;
  end;
end;

procedure TFrmSelectDateTimeDlg.FormCreate(Sender: TObject);
begin
  Caption := SSelectDateTime;
end;

procedure TFrmSelectDateTimeDlg.mnuNowClick(Sender: TObject);
begin
  dtpDate.Date := Now;
  dtpTime.Time := frac(Now);
end;

procedure TFrmSelectDateTimeDlg.mnuDateClick(Sender: TObject);
begin
  dtpDate.Date := Date;
end;

procedure TFrmSelectDateTimeDlg.mnuTimeClick(Sender: TObject);
begin
  dtpTime.Time := frac(Now);
end;

end.

