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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

{ Property editors for TDate, TTime and TDateTime data types }
unit JvDateTimeDlg;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF Delphi6_UP} Variants, {$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  { define the different types of date and time combinations supported by the dialog }
  TDateSelectType = (dstDate,dstTime,dstDateTime);
  { the edit dialog }
  TfrmSelectDateTimeDlg = class(TForm)
    dtpDate: TDateTimePicker;
    lblDate: TLabel;
    lblTime: TLabel;
    dtpTime: TDateTimePicker;
    btnOK: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    class function SelectDateTime(var ADate:TDateTime;AType:TDateSelectType):boolean;
  end;

resourcestring
  SSelectDate = 'Select Date';
  SSelectTime = 'Select Time';
  SSelectDateTime = 'Select Date and Time';

implementation

{$R *.dfm}

{ TfrmSelectDateTimeDlg }

class function TfrmSelectDateTimeDlg.SelectDateTime(var ADate: TDateTime;
  AType: TDateSelectType): boolean;
var f:TfrmSelectDateTimeDlg;
begin
  f := self.Create(Application);
  try
    f.dtpDate.Date := ADate;
    f.dtpTime.Time := ADate;
    case AType of
      dstDate:
      begin
        f.Caption := SSelectDate;
        f.lblTime.Visible := false;
        f.dtpTime.Visible := false;
        f.Height := f.Height - f.lblTime.Height - f.dtpTime.Height;
      end;
      dstTime:
      begin
        f.Caption := SSelectTime;
        f.lblDate.Visible := false;
        f.dtpDate.Visible := false;
        f.lblTime.Top := f.lblDate.Top;
        f.dtpTime.Top := f.dtpDate.Top;
        f.Height := f.Height - f.lblTime.Height - f.dtpTime.Height;
      end;
    end;
    Result := f.ShowModal = mrOK;
    if Result then
      ADate := trunc(f.dtpDate.Date) + frac(f.dtpTime.Time);
  finally
    f.Free;
  end;
end;

procedure TfrmSelectDateTimeDlg.FormCreate(Sender: TObject);
begin
  Caption := SSelectDateTime;
end;

end.
