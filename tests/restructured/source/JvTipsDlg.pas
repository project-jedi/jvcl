{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTipsDlg.PAS, released on 2002-05-26.

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

{ A form used by TJvTipWindow }

unit JvTipsDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type
  TJvTipsEvent = procedure (Sender:TObject;var Tips:string) of object;
  TJvTipsFrm = class(TForm)
    Shape1: TShape;
    ShowCheck: TCheckBox;
    NextTipBtn: TButton;
    CancelBtn: TButton;
    Shape2: TShape;
    Shape3: TShape;
    TitleLabel: TLabel;
    Image1: TImage;
    TipWindow: TMemo;
    procedure CancelBtnClick(Sender: TObject);
    procedure NextTipBtnClick(Sender: TObject);
  private
    { Private declarations }
    FTipsEvent:TJvTipsEvent;
  protected
    procedure GetTips(Strings:Tstrings);
  public
    property OnGetTipsEvent:TJvTipsEvent read FTipsEvent write FTipsEvent;
    { Public declarations }
    class function ShowTips(const ACaption:string;ShowTipsChecked:boolean;TitleFont,TextFont:TFont;AOnTipEvent:TJvTipsEvent):boolean;
  end;


implementation

{$R *.DFM}

procedure TJvTipsFrm.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TJvTipsFrm.NextTipBtnClick(Sender: TObject);
begin
  GetTips(TipWindow.Lines);
end;

procedure TJvTipsFrm.GetTips(Strings:TStrings);
var S:String;
begin
  S := '';
  if Assigned(FTipsEvent) then
  begin
    FTipsEvent(self,S);
    TipWindow.Text := S;
  end;
  NextTipBtn.Enabled := S <> '';
end;

class function TJvTipsFrm.ShowTips(const ACaption: string;
  ShowTipsChecked: boolean; TitleFont, TextFont: TFont;
  AOnTipEvent: TJvTipsEvent): boolean;
var f:TJvTipsFrm;
begin
  f := self.Create(Application);
  try
    f.ShowCheck.Checked := ShowTipsChecked;
    f.Caption := ACaption;
    f.TitleLabel.Font := TitleFont;
    f.TitleLabel.Caption := ACaption;
    f.TipWindow.Font := TextFont;
    f.OnGetTipsEvent := AOnTipEvent;
    f.GetTips(f.TipWindow.Lines);
    f.ShowModal;
    Result := f.ShowCheck.Checked;
  finally
    f.Free;
  end;
end;

end.
