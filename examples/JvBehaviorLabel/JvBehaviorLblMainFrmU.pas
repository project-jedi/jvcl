{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit JvBehaviorLblMainFrmU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvBehaviorLabel, JvExStdCtrls;

type
  TJvBehaviorLblMainFrm = class(TForm)
    lblCodeBreaker: TJvBehaviorLabel;
    btnCodeBreak: TButton;
    lblAppearing: TJvBehaviorLabel;
    btnAppear: TButton;
    lblBlinking: TJvBehaviorLabel;
    btnBlink: TButton;
    lblBouncing: TJvBehaviorLabel;
    btnBounce: TButton;
    lblScrolling: TJvBehaviorLabel;
    btnScroll: TButton;
    lblSpecial: TJvBehaviorLabel;
    btnSpecial: TButton;
    lblTyping: TJvBehaviorLabel;
    btnType: TButton;
    btnAll: TButton;
    procedure btnCodeBreakClick(Sender: TObject);
    procedure btnAppearClick(Sender: TObject);
    procedure btnBlinkClick(Sender: TObject);
    procedure btnBounceClick(Sender: TObject);
    procedure btnScrollClick(Sender: TObject);
    procedure btnSpecialClick(Sender: TObject);
    procedure btnTypeClick(Sender: TObject);
    procedure btnAllClick(Sender: TObject);
  private
    procedure DoCodeBreakStart(Sender:TObject);
    procedure DoCodeBreakStop(Sender:TObject);
  public
  end;

var
  JvBehaviorLblMainFrm: TJvBehaviorLblMainFrm;

implementation

{$R *.dfm}

procedure TJvBehaviorLblMainFrm.btnCodeBreakClick(Sender: TObject);
begin
  Randomize;
  lblCodeBreaker.OnStart := nil;
  lblCodeBreaker.OnStop := nil;
  if lblCodeBreaker.Caption = TJvLabelCodeBreaker(lblCodeBreaker.BehaviorOptions).DecodedText then
    lblCodeBreaker.Caption := 'x6/yhjSkhHHDski"=90sd';
  // this might trigger the OnStart/OnStop events, so set to nil
  lblCodeBreaker.BehaviorOptions.Active := not lblCodeBreaker.BehaviorOptions.Active;
  lblCodeBreaker.OnStart := DoCodeBreakStart;
  lblCodeBreaker.OnStop := DoCodeBreakStop;
end;

procedure TJvBehaviorLblMainFrm.DoCodeBreakStart(Sender: TObject);
begin
  lblCodeBreaker.Caption := 'BREAK THE CODE';
end;

procedure TJvBehaviorLblMainFrm.DoCodeBreakStop(Sender: TObject);
begin
  ShowMessage('Congratulations! You''ve hacked the system!');
end;

procedure TJvBehaviorLblMainFrm.btnAppearClick(Sender: TObject);
begin
  lblAppearing.Alignment := taCenter;
  lblAppearing.BehaviorOptions.Active := not lblAppearing.BehaviorOptions.Active;
end;

procedure TJvBehaviorLblMainFrm.btnBlinkClick(Sender: TObject);
begin
  lblBlinking.BehaviorOptions.Active := not lblBlinking.BehaviorOptions.Active;
end;

procedure TJvBehaviorLblMainFrm.btnBounceClick(Sender: TObject);
begin
  lblBouncing.BehaviorOptions.Active := not lblBouncing.BehaviorOptions.Active;
end;

procedure TJvBehaviorLblMainFrm.btnScrollClick(Sender: TObject);
begin
  lblScrolling.BehaviorOptions.Active := not lblScrolling.BehaviorOptions.Active;
end;

procedure TJvBehaviorLblMainFrm.btnSpecialClick(Sender: TObject);
begin
  lblSpecial.BehaviorOptions.Active := not lblSpecial.BehaviorOptions.Active;
end;

procedure TJvBehaviorLblMainFrm.btnTypeClick(Sender: TObject);
begin
  lblTyping.BehaviorOptions.Active := not lblTyping.BehaviorOptions.Active;
end;

procedure TJvBehaviorLblMainFrm.btnAllClick(Sender: TObject);
var i:integer;
begin
  for i := 0 to ControlCount -1 do
    if (Controls[i] <> btnAll) and (Controls[i] is TButton) then
      TButton(Controls[i]).Click;
end;

end.
