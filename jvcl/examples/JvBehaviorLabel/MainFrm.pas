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

unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvBehaviorLabel;

type
  TForm1 = class(TForm)
    lblCodeBreaker: TJvBehaviorLabel;
    Button1: TButton;
    lblAppearing: TJvBehaviorLabel;
    Button2: TButton;
    lblBlinking: TJvBehaviorLabel;
    Button3: TButton;
    lblBouncing: TJvBehaviorLabel;
    Button4: TButton;
    lblScrolling: TJvBehaviorLabel;
    Button5: TButton;
    lblSpecial: TJvBehaviorLabel;
    Button6: TButton;
    lblTyping: TJvBehaviorLabel;
    Button7: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    procedure DoCodeBreakStart(Sender:TObject);
    procedure DoCodeBreakStop(Sender:TObject);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  lblCodeBreaker.OnStart := nil;
  lblCodeBreaker.OnStop := nil;
  // this might trigger the OnStart/OnStop events, so set to nil
  lblCodeBreaker.BehaviorOptions.Active := not lblCodeBreaker.BehaviorOptions.Active;
  lblCodeBreaker.OnStart := DoCodeBreakStart;
  lblCodeBreaker.OnStop := DoCodeBreakStop;
end;

procedure TForm1.DoCodeBreakStart(Sender: TObject);
begin
  lblCodeBreaker.Caption := 'BREAK THE CODE';
end;

procedure TForm1.DoCodeBreakStop(Sender: TObject);
begin
  ShowMessage('Congratulations! You''ve hacked the system!');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  lblAppearing.BehaviorOptions.Active := not lblAppearing.BehaviorOptions.Active; 
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  lblBlinking.BehaviorOptions.Active := not lblBlinking.BehaviorOptions.Active;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  lblBouncing.BehaviorOptions.Active := not lblBouncing.BehaviorOptions.Active;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  lblScrolling.BehaviorOptions.Active := not lblScrolling.BehaviorOptions.Active;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  lblSpecial.BehaviorOptions.Active := not lblSpecial.BehaviorOptions.Active;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  lblTyping.BehaviorOptions.Active := not lblTyping.BehaviorOptions.Active;
end;

end.
