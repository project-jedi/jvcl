{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

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

unit jvFormanimationdemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvComponent, JvFormAnimation, StdCtrls;

type
  TfrFormAnimation = class(TForm)
    JvFormAnimation1: TJvFormAnimation;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
  private
  public
  end;


implementation

{$R *.dfm}

procedure TfrFormAnimation.Button1Click(Sender: TObject);
begin
  JvFormAnimation1.AppearEllipse;
end;

procedure TfrFormAnimation.Button2Click(Sender: TObject);
begin
  JvFormAnimation1.AppearRectangle;
end;

procedure TfrFormAnimation.Button3Click(Sender: TObject);
begin
  JvFormAnimation1.AppearRoundedRectangle(20, 20);
end;

procedure TfrFormAnimation.Button4Click(Sender: TObject);
begin
  JvFormAnimation1.AppearHorizontally;
end;

procedure TfrFormAnimation.Button5Click(Sender: TObject);
begin
  JvFormAnimation1.AppearVertically;
end;

procedure TfrFormAnimation.Button6Click(Sender: TObject);
begin
  JvFormAnimation1.AppearTelevision;
end;

procedure TfrFormAnimation.Button7Click(Sender: TObject);
begin
  JvFormAnimation1.AppearToTop;
end;

procedure TfrFormAnimation.Button8Click(Sender: TObject);
begin
  JvFormAnimation1.AppearToBottom;
end;

end.

