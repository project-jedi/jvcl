{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
unit Splash;

interface

uses
  Windows, Messages, SysUtils{, Variants}, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TSplashs = class(TForm)
    Image1: TImage;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    SplashClosed: Boolean;
  end;

var
  Splashs: TSplashs;

implementation

{$R *.dfm}

procedure TSplashs.Timer1Timer(Sender: TObject);
begin
  SplashClosed := True;
end;

procedure TSplashs.FormCreate(Sender: TObject);
begin
  SplashClosed := False;
end;

procedure TSplashs.Image1Click(Sender: TObject);
begin
  SplashClosed := True;
end;

end.
