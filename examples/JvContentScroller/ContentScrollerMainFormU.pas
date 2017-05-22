{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.delphi-jedi.org

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

unit ContentScrollerMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvGIF, ExtCtrls, JvComponent, JvCaptionPanel,
  JvContentScroller, JvExExtCtrls;

type
  TContentScrollerMainForm = class(TForm)
    JvContentScroller1: TJvContentScroller;
    Label1: TLabel;
    Image11: TImage;
    Button1: TButton;
    rgDirection: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure rgDirectionClick(Sender: TObject);
  end;

var
  ContentScrollerMainForm: TContentScrollerMainForm;

implementation

{$R *.dfm}

procedure TContentScrollerMainForm.FormCreate(Sender: TObject);
var
  Buffer: array [0..255] of Char;
begin
  GetWindowsDirectory(Buffer, SizeOf(Buffer));
  JvContentScroller1.MediaFile := Buffer + '\Media\Tada.wav';
end;

procedure TContentScrollerMainForm.Button1Click(Sender: TObject);
begin
   JvContentScroller1.ScrollDirection := TJvContentScrollDirection(rgDirection.ItemIndex);
   JvContentScroller1.Active := not JvContentScroller1.Active;
end;

procedure TContentScrollerMainForm.rgDirectionClick(Sender: TObject);
begin
   JvContentScroller1.Active := False;
   JvContentScroller1.ScrollDirection := TJvContentScrollDirection(rgDirection.ItemIndex);
   JvContentScroller1.Active := True;
end;

end.
