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

unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    Panel8: TPanel;
    Button1: TButton;
    Panel1: TPanel;
    Panel7: TPanel;
    Panel6: TPanel;
    Panel5: TPanel;
    Panel4: TPanel;
    Panel3: TPanel;
    Panel2: TPanel;
    Panel9: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

uses
  JvJVCLUtils, JvConsts;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  LoadOLEDragCursors;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Panel4.Cursor := crMultiDragLink;
  Panel5.Cursor := crDragAlt;
  Panel6.Cursor := crMultiDragAlt;
  Panel7.Cursor := crMultiDragLinkAlt;
end;

end.

