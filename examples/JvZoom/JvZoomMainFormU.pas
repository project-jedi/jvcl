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

unit JvZoomMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvZoom, ExtCtrls, JvComponent, JvCaptionPanel, JvExControls;
  
type
  TJvZoomMainForm = class(TForm)
    Label1: TLabel;
    JvZoom1: TJvZoom;
    CheckBox1: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    procedure CheckBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  JvZoomMainForm: TJvZoomMainForm;

implementation

{$R *.DFM}

procedure TJvZoomMainForm.CheckBox1Click(Sender: TObject);
begin
  JvZoom1.active := checkbox1.checked;
end;

procedure TJvZoomMainForm.Button1Click(Sender: TObject);
begin
  JvZoom1.ZoomLevel := JvZoom1.ZoomLevel div 2;
end;

procedure TJvZoomMainForm.Button2Click(Sender: TObject);
begin
  JvZoom1.ZoomLevel := JvZoom1.ZoomLevel*2;
end;

procedure TJvZoomMainForm.FormCreate(Sender: TObject);
begin
  JvZoom1.Active := true;
end;

end.