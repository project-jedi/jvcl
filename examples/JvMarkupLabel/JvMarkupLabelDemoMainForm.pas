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
unit JvMarkupLabelDemoMainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, JvComCtrls, JvComponent, JvMarkupLabel, StdCtrls,
  JvExControls, JvExComCtrls, JvExStdCtrls, JvGroupBox;

type
  TJvMarkupLabelDemoMainFrm = class(TForm)
    JvMarkupLabel1: TJvMarkupLabel;
    Label1: TLabel;
    Edit1: TMemo;
    Button1: TButton;
    Label4: TLabel;
    JvGroupBox1: TJvGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    JvTrackBar1: TJvTrackBar;
    CheckBox1: TCheckBox;
    JvTrackBar2: TJvTrackBar;
    procedure JvTrackBarChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    procedure RecalibrateTrackbars;
  end;

var
  JvMarkupLabelDemoMainFrm: TJvMarkupLabelDemoMainFrm;

implementation

{$R *.dfm}

procedure TJvMarkupLabelDemoMainFrm.FormCreate(Sender: TObject);
begin
  Button1Click(nil);
end;

procedure TJvMarkupLabelDemoMainFrm.JvTrackBarChange(Sender: TObject);
begin
  case (Sender as TJvTrackBar).Tag of
  0: JvMarkupLabel1.Width := JvTrackBar1.Position;
  1: JvMarkupLabel1.Height := JvTrackBar2.Position;
  end;
end;

procedure TJvMarkupLabelDemoMainFrm.Button1Click(Sender: TObject);
begin
  JvMarkupLabel1.Text := Edit1.Lines.Text;
end;

procedure TJvMarkupLabelDemoMainFrm.CheckBox1Click(Sender: TObject);
begin
  JvMarkupLabel1.AutoSize := CheckBox1.Checked;
  if not CheckBox1.Checked then
  begin
    JvMarkupLabel1.Width := Self.Width-27;
    JvMarkupLabel1.Height := Self.Height - Label4.Top - 64;
  end;
  RecalibrateTrackbars;
end;

procedure TJvMarkupLabelDemoMainFrm.RecalibrateTrackbars;
begin
  JvTrackBar1.Max := JvMarkupLabel1.Width;
  JvTrackBar1.Position := JvMarkupLabel1.Width;
  JvTrackBar2.Max := JvMarkupLabel1.Height;
  JvTrackBar2.Position := JvMarkupLabel1.Height;
end;

procedure TJvMarkupLabelDemoMainFrm.FormResize(Sender: TObject);
begin
  RecalibrateTrackbars;
end;

end.

