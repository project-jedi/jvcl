{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

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

{$I jvcl.inc}

unit ScrollWinMainFormU;

interface

uses
  QWindows, QMessages, SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QToolWin, QStdCtrls, QComCtrls, QMenus, QExtCtrls, QImgList, JvQScrollPanel, JvQLookOut,
  JvQComponent, JvQExControls, JvQExForms, QTypes, QComCtrlsEx;

type
  TJvScrollingWindowMainForm = class(TForm)
    ImageList1: TImageList;
    Panel1: TPanel;
    Button1: TButton;
    Label1: TLabel;
    Button2: TButton;
    Label2: TLabel;
    Button3: TButton;
    Label3: TLabel;
    Button4: TButton;
    Label4: TLabel;
    Edit1: TEdit;
    Label5: TLabel;
    UpDown1: TUpDown;
    ScrollPanel1: TJvScrollingWindow;
    ExpressButton1: TJvExpressButton;
    ExpressButton2: TJvExpressButton;
    ExpressButton3: TJvExpressButton;
    ExpressButton4: TJvExpressButton;
    ExpressButton5: TJvExpressButton;
    ExpressButton21: TJvExpressButton;
    ExpressButton22: TJvExpressButton;
    ExpressButton23: TJvExpressButton;
    ExpressButton24: TJvExpressButton;
    ExpressButton25: TJvExpressButton;
    ExpressButton26: TJvExpressButton;
    ExpressButton27: TJvExpressButton;
    ExpressButton28: TJvExpressButton;
    PopupMenu1: TPopupMenu;
    Move1: TMenuItem;
    Hide1: TMenuItem;
    Flat1: TMenuItem;
    AutoRepeat1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure ExpressButton1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
    procedure Button4Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  end;

var
  JvScrollingWindowMainForm: TJvScrollingWindowMainForm;

implementation

const
  BoolOnOff: array[boolean] of string = (' Off ', ' On');

  AlignStr: array[TAlign] of string = ('alNone', 'alTop', 'alBottom', 'alLeft',
    'alRight', 'alClient', 'alCustom');

{$R *.xfm}

procedure TJvScrollingWindowMainForm.Button1Click(Sender: TObject);
var i, tmp: integer;
begin

{ AutoArrange only moves children - not the panel itself,
 so you'll have to do it manually: }

  with ScrollPanel1 do
    if ScrollDirection = sdVertical then
    begin
      tmp := Width;
      ScrollDirection := sdHorizontal;
      if Align = alRight then
        Align := alTop
      else
        Align := alBottom;
      Height := tmp;
    end
    else
    begin
      tmp := Height;
      ScrollDirection := sdVertical;
      if Align = alTop then
        Align := alLeft
      else
        Align := alRight;
      Width := tmp;
    end;
       { Adjust the TDividers }
  with ScrollPanel1 do
    for i := 0 to ControlCount - 1 do
      if Controls[i] is TJvDivider then
        TJvDivider(Controls[i]).Vertical := ScrollPanel1.Align in [alTop,
          alBottom];

  Caption := 'Align ' + AlignStr[ScrollPanel1.Align];
end;

procedure TJvScrollingWindowMainForm.ExpressButton1Click(Sender: TObject);
begin
  Caption := (Sender as TComponent).Name;
end;

procedure TJvScrollingWindowMainForm.Button2Click(Sender: TObject);
begin
  ScrollPanel1.AutoHide := not ScrollPanel1.AutoHide;
  Caption := 'Hidden' + BoolOnOff[ScrollPanel1.AutoHide];
end;

procedure TJvScrollingWindowMainForm.Button3Click(Sender: TObject);
begin
  ScrollPanel1.Flat := not ScrollPanel1.Flat;
  Caption := 'Flat' + BoolOnOff[ScrollPanel1.Flat];
end;

procedure TJvScrollingWindowMainForm.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
  ScrollPanel1.BorderWidth := UpDown1.Position;
end;

procedure TJvScrollingWindowMainForm.Button4Click(Sender: TObject);
begin
  ScrollPanel1.AutoRepeat := not ScrollPanel1.AutoRepeat;
  Caption := 'AutoRepeat' + BoolOnOff[ScrollPanel1.AutoRepeat];
end;

procedure TJvScrollingWindowMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TJvScrollingWindowMainForm.FormResize(Sender: TObject);
begin
  Panel1.Top := (Height - Panel1.Height) div 2;
  Panel1.Left := (Width - Panel1.Width) div 2;
end;

procedure TJvScrollingWindowMainForm.CheckBox1Click(Sender: TObject);
begin
  ScrollPanel1.Enabled := CheckBox1.Checked;
end;

end.

