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

unit TransBtnFormMainU;

interface

uses
  Types, QWindows, QMessages, SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, QMenus, QButtons, JvQComponent, JvQButton,
  JvQTransparentButton, JvQExControls, QComCtrls, JvQExComCtrls, JvQComCtrls,
  QImgList, QTypes;

type
  TTransBtnFormMain = class(TForm)
    Label1: TLabel;
    PopupMenu1: TPopupMenu;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Allgroups1: TMenuItem;
    Selectedgroups1: TMenuItem;
    Delete1: TMenuItem;
    Previous1: TMenuItem;
    N2: TMenuItem;
    Exit2: TMenuItem;
    Label3: TLabel;
    JvTransparentButton21: TJvTransparentButton2;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TransparentButton1: TJvTransparentButton;
    TransparentButton2: TJvTransparentButton;
    TransparentButton3: TJvTransparentButton;
    TransparentButton7: TJvTransparentButton;
    TransparentButton8: TJvTransparentButton;
    TransparentButton9: TJvTransparentButton;
    TransparentButton6: TJvTransparentButton;
    TransparentButton10: TJvTransparentButton;
    TransparentButton11: TJvTransparentButton;
    TransparentButton5: TJvTransparentButton;
    TransparentButton15: TJvTransparentButton;
    TransparentButton12: TJvTransparentButton;
    TransparentButton13: TJvTransparentButton;
    TransparentButton14: TJvTransparentButton;
    Button1: TButton;
    JvTransparentButton22: TJvTransparentButton2;
    JvTransparentButton23: TJvTransparentButton2;
    JvTransparentButton24: TJvTransparentButton2;
    JvTransparentButton25: TJvTransparentButton2;
    JvTransparentButton26: TJvTransparentButton2;
    JvTransparentButton27: TJvTransparentButton2;
    JvTransparentButton28: TJvTransparentButton2;
    ImageList1: TImageList;
    Image2: TImage;
    Image3: TImage;
    procedure TransparentButton1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TransparentButton1MouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TransparentButton1MouseEnter(Sender: TObject);
    procedure TB1Click(Sender: TObject);
    procedure TransparentButton4MouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure TransparentButton1Click(Sender: TObject);
    procedure TransparentButton3Click(Sender: TObject);
    procedure TransparentButton10Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure TransparentButton6Click(Sender: TObject);
    procedure Exit2Click(Sender: TObject);
    procedure TransparentButton1MouseLeave(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure JvTransparentButton26Click(Sender: TObject);
    procedure JvTransparentButton28Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  public
    Activated: Boolean;
  end;

var
  TransBtnFormMain: TTransBtnFormMain;

implementation

{$R *.xfm}

{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$ENDIF LINUX}

function GetOS: string;
begin
  {$IFDEF MSWINDOWS}
  case Win32Platform of
    VER_PLATFORM_WIN32_NT: Result := 'Windows NT 4.0';
    VER_PLATFORM_WIN32_WINDOWS: Result := 'Windows 95';
    VER_PLATFORM_WIN32S: Result := 'Windows 3.1 with Win32s';
  end;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Result := 'Linux';
  {$ENDIF LINUX}

end;

{ tile the background }

procedure TTransBtnFormMain.TransparentButton1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  self.Caption := 'Down';
end;

procedure TTransBtnFormMain.TransparentButton1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  self.Caption := 'Up';
end;

procedure TTransBtnFormMain.TransparentButton1MouseEnter(Sender: TObject);
begin
  self.Caption := 'MouseEnter';
end;

procedure TTransBtnFormMain.TB1Click(Sender: TObject);
begin
  self.Caption := 'Clicked';
end;

procedure TTransBtnFormMain.TransparentButton4MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  self.Caption := 'MouseMove';
end;

procedure TTransBtnFormMain.TransparentButton1Click(Sender: TObject);
begin
  TransparentButton2.Enabled := not TransparentButton2.Enabled;
  TransparentButton3.Enabled := not TransparentButton2.Enabled;
end;


procedure TTransBtnFormMain.TransparentButton3Click(Sender: TObject);
begin
  ShowMessage('Clicked button. (try the shortkey Alt+W too)');
end;

procedure TTransBtnFormMain.TransparentButton10Click(Sender: TObject);
begin
  TransparentButton6.Down := not TransparentButton6.Down;
end;

procedure TTransBtnFormMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    TransparentButton1.OnClick(nil);
  end;
end;

procedure TTransBtnFormMain.TransparentButton6Click(Sender: TObject);
begin
  TransparentButton6.Down := not TransparentButton6.Down;
  ShowMessage('OS is: ' + GetOS + #13'(Note that an OnClick event is generated when going down and when going up)');
end;

procedure TTransBtnFormMain.Exit2Click(Sender: TObject);
begin
  Close;
end;

procedure TTransBtnFormMain.TransparentButton1MouseLeave(Sender: TObject);
begin
  self.Caption := 'MouseLeave';
end;

procedure TTransBtnFormMain.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TTransBtnFormMain.FormPaint(Sender: TObject);
{
var
  X, Y, W, H: Longint;
  ACanvas: TControlCanvas;
}
begin
{
  ACanvas := TControlCanvas.Create;
  try
    ACanvas.Control := PageControl1.ActivePage;
  with Image1.Picture.Bitmap do
  begin
    W := Width;
    H := Height;
  end;
  Y := 0;
  while Y < PageControl1.ActivePage.Height do begin
    X := 0;
    while X < PageControl1.ActivePage.Width do
    begin
      ACanvas.Draw(X, Y, Image1.Picture.Bitmap);
      Inc(X, W);
    end;
    Inc(Y, H);
  end;

  finally
    ACanvas.Free;
  end;
  }
end;

procedure TTransBtnFormMain.PageControl1Change(Sender: TObject);
begin
  Invalidate;
end;

procedure TTransBtnFormMain.JvTransparentButton26Click(Sender: TObject);
begin
  JvTransparentButton26.Down := not JvTransparentButton26.Down;

end;

procedure TTransBtnFormMain.JvTransparentButton28Click(Sender: TObject);
begin
  JvTransparentButton27.Enabled := not JvTransparentButton27.Enabled;
end;

procedure TTransBtnFormMain.FormActivate(Sender: TObject);
begin
  if not Activated then
  begin
    Activated := True;
    Image2.Picture.Assign(Image3.Picture);
  end;
end;

end.

