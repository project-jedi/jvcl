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

unit ContentScrollerMainFormU;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  QWindows, QMessages, SysUtils, Classes, Types, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QExtCtrls, JvQComponent, JvQCaptionPanel,
  JvQContentScroller, JvQExExtCtrls;

type
  TContentScrollerMainForm = class(TForm)
    JvContentScroller1: TJvContentScroller;
    Label1: TLabel;
    Image11: TImage;
    chkGoDown: TCheckBox;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  end;

var
  ContentScrollerMainForm: TContentScrollerMainForm;

implementation

{$R *.xfm}

procedure TContentScrollerMainForm.FormCreate(Sender: TObject);
{$IFDEF MSWINDOWS}
var
  Buffer: array [0..255] of Char;
{$ENDIF MSWINDOWS}
begin
  {$IFDEF MSWINDOWS}
  GetWindowsDirectory(Buffer, SizeOf(Buffer));
  JvContentScroller1.MediaFile := Buffer + '\Media\Tada.wav';
  {$ENDIF MSWINDOWS}
end;

procedure TContentScrollerMainForm.Button1Click(Sender: TObject);
begin
  if chkGoDown.Checked then
    JvContentScroller1.ScrollDirection := sdDown
  else
    JvContentScroller1.ScrollDirection := sdUp;
  JvContentScroller1.Active := not JvContentScroller1.Active;
end;

end.
