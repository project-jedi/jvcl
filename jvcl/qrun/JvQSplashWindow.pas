{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSplshWnd.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQSplashWindow;

interface

uses
  SysUtils, Classes,  
  Qt, Types, QGraphics, QControls, QForms, QStdCtrls, QExtCtrls, QWindows, 
  JvQAnimatedImage;

type
  TJvSplashWindow = class(TForm)
  private
    FTextMessage: TLabel;
    function GetMessageText: string;
    procedure SetMessageText(const Value: string);
  protected  
    function WidgetFlags: Integer; override; 
  public
    Image: TImage;
    Animation: TJvAnimatedImage;
    procedure CenterFor(Form: TCustomForm);
    property MessageText: string read GetMessageText write SetMessageText;
  end;

function ShowSplashWindow(Graphic: TGraphic; const MsgText: string;
  JvxAnimate: Boolean; AlignForm: TCustomForm): TJvSplashWindow;

// (rom) changed to var (otherwise it makes no sense)
var
  SplashStayOnTop: Boolean = True;

implementation

uses
  Math;

const
  defSplashHeight = 64;
  defImageLeft = 16;
  defImageTop = 16;
  defTextWidth = 238;
  defTextLeft = 56;
  defTextRight = 16;

function CreateSplashWindow: TJvSplashWindow;
begin
  Result := TJvSplashWindow.CreateNew(Application, 0);
  with Result do
  begin
    BorderIcons := [];  
    BorderStyle := fbsNone;
    AutoScroll := False; 
    if SplashStayOnTop then
      FormStyle := fsStayOnTop
    else
      FormStyle := fsNormal;
    ClientHeight := defSplashHeight;
    ClientWidth := defImageLeft + defTextRight + 32;
    Enabled := False;
    {$IFDEF MSWINDOWS}
    Font.Height := -11;
    Font.Name := 'MS Sans Serif';
    PixelsPerInch := 96;
    Scaled := True;
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    Font.Height := 11;
    Font.Name := 'Helvetica';
    PixelsPerInch := 96;
    Scaled := False;
    {$ENDIF LINUX}
    Font.Style := [];
    Font.Color := clWindowText;

    Image := TImage.Create(Result);
    Image.Parent := Result;
    Image.Left := defImageLeft;
    Image.Top := defImageTop;
    Image.Width := 32;
    Image.Height := 32;
    Image.AutoSize := False;
    Image.Stretch := True;
    Image.Visible := False;

    FTextMessage := TLabel.Create(Result);
    FTextMessage.Parent := Result;
    FTextMessage.Left := defTextLeft;
    FTextMessage.Width := defTextWidth;
    FTextMessage.AutoSize := False;
    FTextMessage.Alignment := taCenter;
    FTextMessage.WordWrap := True;

    Animation := TJvAnimatedImage.Create(Result);
    Animation.Parent := Result;
    Animation.Left := defImageLeft;
    Animation.Top := defImageTop;
    Animation.Width := 32;
    Animation.Height := 32;
    Animation.Active := False; 
    Animation.Stretch := True;
    Animation.Visible := False;
  end;
end;

function ShowSplashWindow(Graphic: TGraphic; const MsgText: string;
  JvxAnimate: Boolean; AlignForm: TCustomForm): TJvSplashWindow;
begin
  Result := CreateSplashWindow;
  with Result do
  begin
    if JvxAnimate and (Graphic <> nil) then
    begin
      Animation.Glyph := Graphic as TBitmap;
      Animation.Visible := True;
      Animation.AsyncDrawing := True;
      Animation.Active := True;
    end
    else
    if Graphic <> nil then
    begin
      Image.Picture.Graphic := Graphic;
      Image.Visible := True;
    end
    else
      FTextMessage.Left := defImageLeft;
    FTextMessage.Caption := MsgText;
    MessageText := MsgText;
    CenterFor(AlignForm);
    Show;
    Update;
  end;
end;




function TJvSplashWindow.WidgetFlags: Integer;
begin
  Result := inherited WidgetFlags or Integer(WidgetFlags_WStyle_DialogBorder) or
    Integer(WidgetFlags_WStyle_Tool); 
end;


function TJvSplashWindow.GetMessageText: string;
begin
  Result := FTextMessage.Caption;
end;

procedure TJvSplashWindow.SetMessageText(const Value: string);
var
  TextRect: TRect;
  VertOff: Integer;
begin
  TextRect := Rect(FTextMessage.Left, 0, Max(Screen.Width div 2 - 64,
    defTextWidth), 0);              
  Canvas.Start;
  try 
    DrawText(Canvas.Handle,
      PChar(Value), - 1, TextRect, DT_CALCRECT or DT_WORDBREAK); 
  finally
    Canvas.Stop;
  end; 
  VertOff := (ClientHeight div 2) - ((TextRect.Bottom - TextRect.Top) div 2);
  if VertOff < 0 then
    VertOff := 10;
  TextRect.Top := VertOff;
  TextRect.Bottom := TextRect.Bottom + VertOff;
  FTextMessage.BoundsRect := TextRect;
  ClientWidth := Max(ClientWidth, TextRect.Right + defTextRight);
  ClientHeight := Max(ClientHeight, VertOff * 2);
  if Value <> FTextMessage.Caption then
  begin
    FTextMessage.Caption := Value;
    Update;
  end;
end;

procedure TJvSplashWindow.CenterFor(Form: TCustomForm);
var
  NewLeft, NewTop: Integer;
  DstRect: TRect;
begin
  if Form = nil then
    DstRect := Rect(0, 0, Screen.Width, Screen.Height)
  else
    DstRect := Form.BoundsRect;
  NewLeft := DstRect.Left + ((DstRect.Right - DstRect.Left) div 2) - (Width div 2);
  NewTop := DstRect.Top + ((DstRect.Bottom - DstRect.Top) div 2) - (Height div 2);
  SetBounds(NewLeft, NewTop, Width, Height);
end;

end.

