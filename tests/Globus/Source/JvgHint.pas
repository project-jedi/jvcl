{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgHint.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15 

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgHint;

interface

uses Windows, Messages, Controls, Classes, Forms, SysUtils, Graphics;

var
  lpFrHintComponent: Pointer;

type
  TJvgHintWindow = class;

  TJvgHint = class(TComponent)
  private
    FOnShowHint 	: TShowHintEvent;
    FOnHint		: TNotifyEvent;
    FActive             : boolean;
//    HW                  : TJvgHintWindow;
    AOnHintOld		: TNotifyEvent;
    AOnShowHintOld	: TShowHintEvent;
//    FActivateHint : THintActivateEvent;
//    FHintColor : TColor;
//    FHintFont : TFont;
//    FHintPause : integer;
//    FHintHidePause : integer;
//    FHintShortPause : integer;
    FShowHint : boolean;
//    FStyle : THintStyle;
//    FOnDraw : TDrawEvent;
//    FOnHintHide : THintHideEvent;
    AHintWindow : THintWindow;
    AHintControl : TControl;
//    procedure SetFont(Value : TFont);
//    procedure SetColor(Value : TColor);
//    procedure SetPause(Value : integer);
//    procedure SetHidePause(Value : integer);
//    procedure SetShortPause(Value : integer);
  protected
//    property HintWindow : THintWindow read FHintWindow;
//    property HintControl : TControl read FHintControl;
    procedure Notification( Component: TComponent; Operation: TOperation ); override;
    procedure Loaded; override;
    procedure OnHintNew(Sender: TObject);
    procedure OnShowHintNew(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    procedure InitHint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowHintAt(x, y: integer; caption: string);
  published
//    property HintColor : TColor read FHintColor write SetColor;
//    property HintPause : integer read FHintPause write SetPause;
//    property HintHidePause : integer read FHintHidePause write SetHidePause;
//    property HintShortPause : integer read FHintShortPause write SetShortPause;
//    property HintFont : TFont read FHintFont write SetFont;
    property Active : boolean read FActive write FActive;
    property ShowHint : boolean read FShowHint write FShowHint;
//    property Style : THintStyle read FStyle write FStyle;
    property OnShowHint : TShowHintEvent read FOnShowHint write FOnShowHint;
    property OnHint : TNotifyEvent read FOnHint write FOnHint;
//    property OnDraw : TDrawEvent read FOnDraw write FOnDraw;
//    property OnActivateHint : THintActivateEvent read FActivateHint write FActivateHint;
//    property OnHintHide : THintHideEvent read FOnHintHide write FOnHintHide;

  end;

TJvgHintWindow = class(THintWindow)
private
  //procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
protected
  AFrHintComponent : TJvgHint;
  procedure CreateParams(var Params: TCreateParams); override;
//  function  isHintMsg(var Msg: TMsg): Boolean;override;
//  procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
//  procedure WMGetDLGCode(var Message: TMessage); message WM_GETDLGCODE;
  procedure WMKillFocus(var Message: TMessage); message WM_ACTIVATE;
  procedure Paint; override;
public
  constructor Create(AOwner: TComponent); override;
  procedure ActivateHint(Rect: TRect; const AHint: string); override;
//  function IsHintMsg(var Msg: TMsg): Boolean; virtual;
end;

implementation
uses JvgTypes, JvgUtils, ExtCtrls;
{$R JvgHint.res}
constructor TJvgHint.Create(AOwner: TComponent);
begin
  inherited;
  {if (csDesigning in ComponentState) then }InitHint;
  Application.ShowHint := false;
  Application.ShowHint := true;
//  HW := TJvgHintWindow.Create(Application);
end;

destructor TJvgHint.Destroy;
begin
 Application.OnShowHint := AOnShowHintOld;
 Application.OnHint := AOnHintOld;
 inherited Destroy;
end;

procedure TJvgHint.Notification( Component: TComponent; Operation: TOperation );
begin
  if (Component <> Self)and(Operation = opInsert)and(Component is TJvgHint ) then
    raise Exception.Create('Cannot create more than one instance of TJvgHint component');
end;

procedure TJvgHint.InitHint;
begin
  with Application do begin
    AOnHintOld := OnHint;
    AOnShowHintOld := OnShowHint;
    OnShowHint := OnShowHintNew;
    OnHint := OnHintNew;
    HintWindowClass := TJvgHintWindow;
    lpFrHintComponent := Self;
  end;
  FShowHint := true;
end;

procedure TJvgHint.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState)and(Active) then
  begin
    InitHint;
    Application.ShowHint := false;
    Application.ShowHint := FShowHint;
  end;
end;

procedure TJvgHint.OnHintNew(Sender: TObject);
begin
  if Assigned(FOnHint) then FOnHint(Sender);
end;

procedure TJvgHint.OnShowHintNew(var HintStr: string;
	     var CanShow: Boolean; var HintInfo: THintInfo);
begin
  AHintControl := HintInfo.HintControl;
  if Assigned(FOnShowHint) then FOnShowHint(HintStr, CanShow, HintInfo);
end;

procedure TJvgHint.ShowHintAt(x, y: integer; Caption: string);
var
  R: TRect;
  HW: TJvgHintWindow;
begin
  HW := TJvgHintWindow.Create(Application);
//  SetWindowPos(HW.Handle, HWND_TOP, pt.x, pt.y, 100, 100, SWP_SHOWWINDOW);
  R := Bounds(X, Y, 10, 10);

  DrawText( HW.Canvas.Handle, PChar(Caption), length(Caption), R, DT_WORDBREAK or DT_CALCRECT);
  HW.ActivateHint(R, Caption);

end;
//________________________________________________

constructor TJvgHintWindow.Create(AOwner: TComponent);
begin
  inherited;
  AFrHintComponent := TJvgHint(lpFrHintComponent);
  try
    if Assigned(AFrHintComponent) then AFrHintComponent.AHintWindow := Self;
    with Canvas do
    begin
      Font.Name := 'Arial';
      Font.Size := 8;
//      Font.Style := [fsItalic];
      {$IFDEF GL_RUS}
      Font.CharSet	    := RUSSIAN_CHARSET;
      {$ENDIF}
     end;
  except
    //Free;
  end;
end;

procedure TJvgHintWindow.CreateParams(var Params: TCreateParams);
begin
 inherited CreateParams(Params);
 Params.Style := Params.Style and not WS_BORDER;
{ if (FHintEngine <> nil) and (FHintEngine.Style = hsTransparent)
   then Params.ExStyle := Params.ExStyle or ws_ex_Transparent;
 if (FHintEngine <> nil) and (FHintEngine.Style <> hsStandart)
   then Params.Style := Params.Style and not WS_BORDER;}
end;

procedure TJvgHintWindow.ActivateHint(Rect: TRect; const AHint: string);
//var h,w : integer;
begin
  Caption := AHint;
  BoundsRect := Rect;
  tag := 1;
  Width := Width + 20;
  Height := Height + 1;
//  H := Height; W := Width;
  if Rect.Top + Height > Screen.Height then
    Rect.Top := Screen.Height - Height;
  if Rect.Left + Width > Screen.Width then
    Rect.Left := Screen.Width - Width;
  if Rect.Left < 0 then Rect.Left := 0;
  if Rect.Bottom < 0 then Rect.Bottom := 0;

//  if (FHintEngine <> nil) and Assigned(FHintEngine.OnActivateHint)
//    then FHintEngine.OnActivateHint(H, W, Rect);

  SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, Width, Height,
	       SWP_SHOWWINDOW or SWP_NOACTIVATE );
end;

procedure TJvgHintWindow.Paint;
var
  R: TRect;
  bmp: TBitmap;
begin
  R := ClientRect;
//  dec(R.right, 1); dec(R.Bottom, 1);
  Canvas.Brush.Color := clWhite;
  Canvas.Pen.Color := 0;
  {$IFDEF GLVER_D5}
    Canvas.Rectangle(R);
  {$ELSE}
    Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
  {$ENDIF}
//  DrawBoxEx( Canvas.Handle, Rect(R.Left, R.Top, R.Left+14, R.Bottom),
//	     [ fsdLeft, fsdTop, fsdRight, fsdBottom ],
//	       bvNone, bvRaised, false, GetSysColor( COLOR_BTNFACE ), true );

  bmp := TBitmap.Create;
//  bmp.LoadfromFile('c:\help.bmp');
  bmp.LoadFromResourceName(hInstance, 'HELP');
  BitBlt( Canvas.Handle, R.Left, R.Top+1, bmp.Width, bmp.Height, bmp.Canvas.Handle, 0, 0, SRCCOPY);
  bmp.Free;
  SetBkMode( Canvas.Handle, TRANSPARENT );
  Canvas.Font.Color:=clBlack;
  inc(R.Left, 20); inc(R.Top, 1);
  DrawText( Canvas.Handle, PChar(Caption), length(Caption), R, DT_VCENTER or DT_WORDBREAK );
end;



procedure TJvgHintWindow.WMKillFocus(var Message: TMessage);
begin
  hide;
end;

end.
