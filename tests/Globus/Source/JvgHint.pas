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

UNIT JvgHint;

INTERFACE

USES Windows,
   Messages,
   Controls,
   Classes,
   Forms,
   JVComponent,
   SysUtils,
   Graphics;

VAR
   lpFrHintComponent          : Pointer;

TYPE
   TJvgHintWindow = CLASS;

   TJvgHint = CLASS(TJvComponent)
   PRIVATE
      FOnShowHint: TShowHintEvent;
      FOnHint: TNotifyEvent;
      FActive: boolean;
      //    HW                  : TJvgHintWindow;
      AOnHintOld: TNotifyEvent;
      AOnShowHintOld: TShowHintEvent;
      //    FActivateHint : THintActivateEvent;
      //    FHintColor : TColor;
      //    FHintFont : TFont;
      //    FHintPause : integer;
      //    FHintHidePause : integer;
      //    FHintShortPause : integer;
      FShowHint: boolean;
      //    FStyle : THintStyle;
      //    FOnDraw : TDrawEvent;
      //    FOnHintHide : THintHideEvent;
      AHintWindow: THintWindow;
      AHintControl: TControl;
      //    procedure SetFont(Value : TFont);
      //    procedure SetColor(Value : TColor);
      //    procedure SetPause(Value : integer);
      //    procedure SetHidePause(Value : integer);
      //    procedure SetShortPause(Value : integer);
   PROTECTED
      //    property HintWindow : THintWindow read FHintWindow;
      //    property HintControl : TControl read FHintControl;
      PROCEDURE Notification(Component: TComponent; Operation: TOperation);
         OVERRIDE;
      PROCEDURE Loaded; OVERRIDE;
      PROCEDURE OnHintNew(Sender: TObject);
      PROCEDURE OnShowHintNew(VAR HintStr: STRING; VAR CanShow: Boolean; VAR
         HintInfo: THintInfo);
      PROCEDURE InitHint;
   PUBLIC
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
      PROCEDURE ShowHintAt(x, y: integer; caption: STRING);
   PUBLISHED
      //    property HintColor : TColor read FHintColor write SetColor;
      //    property HintPause : integer read FHintPause write SetPause;
      //    property HintHidePause : integer read FHintHidePause write SetHidePause;
      //    property HintShortPause : integer read FHintShortPause write SetShortPause;
      //    property HintFont : TFont read FHintFont write SetFont;
      PROPERTY Active: boolean READ FActive WRITE FActive;
      PROPERTY ShowHint: boolean READ FShowHint WRITE FShowHint;
      //    property Style : THintStyle read FStyle write FStyle;
      PROPERTY OnShowHint: TShowHintEvent READ FOnShowHint WRITE FOnShowHint;
      PROPERTY OnHint: TNotifyEvent READ FOnHint WRITE FOnHint;
      //    property OnDraw : TDrawEvent read FOnDraw write FOnDraw;
      //    property OnActivateHint : THintActivateEvent read FActivateHint write FActivateHint;
      //    property OnHintHide : THintHideEvent read FOnHintHide write FOnHintHide;

   END;

   TJvgHintWindow = CLASS(THintWindow)
   PRIVATE
      //procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
   PROTECTED
      AFrHintComponent: TJvgHint;
      PROCEDURE CreateParams(VAR Params: TCreateParams); OVERRIDE;
      //  function  isHintMsg(var Msg: TMsg): Boolean;override;
      //  procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
      //  procedure WMGetDLGCode(var Message: TMessage); message WM_GETDLGCODE;
      PROCEDURE WMKillFocus(VAR Message: TMessage); MESSAGE WM_ACTIVATE;
      PROCEDURE Paint; OVERRIDE;
   PUBLIC
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      PROCEDURE ActivateHint(Rect: TRect; CONST AHint: STRING); OVERRIDE;
      //  function IsHintMsg(var Msg: TMsg): Boolean; virtual;
   END;

IMPLEMENTATION
USES JvgTypes,
   JvgUtils,
   ExtCtrls;
{$R JvgHint.res}

CONSTRUCTOR TJvgHint.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   {if (csDesigning in ComponentState) then } InitHint;
   Application.ShowHint := false;
   Application.ShowHint := true;
   //  HW := TJvgHintWindow.Create(Application);
END;

DESTRUCTOR TJvgHint.Destroy;
BEGIN
   Application.OnShowHint := AOnShowHintOld;
   Application.OnHint := AOnHintOld;
   INHERITED Destroy;
END;

PROCEDURE TJvgHint.Notification(Component: TComponent; Operation: TOperation);
BEGIN
   IF (Component <> Self) AND (Operation = opInsert) AND (Component IS TJvgHint)
      THEN
      RAISE
         Exception.Create('Cannot create more than one instance of TJvgHint component');
END;

PROCEDURE TJvgHint.InitHint;
BEGIN
   WITH Application DO
   BEGIN
      AOnHintOld := OnHint;
      AOnShowHintOld := OnShowHint;
      OnShowHint := OnShowHintNew;
      OnHint := OnHintNew;
      HintWindowClass := TJvgHintWindow;
      lpFrHintComponent := Self;
   END;
   FShowHint := true;
END;

PROCEDURE TJvgHint.Loaded;
BEGIN
   INHERITED;
   IF NOT (csDesigning IN ComponentState) AND (Active) THEN
   BEGIN
      InitHint;
      Application.ShowHint := false;
      Application.ShowHint := FShowHint;
   END;
END;

PROCEDURE TJvgHint.OnHintNew(Sender: TObject);
BEGIN
   IF Assigned(FOnHint) THEN
      FOnHint(Sender);
END;

PROCEDURE TJvgHint.OnShowHintNew(VAR HintStr: STRING;
   VAR CanShow: Boolean; VAR HintInfo: THintInfo);
BEGIN
   AHintControl := HintInfo.HintControl;
   IF Assigned(FOnShowHint) THEN
      FOnShowHint(HintStr, CanShow, HintInfo);
END;

PROCEDURE TJvgHint.ShowHintAt(x, y: integer; Caption: STRING);
VAR
   R                          : TRect;
   HW                         : TJvgHintWindow;
BEGIN
   HW := TJvgHintWindow.Create(Application);
   //  SetWindowPos(HW.Handle, HWND_TOP, pt.x, pt.y, 100, 100, SWP_SHOWWINDOW);
   R := Bounds(X, Y, 10, 10);

   DrawText(HW.Canvas.Handle, PChar(Caption), length(Caption), R, DT_WORDBREAK OR
      DT_CALCRECT);
   HW.ActivateHint(R, Caption);

END;
//________________________________________________

CONSTRUCTOR TJvgHintWindow.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   AFrHintComponent := TJvgHint(lpFrHintComponent);
   TRY
      IF Assigned(AFrHintComponent) THEN
         AFrHintComponent.AHintWindow := Self;
      WITH Canvas DO
      BEGIN
         Font.Name := 'Arial';
         Font.Size := 8;
         //      Font.Style := [fsItalic];
         {$IFDEF GL_RUS}
         Font.CharSet := RUSSIAN_CHARSET;
         {$ENDIF}
      END;
   EXCEPT
      //Free;
   END;
END;

PROCEDURE TJvgHintWindow.CreateParams(VAR Params: TCreateParams);
BEGIN
   INHERITED CreateParams(Params);
   Params.Style := Params.Style AND NOT WS_BORDER;
   { if (FHintEngine <> nil) and (FHintEngine.Style = hsTransparent)
      then Params.ExStyle := Params.ExStyle or ws_ex_Transparent;
    if (FHintEngine <> nil) and (FHintEngine.Style <> hsStandart)
      then Params.Style := Params.Style and not WS_BORDER;}
END;

PROCEDURE TJvgHintWindow.ActivateHint(Rect: TRect; CONST AHint: STRING);
//var h,w : integer;
BEGIN
   Caption := AHint;
   BoundsRect := Rect;
   tag := 1;
   Width := Width + 20;
   Height := Height + 1;
   //  H := Height; W := Width;
   IF Rect.Top + Height > Screen.Height THEN
      Rect.Top := Screen.Height - Height;
   IF Rect.Left + Width > Screen.Width THEN
      Rect.Left := Screen.Width - Width;
   IF Rect.Left < 0 THEN
      Rect.Left := 0;
   IF Rect.Bottom < 0 THEN
      Rect.Bottom := 0;

   //  if (FHintEngine <> nil) and Assigned(FHintEngine.OnActivateHint)
   //    then FHintEngine.OnActivateHint(H, W, Rect);

   SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, Width, Height,
      SWP_SHOWWINDOW OR SWP_NOACTIVATE);
END;

PROCEDURE TJvgHintWindow.Paint;
VAR
   R                          : TRect;
   bmp                        : TBitmap;
BEGIN
   R := ClientRect;
   //  dec(R.right, 1); dec(R.Bottom, 1);
   Canvas.Brush.Color := clWhite;
   Canvas.Pen.Color := 0;
   {$IFDEF COMPILER5_UP}
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
   BitBlt(Canvas.Handle, R.Left, R.Top + 1, bmp.Width, bmp.Height,
      bmp.Canvas.Handle, 0, 0, SRCCOPY);
   bmp.Free;
   SetBkMode(Canvas.Handle, TRANSPARENT);
   Canvas.Font.Color := clBlack;
   inc(R.Left, 20);
   inc(R.Top, 1);
   DrawText(Canvas.Handle, PChar(Caption), length(Caption), R, DT_VCENTER OR
      DT_WORDBREAK);
END;

PROCEDURE TJvgHintWindow.WMKillFocus(VAR Message: TMessage);
BEGIN
   hide;
END;

END.

