{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCaptionPanel.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright © 1997-2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

{ @abstract(TJvCaptionPanel is a panel that looks like a form, with a Caption area,
  system buttons but is derived from a normal panel.) }

unit JvCaptionPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, JvComponent;

type
  TJvCapBtnStyle=(capClose,capMax,capMin,capRestore,capHelp);
  TJvCapBtnStyles = set of TJvCapBtnStyle;
  TJvDrawPosition=(dpLeft,dpTop,dpRight,dpBottom);
  TJvCapBtnEvent=procedure(Sender:TObject;Button:TJvCapBtnStyle) of object;
  { internal class }

  TJvCapBtn=class(TGraphicControl)
  private
    FOwner:TComponent;
    FStyle:TJvCapBtnStyle;
    FMouseDown:boolean;
    FDown:boolean;
    FFlat:boolean;
    FInside:boolean;
    procedure SetFlat(Value:boolean);
    procedure SetStyle(Value:TJvCapBtnStyle);
    procedure BtnClick;
  protected
    procedure Click;override;
    procedure Paint;override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
    procedure MouseMove(Shift:TShiftState;X,Y:integer);override;
    procedure CMMouseEnter(var msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var msg: TMessage); message CM_MOUSELEAVE;
  public
    constructor Create(AOwner:TComponent);override;
    property Style:TJvCapBtnStyle read FStyle write SetStyle default capClose;
    property Flat:boolean read FFlat write SetFlat default false;
    property Visible default false;
  end;

  TJvCaptionPanel = class(TJvCustomPanel)
  private
    { Private declarations }
    FButtonArray:array[TJvCapBtnStyle] of TJvCapBtn;
    FButtonClick:TJvCapBtnEvent;
    FDrawPosition:TJvDrawPosition;
    FCaptionWidth:integer;
    FOffset:integer;
    FButtons:TJvCapBtnStyles;
    FAutoDrag:boolean;
    FMouseDown:boolean;
    FCaptionRect:TRect;
    FCaption:string;
    FCaptionColor:TColor;
    FFlat:boolean;
    FBevel:integer;
    FDragging:boolean;
    FEndDrag:TNotifyEvent;
    FFont:TFont;
    procedure SetFont(Value:TFont);
    procedure SetCaptionColor(Value:TColor);
    procedure SetFlat(Value:boolean);
    procedure SetButtons(Value:TJvCapBtnStyles);
    procedure SetCaption(Value:string);
    procedure SeTJvDrawPosition(Value:TJvDrawPosition);
    procedure DrawRotatedText(Rotation:integer);
    procedure DrawButtons;
    procedure WMNCHitTest(var Message:TWMNCHitTest); message WM_NCHITTEST;
    procedure WMSize(var Message:TWMNoParams);message WM_SIZE;
  protected
    { Protected declarations }
    procedure Paint;override;
    procedure AlignControls(AControl: TControl; var Rect: TRect);override;
    procedure CreateParams(var Params:TCreateParams);override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
    procedure MouseMove(Shift:TShiftState;X,Y:integer);override;
    procedure ClickButton(Button:TJvCapBtnStyle);virtual;
    procedure DoLeaveDrag;virtual;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
  published
    { Published declarations }
    property Align;
    property AutoDrag:boolean read FAutoDrag write FAutoDrag default true;
    property Buttons:TJvCapBtnStyles read FButtons write SetButtons;
    property BorderStyle default bsSingle;
    property Caption:string read FCaption write SetCaption;
    property CaptionColor:TColor read FCaptionColor write SetCaptionColor default clActiveCaption;
    property CaptionPosition:TJvDrawPosition read FDrawPosition write SeTJvDrawPosition default dpLeft;
    property CaptionFont:TFont read FFont write SetFont;
    property Color;
    property Cursor;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FlatButtons:boolean read FFlat write SetFlat default false;
    property Font;
    property FullRepaint;
    property Hint;
    property Locked;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopUpMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnButtonClick:TJvCapBtnEvent read FButtonClick write FButtonClick;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEndAutoDrag:TNotifyEvent read FEndDrag write FEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;


implementation

{ TJvCapBtn}

constructor TJvCapBtn.Create(Aowner:TComponent);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
  Width := GetSystemMetrics(SM_CYCAPTION) - 3;
  Height := Width - 2;
  FStyle := capClose;
  Visible := false;
  FFlat := false;
end;

procedure TJvCapBtn.BtnClick;
begin
  if FOwner is TJvCaptionPanel then
    TJvCaptionPanel(FOwner).ClickButton(Style);
end;

procedure TJvCapBtn.SetFlat(Value:boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TJvCapBtn.SetStyle(Value:TJvCapBtnStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Invalidate;
  end;
end;

procedure TJvCapBtn.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not Enabled then Exit;
  inherited MouseDown(Button,Shift,X,Y);
  if not FMouseDown then
  begin
    FMouseDown := true;
    FDown := true;
    Repaint;
  end;
end;

procedure TJvCapBtn.Click;
begin
  inherited Click;
  BtnClick;
end;

procedure TJvCapBtn.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not Enabled then Exit;
  inherited MouseUp(Button,Shift,X,Y);
  if FMouseDown then
  begin
    FMouseDown := false;
    FDown := false;
    Repaint;
  end;
end;

procedure TJvCapBtn.MouseMove(Shift:TShiftState;X,Y:integer);
begin
  inherited MouseMove(Shift,X,Y);
  if FMouseDown then
  begin
    FInside := PtInRect(ClientRect,Point(X,Y));
    if not FInside then
    begin
      if FDown then { mouse has slid off, so release }
      begin
        FDown := false;
        Repaint;
      end;
    end
    else
    begin
      if not FDown then { mouse has slid back on, so push }
      begin
        FDown := true;
        Repaint;
      end;
    end;
  end;
end;

procedure TJvCapBtn.CMMouseEnter(var msg: TMessage);
var R:TRect;
begin
  inherited;
  FInside := true;
  if FFlat then
  begin
    R := ClientRect;
    if FDown then
      Frame3D(Canvas,R,clBtnShadow,clBtnHighLight,1)
    else
      Frame3D(Canvas,R,clBtnHighLight,clBtnShadow,1);
  end;
end;

procedure TJvCapBtn.CMMouseLeave(var msg: TMessage);
var R:TRect;
begin
  inherited;
  FInside := false;
  if FFlat then
  begin
    R := ClientRect;
    Frame3D(Canvas,R,clBtnFace,clBtnFace,1);
  end;
end;

procedure TJvCapBtn.Paint;
var Flags:integer;R:TRect;
begin
  if not Visible then Exit;
  Flags := 0;
  case FStyle of
    capClose:   Flags := DFCS_CAPTIONCLOSE;
    capMax:     Flags := DFCS_CAPTIONMAX;
    capMin:     Flags := DFCS_CAPTIONMIN;
    capRestore: Flags := DFCS_CAPTIONRESTORE;
    capHelp:    Flags := DFCS_CAPTIONHELP;
  end;

  if not Enabled then
    Flags := Flags or DFCS_INACTIVE
  else if FDown and FMouseDown and Enabled then
    Flags := Flags or DFCS_PUSHED;
  if FFlat then
    Flags := Flags or DFCS_FLAT;

  Canvas.Brush.Color := Color;
  SetBkMode(Canvas.Handle,TRANSPARENT);
  DrawFrameControl(Canvas.Handle,ClientRect,DFC_CAPTION,Flags);
  if FFlat then
  begin
    R := ClientRect;
    if FDown and FMouseDown then
      Frame3D(Canvas,R,clBtnShadow,clBtnHighLight,1)
    else if FInside then
      Frame3D(Canvas,R,clBtnHighLight,clBtnShadow,1)
    else
      Frame3D(Canvas,R,clBtnFace,clBtnFace,1);
  end;
end;

{ TJvCaptionPanel }
constructor TJvCaptionPanel.Create(AOwner:TComponent);
var i:TJvCapBtnStyle;
begin
  inherited Create(AOwner);
  FFont := TFont.Create;
  FFont.Name := 'Arial';
  FFont.Size := 10;
  FFont.Style := [fsBold];
  FFont.Color := clWhite;
  FDrawPosition := dpLeft;
  FCaptionWidth := GetSystemMetrics(SM_CYCAPTION);
  FOffset := 8;
  FAutoDrag := true;
  FCaptionColor := clActiveCaption;
  FFlat := false;
  for i := Low(FButtonArray) to High(FButtonArray) do    //Iterate
  begin
    FButtonArray[i] := TJvCapBtn.Create(self);
    FButtonArray[i].Parent := self;
    FButtonArray[i].Style := i;
    FButtonArray[i].Flat := FFlat;
  end;
  FButtons := [];
  BorderStyle := bsSingle;
end;

destructor TJvCaptionPanel.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TJvCaptionPanel.SetFont(Value:TFont);
begin
  FFont.Assign(Value);
  Invalidate;
end;    

procedure TJvCaptionPanel.SetCaption(Value:string);
begin
  FCaption := Value;
  inherited Caption := '';
  Invalidate;
end;

procedure TJvCaptionPanel.SetCaptionColor(Value:TColor);
begin
  if FCaptionColor <> Value then
  begin
    FCaptionColor := Value;
    Invalidate;
  end;
end;

procedure TJvCaptionPanel.SetFlat(Value:boolean);
var i:TJvCapBtnStyle;
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    for i := Low(FbuttonArray) to High(FButtonArray) do    //Iterate
      FButtonArray[i].Flat := FFlat;
  end;
end;

procedure TJvCaptionPanel.SetButtons(Value:TJvCapBtnStyles);
var i:TJvCapBtnStyle;
begin
  if FButtons <> Value then
  begin
    FButtons := Value;
    for i := Low(FButtonArray) to High(FButtonArray) do    //Iterate
      FButtonArray[i].Visible := i in FButtons;
    Invalidate;
  end;
end;

procedure TJvCaptionPanel.SeTJvDrawPosition(Value:TJvDrawPosition);
begin
  if FDrawPosition <> Value then
  begin
    FDrawPosition := Value;
    RecreateWnd;
  end;
end;

procedure TJvCaptionPanel.AlignControls(AControl: TControl; var Rect: TRect);
begin
  case FDrawPosition of    //
    dpLeft:   Rect := Classes.Rect(FCaptionWidth + 2,0,ClientWidth,ClientHeight);
    dpTop:    Rect := Classes.Rect(0,FCaptionWidth + 2,ClientWidth,ClientHeight);
    dpRight:  Rect := Classes.Rect(0,0,ClientWidth - FCaptionWidth - 2,ClientHeight);
    dpBottom: Rect := Classes.Rect(0,0,ClientWidth,ClientHeight - FCaptionWidth - 2);
  end;    //case
  inherited AlignControls(AControl,Rect);
end;

procedure TJvCaptionPanel.CreateParams(var Params:TCreateParams);
begin
  inherited CreateParams(Params);
  if BorderStyle = bsSingle then
    with Params do
    begin
      Style := Style or WS_THICKFRAME;
      ExStyle := ExStyle and not WS_EX_CLIENTEDGE;
    end;
end;

procedure TJvCaptionPanel.Paint;
var Rotation:integer;R:TRect;
begin
  R := ClientRect;
  with Canvas do
  begin
    Brush.Color := Color;
    FillRect(R);
    Brush.Color := FCaptionColor;
  end;    //with
  FBevel := 2;
  Rotation := 0;

  case FDrawPosition of    //
    dpLeft:
    begin
      FCaptionRect := Rect(FBevel,FBevel,FCaptionWidth + FBevel,ClientHeight - FBevel);
      Rotation := 90;
    end;
    dpTop:
      FCaptionRect := Rect(FBevel,FBevel,ClientWidth - FBevel,FCaptionWidth + FBevel);
    dpRight:
    begin
      FCaptionRect := Rect(ClientWidth - FCaptionWidth  - FBevel,FBevel,ClientWidth - FBevel,ClientHeight - FBevel);
      Rotation := -90;
    end;
    dpBottom:
      FCaptionRect := Rect(FBevel,ClientHeight - FCaptionWidth - FBevel,ClientWidth - FBevel,ClientHeight - FBevel);
  end;    //case
  Canvas.FillRect(FCaptionRect);
  DrawRotatedText(Rotation);
  DrawButtons;
end;

procedure TJvCaptionPanel.DrawRotatedText(Rotation:integer);
  var
  lf : TLogFont;
  tf : TFont;
  R:TRect;
  Flags,tH,tW:integer;
begin
  if FCaption = '' then Exit;
  SetBkMode(Canvas.Handle,TRANSPARENT);
  R := FCaptionRect;
  tH := ((R.Bottom - R.Top) - Canvas.TextHeight(FCaption)) div 2;
  tW := ((R.Right - R.Left) - Canvas.TextHeight(FCaption)) div 2;
  with Canvas do
  begin
    tf := TFont.Create;
    try
      tf.Assign(FFont);
      GetObject(tf.Handle, sizeof(lf), @lf);
      lf.lfEscapement := Rotation * 10;
      lf.lfOrientation := Rotation * 10;
      tf.Handle := CreateFontIndirect(lf);
      Canvas.Font.Assign(tf);
    finally
      tf.Free;
    end;
    case FDrawPosition of
      dpLeft:
      begin
        with FCaptionRect do
          R := Rect(Left,Bottom,Right,Top);
        OffsetRect(R,tW,-FOffset);
      end;
      dpTop:   OffsetRect(R,FOffset,tH);
      dpRight:
      begin
        with FCaptionRect do
          R := Rect(Right,Top,Left,Bottom);
        OffsetRect(R,-tW,FOffset);
      end;
      dpBottom: OffsetRect(R,FOffset,tH);
    end;    //case
    Flags := DT_NOPREFIX;
    if FDrawPosition in [dpTop,dpBottom] then
      Flags := Flags or DT_VCENTER;

    if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
      Flags := Flags or DT_NOCLIP; { bug or feature? }
    DrawText(Canvas.Handle,PChar(Caption),-1,R,Flags);
  end;
end;

procedure TJvCaptionPanel.DrawButtons;
var R:TRect;FWidth,FHeight:integer;
begin

  if FButtons = [] then Exit;

  FWidth := FButtonArray[capClose].Width;
  FHeight := FButtonArray[capClose].Height;
  if FFlat then
  begin
    Inc(FWidth,1);
    Inc(FHeight,1);
  end;

  case FDrawPosition of
    dpLeft:    R  := Rect(FCaptionRect.Left + 2,FCaptionRect.Top + 2,0,0);
    dpTop:     R  := Rect(FCaptionRect.Right - FWidth - 2,FCaptionRect.Top + 3,0,0);
    dpRight:   R  := Rect(FCaptionRect.Left + 2,FCaptionRect.Bottom - FHeight - 2,0,0);
    dpBottom:  R  := Rect(FCaptionRect.Right - FWidth - 2,FCaptionRect.Top + 3,0,0);
  end;    //case

  if capClose in FButtons then
  begin
    FButtonArray[capClose].Top := R.Top;
    FButtonArray[capClose].Left := R.Left;
    FButtonArray[capClose].Visible := true;
    case FDrawPosition of
      dpLeft:   OffsetRect(R,0,FHeight + 2);
      dpTop:    OffsetRect(R,-FWidth - 2,0);
      dpRight:  OffsetRect(R,0,-FHeight - 2);
      dpBottom: OffsetRect(R,-FWidth - 2,0);
    end;
  end
  else
    FButtonArray[capClose].Visible := false;

  if  (capMax in FButtons) then
  begin
    FButtonArray[capMax].Top := R.Top;
    FButtonArray[capMax].Left := R.Left;
    FButtonArray[capMax].Visible := true;
    case FDrawPosition of
      dpLeft:   OffsetRect(R,0,FHeight);
      dpTop:    OffsetRect(R,-FWidth,0);
      dpRight:  OffsetRect(R,0,-FHeight);
      dpBottom: OffsetRect(R,-FWidth,0);
    end;    //case
  end
  else
    FButtonArray[capMax].Visible := false;

  if  (capRestore in FButtons) then
  begin
    FButtonArray[capRestore].Top := R.Top;
    FButtonArray[capRestore].Left := R.Left;
    FButtonArray[capRestore].Visible := true;
    case FDrawPosition of
      dpLeft:   OffsetRect(R,0,FHeight);
      dpTop:    OffsetRect(R,-FWidth,0);
      dpRight:  OffsetRect(R,0,-FHeight);
      dpBottom: OffsetRect(R,-FWidth,0);
    end;    //case
  end
  else
    FButtonArray[capRestore].Visible := false;

  if (capMin in FButtons) then
  begin
    FButtonArray[capMin].Top := R.Top;
    FButtonArray[capMin].Left := R.Left;
    FButtonArray[capMin].Visible := true;
    case FDrawPosition of
      dpLeft:   OffsetRect(R,0,FHeight);
      dpTop:    OffsetRect(R,-FWidth,0);
      dpRight:  OffsetRect(R,0,-FHeight);
      dpBottom: OffsetRect(R,-FWidth,0);
    end;    //case
  end
  else
    FButtonArray[capMin].Visible := false;

  if capHelp in FButtons then
  begin
    FButtonArray[capHelp].Top := R.Top;
    FButtonArray[capHelp].Left := R.Left;
    FButtonArray[capHelp].Visible := true;
  end
  else
    FButtonArray[capHelp].Visible := false;
end;

{ this method is called only by the caption buttons }
procedure TJvCaptionPanel.ClickButton(Button:TJvCapBtnStyle);
begin
  if Assigned(FButtonClick) then FButtonClick(self,Button);
end;

procedure TJvCaptionPanel.DoLeaveDrag;
begin
  if Assigned(FEndDrag) then FEndDrag(self);
end;

procedure TJvCaptionPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button,Shift,X,Y);
  if FDragging then
  begin
    FDragging := false;
    DoLeaveDrag;
  end;
end;

procedure TJvCaptionPanel.MouseMove(Shift:TShiftState;X,Y:integer);
begin
  inherited MouseMove(Shift,X,Y);
end;

procedure TJvCaptionPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
  SC_DragMove = $F012;
begin
  inherited MouseDown(Button,Shift,X,Y);

  FMouseDown := true;
  if not PtInRect(FCaptionRect,Point(X,Y)) then Exit;

  if FAutoDrag then
  begin
    SetZOrder(true);
    FDragging := true;
    ReleaseCapture;
    Perform(WM_SysCommand, SC_DragMove, 0);
  end;
end;

procedure TJvCaptionPanel.WMSize(var Message:TWMNoParams);
begin
  inherited;
  Repaint;
end;

procedure TJvCaptionPanel.WMNCHitTest(var Message:TWMNCHitTest);
begin
  inherited;
end;


end.
