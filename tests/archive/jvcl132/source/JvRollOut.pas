{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRollOut.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

{ TJvRollout is an autoexpanding / collapsing panel. }

unit JvRollOut;

{  TODO:
    Doesn't draw an underline for speed-keys (the '&' character ) if
    Placement = plLeft. Something with DrawText ?
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,ExtCtrls, JvComponent;
const
  CM_EXPANDED = WM_USER + 155;

type
  TJvPlacement=(plTop,plLeft);

  TJvCustomRollOut = class(TJvCustomPanel)
  private
    { Private declarations }
    FGroupIndex      :Integer;
    FImageList       :TImageList;
    FButtonRect      :TRect;
    FPlacement       :TJvPlacement;
    FExpandedImage   :integer;
    FCollapsedImage  :integer;
    FCollapsed       :boolean;
    FMouseDown       :boolean;
    FInsideButton    :boolean;
    FCWidth          :integer;
    FCHeight         :integer;
    FAWidth          :integer;
    FAHeight         :integer;
    FImageOffset     :integer;
    FButtonHeight    :integer;
    FChildOffset     :integer;
    FCaption         :TCaption;
    FTopColor        :TColor;
    FBottomColor     :TColor;
    FBTopColor       :TColor;
    FBBottomColor    :TColor;
    FButtonColor     :TColor;
    FHiText          :TColor;
    FOnExpand        :TNotifyEvent;
    FOnCollapse      :TNotifyEvent;
    procedure SetGroupIndex(Value: Integer);
    procedure SetPlacement(Value:TJvPlacement);
    procedure SetExpandedImage(Value:integer);
    procedure SetCollapsedImage(Value:integer);
    procedure WriteAWidth(Writer:TWriter);
    procedure WriteAHeight(Writer:TWriter);
    procedure WriteCWidth(Writer:TWriter);
    procedure WriteCHeight(Writer:TWriter);
    procedure ReadAWidth(Reader:TReader);
    procedure ReadAHeight(Reader:TReader);
    procedure ReadCWidth(Reader:TReader);
    procedure ReadCHeight(Reader:TReader);
    procedure SetHiTextColor(Value:TColor);
    procedure SetCollapsed(Value:boolean);
    procedure SetImageList(Value:TImageList);
    procedure SetImageOffset(Value:integer);
    procedure SetButtonHeight(Value:integer);
    procedure SetChildOffset(Value:integer);
    procedure SetCaption(Value:TCaption);
    procedure SetTopColor(Value:TColor);
    procedure SetBottomColor(Value:TColor);
    procedure SetBTopColor(Value:TColor);
    procedure SetBBottomColor(Value:TColor);
    procedure SetButtonColor(Value:TColor);
    procedure RedrawControl(DrawAll:boolean);
    procedure DrawButtonFrame;
    procedure UpdateGroup;
    procedure CMExpanded(var Message:TMessage); message CM_EXPANDED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkgnd(var Message:TMessage);message WM_ERASEBKGND;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    { Protected declarations }
    procedure CreateWnd;override;
    procedure AlignControls(AControl: TControl; var Rect: TRect);override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
    procedure DefineProperties(Filer:TFiler);override;
    procedure Expanding;virtual;
    procedure Collapsing;virtual;
    procedure Paint;override;
    procedure Click;override;
    property ButtonColor:TColor read FButtonColor write SetButtonColor default clBtnFace;
    property ButtonHeight:integer read FButtonHeight write SetButtonHeight default 20;
    property ButtonColTop:TColor read FBTopColor write SetBTopColor default clBtnHighLight;
    property ButtonColBtm:TColor read FBBottomColor write SetBBottomColor default clBtnShadow;
    property ColHiText:TColor read FHiText write SetHiTextColor default clBlack;
    property ChildOffset:integer read FChildOffset write SetChildOffset default 0;
    property Collapsed:boolean read FCollapsed write SetCollapsed default false;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property ImageExpanded:integer read FExpandedImage write SetExpandedImage default 0;
    property ImageCollapsed:integer read FCollapsedImage write SetCollapsedImage default 0;
    property ImageList:TImageList read FImageList write SetImageList;
    property ImageOffset:integer read FImageOffset write SetImageOffset default 5;
    property FrameColTop:TColor read FTopColor write SetTopColor default clBtnShadow;
    property FrameColBtm:TColor read FBottomColor write SetBottomColor default clBtnHighLight;
    property Placement:TJvPlacement read FPlacement write SetPlacement default plTop;
    property OnCollapse:TNotifyEvent read FOnCollapse write FOnCollapse;
    property OnExpand:TNotifyEvent read FOnExpand write FOnExpand;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor Destroy; override;
    procedure SetBounds(ALeft,ATop,AWidth,AHeight:integer);override;
    procedure Collapse;virtual;
    procedure Expand;virtual;
  published
    { Published declarations }
    property Caption:TCaption read FCaption write SetCaption;
  end;

  TJvRollout=class(TJvCustomRollOut)
  published
    property Align;
    property BevelWidth;
    property BorderWidth;
    property ButtonColor;
    property ButtonColBtm;
    property ButtonHeight;
    property ButtonColTop;
    property ChildOffset;
    property Collapsed;
    property Color;
    property ColHiText;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FrameColTop;
    property FrameColBtm;
    property FullRepaint;
    property Font;
    property GroupIndex;
    property ImageExpanded;
    property ImageCollapsed;
    property ImageList;
    property ImageOffset;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property Placement;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnExpand;
    property OnCollapse;
  end;


implementation

const
  FIncrement=24;
  FSmooth:boolean = false;

procedure SetTextAngle(Cnv: TCanvas; Angle: integer);
var
  FntLogRec: TLogFont;
begin
  GetObject(Cnv.Font.Handle, SizeOf(FntLogRec), Addr(FntLogRec));
  FntLogRec.lfEscapement := Angle * 10;
  FntLogRec.lfOutPrecision := OUT_TT_ONLY_PRECIS;
  Cnv.Font.Handle := CreateFontIndirect(FntLogRec);
end;

procedure SmoothHeight(Instance:TWinControl;NewHeight,Increment:integer;Smooth:boolean);
var oldHeight:integer;
begin
  Instance.Parent.DisableAlign;
  Instance.DisableAlign;
  if Smooth then
  begin
    oldHeight := Instance.Height;
    if oldHeight < NewHeight then
      while oldHeight < NewHeight do
      begin
        oldHeight := oldHeight + Increment;
        Instance.Height := oldHeight;
        Instance.Invalidate;
      end
    else if oldHeight > NewHeight then
      while oldHeight > NewHeight do
      begin
        oldHeight := oldHeight - Increment;
        Instance.Height := oldHeight;
        Instance.Invalidate;
      end;
  end;
  Instance.Height := NewHeight;
  Instance.EnableAlign;
  Instance.Parent.EnableAlign;
end;

procedure SmoothWidth(Instance:TWinControl;NewWidth,Increment:integer;Smooth:boolean);
begin
  Instance.Parent.DisableAlign;
  Instance.DisableAlign;
  if Smooth then
  begin
    if Instance.Width < NewWidth then
      while Instance.Width < NewWidth do
        Instance.Width := Instance.Width + Increment
    else if Instance.Width > NewWidth then
      while Instance.Width > NewWidth do
        Instance.Width := Instance.Width - Increment;
  end;
  Instance.Width := NewWidth;
  Instance.EnableAlign;
  Instance.Parent.EnableAlign;
end;


{ TJvCustomRollOut }

constructor TJvCustomRollOut.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FGroupIndex := 0;
  Caption := 'Rollout';
  FHiText := clBlack;
  FCollapsed := false;
  FMouseDown := False;
  FInsideButton := False;
  FImageOffset := 5;
  FChildOffset := 0;
  FButtonHeight := 20;
  FPlacement := plTop;
  FButtonColor := clBtnFace;
  FTopColor := clBtnShadow;
  FBottomColor := clBtnHighLight;
  FBTopColor := clBtnHighLight;
  FBBottomColor := clBtnShadow;
  FAWidth := 145;
  FAHeight := 170;
  FCWidth := 22;
  FCHeight := 22;
  SetBounds(0,0,145,170);
end;

destructor TJvCustomRollOut.Destroy;
begin
  inherited Destroy;
end;

procedure TJvCustomRollOut.Click;
begin
  SetCollapsed(not FCollapsed);
  inherited Click;
  RedrawControl(false);
end;


procedure TJvCustomRollOut.CreateWnd;
begin
  inherited CreateWnd;
  if not Collapsed then
    UpdateGroup;
end;

procedure TJvCustomRollOut.AlignControls(AControl: TControl; var Rect: TRect);
begin
  Rect.Left := Rect.Left + ChildOffset;
  if FPlacement = plTop then
    Rect.Top := Rect.Top + FButtonHeight
  else
    Rect.Left := Rect.Left + FButtonHeight;
  inherited AlignControls(AControl,Rect);
end;

procedure TJvCustomRollOut.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button,Shift,X,Y);
  if not FMouseDown then
  begin
    FMouseDown := true;
    RedrawControl(false);
  end;
end;

procedure TJvCustomRollOut.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button,Shift,X,Y);
  if FMouseDown then
  begin
    FMouseDown := False;
    RedrawControl(false);
  end;
end;

procedure TJvCustomRollOut.MouseMove(Shift: TShiftState; X, Y: Integer);
var b1:boolean;
begin
  b1 := FInsideButton;
  inherited MouseMove(Shift,X,Y);
  FInsideButton := PtInRect(FButtonRect,Point(X,Y));
  if (FInsideButton <> b1) then
    RedrawControl(false);
end;

procedure TJvCustomRollOut.RedrawControl(DrawAll:boolean);
begin
  if DrawAll then
    Invalidate
  else
    DrawButtonFrame;
end;

procedure TJvCustomRollOut.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    if not Collapsed then UpdateGroup;
  end;
end;

procedure TJvCustomRollOut.SetPlacement(Value:TJvPlacement);
begin
  if FPlacement <> Value then
  begin
    FPlacement := Value;
    if Collapsed then
    begin
      if FPlacement = plTop then
        Height := FCHeight
      else
        Width := FCWidth;
    end
    else
    begin
      if FPlacement = plTop then
        Height := FAHeight
      else
        Width := FAWidth;
    end;
    if FPlacement = plTop then
      FButtonRect := Rect(1,1,Width - 1,FButtonHeight - 1)
    else
      FButtonRect := Rect(1,1,FButtonHeight - 1,Height - 1);
    Realign;
    RedrawControl(True);
  end;
end;

procedure TJvCustomRollOut.SetExpandedImage(Value:integer);
begin
  if FExpandedImage <> Value then
  begin
    FExpandedImage := Value;
    RedrawControl(False);
  end;
end;

procedure TJvCustomRollOut.SetImageList(Value:TImageList);
begin
  FImageList := Value;
  RedrawControl(true);
end;

procedure TJvCustomRollOut.SetCollapsedImage(Value:integer);
begin
  if FCollapsedImage <> Value then
  begin
    FCollapsedImage := Value;
    RedrawControl(False);
  end;
end;

procedure TJvCustomRollOut.SetHiTextColor(Value:TColor);
begin
  if FHiText <> Value then
  begin
    FHiText := Value;
    RedrawControl(false);
  end;
end;

procedure TJvCustomRollOut.SetCollapsed(Value:boolean);
begin
  if FCollapsed <> Value then
  begin
    FCollapsed := Value;
    if Value then
    begin
      if FPlacement = plTop then
        SmoothHeight(self,FCHeight,FIncrement,FSmooth)
      else
        SmoothWidth(self,FCWidth,FIncrement,FSmooth);
      Collapsing;
    end
    else
    begin
      if FPlacement = plTop then
        SmoothHeight(self,FAHeight,FIncrement,FSmooth)
      else
        SmoothWidth(self,FAWidth,FIncrement,FSmooth);
      Expanding;
    end;
    if not Value then UpdateGroup;
  end;
end;

procedure TJvCustomRollOut.Expanding;
begin
  if Assigned(FOnExpand) then FOnExpand(self);
end;    //

procedure TJvCustomRollOut.Collapsing;
begin
  if Assigned(FOnCollapse) then FOnCollapse(self);
end;    //

procedure TJvCustomRollOut.SetBounds(ALeft,ATop,AWidth,AHeight:integer);
begin
  if FCollapsed then
  begin
    if Placement = plTop then
      FCHeight := AHeight
    else
      FCWidth := AWidth;
  end
  else
  begin
    if Placement = plTop then
      FAHeight := AHeight
    else
      FAWidth := AWidth;
  end;
  inherited SetBounds(ALeft,ATop,AWidth,AHeight);
  if not Collapsed then UpdateGroup;
end;

procedure TJvCustomRollOut.DefineProperties(Filer:TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('FAWidth',ReadAWidth,WriteAWidth,true);
  Filer.DefineProperty('FAHeight',ReadAHeight,WriteAHeight,true);
  Filer.DefineProperty('FCWidth',ReadCWidth,WriteCWidth,true);
  Filer.DefineProperty('FCHeight',ReadCHeight,WriteCHeight,true);
end;

procedure TJvCustomRollOut.WriteAWidth(Writer:TWriter);
begin
  Writer.WriteInteger(FAWidth);
end;

procedure TJvCustomRollOut.WriteAHeight(Writer:TWriter);
begin
  Writer.WriteInteger(FAHeight);
end;

procedure TJvCustomRollOut.WriteCWidth(Writer:TWriter);
begin
  Writer.WriteInteger(FCWidth);
end;

procedure TJvCustomRollOut.WriteCHeight(Writer:TWriter);
begin
  Writer.WriteInteger(FCHeight);
end;

procedure TJvCustomRollOut.ReadAWidth(Reader:TReader);
begin
  FAWidth := Reader.ReadInteger;
  if not Collapsed then
    SetBounds(Left,Top,FAWidth,Height);
end;

procedure TJvCustomRollOut.ReadAHeight(Reader:TReader);
begin
  FAHeight := Reader.ReadInteger;
  if not Collapsed then
    SetBounds(Left,Top,Width,FAHeight);
end;

procedure TJvCustomRollOut.ReadCWidth(Reader:TReader);
begin
  FCWidth := Reader.ReadInteger;
  if Collapsed and (Placement = plLeft) then
    SetBounds(Left,Top,FCWidth,Height);
end;

procedure TJvCustomRollOut.ReadCHeight(Reader:TReader);
begin
  FCHeight := Reader.ReadInteger;
  if Collapsed and (Placement = plTop) then
    SetBounds(Left,Top,Width,FCHeight);
end;

procedure TJvCustomRollOut.SetImageOffset(Value:integer);
begin
  if FImageOffset <> Value then
  begin
    FImageOffset := Value;
    RedrawControl(False);
  end;
end;

procedure TJvCustomRollOut.SetButtonColor(Value:TColor);
begin
  if FButtonColor <> Value then
  begin
    FButtonColor := Value;
    RedrawControl(false);
  end;
end;

procedure TJvCustomRollOut.SetButtonHeight(Value:integer);
begin
  if FButtonHeight <> Value then
  begin
    FButtonHeight := Value;
    if FPlacement = plTop then
      FButtonRect := Rect(BevelWidth,BevelWidth,Width - BevelWidth,FButtonHeight + BevelWidth)
    else
      FButtonRect := Rect(BevelWidth,BevelWidth,FButtonHeight + BevelWidth,Height - BevelWidth);
    ReAlign;
    RedrawControl(True);
  end;
end;

procedure TJvCustomRollOut.SetChildOffset(Value:integer);
begin
  if FChildOffset <> Value then
  begin
    FChildOffset := Value;
    ReAlign;
//    R := ClientRect;
//    AlignControls(nil,R);
  end;
end;

procedure TJvCustomRollOut.SetCaption(Value:TCaption);
begin
  FCaption := Value;
  ReDrawControl(True);
end;

procedure TJvCustomRollOut.SetTopColor(Value:TColor);
begin
  if FTopColor <> Value then
  begin
    FTopColor := Value;
    ReDRawControl(true);
  end;
end;

procedure TJvCustomRollOut.SetBTopColor(Value:TColor);
begin
  if FBTopColor <> Value then
  begin
    FBTopColor := Value;
    RedrawControl(true);
  end;
end;

procedure TJvCustomRollOut.SetBottomColor(Value:TColor);
begin
  if FBottomColor <> Value then
  begin
    FBottomColor := Value;
    RedrawControl(true);
  end;
end;

procedure TJvCustomRollOut.SetBBottomColor(Value:TColor);
begin
  if FBBottomColor <> Value then
  begin
    FBBottomColor := Value;
    ReDrawControl(true);
  end;
end;

procedure TJvCustomRollOut.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  RedrawControl(false);
end;

procedure TJvCustomRollOut.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FInsideButton then
  begin
    FInsideButton := false;
    FMouseDown := False;
  end;
  RedrawControl(false);
end;

procedure TJvCustomRollOut.WMEraseBkgnd(var Message:TMessage);
begin
//  inherited;
  Message.Result := 0;
end;

procedure TJvCustomRollOut.DrawButtonFrame;
var R:TRect;TopC,BottomC:TColor;FIndex:integer;
begin
  if FPlacement = plTop then
    FButtonRect := Rect(BevelWidth,BevelWidth,Width - BevelWidth,FButtonHeight + BevelWidth)
  else
    FButtonRect := Rect(BevelWidth,BevelWidth,FButtonHeight + BevelWidth,Height - BevelWidth);

  R := FButtonRect;
  Canvas.Brush.Color := ButtonColor;
  Canvas.FillRect(R);

  if FMouseDown and FInsideButton then
  begin
    TopC := FBBottomColor;
    BottomC := FBTopColor;
  end
  else if FInsideButton then
  begin
    TopC := FBTopColor;
    BottomC := FBBottomColor;
  end
  else
  begin
    TopC := Color;
    BottomC := Color;
  end;

  Frame3D(Canvas,R,TopC,BottomC,1);
  if Collapsed then
    FIndex := FCollapsedImage
  else
    FIndex := FExpandedImage;

  R := FButtonRect;
  if FPlacement = plTop then
  begin
    if Assigned(FImageList) then
    begin
      FImageList.Draw(Canvas,FImageOffset + BevelWidth,BevelWidth + (FButtonHeight - FImageList.Height) div 2,FIndex);
      R.Left := FImageList.Width + FImageOffset * 2 + BevelWidth;
    end
    else
      R.Left := FImageOffset * 2 + BevelWidth;
    R.Top := R.Top - (Canvas.TextHeight(FCaption) - (FButtonRect.Bottom - FButtonRect.Top)) div 2 + BevelWidth div 2;
  end
  else
  begin
    if Assigned(FImageList) then
    begin
      FImageList.Draw(Canvas,BevelWidth + (FButtonHeight + - FImageList.Width) div 2,FImageOffset + BevelWidth,FIndex);
      R.Top := FImageList.Height + FImageOffset * 2 + BevelWidth;
    end
    else
      R.Top := FImageOffset * 2 + BevelWidth;
    R.Left := R.Left + (Canvas.TextHeight(FCaption) + (FButtonRect.Right - FButtonRect.Left)) div 2 + BevelWidth div 2;
  end;
  Canvas.Font := Font;
  if FInsideButton then
    Canvas.Font.Color := FHiText;

  if Length(FCaption) > 0 then
  begin
    SetBkMode(Canvas.Handle,Transparent);
    if Placement = plLeft then
      SetTextAngle(Canvas,270);
    if FMouseDown and FInsideButton then
      OffsetRect(R,1,1);
    DrawText(Canvas.Handle,PChar(FCaption),-1,R,DT_NOCLIP);
    if Placement = plLeft then
      SetTextAngle(Canvas,0);
  end;
end;

procedure TJvCustomRollOut.Paint;
var R:TRect;
begin
  R := ClientRect;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(R);
  Frame3D(Canvas,R,FTopColor,FBottomColor,BevelWidth);
  DrawButtonFrame;
end;

procedure TJvCustomRollOut.Collapse;
begin
  SetCollapsed(True);
end;

procedure TJvCustomRollOut.Expand;
begin
  SetCollapsed(False);
end;

procedure TJvCustomRollOut.UpdateGroup;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_EXPANDED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := Longint(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TJvCustomRollOut.CMExpanded(var Message: TMessage);
var
  Sender: TJvCustomRollOut;
begin
  if Message.WParam = FGroupIndex then
  begin
    Sender := TJvCustomRollOut(Message.LParam);
    if Sender <> Self then
    begin
      SetCollapsed(true);
      Invalidate;
    end;
  end;
end;

function IsAccel(VK: Word; const Str: string): Boolean;
var
  P: Integer;
begin
  P := Pos('&', Str);
  Result := (P <> 0) and (P < Length(Str)) and
    (AnsiCompareText(Str[P + 1], Char(VK)) = 0);
end;

procedure TJvCustomRollOut.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, FCaption) and Enabled then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;


end.


