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
  Doesn't draw an underline for speed-keys (the '&' character ) if
  Placement = plLeft. Something with DrawText ?
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvRollOut;

{ TJvRollout is an autoexpanding / collapsing panel. }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ExtCtrls,
  JvComponent;

const
  CM_EXPANDED = WM_USER + 155;

type
  TJvPlacement = (plTop, plLeft);

  TJvCustomRollOut = class(TJvCustomPanel)
  private
    FGroupIndex: Integer;
    FImageList: TImageList;
    FButtonRect: TRect;
    FPlacement: TJvPlacement;
    FExpandedImage: Integer;
    FCollapsedImage: Integer;
    FCollapsed: Boolean;
    FMouseDown: Boolean;
    FInsideButton: Boolean;
    FCWidth: Integer;
    FCHeight: Integer;
    FAWidth: Integer;
    FAHeight: Integer;
    FImageOffset: Integer;
    FButtonHeight: Integer;
    FChildOffset: Integer;
    FCaption: TCaption;
    FTopColor: TColor;
    FBottomColor: TColor;
    FBTopColor: TColor;
    FBBottomColor: TColor;
    FButtonColor: TColor;
    FHiText: TColor;
    FOnExpand: TNotifyEvent;
    FOnCollapse: TNotifyEvent;
    procedure SetGroupIndex(Value: Integer);
    procedure SetPlacement(Value: TJvPlacement);
    procedure SetExpandedImage(Value: Integer);
    procedure SetCollapsedImage(Value: Integer);
    procedure WriteAWidth(Writer: TWriter);
    procedure WriteAHeight(Writer: TWriter);
    procedure WriteCWidth(Writer: TWriter);
    procedure WriteCHeight(Writer: TWriter);
    procedure ReadAWidth(Reader: TReader);
    procedure ReadAHeight(Reader: TReader);
    procedure ReadCWidth(Reader: TReader);
    procedure ReadCHeight(Reader: TReader);
    procedure SetHiTextColor(Value: TColor);
    procedure SetCollapsed(Value: Boolean);
    procedure SetImageList(Value: TImageList);
    procedure SetImageOffset(Value: Integer);
    procedure SetButtonHeight(Value: Integer);
    procedure SetChildOffset(Value: Integer);
    procedure SetCaption(Value: TCaption);
    procedure SetTopColor(Value: TColor);
    procedure SetBottomColor(Value: TColor);
    procedure SetBTopColor(Value: TColor);
    procedure SetBBottomColor(Value: TColor);
    procedure SetButtonColor(Value: TColor);
    procedure RedrawControl(DrawAll: Boolean);
    procedure DrawButtonFrame;
    procedure UpdateGroup;
    procedure CMExpanded(var Msg: TMessage); message CM_EXPANDED;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure CMDialogChar(var Msg: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    procedure CreateWnd; override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Expanding; virtual;
    procedure Collapsing; virtual;
    procedure Paint; override;
    procedure Click; override;
    property ButtonColor: TColor read FButtonColor write SetButtonColor default clBtnFace;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight default 20;
    property ButtonColTop: TColor read FBTopColor write SetBTopColor default clBtnHighLight;
    property ButtonColBtm: TColor read FBBottomColor write SetBBottomColor default clBtnShadow;
    property ColHiText: TColor read FHiText write SetHiTextColor default clBlack;
    property ChildOffset: Integer read FChildOffset write SetChildOffset default 0;
    property Collapsed: Boolean read FCollapsed write SetCollapsed default False;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property ImageExpanded: Integer read FExpandedImage write SetExpandedImage default 0;
    property ImageCollapsed: Integer read FCollapsedImage write SetCollapsedImage default 0;
    property ImageList: TImageList read FImageList write SetImageList;
    property ImageOffset: Integer read FImageOffset write SetImageOffset default 5;
    property FrameColTop: TColor read FTopColor write SetTopColor default clBtnShadow;
    property FrameColBtm: TColor read FBottomColor write SetBottomColor default clBtnHighLight;
    property Placement: TJvPlacement read FPlacement write SetPlacement default plTop;
    property OnCollapse: TNotifyEvent read FOnCollapse write FOnCollapse;
    property OnExpand: TNotifyEvent read FOnExpand write FOnExpand;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure Collapse; virtual;
    procedure Expand; virtual;
  published
    // (rom) why published?
    property Caption: TCaption read FCaption write SetCaption;
  end;

  TJvRollout = class(TJvCustomRollOut)
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
  cIncrement = 24;
  cSmooth = False;

procedure SetTextAngle(Cnv: TCanvas; Angle: Integer);
var
  FntLogRec: TLogFont;
begin
  GetObject(Cnv.Font.Handle, SizeOf(FntLogRec), Addr(FntLogRec));
  FntLogRec.lfEscapement := Angle * 10;
  FntLogRec.lfOutPrecision := OUT_TT_ONLY_PRECIS;
  Cnv.Font.Handle := CreateFontIndirect(FntLogRec);
end;

procedure SmoothHeight(Instance: TWinControl; NewHeight, Increment: Integer; Smooth: Boolean);
var
  OldHeight: Integer;
begin
  Instance.Parent.DisableAlign;
  Instance.DisableAlign;
  if Smooth then
  begin
    OldHeight := Instance.Height;
    if OldHeight < NewHeight then
      while OldHeight < NewHeight do
      begin
        OldHeight := OldHeight + Increment;
        Instance.Height := OldHeight;
        Instance.Invalidate;
      end
    else
    if OldHeight > NewHeight then
      while OldHeight > NewHeight do
      begin
        OldHeight := OldHeight - Increment;
        Instance.Height := OldHeight;
        Instance.Invalidate;
      end;
  end;
  Instance.Height := NewHeight;
  Instance.EnableAlign;
  Instance.Parent.EnableAlign;
end;

procedure SmoothWidth(Instance: TWinControl; NewWidth, Increment: Integer; Smooth: Boolean);
begin
  Instance.Parent.DisableAlign;
  Instance.DisableAlign;
  if Smooth then
  begin
    if Instance.Width < NewWidth then
      while Instance.Width < NewWidth do
        Instance.Width := Instance.Width + Increment
    else
    if Instance.Width > NewWidth then
      while Instance.Width > NewWidth do
        Instance.Width := Instance.Width - Increment;
  end;
  Instance.Width := NewWidth;
  Instance.EnableAlign;
  Instance.Parent.EnableAlign;
end;

constructor TJvCustomRollOut.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGroupIndex := 0;
  Caption := 'Rollout';
  FHiText := clBlack;
  FCollapsed := False;
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
  SetBounds(0, 0, 145, 170);
  FAWidth := 145;
  FAHeight := 170;
  FCWidth := 22;
  FCHeight := 22;
end;

destructor TJvCustomRollOut.Destroy;
begin
  inherited Destroy;
end;

procedure TJvCustomRollOut.Click;
begin
  SetCollapsed(not FCollapsed);
  inherited Click;
  RedrawControl(False);
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
  inherited AlignControls(AControl, Rect);
end;

procedure TJvCustomRollOut.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if not FMouseDown then
  begin
    FMouseDown := True;
    RedrawControl(False);
  end;
end;

procedure TJvCustomRollOut.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FMouseDown then
  begin
    FMouseDown := False;
    RedrawControl(False);
  end;
end;

procedure TJvCustomRollOut.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  B: Boolean;
begin
  B := FInsideButton;
  inherited MouseMove(Shift, X, Y);
  FInsideButton := PtInRect(FButtonRect, Point(X, Y));
  if FInsideButton <> B then
    RedrawControl(False);
end;

procedure TJvCustomRollOut.RedrawControl(DrawAll: Boolean);
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
    if not Collapsed then
      UpdateGroup;
  end;
end;

procedure TJvCustomRollOut.SetPlacement(Value: TJvPlacement);
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
      FButtonRect := Rect(1, 1, Width - 1, FButtonHeight - 1)
    else
      FButtonRect := Rect(1, 1, FButtonHeight - 1, Height - 1);
    Realign;
    RedrawControl(True);
  end;
end;

procedure TJvCustomRollOut.SetExpandedImage(Value: Integer);
begin
  if FExpandedImage <> Value then
  begin
    FExpandedImage := Value;
    RedrawControl(False);
  end;
end;

procedure TJvCustomRollOut.SetImageList(Value: TImageList);
begin
  FImageList := Value;
  RedrawControl(True);
end;

procedure TJvCustomRollOut.SetCollapsedImage(Value: Integer);
begin
  if FCollapsedImage <> Value then
  begin
    FCollapsedImage := Value;
    RedrawControl(False);
  end;
end;

procedure TJvCustomRollOut.SetHiTextColor(Value: TColor);
begin
  if FHiText <> Value then
  begin
    FHiText := Value;
    RedrawControl(False);
  end;
end;

procedure TJvCustomRollOut.SetCollapsed(Value: Boolean);
begin
  if FCollapsed <> Value then
  begin
    FCollapsed := Value;
    if Value then
    begin
      if FPlacement = plTop then
        SmoothHeight(Self, FCHeight, cIncrement, cSmooth)
      else
        SmoothWidth(Self, FCWidth, cIncrement, cSmooth);
      Collapsing;
    end
    else
    begin
      if FPlacement = plTop then
        SmoothHeight(Self, FAHeight, cIncrement, cSmooth)
      else
        SmoothWidth(Self, FAWidth, cIncrement, cSmooth);
      Expanding;
    end;
    if not Value then
      UpdateGroup;
  end;
end;

procedure TJvCustomRollOut.Expanding;
begin
  if Assigned(FOnExpand) then
    FOnExpand(Self);
end;

procedure TJvCustomRollOut.Collapsing;
begin
  if Assigned(FOnCollapse) then
    FOnCollapse(Self);
end;

procedure TJvCustomRollOut.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
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
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if not Collapsed then
    UpdateGroup;
end;

procedure TJvCustomRollOut.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('FAWidth', ReadAWidth, WriteAWidth, True);
  Filer.DefineProperty('FAHeight', ReadAHeight, WriteAHeight, True);
  Filer.DefineProperty('FCWidth', ReadCWidth, WriteCWidth, True);
  Filer.DefineProperty('FCHeight', ReadCHeight, WriteCHeight, True);
end;

procedure TJvCustomRollOut.WriteAWidth(Writer: TWriter);
begin
  Writer.WriteInteger(FAWidth);
end;

procedure TJvCustomRollOut.WriteAHeight(Writer: TWriter);
begin
  Writer.WriteInteger(FAHeight);
end;

procedure TJvCustomRollOut.WriteCWidth(Writer: TWriter);
begin
  Writer.WriteInteger(FCWidth);
end;

procedure TJvCustomRollOut.WriteCHeight(Writer: TWriter);
begin
  Writer.WriteInteger(FCHeight);
end;

procedure TJvCustomRollOut.ReadAWidth(Reader: TReader);
begin
  FAWidth := Reader.ReadInteger;
  if not Collapsed and (Placement = plLeft) then
    SetBounds(Left, Top, FAWidth, Height);
end;

procedure TJvCustomRollOut.ReadAHeight(Reader: TReader);
begin
  FAHeight := Reader.ReadInteger;
  if not Collapsed and (Placement = plTop) then
    SetBounds(Left, Top, Width, FAHeight);
end;

procedure TJvCustomRollOut.ReadCWidth(Reader: TReader);
begin
  FCWidth := Reader.ReadInteger;
  if Collapsed and (Placement = plLeft) then
    SetBounds(Left, Top, FCWidth, Height);
end;

procedure TJvCustomRollOut.ReadCHeight(Reader: TReader);
begin
  FCHeight := Reader.ReadInteger;
  if Collapsed and (Placement = plTop) then
    SetBounds(Left, Top, Width, FCHeight);
end;

procedure TJvCustomRollOut.SetImageOffset(Value: Integer);
begin
  if FImageOffset <> Value then
  begin
    FImageOffset := Value;
    RedrawControl(False);
  end;
end;

procedure TJvCustomRollOut.SetButtonColor(Value: TColor);
begin
  if FButtonColor <> Value then
  begin
    FButtonColor := Value;
    RedrawControl(False);
  end;
end;

procedure TJvCustomRollOut.SetButtonHeight(Value: Integer);
begin
  if FButtonHeight <> Value then
  begin
    FButtonHeight := Value;
    if FPlacement = plTop then
      FButtonRect := Rect(BevelWidth, BevelWidth, Width - BevelWidth, FButtonHeight + BevelWidth)
    else
      FButtonRect := Rect(BevelWidth, BevelWidth, FButtonHeight + BevelWidth, Height - BevelWidth);
    ReAlign;
    RedrawControl(True);
  end;
end;

procedure TJvCustomRollOut.SetChildOffset(Value: Integer);
begin
  if FChildOffset <> Value then
  begin
    FChildOffset := Value;
    ReAlign;
//    R := ClientRect;
//    AlignControls(nil,R);
  end;
end;

procedure TJvCustomRollOut.SetCaption(Value: TCaption);
begin
  FCaption := Value;
  ReDrawControl(True);
end;

procedure TJvCustomRollOut.SetTopColor(Value: TColor);
begin
  if FTopColor <> Value then
  begin
    FTopColor := Value;
    ReDRawControl(True);
  end;
end;

procedure TJvCustomRollOut.SetBTopColor(Value: TColor);
begin
  if FBTopColor <> Value then
  begin
    FBTopColor := Value;
    RedrawControl(True);
  end;
end;

procedure TJvCustomRollOut.SetBottomColor(Value: TColor);
begin
  if FBottomColor <> Value then
  begin
    FBottomColor := Value;
    RedrawControl(True);
  end;
end;

procedure TJvCustomRollOut.SetBBottomColor(Value: TColor);
begin
  if FBBottomColor <> Value then
  begin
    FBBottomColor := Value;
    ReDrawControl(True);
  end;
end;

procedure TJvCustomRollOut.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  RedrawControl(False);
end;

procedure TJvCustomRollOut.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  if FInsideButton then
  begin
    FInsideButton := False;
    FMouseDown := False;
  end;
  RedrawControl(False);
end;

procedure TJvCustomRollOut.WMEraseBkgnd(var Msg: TMessage);
begin
//  inherited;
  Msg.Result := 0;
end;

procedure TJvCustomRollOut.DrawButtonFrame;
var
  R: TRect;
  TopC, BottomC: TColor;
  FIndex: Integer;
begin
  if FPlacement = plTop then
    FButtonRect := Rect(BevelWidth, BevelWidth, Width - BevelWidth, FButtonHeight + BevelWidth)
  else
    FButtonRect := Rect(BevelWidth, BevelWidth, FButtonHeight + BevelWidth, Height - BevelWidth);

  R := FButtonRect;
  Canvas.Brush.Color := ButtonColor;
  Canvas.FillRect(R);

  if FMouseDown and FInsideButton then
  begin
    TopC := FBBottomColor;
    BottomC := FBTopColor;
  end
  else
  if FInsideButton then
  begin
    TopC := FBTopColor;
    BottomC := FBBottomColor;
  end
  else
  begin
    TopC := Color;
    BottomC := Color;
  end;

  Frame3D(Canvas, R, TopC, BottomC, 1);
  if Collapsed then
    FIndex := FCollapsedImage
  else
    FIndex := FExpandedImage;

  R := FButtonRect;
  if FPlacement = plTop then
  begin
    if Assigned(FImageList) then
    begin
      FImageList.Draw(Canvas, FImageOffset + BevelWidth, BevelWidth + (FButtonHeight - FImageList.Height) div 2,
        FIndex);
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
      FImageList.Draw(Canvas, BevelWidth + (FButtonHeight + -FImageList.Width) div 2, FImageOffset + BevelWidth,
        FIndex);
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
    SetBkMode(Canvas.Handle, Transparent);
    if Placement = plLeft then
      SetTextAngle(Canvas, 270);
    if FMouseDown and FInsideButton then
      OffsetRect(R, 1, 1);
    DrawText(Canvas.Handle, PChar(FCaption), -1, R, DT_NOCLIP);
    if Placement = plLeft then
      SetTextAngle(Canvas, 0);
  end;
end;

procedure TJvCustomRollOut.Paint;
var
  R: TRect;
begin
  R := ClientRect;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(R);
  Frame3D(Canvas, R, FTopColor, FBottomColor, BevelWidth);
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

procedure TJvCustomRollOut.CMExpanded(var Msg: TMessage);
var
  Sender: TJvCustomRollOut;
begin
  if Msg.WParam = FGroupIndex then
  begin
    Sender := TJvCustomRollOut(Msg.LParam);
    if Sender <> Self then
    begin
      SetCollapsed(True);
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

procedure TJvCustomRollOut.CMDialogChar(var Msg: TCMDialogChar);
begin
  with Msg do
    if IsAccel(CharCode, FCaption) and Enabled then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;

end.

