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

Last Modified: 2003-03-23

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  Doesn't draw an underline for speed-keys (the '&' character ) if
  Placement = plLeft. Something with DrawText ?

Changes 2003-03-23:
  * Several properties have changed and been put into nested sub-properties.
    To update current usage do the following:
     - Color: change to Colors.Color
     - ButtonColor: change to Colors.ButtonColor
     - ButtonColTop: change to Colors.ButtonTop
     - ButtonColBtm: change to Colors.ButtonBottom
     - ColHiText: change to Colors.HotTrackText
     - FrameColTop: change to Colors.FrameTop
     - FrameColBtm: change to Colors.FrameBottom
     - ImageExpanded: change to ImageOptions.IndexExpanded
     - ImageCollapsed: change to ImageOptions.IndexCollapsed
     - ImageList: change to ImageOptions.Images
     - ImageOffset: change to ImageOptions.Offset // peter3

-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvRollOut;

{ TJvRollout is an autoexpanding / collapsing panel. }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, ImgList, Controls, ExtCtrls,
  JvComponent;

const
  CM_EXPANDED = WM_USER + 155;

type
  TJvPlacement = (plTop, plLeft);
  TJvRolloutColors = class(TPersistent)
  private
    FFrameBottom: TColor;
    FHotTrackText: TColor;
    FFrameTop: TColor;
    FColor: TColor;
    FButtonTop: TColor;
    FButtonBottom: TColor;
    FOnChange: TNotifyEvent;
    FButtonColor: TColor;
    procedure SetButtonBottom(const Value: TColor);
    procedure SetButtonTop(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure SetFrameBottom(const Value: TColor);
    procedure SetFrameTop(const Value: TColor);
    procedure SetHotTrackText(const Value: TColor);
    procedure SetButtonColor(const Value: TColor);
  protected
    procedure Change;
  public
    constructor Create;
  published
    property ButtonBottom: TColor read FButtonBottom write SetButtonBottom default clBtnShadow;
    property ButtonTop: TColor read FButtonTop write SetButtonTop default clBtnHighlight;
    property ButtonColor:TColor read FButtonColor write SetButtonColor default clBtnFace;
    property HotTrackText: TColor read FHotTrackText write SetHotTrackText default clWindowText;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property FrameBottom: TColor read FFrameBottom write SetFrameBottom default clBtnHighlight;
    property FrameTop: TColor read FFrameTop write SetFrameTop default clBtnShadow;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvRollOutImageOptions = class(TPersistent)
  private
    FOffset: integer;
    FImages: TCustomImageList;
    FIndexCollapsed: TImageIndex;
    FIndexExpanded: TImageIndex;
    FOnChange: TNotifyEvent;
    FChangeLink:TChangeLink;
    FOwner:TComponent;
    procedure SetImages(const Value: TCustomImageList);
    procedure SetIndexCollapsed(const Value: TImageIndex);
    procedure SetIndexExpanded(const Value: TImageIndex);
    procedure SetOffset(const Value: integer);
  protected
    procedure Change;
    procedure DoChangeLink(Sender:TObject);
  public
    constructor Create;
    destructor Destroy;override;
  published
    property IndexCollapsed: TImageIndex read FIndexCollapsed write SetIndexCollapsed default 1;
    property IndexExpanded: TImageIndex read FIndexExpanded write SetIndexExpanded  default 0;
    property Images: TCustomImageList read FImages write SetImages;
    property Offset: integer read FOffset write SetOffset default 5;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvCustomRollOut = class(TJvCustomPanel)
  private
    FGroupIndex: Integer;
    FButtonRect: TRect;
    FPlacement: TJvPlacement;
    FCollapsed: Boolean;
    FMouseDown: Boolean;
    FInsideButton: Boolean;
    FCWidth: Integer;
    FCHeight: Integer;
    FAWidth: Integer;
    FAHeight: Integer;
    FButtonHeight: Integer;
    FChildOffset: Integer;
    FCaption: TCaption;
    FOnExpand: TNotifyEvent;
    FOnCollapse: TNotifyEvent;
    FColors: TJvRollOutColors;
    FImageOptions: TJvRollOutImageOptions;

    procedure SetGroupIndex(Value: Integer);
    procedure SetPlacement(Value: TJvPlacement);
    procedure WriteAWidth(Writer: TWriter);
    procedure WriteAHeight(Writer: TWriter);
    procedure WriteCWidth(Writer: TWriter);
    procedure WriteCHeight(Writer: TWriter);
    procedure ReadAWidth(Reader: TReader);
    procedure ReadAHeight(Reader: TReader);
    procedure ReadCWidth(Reader: TReader);
    procedure ReadCHeight(Reader: TReader);
    procedure SetCollapsed(Value: Boolean);
    procedure SetButtonHeight(Value: Integer);
    procedure SetChildOffset(Value: Integer);
    procedure SetCaption(Value: TCaption);
    procedure RedrawControl(DrawAll: Boolean);
    procedure DrawButtonFrame;
    procedure UpdateGroup;
    procedure CMExpanded(var Msg: TMessage); message CM_EXPANDED;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure CMDialogChar(var Msg: TCMDialogChar); message CM_DIALOGCHAR;
    function GetParentColor: boolean;
    procedure SetParentColor(const Value: boolean);
  protected
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Expanding; virtual;
    procedure Collapsing; virtual;
    procedure Paint; override;
    procedure Click; override;
    procedure DoImageOptionsChange(Sender:TObject);
    procedure DoColorsChange(Sender:TObject);

    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight default 20;
    property ChildOffset: Integer read FChildOffset write SetChildOffset default 0;
    property Collapsed: Boolean read FCollapsed write SetCollapsed default False;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Placement: TJvPlacement read FPlacement write SetPlacement default plTop;
    property Colors: TJvRollOutColors read FColors write FColors;
    property ImageOptions: TJvRolloutImageOptions read FImageOptions write FImageOptions;

    property OnCollapse: TNotifyEvent read FOnCollapse write FOnCollapse;
    property OnExpand: TNotifyEvent read FOnExpand write FOnExpand;
    property ParentColor:boolean read GetParentColor write SetParentColor;
    property Caption: TCaption read FCaption write SetCaption;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure Collapse; virtual;
    procedure Expand; virtual;
  end;

  TJvRollout = class(TJvCustomRollOut)
  published
    property Align;
    property BevelWidth;
    property BorderWidth;
    property Caption;
    property ChildOffset;
    property Collapsed;
    property Colors;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property GroupIndex;
    property ImageOptions;
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
    else if OldHeight > NewHeight then
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
    else if Instance.Width > NewWidth then
      while Instance.Width > NewWidth do
        Instance.Width := Instance.Width - Increment;
  end;
  Instance.Width := NewWidth;
  Instance.EnableAlign;
  Instance.Parent.EnableAlign;
end;

{ TJvRollOutImageOptions }

procedure TJvRollOutImageOptions.Change;
begin
  if Assigned(FOnChange) then FOnChange(self);
end;

constructor TJvRollOutImageOptions.Create;
begin
  inherited Create;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := DoChangeLink;
  FIndexCollapsed := 1;
  FIndexExpanded := 0;
  FOffset := 5;
end;

destructor TJvRollOutImageOptions.Destroy;
begin
  FChangeLink.Free;
  inherited;
end;

procedure TJvRollOutImageOptions.DoChangeLink(Sender: TObject);
begin
  Change;
end;

procedure TJvRollOutImageOptions.SetImages(const Value: TCustomImageList);
begin
  if FImages <> nil then
  begin
    FImages.UnRegisterChanges(FChangeLink);
    FImages.RemoveFreeNotification(FOwner);
  end;

  FImages := Value;
  if FImages <> nil then
  begin
    FImages.RegisterChanges(FChangeLink);
    FImages.FreeNotification(FOwner);
  end;
end;

procedure TJvRollOutImageOptions.SetIndexCollapsed(
  const Value: TImageIndex);
begin
  if FIndexCollapsed <> Value then
  begin
    FIndexCollapsed := Value;
    Change;
  end;
end;

procedure TJvRollOutImageOptions.SetIndexExpanded(
  const Value: TImageIndex);
begin
  if FIndexExpanded <> Value then
  begin
    FIndexExpanded := Value;
    Change;
  end;
end;

procedure TJvRollOutImageOptions.SetOffset(const Value: integer);
begin
  if FOffset <> Value then
  begin
    FOffset := Value;
    Change;
  end;
end;

{ TJvRolloutColors }

procedure TJvRolloutColors.Change;
begin
  if Assigned(FOnChange) then FOnChange(self);
end;

constructor TJvRolloutColors.Create;
begin
  inherited Create;
  FButtonBottom := clBtnShadow;
  FButtonTop := clBtnHighlight;
  FButtonColor := clBtnFace;
  FHotTrackText := clWindowText;
  FColor := clBtnFace;
  FFrameBottom := clBtnHighlight;
  FFrameTop := clBtnShadow;
end;

procedure TJvRolloutColors.SetButtonBottom(const Value: TColor);
begin
  if FButtonBottom <> Value then
  begin
    FButtonBottom := Value;
    Change;
  end;
end;

procedure TJvRolloutColors.SetButtonColor(const Value: TColor);
begin
  FButtonColor := Value;
end;

procedure TJvRolloutColors.SetButtonTop(const Value: TColor);
begin
  if FButtonTop <> Value then
  begin
    FButtonTop := Value;
    Change;
  end;
end;

procedure TJvRolloutColors.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Change;
  end;
end;

procedure TJvRolloutColors.SetFrameBottom(const Value: TColor);
begin
  if FFrameBottom <> Value then
  begin
    FFrameBottom := Value;
    Change;
  end;
end;

procedure TJvRolloutColors.SetFrameTop(const Value: TColor);
begin
  if FFrameTop <> Value then
  begin
    FFrameTop := Value;
    Change;
  end;
end;

procedure TJvRolloutColors.SetHotTrackText(const Value: TColor);
begin
  if FHotTrackText <> Value then
  begin
    FHotTrackText := Value;
    Change;
  end;
end;

{ TJvCustomRollOut }

constructor TJvCustomRollOut.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageOptions := TJvRolloutImageOptions.Create;
  FImageOptions.FOwner := self;
  FImageOptions.OnChange := DoImageOptionsChange;

  FColors := TJvRolloutColors.Create;
  FColors.OnChange := DoColorsChange;

  FGroupIndex := 0;
  Caption := 'Rollout';
  FCollapsed := False;
  FMouseDown := False;
  FInsideButton := False;
  FChildOffset := 0;
  FButtonHeight := 20;
  FPlacement := plTop;
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
  Canvas.Brush.Color := Colors.ButtonColor;
  Canvas.FillRect(R);

  if FMouseDown and FInsideButton then
  begin
    TopC := Colors.ButtonBottom;
    BottomC := Colors.ButtonTop;
  end
  else if FInsideButton then
  begin
    TopC := Colors.ButtonTop;
    BottomC := Colors.ButtonBottom;
  end
  else
  begin
    TopC := Colors.Color;
    BottomC := Colors.Color;
  end;

  Frame3D(Canvas, R, TopC, BottomC, 1);
  if Collapsed then
    FIndex := ImageOptions.IndexCollapsed
  else
    FIndex := ImageOptions.IndexExpanded;

  R := FButtonRect;
  if FPlacement = plTop then
  begin
    if Assigned(ImageOptions.Images) then
    begin
      ImageOptions.Images.Draw(Canvas, ImageOptions.Offset + BevelWidth,
        BevelWidth + (FButtonHeight - ImageOptions.Images.Height) div 2, FIndex);
      R.Left := ImageOptions.Images.Width + ImageOptions.Offset * 2 + BevelWidth;
    end
    else
      R.Left := ImageOptions.Offset * 2 + BevelWidth;
    R.Top := R.Top - (Canvas.TextHeight(FCaption) - (FButtonRect.Bottom - FButtonRect.Top)) div 2 + BevelWidth div 2;
  end
  else
  begin
    if Assigned(ImageOptions.Images) then
    begin
      ImageOptions.Images.Draw(Canvas, BevelWidth + (FButtonHeight - ImageOptions.Images.Width) div 2,
        ImageOptions.Offset + BevelWidth, FIndex);
      R.Top := ImageOptions.Images.Height + ImageOptions.Offset * 2 + BevelWidth;
    end
    else
      R.Top := ImageOptions.Offset * 2 + BevelWidth;
    R.Left := R.Left + (Canvas.TextHeight(FCaption) + (FButtonRect.Right - FButtonRect.Left)) div 2 + BevelWidth div 2;
  end;
  Canvas.Font := Font;
  if FInsideButton then
    Canvas.Font.Color := Colors.HotTrackText;

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
  Canvas.Brush.Color := Colors.Color;
  Canvas.FillRect(R);
  Frame3D(Canvas, R, Colors.FrameTop, Colors.FrameBottom, BevelWidth);
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

procedure TJvCustomRollOut.DoColorsChange(Sender: TObject);
begin
  RedrawControl(true);
end;

procedure TJvCustomRollOut.DoImageOptionsChange(Sender: TObject);
begin
  RedrawControl(true);
end;

function TJvCustomRollOut.GetParentColor: boolean;
begin
  Result := inherited ParentColor;
end;

procedure TJvCustomRollOut.SetParentColor(const Value: boolean);
begin
  inherited ParentColor := Value;
  if Value then
    Colors.Color := self.Color;
end;

procedure TJvCustomRollOut.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = ImageOptions.Images) then
    ImageOptions.Images := nil;
end;

end.

