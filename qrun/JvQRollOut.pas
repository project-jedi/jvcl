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

The Original Code is: JvRollOut.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

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
// $Id$

{$I jvcl.inc}

unit JvQRollOut;

{ TJvRollOut is an autoexpanding / collapsing panel. }

interface

uses
  SysUtils, Classes,  
  QGraphics, QImgList, QControls, QExtCtrls,  QActnList, Types, QWindows, 
  JvQComponent, JvQThemes;

const
  CM_EXPANDED = WM_USER + 155;

type
  TJvPlacement = (plTop, plLeft);

  TJvRollOutColors = class(TPersistent)
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
    property ButtonColor: TColor read FButtonColor write SetButtonColor default clBtnFace;
    property HotTrackText: TColor read FHotTrackText write SetHotTrackText default clWindowText;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property FrameBottom: TColor read FFrameBottom write SetFrameBottom default clBtnHighlight;
    property FrameTop: TColor read FFrameTop write SetFrameTop default clBtnShadow;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvRollOutImageOptions = class(TPersistent)
  private
    FOffset: Integer;
    FImages: TCustomImageList;
    FIndexCollapsed: TImageIndex;
    FIndexExpanded: TImageIndex;
    FOnChange: TNotifyEvent;
    FChangeLink: TChangeLink;
    FOwner: TComponent;
    procedure SetImages(const Value: TCustomImageList);
    procedure SetIndexCollapsed(const Value: TImageIndex);
    procedure SetIndexExpanded(const Value: TImageIndex);
    procedure SetOffset(const Value: Integer);
  protected
    procedure Change;
    procedure DoChangeLink(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property IndexCollapsed: TImageIndex read FIndexCollapsed write SetIndexCollapsed default 1;
    property IndexExpanded: TImageIndex read FIndexExpanded write SetIndexExpanded default 0;
    property Images: TCustomImageList read FImages write SetImages;
    property Offset: Integer read FOffset write SetOffset default 5;
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
    FOnExpand: TNotifyEvent;
    FOnCollapse: TNotifyEvent;
    FColors: TJvRollOutColors;
    FImageOptions: TJvRollOutImageOptions;
    FToggleAnywhere: Boolean;
    FShowFocus: Boolean;
    FTabStops: TStringList;
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
    procedure RedrawControl(DrawAll: Boolean);
    procedure DrawButtonFrame;
    procedure UpdateGroup;
    procedure CMExpanded(var Msg: TMessage); message CM_EXPANDED;
    procedure ChangeHeight(NewHeight: Integer);
    procedure ChangeWidth(NewWidth: Integer);
    procedure SetShowFocus(const Value: Boolean);
  protected
    // Sets or gets the TabStop value of child controls depending on the value of Collapsed.
    // When Collapsed is True, calls GetChildTabStops.
    // When Collapsed is False, calls SetChildTabStops.
    procedure CheckChildTabStops;
    // Checks the TabStop value of all child controls and adds the control to
    // an internal list if TabStop is True. TabStop is then set to False.
    // This is done to disable tabbing into the child control when the rollout
    // is collapsed. Normally, you don't need to call this method.
    procedure GetChildTabStops;
    // Resets the TabStop value of child controls to True if they where added to
    // an internal list with a previous call to GetTabStops.
    // Does nothing if GetChildTabStops hasn't been called.
    // Normally, you don't need to call this method.
    procedure SetChildTabStops;
    // Clears the internal list with children TabStop values *without* restoring
    // the childrens TabStop values first.
    // Normally, you don't need to call this method.
    procedure ClearChildTabStops;

    procedure DoKillFocus(FocusedWnd: HWND); override;
    procedure DoSetFocus(FocusedWnd: HWND); override;
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    function WantKey(Key: Integer; Shift: TShiftState; const KeyText: WideString): Boolean; override;
    procedure ParentColorChanged; override;
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoExpand; dynamic;
    procedure DoCollapse; dynamic;
    procedure Paint; override;
    procedure Click; override;
    procedure DoImageOptionsChange(Sender: TObject);
    procedure DoColorsChange(Sender: TObject);
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight default 20;
    property ChildOffset: Integer read FChildOffset write SetChildOffset default 0;
    property Collapsed: Boolean read FCollapsed write SetCollapsed default False;
    property Colors: TJvRollOutColors read FColors write FColors;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property ImageOptions: TJvRollOutImageOptions read FImageOptions write FImageOptions;
    property Placement: TJvPlacement read FPlacement write SetPlacement default plTop;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default True;
    property ToggleAnywhere: Boolean read FToggleAnywhere write FToggleAnywhere default True;

    property OnCollapse: TNotifyEvent read FOnCollapse write FOnCollapse;
    property OnExpand: TNotifyEvent read FOnExpand write FOnExpand;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function MouseIsOnButton: Boolean;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure Collapse; virtual;
    procedure Expand; virtual;
  end;

  TJvRollOutAction = class(TAction)
  private
    FRollOut: TJvCustomRollOut;
    FLinkCheckedToCollapsed: Boolean;
    procedure SetRollOut(const Value: TJvCustomRollOut);
    procedure SetLinkCheckedToCollapsed(const Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
    function HandlesTarget(Target: TObject): Boolean; override;
    function Execute: Boolean; override;
    destructor Destroy; override;
  published
    property RollOut: TJvCustomRollOut read FRollOut write SetRollOut;
    property LinkCheckedToCollapsed: Boolean read FLinkCheckedToCollapsed write SetLinkCheckedToCollapsed;
  end;

  TJvRollOut = class(TJvCustomRollOut)
  published
    property Action;
    property Align;
    property BevelWidth;
    property BorderWidth;
    property ButtonHeight;
    property Caption;
    property ChildOffset;
    property Placement;
    property Collapsed;
    property Colors; 
    property DragMode;
    property Enabled;
    property Font;
    property GroupIndex;
    property ImageOptions; 
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowFocus;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property ToggleAnywhere;
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


uses
  QForms; 

// (p3) not used
// const
//  cIncrement = 24;
//  cSmooth = False;



procedure InternalFrame3D(Canvas: TCanvas; var Rect: TRect; TopColor, BottomColor: TColor; Width: Integer);

  procedure DoRect;
  var
    TopRight, BottomLeft: TPoint;
  begin
    with Canvas, Rect do
    begin
      TopRight.X := Right;
      TopRight.Y := Top;
      BottomLeft.X := Left;
      BottomLeft.Y := Bottom;
      if TopColor <> clNone then
      begin
        Pen.Color := TopColor;
        PolyLine([BottomLeft, TopLeft, TopRight]);
      end;
      if BottomColor <> clNone then
      begin
        Pen.Color := BottomColor;
        Dec(BottomLeft.X);
        PolyLine([TopRight, BottomRight, BottomLeft]);
      end;
    end;
  end;

begin
  Canvas.Pen.Width := 1;
  Dec(Rect.Bottom);
  Dec(Rect.Right);
  while Width > 0 do
  begin
    Dec(Width);
    DoRect;
    InflateRect(Rect, -1, -1);
  end;
  Inc(Rect.Bottom);
  Inc(Rect.Right);
end;

//=== { TJvRollOutImageOptions } =============================================

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
  inherited Destroy;
end;

procedure TJvRollOutImageOptions.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
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
    if FOwner <> nil then
      FImages.RemoveFreeNotification(FOwner);
  end;

  FImages := Value;
  if FImages <> nil then
  begin
    FImages.RegisterChanges(FChangeLink);
    if FOwner <> nil then
      FImages.FreeNotification(FOwner);
  end;
  Change;
end;

procedure TJvRollOutImageOptions.SetIndexCollapsed(const Value: TImageIndex);
begin
  if FIndexCollapsed <> Value then
  begin
    FIndexCollapsed := Value;
    Change;
  end;
end;

procedure TJvRollOutImageOptions.SetIndexExpanded(const Value: TImageIndex);
begin
  if FIndexExpanded <> Value then
  begin
    FIndexExpanded := Value;
    Change;
  end;
end;

procedure TJvRollOutImageOptions.SetOffset(const Value: Integer);
begin
  if FOffset <> Value then
  begin
    FOffset := Value;
    Change;
  end;
end;

//=== { TJvRollOutColors } ===================================================

constructor TJvRollOutColors.Create;
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

procedure TJvRollOutColors.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvRollOutColors.SetButtonBottom(const Value: TColor);
begin
  if FButtonBottom <> Value then
  begin
    FButtonBottom := Value;
    Change;
  end;
end;

procedure TJvRollOutColors.SetButtonColor(const Value: TColor);
begin
  if FButtonColor <> Value then
  begin
    FButtonColor := Value;
    Change;
  end;
end;

procedure TJvRollOutColors.SetButtonTop(const Value: TColor);
begin
  if FButtonTop <> Value then
  begin
    FButtonTop := Value;
    Change;
  end;
end;

procedure TJvRollOutColors.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Change;
  end;
end;

procedure TJvRollOutColors.SetFrameBottom(const Value: TColor);
begin
  if FFrameBottom <> Value then
  begin
    FFrameBottom := Value;
    Change;
  end;
end;

procedure TJvRollOutColors.SetFrameTop(const Value: TColor);
begin
  if FFrameTop <> Value then
  begin
    FFrameTop := Value;
    Change;
  end;
end;

procedure TJvRollOutColors.SetHotTrackText(const Value: TColor);
begin
  if FHotTrackText <> Value then
  begin
    FHotTrackText := Value;
    Change;
  end;
end;

//=== { TJvCustomRollOut } ===================================================

constructor TJvCustomRollOut.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IncludeThemeStyle(Self, [csNeedsBorderPaint, csParentBackground]);
  FImageOptions := TJvRollOutImageOptions.Create;
  FImageOptions.FOwner := Self;
  FImageOptions.OnChange := DoImageOptionsChange;

  FColors := TJvRollOutColors.Create;
  FColors.OnChange := DoColorsChange;
  FToggleAnywhere := True;
  FGroupIndex := 0;
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
  FShowFocus := True;
end;

destructor TJvCustomRollOut.Destroy;
begin
  FreeAndNil(FImageOptions);
  FreeAndNil(FTabStops);
  FreeAndNil(FColors);
  inherited Destroy;
end;

procedure TJvCustomRollOut.Click;
begin
  if (Action = nil) and (MouseIsOnButton or ToggleAnywhere) then
    Collapsed := not FCollapsed;
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
    if CanFocus {and not (csDesigning in ComponentState)} then
      SetFocus;
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
  begin 
    Canvas.Brush.Style := bsSolid; 
    Invalidate;
  end
  else
  begin 
    Canvas.Brush.Style := bsClear; 
    DrawButtonFrame;
  end;
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
      if Placement = plTop then
        ChangeHeight(FCHeight)
      else
        ChangeWidth(FCWidth);
      DoCollapse;
    end
    else
    begin
      if Placement = plTop then
        ChangeHeight(FAHeight)
      else
        ChangeWidth(FAWidth);
      DoExpand;
      UpdateGroup;
    end;
    CheckChildTabStops;
  end;
end;

procedure TJvCustomRollOut.ChangeHeight(NewHeight: Integer);
var
  OldHeight: Integer;
begin
  OldHeight := Height;
  Parent.DisableAlign;
  DisableAlign;
  try
    Height := NewHeight;
    if Align = alBottom then
      Top := Top + (OldHeight - NewHeight);
  finally
    EnableAlign;
    Parent.EnableAlign;
  end;
end;

procedure TJvCustomRollOut.ChangeWidth(NewWidth: Integer);
var
  OldWidth: Integer;
begin
  Parent.DisableAlign;
  DisableAlign;
  try
    OldWidth := Width;
    Width := NewWidth;
    if Align = alRight then
      Left := Left + (OldWidth - NewWidth);
  finally
    EnableAlign;
    Parent.EnableAlign;
  end;
end;

procedure TJvCustomRollOut.DoExpand;
begin
  if Assigned(FOnExpand) then
    FOnExpand(Self);
end;

procedure TJvCustomRollOut.DoCollapse;
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
    FCHeight := Value + 2;
    if FPlacement = plTop then
      FButtonRect := Rect(BevelWidth, BevelWidth, Width - BevelWidth, FButtonHeight + BevelWidth)
    else
      FButtonRect := Rect(BevelWidth, BevelWidth, FButtonHeight + BevelWidth, Height - BevelWidth);
    Realign;
    RedrawControl(True);
  end;
end;

procedure TJvCustomRollOut.SetChildOffset(Value: Integer);
begin
  if FChildOffset <> Value then
  begin
    FChildOffset := Value;
    Realign;
    //    R := ClientRect;
    //    AlignControls(nil,R);
  end;
end;

procedure TJvCustomRollOut.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if csDesigning in ComponentState then
    Exit;
  RedrawControl(False);
end;

procedure TJvCustomRollOut.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if csDesigning in ComponentState then
    Exit;
  if FInsideButton then
  begin
    FInsideButton := False;
    FMouseDown := False;
  end;
  RedrawControl(False);
end;

function TJvCustomRollOut.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  //  inherited DoPaintBackground(Canvas, Param);
  Result := False;
end;

procedure TJvCustomRollOut.DrawButtonFrame;
var
  R: TRect;
  TopC, BottomC: TColor;
  FIndex: Integer; 
  WS: WideString; 
begin
  if FPlacement = plTop then
    FButtonRect := Rect(BevelWidth, BevelWidth, Width - BevelWidth, FButtonHeight + BevelWidth)
  else
    FButtonRect := Rect(BevelWidth, BevelWidth, FButtonHeight + BevelWidth, Height - BevelWidth);

  R := FButtonRect;
  Canvas.Brush.Color := Colors.ButtonColor;
  if Canvas.Brush.Color <> clNone then
    Canvas.FillRect(R);

  if FMouseDown and FInsideButton then
  begin
    TopC := Colors.ButtonBottom;
    BottomC := Colors.ButtonTop;
  end
  else
  if FInsideButton then
  begin
    TopC := Colors.ButtonTop;
    BottomC := Colors.ButtonBottom;
  end
{ else
  if Focused then
  begin
    TopC := clHighlight;
    BottomC := clHighlight;
  end}
  else
  begin
    TopC := Colors.Color;
    BottomC := Colors.Color;
  end;
//  if not (csDesigning in ComponentState) then
  InternalFrame3D(Canvas, R, TopC, BottomC, 1);
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
    R.Top := R.Top - (Canvas.TextHeight(Caption) - (FButtonRect.Bottom - FButtonRect.Top)) div 2 + BevelWidth div 2;
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
    R.Left := R.Left + (Canvas.TextHeight(Caption) + (FButtonRect.Right - FButtonRect.Left)) div 2 + BevelWidth div 2;
  end;
  Canvas.Font := Font;
  if FInsideButton then
    Canvas.Font.Color := Colors.HotTrackText;

  if Length(Caption) > 0 then
  begin
    SetBkMode(Canvas.Handle, Transparent);
    if FMouseDown and FInsideButton then
      OffsetRect(R, 1, 1);  
    WS := Caption;
    SetPenColor(Canvas.Handle, Font.Color);
    if Placement = plLeft then
      DrawText(Canvas.Handle, WS, -1, R, DT_VCENTER, 270)
    else
      DrawText(Canvas.Handle, WS, -1, R, DT_VCENTER, 0) 
  end;
  if ShowFocus and Focused then
  begin
    R := FButtonRect;
    InflateRect(R, -2, -2);
    Canvas.DrawFocusRect(R);
  end;
end;

procedure TJvCustomRollOut.Paint;
var
  R: TRect;
begin
  R := ClientRect;
  if Colors.Color <> clNone then
  begin
    Canvas.Brush.Color := Colors.Color;
    DrawThemedBackground(Self, Canvas, R);
  end;
  InternalFrame3D(Canvas, R, Colors.FrameTop, Colors.FrameBottom, BevelWidth);
  if Colors.FrameTop = clNone then
  begin
    Dec(R.Left);
    Dec(R.Top);
  end;
  if Colors.FrameBottom = clNone then
  begin
    Inc(R.Right);
    Inc(R.Bottom);
  end;
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
    if (Sender <> Self) then
    begin
      SetCollapsed(True);
      CheckChildTabStops;
      Invalidate;
    end;
  end;
end;

(*
function IsAccel(VK: Word; const Str: string): Boolean;
var
  P: Integer;
begin
  P := Pos('&', Str);
  Result := (P <> 0) and (P < Length(Str)) and
    (AnsiCompareText(Str[P + 1], Char(VK)) = 0);
end;
*)

function TJvCustomRollOut.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := Enabled and (IsAccel(Key, Caption) and (ssAlt in Shift)) or ((Key = VK_SPACE) and Focused);
  if Result then
  begin
    SetCollapsed(not FCollapsed);
    if CanFocus then
      SetFocus;
  end
  else
    Result := inherited WantKey(Key, Shift, KeyText);
end;

procedure TJvCustomRollOut.DoColorsChange(Sender: TObject);
begin
  RedrawControl(True);
end;

procedure TJvCustomRollOut.DoImageOptionsChange(Sender: TObject);
begin
  RedrawControl(True);
end;

procedure TJvCustomRollOut.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (ImageOptions <> nil) and (AComponent = ImageOptions.Images) then
    ImageOptions.Images := nil;
end;

procedure TJvCustomRollOut.ParentColorChanged;
begin
  inherited ParentColorChanged;
  Colors.Color := Color;
end;

function TJvCustomRollOut.MouseIsOnButton: Boolean;
var
  P: TPoint;
  R: TRect;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);
  R := FButtonRect;
  // (p3) include edges in hit test
  InflateRect(R, 1, 1);
  Result := PtInRect(R, P);
end;

procedure TJvCustomRollOut.DoKillFocus(FocusedWnd: HWND);
begin
  CheckChildTabStops;
  inherited DoKillFocus(FocusedWnd);
  Invalidate;
end;

procedure TJvCustomRollOut.DoSetFocus(FocusedWnd: HWND);
begin
  CheckChildTabStops;
  inherited DoSetFocus(FocusedWnd);
  Invalidate;
end;

procedure TJvCustomRollOut.SetShowFocus(const Value: Boolean);
begin
  if FShowFocus <> Value then
  begin
    FShowFocus := Value;
    if Focused then
      Invalidate;
  end;
end;

procedure TJvCustomRollOut.CheckChildTabStops;
begin
  if csDesigning in ComponentState then
    Exit;
  if Collapsed then
    GetChildTabStops
  else
    SetChildTabStops;
end;

procedure TJvCustomRollOut.GetChildTabStops;
var
  I: Integer;
begin
  if FTabStops = nil then
  begin
    FTabStops := TStringList.Create;
    FTabStops.Sorted := True;
  end;
  for I := 0 to ControlCount - 1 do
    if (Controls[I] is TWinControl) and (TWinControl(Controls[I]).TabStop) then
    begin
      FTabStops.AddObject(Controls[I].Name, Controls[I]);
      TWinControl(Controls[I]).TabStop := False;
    end;
end;

procedure TJvCustomRollOut.SetChildTabStops;
var
  I: Integer;
begin
  if FTabStops <> nil then
  begin
    for I := 0 to FTabStops.Count - 1 do
      if FindChildControl(FTabStops[I]) <> nil then
        TWinControl(FTabStops.Objects[I]).TabStop := True;
    FreeAndNil(FTabStops);
  end;
end;

procedure TJvCustomRollOut.ClearChildTabStops;
begin
  FreeAndNil(FTabStops);
end;

//=== { TJvRollOutAction } ===================================================

destructor TJvRollOutAction.Destroy;
begin
  if RollOut <> nil then
    RollOut.RemoveFreeNotification(Self);
  inherited Destroy;
end;

function TJvRollOutAction.Execute: Boolean;
begin
  Result := inherited Execute;
  if Result then
  begin 
    if ActionComponent is TJvCustomRollOut then
    begin
      if LinkCheckedToCollapsed then
        TJvCustomRollOut(ActionComponent).Collapsed := not Checked
      else
        TJvCustomRollOut(ActionComponent).Collapsed := not TJvCustomRollOut(ActionComponent).Collapsed;
    end
    else 
    if RollOut <> nil then
    begin
      if LinkCheckedToCollapsed then
        RollOut.Collapsed := not Checked
      else
        RollOut.Collapsed := not RollOut.Collapsed;
    end;
  end;
end;  

procedure TJvRollOutAction.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
  if Target is TJvCustomRollOut then
  begin
    if LinkCheckedToCollapsed then
      TJvCustomRollOut(Target).Collapsed := not Checked
    else
      TJvCustomRollOut(Target).Collapsed := not TJvCustomRollOut(Target).Collapsed;
  end
  else
    if RollOut <> nil then
    begin
      if LinkCheckedToCollapsed then
        RollOut.Collapsed := not Checked
      else
        RollOut.Collapsed := not RollOut.Collapsed;
    end;
end;

function TJvRollOutAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := ((RollOut <> nil) and (Target = RollOut) or
    (RollOut = nil) and (Target is TJvCustomRollOut)) and TJvCustomRollOut(Target).Enabled;
end;

procedure TJvRollOutAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if AComponent = RollOut then
    RollOut := nil;
end;

procedure TJvRollOutAction.SetLinkCheckedToCollapsed(const Value: Boolean);
begin
  if FLinkCheckedToCollapsed <> Value then
  begin
    FLinkCheckedToCollapsed := Value;
    if FLinkCheckedToCollapsed then
    begin
      if RollOut <> nil then
        RollOut.Collapsed := not Checked
      else 
      if ActionComponent is TJvCustomRollOut then
        TJvCustomRollOut(ActionComponent).Collapsed := not Checked; 
    end;
  end;
end;

procedure TJvRollOutAction.SetRollOut(const Value: TJvCustomRollOut);
begin
  if FRollOut <> Value then
  begin
    if FRollOut <> nil then
      FRollOut.RemoveFreeNotification(Self);
    FRollOut := Value;
    if FRollOut <> nil then
      FRollOut.FreeNotification(Self);
  end;
end;

procedure TJvRollOutAction.UpdateTarget(Target: TObject);
begin
  if LinkCheckedToCollapsed then
    Checked := not (Target as TJvCustomRollOut).Collapsed;
end;

end.

