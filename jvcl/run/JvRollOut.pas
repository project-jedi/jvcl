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
located at http://jvcl.delphi-jedi.org

Description:
  TJvRollOut is an autoexpanding / collapsing panel.

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

unit JvRollOut;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes,
  Windows, Forms, Messages, Controls, Graphics, ImgList, ExtCtrls, ActnList,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  JvExtComponent, JvThemes;

const
  CM_EXPANDED = WM_USER + 155;
  DefaultButtonColor = clBtnFace;
  DefaultHotTextColor = clWindowText;

type
  TJvPlacement = (plTop, plLeft);

  TJvRollOutButtonStyle = (bsButton, bsHeader);

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
    property ButtonColor: TColor read FButtonColor write SetButtonColor default DefaultButtonColor;
    property HotTrackText: TColor read FHotTrackText write SetHotTrackText default DefaultHotTextColor;
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
    FChildControlVisibility: TStringList;

    FButtonFont: TFont;
    FCollapsedList: array of Boolean;
    FSmartExpand: Boolean;
    FSmartShow: Boolean;
    FTopForm: TForm;
    FOldParent: TControl;
    FOldPos: TPoint;
    FOldWidthHeight: TPoint;
    FOldAlign: TAlign;

    FButtonStyle: TJvRollOutButtonStyle;
    FCollapseCtrlsOnButton: Boolean;
    FUseGroupBoxCaptionColor: Boolean;

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
    {$IFDEF RTL230_UP}
    procedure DrawThemedButtonFrame;
    {$ENDIF RTL230_UP}
    procedure UpdateGroup;
    procedure SetExpandedSize(const Value: Integer);
    procedure CMExpanded(var Msg: TMessage); message CM_EXPANDED;
    procedure ChangeHeight(NewHeight: Integer);
    procedure ChangeWidth(NewWidth: Integer);
    procedure SetShowFocus(const Value: Boolean);
    procedure SetButtonFont(const Value: TFont);

    procedure SetSmartExpand(const Value: Boolean);
    procedure OnTopDeactivate(Sender : TObject);
    procedure RestoreFromTopForm;
    procedure PutOnForm;
    function IsButtonFontStored: Boolean;
    procedure SetButtonStyle(const Value: TJvRollOutButtonStyle);
  protected
    // When the rollout-panel is collaped all contained controls are hidden
    //   to avoid tabbing into the child when the child is not visible or the
    //   rollout-caption-button being hidden by a contained control that is
    //   aligned tot he bottom
    // The original visiblility of each control is restored then the rollout
    //   is expanded again
    procedure CheckChildVisibility;

    procedure FocusKilled(NextWnd: THandle); override;
    procedure FocusSet(PrevWnd: THandle); override;
    function DoEraseBackground(Canvas: TCanvas; Param: LPARAM): Boolean; override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    function WantKey(Key: Integer; Shift: TShiftState): Boolean; override;
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
    procedure DoButtonFontChange(Sender: TObject);
    property ButtonFont: TFont read FButtonFont write SetButtonFont stored IsButtonFontStored;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight default 20;
    property ButtonStyle: TJvRollOutButtonStyle read FButtonStyle write SetButtonStyle default bsHeader;
    property ChildOffset: Integer read FChildOffset write SetChildOffset default 0;
    property CollapseCtrlsOnButton: Boolean read FCollapseCtrlsOnButton write FCollapseCtrlsOnButton default True;
    property Collapsed: Boolean read FCollapsed write SetCollapsed default False;
    property Colors: TJvRollOutColors read FColors write FColors;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property ImageOptions: TJvRollOutImageOptions read FImageOptions write FImageOptions;
    property Placement: TJvPlacement read FPlacement write SetPlacement default plTop;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default True;
    property ToggleAnywhere: Boolean read FToggleAnywhere write FToggleAnywhere default True;
    property SmartExpand: Boolean read FSmartExpand write SetSmartExpand default True;
    property SmartShow: Boolean read FSmartShow write FSmartShow default True;
    property UseGroupBoxCaptionColor: Boolean read FUseGroupBoxCaptionColor write FUseGroupBoxCaptionColor default False;

    property OnCollapse: TNotifyEvent read FOnCollapse write FOnCollapse;
    property OnExpand: TNotifyEvent read FOnExpand write FOnExpand;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function MouseIsOnButton: Boolean;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure Collapse; virtual;
    procedure Expand; virtual;

    property ExpandedSize: Integer write SetExpandedSize stored False;
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

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvRollOut = class(TJvCustomRollOut)
  published
    property Action;
    property Align;
    property BevelWidth;
    property BorderWidth;
    property ButtonFont;
    property ButtonHeight;
    property ButtonStyle;
    property Caption;
    property ChildOffset;
    property Placement;
    property CollapseCtrlsOnButton;
    property Collapsed;
    property Colors;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property GroupIndex;
    property ImageOptions;
    {$IFDEF JVCLThemesEnabled}
    property ParentBackground default True;
    {$ENDIF JVCLThemesEnabled}
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowFocus;
    property ShowHint;
    property SmartExpand;
    property SmartShow;
    property TabOrder;
    property TabStop;
    property ToggleAnywhere;
    property UseGroupBoxCaptionColor;
    property Visible;
    property OnClick;
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

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF RTL230_UP}
  Vcl.Themes, Vcl.Styles,
  {$ENDIF RTL230_UP}
  Types,
  JvJVCLUtils; // for IsAccel()

procedure SetTextAngle(Cnv: TCanvas; Angle: Integer);
var
  FntLogRec: TLogFont;
begin
  GetObject(Cnv.Font.Handle, SizeOf(FntLogRec), Addr(FntLogRec));
  FntLogRec.lfEscapement := Angle * 10;
  FntLogRec.lfOutPrecision := OUT_TT_ONLY_PRECIS;
  Cnv.Font.Handle := CreateFontIndirect(FntLogRec);
end;


procedure InternalFrame3D(Canvas: TCanvas; var Rect: TRect; TopColor, BottomColor: TColor; Width: Integer);

  procedure DoRect;
  var
    TopRight, BottomLeft: TPoint;
  begin
    TopRight.X := Rect.Right;
    TopRight.Y := Rect.Top;
    BottomLeft.X := Rect.Left;
    BottomLeft.Y := Rect.Bottom;
    if TopColor <> clNone then
    begin
      Canvas.Pen.Color := TopColor;
      Canvas.PolyLine([BottomLeft, Rect.TopLeft, TopRight]);
    end;
    if BottomColor <> clNone then
    begin
      Canvas.Pen.Color := BottomColor;
      Dec(BottomLeft.X);
      Canvas.PolyLine([TopRight, Rect.BottomRight, BottomLeft]);
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
  ReplaceImageListReference(FOwner, Value, FImages, FChangeLink);
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
  FButtonColor := DefaultButtonColor;
  FHotTrackText := DefaultHotTextColor;
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
  FButtonHeight := 20;
  FButtonStyle := bsHeader;
  FCollapseCtrlsOnButton := True;
  FPlacement := plTop;
  SetBounds(0, 0, 145, 170);
  FAWidth := 145;
  FAHeight := 170;
  FCWidth := 22;
  FCHeight := 22;
  FShowFocus := True;

  FButtonFont := TFont.Create;
  FButtonFont.Name := 'Verdana';
  FButtonFont.Size := 7;
  FButtonFont.Style := [fsBold];
  FButtonFont.Color := clWindowText;
  FButtonFont.OnChange := DoButtonFontChange;

  // SmartExpand / SmartShow
  FSmartExpand := True;
  FSmartShow := True;

  FTopForm := TForm.Create(self);
  with FTopForm do
  begin
    BorderStyle := bsNone;
    FormStyle := fsStayOnTop;
    OnDeactivate := OnTopDeactivate;
    Position := poDesigned;
  end;

  ControlStyle := ControlStyle - [csDoubleClicks];    // Doubleclicks are converted into single clicks
end;

destructor TJvCustomRollOut.Destroy;
begin
  FreeAndNil(FButtonFont);
  FreeAndNil(FImageOptions);
  FreeAndNil(FChildControlVisibility);
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
    Invalidate
  else
  {$IFDEF RTL230_UP}
  if StyleServices.Enabled then
    DrawThemedButtonFrame
  else
  {$ENDIF RTL230_UP}
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
    if FCollapsed then
    begin
      // If Rollout panel was floating (= mapped onto a special form)
      //   -> restore old state
      if FSmartShow and (FOldParent <> nil) then
        RestoreFromTopForm;

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
    CheckChildVisibility;
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
var 
  I: Integer;
  OldSmartExpand: Boolean;
begin
  // Smart-Expand: If there's not enough space to expand the rollup-panel
  //   then collapse the other rollout-panels
  if FSmartExpand then
  begin
    // Todo: SmartExpand was only made for panels that are bottom-aligned

    // Remember Collapsed status of all other TJvCustomRollOut components:
    SetLength(FCollapsedList, 0);
    if Assigned(Parent) and (Top + Height > Parent.Height) then
    begin
      for I := 0 to Parent.ControlCount-1 do
      begin
        if (Parent.Controls[I] is TJvCustomRollOut) and (Parent.Controls[I] <> Self) then
        begin
          SetLength(FCollapsedList, Length(FCollapsedList) + 1);
          FCollapsedList[Length(FCollapsedList) - 1] := (Parent.Controls[I] as TJvCustomRollOut).Collapsed;

          // Disable SmartExpand because it may cause troubles!!
          // especially when there is less space and another panel would be
          // shown obove the window (smartshow)
          OldSmartExpand := (Parent.Controls[I] as TJvCustomRollOut).SmartExpand;
          (Parent.Controls[I] as TJvCustomRollOut).SmartExpand := False;
          (Parent.Controls[I] as TJvCustomRollOut).Collapsed := True;

          (Parent.Controls[I] as TJvCustomRollOut).SmartExpand := OldSmartExpand;
        end;
      end;
    end;
  end;

  if FSmartShow then
    PutOnForm;

  if Assigned(FOnExpand) then
    FOnExpand(Self);
end;

procedure TJvCustomRollOut.DoCollapse;
var
  ColIndex: Integer;
  I : integer;
  DoRestore: Boolean;
begin
  // Smart-Expand: If other rollouts where collapsed automatically when this rollout
  //   expanded, then their old collapsed-state is now restored
  if FSmartExpand then
  begin
    DoRestore := Length(FCollapsedList)<>0;

    // Check if one of the auto-collapsed rollouts wad expanded manually
    // In this case we do not restore the old collapsed-states
    for I := 0 to Parent.ControlCount-1 do
    begin
      if (Parent.Controls[I] is TJvCustomRollOut) and
      (Parent.Controls[I] <> Self) then
      begin
        if (Parent.Controls[I] as TJvCustomRollOut).Collapsed = False then
        begin
          DoRestore := False;
          Break;
        end;
      end;
    end;

    if DoRestore then
    begin
      // Restore other rollouts
      ColIndex := 0;
      for I := 0 to Parent.ControlCount - 1 do
      begin
        if (Parent.Controls[I] is TJvCustomRollOut) and (Parent.Controls[I] <> Self) then
        begin
          (Parent.Controls[I] as TJvCustomRollOut).Collapsed := FCollapsedList[ColIndex];
          Inc(ColIndex);

          if ColIndex > Length(FCollapsedList) then
            Break;
        end;
      end;
    end;
    SetLength(FCollapsedList, 0);
  end;

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


procedure TJvCustomRollOut.SetButtonStyle(const Value: TJvRollOutButtonStyle);
begin
  if Value <> FButtonStyle then
  begin
    FButtonStyle := Value;
    RedrawControl(False);
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

procedure TJvCustomRollOut.SetSmartExpand(const Value: boolean);
begin
  FSmartExpand := Value;
  SetLength(FCollapsedList, 0);
end;

// To make Setting of expanded size possible, even if panel is collapsed
procedure TJvCustomRollOut.SetExpandedSize(const Value: integer);
begin
  if ((FPlacement = plTop) and (FAHeight = Value)) or
     ((FPlacement = plLeft) and (FAWidth = Value)) then
    Exit;

  if FPlacement = plTop then
    FAHeight := Value
  else
    FAWidth := Value;

  if not FCollapsed then
  begin
    // The top form is assigned so set the width and height of this form
    if Parent = FTopForm then
    begin
      FTopForm.DisableAlign;
      if FPlacement = plTop then
      begin
        FTopForm.Height := FAHeight;
        FOldWidthHeight.Y := FAHeight;
      end
      else
      begin
        FTopForm.Width := FAWidth;
        FOldWidthHeight.X := FAWidth;
      end;
      FTopForm.EnableAlign;
    end;

    if FPlacement = plTop then
      ChangeHeight(FAHeight)
    else
      ChangeWidth(FAWidth);

    if (Parent = FTopForm) and (FOldPos.Y + Height < FOldParent.Height) then
      RestoreFromTopForm
    else
      PutOnForm;
  end;
end;

procedure TJvCustomRollOut.SetButtonFont(const Value: TFont);
begin
  if Value <> FButtonFont then
    FButtonFont.Assign(Value);
end;

// Only store button font if not default value
function TJvCustomRollOut.IsButtonFontStored: Boolean;
begin
  Result := (FButtonFont.Name <> 'Verdana') or
            (FButtonFont.Size <> 7) or
            (FButtonFont.Style <> [fsBold]) or
            (FButtonFont.Color <> clWindowText);
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

function TJvCustomRollOut.DoEraseBackground(Canvas: TCanvas; Param: LPARAM): Boolean;
begin
  //  inherited DoEraseBackground(Canvas, Param);
  Result := False;
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

  Canvas.Font.Assign(FButtonFont);
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
  if FInsideButton then
    Canvas.Font.Color := Colors.HotTrackText;

  if Length(Caption) > 0 then
  begin
    SetBkMode(Canvas.Handle, Transparent);
    if FMouseDown and FInsideButton then
      OffsetRect(R, 1, 1);
    if Placement = plLeft then
      SetTextAngle(Canvas, 270);
    DrawText(Canvas.Handle, PChar(Caption), -1, R, DT_NOCLIP);
    if Placement = plLeft then
      SetTextAngle(Canvas, 0);
  end;
  if ShowFocus and Focused then
  begin
    R := FButtonRect;
    InflateRect(R, -2, -2);
    Canvas.DrawFocusRect(R);
  end;
end;

{$IFDEF RTL230_UP}
procedure TJvCustomRollOut.DrawThemedButtonFrame;
var
  R: TRect;
  State1: TThemedHeader;
  State2: TThemedButton;
  FIndex: Integer;
begin
  if FPlacement = plTop then
    FButtonRect := Rect(BevelWidth, BevelWidth, Width - BevelWidth, FButtonHeight + BevelWidth)
  else
    FButtonRect := Rect(BevelWidth, BevelWidth, FButtonHeight + BevelWidth, Height - BevelWidth);

  //Draw button
  if FButtonStyle = bsHeader then
  begin
    if not Enabled then
      State1 := thHeaderDontCare
    else
      if FMouseDown and FInsideButton then
        State1 := thHeaderItemPressed
      else
        if FInsideButton then
          State1 := thHeaderItemHot
        else
          State1 := thHeaderItemNormal;
    R := FButtonRect;
    StyleServices.DrawElement(Canvas.Handle, StyleServices.GetElementDetails(State1), R);
  end
  else //FButtonStyle = bsButton
  begin
    if not Enabled then
      State2 := tbPushButtonDisabled
    else
      if FMouseDown and FInsideButton then
        State2 := tbPushButtonPressed
      else
        if FInsideButton then
          State2 := tbPushButtonHot
        else
          if ShowFocus and Focused then
            State2 := tbPushButtonDefaulted
          else
            State2 := tbPushButtonNormal;
    R := FButtonRect;
    StyleServices.DrawElement(Canvas.Handle, StyleServices.GetElementDetails(State2), R);
  end;

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
        BevelWidth + (FButtonHeight - ImageOptions.Images.Height) div 2 - Integer(FButtonStyle = bsHeader) + Integer(FButtonStyle = bsButton), FIndex);
      R.Left := ImageOptions.Images.Width + ImageOptions.Offset * 2 + BevelWidth;
    end
    else
      R.Left := ImageOptions.Offset * 2 + BevelWidth;
    R.Top := R.Top - (Canvas.TextHeight(Caption) - (FButtonRect.Bottom - FButtonRect.Top)) div 2 + BevelWidth div 2 - 2*Integer(FButtonStyle = bsHeader);
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
    R.Left := R.Left + (Canvas.TextHeight(Caption) + (FButtonRect.Right - FButtonRect.Left)) div 2 + BevelWidth div 2 +2;
  end;

  //Draw caption
  Canvas.Font.Assign(FButtonFont);
  if FUseGroupBoxCaptionColor then
  begin
    if not Enabled then
      Canvas.Font.Color  := StyleServices.GetStyleFontColor(sfGroupBoxTextDisabled)
    else
      Canvas.Font.Color  := StyleServices.GetStyleFontColor(sfGroupBoxTextNormal);
  end
  else
  if not Enabled then
    Canvas.Font.Color  := StyleServices.GetStyleFontColor(sfHeaderSectionTextDisabled)
  else
  if FMouseDown and FInsideButton then
    Canvas.Font.Color  := StyleServices.GetStyleFontColor(sfHeaderSectionTextPressed)
  else
  if FInsideButton then
    Canvas.Font.Color  := StyleServices.GetStyleFontColor(sfHeaderSectionTextHot)
  else
    Canvas.Font.Color  := StyleServices.GetStyleFontColor(sfHeaderSectionTextNormal);

  if Length(Caption) > 0 then
  begin
    SetBkMode(Canvas.Handle, Transparent);
    if FMouseDown and FInsideButton then
      OffsetRect(R, 1, 1);
    if Placement = plLeft then
      SetTextAngle(Canvas, 270);
    DrawText(Canvas.Handle, PChar(Caption), -1, R, DT_NOCLIP);
    if Placement = plLeft then
      SetTextAngle(Canvas, 0);
  end;

  if ShowFocus and Focused then
  begin
    R := FButtonRect;
    InflateRect(R, -2, -2);
    Canvas.DrawFocusRect(R);
  end;
end;
{$ENDIF RTL230_UP}

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

  {$IFDEF RTL230_UP}
  if StyleServices.Enabled then
  begin
    DrawThemedBorder(Self);
    DrawThemedButtonFrame;
  end
  else
  {$ENDIF RTL230_UP}
  begin
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
    Msg.LParam := LPARAM(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TJvCustomRollOut.CMExpanded(var Msg: TMessage);
var
  Sender: TJvCustomRollOut;
begin
  if Msg.WParam = WPARAM(FGroupIndex) then
  begin
    Sender := TJvCustomRollOut(Msg.LParam);
    if (Sender <> Self) then
    begin
      SetCollapsed(True);
      CheckChildVisibility;
      Invalidate;
    end;
  end;
end;

function TJvCustomRollOut.WantKey(Key: Integer; Shift: TShiftState): Boolean;
begin
  Result := Enabled and (IsAccel(Key, Caption) and (ssAlt in Shift)) or ((Key = VK_SPACE) and Focused);
  if Result then
  begin
    SetCollapsed(not FCollapsed);
    if CanFocus then
      SetFocus;
  end
  else
    Result := inherited WantKey(Key, Shift);
end;

procedure TJvCustomRollOut.DoColorsChange(Sender: TObject);
begin
  RedrawControl(True);
end;

procedure TJvCustomRollOut.DoImageOptionsChange(Sender: TObject);
begin
  RedrawControl(True);
end;

procedure TJvCustomRollOut.DoButtonFontChange(Sender: TObject);
begin
  Invalidate;
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
  if ParentColor then
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

procedure TJvCustomRollOut.FocusKilled(NextWnd: THandle);
begin
  CheckChildVisibility;
  inherited FocusKilled(NextWnd);
  Invalidate;
end;

procedure TJvCustomRollOut.FocusSet(PrevWnd: THandle);
begin
  CheckChildVisibility;
  inherited FocusSet(PrevWnd);
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

procedure TJvCustomRollOut.CheckChildVisibility;
  procedure GetChildVisibility;
  var
    I: Integer;
  begin
    if FChildControlVisibility = nil then
    begin
      FChildControlVisibility := TStringList.Create;
      FChildControlVisibility.Sorted := True;
    end;

    for I := 0 to ControlCount - 1 do
      if (Controls[I] is TWinControl) and (TWinControl(Controls[I]).Visible) then
      begin
        FChildControlVisibility.AddObject(Controls[I].Name, Controls[I]);
        if CollapseCtrlsOnButton or (TWinControl(Controls[I]).Top > ButtonHeight) then
          TWinControl(Controls[I]).Visible := False;
      end;
  end;

  procedure SetChildVisibility;
  var
    I: Integer;
  begin
    if FChildControlVisibility <> nil then
    begin
      for I := 0 to FChildControlVisibility.Count - 1 do
        if FindChildControl(FChildControlVisibility[I]) <> nil then
          TWinControl(FChildControlVisibility.Objects[I]).Visible := True;
      FreeAndNil(FChildControlVisibility);
    end;
  end;
begin
  if csDesigning in ComponentState then
    Exit;

  if Collapsed then
    GetChildVisibility
  else
    SetChildVisibility;
end;

// Event handler called by the "TopWindow" when the window-rolloutpanel loses focus
//   to automatically collapse panel again
procedure TJvCustomRollOut.OnTopDeactivate(Sender: TObject);
begin
  if not FCollapsed then
    RestoreFromTopForm;
//  Collapse;       // Use this line instead of the previous one if you want the rollout
                  //   to collapse after the "topForm" lost focus
end;

procedure TJvCustomRollOut.RestoreFromTopForm;
var
  OldCollapsed: Boolean;
begin
  if not FSmartShow then
    Exit;

  // Rollout panel was mapped onto a special form (TopForm)
  // -> restore old state
  if Parent = FTopForm then
  begin
    FTopForm.OnDeactivate := nil; // Deactivate the Event to prevent
                                // calling this method a second time
    FTopForm.Hide;

    OldCollapsed := FCollapsed;
    FCollapsed := False;  // Set control to expanded, so that SetBounds stores expanded dimesions

    // Set the control back to it's old position!!
    Parent := FOldParent as TWinControl;
    Align := FOldAlign;
    SetBounds(FOldPos.X, FOldPos.Y, FOldWidthHeight.X, FOldWidthHeight.Y);
    FOldParent := nil;

    FCollapsed := OldCollapsed;
    FTopForm.OnDeactivate := OnTopDeactivate; // restore Event handling
  end;
end;

// If expanded panel doesn't fit on parent form -> create a separate form
//   so panel can be shown in it's full size:
procedure TJvCustomRollOut.PutOnForm;
var
  ScrPos : TPoint;
begin
  // Remember old pos
  if FSmartShow and not Assigned(FOldParent) then
  begin
    // Don't Smart-Expand if parent form not visible
    //   (e.g. Collapsed-property is set from outside)
    if (Owner is TForm) and not (Owner as TForm).Visible then
      Exit;

    FOldPos := Point( Left, Top );
    FOldAlign := Align;

    if Top + Height > Parent.Height then
    begin
      // Save old size and position to be able to restore it
      FOldParent:=Parent;
      FOldWidthHeight:=Point(Width, Height);

      // set size of the special form
      FTopForm.Width := Width;
      FTopForm.Height := Height;
      ScrPos := Parent.ClientToScreen(Point(Left, Top));
      FTopForm.Left := ScrPos.X;
      FTopForm.Top := ScrPos.Y;

      Parent := FTopForm;
      Align := alClient;
      FTopForm.Show;
    end;
  end;
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
  ReplaceComponentReference(Self, Value, TComponent(FRollOut));
end;

procedure TJvRollOutAction.UpdateTarget(Target: TObject);
begin
  if LinkCheckedToCollapsed then
    Checked := not (Target as TJvCustomRollOut).Collapsed;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
