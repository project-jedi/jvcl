{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvOfficeColorButton.PAS, released on 2004-02-26.

The Initial Developer of the Original Code is dejoy [dejoy att ynl dott gov dott cn]
Portions created by Peter Thörnqvist are Copyright (C) 2004 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  A office color selection button that mimics the one on the 'Display Properties'
  page in Win95/NT4

Known Issues:
    If the OtherCaption is set to an empty string, the default '&Other..' magically appears.
    Solution: Set OtherCaption to ' ' instead
-----------------------------------------------------------------------------}
// $Id$

unit JvOfficeColorButton;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, Forms, StdCtrls, Dialogs, ExtCtrls,
  JvComponent, JvSpeedButton, JvOfficeColorForm, JvOfficeColorPanel;

const
  MinArrowWidth = 9 + 4;
  Tag_ArrowWidth = 11;

type
  TJvOfficeColorButtonProperties = class(TJvOfficeColorPanelProperties)
  private
    FShowDragBar: Boolean;
    FDragCaption: string;
    FEdgeWidth: Integer;
    FArrowWidth: Integer;
    FDragBarHeight: Integer;
    FDragBarSpace: Integer;
    FDragBarHint: string;
    procedure SetShowDragBar(const Value: Boolean);
    procedure SetDragCaption(const Value: string);
    procedure SetArrowWidth(const Value: Integer);
    procedure SetEdgeWidth(const Value: Integer);
    procedure SetDragBarHeight(const Value: Integer);
    procedure SetDragBarSpace(const Value: Integer);
    procedure SetDragBarHint(const Value: string);
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
  published
    property EdgeWidth: Integer read FEdgeWidth write SetEdgeWidth default 4;
    property ArrowWidth: Integer read FArrowWidth write SetArrowWidth default MinArrowWidth;
    property ShowDragBar: Boolean read FShowDragBar write SetShowDragBar default True;
    property DragCaption: string read FDragCaption write SetDragCaption;
    property DragBarHint: string read FDragBarHint write SetDragBarHint;
    property DragBarHeight: Integer read FDragBarHeight write SetDragBarHeight default MinDragBarHeight;
    property DragBarSpace: Integer read FDragBarSpace write SetDragBarSpace default MinDragBarSpace;
  end;

  TJvCustomOfficeColorButton = class(TJvCustomPanel)
  private
    FMainButton: TJvSubColorButton;
    FArrowButton: TJvColorSpeedButton;
    FColorsForm: TJvOfficeColorForm;
    FProperties: TJvOfficeColorButtonProperties;
    FFlat: Boolean;
    FCurrentColor: TColor;
    FColorFormDropDown: Boolean;
    FInited: Boolean;
    FOnColorChange: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    FOnColorButtonClick: TNotifyEvent;
    FOnArrowClick: TNotifyEvent;
    procedure SetFlat(const Value: Boolean);
    // Set Control Color
    procedure SetControlBgColor(const Value: TColor);
    function GetControlBgColor: TColor;
    // Get Selection Color: (The Value of this control)
    procedure SetSelectedColor(const Value: TColor);
    function GetSelectedColor: TColor;
    function GetCustomColors: TStrings;
    procedure SetCustomColors(const Value: TStrings);
    function GetGlyph: TBitmap;
    procedure SetGlyph(const Value: TBitmap);
    function GetProperties: TJvOfficeColorButtonProperties;
    procedure SetProperties(const Value: TJvOfficeColorButtonProperties);
    {$IFDEF VCL}
    function GetColorDialogOptions: TColorDialogOptions;
    procedure SetColorDialogOptions(const Value: TColorDialogOptions);
    {$ENDIF VCL}
    procedure ReadArrowWidth(Reader: TReader);
    procedure ReadEdgeWidth(Reader: TReader);
    procedure ReadOtherCaption(Reader: TReader);
    procedure DoOnColorChange(Sender: TObject);
    procedure DoFormShowingChanged(Sender: TObject);
    procedure DoFormKillFocus(Sender: TObject);
    procedure DoFormClose(Sender: TObject; var Action: TCloseAction);
    procedure DoFormWindowStyleChanged(Sender: TObject);
    procedure DoButtonMouseEnter(Sender: TObject);
    procedure DoButtonMouseLeave(Sender: TObject);
    procedure DoArrowClick(Sender: TObject);
    procedure DoColorButtonClick(Sender: TObject);
    procedure DoClick(Sender: TObject);
  protected
    procedure AdjustColorForm(X: Integer = 0; Y: Integer = 0); //Screen position
    procedure ShowColorForm(X: Integer = 0; Y: Integer = 0); virtual; //Screen position
    {$IFDEF VCL}
    procedure CreateWnd; override;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure Loaded; override;
    {$ENDIF VisualCLX}
    procedure SetEnabled({$IFDEF VisualCLX} const {$ENDIF} Value: Boolean); override;
    procedure FontChanged; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure PropertiesChanged(Sender: TObject; PropName: string); virtual;
    property ColorsForm: TJvOfficeColorForm read FColorsForm;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AdjustSize; override;
    property Flat: Boolean read FFlat write SetFlat default True;
    property Color:TColor read GetControlBgColor write SetControlBgColor default clDefault;
//    property Color: TColor read GetColor write SetColor default clBtnFace; // COLOR OF THE BACKGROUND OF THE CONTROL
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clBlack; // COLOR SELECTED IN THE BUTTON.
    property CustomColors: TStrings read GetCustomColors write SetCustomColors;
    property Properties: TJvOfficeColorButtonProperties read GetProperties write SetProperties;
    {$IFDEF VCL}
    property Options: TColorDialogOptions read GetColorDialogOptions write SetColorDialogOptions default [];
    {$ENDIF VCL}
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnColorChange: TNotifyEvent read FOnColorChange write FOnColorChange;
    property OnArrowClick: TNotifyEvent read FOnArrowClick write FOnArrowClick;
    property OnColorButtonClick: TNotifyEvent read FOnColorButtonClick write FOnColorButtonClick;
  end;

  TJvOfficeColorButton = class(TJvCustomOfficeColorButton)
  published
    {$IFDEF VCL}
    property BiDiMode;
    property DragCursor;
    property DragKind;
    property ParentBiDiMode;
    property OnCanResize;
    property OnEndDock;
    property OnGetSiteInfo;
    {$ENDIF VCL}
    property Align;
    property Anchors;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property Flat;
    property Color;  // basic Control color.
    property SelectedColor; // WPostma. Added to published!

    property CustomColors;
    {$IFDEF VCL}
    property Options;
    {$ENDIF VCL}
    property Glyph;
    property Properties;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnDropDown;
    property OnArrowClick;
    property OnColorChange;
    property OnColorButtonClick;
    property OnClick;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  TypInfo,
  JvJCLUtils, JvExExtCtrls, JvThemes, JvResources;

const
  cArrowWidth = 'ArrowWidth';
  cDragBarHeight = 'DragBarHeight';
  cDragBarHint = 'DragBarHint';
  cDragBarSpace = 'DragBarSpace';
  cDragCaption = 'DragCaption';
  cEdgeWidth = 'EdgeWidth';
  cOtherCaption = 'OtherCaption';
  cShowDragBar = 'ShowDragBar';

type
  TColorSpeedButtonAccessProtected = class(TJvColorSpeedButton);
  TJvOfficeColorFormAccessProtected = class(TJvOfficeColorForm);
  TJvOfficeColorPanelAccessProtected = class(TJvOfficeColorPanel);

//=== { TJvColorArrowButton } ================================================

type
  TJvColorArrowButton = class(TJvColorSpeedButton)
  protected
    procedure Paint; override;
  end;

procedure DrawTriangle(Canvas: TCanvas; Top, Left, Width: Integer);
begin
  if Odd(Width) then
    Inc(Width);
  Canvas.Polygon([Point(Left, Top), Point(Left + Width, Top),
    Point(Left + Width div 2, Top + Width div 2)]);
end;

procedure TJvColorArrowButton.Paint;
const
  DownStyles: array [Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
  FillStyles: array [Boolean] of Integer = (BF_MIDDLE, 0);
  FArrowWidth = 6;
var
  PaintRect: TRect;
  DrawFlags: Integer;
  Offset: TPoint;
  Push: Boolean;
begin
  inherited Paint;

  { calculate were to put arrow part }
  PaintRect := Rect(0, 0, Width, Height);
  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled then
    Dec(PaintRect.Left);
  {$ENDIF JVCLThemesEnabled}

  Push := Down or (FState in [rbsDown, rbsExclusive]);
  if Push then
  begin
    Offset.X := 1;
    Offset.Y := 1;
  end
  else
  begin
    Offset.X := 0;
    Offset.Y := 0;
  end;

  if not Flat then
  begin
    DrawFlags := DFCS_BUTTONPUSH; // or DFCS_ADJUSTRECT;
    if Push then
      DrawFlags := DrawFlags or DFCS_PUSHED;
    if IsMouseOver(Self) then
      DrawFlags := DrawFlags or DFCS_HOT;
    DrawThemedFrameControl(Self, Canvas.Handle, PaintRect, DFC_BUTTON, DrawFlags);
  end
  else
  if MouseOver and Enabled or (csDesigning in ComponentState) then
    DrawEdge(Canvas.Handle, PaintRect, DownStyles[Push],
      FillStyles[Flat] or BF_RECT);

  { Draw arrow }
  if Enabled then
  begin
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Color := clBlack;
  end
  else
    Canvas.Pen.Color := clBtnShadow;
  Canvas.Brush.Style := bsSolid;
  DrawTriangle(Canvas, (Height div 2) - 2, (Width - FArrowWidth) div 2, FArrowWidth);
end;

//=== { TJvColorMainButton } =================================================

type
  TJvColorMainButton = class(TJvSubColorButton)
  protected
    function GetEdgeWidth: Integer; override;
  end;

function TJvColorMainButton.GetEdgeWidth: Integer;
begin
  Result := FEdgeWidth;
end;

//=== { TJvCustomOfficeColorButton } =========================================

constructor TJvCustomOfficeColorButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInited := False;

  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  BevelOuter := bvNone;
  {$IFDEF VCL}
  Locked := True;
  {$ENDIF VCL}
  Width := MinButtonWidth + MinArrowWidth;
  Height := MinButtonHeight;

  FCurrentColor := clDefault;

  FMainButton := TJvColorMainButton.Create(Self);
  with FMainButton do
  begin
    Parent := Self;
    NumGlyphs := 2;
    ButtonColor := clDefault;
    Tag := MaxColorButtonNumber + 3;
    OnClick := DoClick;
  end;

  FArrowButton := TJvColorArrowButton.Create(Self);
  with FArrowButton do
  begin
    Parent := Self;
    GroupIndex := 2;
    AllowAllUp := True;
    Tag := MaxColorButtonNumber + 4;
    OnClick := DoArrowClick;
  end;

  FColorsForm := TJvOfficeColorForm.CreateNew(Self);
  with TJvOfficeColorFormAccessProtected(FColorsForm) do
  begin
    FormStyle := fsStayOnTop;
    ToolWindowStyle := False;
    OnShowingChanged := DoFormShowingChanged;
    OnKillFocus := DoFormKillFocus;
    OnClose := DoFormClose;
    OnWindowStyleChanged := DoFormWindowStyleChanged;

    ColorPanel.OnColorChange := DoOnColorChange;
    ColorPanel.OnColorButtonClick := DoColorButtonClick;
  end;

  FProperties := TJvOfficeColorButtonProperties.Create;
  FProperties.Assign(FColorsForm.ColorPanel.Properties);
  FProperties.OnPropertiesChanged := PropertiesChanged;
  FColorsForm.ColorPanel.Properties.OnPropertiesChanged := nil;

//  Font.Name := 'MS Shell Dlg 2';
  Flat := True;
  {$IFDEF VisualCLX}
  // in CLX and a bug not fix when drag the colors form
  Properties.ShowDragBar := False;
  {$ENDIF VisualCLX}
  FMainButton.OnMouseEnter := DoButtonMouseEnter;
  FArrowButton.OnMouseEnter := DoButtonMouseEnter;
  FMainButton.OnMouseLeave := DoButtonMouseLeave;
  FArrowButton.OnMouseLeave := DoButtonMouseLeave;

  FInited := True;
end;

destructor TJvCustomOfficeColorButton.Destroy;
begin
  if FColorsForm.Visible then
  begin
    with TJvOfficeColorFormAccessProtected(FColorsForm) do
    begin
      OnShowingChanged := nil;
      OnKillFocus := nil;
      OnClose := nil;
      OnWindowStyleChanged := nil;

      ColorPanel.OnColorChange := nil;
      ColorPanel.OnColorButtonClick := nil;
      Hide;
    end;
  end;
  Action.Free;
  FProperties.Free;
  inherited Destroy;
end;

procedure TJvCustomOfficeColorButton.AdjustSize;
begin
  if FInited then
    with Properties do
    begin
      if ArrowWidth < MinArrowWidth then
        ArrowWidth := MinArrowWidth;
      if (Width - ArrowWidth) < MinButtonWidth then
        Width := MinButtonWidth + ArrowWidth;
      if Height < MinButtonHeight then
        Height := MinButtonHeight;

      FMainButton.SetBounds(0, 0, Width - FArrowWidth, Height);

      FArrowButton.SetBounds(FMainButton.Width, 0, ArrowWidth, Height);
    end;
  inherited AdjustSize;
end;

{$IFDEF VCL}
procedure TJvCustomOfficeColorButton.CreateWnd;
begin
  inherited CreateWnd;
  AdjustSize;
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
procedure TJvCustomOfficeColorButton.Loaded;
begin
  inherited Loaded;
  AdjustSize;
end;
{$ENDIF VisualCLX}

procedure TJvCustomOfficeColorButton.SetEnabled({$IFDEF VisualCLX} const {$ENDIF} Value: Boolean);
begin
  inherited SetEnabled(Value);
  FMainButton.Enabled := Value;
  FArrowButton.Enabled := Value;
  FColorsForm.ColorPanel.Enabled := Value;
end;

procedure TJvCustomOfficeColorButton.FontChanged;
begin
  inherited FontChanged;
  FColorsForm.Font.Assign(Font);
end;

procedure TJvCustomOfficeColorButton.DoArrowClick(Sender: TObject);
begin
  if TJvColorSpeedButton(Sender).Tag = FArrowButton.Tag then
  begin
    if FColorsForm.Visible or FColorFormDropDown then
    begin
      FColorsForm.Hide;
      FColorFormDropDown := False;
      FArrowButton.Down := False;
    end
    else
    begin
      if Assigned(FOnDropDown) then
        FOnDropDown(Self);
      ShowColorForm;
      FColorFormDropDown := True;
    end
  end
  else
  begin
    TJvSubColorButton(Sender).Down := True;
    SetSelectedColor(TJvSubColorButton(Sender).ButtonColor);
  end;
  if Assigned(FOnArrowClick) then
    FOnArrowClick(Self);
end;

procedure TJvCustomOfficeColorButton.DoColorButtonClick(Sender: TObject);
begin
  if not FColorsForm.ToolWindowStyle then
  begin
    FColorsForm.Hide;
    FColorsForm.ToolWindowStyle := False;
    if FArrowButton.Down then
      FArrowButton.Down := False;
    FColorFormDropDown := False;
  end
  else
  begin
    if FColorsForm.ColorPanel.ClickColorButton = cbctOtherButton then
      FColorsForm.FormStyle := fsNormal;
  end;

  if Assigned(FOnColorButtonClick) then
    FOnColorButtonClick(Sender);
end;

function TJvCustomOfficeColorButton.GetCustomColors: TStrings;
begin
  Result := FColorsForm.ColorPanel.CustomColors;
end;

function TJvCustomOfficeColorButton.GetSelectedColor: TColor;
begin
  Result := FColorsForm.ColorPanel.Color;
end;

procedure TJvCustomOfficeColorButton.DoOnColorChange(Sender: TObject);
begin
  FMainButton.ButtonColor := FColorsForm.ColorPanel.SelectedColor;
  if FColorsForm.ToolWindowStyle and (FColorsForm.FormStyle <> fsStayOnTop) then
    FColorsForm.FormStyle := fsStayOnTop;
  if Assigned(FOnColorChange) then
    FOnColorChange(Self);
end;

procedure TJvCustomOfficeColorButton.SetCustomColors(const Value: TStrings);
begin
  FColorsForm.ColorPanel.CustomColors.Assign(Value);
end;

procedure TJvCustomOfficeColorButton.SetFlat(const Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    FMainButton.Flat := Value;
    FArrowButton.Flat := Value;
    FColorsForm.Flat := Value;
  end;
end;

// NEW: Set Control Background Color
procedure TJvCustomOfficeColorButton.SetControlBgColor(const Value: TColor);
begin
  if Value = clDefault then // If set to clDefault then no change.
    Exit;
  if Value <> FArrowButton.Color then
  begin
    FMainButton.Color := Value;
    FArrowButton.Color := Value;
  end;
end;

function TJvCustomOfficeColorButton.GetControlBgColor: TColor;
begin
  Result := FArrowButton.Color;
end;

{WPostma - Property SelectedColor (GetColor/SetColor) is the actual user-selected value}
procedure TJvCustomOfficeColorButton.SetSelectedColor(const Value: TColor);
begin
  if FColorsForm.ColorPanel.SelectedColor <> Value then
    FColorsForm.ColorPanel.SelectedColor := Value;
end;

procedure TJvCustomOfficeColorButton.AdjustColorForm(X: Integer = 0; Y: Integer = 0);
var
  Pt: TPoint;
begin
  if (X = 0) and (Y = 0) then
    Pt := ClientToScreen(Point(FMainButton.Left, FMainButton.Top))
  else
    Pt := Point(X, Y);

  FColorsForm.Left := Pt.X;
  if (FColorsForm.Left + FColorsForm.Width) > Screen.Width then
    FColorsForm.Left := Screen.Width - FColorsForm.Width;
  FColorsForm.Top := Pt.Y + Height;
  if (FColorsForm.Top + FColorsForm.Height) > Screen.Height then
    FColorsForm.Top := Pt.Y - FColorsForm.Height;
end;

procedure TJvCustomOfficeColorButton.ShowColorForm(X: Integer = 0; Y: Integer = 0);
begin
  AdjustColorForm(X, Y);
  FColorsForm.Show;
  FColorFormDropDown := True;
end;

procedure TJvCustomOfficeColorButton.DoFormShowingChanged(Sender: TObject);
begin
  if not FColorsForm.Visible then
  begin
    FArrowButton.Down := False;
    {$IFDEF VCL}
    FMainButton.Perform(CM_MOUSELEAVE, 0, 0);
    FArrowButton.Perform(CM_MOUSELEAVE, 0, 0);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    TColorSpeedButtonAccessProtected(FArrowButton).MouseLeave(FArrowButton);
    TColorSpeedButtonAccessProtected(FMainButton).MouseLeave(FMainButton);
    {$ENDIF VisualCLX}
  end;
end;

procedure TJvCustomOfficeColorButton.DoFormKillFocus(Sender: TObject);
var
  R: TRect;
  P: TPoint;
begin
  R := FArrowButton.ClientRect;
  GetCursorPos(P);
  P := FArrowButton.ScreenToClient(P);
  if (not FColorsForm.ToolWindowStyle) and (not PtInRect(R, P)) then //mouse in ArrowButton
  begin
    FColorsForm.Hide;
    FColorsForm.ToolWindowStyle := False;
    if FArrowButton.Down then
      FArrowButton.Down := False;
    FColorFormDropDown := False;
  end;
end;

procedure TJvCustomOfficeColorButton.DoFormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FColorsForm.ToolWindowStyle then
    FColorFormDropDown := False;
  if csDestroying in ComponentState then
    Action := caFree
  else
    Action := caHide;
end;

procedure TJvCustomOfficeColorButton.DoFormWindowStyleChanged(Sender: TObject);
begin
  if FColorsForm.ToolWindowStyle then
  begin
    FArrowButton.Down := False;
    {$IFDEF VCL}
    FMainButton.Perform(CM_MOUSELEAVE, 0, 0);
    FArrowButton.Perform(CM_MOUSELEAVE, 0, 0);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    TColorSpeedButtonAccessProtected(FArrowButton).MouseLeave(FArrowButton);
    TColorSpeedButtonAccessProtected(FMainButton).MouseLeave(FMainButton);
    {$ENDIF VisualCLX}
  end;
end;

procedure TJvCustomOfficeColorButton.DoButtonMouseEnter(Sender: TObject);
begin
  if FFlat and Enabled then
  begin
    {$IFDEF VCL}
    FMainButton.Perform(CM_MOUSEENTER, 0, 0);
    FArrowButton.Perform(CM_MOUSEENTER, 0, 0);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    TColorSpeedButtonAccessProtected(FMainButton).MouseEnter(FMainButton);
    TColorSpeedButtonAccessProtected(FArrowButton).MouseEnter(FArrowButton);
    {$ENDIF VisualCLX}
  end;
end;

procedure TJvCustomOfficeColorButton.DoButtonMouseLeave(Sender: TObject);
begin
  if FFlat and Enabled then
  begin
    if Sender = FMainButton then
    begin
      if FColorsForm.Visible then
      {$IFDEF VCL}
        FMainButton.Perform(CM_MOUSEENTER, 0, 0)
      else
        FArrowButton.Perform(CM_MOUSELEAVE, 0, 0);
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
        TColorSpeedButtonAccessProtected(FMainButton).MouseEnter(FMainButton)
      else
        TColorSpeedButtonAccessProtected(FArrowButton).MouseLeave(FArrowButton);
      {$ENDIF VisualCLX}
    end
    else
    if Sender = FArrowButton then
    begin
      if not FColorsForm.Visible then
      {$IFDEF VCL}
        FMainButton.Perform(CM_MOUSELEAVE, 0, 0)
      else
        FArrowButton.Perform(CM_MOUSEENTER, 0, 0);
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
        TColorSpeedButtonAccessProtected(FMainButton).MouseLeave(FMainButton)
      else
        TColorSpeedButtonAccessProtected(FArrowButton).MouseEnter(FArrowButton);
      {$ENDIF VisualCLX}
    end;
  end;
end;

function TJvCustomOfficeColorButton.GetGlyph: TBitmap;
begin
  Result := FMainButton.Glyph;
end;

procedure TJvCustomOfficeColorButton.SetGlyph(const Value: TBitmap);
begin
  FMainButton.Glyph := Value;
end;

{$IFDEF VCL}

function TJvCustomOfficeColorButton.GetColorDialogOptions: TColorDialogOptions;
begin
  Result := FColorsForm.ColorPanel.Options;
end;

procedure TJvCustomOfficeColorButton.SetColorDialogOptions(const Value: TColorDialogOptions);
begin
  FColorsForm.ColorPanel.Options := Value;
end;

{$ENDIF VCL}

function TJvCustomOfficeColorButton.GetProperties: TJvOfficeColorButtonProperties;
begin
  Result := FProperties;
end;

procedure TJvCustomOfficeColorButton.SetProperties(const Value: TJvOfficeColorButtonProperties);
begin
  if FProperties <> Value then
  begin
    FProperties.Assign(Value);
    FColorsForm.ColorPanel.Properties.Assign(Value);
  end;
end;

procedure TJvCustomOfficeColorButton.PropertiesChanged(Sender: TObject;
  PropName: string);
begin
  if Cmp(PropName, cShowDragBar) then
  begin
    if FColorsForm.ShowDragBar <> Properties.ShowDragBar then
      FColorsForm.ShowDragBar := Properties.ShowDragBar;
    if not Properties.ShowDragBar and
      TJvOfficeColorFormAccessProtected(FColorsForm).DropDownMoved then
      AdjustColorForm;
  end
  else
  if Cmp(PropName, cDragCaption) then
    FColorsForm.Caption := Properties.DragCaption
  else
  if Cmp(PropName, cDragBarHeight) then
  begin
    FColorsForm.DragBarHeight := Properties.DragBarHeight;
    AdjustColorForm;
  end
  else
  if Cmp(PropName, cDragBarHint) then
    FColorsForm.DragBarHint := Properties.DragBarHint
  else
  if Cmp(PropName, cDragBarSpace) then
  begin
    FColorsForm.DragBarSpace := Properties.DragBarSpace;
    AdjustColorForm;
  end
  else
  if Cmp(PropName, cArrowWidth) then
    AdjustSize
  else
  if Cmp(PropName, cEdgeWidth) then
    FMainButton.EdgeWidth := Properties.EdgeWidth
  else
  begin
    FColorsForm.ColorPanel.Properties.Assign(Properties);
    TJvOfficeColorPanelAccessProtected(FColorsForm.ColorPanel).PropertiesChanged(Properties, PropName);
    TJvOfficeColorFormAccessProtected(FColorsForm).AdjustColorForm;
  end;
end;

procedure TJvCustomOfficeColorButton.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  //Hint: next 3 for compatible old version
  Filer.DefineProperty(cArrowWidth, ReadArrowWidth, nil, True);
  Filer.DefineProperty(cEdgeWidth, ReadEdgeWidth, nil, True);
  Filer.DefineProperty(cOtherCaption, ReadOtherCaption, nil, True);
end;

procedure TJvCustomOfficeColorButton.ReadArrowWidth(Reader: TReader);
begin
  Properties.ArrowWidth := Reader.ReadInteger;
end;

procedure TJvCustomOfficeColorButton.ReadEdgeWidth(Reader: TReader);
begin
  Properties.EdgeWidth := Reader.ReadInteger;
end;

procedure TJvCustomOfficeColorButton.ReadOtherCaption(Reader: TReader);
begin
  Properties.OtherCaption := Reader.ReadString;
end;

//=== { TJvOfficeColorButtonProperties } =====================================

constructor TJvOfficeColorButtonProperties.Create;
begin
  inherited Create;
  FShowDragBar := True;
  FEdgeWidth := 4;
  FArrowWidth := MinArrowWidth;
  FDragBarHeight := MinDragBarHeight;
  FDragBarSpace := MinDragBarSpace;
  FDragBarHint := RsDragToFloating;
end;

procedure TJvOfficeColorButtonProperties.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvOfficeColorButtonProperties then
    with TJvOfficeColorButtonProperties(Source) do
    begin
      Self.FShowDragBar := ShowDragBar;
      Self.FDragCaption := DragCaption;
      Self.FEdgeWidth := EdgeWidth;
      Self.FArrowWidth := ArrowWidth;
      Self.FDragBarHeight := DragBarHeight;
      Self.FDragBarHint := DragBarHint;
      Self.FDragBarSpace := DragBarSpace;
    end;
end;

procedure TJvOfficeColorButtonProperties.SetArrowWidth(const Value: Integer);
begin
  if FArrowWidth <> Value then
  begin
    FArrowWidth := Value;
    Changed(cArrowWidth);
  end;
end;

procedure TJvOfficeColorButtonProperties.SetDragBarHeight(const Value: Integer);
begin
  if FDragBarHeight <> Value then
  begin
    FDragBarHeight := Value;
    Changed(cDragBarHeight);
  end;
end;

procedure TJvOfficeColorButtonProperties.SetDragBarSpace(const Value: Integer);
begin
  if FDragBarSpace <> Value then
  begin
    FDragBarSpace := Value;
    Changed(cDragBarSpace);
  end;
end;

procedure TJvOfficeColorButtonProperties.SetDragBarHint(const Value: string);
begin
  if FDragBarHint<>Value then
  begin
    FDragBarHint := Value;
    Changed(cDragBarHint);
  end;
end;

procedure TJvOfficeColorButtonProperties.SetDragCaption(const Value: string);
begin
  if FDragCaption <> Value then
  begin
    FDragCaption := Value;
    Changed(cDragCaption);
  end;
end;

procedure TJvOfficeColorButtonProperties.SetEdgeWidth(const Value: Integer);
begin
  if FEdgeWidth <> Value then
  begin
    FEdgeWidth := Value;
    Changed(cEdgeWidth);
  end;
end;

procedure TJvOfficeColorButtonProperties.SetShowDragBar(const Value: Boolean);
begin
  if FShowDragBar <> Value then
  begin
    FShowDragBar := Value;
    Changed(cShowDragBar);
  end;
end;

procedure TJvCustomOfficeColorButton.DoClick(Sender: TObject);
begin
  if Assigned(OnClick) then
    OnClick(Self);
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

