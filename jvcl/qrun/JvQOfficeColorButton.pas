{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvColorBtn.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):
  dejoy(dejoy att ynl dott gov dott cn)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  A color selection button that mimicks the one on the 'Display Properties' page in Win95/NT4

Known Issues:
    If the OtherCaption is set to an empty string, the default '&Other..' magically appears.
    Solution: Set OtherCaption to ' ' instead
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQOfficeColorButton;

interface

uses
  SysUtils, Classes,
  
  
  Types, QGraphics, QWindows, QControls, QForms, QStdCtrls, QDialogs, QExtCtrls,
  
  JvQComponent, JvQSpeedButton, JvQOfficeColorForm, JvQOfficeColorPanel;

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
    procedure SetColor(const Value: TColor);
    function GetCustomColors: TStrings;
    procedure SetCustomColors(const Value: TStrings);
    function GetColor: TColor;
    function GetGlyph: TBitmap;
    procedure SetGlyph(const Value: TBitmap);
    function GetProperties: TJvOfficeColorButtonProperties;
    procedure SetProperties(const Value: TJvOfficeColorButtonProperties);
    
    procedure ReadArrowWidth(Reader: TReader);
    procedure ReadEdgeWidth(Reader: TReader);
    procedure ReadOtherCaption(Reader: TReader);
    procedure DoOnColorChange(Sender: Tobject);
    procedure DoFormShowingChanged(Sender: TObject);
    procedure DoFormKillFocus(Sender: TObject);
    procedure DoFormClose(Sender: TObject; var Action: TCloseAction);
    procedure DoFormWindowStyleChanged(Sender: TObject);
    procedure DoButtonMouseEnter(Sender: TObject);
    procedure DoButtonMouseLeave(Sender: TObject);
    procedure DoArrowClick(Sender: TObject);
    procedure DoColorButtonClick(Sender: TObject);
    procedure DoClick(Sender:TObject);
  protected
    procedure AdjustColorForm(X: Integer = 0; Y: Integer = 0); //Screen postion
    procedure ShowColorForm(X: Integer = 0; Y: Integer = 0); virtual; //Screen postion
    
    
    procedure Loaded; override;
    
    procedure SetEnabled( const  Value: Boolean); override;
    procedure FontChanged; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure PropertiesChanged(Sender: TObject; PropName: string); virtual;

    property ColorsForm :TJvOfficeColorForm read FColorsForm;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AdjustSize; override;
    property Flat: Boolean read FFlat write SetFlat default True;
    property Color: TColor read GetColor write SetColor default clBlack;
    property SelectedColor: TColor read GetColor write SetColor default clBlack;

    property CustomColors: TStrings read GetCustomColors write SetCustomColors;
    property Properties: TJvOfficeColorButtonProperties read GetProperties write SetProperties;
    
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnColorChange: TNotifyEvent read FOnColorChange write FOnColorChange;
    property OnArrowClick: TNotifyEvent read FOnArrowClick write FOnArrowClick;
    property OnColorButtonClick: TNotifyEvent read FOnColorButtonClick write FOnColorButtonClick;
  end;

  TJvOfficeColorButton = class(TJvCustomOfficeColorButton)
  published
    
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
    property Color;
    property CustomColors;
    
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
  TypInfo,
  JvQJCLUtils, JvQExExtCtrls, JvQThemes;

type
  THackColorSpeedButton = class(TJvColorSpeedButton);
  THackJvColorForm = class(TJvOfficeColorForm);
  THackColorPanel = class(TJvOfficeColorPanel);

//=== TJvColorArrowButton ====================================================

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
  begin
    Canvas.Pen.Color := clBtnShadow;
  end;
  Canvas.Brush.Style := bsSolid;
  DrawTriangle(Canvas, (Height div 2) - 2, (Width - FArrowWidth) div 2, FArrowWidth);
end;

//=== TJvColorMainButton =====================================================

type
  TJvColorMainButton = class(TJvSubColorButton)
  protected
    function GetEdgeWidth: Integer; override;
  end;

function TJvColorMainButton.GetEdgeWidth: Integer;
begin
  Result := FEdgeWidth;
end;

//=== TJvCustomOfficeColorButton =============================================

constructor TJvCustomOfficeColorButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInited := False;

  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  BevelOuter := bvNone;
  
  Width := MinButtonWidth + MinArrowWidth;
  Height := MinButtonHeight;

  FCurrentColor := clDefault;

  FMainButton := TJvColorMainButton.Create(Self);
  with FMainButton do
  begin
    Parent := Self;
    NumGlyphs := 2;
    Color := clDefault;
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
  with THackJvColorForm(FColorsForm) do
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
  
  // in CLX and a bug not fix when drag the colors form
  Properties.ShowDragBar := False;

  
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
    with THackJvColorForm(FColorsForm) do
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




procedure TJvCustomOfficeColorButton.Loaded;
begin
  inherited;
  AdjustSize;
end;


procedure TJvCustomOfficeColorButton.SetEnabled( const  Value: Boolean);
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
    if (FColorsForm.Visible) or (FColorFormDropDown) then
    begin
      FColorsForm.Hide;
      FColorFormDropDown := False;
      FArrowButton.Down := False;
    end
    else
    begin
      if Assigned(FOnDropDown) then
        FOnDropDown(self);
      ShowColorForm;
      FColorFormDropDown := True;
    end
  end
  else
  begin
    TJvSubColorButton(Sender).Down := True;
    SetColor(TJvSubColorButton(Sender).Color);
  end;
  if Assigned(FOnArrowClick) then
    FOnArrowClick(Self);
end;

procedure TJvCustomOfficeColorButton.DoColorButtonClick(Sender: TObject);
begin
  if  not FColorsForm.ToolWindowStyle then
  begin
    FColorsForm.Hide;
    FColorsForm.ToolWindowStyle := False;
    if FArrowButton.Down then
      FArrowButton.Down := False;
    FColorFormDropDown := False;
  end else
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

function TJvCustomOfficeColorButton.GetColor: TColor;
begin
  Result := FColorsForm.ColorPanel.Color;
end;

procedure TJvCustomOfficeColorButton.DoOnColorChange(Sender: Tobject);
begin
  FMainButton.Color := FColorsForm.ColorPanel.SelectedColor;
  if FColorsForm.ToolWindowStyle and (FColorsForm.FormStyle<>fsStayOnTop) then
  begin
    FColorsForm.FormStyle := fsStayOnTop;
  end;
  if Assigned(FOnColorChange) then
    FOnColorChange(self);
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

procedure TJvCustomOfficeColorButton.SetColor(const Value: TColor);
begin
  if FColorsForm.ColorPanel.SelectedColor <> Value then
  begin
    FColorsForm.ColorPanel.SelectedColor := Value;
    FMainButton.Color := Value;
  end;
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
    THackColorSpeedButton(FArrowButton).MouseLeave(FArrowButton);
    THackColorSpeedButton(FMainButton).MouseLeave(FMainButton);
  end
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
    THackColorSpeedButton(FArrowButton).MouseLeave(FArrowButton);
    THackColorSpeedButton(FMainButton).MouseLeave(FMainButton);
  end;
end;

procedure TJvCustomOfficeColorButton.DoButtonMouseEnter(Sender: TObject);
begin
  if FFlat and Enabled then
  begin
    THackColorSpeedButton(FMainButton).MouseEnter(FMainButton);
    THackColorSpeedButton(FArrowButton).MouseEnter(FArrowButton);
  end;
end;

procedure TJvCustomOfficeColorButton.DoButtonMouseLeave(Sender: TObject);
begin
  if FFlat and Enabled then
  begin
    if Sender = FMainButton then
    begin
      if FColorsForm.Visible then
        THackColorSpeedButton(FMainButton).MouseEnter(FMainButton)
      else
        THackColorSpeedButton(FArrowButton).MouseLeave(FArrowButton);
    end
    else
    if Sender = FArrowButton then
    begin
      if not FColorsForm.Visible then
        THackColorSpeedButton(FMainButton).MouseLeave(FMainButton)
      else
        THackColorSpeedButton(FArrowButton).MouseEnter(FArrowButton);
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
  if Cmp(PropName, 'ShowDragBar') then
  begin
    if FColorsForm.ShowDragBar <> Properties.ShowDragBar then
      FColorsForm.ShowDragBar := Properties.ShowDragBar;
    if not Properties.ShowDragBar and
      THackJvColorForm(FColorsForm).DropDownMoved then
      AdjustColorForm;
  end
  else
  if Cmp(PropName, 'DragCaption') then
  begin
    FColorsForm.Caption := Properties.DragCaption;
  end
  else
  if Cmp(PropName, 'DragBarHeight') then
  begin
    FColorsForm.DragBarHeight := Properties.DragBarHeight;
    AdjustColorForm;
  end
  else
  if Cmp(PropName, 'DragBarHint') then
  begin
    FColorsForm.DragBarHint := Properties.DragBarHint;
  end
  else
  if Cmp(PropName, 'DragBarSpace') then
  begin
    FColorsForm.DragBarSpace := Properties.DragBarSpace;
    AdjustColorForm;
  end
  else
  if Cmp(PropName, 'ArrowWidth') then
    AdjustSize
  else
  if Cmp(PropName, 'EdgeWidth') then
    FMainButton.EdgeWidth := Properties.EdgeWidth
  else
  begin
    FColorsForm.ColorPanel.Properties.Assign(Properties);
    THackColorPanel(FColorsForm.ColorPanel).PropertiesChanged(Properties, PropName);
    THackJvColorForm(FColorsForm).AdjustColorForm;
  end;
end;

procedure TJvCustomOfficeColorButton.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  //Hint: next 3 for compatible old version
  Filer.DefineProperty('ArrowWidth', ReadArrowWidth, nil, True);
  Filer.DefineProperty('EdgeWidth', ReadEdgeWidth, nil, True);
  Filer.DefineProperty('OtherCaption', ReadOtherCaption, nil, True);
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

//=== TJvOfficeColorButtonProperties =========================================

constructor TJvOfficeColorButtonProperties.Create;
begin
  inherited Create;
  FShowDragBar := True;
  FEdgeWidth := 4;
  FArrowWidth := MinArrowWidth;
  FDragBarHeight := MinDragBarHeight;
  FDragBarSpace := MinDragBarSpace;
  FDragBarHint := 'Drag to floating';
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
      self.FDragBarHint := DragBarHint;
      Self.FDragBarSpace := DragBarSpace;
    end;
end;

procedure TJvOfficeColorButtonProperties.SetArrowWidth(const Value: Integer);
begin
  if FArrowWidth <> Value then
  begin
    FArrowWidth := Value;
    Changed('ArrowWidth');
  end;
end;

procedure TJvOfficeColorButtonProperties.SetDragBarHeight(const Value: Integer);
begin
  if FDragBarHeight <> Value then
  begin
    FDragBarHeight := Value;
    Changed('DragBarHeight');
  end;
end;

procedure TJvOfficeColorButtonProperties.SetDragBarSpace(const Value: Integer);
begin
  if FDragBarSpace <> Value then
  begin
    FDragBarSpace := Value;
    Changed('DragBarSpace');
  end;
end;

procedure TJvOfficeColorButtonProperties.SetDragBarHint(const Value: string);
begin
  if FDragBarHint<>Value then
  begin
    FDragBarHint := Value;
    Changed('DragBarHint');
  end;
end;

procedure TJvOfficeColorButtonProperties.SetDragCaption(const Value: string);
begin
  if FDragCaption <> Value then
  begin
    FDragCaption := Value;
    Changed('DragCaption');
  end;
end;

procedure TJvOfficeColorButtonProperties.SetEdgeWidth(const Value: Integer);
begin
  if FEdgeWidth <> Value then
  begin
    FEdgeWidth := Value;
    Changed('EdgeWidth');
  end;
end;

procedure TJvOfficeColorButtonProperties.SetShowDragBar(const Value: Boolean);
begin
  if FShowDragBar <> Value then
  begin
    FShowDragBar := Value;
    Changed('ShowDragBar');
  end;
end;

procedure TJvCustomOfficeColorButton.DoClick(Sender: TObject);
begin
  if Assigned(OnClick) then OnClick(Self);
end;

end.

