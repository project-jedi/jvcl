{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvXPButtons.PAS, released on 2004-01-01.

The Initial Developer of the Original Code is Marc Hoffman.
Portions created by Marc Hoffman are Copyright (C) 2002 APRIORI business solutions AG.
Portions created by APRIORI business solutions AG are Copyright (C) 2002 APRIORI business solutions AG
All Rights Reserved.

Contributor(s):

Last Modified: 2004-01-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

unit JvXPButtons;

interface

uses
  Windows, Classes, Graphics, Controls, Forms, ActnList, ImgList,
  JvXPCore, JvXPCoreUtils, TypInfo;

type
{ TJvXPCustomButtonActionLink }

  TJvXPCustomButtonActionLink = class(TWinControlActionLink)
  protected
    { Protected declarations }
    function IsImageIndexLinked: Boolean; override;
    procedure AssignClient(AClient: TObject); override;
    procedure SetImageIndex(Value: Integer); override;
  public
    { Public declarations }
    destructor Destroy; override;
  end;

{ TJvXPCustomButton }

  TJvXPLayout = (blGlyphLeft, blGlyphRight, blGlyphTop, blGlyphBottom);

  TJvXPCustomButton = class(TJvXPCustomStyleControl)
  private
    { Private declarations }
    FAutoGray: Boolean;
    FBgGradient: TBitmap;
    FCancel: Boolean;
    FCkGradient: TBitmap;
    FDefault: Boolean;
    FFcGradient: TBitmap;
    FGlyph: TBitmap;
    FHlGradient: TBitmap;
    FImageChangeLink: TChangeLink;
    FImageIndex: Integer;
    FLayout: TJvXPLayout;
    FShowAccelChar: Boolean;
    FShowFocusRect: Boolean;
    FSmoothEdges: Boolean;
    FSpacing: Byte;
    FWordWrap: Boolean;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure ImageListChange(Sender: TObject);
  protected
    { Protected declarations }
    function GetActionLinkClass: TControlActionLinkClass; override;
    function IsSpecialDrawState(IgnoreDefault: Boolean = False): Boolean;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure SetAutoGray(Value: Boolean); virtual;
    procedure SetDefault(Value: Boolean); virtual;
    procedure SetGlyph(Value: TBitmap); virtual;
    procedure SetLayout(Value: TJvXPLayout); virtual;
    procedure SetShowAccelChar(Value: Boolean); virtual;
    procedure SetShowFocusRect(Value: Boolean); virtual;
    procedure SetSmoothEdges(Value: Boolean); virtual;
    procedure SetSpacing(Value: Byte); virtual;
    procedure SetWordWrap(Value: Boolean); virtual;
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure HookResized; override;
  published
    { Published declarations }

    // common properties.
    property Action;
    property Caption;
    property Enabled;
    property TabOrder;
    property TabStop default True;
    property Height default 21;
    property Width default 73;

    // advanced properties.
    property AutoGray: Boolean read FAutoGray write SetAutoGray default True;
    property Cancel: Boolean read FCancel write FCancel default False;
    property Default: Boolean read FDefault write SetDefault default False;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property Layout: TJvXPLayout read FLayout write SetLayout default blGlyphLeft;
    property ModalResult;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default True;
    property ShowFocusRect: Boolean read FShowFocusRect write SetShowFocusRect default False;
    property SmoothEdges: Boolean read FSmoothEdges write SetSmoothEdges default True;
    property Spacing: Byte read FSpacing write SetSpacing default 3;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default True;
  end;

{ TJvXPButton }

  TJvXPButton = class(TJvXPCustomButton);

{ TJvXPToolType }

  TJvXPToolType = (ttArrowLeft, ttArrowRight, ttClose, ttMaximize, ttMinimize,
    ttPopup, ttRestore);

{ TJvXPCustomToolButton }

  TJvXPCustomToolButton = class(TJvXPCustomStyleControl)
  private
    { Private declarations }
    FToolType: TJvXPToolType;
  protected
    { Protected declarations }
    procedure SetToolType(Value: TJvXPToolType); virtual;
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure HookResized; override;
  published
    { Published declarations }
    property Enabled;
    property Color default clBlack;
    property Height default 15;
    property ToolType: TJvXPToolType read FToolType write SetToolType default ttClose;
    property Width default 15;
  end;

{ TJvXPToolButton }

  TJvXPToolButton = class(TJvXPCustomToolButton);

implementation

{ TJvXPCustomButtonActionLink }

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomButtonActionLink.AssignClient
  Author:    mh
  Date:      09-Apr-2002
  Arguments: AClient: TObject
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TJvXPCustomButton;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomButtonActionLink.Destroy
  Author:    mh
  Date:      12-Apr-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TJvXPCustomButtonActionLink.Destroy;
begin
  TJvXPCustomButton(FClient).Invalidate;
  inherited;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomButtonActionLink.IsImageIndexLinked
  Author:    mh
  Date:      09-Apr-2002
  Arguments: None
  Result:    Boolean
-----------------------------------------------------------------------------}

function TJvXPCustomButtonActionLink.IsImageIndexLinked: Boolean;
begin
  Result := True;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomButtonActionLink.SetImageIndex
  Author:    mh
  Date:      09-Apr-2002
  Arguments: Value: Integer
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomButtonActionLink.SetImageIndex(Value: Integer);
begin
  inherited;
  (FClient as TJvXPCustomButton).FImageIndex := Value;
  (FClient as TJvXPCustomButton).Invalidate;
end;

{ TJvXPCustomButton }

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomButton.Create
  Author:    mh
  Date:      21-Feb-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TJvXPCustomButton.Create(AOwner: TComponent);
begin
  inherited;

  // set default properties.
  ControlStyle := ControlStyle - [csDoubleClicks];
  Height := 21;
  Width := 73;
  TabStop := True;

  // set custom properties.
  FAutoGray := True;
  FCancel := False;
  FDefault := False;
  FImageIndex := -1;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FGlyph := TBitmap.Create;
  FLayout := blGlyphLeft;
  FShowAccelChar := True;
  FShowFocusRect := False;
  FSmoothEdges := True;
  FSpacing := 3;
  FWordWrap := True;

  // create ...
  FBgGradient := TBitmap.Create; // background gradient
  FCkGradient := TBitmap.Create; // clicked gradient
  FFcGradient := TBitmap.Create; // focused gradient
  FHlGradient := TBitmap.Create; // Highlight gradient
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomButton.Destroy
  Author:    mh
  Date:      21-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TJvXPCustomButton.Destroy;
begin
  FBgGradient.Free;
  FCkGradient.Free;
  FFcGradient.Free;
  FHlGradient.Free;
  FGlyph.Free;
  FImageChangeLink.OnChange := nil;
  FImageChangeLink.Free;
  FImageChangeLink := nil;
  inherited;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomButton.GetActionLinkClass
  Author:    mh
  Date:      09-Apr-2002
  Arguments: None
  Result:    TControlActionLinkClass
-----------------------------------------------------------------------------}

function TJvXPCustomButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TJvXPCustomButtonActionLink;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomButton.CMDialogKey
  Author:    mh
  Date:      22-Feb-2002
  Arguments: var Message: TCMDialogKey
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomButton.CMDialogKey(var Message: TCMDialogKey);
begin
  inherited;
  with Message do
    if (((CharCode = VK_RETURN) and (Focused or (FDefault and not (IsSibling))))
      or ((CharCode = VK_ESCAPE) and FCancel) and (KeyDataToShiftState(KeyData) = []))
      and CanFocus then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomButton.SetAutoGray
  Author:    mh
  Date:      12-Apr-2002
  Arguments: Value: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomButton.SetAutoGray(Value: Boolean);
begin
  if Value <> FAutoGray then
  begin
    FAutoGray := Value;
    if not IsLocked then
      Invalidate;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomButton.SetDefault
  Author:    mh
  Date:      22-Feb-2002
  Arguments: Value: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomButton.SetDefault(Value: Boolean);
begin
  if Value <> FDefault then
  begin
    FDefault := Value;
    with GetParentForm(Self) do
      Perform(CM_FOCUSCHANGED, 0, LongInt(ActiveControl));
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomButton.SetGlyph
  Author:    mh
  Date:      22-Feb-2002
  Arguments: Value: TPicture
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomButton.SetGlyph(Value: TBitmap);
begin
  FGlyph.Assign(Value);
  if not IsLocked then
    Invalidate;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomButton.SetLayout
  Author:    mh
  Date:      12-Apr-2002
  Arguments: Value: TJvXPLayout
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomButton.SetLayout(Value: TJvXPLayout);
begin
  if Value <> FLayout then
  begin
    FLayout := Value;
    if not IsLocked then
      Invalidate;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomButton.SetShowAccelChar
  Author:    mh
  Date:      22-Feb-2002
  Arguments: Value: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomButton.SetShowAccelChar(Value: Boolean);
begin
  if Value <> FShowAccelChar then
  begin
    FShowAccelChar := Value;
    if not IsLocked then
      Invalidate;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomButton.SetShowFocusRect
  Author:    mh
  Date:      22-Feb-2002
  Arguments: Value: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomButton.SetShowFocusRect(Value: Boolean);
begin
  if Value <> FShowFocusRect then
  begin
    FShowFocusRect := Value;
    if not IsLocked then
      Invalidate;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomButton.SetSmoothEdges
  Author:    mh
  Date:      05-Jul-2002
  Arguments: Value: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomButton.SetSmoothEdges(Value: Boolean);
begin
  if Value <> FSmoothEdges then
  begin
    FSmoothEdges := Value;
    if not IsLocked then
      Invalidate;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomButton.SetSpacing
  Author:    mh
  Date:      11-Apr-2002
  Arguments: Value: Integer
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomButton.SetSpacing(Value: Byte);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    if not IsLocked then
      Invalidate;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomButton.SetWordWrap
  Author:    mh
  Date:      29-Apr-2002
  Arguments: Value: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomButton.SetWordWrap(Value: Boolean);
begin
  if Value <> FWordWrap then
  begin
    FWordWrap := Value;
    if not IsLocked then
      Invalidate;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomButton.ImageListChange
  Author:    mh
  Date:      09-Apr-2002
  Arguments: Sender: TObject
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomButton.ImageListChange(Sender: TObject);
begin
  if Assigned(Action) and (Sender is TCustomImageList)
    and Assigned(TAction(Action).ActionList.Images)
    and ((TAction(Action).ImageIndex < (TAction(Action).ActionList.Images.Count))) then
    FImageIndex := TAction(Action).ImageIndex
  else
    FImageIndex := -1;
  if not IsLocked then
    Invalidate;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomButton.KeyDown
  Author:    mh
  Date:      22-Feb-2002
  Arguments: var Key: Word; Shift: TShiftState
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Shift = []) and (Key = VK_SPACE) then
  begin
    DrawState := DrawState + [dsHighlight];
    HookMouseDown;
  end;
  inherited;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomButton.KeyUp
  Author:    mh
  Date:      22-Feb-2002
  Arguments: var Key: Word; Shift: TShiftState
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomButton.KeyUp(var Key: Word; Shift: TShiftState);
var
  cPos: TPoint;
begin
  //
  // it's not possible to call the 'HookMouseUp' or 'HookMouseLeave' methods,
  // because we don't want to call there event handlers.
  //
  if dsClicked in DrawState then
  begin
    GetCursorPos(cPos);
    cPos := ScreenToClient(cPos);
    if not PtInRect(Bounds(0, 0, Width, Height), cPos) then
      DrawState := DrawState - [dsHighlight];
    DrawState := DrawState - [dsClicked];
    if not IsLocked then
      Invalidate;
    Click;
  end;
  inherited;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomButton.IsSpecialDrawState
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    Boolean
-----------------------------------------------------------------------------}

function TJvXPCustomButton.IsSpecialDrawState(IgnoreDefault: Boolean = False): Boolean;
begin
  if dsClicked in DrawState then
    Result := not (dsHighlight in DrawState)
  else
    Result := (dsHighlight in DrawState) or (dsFocused in DrawState);
  if not IgnoreDefault then
    Result := Result or (FDefault and CanFocus) and not IsSibling;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomButton.ActionChange
  Author:    mh
  Date:      09-Apr-2002
  Arguments: Sender: TObject; CheckDefaults: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if Assigned(TCustomAction(Sender).ActionList.Images) and
        (FImageChangeLink.Sender <> TCustomAction(Sender).ActionList.Images) then
        TCustomAction(Sender).ActionList.Images.RegisterChanges(FImageChangeLink);
      if (ActionList <> nil) and (ActionList.Images <> nil) and
        (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
        FImageIndex := ImageIndex;
      if not IsLocked then
        Invalidate;
    end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomButton.HookResized
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomButton.HookResized;
const
  ColSteps = 64;
  Dithering = True;
var
  Offset: Integer;
begin
  inherited;

  // calculate offset.
  Offset := 4 * (Integer(IsSpecialDrawState(True)));

  //
  // create gradient rectangles for...
  //

  // background.
  JvXPCreateGradientRect(Width - (2 + Offset), Height - (2 + Offset),
    dxColor_Btn_Enb_BgFrom_WXP,  dxColor_Btn_Enb_BgTo_WXP, ColSteps, gsTop, Dithering,
    FBgGradient);

  // clicked.
  JvXPCreateGradientRect(Width - 2, Height - 2, dxColor_Btn_Enb_CkFrom_WXP,
    dxColor_Btn_Enb_CkTo_WXP, ColSteps, gsTop, Dithering, FCkGradient);

  // focused.
  JvXPCreateGradientRect(Width - 2, Height - 2, dxColor_Btn_Enb_FcFrom_WXP,
    dxColor_Btn_Enb_FcTo_WXP, ColSteps, gsTop, Dithering, FFcGradient);

  // highlight.
  JvXPCreateGradientRect(Width - 2, Height - 2, dxColor_Btn_Enb_HlFrom_WXP,
    dxColor_Btn_Enb_HlTo_WXP, ColSteps, gsTop, Dithering, FHlGradient);

  // redraw.
  if not IsLocked then
    Invalidate;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomButton.Paint
  Author:    mh
  Date:      21-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomButton.Paint;
var
  Rect: TRect;
  Offset, Flags: Integer;
  DrawPressed: Boolean;
  Image: TBitmap;
  Bitmap: TBitmap;
begin
  with Canvas do
  begin
    // clear background.
    Rect := GetClientRect;
    Brush.Color := Self.Color;
    FillRect(Rect);

    // draw gradient borders.
    if IsSpecialDrawState then
    begin
      Bitmap := TBitmap.Create;
      try
        if dsHighlight in DrawState then
          Bitmap.Assign(FHlGradient)
        else
          Bitmap.Assign(FFcGradient);
        BitBlt(Handle, 1, 1, Width, Height, Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
      finally
        Bitmap.Free;
      end;
    end;

    // draw background gradient...
    if not ((dsHighlight in DrawState) and (dsClicked in DrawState)) then
    begin
      Offset := 2 * Integer(IsSpecialDrawState);
      BitBlt(Handle, 1 + Offset, 1 + Offset, Width - 3 * Offset, Height - 3 * Offset,
        FBgGradient.Canvas.Handle, 0, 0, SRCCOPY);
    end
    // ...or click gradient.
    else
      BitBlt(Handle, 1, 1, Width, Height, FCkGradient.Canvas.Handle, 0, 0, SRCCOPY);

    // draw border lines.
    if Enabled then
      Pen.Color := dxColor_Btn_Enb_Border_WXP
    else
      Pen.Color := dxColor_Btn_Dis_Border_WXP;
    Brush.Style := bsClear;
    RoundRect(0, 0, Width, Height, 5, 5);

    // draw border edges.
    if FSmoothEdges then
    begin
      if Enabled then
        Pen.Color := dxColor_Btn_Enb_Edges_WXP
      else
        Pen.Color := dxColor_Btn_Dis_Edges_WXP;
      JvXPDrawLine(Canvas, 0, 1, 2, 0);
      JvXPDrawLine(Canvas, Width - 2, 0, Width, 2);
      JvXPDrawLine(Canvas, 0, Height - 2, 2, Height);
      JvXPDrawLine(Canvas, Width - 3, Height, Width, Height - 3);
    end;

    // set drawing flags.
    Flags := {DT_VCENTER or }DT_END_ELLIPSIS;
    if FWordWrap then
      Flags := Flags or DT_WORDBREAK;

    // draw image & caption.
    Image := TBitmap.Create;
    try
      // get image from action or glyph property.
      if Assigned(Action) and Assigned(TAction(Action).ActionList.Images) and
        (FImageIndex > -1) and (FImageIndex < TAction(Action).ActionList.Images.Count) then
        TAction(Action).ActionList.Images.GetBitmap(FImageIndex, Image)
      else
        Image.Assign(FGlyph);

      // autogray image (if allowed).
      if FAutoGray and not Enabled then
        JvXPConvertToGray2(Image);

      // assign canvas font (change HotTrack-Color, if necessary).
      Font.Assign(Self.Font);

      // calculate textrect.
      if not Image.Empty then
        case FLayout of
          blGlyphLeft:
            Inc(Rect.Left, Image.Width + FSpacing);
          blGlyphRight:
            begin
              Dec(Rect.Left, Image.Width + FSpacing);
              Dec(Rect.Right, (Image.Width + FSpacing) * 2);
              Flags := Flags or DT_RIGHT;
            end;
          blGlyphTop:
            Inc(Rect.Top, Image.Height + FSpacing);
          blGlyphBottom:
            Dec(Rect.Top, Image.Height + FSpacing);
        end;
      JvXPRenderText(Self, Canvas, Caption, Font, Enabled, FShowAccelChar, Rect, Flags or DT_CALCRECT);
      OffsetRect(Rect, (Width - Rect.Right) div 2, (Height - Rect.Bottom) div 2);

      // should we draw the pressed state?
      DrawPressed := (dsHighlight in DrawState) and (dsClicked in DrawState);
      if DrawPressed then
        OffsetRect(Rect, 1, 1);

      // draw image - if available.
      if not Image.Empty then
      begin
        Image.Transparent := True;
        case FLayout of
          blGlyphLeft:
            Draw(Rect.Left - (Image.Width + FSpacing), (Height - Image.Height) div 2 +
              Integer(DrawPressed), Image);
          blGlyphRight:
            Draw(Rect.Right + FSpacing, (Height - Image.Height) div 2 +
              Integer(DrawPressed), Image);
          blGlyphTop:
            Draw((Width - Image.Width) div 2 + Integer(DrawPressed),
              Rect.Top - (Image.Height + FSpacing), Image);
          blGlyphBottom:
            Draw((Width - Image.Width) div 2 + Integer(DrawPressed),
              Rect.Bottom + FSpacing, Image);
        end;
      end;

      // draw focusrect (if enabled).
      if (dsFocused in DrawState) and (FShowFocusRect) then
      begin
        Brush.Style := bsSolid;
        DrawFocusRect(Bounds(3, 3, Width - 6, Height - 6));
      end;

      // draw caption.
      SetBkMode(Handle, Transparent);
      JvXPRenderText(Self, Canvas, Caption, Font, Enabled, FShowAccelChar, Rect, Flags);
    finally
      Image.Free;
    end;
  end;
end;

{ TJvXPCustomToolButton }

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomToolButton.Create
  Author:    mh
  Date:      16-Aug-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TJvXPCustomToolButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csDoubleClicks];
  Color := clBlack;
  FToolType := ttClose;
  HookResized;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomToolButton.HookResized
  Author:    mh
  Date:      16-Aug-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomToolButton.HookResized;
begin
  Height := 15;
  Width := 15;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomToolButton.SetToolType
  Author:    mh
  Date:      16-Aug-2002
  Arguments: Value: TJvXPToolType
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomToolButton.SetToolType(Value: TJvXPToolType);
begin
  if Value <> FToolType then
  begin
    FToolType := Value;
    if not IsLocked then
      Invalidate;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomToolButton.Paint
  Author:    mh
  Date:      16-Aug-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomToolButton.Paint;
var
  Rect: TRect;
  Bitmap: TBitmap;
  Theme: TJvXPTheme;
  Shifted: Boolean;
begin
  with Canvas do
  begin
    Rect := GetClientRect;
    Brush.Color := TJvXPWinControl(Parent).Color;
    FillRect(Rect);
    if csDesigning in ComponentState then
      DrawFocusRect(Rect);
    Brush.Style := bsClear;
    Theme := Style.GetTheme;
    if (Theme = WindowsXP) and (dsClicked in DrawState) and
      not (dsHighlight in DrawState) then
      JvXPFrame3d(Self.Canvas, Rect, clWhite, clBlack);
    if dsHighlight in DrawState then
    begin
      if Theme = WindowsXP then
        JvXPFrame3d(Self.Canvas, Rect, clWhite, clBlack, dsClicked in DrawState)
      else begin
        Pen.Color := dxColor_BorderLineOXP;
        Rectangle(Rect);
        InflateRect(Rect, -1, -1);
        if dsClicked in DrawState then
          Brush.Color := dxColor_BgCkOXP
        else
          Brush.Color := dxColor_BgOXP;
        FillRect(Rect);
      end;
    end;
    Shifted := (Theme = WindowsXP) and (dsClicked in DrawState);
    Bitmap := TBitmap.Create;
    try
      Bitmap.Handle := LoadBitmap(hInstance, PChar(Copy(GetEnumName(TypeInfo(TJvXPToolType),
        Ord(FToolType)), 3, MAXINT)));
      if (dsClicked in DrawState) and (dsHighlight in DrawState) then
        JvXPColorizeBitmap(Bitmap, clWhite)
      else
      if not Enabled then
        JvXPColorizeBitmap(Bitmap, clGray)
      else
      if Color <> clBlack then
        JvXPColorizeBitmap(Bitmap, Color);
      Bitmap.Transparent := True;
      Draw((Width - Bitmap.Width) div 2 + Integer(Shifted),
        (Height - Bitmap.Height) div 2 + Integer(Shifted), Bitmap);
    finally
      Bitmap.Free;
    end;
  end;
end;

end.

