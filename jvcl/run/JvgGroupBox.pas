{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgGroupBox.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgGroupBox;

{$I jvcl.inc}

// Illumination - fake :)
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls,
  {$IFDEF USEJVCL}
  JVCLVer,
  {$ENDIF USEJVCL}
  JvgTypes, JvgCommClasses;

type
  TCaptionAlignment = (fcaNone, fcaLeft, fcaRight, fcaCenter, fcaWidth);
  TJvgGroupBox = class(TCustomGroupBox)
  private
    {$IFDEF USEJVCL}
    FAboutJVCL: TJVCLAboutInfo;
    {$ENDIF USEJVCL}
    FBorder: TJvgBevelOptions;
    FCaptionBorder: TJvgBevelOptions;
    FGradient: TJvgGradient;
    FCaptionGradient: TJvgGradient;
    FCaptionShift: TJvgPointClass;
    FCaptionTextStyle: TglTextStyle;
    FCaptionAlignment: TCaptionAlignment;
    FColors: TJvgGroupBoxColors;
    FIllumination: TJvgIllumination;
    FTransparent: Boolean;
    FTransparentCaption: Boolean;
    FOptions: TglGroupBoxOptions;
    FCollapsed: Boolean;
    FAfterPaint: TNotifyEvent;
    FOnCollapsed: TNotifyEvent;
    FOnExpanded: TNotifyEvent;
    FGroupIndex: Integer;
    FGlyphCollapsed: TBitmap;
    FGlyphExpanded: TBitmap;

    ChildFocusedControl: TWinControl;
//    FImage: TBitmap;
    FullHeight: Integer;
    CaptionRect: TRect;
    ptScroll: TPoint;
    fScrolling: Boolean;
    procedure SetCaptionAlignment(Value: TCaptionAlignment);
    procedure SetCaptionTextStyle(Value: TglTextStyle);
    procedure SetCollapsed(Value: Boolean);
    procedure SetOptions(Value: TglGroupBoxOptions);
    procedure SetTransparent(Value: Boolean);
    procedure SetTransparentCaption(Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    function GetGlyphCollapsed: TBitmap;
    procedure SetGlyphCollapsed(Value: TBitmap);
    function GetGlyphExpanded: TBitmap;
    procedure SetGlyphExpanded(Value: TBitmap);

    procedure Collapse_(fCollapse: Boolean);
    procedure SmthChanged(Sender: TObject);
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    procedure ReadFullHeight(Reader: TReader);
    procedure WriteFullHeight(Writer: TWriter);
    //    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN;

    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMMouseMove(var Msg: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonUp(var Msg: TWMLButtonUp); message WM_LBUTTONUP;
    procedure CMEnabledChanged(var Msg: TMessage);  message CM_ENABLEDCHANGED;
  protected
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure ComputeCaptionRect;
  public
    procedure Collapse(fCollapse: Boolean);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefineProperties(Filer: TFiler); override;
  published
    {$IFDEF USEJVCL}
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    {$ENDIF USEJVCL}
    property Anchors;
    property Align;
    property Caption: string read GetCaption write SetCaption;
    property Color;
    property DragCursor;
    property DragMode;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
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
    property Enabled;
    property Border: TJvgBevelOptions read FBorder write FBorder;
    property CaptionAlignment: TCaptionAlignment
      read FCaptionAlignment write SetCaptionAlignment default fcaNone;
    property CaptionBorder: TJvgBevelOptions read FCaptionBorder write FCaptionBorder;
    property CaptionGradient: TJvgGradient read FCaptionGradient write FCaptionGradient;
    property CaptionShift: TJvgPointClass read FCaptionShift write FCaptionShift;
    property CaptionTextStyle: TglTextStyle read FCaptionTextStyle write SetCaptionTextStyle default fstNone;
    property Collapsed: Boolean read FCollapsed write SetCollapsed default False;
    property Colors: TJvgGroupBoxColors read FColors write FColors;
    property Gradient: TJvgGradient read FGradient write FGradient;
    property Illumination: TJvgIllumination read FIllumination write FIllumination stored False;
    property Options: TglGroupBoxOptions read FOptions write SetOptions;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property TransparentCaption: Boolean read FTransparentCaption write SetTransparentCaption default False;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property GlyphCollapsed: TBitmap read GetGlyphCollapsed write SetGlyphCollapsed;
    property GlyphExpanded: TBitmap read GetGlyphExpanded write SetGlyphExpanded;
    property AfterPaint: TNotifyEvent read FAfterPaint write FAfterPaint;
    property OnCollapsed: TNotifyEvent read FOnCollapsed write FOnCollapsed;
    property OnExpanded: TNotifyEvent read FOnExpanded write FOnExpanded;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Math,
  JvgUtils;

constructor TJvgGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //  ControlStyle := ControlStyle + [csOpaque];
  FBorder := TJvgBevelOptions.Create;
  FCaptionBorder := TJvgBevelOptions.Create;
  FGradient := TJvgGradient.Create;
  FCaptionGradient := TJvgGradient.Create;
  FColors := TJvgGroupBoxColors.Create;
  FIllumination := TJvgIllumination.Create;
  FCaptionShift := TJvgPointClass.Create;
  //...defaults

  FColors.Caption := clBtnShadow;
  FColors.CaptionActive := clBtnShadow;
  FColors.Text := clHighlightText;
  FColors.TextActive := clHighlightText;
  FBorder.Outer := bvNone;
  FBorder.Inner := bvSpace;
  FCaptionBorder.Outer := bvNone;
  FCaptionBorder.Inner := bvSpace;
  FGradient.FromColor := clBlack;
  FGradient.ToColor := clGray;
  FCaptionShift.X := 8;
  FCaptionShift.Y := 0;
  FCaptionTextStyle := fstNone;
  FCaptionAlignment := fcaNone;
  FOptions := [fgoCanCollapse, fgoFilledCaption, fgoFluentlyCollapse,
    fgoFluentlyExpand, fgoHideChildrenWhenCollapsed, fgoSaveChildFocus];
  {$IFDEF GL_RUS}
  Font.CharSet := RUSSIAN_CHARSET;
  {$ENDIF GL_RUS}
  FBorder.OnChanged := SmthChanged;
  FCaptionBorder.OnChanged := SmthChanged;
  FGradient.OnChanged := SmthChanged;
  FCaptionGradient.OnChanged := SmthChanged;
  FCaptionShift.OnChanged := SmthChanged;
  FColors.OnChanged := SmthChanged;
  FIllumination.OnChanged := SmthChanged;
  ControlStyle := ControlStyle + [csOpaque];
end;

destructor TJvgGroupBox.Destroy;
begin
  FBorder.Free;
  FCaptionBorder.Free;
  FGradient.Free;
  FCaptionGradient.Free;
  FCaptionShift.Free;
  FColors.Free;
  FIllumination.Free;
//  FImage.Free;
  FGlyphExpanded.Free;
  FGlyphCollapsed.Free;
  inherited Destroy;
end;

procedure TJvgGroupBox.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('FullHeight', ReadFullHeight, WriteFullHeight, True);
end;

procedure TJvgGroupBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Transparent or TransparentCaption then
    Params.ExStyle := Params.ExStyle or WS_EX_Transparent;
end;

procedure TJvgGroupBox.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  Inc(Rect.Top, 1);
end;

procedure TJvgGroupBox.WMLButtonDown(var Msg: TWMLButtonDown);
var
  pt: TPoint;
begin
  inherited;
  if fgoIgnoreMouse in Options then
    Exit;
  pt.X := Msg.Pos.X;
  pt.Y := Msg.Pos.Y;
  if (fgoCanCollapse in Options) and PtInRect(CaptionRect, pt) then
    Collapse(not Collapsed)
  else
  begin
    Screen.Cursor := crHandPoint;
    {ptScroll.X := pt.X;} ptScroll.Y := pt.Y;
    fScrolling := True;
  end;

end;

procedure TJvgGroupBox.WMMouseMove(var Msg: TWMMouseMove);
begin
  if fScrolling and (Parent is TScrollBox) then
    (Parent as TScrollBox).VertScrollBar.Position := (Parent as
      TScrollBox).VertScrollBar.Position + ptScroll.Y - Msg.Pos.Y;
  inherited;
end;

procedure TJvgGroupBox.WMLButtonUp(var Msg: TWMLButtonUp);
begin
  inherited;
  if fgoIgnoreMouse in Options then
    Exit;
  fScrolling := False;
  Screen.Cursor := crDefault;
end;

procedure TJvgGroupBox.CMEnabledChanged(var Msg: TMessage);
var
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
    Controls[I].Enabled := Enabled;
end;

procedure TJvgGroupBox.ReadFullHeight(Reader: TReader);
begin
  FullHeight := Reader.ReadInteger;
end;

procedure TJvgGroupBox.WriteFullHeight(Writer: TWriter);
begin
  Writer.WriteInteger(FullHeight);
end;

procedure TJvgGroupBox.Paint;
type
  TgbColor = record
    Text: TColor;
    Caption: TColor;
    Background: TColor;
    Client: TColor;
    Delineate: TColor;
  end;
var
  H, GlyphWidth: Integer;
  R, NewR: TRect;
  Glyph: TBitmap;
  DrawState: TglDrawState;
  Interspace: Integer;
  Color: TgbColor;
begin
  //inherited;
  //Exit;
  Interspace := 2;
  if Collapsed then
  begin
    Color.Text := Colors.Text;
    Color.Caption := Colors.Caption;
    //    Color.Background := Colors.Background;
    Color.Client := Colors.Client;
    Color.Delineate := Colors.Delineate;
  end
  else
  begin
    Color.Text := Colors.TextActive;
    Color.Caption := Colors.CaptionActive;
    //    Color.Background := Colors.BackgroundActive;
    Color.Client := Colors.ClientActive;
    Color.Delineate := Colors.DelineateActive;
  end;

  with Canvas do
  begin
    Canvas.Font.Assign(Self.Font);
    R := GetClientRect;
    //Font := Self.Font;
//    if CaptionHeight = 0 then H := TextHeight('0') - FCaptionShift.Y
//                         else H := CaptionHeight - FCaptionShift.Y
    H := TextHeight(Text) - FCaptionShift.Y;
    R := Rect(0, H div 2 { - 1}, Width, Height);
    if FGradient.Active then
      GradientBox(Handle, R, FGradient, PS_SOLID, 1);

    Dec(R.Right);
    Dec(R.Bottom);
    DrawBoxEx(Canvas.Handle, R,
      Border.Sides, Border.Inner, Border.Outer,
      Border.Bold, Color.Client, FGradient.Active or Transparent);

    if Text <> '' then
    begin
      if Assigned(FGlyphExpanded) then
        GlyphWidth := FGlyphExpanded.Width
      else
        GlyphWidth := 0;
      if Assigned(FGlyphCollapsed) then
        GlyphWidth := max(FGlyphCollapsed.Width, GlyphWidth);

      if Collapsed then
        Glyph := FGlyphCollapsed
      else
        Glyph := FGlyphExpanded;

      ComputeCaptionRect;
      R := CaptionRect;

      if not TransparentCaption then
      begin
        Canvas.Brush.Color := Colors.Caption;
        Windows.FillRect(Canvas.Handle, R, Canvas.Brush.Handle);
      end;
      GradientBox(Canvas.Handle, R, FCaptionGradient, PS_SOLID, 1);

      NewR := DrawBoxEx(Canvas.Handle, R, CaptionBorder.Sides,
        CaptionBorder.Inner, CaptionBorder.Outer,
        CaptionBorder.Bold, Color.Caption,
        TransparentCaption or not (fgoFilledCaption in Options));
      //      Brush.Color := Color;

      SetBkMode(Handle, Integer(TRANSPARENT));

      if Assigned(Glyph) then
      begin
        if Enabled then
          DrawState := fdsDefault
        else
          DrawState := fdsDisabled;
        CreateBitmapExt(Handle, Glyph, NewR, 0, max(0, (NewR.Bottom -
          NewR.Top - Glyph.Height) div 2),
          fwoNone, DrawState, True,
          GetPixel(Glyph.Canvas.Handle, 0, Glyph.Height - 1)
          {TransparentColor},
          {DisabledMaskColor} 0);
      end;

      ExtTextOutExt((Handle), (NewR.Left + GlyphWidth + Interspace),
        (NewR.Top), NewR, Caption,
        IIF(Enabled, FCaptionTextStyle, fstPushed), fgoDelineatedText in
        Options,
        {fNeedUpdateOnlyMainText} False, Color.Text, Color.Delineate,
        Colors.Highlight, Colors.Shadow,
        FIllumination, nil {Gradient}, Font);

    end;
  end;
  //  if Transparent then for I:=0 to ComponentCount-1 do
  //    TControl(Components[I]).Repaint;
  if Assigned(AfterPaint) then
    AfterPaint(Self);
end;

procedure TJvgGroupBox.Collapse(fCollapse: Boolean);
var
  I: Integer;
  AnotherExpandedWasFound: Boolean;
begin
  if csLoading in ComponentState then
    Exit;

  if fCollapse then
  begin
    if Align = alClient then
      Exit;
    if (FGroupIndex <> 0) and (fgoOneAlwaysExpanded in Options) then
    begin //...One Stay Always Expanded in group
      AnotherExpandedWasFound := False;
      for I := 0 to Owner.ComponentCount - 1 do
        with TControl(Owner) do
          if (Components[I] is TJvgGroupBox) and
            (TJvgGroupBox(Components[I]).GroupIndex = FGroupIndex) then
            if (not TJvgGroupBox(Components[I]).Collapsed) and
              (Components[I] <> Self) then
            begin
              AnotherExpandedWasFound := True;
              Break;
            end; //...are another expanded controls in group
      if not AnotherExpandedWasFound then
        Exit; //..i'm last- can't collapse
    end;
  end
  else
  if (FGroupIndex <> 0) and (fgoCollapseOther in Options) then
    for I := 0 to Owner.ComponentCount - 1 do
      with TControl(Owner) do
        if (Components[I] is TJvgGroupBox) and
          (TJvgGroupBox(Components[I]).GroupIndex = FGroupIndex) and
          (Components[I] <> Self) then
          TJvgGroupBox(Components[I]).Collapsed := True;

  Collapse_(fCollapse);
  if fCollapse and Assigned(FOnCollapsed) then
    FOnCollapsed(Self);
  if not fCollapse and Assigned(FOnExpanded) then
    FOnExpanded(Self);
end;

procedure TJvgGroupBox.Collapse_(fCollapse: Boolean);
var
  I {, Step}: Integer;
  {  DC: HDC;
    pt: TPoint;
    R, CR: TRect;
    Scroll: HRGN;
    SpaceBrush: HBRUSH;
    fFirst: Boolean;
    LastTickCount: Integer;}
begin
  if Align = alClient then
    Exit;

  FCollapsed := fCollapse;
  if fCollapse then
  begin
    FullHeight := Height;
    if fgoResizeParent in Options then
      Parent.Height := Parent.Height - (FullHeight - CaptionRect.Bottom);
    Height := CaptionRect.Bottom + 1;

    //...set all Children invisible
    if (fgoHideChildrenWhenCollapsed in Options) or (fgoSaveChildFocus in Options) then
      for I := 0 to Owner.ComponentCount - 1 do
        with TControl(Owner) do
          if (Components[I] is TControl) and
            (TControl(Components[I]).Parent = Self) then
          begin
            if (fgoSaveChildFocus in Options) and (Components[I] is TWinControl) and
              TWinControl(Components[I]).Focused then
              ChildFocusedControl := TWinControl(Components[I]);
            if fgoHideChildrenWhenCollapsed in Options then //...hide
              TControl(Components[I]).Visible := False;
          end;
  end
  else
  begin
    if fgoResizeParent in Options then
      Parent.Height := Parent.Height + (FullHeight - CaptionRect.Bottom);
    Height := FullHeight;

    //...set all Children visible
    if fgoHideChildrenWhenCollapsed in Options then
      for I := 0 to Owner.ComponentCount - 1 do
        with TControl(Owner) do
          if (Components[I] is TControl) and
            (TControl(Components[I]).Parent = Self) then
            TControl(Components[I]).Visible := True;

    if ChildFocusedControl <> nil then
    try
      ChildFocusedControl.SetFocus;
    except
    end;
  end;

  Exit; { patch for win 98 }

  (*
  if fCollapse then
  begin
    //...prepare image

    if FImage=nil then FImage := TBitmap.Create;
    FImage.Height := Height;
    FImage.Width := Width;
    GetWindowImage( Self, True{fDrawSelf}, True{fDrawChildWindows}, FImage.Canvas.Handle );

    DC := GetDC(0);
    CR := ClientRect; Dec(CR.Bottom,CaptionRect.Bottom);
    pt.X := 0; pt.Y := CaptionRect.Bottom+1;//16;
    pt := ClientToScreen(pt);
    offsetRect( CR, pt.X, pt.Y );
    R := CR;
    Scroll := CreateRectRgn( R.Left, R.Top, R.Right-R.Left, R.Bottom-R.Top );
    Inc( R.Top );
    I := Height-Canvas.TextHeight('0y');
    // Step := 1;//max( Height div 100, 1 );

    SpaceBrush := CreateSolidBrush( ColorToRGB(TGroupBox(Parent).Color) );
    // fFirst := True; LastTickCount := GetTickCount;
    FullHeight := Height;
    if fgoFluentlyCollapse in Options then
    while Height > CaptionRect.Bottom+1 do
    begin
      Application.ProcessMessages;
//      ScrollDC( DC, 0, -Step, R, CR,  Scroll, nil);
      //if fFirst then
//      begin
//        FillRect( DC, Rect( R.Left, R.Bottom-Step, R.Right, R.Bottom ), SpaceBrush );
//        fFirst := False;
//      end;
      Height := max(CaptionRect.Bottom, Height - Height * 30 div 100);
      ValidateRect(Handle, @CaptionRect);
//      while GetTickCount - LastTickCount <= 0 do;
//      Step := GetTickCount - LastTickCount; if Step > 20 then Step := 20;
//      LastTickCount := GetTickCount;
    end;
    DeleteObject(SpaceBrush);
//    FullHeight := Height;
    Height := CaptionRect.Bottom+1;//max( Canvas.TextHeight(Caption), 16 );
    DeleteObject( Scroll );
    ReleaseDC( 0, DC );

  end
  else
  begin//..expanded

    if fgoFluentlyExpand in Options then
    while Height < FullHeight do
    begin
      Application.ProcessMessages;
      Height := Height + 1;
      ValidateRect(Handle, @CaptionRect);
    end;
    Height := FullHeight;
  end;
  InValidateRect(Handle, @CaptionRect, False);
  FCollapsed := fCollapse;
  *)
end;

procedure TJvgGroupBox.SmthChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvgGroupBox.SetCaptionAlignment(Value: TCaptionAlignment);
begin
  if FCaptionAlignment <> Value then
  begin
    FCaptionAlignment := Value;
    Invalidate;
  end;
end;

procedure TJvgGroupBox.SetCaptionTextStyle(Value: TglTextStyle);
begin
  if FCaptionTextStyle <> Value then
  begin
    FCaptionTextStyle := Value;
    Invalidate;
  end;
end;

procedure TJvgGroupBox.SetCollapsed(Value: Boolean);
begin
  if FCollapsed <> Value then
  begin
    if not (fgoCanCollapse in Options) and Value then
      Exit;
    FCollapsed := Value;
    if csLoading in ComponentState then
      Exit;
    Collapse_(Value);
  end;
end;

procedure TJvgGroupBox.SetOptions(Value: TglGroupBoxOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    //  if not(fgoCanCollapse in Options) then Collapsed := False;
    if Assigned(Parent) then
      CalcShadowAndHighlightColors((Parent as TWinControl).Brush.Color,
        TJvgLabelColors(Colors));
    Invalidate;
  end;
end;

procedure TJvgGroupBox.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    RecreateWnd;
  end;
end;

procedure TJvgGroupBox.SetTransparentCaption(Value: Boolean);
begin
  if FTransparentCaption <> Value then
  begin
    FTransparentCaption := Value;
    RecreateWnd;
  end;
end;

procedure TJvgGroupBox.SetGroupIndex(Value: Integer);
var
  I: Integer;
begin
  if FGroupIndex = Value then
    Exit;
  FGroupIndex := Value;
  if csLoading in ComponentState then
    Exit;
  if (not Collapsed) and (FGroupIndex <> 0) then
    for I := 0 to Owner.ComponentCount - 1 do
      with TControl(Owner) do
        if (Components[I] is TJvgGroupBox) and
          (TJvgGroupBox(Components[I]).GroupIndex = FGroupIndex) and
          (Components[I] <> Self) then
          TJvgGroupBox(Components[I]).Collapsed := True;
end;

function TJvgGroupBox.GetGlyphCollapsed: TBitmap;
begin
  if FGlyphCollapsed = nil then
    FGlyphCollapsed := TBitmap.Create;
  Result := FGlyphCollapsed;
end;

procedure TJvgGroupBox.SetGlyphCollapsed(Value: TBitmap);
begin
  GlyphCollapsed.Assign(Value);
  Invalidate;
end;

function TJvgGroupBox.GetGlyphExpanded: TBitmap;
begin
  if FGlyphExpanded = nil then
    FGlyphExpanded := TBitmap.Create;
  Result := FGlyphExpanded;
end;

procedure TJvgGroupBox.SetGlyphExpanded(Value: TBitmap);
begin
  GlyphExpanded.Assign(Value);
  Invalidate;
end;

function TJvgGroupBox.GetCaption: string;
begin
  Result := inherited Caption;
end;

procedure TJvgGroupBox.SetCaption(const Value: string);
begin
  inherited Caption := Value;
  // (obones) force the computation of CaptionRect so that
  // we don't need to paint to have the correct values.
  ComputeCaptionRect;
end;

procedure TJvgGroupBox.ComputeCaptionRect;
var
  R: TRect;
  I, RW, GlyphWidth: Integer;
  Interspace: Integer;
begin
  Canvas.Font.Assign(Self.Font);
  
  Interspace := 2;
  R := Rect(FCaptionShift.X, 0, 0, Canvas.TextHeight(Text) - FCaptionShift.Y);

  if Assigned(FGlyphExpanded) then
    GlyphWidth := FGlyphExpanded.Width
  else
    GlyphWidth := 0;
  if Assigned(FGlyphCollapsed) then
    GlyphWidth := Max(FGlyphCollapsed.Width, GlyphWidth);

  Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), R, DT_LEFT or DT_SINGLELINE or DT_CALCRECT);

  Inc(R.Right, Interspace + GlyphWidth);
  with CaptionBorder do
  begin
    I := 0;
    if Inner <> bvNone then
    begin
      Inc(I, 2);
      if Bold then
        Inc(I);
    end;
    if Outer <> bvNone then
    begin
      Inc(I, 2);
      if Bold then
        Inc(I);
    end;

    Inc(R.Right, I);
    Inc(R.Bottom, I);
  end;

  RW := R.Right - R.Left;
  case FCaptionAlignment of
    fcaLeft:
      begin
        R.Right := RW;
        R.Left := 0;
      end;
    fcaRight:
      begin
        R.Left := Width - RW - 1;
        R.Right := R.Left + RW;
      end;
    fcaCenter:
      begin
        R.Left := (Width - RW) div 2;
        R.Right := R.Left + RW;
      end;
    fcaWidth:
      begin
        R.Left := 0;
        R.Right := Width - 1;
      end;
  end;
  if fgoDelineatedText in Options then
    Inc(R.Bottom, 2);
  if CaptionTextStyle = fstShadow then
  begin
    if fgoDelineatedText in Options then
      Inc(R.Bottom, FIllumination.ShadowDepth - 2)
    else
      Inc(R.Bottom, FIllumination.ShadowDepth);
  end
  else
  if CaptionTextStyle <> fstNone then
    Inc(R.Bottom, 2);

  CaptionRect := R;
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

