{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgHoleShape.PAS, released on 2003-01-15.

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

unit JvgHoleShape;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls,
  {$IFDEF USEJVCL}
  JvComponent,
  {$ENDIF USEJVCL}
  JvgTypes, JvgCommClasses;

type
  TRGNCombineMode = (cmAND, cmCOPY, cmDIFF, cmOR, cmXOR);
  THoleShapeType = (stRectangle, stSquare, stRoundRect, stRoundSquare,
    stEllipse, stCircle);

  {$IFDEF USEJVCL}
  TJvgHoleShape = class(TJvGraphicControl)
  {$ELSE}
  TJvgHoleShape = class(TGraphicControl)
  {$ENDIF USEJVCL}
  private
    FCombineMode: TRGNCombineMode;
    FEnabledAllInDesignTime: Boolean;
    FEnabled: Boolean;
    FShape: THoleShapeType;
    FShapeBitmap: TBitmap;
    FBevelInner: TPanelBevel;
    FBevelOuter: TPanelBevel;
    FBevelInnerBold: Boolean;
    FBevelOuterBold: Boolean;
    FRectEllipse: TJvgPointClass;
    FBevelOffset: Integer;
    FNeedUpdateRgn: Boolean;
    FNeedRebuildBitmapShape: Boolean;
    FRGNInner: HRGN;
    FRGNOuter: HRGN;
    FOldX: Integer;
    FOldY: Integer;
    FOldW: Integer;
    FOldH: Integer;
    procedure SetEnabledAllInDesignTime(Value: Boolean);
    procedure SetShape(Value: THoleShapeType);
    procedure SetShapeBitmap(Value: TBitmap);
    procedure SetBevelInner(Value: TPanelBevel);
    procedure SetBevelOuter(Value: TPanelBevel);
    procedure SetBevelInnerBold(Value: Boolean);
    procedure SetBevelOuterBold(Value: Boolean);
    procedure SetCombineMode(Value: TRGNCombineMode);
    procedure SetBevelOffset(Value: Integer);
    procedure InternalUpdate;
    procedure CalcRGNs;
    procedure SmthChanged(Sender: TObject);
    procedure SayAllDTEnabledState(EnabledDT: Boolean);
  protected
    procedure SetEnabled(Value: Boolean); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateRGN;
    procedure Loaded; override;
    property RGNInner: HRGN read FRGNInner write FRGNInner;
    property RGNOuter: HRGN read FRGNOuter write FRGNOuter;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property EnabledAllInDesignTime: Boolean read FEnabledAllInDesignTime
      write SetEnabledAllInDesignTime default True;
    property Shape: THoleShapeType read FShape write SetShape default stEllipse;
    property BevelInner: TPanelBevel read FBevelInner write SetBevelInner default bvNone;
    property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter default bvLowered;
    property BevelInnerBold: Boolean read FBevelInnerBold write SetBevelInnerBold default True;
    property BevelOuterBold: Boolean read FBevelOuterBold write SetBevelOuterBold default True;
    property CombineMode: TRGNCombineMode read FCombineMode write SetCombineMode default cmDIFF;
    property BevelOffset: Integer read FBevelOffset write SetBevelOffset default 0;
    property RectEllipse: TJvgPointClass read FRectEllipse write FRectEllipse;
    property ShapeBitmap: TBitmap read FShapeBitmap write SetShapeBitmap;
    property Align;
    property ShowHint;
    property ParentShowHint;
    property PopupMenu;
    // property Visible;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Math,
  JvgUtils;

constructor TJvgHoleShape.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShapeBitmap := TBitmap.Create;
  FEnabled := (Owner is TWinControl);

  ControlStyle := ControlStyle - [csOpaque];
  FEnabledAllInDesignTime := FEnabled;
  FRectEllipse := TJvgPointClass.Create;
  FRectEllipse.X := 30;
  FRectEllipse.Y := 30;
  FRectEllipse.OnChanged := SmthChanged;
  FShape := stEllipse;
  FBevelOuter := bvLowered;
  FBevelInner := bvNone;
  FCombineMode := cmDIFF;
  FBevelInnerBold := True;
  FBevelOuterBold := True;
  FRectEllipse.Y := 45;
  FRectEllipse.X := 45;
  FBevelOffset := 0;
  Width := 112;
  Height := 112;
  FNeedUpdateRgn := False;
end;

destructor TJvgHoleShape.Destroy;
begin
  FShapeBitmap.Free;
  FRectEllipse.Free;
  if not (csDestroying in Owner.ComponentState) then
  begin
    FEnabledAllInDesignTime := False;
    FEnabled := False;
    UpdateRGN;
  end;
  inherited Destroy;
end;

procedure TJvgHoleShape.Loaded;
begin
  inherited Loaded;
  FNeedRebuildBitmapShape := True;
  UpdateRGN;
  Refresh;
end;

procedure TJvgHoleShape.Paint;
var
  R: TRect;
  H, W, EH, EW, I: Integer;

  procedure DrawShape(Bevel: TPanelBevel; ABold, ARect: Boolean);

    procedure SetPenAndBrush(C: TColor);
    begin
      Canvas.Pen.Color := C;
      if ARect and ((EW and EH) = 0) then
        Canvas.Brush.Style := bsClear
      else
        Canvas.Brush.Color := C;
    end;

  begin
    Canvas.Brush.Style := bsClear; //bsSolid;//bsClear;
    I := Integer(ABold);
    with Canvas do
      case Bevel of
        bvLowered:
          begin
            SetPenAndBrush(clBtnHighlight);
            if ARect then
              RoundRect(R.Left, R.Top, R.Right, R.Bottom, EW, EH)
            else
              Ellipse(R.Left, R.Top, R.Right, R.Bottom);
            SetPenAndBrush(clBtnShadow);
            if ARect then
              RoundRect(R.Left, R.Top, R.Right - 1, R.Bottom - 1, EW, EH)
            else
              Ellipse(R.Left, R.Top, R.Right - 1, R.Bottom - 1);
            if ABold then
            begin
              SetPenAndBrush(cl3DDkShadow);
              if ARect then
                RoundRect(R.Left + 1, R.Top + 1, R.Right - 1, R.Bottom - 1, EW, EH)
              else
                Ellipse(R.Left + 1, R.Top + 1, R.Right - 1, R.Bottom - 1);
            end;
            InflateRect(R, -1, -1);
            Inc(R.Left, I);
            Inc(R.Top, I);
          end;
        bvRaised:
          begin
            SetPenAndBrush(clBtnHighlight);
            if ARect then
              RoundRect(R.Left, R.Top, R.Right, R.Bottom, EW, EH)
            else
              Ellipse(R.Left, R.Top, R.Right, R.Bottom);
            if ABold then
            begin
              SetPenAndBrush(cl3DDkShadow);
              if ARect then
                RoundRect(R.Left + 1, R.Top + 1, R.Right, R.Bottom, EW, EH)
              else
                Ellipse(R.Left + 1, R.Top + 1, R.Right, R.Bottom);
            end;
            SetPenAndBrush(clBtnShadow);
            if ARect then
              RoundRect(R.Left + 1, R.Top + 1, R.Right - I, R.Bottom - I, EW, EH)
            else
              Ellipse(R.Left + 1, R.Top + 1, R.Right - I, R.Bottom - I);
            InflateRect(R, -1, -1);
            Dec(R.Right, I);
            Dec(R.Bottom, I);
          end;
      else
        begin
          //Brush.Color:=clBlack;
          //FrameRect( Rect(Left, Top, Left+W, Top+H) );
        end;
      end;
    SetPenAndBrush(clBtnFace);
  end;

begin
  FNeedUpdateRgn := FNeedUpdateRgn or (FOldX <> Left) or (FOldY <> Top) or
    (FOldW <> Width) or (FOldH <> Height);

  if FNeedUpdateRgn then
    UpdateRGN;
  FOldX := Left;
  FOldY := Top;
  FOldW := Width;
  FOldH := Height;

  if IsItAFilledBitmap(FShapeBitmap) then
  begin
    BitBlt(Canvas.Handle, -1, -1, Width, Height, FShapeBitmap.Canvas.Handle,
      0, 0, SRCCopy);
    Exit;
  end;

  case FShape of
    stRectangle, stRoundRect, stEllipse:
      begin
        H := Height;
        W := Width;
      end;
  else
    H := Min(Height, Width);
    W := H;
  end;
  R := Bounds(0, 0, W, H);
  with Canvas do
    case FShape of
      stRectangle, stSquare, stRoundRect, stRoundSquare:
        begin
          if (FShape = stRectangle) or (FShape = stSquare) then
          begin
            EW := 0;
            EH := 0;
          end;
          if (FShape = stRoundRect) or (FShape = stRoundSquare) then
          begin
            EW := FRectEllipse.X;
            EH := FRectEllipse.Y;
          end;

          DrawShape(FBevelOuter, FBevelOuterBold, True);
          InflateRect(R, -FBevelOffset, -FBevelOffset);
          DrawShape(FBevelInner, FBevelInnerBold, True);

          //Pen.Color:=clBtnFace;
          //Rect( R.Left, R.Top, R.Right, R.Bottom );
        end;
      stEllipse, stCircle:
        begin
          DrawShape(FBevelOuter, FBevelOuterBold, False);
          InflateRect(R, -FBevelOffset, -FBevelOffset);
          DrawShape(FBevelInner, FBevelInnerBold, False);
        end;
    end;
end;

procedure TJvgHoleShape.CalcRGNs;
var
  H, W, xOffs, yOffs: Integer;
  R: TRect;
  BmpInfo: Windows.TBitmap;
  BorderStyle: TFormBorderStyle;

  procedure CalcShape(Bevel: TPanelBevel; ABold: Boolean);
  var
    I: Integer;
  begin
    I := Integer(ABold);
    case Bevel of
      bvLowered:
        begin
          InflateRect(R, -1, -1);
          Inc(R.Left, I);
          Inc(R.Top, I);
        end;
      bvRaised:
        begin
          InflateRect(R, -1, -1);
          Dec(R.Right, I);
          Dec(R.Bottom, I);
        end;
    end;
  end;

  procedure CalcBmpRgn(var Rgn: HRGN);
  var
    I, J: Integer;
    Rgn2: HRGN;
    TransparentColor: TColor;
  begin
    TransparentColor := FShapeBitmap.Canvas.Pixels[0, FShapeBitmap.Height - 1];
    for J := 0 to FShapeBitmap.Height do
      for I := 0 to FShapeBitmap.Width do
        if FShapeBitmap.Canvas.Pixels[I, J] = TransparentColor then
        begin
          Rgn2 := CreateRectRgn(I, J, I + 1, J + 1);
          CombineRgn(Rgn, Rgn2, Rgn, RGN_OR);
          DeleteObject(Rgn2);
        end;
  end;

begin
  if not FShapeBitmap.Empty then
  begin
    {if FNeedRebuildBitmapShape then}
    with FShapeBitmap do
    begin
      GetObject(FShapeBitmap.Handle, SizeOf(Windows.TBitmap), @BmpInfo);
      DeleteObject(RGNOuter);
      DeleteObject(RGNInner);
      RGNInner := CreateRectRgn(0, 0, 0, 0);
      CalcBmpRgn(FRGNInner);
      FNeedRebuildBitmapShape := False;
    end;
  end
  else
  begin
    case FShape of
      stRectangle, stRoundRect, stEllipse:
        begin
          H := Height;
          W := Width;
        end
    else
      H := Min(Height, Width);
      W := H;
    end;
    R := Bounds(0, 0, W, H);
    DeleteObject(RGNOuter);
    DeleteObject(RGNInner);

    if FBevelOffset <> 0 then
    begin
      CalcShape(FBevelOuter, FBevelOuterBold);
      OffsetRect(R, 1, 1);
    end;
    case FShape of
      stRectangle, stSquare:
        RGNOuter := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
      stRoundRect, stRoundSquare:
        RGNOuter := CreateRoundRectRgn(R.Left, R.Top, R.Right, R.Bottom,
          FRectEllipse.X, FRectEllipse.Y);
      stEllipse, stCircle:
        RGNOuter := CreateEllipticRgn(R.Left, R.Top, R.Right, R.Bottom);
    end;
    if FBevelOffset = 0 then
      CalcShape(FBevelOuter, FBevelOuterBold);
    InflateRect(R, -FBevelOffset, -FBevelOffset);
    if FBevelOffset = 0 then
      CalcShape(FBevelInner, FBevelInnerBold)
    else
      OffsetRect(R, -1, -1);
    case FShape of
      stRectangle, stSquare:
        RGNInner := CreateRectRgn(R.Left + 1, R.Top + 1, R.Right + 1,
          R.Bottom + 1);
      stRoundRect, stRoundSquare:
        RGNInner := CreateRoundRectRgn(R.Left + 1, R.Top + 1, R.Right + 2,
          R.Bottom + 2, FRectEllipse.X, FRectEllipse.Y);
      stEllipse, stCircle:
        RGNInner := CreateEllipticRgn(R.Left + 1, R.Top + 1, R.Right + 2,
          R.Bottom + 2);
    end;
  end;

  { calc offsets }
  if Owner is TForm then
  begin
    if csDesigning in ComponentState then
      BorderStyle := bsSizeable
    else
      BorderStyle := TForm(Owner).BorderStyle;
    case BorderStyle of
      bsSizeable:
        begin
          xOffs := GetSystemMetrics(SM_CXFRAME) - 1;
          yOffs := GetSystemMetrics(SM_CYFRAME) - 1;
          Inc(yOffs, GetSystemMetrics(SM_CYCAPTION));
        end;
      bsDialog:
        begin
          xOffs := GetSystemMetrics(SM_CXDLGFRAME) - 1;
          yOffs := GetSystemMetrics(SM_CYDLGFRAME) - 1;
          Inc(yOffs, GetSystemMetrics(SM_CYCAPTION));
        end;
      bsSingle:
        begin
          xOffs := GetSystemMetrics(SM_CXBORDER);
          yOffs := GetSystemMetrics(SM_CYBORDER);
          Inc(yOffs, GetSystemMetrics(SM_CYCAPTION));
        end;
      bsToolWindow:
        begin
          xOffs := GetSystemMetrics(SM_CXBORDER);
          yOffs := GetSystemMetrics(SM_CYBORDER);
          Inc(yOffs, GetSystemMetrics(SM_CYSMCAPTION));
        end;
      bsSizeToolWin:
        begin
          xOffs := GetSystemMetrics(SM_CXSIZEFRAME);
          yOffs := GetSystemMetrics(SM_CYSIZEFRAME);
          Inc(yOffs, GetSystemMetrics(SM_CYSMCAPTION));
        end;
    else
      begin
        xOffs := -1;
        yOffs := -1;
      end;
    end;

    OffsetRgn(RGNInner, Left + xOffs, Top + yOffs);
    OffsetRgn(RGNOuter, Left + xOffs, Top + yOffs);
  end;
end;

//...set all enabled/disabled in design time

procedure TJvgHoleShape.SayAllDTEnabledState(EnabledDT: Boolean);
var
  I: Integer;
begin
  for I := 0 to TWinControl(Owner).ControlCount - 1 do
    with TWinControl(Owner) do
      if Controls[I] is TJvgHoleShape then
        TJvgHoleShape(Controls[I]).FEnabledAllInDesignTime := EnabledDT;
end;

procedure TJvgHoleShape.UpdateRGN;
const
  cCombMode: array [0..4] of Integer =
    (RGN_AND, RGN_COPY, RGN_DIFF, RGN_OR, RGN_XOR);
var
  I: Integer;
  NewRGN: HRGN;
begin
  if not (Owner is TWinControl) then
    Exit;
  NewRGN := CreateRectRgn(0, 0, 2000, 1000);

  for I := 0 to TWinControl(Owner).ControlCount - 1 do
    with TWinControl(Owner) do
      if Controls[I] is TJvgHoleShape then
        with TJvgHoleShape(Controls[I]) do
          if ((csDesigning in ComponentState) and FEnabledAllInDesignTime) or
            ((not (csDesigning in ComponentState)) and FEnabled) then
          begin
            CalcRGNs;
            CombineRgn(NewRGN, NewRGN, RGNInner, cCombMode[Integer(FCombineMode)]);
          end;

  SetWindowRgn(TWinControl(Owner).Handle, NewRGN, True);
  FNeedUpdateRgn := False;
end;

procedure TJvgHoleShape.InternalUpdate;
begin
  if not (csLoading in ComponentState) then
  begin
    UpdateRGN;
    Refresh;
  end;
end;

procedure TJvgHoleShape.SmthChanged(Sender: TObject);
begin
  InternalUpdate;
end;

procedure TJvgHoleShape.SetEnabled(Value: Boolean);
begin
  if (FEnabled <> Value) and (Owner is TWinControl) then
  begin
    FEnabled := Value;
    InternalUpdate;
  end;
end;

procedure TJvgHoleShape.SetEnabledAllInDesignTime(Value: Boolean);
begin
  if (FEnabledAllInDesignTime <> Value) and (Owner is TWinControl) then
  begin
    FEnabledAllInDesignTime := Value;
    SayAllDTEnabledState(FEnabledAllInDesignTime);
    InternalUpdate;
  end;
end;

procedure TJvgHoleShape.SetShape(Value: THoleShapeType);
begin
  if FShape <> Value then
  begin
    FShape := Value;
    InternalUpdate;
  end;
end;

procedure TJvgHoleShape.SetShapeBitmap(Value: TBitmap);
begin
  if FShapeBitmap <> Value then
  begin
    FNeedRebuildBitmapShape := True;
    FShapeBitmap.Assign(Value);
    if Assigned(FShapeBitmap) then
    begin
      Width := FShapeBitmap.Width;
      Height := FShapeBitmap.Width;
    end;
    InternalUpdate;
  end;
end;

procedure TJvgHoleShape.SetBevelInner(Value: TPanelBevel);
begin
  if FBevelInner <> Value then
  begin
    FBevelInner := Value;
    InternalUpdate;
  end;
end;

procedure TJvgHoleShape.SetBevelOuter(Value: TPanelBevel);
begin
  if FBevelOuter <> Value then
  begin
    FBevelOuter := Value;
    InternalUpdate;
  end;
end;

procedure TJvgHoleShape.SetBevelInnerBold(Value: Boolean);
begin
  if FBevelInnerBold <> Value then
  begin
    FBevelInnerBold := Value;
    InternalUpdate;
  end;
end;

procedure TJvgHoleShape.SetBevelOuterBold(Value: Boolean);
begin
  if FBevelOuterBold <> Value then
  begin
    FBevelOuterBold := Value;
    InternalUpdate;
  end;
end;

procedure TJvgHoleShape.SetCombineMode(Value: TRGNCombineMode);
begin
  if FCombineMode <> Value then
  begin
    FCombineMode := Value;
    InternalUpdate;
  end;
end;

procedure TJvgHoleShape.SetBevelOffset(Value: Integer);
begin
  if (FBevelOffset <> Value) and (Value >= 0) then
  begin
    if (Value > Width - 2) or (Value > Height - 2) then
      Value := Min(Width, Height) - 2;
    FBevelOffset := Value;
    InternalUpdate;
  end;
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

