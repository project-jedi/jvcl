{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSlider.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvSlider;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages,
  {$ENDIF VCL}
  Types, Graphics, Controls, ExtCtrls,
  JvComponent;

type
  TJvSlider = class(TJvCustomControl)
  private
    {$IFDEF VisualCLX}
    FAutoSize: boolean
    {$ENDIF VisualCLX}
    FImageRuler: TBitmap;
    FImageThumb: TBitmap;
    FThumb1: TBitmap;
    FThumb2: TBitmap;
    FHorizontal: Boolean;
    FClicked: Boolean;
    FTracking: Boolean;
    FMaximum: Integer;
    FDifference: Real;
    FPosition: Integer;
    FFrom: Integer;
    FChanged: Boolean;
    FChanging: Boolean;
    FOnChanged: TNotifyEvent;
    FOnStopChanged: TNotifyEvent;
    FOnBeginChange: TNotifyEvent;
    FTimer: TTimer;
    procedure SetImageThumb(Value: TBitmap);
    procedure SetImageRuler(Value: TBitmap);
    procedure ThumbChanged(Sender: TObject);
    procedure SetMaximum(Value: Integer);
    procedure Calculate;
    procedure ReCalcule(Sender: TObject);
    procedure SetPosition(Value: Integer);
    procedure Loading(Sender: TObject);
  protected
    {$IFDEF VCL}
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    {$ENDIF VCL}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;{$IFDEF VCL} override;{$ENDIF}
  published
    property ImageRuler: TBitmap read FImageRuler write SetImageRuler;
    property ImageThumb: TBitmap read FImageThumb write SetImageThumb;
    property Align;
    property Visible;
    property Enabled;
    property Cursor;
    property DragMode;
    {$IFDEF VCL}
    property DragCursor;
    {$ENDIF VCL}
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property Width default 191;
    property Height default 11;
    {$IFDEF VCL}
    property AutoSize default True;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    property AutoSize: boolean read FAutoSize write FAutoSize default True;
    {$ENDIF VisualCLX}
    property Horizontal: Boolean read FHorizontal write FHorizontal default True;
    property Maximum: Integer read FMaximum write SetMaximum default 100;
    property Position: Integer read FPosition write SetPosition default 0;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnDragOver;
    property OnDragDrop;
    property OnEndDrag;
    property OnStartDrag;
    property OnBeginChange: TNotifyEvent read FOnBeginChange write FOnBeginChange;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnStopChanged: TNotifyEvent read FOnStopChanged write FOnStopChanged;
  end;

implementation

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvSlider.res}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvSlider.res}
{$ENDIF LINUX}

constructor TJvSlider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  Width := 191;
  Height := 11;
  FImageRuler := TBitmap.Create;
  FImageThumb := TBitmap.Create;
  FThumb1 := TBitmap.Create;
  FThumb2 := TBitmap.Create;
  FClicked := False;
  FMaximum := 100;
  FTracking := False;
  FPosition := 0;
  FFrom := 0;
  FChanged := False;
  FHorizontal := True;
  FChanging := False;
  FImageThumb.LoadFromResourceName(hInstance, 'THUMB');
  FImageRuler.LoadFromResourceName(hInstance, 'RULER');
  Calculate;
  FImageThumb.OnChange := ThumbChanged;
  Self.OnResize := ReCalcule;
  Calculate;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 10;
  FTimer.OnTimer := Loading;
  FTimer.Enabled := True;
  AutoSize := True;
end;

destructor TJvSlider.Destroy;
begin
  FImageRuler.Free;
  FImageThumb.Free;
  FThumb1.Free;
  FThumb2.Free;
  inherited Destroy;
end;

procedure TJvSlider.Paint;
var
  T: TRect;
begin
  T.Left := 0;
  T.Top := 0;
  T.Right := Width;
  T.Bottom := Height;
  Canvas.StretchDraw(T, FImageRuler);
  if FHorizontal then
  begin
    // horizontal
    T.Left := Round(FDifference * FPosition);
    if Height - FThumb1.Height < 0 then
      T.Top := 0
    else
      T.Top := (Height - FThumb1.Height) div 2;
    FFrom := T.Top;
    Canvas.Draw(T.Left, T.Top, FThumb1);
    FClicked := False;
  end
  else
  begin
    // vertical
    if Width - FThumb1.Width < 0 then
      T.Left := 0
    else
      T.Left := (Width - FThumb1.Width) div 2;
    T.Top := Round(FDifference * FPosition);
    FFrom := T.Left;
    Canvas.Draw(T.Left, T.Top, FThumb1);
    FClicked := False;
  end;
end;

procedure TJvSlider.SetMaximum(Value: Integer);
begin
  FMaximum := Value;
  if FPosition > FMaximum then
    FPosition := FMaximum;
  Calculate;
  SetPosition(FPosition);
end;

procedure TJvSlider.SetPosition(Value: Integer);
begin
  if Value > FMaximum then
    Value := FMaximum;
  // (rom) fixed the if
  if Value < 0 then
    Value := 0;
  FPosition := Value;

  if not FTracking then
  begin
    Calculate;
    Repaint;
  end;
end;

procedure TJvSlider.Calculate;
begin
  // calculate the difference between pixels
  if FHorizontal then
    FDifference := (Width - FThumb1.Width) / FMaximum
  else
    FDifference := (Height - FThumb1.Height) / FMaximum;
end;

procedure TJvSlider.ReCalcule(Sender: TObject);
begin
  Calculate;
end;

procedure TJvSlider.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  if FTracking and (ssLeft in Shift) then
  begin
    if FHorizontal then
    begin
      if (Y in [0..FThumb1.Height]) or FChanging then
      begin
        I := X - FThumb1.Width div 2;
        if I > 0 then
        begin
          FChanging := True;
          I := Round(I / FDifference);
          if I > FMaximum then
            I := FMaximum;
          FPosition := I;
          if Assigned(FOnChanged) then
            FOnChanged(Self);
          Repaint;
        end;
      end;
    end
    else
    begin
      if (X in [0..FThumb1.Width]) or FChanging then
      begin
        I := Y - FThumb1.Height div 2;
        if I > 0 then
        begin
          FChanging := True;
          I := Round(I / FDifference);
          if I > FMaximum then
            I := FMaximum;
          FPosition := I;
          if Assigned(FOnChanged) then
            FOnChanged(Self);
          Repaint;
        end;
      end;
    end;
    Repaint;
  end;
end;

procedure TJvSlider.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Tmp: TBitmap;
  R:TRect;
  P:TPoint;
begin
  FTracking := True;
  MouseCapture := True;
  R := ClientRect;
  P := ClientToScreen(Point(0,0));
  OffsetRect(R, P.X, P.Y);
  ClipCursor(@R);
  if Assigned(FOnBeginChange) then
    FOnBeginChange(Self);
  if not FChanged then
  begin
    Tmp := TBitmap.Create;
    Tmp.Assign(FThumb1);
    FThumb1.Assign(FThumb2);
    FThumb2.Assign(Tmp);
    Tmp.Free;
    FChanged := True;
  end;
  Repaint;
end;

procedure TJvSlider.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Tmp: TBitmap;
begin
  FTracking := False;
  FChanging := False;
  ClipCursor(nil);
  if FChanged then
  begin
    Tmp := TBitmap.Create;
    Tmp.Assign(FThumb1);
    FThumb1.Assign(FThumb2);
    FThumb2.Assign(Tmp);
    Tmp.Free;
    FChanged := False;
  end;
  Repaint;
  if Assigned(FOnStopChanged) then
    FOnStopChanged(Self);
end;

procedure TJvSlider.ThumbChanged(Sender: TObject);
var
  Src, Dest: TRect;
begin
  Dest.Left := 0;
  Dest.Top := 0;
  Dest.Right := FImageThumb.Width div 2;
  Dest.Bottom := FImageThumb.Height;
  FThumb1.Width := Dest.Right;
  FThumb1.Height := Dest.Bottom;
  FThumb1.Canvas.CopyRect(Dest, FImageThumb.Canvas, Dest);
  FThumb2.Width := Dest.Right;
  FThumb2.Height := Dest.Bottom;
  Dest.Left := Dest.Right;
  Dest.Top := 0;
  Dest.Bottom := FImageThumb.Height;
  Dest.Right := FImageThumb.Width;
  Src.Left := 0;
  Src.Top := 0;
  Src.Right := Dest.Left;
  Src.Bottom := FImageThumb.Height;
  FThumb2.Canvas.CopyRect(Src, FImageThumb.Canvas, Dest);
  Invalidate;
  Calculate;
end;

procedure TJvSlider.SetImageThumb(Value: TBitmap);
begin
  FImageThumb.Assign(Value);
  ThumbChanged(nil);
end;

procedure TJvSlider.SetImageRuler(Value: TBitmap);
begin
  FImageRuler.Assign(Value);
  if (Value.Width > 0) and (Value.Height > 0) and AutoSize then
  begin
    Height := Value.Height;
    Width := Value.Width;
  end;
  Repaint;
  Calculate;
end;

procedure TJvSlider.Loading(Sender: TObject);
begin
  FTimer.Enabled := False;
  SetImageThumb(FImageThumb);
  ThumbChanged(Self);
  Calculate;
  FTimer.Free;
end;
{$IFDEF VCL}
procedure TJvSlider.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;{$ENDIF VCL}

function TJvSlider.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  if AutoSize and (FImageRuler.Width > 0) and (FImageRuler.Height > 0) then
  begin
    NewHeight := FImageRuler.Height;
    NewWidth := FImageRuler.Width;
    Result := True;
  end
  else
    Result := False;
end;

end.

