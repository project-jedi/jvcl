{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgSplit.PAS, released on 2003-01-15.

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

unit JvgSplit;

{$I jvcl.inc}

interface

uses
  Windows, Messages, Classes, Controls, Graphics, ExtCtrls,
  JvComponent, JVCLVer;

type
  TJvgSplitter = class(TSplitter)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FHotTrack: Boolean;
    FTrackCount: Integer;
    FActive: Boolean;
    FDisplace: Boolean;
    FKeepSize: Integer; (* +++ RDB --- *)
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMMouseDblClick(var Msg: TMessage); message WM_LBUTTONDBLCLK;
    procedure SetTrackCount(const Value: Integer);
    procedure UpdateControlSize;
    function FindControl: TControl;
    procedure PrepareMarcs(Align: TAlign; var Pt1, Pt2, Pt3, Pt4, Pt5, Pt6: TPoint);
    procedure SetDisplace(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Displace: Boolean read FDisplace write SetDisplace default True;
    property HotTrack: Boolean read FHotTrack write FHotTrack default True;
    property TrackCount: Integer read FTrackCount write SetTrackCount default 20;
    property Width default 6;
  end;

implementation

uses
  JvThemes;

constructor TJvgSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IncludeThemeStyle(Self, [csParentBackground]);
  FKeepSize := 0;
  //..defaults
  Width := 6;
  FHotTrack := True;
  FDisplace := True;
  FTrackCount := 20;
end;

procedure TJvgSplitter.Paint;
var
  I: Integer;
  SColor: TColor;
  Pt1, Pt2, Pt3, Pt4, Pt5, Pt6: TPoint;
  R, R1, R2: TRect;
begin
  with Canvas do
  begin

    Brush.Color := Self.Color;
    DrawThemedBackground(Self, Canvas, ClientRect);

    if (Align = alBottom) or (Align = alTop) then
    begin
      R1 := Classes.Bounds((Width - FTrackCount * 4) div 2, 0, 3, 3);
      R2 := Classes.Bounds((Width - FTrackCount * 4) div 2, 3, 3, 3);
    end
    else
    begin
      R1 := Classes.Bounds(0, (Height - FTrackCount * 4) div 2, 3, 3);
      R2 := Classes.Bounds(3, (Height - FTrackCount * 4) div 2, 3, 3);
    end;

    for I := 0 to FTrackCount - 1 do
    begin
      {$IFDEF JVCLThemesEnabled}
      if FActive and HotTrack and ThemeServices.ThemesEnabled then
        SColor := RGB(100, 100, 100)
      else
      {$ENDIF JVCLThemesEnabled}
      if FActive and HotTrack then
        SColor := clBlack
      else
        SColor := clBtnShadow;

      R := R1;
      Frame3D(Canvas, R, clBtnHighlight, SColor, 1);
      R := R2;
      Frame3D(Canvas, R, clBtnHighlight, SColor, 1);

      if (Align = alBottom) or (Align = alTop) then
      begin
        OffsetRect(R1, 4, 0);
        OffsetRect(R2, 4, 0);
      end
      else
      begin
        OffsetRect(R1, 0, 4);
        OffsetRect(R2, 0, 4);
      end;

    end;
    if FDisplace then
    begin
      PrepareMarcs(Align, Pt1, Pt2, Pt3, Pt4, Pt5, Pt6);
      if FActive then
        Canvas.Brush.Color := clGray
      else
        Canvas.Brush.Color := clWhite;
      Canvas.Polygon([Pt1, Pt2, Pt3]);
      Canvas.Polygon([Pt4, Pt5, Pt6]);
    end;
  end;
end;

procedure TJvgSplitter.PrepareMarcs(Align: TAlign; var Pt1, Pt2, Pt3, Pt4, Pt5, Pt6: TPoint);
begin
  case Align of
    alRight:
      begin
        Pt1.X := 1;
        Pt1.Y := (Height - FTrackCount * 4) div 2 - 30;
        Pt2.X := 1;
        Pt2.Y := Pt1.Y + 6;
        Pt3.X := 4;
        Pt3.Y := Pt1.Y + 3;

        Pt4.X := 1;
        Pt4.Y := (Height - FTrackCount * 4) div 2 + FTrackCount * 4 + 30 -
          7;
        Pt5.X := 1;
        Pt5.Y := Pt4.Y + 6;
        Pt6.X := 4;
        Pt6.Y := Pt4.Y + 3;
      end;
    alLeft:
      begin
        Pt1.X := 3;
        Pt1.Y := (Height - FTrackCount * 4) div 2 - 30;
        Pt2.X := 3;
        Pt2.Y := Pt1.Y + 6;
        Pt3.X := 0;
        Pt3.Y := Pt1.Y + 3;

        Pt4.X := 3;
        Pt4.Y := (Height - FTrackCount * 4) div 2 + FTrackCount * 4 + 30 -
          7;
        Pt5.X := 3;
        Pt5.Y := Pt4.Y + 6;
        Pt6.X := 0;
        Pt6.Y := Pt4.Y + 3;
      end;
    alTop:
      begin
        Pt1.X := (Width - FTrackCount * 4) div 2 - 30;
        Pt1.Y := 4;
        Pt2.X := Pt1.X + 6;
        Pt2.Y := 4;
        Pt3.X := Pt1.X + 3;
        Pt3.Y := 1;

        Pt4.X := (Width - FTrackCount * 4) div 2 + FTrackCount * 4 + 30 - 7;
        Pt4.Y := 4;
        Pt5.X := Pt4.X + 6;
        Pt5.Y := 4;
        Pt6.X := Pt4.X + 3;
        Pt6.Y := 1;
      end;
    alBottom:
      begin
        Pt1.X := (Width - FTrackCount * 4) div 2 - 30;
        Pt1.Y := 1;
        Pt2.X := Pt1.X + 6;
        Pt2.Y := 1;
        Pt3.X := Pt1.X + 3;
        Pt3.Y := 4;

        Pt4.X := (Width - FTrackCount * 4) div 2 + FTrackCount * 4 + 30 - 7;
        Pt4.Y := 1;
        Pt5.X := Pt4.X + 6;
        Pt5.Y := 1;
        Pt6.X := Pt4.X + 3;
        Pt6.Y := 4;
      end;
  end;
end;

procedure TJvgSplitter.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  FActive := True;
  Invalidate;
end;

procedure TJvgSplitter.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FActive := False;
  Invalidate;
end;

procedure TJvgSplitter.SetTrackCount(const Value: Integer);
begin
  FTrackCount := Value;
  Invalidate;
end;

procedure TJvgSplitter.WMMouseDblClick(var Msg: TMessage);
begin
  if FDisplace then
    UpdateControlSize;
end;

procedure TJvgSplitter.UpdateControlSize;
const
  cNewSize = 0;
var
  FControl: TControl;
begin
  FControl := FindControl;
  if not Assigned(FControl) then
    Exit;
  if FKeepSize = 0 then
  begin
    case Align of
      alLeft:
        begin
          FKeepSize := FControl.Width;
          FControl.Width := cNewSize;
        end;
      alTop:
        begin
          FKeepSize := FControl.Height;
          FControl.Height := cNewSize;
        end;
      alRight:
        begin
          FKeepSize := FControl.Width;
          Parent.DisableAlign;
          try
            FControl.Left := FControl.Left + (FControl.Width - cNewSize);
            FControl.Width := cNewSize;
          finally
            Parent.EnableAlign;
          end;
        end;
      alBottom:
        begin
          FKeepSize := FControl.Height;
          Parent.DisableAlign;
          try
            FControl.Top := FControl.Top + (FControl.Height - cNewSize);
            FControl.Height := cNewSize;
          finally
            Parent.EnableAlign;
          end;
        end;
    end;
  end
  else (* ++++ RDB +++ *)
  begin
    case Align of
      alLeft:
        FControl.Width := FKeepSize;
      alTop:
        FControl.Height := FKeepSize;
      alRight:
        begin
          Parent.DisableAlign;
          try
            FControl.Left := FControl.Left + (FControl.Width - FKeepSize);
            FControl.Width := FKeepSize;
          finally
            Parent.EnableAlign;
          end;
        end;
      alBottom:
        begin
          Parent.DisableAlign;
          try
            FControl.Top := FControl.Top + (FControl.Height - FKeepSize);
            FControl.Height := FKeepSize;
          finally
            Parent.EnableAlign;
          end;
        end;
    end;
    FKeepSize := 0; (* --- RDB --- *)
  end;
  Update;
  if Assigned(OnMoved) then
    OnMoved(Self);
end;

function TJvgSplitter.FindControl: TControl;
var
  P: TPoint;
  I: Integer;
  R: TRect;
begin
  Result := nil;
  P := Point(Left, Top);
  case Align of
    alLeft:
      Dec(P.X);
    alRight:
      Inc(P.X, Width);
    alTop:
      Dec(P.Y);
    alBottom:
      Inc(P.Y, Height);
  else
    Exit;
  end;
  for I := 0 to Parent.ControlCount - 1 do
  begin
    Result := Parent.Controls[I];
    if Result.Visible and Result.Enabled then
    begin
      R := Result.BoundsRect;
      if (R.Right - R.Left) = 0 then
        if Align in [alTop, alLeft] then
          Dec(R.Left)
        else
          Inc(R.Right);
      if (R.Bottom - R.Top) = 0 then
        if Align in [alTop, alLeft] then
          Dec(R.Top)
        else
          Inc(R.Bottom);
      if PtInRect(R, P) then
        Exit;
    end;
  end;
  Result := nil;
end;

procedure TJvgSplitter.SetDisplace(const Value: Boolean);
begin
  FDisplace := Value;
  Invalidate;
end;

end.

