{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvImageWindow.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 att users dott sourceforge dott net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{ A component that can display a grid of images taken from a TCustomImageList }

unit JvImageWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, ExtCtrls, CommCtrl, ImgList,
  JvComponent;

type
  TJvMargin = 2..24;
  TJvPositive = 1..MaxInt;

  TJvImageWindow = class(TJvGraphicControl)
  private
    FImageList: TCustomImageList;
    FIndex: Integer;
    OldX: Integer;
    OldY: Integer;
    imWidth: Integer;
    imHeight: Integer;
    FBackColor: TColor;
    FFrontColor: TColor;
    FGridColor: TColor;
    FMargin: TJvMargin;
    FColCount: TJvPositive;
    FImageCount: Integer;
    FShowFrame: Boolean;
    FShowGrid: Boolean;
    FGhost: Boolean;
    FAutoSize: Boolean;
    FOptimal: Boolean;
    FImageChangeLink: TChangeLink;
    procedure DrawFocusFrame(X, Y: Integer);
    procedure SetBackColor(Value: TColor);
    procedure SetFrontColor(Value: TColor);
    procedure SetGridColor(Value: TColor);
    procedure SetMargin(Value: TJvMargin);
    procedure SetColCount(Value: TJvPositive);
    procedure SetImageCount(Value: Integer);
    procedure SetShowFrame(Value: Boolean);
    procedure SetShowGrid(Value: Boolean);
    procedure SetGhost(Value: Boolean);
    procedure SetImageList(Value: TCustomImageList);
    procedure ImageListChange(Sender: Tobject);
  protected
    procedure Paint; override;
    procedure Changed; dynamic;
    {$IFDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); override;
    {$ENDIF}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SaveImage(Index: Integer; Filename: string; AsBmp: Boolean);
    procedure SaveImageList(Filename: string);
    property ImageIndex: Integer read FIndex default -1; { read-only }
  published
    property Optimal: Boolean read FOptimal write FOptimal default False;
    {$IFDEF COMPILER6_UP}
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    {$ENDIF}
    property BackColor: TColor read FBackColor write SetBackColor default clWindow;
    //    property Filled: Boolean read FFilled write SetFilled default False;
    property FrontColor: TColor read FFrontColor write SetFrontColor default clWindowText;
    property Ghost: Boolean read FGhost write SetGhost;
    property Margin: TJvMargin read FMargin write SetMargin default 2;
    property ColCount: TJvPositive read FColCount write SetColCount default 4;
    property ImageCount: Integer read FImageCount write SetImageCount default 0;
    property Images: TCustomImageList read FImageList write SetImageList;
    property ShowFrame: Boolean read FShowFrame write SetShowFrame default True;
    property ShowGrid: Boolean read FShowGrid write SetShowGrid default True;
    property GridColor: TColor read FGridColor write SetGridColor default clActiveCaption;
    property Width default 64;
    property Height default 64;
    property Align;
    property Visible;
    property Enabled;
    property DragCursor;
    property DragMode;
    property PopupMenu;
    property ParentShowHint;
    property Hint;
    property ShowHint;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
  end;

  TJvImageSquare = class(TJvGraphicControl)
  private
    FHiColor, TmpColor, FBackColor: TColor;
    FBorderStyle: TBorderStyle;
    FImageList: TCustomImageList;
    FIndex: Integer;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FDown: Boolean;
    FShowClick: Boolean;
    FImageChangeLink: TChangeLink;
    procedure SetHiColor(Value: TColor);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetIndex(Value: Integer);
    procedure SetImageList(Value: TCustomImageList);
    procedure ImageListChange(Sender: Tobject);
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintFrame; virtual;
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Color default clWindow;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property HiColor: TColor read FHiColor write SetHiColor default clActiveCaption;
    property Images: TCustomImageList read FImageList write SetImageList;
    property ImageIndex: Integer read FIndex write SetIndex default 0;
    property ShowClick: Boolean read FShowClick write FShowClick default False;
    property Width default 36;
    property Height default 36;

    property Align;
    property Anchors;
    property Action;
    property Text;
    property Visible;
    property Enabled;
    property DragCursor;
    property DragMode;
    property PopupMenu;
    property ParentShowHint;
    property Hint;
    property ShowHint;
    property OnMouseEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnMouseLeave: TNotifyEvent read FOnExit write FOnExit;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
  end;

implementation

uses
  Math,
  JvConsts, JvTypes, JvThemes, JvResources;

//=== TJvImageWindow =========================================================

constructor TJvImageWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackColor := clWindow;
  FFrontColor := clWindowText;
  FColCount := 4;
  FImageCount := 0;
  FShowFrame := True;
  FGhost := False;
  FShowGrid := True;
  FIndex := -1;
  FMargin := 2;
  FAutoSize := False;
  FOptimal := False;
  OldX := -1;
  OldY := -1;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  SetBounds(0, 0, 64, 64);
  Changed;
end;

destructor TJvImageWindow.Destroy;
begin
  FImageChangeLink.Free;
  inherited Destroy;
end;

procedure TJvImageWindow.SaveImageList(Filename: string);
var
  TmpBmp, Bmp: TBitmap;
  I: Integer;
begin
  if not Assigned(FImageList) then
  begin
    EJVCLException.Create(RsEImagesNotAssigned);
    Exit;
  end;
  Bmp := TBitmap.Create;
  TmpBmp := TBitmap.Create;

  try
    Bmp.Height := FImageList.Height;
    Bmp.Width := FImageList.Width * FImageList.Count;
    for I := 0 to FImageList.Count - 1 do
    begin
      FImageList.GetBitmap(I, TmpBmp);
      Bmp.Canvas.Draw(FImageList.Width * I, 0, TmpBmp);
    end;
    Bmp.SaveToFile(Filename);
  finally
    Bmp.Free;
    TmpBmp.Free;
  end;
end;

procedure TJvImageWindow.SaveImage(Index: Integer; Filename: string; AsBmp: Boolean);
var
  Bmp: TBitmap;
  Ico: TIcon;
begin
  if Assigned(FImageList) then
    if AsBmp then
    begin
      Bmp := TBitmap.Create;
      FImageList.GetBitmap(Index, Bmp);
      Bmp.SaveToFile(Filename);
      Bmp.Free;
    end
    else
    begin
      Ico := TIcon.Create;
      FImageList.GetIcon(ImageIndex, Ico);
      Ico.SaveToFile(Filename);
      Ico.Free;
    end;

end;

procedure TJvImageWindow.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (aComponent = FImageList) and (Operation = opRemove) then
  begin
    FImageList := nil;
    SetImageCount(0);
  end;
end;

procedure TJvImageWindow.Paint;
var
  I, X, Y: Integer;
begin
  with Canvas do
  begin
    Brush.Color := FBackColor;
    Pen.Color := FFrontColor;
    if csDesigning in ComponentState then
      Rectangle(0, 0, Width, Height)
    else
      FillRect(Rect(0, 0, Width, Height));
  end;

  if Assigned(FImageList) then
  begin
    X := 0;
    Y := 0;
    for I := 0 to Min(FImageCount - 1, FImageList.Count - 1) do
    begin
      if FShowGrid then
        DrawFocusFrame(X + FMargin, Y + FMargin);
      ImageList_DrawEx(FImageList.Handle, I,
        Canvas.Handle, X + FMargin * 2, Y + Fmargin * 2, 0, 0, CLR_NONE, CLR_NONE, ILD_TRANSPARENT);
      Inc(X, imWidth + FMargin * 2);
      if I mod FColCount = FColCount - 1 then
      begin
        Inc(Y, imHeight + FMargin * 2);
        X := 0;
      end;
    end;
  end;
end;

procedure TJvImageWindow.DrawFocusFrame(X, Y: Integer);
var
  iWidth, iHeight: Integer;
  Rec: TRect;
  Leaving: Boolean;
  FRows: Integer;
begin
  Leaving := False;

  iWidth := Max(imWidth + FMargin * 2, 1);
  iHeight := Max(imHeight + FMargin * 2, 1);

  { get index for X and Y }
  X := trunc(X / iWidth);
  Y := trunc(Y / iHeight);
  FRows := Max((FImageCount div FColCount), 1);

  { inside bounds ? }
  { special case FRows = 1 }
  if ((Y > FRows) and (FRows < 2)) then
    Leaving := True;
  if (X >= FColCount) or (X + Y * FColCount >= FImageCount)
    or ((Y > FRows) and (FImageCount mod FRows = 0)) then
    Leaving := True;
  { get new starting points }
  X := X * (iWidth);
  Y := Y * (iHeight);

  { erase old frame }
  if ((OldX <> X) or (OldY <> Y)) and (OldX <> -1) then
  begin
    //    if FShowGrid then
    Canvas.Brush.Color := FGridColor;
    //    else
    //      Canvas.Brush.Color := FBackColor;
    Rec := Rect(FMargin, FMargin, iWidth + FMargin + 1, iHeight + FMargin + 1);
    OffsetRect(Rec, OldX, OldY);
    {    if FFilled then
          Canvas.FillRect(Rec)
        else}
    Canvas.FrameRect(Rec);
  end;
  if Leaving then
    Exit;

  // draw the actual frame
  Canvas.Brush.Color := FFrontColor;
  Rec := Rect(FMargin, FMargin, iWidth + FMargin + 1, iHeight + FMargin + 1);
  //  if FImageCount > 1 then
  OffsetRect(Rec, X, Y);
  {  if FFilled then
      Canvas.FillRect(Rec)
    else}
  Canvas.FrameRect(Rec);
  if FGhost and FShowGrid then
  begin
    InflateRect(Rec, -1, -1);
    Canvas.FrameRect(Rec);
  end;
  OldX := X;
  OldY := Y;
end;

procedure TJvImageWindow.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if FShowFrame and Assigned(FImageList) then
    DrawFocusFrame(X, Y);
end;

procedure TJvImageWindow.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  iWidth, iHeight: Integer;
begin
  if Assigned(FImageList) then
  begin
    iWidth := Max(imWidth + FMargin * 2, 1);
    iHeight := Max(imHeight + FMargin * 2, 1);

    { get index for X and Y }
    X := Trunc(X / iWidth);
    Y := Trunc(Y / iHeight);
    { convert to imageindex }
    FIndex := X + Y * FColCount;
    if FIndex > FImageCount - 1 then
      FIndex := FImageCount - 1;
    if FIndex < 0 then
      FIndex := -1;
  end;

  if Assigned(OnClick) then
    OnClick(Self);
  //  inherited MouseUp(Button,Shift,X,Y);
end;

{$IFDEF COMPILER6_UP}
procedure TJvImageWindow.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    Changed;
  end;
end;
{$ENDIF}

{ draw a ghost frame too }

procedure TJvImageWindow.SetGhost(Value: Boolean);
begin
  if FGhost <> Value then
  begin
    FGhost := Value;
    Changed;
  end;
end;

procedure TJvImageWindow.ImageListChange(Sender: Tobject);
begin
  FImageCount := Min(FImageCount, FImageList.Count);
  Changed;
end;

procedure TJvImageWindow.SetImageList(Value: TCustomImageList);
begin
  if Images <> nil then
    Images.UnRegisterChanges(FImageChangeLink);
  FImageList := Value;
  if Images <> nil then
    FImageList.RegisterChanges(FImageChangeLink);

  if Assigned(FImageList) then
  begin
    imWidth := FImageList.Width;
    imHeight := FImageList.Height;
    FImageCount := Min(FImageCount, FImageList.Count);
  end
  else
  begin
    imWidth := 16;
    imHeight := 16;
  end;
  Changed;
end;

{
procedure TJvImageWindow.WMEraseBkgnd(var M : TWMEraseBkgnd);
begin
  M.Result := LRESULT(False);
end;
}

procedure TJvImageWindow.SetBackColor(Value: TColor);
begin
  if FBackColor <> Value then
  begin
    FBackColor := Value;
    Invalidate;
  end;
end;

procedure TJvImageWindow.SetFrontColor(Value: TColor);
begin
  if FFrontColor <> Value then
  begin
    FFrontColor := Value;
    Invalidate;
  end;
end;

procedure TJvImageWindow.SetGridColor(Value: TColor);
begin
  if FGridColor <> Value then
  begin
    FGridColor := Value;
    Invalidate;
  end;
end;

procedure TJvImageWindow.SetMargin(Value: TJvMargin);
begin
  if FMargin <> Value then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TJvImageWindow.SetColCount(Value: TJvPositive);
begin
  if FColCount <> Value then
  begin
    FColCount := Value;
    if Assigned(FImageList) and (FColCount > FImageCount) then
      FColCount := Max(FImageCount, 1);
    Changed;
  end;
end;

procedure TJvImageWindow.SetImageCount(Value: Integer);
begin
  if FImageCount <> Value then
  begin
    if Assigned(FImageList) then
      FImageCount := Min(Value, FImageList.Count)
    else
      FImageCount := Value;
    Changed;
  end;
end;

procedure TJvImageWindow.SetShowFrame(Value: Boolean);
begin
  if FShowFrame <> Value then
  begin
    FShowFrame := Value;
    Invalidate;
  end;
end;

procedure TJvImageWindow.SetShowGrid(Value: Boolean);
begin
  if FShowGrid <> Value then
  begin
    FShowGrid := Value;
    Invalidate;
  end;
end;

procedure TJvImageWindow.Changed;
var
  tmp, FNewHeight, FNewWidth: Integer;
begin
  if FOptimal and Assigned(FImageList) then
  begin
    if ImageCount < 3 then
      ColCount := 1
    else
      ColCount := Max(Ceil(Sqrt(ImageCount)), 1);
  end;

  if FAutoSize and Assigned(FImageList) then
  begin
    FColCount := Max(FColCount, 1);
    tmp := FImageCount div FColCount + 1;
    FNewHeight := imHeight * tmp + FMargin * tmp * 2 + FMargin * 2 + 1;
    FNewWidth := imWidth * FColCount + FMargin * FColCount * 2 + FMargin * 2 + 1;
    case Align of
      alNone:
        begin
          Height := FNewHeight;
          Width := FNewWidth;
        end;
      alRight, alLeft:
        Width := FNewWidth;
      alTop, alBottom:
        Height := FNewHeight;
    end;
  end;
  Invalidate;
end;

//=== TJvImageSquare =========================================================

constructor TJvImageSquare.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHiColor := clActiveCaption;
  Color := clWindow;
  TmpColor := clWindow;
  FBackColor := clWindow;
  FIndex := 0;
  FDown := False;
  FShowClick := False;
  Width := 36;
  Height := 36;
  FBorderStyle := bsSingle;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
end;

destructor TJvImageSquare.Destroy;
begin
  FImageChangeLink.Free;
  inherited Destroy;
end;

procedure TJvImageSquare.ImageListChange(Sender: Tobject);
begin
  Repaint;
end;

procedure TJvImageSquare.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (aComponent = FImageList) and (Operation = opRemove) then
    FImageList := nil;
end;

procedure TJvImageSquare.PaintFrame;
var
  R: TRect;
begin
  R := GetClientRect;
  if FDown and FShowClick then
  begin
    Frame3d(Canvas, R, cl3DDkShadow, cl3DDkShadow, 1);
    Frame3d(Canvas, R, clBtnHighLight, clBtnHighLight, 1);
    Frame3d(Canvas, R, cl3DDkShadow, cl3DDkShadow, 1);
  end
  else
{$IFDEF JVCLThemesEnabled}
  if (FBorderStyle = bsSingle) and ThemeServices.ThemesEnabled then
    DrawThemedBorder(Self)
  else
{$ENDIF}
  if FBorderStyle = bsSingle then
  begin
    Frame3d(Canvas, R, clBtnFace, clBtnFace, 1);
    Frame3d(Canvas, R, clBtnShadow, clBtnHighLight, 1);
    Frame3d(Canvas, R, cl3DDkShadow, clBtnFace, 1);
  end
  else
    Frame3d(Canvas, R, FHiColor, FHiColor, 3);
end;

procedure TJvImageSquare.Paint;
var
  R: TRect;
  dX, dY: Integer;
begin
  R := Rect(0, 0, Width, Height);

  if FBorderStyle = bsSingle then
  begin
    PaintFrame;
    InflateRect(R, -3, -3);
  end;

  { fill in the rest }
  with Canvas do
  begin
    Brush.Color := TmpColor;
    Brush.Style := bsSolid;
    FillRect(R);
  end;

  if Assigned(FImageList) then
  begin
    { draw in middle }
    dX := (Width - FImageList.Width) div 2;
    dY := (Height - FImageList.Height) div 2;
    ImageList_DrawEx(Fimagelist.Handle, FIndex, Canvas.Handle, dx, dy, 0, 0, CLR_NONE, CLR_NONE, ILD_TRANSPARENT);
    //    FImageList.Draw(Canvas,dX,dY,FIndex);
  end;
end;

procedure TJvImageSquare.SetHiColor(Value: TColor);
begin
  if FHiColor <> Value then
  begin
    FHiColor := Value;
    Repaint;
  end;
end;

procedure TJvImageSquare.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    Repaint;
  end;
end;

procedure TJvImageSquare.SetIndex(Value: Integer);
begin
  if FIndex <> Value then
  begin
    FIndex := Value;
    Repaint;
  end;
end;

procedure TJvImageSquare.SetImageList(Value: TCustomImageList);
begin
  if Images <> nil then
    Images.UnRegisterChanges(FImageChangeLink);
  FImageList := Value;
  if Images <> nil then
    FImageList.RegisterChanges(FImageChangeLink);
  Repaint;
end;

procedure TJvImageSquare.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, y);
  FDown := False;
  if FShowClick then
    PaintFrame;
end;

procedure TJvImageSquare.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FDown := True;
  if FShowClick then
    PaintFrame;
end;

procedure TJvImageSquare.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  if (csDesigning in ComponentState) then Exit;
  if Assigned(FOnEnter) then
    FOnEnter(Self);
  if ColorToRGB(TmpColor) <> ColorToRGB(FHiColor) then
  begin
    TmpColor := FHiColor;
    Repaint;
  end;
end;

procedure TJvImageSquare.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  if (csDesigning in ComponentState) then Exit;
  FDown := False;
  if Assigned(FOnExit) then
    FOnExit(Self);
  if ColorToRGB(TmpColor) <> ColorToRGB(FBackColor) then
  begin
    TmpColor := FBackColor;
    Repaint;
  end;
end;

procedure TJvImageSquare.CMColorChanged(var Message: TMessage);
begin
  inherited;
  FBackColor := Color;
  TmpColor := Color;
  Repaint;
end;

end.

