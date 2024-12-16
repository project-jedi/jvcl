{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTransparentForm.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].
   Andreas Hausladen [Andreas dott Hausladen att gmx dott net]  (complete rewrite)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvFormTransparent;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls,
  JvComponentBase;

type
  TJvTransparentFormMode = (
    tfmWindowRegion,             // Use Mask as the window region
    tfmWindowRegionAlphaChannel, // Use the alpha channel of Mask (32bit with alpha channel) for the window region
    tfmLayeredWindow             // Use Mask (32bit with alpha channel) for the layered window (Windows 2000 or newer)
  );

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvTransparentForm = class(TJvComponent)
  private
    FMask: TBitmap;
    FComponentOwner: TCustomForm;
    FAutoSize: Boolean;
    FActive: Boolean;
    FMode: TJvTransparentFormMode;
    FMovableForm: Boolean;
    FOrgWndProc: TWndMethod;
    FControlForm: TForm;
    FLayeredTransparentControlColor: TColor;
    FLayeredAlphaValue: Integer;
    FMaskFromImage: TImage;
    procedure SetActive(Value: Boolean);
    procedure SetMask(Value: TBitmap);
    procedure SetMode(const Value: TJvTransparentFormMode);
    procedure SetAutoSize(Value: Boolean);
    procedure DisableTransparency;
    procedure UpdateTransparency;
    procedure SetLayeredTransparentControlColor(const Value: TColor);
    procedure ReparentChildControls(OldParent, NewParent: TWinControl);
    procedure SetLayeredAlphaValue(Value: Integer);
    procedure SetMaskFromImage(const Value: TImage);
    procedure UpdateMaskFromImage;
  protected
    procedure Loaded; override;
    procedure WndProc(var Msg: TMessage);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update;
  published
    { Active enables/disables the transparent top level form }
    property Active: Boolean read FActive write SetActive default False;
    { Mask specifies (depending on Mode) the region or the 32bit alpha channel background picture }
    property Mask: TBitmap read FMask write SetMask;
    { With MaskFromImage you can automatically use the image of a TImage component for the mask.
      This is especially helpfull if you want to use a PNG image with tfmLayeredWindow. Setting
      the property automatically sets the image's Visible property to False in the IDE (design time). }
    property MaskFromImage: TImage read FMaskFromImage write SetMaskFromImage;
    { If AutoSize is True the top level window will be resized to fit the Mask }
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    { Mode specifies how Mask should be interpreted }
    property Mode: TJvTransparentFormMode read FMode write SetMode default tfmWindowRegion;
    { If MoveableForm is True the user can move the form by pressing anywhere on the form }
    property MovableForm: Boolean read FMovableForm write FMovableForm default False;
    { Mode=tfmLayeredWindow: LayeredTransparentControlColor controls the transparent color for
      child controls. You should keep the color as near as possible to the Mask bitmap. }
    property LayeredTransparentControlColor: TColor read FLayeredTransparentControlColor write SetLayeredTransparentControlColor default $FEFEFE;
    { Mode=tfmLayeredWindow: LayeredAlphaValue controls the form's general semi-transparency. }
    property LayeredAlphaValue: Integer read FLayeredAlphaValue write SetLayeredAlphaValue default 255;
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
  JclGraphics;

type
  TOpenCustomForm = class(TCustomForm);

  { UpdateLayeredWindow doesn't allow controls to paint themself. So we use a trick here.
    All controls are moved into a TJvControlForm form that is placed above the form and
    that moves and resizes with the semi-transparent form. }
  TJvControlForm = class(TForm)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMClose(var Message: TWMClose); message WM_CLOSE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMErasebkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  end;

{ TControlForm }

procedure TJvControlForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := WS_POPUP;
  { Stay on top of the  semi-transparent form. }
  Params.WndParent := (Owner as TJvTransparentForm).FComponentOwner.Handle;
end;

procedure TJvControlForm.WMClose(var Message: TWMClose);
begin
  { Redirect any close command to the semi-transparent form. }
  Message.Result := (Owner as TJvTransparentForm).FComponentOwner.Perform(Message.Msg, 0, 0);
end;

procedure TJvControlForm.WMErasebkgnd(var Message: TWMEraseBkgnd);
var
  I: Integer;
  R: TRect;
  MaskDC: HDC;
  Control: TControl;
begin
  { Fill with transparent color }
  FillRect(Message.DC, Rect(0, 0, Width, Height), Brush.Handle);

  { Replace the transparent color with the actual Mask content. This lets fonts and
    other transparent controls that use aliasing look much better. }
  MaskDC := TJvTransparentForm(Owner).FMask.Canvas.Handle;
  for I := 0 to ControlCount - 1 do
  begin
    Control := Controls[I];
    if Control.Visible then
    begin
      R := Control.BoundsRect;
      BitBlt(Message.DC, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, MaskDC, R.Left, R.Top, SRCCOPY)
    end;
  end;
  Message.Result := 1;
end;

procedure TJvControlForm.WMNCHitTest(var Message: TWMNCHitTest);
begin
  { It must feel like this helper form doesn't exist. }
  Message.Result := HTTRANSPARENT;
end;

{ TJvTransparentForm }

constructor TJvTransparentForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComponentOwner := GetParentForm(TControl(AOwner));
  if (FComponentOwner <> nil) and not (csDesigning in ComponentState) then
  begin
    FOrgWndProc := FComponentOwner.WindowProc;
    FComponentOwner.WindowProc := WndProc;
  end;
  FLayeredTransparentControlColor := $FEFEFE;
  FLayeredAlphaValue := 255;
  FMask := TBitmap.Create;
  FMask.PixelFormat := pf32bit;
end;

destructor TJvTransparentForm.Destroy;
begin
  if FComponentOwner <> nil then
  begin
    if not (csDestroying in FComponentOwner.ComponentState) then
      Active := False;
    if not (csDesigning in ComponentState) then
      FComponentOwner.WindowProc := FOrgWndProc;
    FComponentOwner := nil;
  end;
  SetMaskFromImage(nil);
  FControlForm.Free;
  FMask.Free;
  inherited Destroy;
end;

procedure TJvTransparentForm.ReparentChildControls(OldParent, NewParent: TWinControl);
var
  I: Integer;
  List: TList;
begin
  List := TList.Create;
  try
    { Reparent the controls but keep the tab order. }
    for I := 0 to OldParent.ControlCount - 1 do
      List.Add(OldParent.Controls[I]);
    for I := 0 to List.Count - 1 do
    begin
      TControl(List[I]).Parent := NewParent;
      if (TControl(List[I]) is TCustomLabel) and not TLabel(List[I]).Transparent then
        TLabel(List[I]).Transparent := True;
    end;
  finally
    List.Free;
  end;
end;

procedure TJvTransparentForm.Update;
begin
  UpdateMaskFromImage;
  UpdateTransparency;
end;

procedure TJvTransparentForm.UpdateMaskFromImage;
begin
  if ([csLoading, csDesigning] * ComponentState = []) and (FMaskFromImage <> nil) and
     (FMaskFromImage.Picture.Graphic <> nil) then
    FMask.Assign(FMaskFromImage.Picture.Graphic);
end;

procedure TJvTransparentForm.DisableTransparency;
var
  Params: TCreateParams;
begin
  if not Active or ([csDesigning, csLoading] * ComponentState <> []) or
     (FComponentOwner = nil) or not FComponentOwner.HandleAllocated then
    Exit;

  { Enable caption }
  FillChar(Params, SizeOf(Params), 0);
  TOpenCustomForm(FComponentOwner).CreateParams(Params);
  SetWindowLong(FComponentOwner.Handle, GWL_STYLE,
    GetWindowLong(FComponentOwner.Handle, GWL_STYLE)
    or (Integer(Params.Style) and not (WS_VISIBLE or WS_MAXIMIZE or WS_DISABLED)));
  case Mode of
    tfmWindowRegion, tfmWindowRegionAlphaChannel:
      begin
        { Remove region }
        SetWindowRgn(FComponentOwner.Handle, 0, True);
      end;

    tfmLayeredWindow:
      begin
        { Disable layered window }
        SetWindowLong(FComponentOwner.Handle, GWL_EXSTYLE,
          GetWindowLong(FComponentOwner.Handle, GWL_EXSTYLE) and not WS_EX_LAYERED);
        if FControlForm <> nil then
          ReparentChildControls(FControlForm, FComponentOwner);
        FreeAndNil(FControlForm);
        FComponentOwner.Refresh;
      end;
  end;
end;

procedure TJvTransparentForm.UpdateTransparency;
const
  BorderStyles = WS_SYSMENU or WS_MINIMIZEBOX or WS_MAXIMIZEBOX or
                 WS_CAPTION or WS_BORDER or WS_THICKFRAME or WS_DLGFRAME;
var
  Region: HRGN;
  BlendFunc: TBlendFunction;
  Pt: TPoint;
  Sz: TSize;
begin
  if not Active or ([csDesigning, csLoading] * ComponentState <> []) or (FComponentOwner = nil) then
    Exit;

  { Remove caption }
  SetWindowLong(FComponentOwner.Handle, GWL_STYLE,
    GetWindowLong(FComponentOwner.Handle, GWL_STYLE) and not BorderStyles);
  case Mode of
    tfmWindowRegion, tfmWindowRegionAlphaChannel:
      begin
        Region := 0;
        if not FMask.Empty then
        begin
          if Mode = tfmWindowRegionAlphaChannel then
            Region := CreateRegionFromBitmap(FMask, 0, rmExclude, True)
          else
            Region := CreateRegionFromBitmap(FMask, FMask.Canvas.Pixels[0, 0], rmExclude, False);
        end;
        if SetWindowRgn(FComponentOwner.Handle, Region, True) = 0 then
          if Region <> 0 then
            DeleteObject(Region);
        { Region is now no longer valid }
      end;

    tfmLayeredWindow:
      begin
        if not FMask.Empty then
        begin
          if FControlForm = nil then
          begin
            FControlForm := TJvControlForm.CreateNew(Self);
            FControlForm.Position := poDesigned;
            FControlForm.BorderStyle := bsNone;
          end;
          FControlForm.Color := LayeredTransparentControlColor;
          FControlForm.TransparentColorValue := FControlForm.Color;
          FControlForm.TransparentColor := True;

          FControlForm.BoundsRect := FComponentOwner.BoundsRect;
          ReparentChildControls(FComponentOwner, FControlForm);

          SetWindowLong(FComponentOwner.Handle, GWL_EXSTYLE,
            GetWindowLong(FComponentOwner.Handle, GWL_EXSTYLE) or WS_EX_LAYERED);

          FillChar(BlendFunc, SizeOf(BlendFunc), 0);
          BlendFunc.BlendOp := AC_SRC_OVER;
          BlendFunc.BlendFlags := 0;
          BlendFunc.AlphaFormat := AC_SRC_ALPHA;
          BlendFunc.SourceConstantAlpha := LayeredAlphaValue;

          Pt := Point(0, 0);
          Sz.cx := FMask.Width;
          Sz.cy := FMask.Height;
          UpdateLayeredWindow(FComponentOwner.Handle, 0, nil, @Sz,
            FMask.Canvas.Handle, @Pt, 0, @BlendFunc, ULW_ALPHA);

          if FComponentOwner.Visible then
            FControlForm.Show;
        end;
      end;
  end;

  if AutoSize and not FMask.Empty then
  begin
    FComponentOwner.Width := FMask.Width;
    FComponentOwner.Height := FMask.Height;
  end;
end;

procedure TJvTransparentForm.WndProc(var Msg: TMessage);
begin
  if Active then
  begin
    case Msg.Msg of
      WM_NCHITTEST:
        if MovableForm then
        begin
          Msg.Result := HTCAPTION;
          Exit;
        end;
    end;
  end;

  if Assigned(FOrgWndProc) then
    FOrgWndProc(Msg);

  if Msg.Msg = CM_RELEASE then // the form can be evil
    Exit;

  if Active then
  begin
    case Msg.Msg of
      WM_MOVE, WM_MOVING, WM_SIZE, WM_SIZING:
        if (FControlForm <> nil) and (FComponentOwner <> nil) then
          FControlForm.BoundsRect := FComponentOwner.BoundsRect;

      WM_SHOWWINDOW:
        begin
          if TWMShowWindow(Msg).Show then
          begin
            if Mode = tfmLayeredWindow then
              UpdateTransparency;
          end
          else
          if FControlForm <> nil then
            FControlForm.Hide;
        end;

      WM_SETFOCUS:
        if (FControlForm <> nil) and FControlForm.Visible then
          FControlForm.Perform(Msg.Msg, Msg.WParam, Msg.LParam);
    end;
  end;
end;

procedure TJvTransparentForm.Loaded;
begin
  inherited Loaded;
  UpdateMaskFromImage;
  UpdateTransparency;
end;

procedure TJvTransparentForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FComponentOwner then
      FComponentOwner := nil
    else if AComponent = FMaskFromImage then
      MaskFromImage := nil;
  end;
end;

procedure TJvTransparentForm.SetMask(Value: TBitmap);
begin
  if Value <> FMask then
  begin
    FMask.Assign(Value);
    UpdateTransparency;
  end;
end;

procedure TJvTransparentForm.SetMaskFromImage(const Value: TImage);
begin
  if Value <> FMaskFromImage then
  begin
    if FMaskFromImage <> nil then
      FMaskFromImage.RemoveFreeNotification(Self);
    FMaskFromImage := Value;
    if FMaskFromImage <> nil then
      FMaskFromImage.FreeNotification(Self);

    if (csDesigning in ComponentState) and (FMaskFromImage <> nil) then
      FMaskFromImage.Visible := False;

    UpdateMaskFromImage;
  end;
end;

procedure TJvTransparentForm.SetMode(const Value: TJvTransparentFormMode);
begin
  if Value <> FMode then
  begin
    if Active then
      DisableTransparency;
    FMode := Value;
    if Active then
      UpdateTransparency;
  end;
end;

procedure TJvTransparentForm.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    if FActive then
      DisableTransparency;
    FActive := Value;
    if FActive then
      UpdateTransparency;
  end;
end;

procedure TJvTransparentForm.SetAutoSize(Value: Boolean);
begin
  FAutoSize := Value;
  if Value and Active and not (csLoading in ComponentState) and not FMask.Empty then
  begin
    FComponentOwner.Width := FMask.Width;
    FComponentOwner.Height := FMask.Height;
  end;
end;

procedure TJvTransparentForm.SetLayeredAlphaValue(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value > 255 then
    Value := 255;

  if Value <> FLayeredAlphaValue then
  begin
    FLayeredAlphaValue := Value;
    if Mode = tfmLayeredWindow then
      UpdateTransparency;
  end;
end;

procedure TJvTransparentForm.SetLayeredTransparentControlColor(const Value: TColor);
begin
  if Value <> FLayeredTransparentControlColor then
  begin
    FLayeredTransparentControlColor := Value;
    if FControlForm <> nil then
    begin
      FControlForm.Color := FLayeredTransparentControlColor;
      FControlForm.TransparentColorValue := FControlForm.Color;
    end;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
