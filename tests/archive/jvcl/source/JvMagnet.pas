{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMagnet.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvMagnet;

{
 TJvFormMagnet
}



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, JvComponent;

type
  TJvFormMagnet = class(TJvComponent)
  private
    FForm: TForm;
    FOldWndProc: Pointer;
    FActive: Boolean;
    FScreen: Boolean;
    FGlue: Boolean;
    FArea: Cardinal;
    FFormMagnet: Boolean;
    FLastRightDock: TDateTime;
    FLastLeftDock: TDateTime;
    FLastTopDock: TDateTime;
    FLastBottomDock: TDateTime;
    procedure NewWndProc(var Msg: TMessage);
    procedure MagnetScreen(OldRect: TRect; var FormRect: TRect; ScreenRect: TRect);
    procedure GlueForms(var FormRect: TRect);
    procedure MagnetToMain(OldRect: TRect; var FormRect: TRect; MainRect: TRect);
  public
    procedure MoveTo(var SrcRect,Rect: TRect);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read FActive write FActive default False;
    property ScreenMagnet: Boolean read FScreen write FScreen default True;
    property Area: Cardinal read FArea write FArea default 15;
    property FormGlue: Boolean read FGlue write FGlue default True;
    property MainFormMagnet: Boolean read FFormMagnet write FFormMagnet default False;
  end;

implementation

{****************************************************}

constructor TJvFormMagnet.Create(AOwner: TComponent);
var
  ptr: Pointer;
begin
  inherited;
  FActive := False;
  FScreen := True;
  FArea := 15;
  FGlue := True;
  FFormMagnet := False;

  FLastRightDock := 0.0;
  FLastLeftDock := 0.0;
  FLastTopDock := 0.0;
  FLastBottomDock := 0.0;

  FForm := TForm(GetParentForm(TControl(AOwner)));
  if not (csDesigning in ComponentState) then
  begin
    FOldWndProc := Pointer(GetWindowLong(FForm.Handle, GWL_WNDPROC));
    ptr := {$IFDEF COMPILER6_UP}Classes.{$ENDIF}MakeObjectInstance(NewWndProc);
    SetWindowLong(FForm.Handle, GWL_WNDPROC, Longint(ptr));
  end;
end;

{****************************************************}

destructor TJvFormMagnet.Destroy;
begin
  if not (csDesigning in ComponentState) then
    if not (csDestroying in FForm.ComponentState) then
      SetWindowLong(FForm.Handle, GWL_WNDPROC, LongInt(FOldWndProc));
  inherited;
end;

{****************************************************}

procedure TJvFormMagnet.MagnetScreen(OldRect: TRect; var FormRect: TRect; ScreenRect: TRect);
var
  FormWidth, FormHeight: Integer;

  function MovingToLeft: Boolean;
  begin
    Result := OldRect.Left > FormRect.Left;
  end;

  function MovingToRight: Boolean;
  begin
    Result := OldRect.Left < FormRect.Left;
  end;

  function MovingToTop: Boolean;
  begin
    Result := OldRect.Top > FormRect.Top;
  end;

  function MovingToBottom: Boolean;
  begin
    Result := OldRect.Top < FormRect.Top;
  end;

  function OkayForAll(var Value: TDateTime): Boolean;
  begin
    if abs(Value - Now) > EncodeTime(0, 0, 0, 250) then
      Result := True
    else
      Result := False;
  end;

  function OkayForRight: Boolean;
  begin
    Result := OkayForAll(FLastRightDock);
  end;

  function OkayForLeft: Boolean;
  begin
    Result := OkayForAll(FLastLeftDock);
  end;

  function OkayForTop: Boolean;
  begin
    Result := OkayForAll(FLastTopDock);
  end;

  function OkayForBottom: Boolean;
  begin
    Result := OkayForAll(FLastBottomDock);
  end;

  procedure DockOnLeft;
  begin
    FormRect.Left := ScreenRect.Left;
    FormRect.Right := FormRect.Left + FormWidth;
    FLastLeftDock := Now;
  end;

  procedure UndockOnLeftOutside;
  begin
    FormRect.Left := ScreenRect.Left - Integer(FArea);
    FormRect.Right := FormRect.Left + FormWidth;
    FLastLeftDock := Now;
  end;

  procedure UndockOnLeftInside;
  begin
    FormRect.Left := ScreenRect.Left + Integer(FArea);
    FormRect.Right := FormRect.Left + FormWidth;
    FLastLeftDock := Now;
  end;

  procedure DockOnRight;
  begin
    FormRect.Left := ScreenRect.Right - FormWidth;
    FormRect.Right := ScreenRect.Right;
    FLastRightDock := Now;
  end;

  procedure UndockOnRightOutside;
  begin
    FormRect.Left := ScreenRect.Right - FormWidth + Integer(FArea);
    FormRect.Right := ScreenRect.Right + Integer(FArea);
    FLastRightDock := Now;
  end;

  procedure UndockOnRightInside;
  begin
    FormRect.Left := ScreenRect.Right - FormWidth - Integer(FArea);
    FormRect.Right := ScreenRect.Right - Integer(FArea);
    FLastRightDock := Now;
  end;

  procedure DockOnTop;
  begin
    FormRect.Top := ScreenRect.Top;
    FormRect.Bottom := FormRect.Top + FormHeight;
    FLastTopDock := Now;
  end;

  procedure UndockOnTopOutside;
  begin
    FormRect.Top := ScreenRect.Top - Integer(FArea);
    FormRect.Bottom := FormRect.Top + FormHeight;
    FLastTopDock := Now;
  end;

  procedure UndockOnTopInside;
  begin
    FormRect.Top := ScreenRect.Top + Integer(FArea);
    FormRect.Bottom := FormRect.Top + FormHeight;
    FLastTopDock := Now;
  end;

  procedure DockOnBottom;
  begin
    FormRect.Top := ScreenRect.Bottom - FormHeight;
    FormRect.Bottom := ScreenRect.Bottom;
    FLastBottomDock := Now;
  end;

  procedure UndockOnBottomInside;
  begin
    FormRect.Top := ScreenRect.Bottom - FormHeight - Integer(FArea);
    FormRect.Bottom := ScreenRect.Bottom - Integer(FArea);
    FLastBottomDock := Now;
  end;

  procedure UndockOnBottomOutside;
  begin
    FormRect.Top := ScreenRect.Bottom - FormHeight + Integer(FArea);
    FormRect.Bottom := ScreenRect.Bottom + Integer(FArea);
    FLastBottomDock := Now;
  end;

begin
  FormWidth := FormRect.Right - FormRect.Left;
  FormHeight := FormRect.Bottom - FormRect.Top;

  //Magnet/UnMagnet Lleft, Magnet/UnMagnet Right
  if MovingToLeft then
    if OkayForLeft then
    begin
      if ((FormRect.Left - ScreenRect.Left) in [2..FArea]) or
        (Abs(FormRect.Left - ScreenRect.Left) = 1) then
        DockOnLeft
      else if Abs(FormRect.Left - ScreenRect.Left) in [2..FArea] then
        UndockOnLeftOutside
      else if (ScreenRect.Right - FormRect.Right) in [2..FArea] then
        UndockOnRightInside
      else if Abs(ScreenRect.Right - FormRect.Right) in [1..FArea] then
        DockOnRight;
    end
    else if Abs(FormRect.Left - ScreenRect.Left) < Integer(FArea) then
      DockOnLeft
    else if Abs(ScreenRect.Right - FormRect.Right) < Integer(FArea) then
      DockOnRight;

  //Magnet/UnMagnet Lleft, Magnet/UnMagnet Right
  if MovingToRight then
    if OkayForRight then
    begin
      if ((ScreenRect.Right - FormRect.Right) in [2..FArea]) or
        (Abs(ScreenRect.Right - FormRect.Right) = 1) then
        DockOnRight
      else if Abs(ScreenRect.Right - FormRect.Right) in [2..FArea] then
        UndockOnRightOutside
      else if (ScreenRect.Left - FormRect.Left) in [2..FArea] then
        DockOnLeft
      else if Abs(ScreenRect.Left - FormRect.Left) in [1..FArea] then
        UndockOnLeftInside;
    end
    else if Abs(ScreenRect.Right - FormRect.Right) < Integer(FArea) then
      DockOnRight
    else if Abs(ScreenRect.Left - FormRect.Left) < Integer(FArea) then
      DockOnLeft;

  //Magnet/UnMagnet Bottom, Magnet/UnMagnet Top
  if MovingToTop then
    if OkayForTop then
    begin
      if ((FormRect.Top - ScreenRect.Top) in [2..FArea]) or
        (Abs(FormRect.Top - ScreenRect.Top) = 1) then
        DockOnTop
      else if Abs(FormRect.Top - ScreenRect.Top) in [2..FArea] then
        UndockOnTopOutside
      else if (ScreenRect.Bottom - FormRect.Bottom) in [2..FArea] then
        UndockOnBottomInside
      else if Abs(ScreenRect.Bottom - FormRect.Bottom) in [1..FArea] then
        DockOnBottom;
    end
    else if Abs(FormRect.Top - ScreenRect.Top) < Integer(FArea) then
      DockOnTop
    else if Abs(ScreenRect.Bottom - FormRect.Bottom) < Integer(FArea) then
      DockOnBottom;

  //Magnet/UnMagnet Bottom, Magnet/UnMagnet Top
  if MovingToBottom then
    if OkayForBottom then
    begin
      if (FormRect.Top - ScreenRect.Top) in [2..FArea] then
        UndockOnTopInside
      else if Abs(FormRect.Top - ScreenRect.Top) < Integer(FArea) then
        DockOnTop
      else if (ScreenRect.Bottom - FormRect.Bottom) in [2..FArea] then
        DockOnBottom
      else if Abs(ScreenRect.Bottom - FormRect.Bottom) in [1..FArea] then
        UndockOnBottomOutside;
    end
    else if Abs(FormRect.Top - ScreenRect.Top) < Integer(FArea) then
      DockOnTop
    else if Abs(ScreenRect.Bottom - FormRect.Bottom) < Integer(FArea) then
      UndockOnBottomOutside;
end;

{****************************************************}

procedure TJvFormMagnet.GlueForms(var FormRect: TRect);
var
  i: Integer;
begin
  for i := 0 to Application.ComponentCount - 1 do
    if Application.Components[i] is TForm then
      with Application.Components[i] as TForm do
        if (Left = FForm.Left + FForm.Width) or
          (Top = FForm.Top + FForm.Height) or
          (Left + Width = FForm.Left) or
          (Top + Height = FForm.Top) then
        begin
          Left := Left + (FormRect.Left - FForm.Left);
          Top := Top + (FormRect.Top - FForm.Top);
        end;
end;

{****************************************************}

procedure TJvFormMagnet.MagnetToMain(OldRect: TRect; var FormRect: TRect; MainRect: TRect);
var
  FormWidth, FormHeight: Integer;

  function OkayForAll(var Value: TDateTime): Boolean;
  begin
    Result := Abs(Value - Now) > EncodeTime(0, 0, 0, 250);
  end;

  function OkayForRight: Boolean;
  begin
    Result := OkayForAll(FLastRightDock);
  end;

  function OkayForTop: Boolean;
  begin
    Result := OkayForAll(FLastTopDock);
  end;

  function MovingToLeft: Boolean;
  begin
    Result := OldRect.Left > FormRect.Left;
  end;

  function MovingToRight: Boolean;
  begin
    Result := OldRect.Left < FormRect.Left;
  end;

  function MovingToTop: Boolean;
  begin
    Result := OldRect.Top > FormRect.Top;
  end;

  function MovingToBottom: Boolean;
  begin
    Result := OldRect.Top < FormRect.Top;
  end;

  function InWidth: Boolean;
  begin
    Result := ((FormRect.Left > MainRect.Left) and (FormRect.Left < MainRect.Right)) or
      ((FormRect.Left < MainRect.Left) and (FormRect.Right > MainRect.Left));
  end;

  function InHeight: Boolean;
  begin
    Result := ((FormRect.Top > MainRect.Top) and (FormRect.Top < MainRect.Bottom)) or
      ((FormRect.Top < MainRect.Top) and (FormRect.Bottom > MainRect.Top));
  end;

  procedure DockOnBottom;
  begin
    FormRect.Top := MainRect.Bottom;
    FormRect.Bottom := FormRect.Top + FormHeight;
    FLastTopDock := Now;
  end;

  procedure UndockOnBottomInside;
  begin
    FormRect.Top := MainRect.Bottom - Integer(FArea);
    FormRect.Bottom := FormRect.Top + FormHeight;
    FLastTopDock := Now;
  end;

  procedure UndockOnBottomOutside;
  begin
    FormRect.Top := MainRect.Bottom + Integer(FArea);
    FormRect.Bottom := FormRect.Top + FormHeight;
    FLastTopDock := Now;
  end;

  procedure DockOnTop;
  begin
    FormRect.Top := MainRect.Top - FormHeight;
    FormRect.Bottom := MainRect.Top;
    FLastTopDock := Now;
  end;

  procedure UndockOnTopOutside;
  begin
    FormRect.Top := MainRect.Top - FormHeight - Integer(FArea);
    FormRect.Bottom := MainRect.Top - Integer(FArea);
    FLastTopDock := Now;
  end;

  procedure UndockOnTopInside;
  begin
    FormRect.Top := MainRect.Top - FormHeight + Integer(FArea);
    FormRect.Bottom := MainRect.Top + Integer(FArea);
    FLastTopDock := Now;
  end;

  procedure DockOnRight;
  begin
    FormRect.Left := MainRect.Right;
    FormRect.Right := FormRect.Left + FormWidth;
    FLastRightDock := Now;
  end;

  procedure UndockOnRightInside;
  begin
    FormRect.Left := MainRect.Right - Integer(FArea);
    FormRect.Right := FormRect.Left + FormWidth;
    FLastRightDock := Now;
  end;

  procedure UndockOnRightOutside;
  begin
    FormRect.Left := MainRect.Right + Integer(FArea);
    FormRect.Right := FormRect.Left + FormWidth;
    FLastRightDock := Now;
  end;

  procedure DockOnLeft;
  begin
    FormRect.Left := MainRect.Left - FormWidth;
    FormRect.Right := MainRect.Left;
    FLastRightDock := Now;
  end;

  procedure UndockOnLeftInside;
  begin
    FormRect.Left := MainRect.Left - FormWidth + Integer(FArea);
    FormRect.Right := MainRect.Left + Integer(FArea);
    FLastRightDock := Now;
  end;

  procedure UndockOnLeftOutside;
  begin
    FormRect.Left := MainRect.Left - FormWidth - Integer(FArea);
    FormRect.Right := MainRect.Left - Integer(FArea);
    FLastRightDock := Now;
  end;

begin
  FormWidth := FormRect.Right - FormRect.Left;
  FormHeight := FormRect.Bottom - FormRect.Top;

  //Magnet/UnMagnet Bottom, Magnet/UnMagnet Top
  if MovingToTop and InWidth then
    if OkayForTop then
    begin
      if (FormRect.Top - MainRect.Bottom) in [2..FArea] then
        DockOnBottom
      else if - (FormRect.Top - MainRect.Bottom) in [2..FArea] then
        UndockOnBottomInside
      else if (FormRect.Bottom - MainRect.Top) in [2..FArea] then
        DockOnTop
      else if - (FormRect.Bottom - MainRect.Top) in [2..FArea] then
        UndockOnTopOutside;
    end
    else if Abs(FormRect.Top - MainRect.Bottom) < Integer(FArea) then
      DockOnBottom
    else if Abs(FormRect.Bottom - MainRect.Top) < Integer(FArea) then
      DockOnTop;

  if MovingToBottom and InWidth then
    if OkayForTop then
    begin
      if (FormRect.Top - MainRect.Bottom) in [2..FArea] then
        UndockOnBottomOutside
      else if - (FormRect.Top - MainRect.Bottom) in [2..FArea] then
        DockOnBottom
      else if (FormRect.Bottom - MainRect.Top) in [1..FArea] then
        DockOnTop
      else if Abs(FormRect.Bottom - MainRect.Top) in [2..FArea] then
        UndockOnTopInside;
    end
    else if Abs(FormRect.Top - MainRect.Bottom) < Integer(FArea) then
      DockOnBottom
    else if (FormRect.Bottom - MainRect.Top) < Integer(FArea) then
      DockOnTop;

  if MovingToLeft and InHeight then
    if OkayForRight then
    begin
      if (FormRect.Left - MainRect.Right) in [2..FArea] then
        DockOnRight
      else if Abs(FormRect.Left - MainRect.Right) in [2..FArea] then
        UndockOnRightInside
      else if (FormRect.Right - MainRect.Left) in [2..FArea] then
        DockOnLeft
      else if Abs(FormRect.Right - MainRect.Left) in [2..FArea] then
        UndockOnLeftOutside;
    end
    else if Abs(FormRect.Left - MainRect.Right) < Integer(FArea) then
      DockOnRight
    else if Abs(FormRect.Right - MainRect.Left) < Integer(FArea) then
      DockOnLeft;

  if MovingToRight and InHeight then
    if OkayForRight then
    begin
      if (MainRect.Left - FormRect.Right) in [2..FArea] then
        DockOnLeft
      else if Abs(MainRect.Left - FormRect.Right) in [2..FArea] then
        UndockOnLeftInside
      else if (MainRect.Right - FormRect.Left) in [2..FArea] then
        DockOnRight
      else if Abs(MainRect.Right - FormRect.Left) in [2..FArea] then
        UndockOnRightOutside;
    end
    else if Abs(MainRect.Left - FormRect.Right) < Integer(FArea) then
      DockOnLeft
    else if Abs(MainRect.Right - FormRect.Left) < Integer(FArea) then
      DockOnRight
end;

{****************************************************}

procedure TJvFormMagnet.NewWndProc(var Msg: TMessage);
var
  r,r3: TRect;
begin
  with Msg do
  begin
    if FActive then
    begin
      case Msg of
        WM_MOVING:
          begin
            r := PRect(lParam)^;
            r3.Left := FForm.Left;
            r3.Top := FForm.Top;
            r3.Right := r3.Left + FForm.Width;
            r3.Bottom := r3.Top + FForm.Height;
            MoveTo(r3,r);
            PRect(lparam)^ := r;
          end;
      end;
    end;
    Result := CallWindowProc(FOldWndProc, FForm.Handle, Msg, WParam, LParam);
  end;
end;

{****************************************************}

procedure TJvFormMagnet.MoveTo(var SrcRect,Rect: TRect);
var
  r2, r3: TRect;
begin
  r3 := SrcRect;

  //Move to an extremity of the desktop ?
  if FScreen then
  begin
    SystemParametersInfo(SPI_GETWORKAREA, 0, @r2, 0);
    MagnetScreen(r3, Rect, r2);
  end;

  //Move another form too ?
  if FGlue then
    GlueForms(Rect);

  //Magnet to main form ?
  if FFormMagnet and (Application.MainForm <> nil) then
  begin
    r2.Left := Application.MainForm.Left;
    r2.Top := Application.MainForm.Top;
    r2.Right := Application.MainForm.Left + Application.MainForm.Width;
    r2.Bottom := Application.MainForm.Top + Application.MainForm.Height;
    MagnetToMain(r3, Rect, r2);
  end;
end;

end.
