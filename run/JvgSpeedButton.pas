{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgSpeedButton.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgSpeedButton;

interface

uses
   Windows,
   Messages,
   Classes,
   Controls,
   Graphics,
   JvgTypes,
   JvgCommClasses,
   JvgUtils,
   ExtCtrls,
   buttons,
   JVCLVer,
   stdctrls,
   Forms;
type

   TJvgSpeedButton = class(TSpeedButton)
   private
      FCanvas: TCanvas;
      fMouseEnter: boolean;
      FColor: TColor;
      IsDown: Boolean;
      FControl: TControl;
      FFrame: boolean;
      FCaptionLabel: TLabel;
      FDefaultStyle: boolean;
      FModalResult: TModalResult;
      FFrameColor: TColor;
      FActiveColor: TColor;
      FAboutJVCL: TJVCLAboutInfo;
      procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
      procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
      procedure SetControl(const Value: TControl);
      procedure SetFrame(const Value: boolean);
      procedure SetCaptionLabel(const Value: TLabel);
      procedure SetDefaultStyle(const Value: boolean);
      procedure SetEnabled(const Value: boolean);
      function GetEnabled: boolean;
      procedure SetColor(const Value: TColor);
      procedure SetFrameColor(const Value: TColor);
   protected
      procedure Paint; override;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
         Integer); override;
      procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
         override;
   public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Click; override;
   published
      property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored
         False;
      property Color: TColor read FColor write SetColor;
      property ActiveColor: TColor read FActiveColor write FActiveColor;
      property Control: TControl read FControl write SetControl;
      property CaptionLabel: TLabel read FCaptionLabel write SetCaptionLabel;
      property Frame: boolean read FFrame write SetFrame default true;
      property FrameColor: TColor read FFrameColor write SetFrameColor;
      property DefaultStyle: boolean read FDefaultStyle write SetDefaultStyle;
      property Enabled: boolean read GetEnabled write SetEnabled;
      property ModalResult: TModalResult read FModalResult write FModalResult;
   end;

procedure Register;

implementation
{~~~~~~~~~~~~~~~~~~~~~~~~~}

procedure Register;
begin
end;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//________________________________________________________ Methods _

constructor TJvgSpeedButton.Create(AOwner: TComponent);
begin
   inherited;
   FCanvas := TControlCanvas.Create;
   TControlCanvas(FCanvas).Control := Self; //...i can draw now! :)
   //..defaults
   FColor := IncColor(GetSysColor(COLOR_BTNFACE), 30);
   FActiveColor := IncColor(FColor, 10);
   FFrame := true;
end;

destructor TJvgSpeedButton.Destroy;
begin
   FCanvas.Free;
   inherited;
end;

procedure TJvgSpeedButton.Paint;
var
   R                          : TRect;
   BevelOuter                 : TPanelBevel;
begin
   if DefaultStyle then
   begin
      inherited Paint;
      exit;
   end;
   if SystemColorDepth < 16 then
      FColor := GetNearestColor(Canvas.handle, FColor);

   R := ClientRect;

   if IsDown and fMouseEnter then
      BevelOuter := bvLowered
   else
      BevelOuter := bvRaised;
   if Flat and not IsDown then
      BevelOuter := bvNone;

   if FFrame then
      InflateRect(R, -1, -1);
   dec(R.Right);
   dec(R.Bottom);
   DrawBoxEx(Canvas.handle, R, ALLGLSIDES, bvNone, BevelOuter, false,
      iif(fMouseEnter, ActiveColor, Color), false);

   SetBkMode(Canvas.handle, integer(TRANSPARENT));

   Canvas.Font.Assign(Font);
   if not Enabled then
      Canvas.Font.Color := clGrayText;
   if Assigned(Glyph) then
      inc(R.Left, Glyph.Width);

   if IsDown then
      OffsetRect(R, 1, 1);
   DrawText(Canvas.handle, PChar(Caption), length(Caption), R, DT_SINGLELINE or
      DT_CENTER or DT_VCENTER);

   R := ClientRect;
   Canvas.Brush.Color := 0;
   if FFrame then
   begin
      Canvas.Font.Color := FFrameColor;
      Canvas.FrameRect(R);
   end;

   if Assigned(Glyph) then
      CreateBitmapExt(Canvas.Handle, Glyph, ClientRect, (Width - Glyph.Width -
         Canvas.TextWidth(Caption)) div 2 + integer(IsDown) - 1 - Spacing, 1 +
         (Height - Glyph.Height) div 2 + integer(IsDown),
         fwoNone, fdsDefault,
         true, GetTransparentColor(Glyph, ftcLeftBottomPixel), 0);

end;

procedure TJvgSpeedButton.CMMouseEnter(var Message: TMessage);
begin
   inherited;
   fMouseEnter := true;
   if IsDown or (Color <> ActiveColor) then
      Invalidate;
end;

procedure TJvgSpeedButton.CMMouseLeave(var Message: TMessage);
begin
   inherited;
   fMouseEnter := false;
   if IsDown or (Color <> ActiveColor) then
      Invalidate;
end;

procedure TJvgSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
   Y: Integer);
begin
   inherited;
   IsDown := true;
   Invalidate;
end;

procedure TJvgSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
   X, Y: Integer);
begin
   inherited;
   IsDown := false;
   Invalidate;
end;

procedure TJvgSpeedButton.Click;
var
   Form                       : TCustomForm;
begin
   inherited;
   if ModalResult = mrNone then
      exit;
   Form := GetParentForm(Self);
   if Form <> nil then
      Form.ModalResult := ModalResult;
end;

procedure TJvgSpeedButton.SetControl(const Value: TControl);
begin
   FControl := Value;
end;

procedure TJvgSpeedButton.SetFrame(const Value: boolean);
begin
   FFrame := Value;
   Invalidate;
end;

procedure TJvgSpeedButton.SetCaptionLabel(const Value: TLabel);
begin
   FCaptionLabel := Value;
   Invalidate;
end;

procedure TJvgSpeedButton.SetDefaultStyle(const Value: boolean);
begin
   FDefaultStyle := Value;
   Invalidate;
end;

procedure TJvgSpeedButton.SetEnabled(const Value: boolean);
begin
   inherited Enabled := Value;
   if Assigned(FControl) then
      FControl.Enabled := Value
end;

function TJvgSpeedButton.GetEnabled: boolean;
begin
   Result := inherited Enabled;
end;

procedure TJvgSpeedButton.SetColor(const Value: TColor);
begin
   FColor := Value;
   Invalidate;
end;

procedure TJvgSpeedButton.SetFrameColor(const Value: TColor);
begin
   FFrameColor := Value;
   Invalidate;
end;

end.

