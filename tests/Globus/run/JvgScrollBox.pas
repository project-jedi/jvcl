{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgScrollBox.PAS, released on 2003-01-15.

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

unit JvgScrollBox;

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
   Forms,
   JVCLver,
   OleCtnrs,
   ExtCtrls,
   SysUtils;
type
   TOnEraseBkgndEvent = procedure(Sender: TObject; DC: HDC) of object;

   TJvgScrollBox = class(TScrollBox)
   private
      FBack: TBitmap;
      Buffer: TBitmap;
      FOnEraseBkgndEvent: TOnEraseBkgndEvent;
      FAboutJVCL: TJVCLAboutInfo;
      procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
      function GetBack: TBitmap;
      procedure SetBack(Value: TBitmap);
      procedure SetOnEraseBkgndEvent(const Value: TOnEraseBkgndEvent);
   public
      BufferedDraw: boolean;
      procedure ApplyBuffer(DC: HDC);
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
   published
      property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored
         False;
      property Background: TBitmap read GetBack write SetBack;
      property OnEraseBkgndEvent: TOnEraseBkgndEvent read FOnEraseBkgndEvent
         write SetOnEraseBkgndEvent;
   end;

procedure Register;
implementation

{~~~~~~~~~~~~~~~~~~~~~~~~~}

procedure Register;
begin
end;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//________________________________________________________ Methods _

constructor TJvgScrollBox.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   {  FBack := TBitmap.Create;
     FBack.Width := 8; FBack.Height := 8;
     FBack.Canvas.Brush.Color := clWhite;//clWindow;
     FBack.Canvas.FillRect( Rect(0,0,8,8) );
     FBack.Canvas.Pixels[7,7] := 0;}
end;

destructor TJvgScrollBox.Destroy;
begin
   if Assigned(FBack) then
      FBack.Free;
   if Assigned(Buffer) then
      Buffer.Free;
   inherited;
end;

procedure TJvgScrollBox.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
var
   DC                         : HDC;
   r                          : TRect;
   IHeight, IWidth, SavedIHeight, x_, y_, XOffset, YOffset, SavedYOffset:
      integer;
   Canvas                     : TCanvas;
begin
   Canvas := nil;

   if BufferedDraw and (Buffer = nil) then
      Buffer := TBitmap.Create;

   if assigned(Buffer) then
   begin
      Buffer.Width := Width;
      Buffer.Height := Height;
   end;

   if BufferedDraw then
      DC := Buffer.Canvas.Handle
   else
      DC := Msg.DC;

   try

      if not Assigned(FBack) then
         exit;

      if FBack.Width <= 8 then
         with Canvas do
         begin
            Canvas := TCanvas.Create;
            Handle := Msg.DC;
            //    Pen.Color := clWindow;
            //    Brush.Color := clWindow;
            //    Brush.Style := bsCross;
            Brush.Bitmap := FBack;
            FillRect(ClientRect);
            Handle := 0;
            Msg.Result := 1;
         end
      else
      begin
         //  Sendmessage(self.Handle, WM_SETREDRAW, 0, 0);
         //    BitBlt( Msg.DC, x_, y_, 100, 100, FBack.canvas.Handle, 0, 0, SRCCOPY);
         r := ClientRect;
         x_ := r.left;
         y_ := r.top;
         IHeight := FBack.Height;
         IWidth := FBack.Width;
         SavedIHeight := IHeight;

         XOffset := HorzScrollBar.Position - trunc(HorzScrollBar.Position /
            IWidth) * IWidth;
         YOffset := VertScrollBar.Position - trunc(VertScrollBar.Position /
            IHeight) * IHeight;
         SavedYOffset := YOffset;
         while x_ < r.right do
         begin
            //if x_+IWidth > r.right then IWidth := r.right-x_;
            while y_ < r.bottom do
            begin
               IHeight := SavedIHeight;
               //if y_+IHeight-YOffset > r.bottom then IHeight := r.bottom-y_;
               BitBlt(DC, x_, y_, IWidth - XOffset, IHeight - YOffset,
                  FBack.canvas.Handle, XOffset, YOffset, SRCCOPY);
               Inc(y_, IHeight - YOffset);
               YOffset := 0;
            end;
            Inc(x_, IWidth - XOffset);
            y_ := r.top;
            XOffset := 0;
            YOffset := SavedYOffset;
         end;

      end;

   finally
      if Assigned(OnEraseBkgndEvent) then
         OnEraseBkgndEvent(self, DC);
      if Assigned(Canvas) then
         Canvas.Free;
      if BufferedDraw then
         ApplyBuffer(Msg.DC);
   end;
end;

//===========================================================================

function TJvgScrollBox.GetBack: TBitmap;
begin
   if not Assigned(FBack) then
      FBack := TBitmap.Create;
   Result := FBack;
end;

procedure TJvgScrollBox.SetBack(Value: TBitmap);
begin
   if Assigned(FBack) then
      FBack.Free;
   FBack := TBitmap.Create;
   FBack.Assign(Value);
   Invalidate;
end;

procedure TJvgScrollBox.SetOnEraseBkgndEvent(const Value: TOnEraseBkgndEvent);
begin
   FOnEraseBkgndEvent := Value;
end;

procedure TJvgScrollBox.ApplyBuffer(DC: HDC);
begin
   BitBlt(DC, 0, 0, Width, Height, Buffer.Canvas.Handle, 0, 0, SRCCOPY);
end;

end.

