{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgGroupBox.PAS, released on 2003-01-15.

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

unit JvgGroupBox;

// Illumination - fake :)
interface

uses
   Windows,
   Messages,
   SysUtils,
   Classes,
   Graphics,
   Controls,
   Forms,
   Dialogs,
   StdCtrls,
   ExtCtrls,
   JVCLVer,
   JvgTypes,
   JvgUtils,
   JvgCommClasses;

type
   TCaptionAlignment = (fcaNone, fcaLeft, fcaRight, fcaCenter, fcaWidth);
   TJvgGroupBox = class(TCustomGroupBox)
   private
      FEnabled: boolean;
      FBorder: TJvgBevel;
      FCaptionBorder: TJvgBevel;
      FGradient: TJvgGradient;
      FCaptionGradient: TJvgGradient;
      FCaptionShift: TJvgPointClass;
      FCaptionTextStyle: TglTextStyle;
      FCaptionAlignment: TCaptionAlignment;
      FColors: TJvgGroupBoxColors;
      FIllumination: TJvgIllumination;
      FTransparent: boolean;
      FTransparentCaption: boolean;
      FOptions: TglGrBoxOptions;
      FCollapsed: boolean;
      FAfterPaint: TNotifyEvent;
      FOnCollapsed: TNotifyEvent;
      FOnExpanded: TNotifyEvent;
      FGroupIndex: integer;
      FGlyphCollapsed: TBitmap;
      FGlyphExpanded: TBitmap;

      ChildFocusedControl: TWinControl;
      Image: TBitmap;
      FullHeight: integer;
      CaptionRect: TRect;
      ptScroll: TPoint;
      fScrolling: boolean;
      FAboutJVCL: TJVCLAboutInfo;

      procedure SetEnabled(Value: boolean);
      procedure SetCaptionAlignment(Value: TCaptionAlignment);
      procedure SetCaptionTextStyle(Value: TglTextStyle);
      procedure SetCollapsed(Value: boolean);
      procedure SetOptions(Value: TglGrBoxOptions);
      procedure SetTransparent(Value: boolean);
      procedure SetTransparentCaption(Value: boolean);
      procedure SetGroupIndex(Value: integer);
      function GetGlyphCollapsed: TBitmap;
      procedure SetGlyphCollapsed(Value: TBitmap);
      function GetGlyphExpanded: TBitmap;
      procedure SetGlyphExpanded(Value: TBitmap);

      procedure Collapse_(fCollapse: boolean);
      procedure SmthChanged(Sender: TObject);
   protected
      procedure ReadFullHeight(Reader: TReader);
      procedure WriteFullHeight(Writer: TWriter);
      procedure Paint; override;
      procedure CreateParams(var Params: TCreateParams); override;
      {$IFDEF COMPILER5_UP}
      procedure AdjustClientRect(var Rect: TRect); override;
      {$ENDIF}
      //    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
      procedure WMLButtonDown(var Message: TWMLButtonDown); message
         WM_LBUTTONDOWN;
      procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
      procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
   public
      procedure Collapse(fCollapse: boolean);
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure DefineProperties(Filer: TFiler); override;
   published
      property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored
         False;
      {$IFDEF COMPILER5_UP}
      property Anchors;
      {$ENDIF}
      property Align;
      property Caption;
      property Color;
      property DragCursor;
      property DragMode;
      property Font;
      property ParentColor;
      property ParentCtl3D;
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
      property Enabled: boolean read FEnabled write SetEnabled default true;
      property Border: TJvgBevel read FBorder write FBorder;
      property CaptionAlignment: TCaptionAlignment
         read FCaptionAlignment write SetCaptionAlignment default fcaNone;
      property CaptionBorder: TJvgBevel read FCaptionBorder write
         FCaptionBorder;
      property CaptionGradient: TJvgGradient read FCaptionGradient write
         FCaptionGradient;
      property CaptionShift: TJvgPointClass read FCaptionShift write
         FCaptionShift;
      property CaptionTextStyle: TglTextStyle
         read FCaptionTextStyle write SetCaptionTextStyle default fstNone;
      property Collapsed: boolean read FCollapsed write SetCollapsed default
         false;
      property Colors: TJvgGroupBoxColors read FColors write FColors;
      property Gradient: TJvgGradient read FGradient write FGradient;
      property Illumination: TJvgIllumination read FIllumination write
         FIllumination stored false;
      property Options: TglGrBoxOptions read FOptions write SetOptions;
      property Transparent: boolean
         read FTransparent write SetTransparent default false;
      property TransparentCaption: boolean
         read FTransparentCaption write SetTransparentCaption default false;
      property GroupIndex: integer read FGroupIndex write SetGroupIndex default
         0;
      property GlyphCollapsed: TBitmap read GetGlyphCollapsed write
         SetGlyphCollapsed;
      property GlyphExpanded: TBitmap read GetGlyphExpanded write
         SetGlyphExpanded;
      property AfterPaint: TNotifyEvent read FAfterPaint write FAfterPaint;
      property OnCollapsed: TNotifyEvent read FOnCollapsed write FOnCollapsed;
      property OnExpanded: TNotifyEvent read FOnExpanded write FOnExpanded;
   end;

procedure Register;

implementation

procedure Register;
begin
end;
//____________________________________________________ Methods _

constructor TJvgGroupBox.Create(AOwner: TComponent);
begin
   inherited;
   //  ControlStyle := ControlStyle + [csOpaque];
   FEnabled := true;
   FBorder := TJvgBevel.Create;
   FCaptionBorder := TJvgBevel.Create;
   FGradient := TJvgGradient.Create;
   FCaptionGradient := TJvgGradient.Create;
   FColors := TJvgGroupBoxColors.Create;
   FIllumination := TJvgIllumination.Create;
   FCaptionShift := TJvgPointClass.Create;
   //...defaults

   {$IFDEF COMPILER5_UP}
   FColors.Caption := clBtnShadow;
   FColors.CaptionActive := clBtnShadow;
   FColors.Text := clHighlightText;
   FColors.TextActive := clHighlightText;
   FBorder.Outer := bvNone;
   FBorder.Inner := bvSpace;
   FCaptionBorder.Outer := bvNone;
   FCaptionBorder.Inner := bvSpace;
   {$ELSE}
   FBorder.Outer := bvLowered;
   FBorder.Inner := bvRaised;
   FCaptionBorder.Outer := bvNone;
   FCaptionBorder.Inner := bvRaised;
   {$ENDIF}
   FGradient.FromColor := clBlack;
   FGradient.ToColor := clGray;
   FCaptionShift.x := 8;
   FCaptionShift.y := 0;
   FCaptionTextStyle := fstNone;
   FCaptionAlignment := fcaNone;
   FOptions := [fgoCanCollapse, fgoFilledCaption, fgoFluentlyCollapse,
      fgoFluentlyExpand, fgoHideChildrenWhenCollapsed, fgoSaveChildFocus];
   {$IFDEF GL_RUS}
   Font.CharSet := RUSSIAN_CHARSET;
   {$ENDIF}
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
   if Assigned(Image) then
      Image.Free;
   inherited;
end;

procedure TJvgGroupBox.DefineProperties(Filer: TFiler);
begin
   inherited;
   Filer.DefineProperty('FullHeight', ReadFullHeight, WriteFullHeight, true);
end;

procedure TJvgGroupBox.CreateParams(var Params: TCreateParams);
begin
   inherited CreateParams(Params);
   if Transparent or TransparentCaption then
      Params.ExStyle := Params.ExStyle or WS_EX_Transparent;
end;
{$IFDEF COMPILER5_UP}

procedure TJvgGroupBox.AdjustClientRect(var Rect: TRect);
begin
   inherited AdjustClientRect(Rect);
   Inc(Rect.Top, 1);
end;
{$ENDIF}

procedure TJvgGroupBox.WMLButtonDown(var Message: TWMLButtonDown);
var
   pt                         : TPoint;
begin
   inherited;
   if fgoIgnoreMouse in Options then
      exit;
   pt.x := Message.Pos.x;
   pt.y := Message.Pos.y;
   if (fgoCanCollapse in Options) and PtInRect(CaptionRect, pt) then
      Collapse(not Collapsed)
   else
   begin
      Screen.Cursor := crHandPoint;
      {ptScroll.x := pt.x;} ptScroll.y := pt.y;
      fScrolling := true;
   end;

end;

procedure TJvgGroupBox.WMMouseMove(var Message: TWMMouseMove);
begin
   if fScrolling and (Parent is TScrollBox) then
      (Parent as TScrollBox).VertScrollBar.Position := (Parent as
         TScrollBox).VertScrollBar.Position + ptScroll.y - Message.Pos.y;
   inherited;
end;

procedure TJvgGroupBox.WMLButtonUp(var Message: TWMLButtonUp);
begin
   inherited;
   if fgoIgnoreMouse in Options then
      exit;
   fScrolling := false;
   Screen.Cursor := crDefault;
end;

procedure TJvgGroupBox.ReadFullHeight(Reader: TReader);
begin
   FullHeight := Reader.ReadInteger;
end;

procedure TJvgGroupBox.WriteFullHeight(Writer: TWriter);
begin
   Writer.writeInteger(FullHeight);
end;

procedure TJvgGroupBox.Paint;
type
   TgbColor = record
      Text, Caption, Background, Client, Delineate: TColor;
   end;
var
   H, i, RW, GlyphWidth       : Integer;
   R, NewR                    : TRect;
   Glyph                      : TBitmap;
   DrawState                  : TglDrawState;
   Interspace                 : integer;
   Color                      : TgbColor;
begin
   //inherited;
   //exit;
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
      Canvas.Font.Assign(self.Font);
      r := GetClientRect;
      //Font := Self.Font;
  //    if CaptionHeight = 0 then H := TextHeight('0') - FCaptionShift.y
  //                         else H := CaptionHeight - FCaptionShift.y
      H := TextHeight(Text) - FCaptionShift.y;
      R := Rect(0, H div 2 { - 1}, Width, Height);
      if FGradient.Active then
         GradientBox(Handle, r, FGradient, PS_SOLID, 1);

      dec(r.right);
      dec(r.bottom);
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

         R := Rect(FCaptionShift.x, 0, 0, H);

         if Collapsed then
            Glyph := FGlyphCollapsed
         else
            Glyph := FGlyphExpanded;

         DrawText(Handle, PChar(Text), Length(Text), R, DT_LEFT or DT_SINGLELINE
            or DT_CALCRECT);

         inc(R.right, Interspace + GlyphWidth);
         with CaptionBorder do
         begin
            i := 0;
            if Inner <> bvNone then
            begin
               inc(i, 2);
               if Bold then
                  inc(i);
            end;
            if Outer <> bvNone then
            begin
               inc(i, 2);
               if Bold then
                  inc(i);
            end;

            inc(R.Right, i);
            inc(R.Bottom, i);
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
                  R.Right := width - 1;
               end;
         end;
         if fgoDelineatedText in Options then
            inc(r.bottom, 2);
         if CaptionTextStyle = fstShadow then
         begin
            if fgoDelineatedText in Options then
               inc(r.bottom, FIllumination.ShadowDepth - 2)
            else
               inc(r.bottom, FIllumination.ShadowDepth)
         end
         else if CaptionTextStyle <> fstNone then
            inc(r.bottom, 2);
         //Canvas.Brush.Color:=clBtnFace; Canvas.Brush.Style:=bsSolid;
         if not TransparentCaption then
         begin
            Canvas.Brush.Color := Colors.Caption;
            Windows.FillRect(Canvas.Handle, r, Canvas.Brush.Handle);
         end;
         GradientBox(Canvas.Handle, r, FCaptionGradient, PS_SOLID, 1);
         CaptionRect := R;

         NewR := DrawBoxEx(Canvas.Handle, R, CaptionBorder.Sides,
            CaptionBorder.Inner, CaptionBorder.Outer,
            CaptionBorder.Bold, Color.Caption,
            TransparentCaption or not (fgoFilledCaption in Options));
         //      Brush.Color := Color;

         SetBkMode(Handle, integer(TRANSPARENT));

         if Assigned(Glyph) then
         begin
            if Enabled then
               DrawState := fdsDefault
            else
               DrawState := fdsDisabled;
            CreateBitmapExt(Handle, Glyph, NewR, 0, max(0, (NewR.bottom -
               NewR.top - Glyph.Height) div 2),
               fwoNone, DrawState, true,
               GetPixel(Glyph.Canvas.Handle, 0, Glyph.Height - 1)
                  {TransparentColor},
               {DisabledMaskColor} 0);
         end;

         ExtTextOutExt((Handle), (NewR.Left + GlyphWidth + Interspace),
            (NewR.Top), NewR, Caption,
            IIF(Enabled, FCaptionTextStyle, fstPushed), fgoDelineatedText in
               Options,
            {fNeedUpdateOnlyMainText} false, Color.Text, Color.Delineate,
            Colors.Highlight, Colors.Shadow,
            FIllumination, nil {Gradient}, Font);

      end;
   end;
   //  if Transparent then for i:=0 to ComponentCount-1 do
   //    TControl(Components[i]).Repaint;
   if Assigned(AfterPaint) then
      AfterPaint(self);
end;

procedure TJvgGroupBox.Collapse(fCollapse: boolean);
var
   i                          : integer;
   fAnotherExpandedWasFound   : boolean;
begin
   if csLoading in ComponentState then
      exit;

   if fCollapse then
   begin
      if Align = alClient then
         exit;
      if (FGroupIndex <> 0) and (fgoOneAlwaysExpanded in Options) then
      begin                             //...One Stay Always Expanded in group
         fAnotherExpandedWasfound := false;
         for i := 0 to Owner.ComponentCount - 1 do
            with TControl(Owner) do
               if (Components[i] is TJvgGroupBox) and
                  (TJvgGroupBox(Components[i]).GroupIndex = FGroupIndex) then
                  if
                     (not TJvgGroupBox(Components[i]).Collapsed) and
                        (Components[i] <> self) then
                  begin
                     fAnotherExpandedWasFound := true;
                     break;
                  end; //...are another expanded controls in group
         if not fAnotherExpandedWasFound then
            exit;                       //..i'm last- can't collapse
      end;
   end
   else if (FGroupIndex <> 0) and (fgoCollapseOther in Options) then
      for i := 0 to Owner.ComponentCount - 1 do
         with TControl(Owner) do
            if (Components[i] is TJvgGroupBox) and
               (TJvgGroupBox(Components[i]).GroupIndex = FGroupIndex) and
               (Components[i] <> self) then
               TJvgGroupBox(Components[i]).Collapsed := true;

   Collapse_(fCollapse);
   if fCollapse and Assigned(FOnCollapsed) then
      FOnCollapsed(self);
   if not fCollapse and Assigned(FOnExpanded) then
      FOnExpanded(self);

end;

procedure TJvgGroupBox.Collapse_(fCollapse: boolean);
var
   i {, Step}                 : integer;
   {  DC: HDC;
     pt: TPoint;
     R, CR: TRect;
     Scroll: HRGN;
     SpaceBrush: HBRUSH;
     fFirst: boolean;
     LastTickCount: integer;}
begin
   if Align = alClient then
      exit;

   FCollapsed := fCollapse;
   if fCollapse then
   begin
      FullHeight := Height;
      if fgoResizeParent in Options then
         Parent.Height := Parent.Height - (FullHeight - CaptionRect.Bottom);
      Height := CaptionRect.Bottom + 1;

      //...set all Children invisible
      if (fgoHideChildrenWhenCollapsed in Options) or (fgoSaveChildFocus in
         Options) then
         for i := 0 to Owner.ComponentCount - 1 do
            with TControl(Owner) do
               if (Components[i] is TControl) and (TControl(Components[i]).Parent
                  = self) then
               begin
                  if (fgoSaveChildFocus in Options) and (Components[i] is
                     TWinControl)
                     and TWinControl(Components[i]).Focused then
                     ChildFocusedControl := TWinControl(Components[i]);
                  if fgoHideChildrenWhenCollapsed in Options then //...hide
                     TControl(Components[i]).Visible := false;
               end;

   end
   else
   begin
      if fgoResizeParent in Options then
         Parent.Height := Parent.Height + (FullHeight - CaptionRect.Bottom);
      Height := FullHeight;

      //...set all Children visible
      if fgoHideChildrenWhenCollapsed in Options then
         for i := 0 to Owner.ComponentCount - 1 do
            with TControl(Owner) do
               if (Components[i] is TControl) and (TControl(Components[i]).Parent
                  = self) then
                  TControl(Components[i]).Visible := true;

      if ChildFocusedControl <> nil then
      try
         ChildFocusedControl.SetFocus;
      except
      end;

   end;

   exit;                                { patch for win 98 }

   (*
   if fCollapse then
   begin
     //...prepare image

     if Image=nil then Image := TBitmap.Create;
     Image.Height := Height;
     Image.Width := Width;
     GetWindowImage( self, true{fDrawSelf}, true{fDrawChildWindows}, Image.Canvas.Handle );

     DC := GetDC(0);
     CR := ClientRect; dec(CR.Bottom,CaptionRect.Bottom);
     pt.x := 0; pt.y := CaptionRect.Bottom+1;//16;
     pt := ClientToScreen(pt);
     offsetRect( CR, pt.x, pt.y );
     R := CR;
     Scroll := CreateRectRgn( R.Left, R.top, R.Right-R.Left, R.Bottom-R.Top );
     inc( R.top );
     i := Height-Canvas.TextHeight('0y');
     // Step := 1;//max( Height div 100, 1 );

     SpaceBrush := CreateSolidBrush( ColorToRGB(TGroupBox(Parent).Color) );
     // fFirst := true; LastTickCount := GetTickCount;
     FullHeight := Height;
     if fgoFluentlyCollapse in Options then
     while Height > CaptionRect.bottom+1 do
     begin
       Application.ProcessMessages;
 //      ScrollDC( DC, 0, -Step, R, CR,  Scroll, nil);
       //if fFirst then
 //      begin
 //        FillRect( DC, Rect( R.Left, R.Bottom-Step, R.right, R.Bottom ), SpaceBrush );
 //        fFirst := false;
 //      end;
       Height := max(CaptionRect.bottom, Height - Height * 30 div 100);
       ValidateRect(Handle, @CaptionRect);
 //      while GetTickCount - LastTickCount <= 0 do;
 //      Step := GetTickCount - LastTickCount; if Step > 20 then Step := 20;
 //      LastTickCount := GetTickCount;
     end;
     DeleteObject(SpaceBrush);
 //    FullHeight := Height;
     Height := CaptionRect.bottom+1;//max( Canvas.TextHeight(Caption), 16 );
     DeleteObject( Scroll );
     ReleaseDC( 0, DC );

   end else
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
   InValidateRect(Handle, @CaptionRect, false);
   FCollapsed := fCollapse;
   *)
end;

procedure TJvgGroupBox.SmthChanged(Sender: TObject);
begin
   Invalidate;
end;
//____________________________________________________ Properties Methods _

procedure TJvgGroupBox.SetEnabled(Value: boolean);
var
   i                          : integer;
begin
   FEnabled := Value;
   inherited Enabled := FEnabled;
   for i := 0 to ControlCount - 1 do
      Controls[i].Enabled := FEnabled;
end;

procedure TJvgGroupBox.SetCaptionAlignment(Value: TCaptionAlignment);
begin
   if FCaptionAlignment = Value then
      exit;
   FCaptionAlignment := Value;
   Invalidate;
end;

procedure TJvgGroupBox.SetCaptionTextStyle(Value: TglTextStyle);
begin
   if FCaptionTextStyle = Value then
      exit;
   FCaptionTextStyle := Value;
   Invalidate;
end;

procedure TJvgGroupBox.SetCollapsed(Value: boolean);
begin
   if FCollapsed = Value then
      exit;
   if not (fgoCanCollapse in Options) and Value then
      exit;
   FCollapsed := Value;
   if csLoading in ComponentState then
      exit;
   Collapse_(Value);
end;

procedure TJvgGroupBox.SetOptions(Value: TglGrBoxOptions);
begin
   if FOptions = Value then
      exit;
   FOptions := Value;
   //  if not(fgoCanCollapse in Options) then Collapsed := false;
   if Assigned(Parent) then
      CalcShadowAndHighlightColors((Parent as TWinControl).Brush.Color,
         TJvgLabelColors(Colors));
   Invalidate;
end;

procedure TJvgGroupBox.SetTransparent(Value: boolean);
begin
   if FTransparent = Value then
      exit;
   FTransparent := Value;
   RecreateWnd;
end;

procedure TJvgGroupBox.SetTransparentCaption(Value: boolean);
begin
   if FTransparentCaption = Value then
      exit;
   FTransparentCaption := Value;
   RecreateWnd;
end;

procedure TJvgGroupBox.SetGroupIndex(Value: integer);
var
   i                          : integer;
begin
   if FGroupIndex = Value then
      exit;
   FGroupIndex := Value;
   if csLoading in ComponentState then
      exit;
   if (not Collapsed) and (FGroupIndex <> 0) then
      for i := 0 to Owner.ComponentCount - 1 do
         with TControl(Owner) do
            if (Components[i] is TJvgGroupBox) and
               (TJvgGroupBox(Components[i]).GroupIndex = FGroupIndex) and
               (Components[i] <> self) then
               TJvgGroupBox(Components[i]).Collapsed := true;
end;

function TJvgGroupBox.GetGlyphCollapsed: TBitmap;
begin
   if FGlyphCollapsed = nil then
      FGlyphCollapsed := TBitmap.Create;
   Result := FGlyphCollapsed;
end;

procedure TJvgGroupBox.SetGlyphCollapsed(Value: TBitmap);
begin
   if Assigned(FGlyphCollapsed) then
      FGlyphCollapsed.Free;
   FGlyphCollapsed := TBitmap.Create;
   FGlyphCollapsed.Assign(Value);
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
   if Assigned(FGlyphExpanded) then
      FGlyphExpanded.Free;
   FGlyphExpanded := TBitmap.Create;
   FGlyphExpanded.Assign(Value);
   Invalidate;
end;

end.

