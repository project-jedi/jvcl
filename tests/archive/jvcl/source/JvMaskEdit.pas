{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMaskEdit.PAS, released on 2001-02-28.

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

unit JvMaskEdit;

interface

uses
   JvComponent,
   Windows,
   Messages,
   SysUtils,
   Classes,
   Graphics,
   Controls,
   Mask,
   Forms,
   JVCLVer;

type
   TJvMaskEdit = class(TMaskEdit)
   private
      FOnMouseEnter: TNotifyEvent;
      FOnMouseLeave: TNotifyEvent;
      FOnParentColorChanged: TNotifyEvent;
      FOnCtl3DChanged: TNotiFyEvent;
      FSaved: TColor;
      FColor: TColor;
      FOver: Boolean;
      FEffect: Boolean;
      FAboutJVCL: TJVCLAboutInfo;
      FCaret: TJvCaret;
      FClipBoardCommands: TJvClipboardCommands;
      FGroupIndex: Integer;
      FDisabledColor: TColor;
      FDisabledTextColor: TColor;

      procedure SetCtl3d(Value: Boolean);
      procedure UpdateEdit;
   protected
      procedure CMEnabledchanged(var Message: TMessage); message CM_ENABLEDCHANGED;
      procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
      procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
      procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
      procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
      procedure CaretChanged(sender: TObject); dynamic;
      procedure WMSetFocus(var msg: TMessage); message WM_SETFOCUS;
      procedure WMPaint(var msg: TWMPaint); message WM_PAINT;
      procedure WMEraseBkGnd(var msg: TWMEraseBkGnd); message WM_ERASEBKGND;
      procedure WMPaste(var Msg: TWMPaste); message WM_PASTE;
      procedure WMCopy(var Msg: TWMCopy); message WM_COPY;
      procedure WMCut(var Msg: TWMCut); message WM_CUT;
      procedure WMUndo(var Msg: TWMUndo); message WM_UNDO;

      procedure SetCaret(const Value: TJvCaret);
      procedure SetDisabledColor(const Value: TColor); virtual;
      procedure SetDisabledTextColor(const Value: TColor); virtual;
      procedure SetClipBoardCommands(const Value: TJvClipboardCommands);
      procedure SetGroupIndex(const Value: Integer);

   public
      constructor Create(AOwner: TComponent); override;
   published
      property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored
         False;
      property HotTrack: Boolean read FEffect write SetCtl3d default False;
      property HintColor: TColor read FColor write FColor default clInfoBk;
      property Caret: TJvCaret read FCaret write SetCaret;
      property ClipBoardCommands: TJvClipboardCommands read FClipBoardCommands
         write SetClipBoardCommands default [caCopy..caUndo];
      property DisabledTextColor: TColor read FDisabledTextColor write
         SetDisabledTextColor default clGrayText;
      property DisabledColor: TColor read FDisabledColor write SetDisabledColor
         default clWindow;

      property GroupIndex: Integer read FGroupIndex write SetGroupIndex;

      property OnMouseEnter: TNotifyEvent read FOnMouseEnter write
         FOnMouseEnter;
      property OnMouseLeave: TNotifyEvent read FOnMouseLeave write
         FOnMouseLeave;
      property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write
         FOnCtl3DChanged;
      property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write
         FOnParentColorChanged;
   end;

implementation

{**************************************************}

procedure TJvMaskEdit.CMCtl3DChanged(var Msg: TMessage);
begin
   inherited;
   if Assigned(FOnCtl3DChanged) then
      FOnCtl3DChanged(Self);
end;

{**************************************************}

procedure TJvMaskEdit.CMParentColorChanged(var Msg: TMessage);
begin
   inherited;
   if Assigned(FOnParentColorChanged) then
      FOnParentColorChanged(Self);
end;

{***********************************************}

constructor TJvMaskEdit.Create(AOwner: TComponent);
begin
   inherited;
   FEffect := False;
   FColor := clInfoBk;
   FOver := False;
   FDisabledColor := clWindow;
   FDisabledTextColor := clGrayText;
   FClipBoardCommands := [caCopy..caUndo];
   FCaret := TJvCaret.Create(self);
   FCaret.OnChanged := CaretChanged;
   FGroupIndex := -1;
   ControlStyle := ControlStyle + [csAcceptsControls];
end;
{**************************************************}
procedure TJvMaskEdit.CMEnabledchanged(var Message: TMessage);
begin
   inherited;
   Invalidate;
end;

procedure TJvMaskEdit.CMMouseEnter(var Msg: TMessage);
begin
   if not FOver then
   begin
      FSaved := Application.HintColor;
      // for D7...
      if csDesigning in ComponentState then Exit;
      Application.HintColor := FColor;
      if FEffect then
         Ctl3d := True;
      FOver := True;
   end;
   if Assigned(FOnMouseEnter) then
      FOnMouseEnter(Self);
end;

{**************************************************}

procedure TJvMaskEdit.CMMouseLeave(var Msg: TMessage);
begin
   if FOver then
   begin
      Application.HintColor := FSaved;
      if FEffect then
         Ctl3d := False;
      FOver := False;
   end;
   if Assigned(FOnMouseLeave) then
      FOnMouseLeave(Self);
end;

{***********************************************}

procedure TJvMaskEdit.SetCtl3d(Value: Boolean);
begin
   FEffect := Value;
   if Value then
      Ctl3d := False;
end;

procedure TJvMaskEdit.CaretChanged(sender: TObject);
begin
   FCaret.CreateCaret;
end;

procedure TJvMaskEdit.SetCaret(const Value: TJvCaret);
begin
   FCaret.Assign(Value);
end;

procedure TJvMaskEdit.SetClipBoardCommands(
   const Value: TJvClipboardCommands);
begin
   if FClipBoardCommands <> Value then
   begin
      FClipBoardCommands := Value;
      ReadOnly := FClipBoardCommands <= [caCopy];
   end;
end;

procedure TJvMaskEdit.SetGroupIndex(const Value: Integer);
begin
   FGroupIndex := Value;
   UpdateEdit;
end;

procedure TJvMaskEdit.UpdateEdit;
var
   i                : Integer;
begin
   for I := 0 to self.Owner.ComponentCount - 1 do
   begin
      if (Self.Owner.Components[i] is TJvMaskEdit) then
      begin
         if
            ((Self.Owner.Components[i].Name <> Self.Name)
            and
            ((Self.Owner.Components[i] as TJvMaskEdit).GroupIndex <> -1)
            and
            ((Self.Owner.Components[i] as TJvMaskEdit).fGroupIndex =
               Self.fGroupIndex)
            ) then
            (Self.Owner.Components[i] as TJvMaskEdit).Caption := '';
      end;
   end;
end;

procedure TJvMaskEdit.SetDisabledColor(const Value: TColor);
begin
   if FDisabledColor <> Value then
   begin
      FDisabledColor := Value;
      if not Enabled then
         Invalidate;
   end;
end;

procedure TJvMaskEdit.SetDisabledTextColor(const Value: TColor);
begin
   if FDisabledTextColor <> Value then
   begin
      FDisabledTextColor := Value;
      if not Enabled then
         Invalidate;
   end;
end;

procedure TJvMaskEdit.WMCopy(var Msg: TWMCopy);
begin
   if caCopy in ClipBoardCommands then
      inherited;
end;

procedure TJvMaskEdit.WMCut(var Msg: TWMCut);
begin
   if caCut in ClipBoardCommands then
      inherited;
end;

procedure TJvMaskEdit.WMPaste(var Msg: TWMPaste);
begin
   if caPaste in ClipBoardCommands then
      inherited;
   UpdateEdit;
end;

procedure TJvMaskEdit.WMUndo(var Msg: TWMUndo);
begin
   if caUndo in ClipBoardCommands then
      inherited;
end;


procedure TJvMaskEdit.WMPaint(var msg: TWMPaint);
var
   canvas           : TCanvas;
   ps               : TPaintStruct;
   callEndPaint     : Boolean;
begin
   if Enabled then
      inherited
   else
   begin
      callEndPaint := False;
      canvas := TCanvas.Create;
      try
         if msg.DC <> 0 then
         begin
            canvas.Handle := msg.DC;
            ps.fErase := true;
         end
         else
         begin
            BeginPaint(handle, ps);
            callEndPaint := true;
            canvas.handle := ps.hdc;
         end;

         if ps.fErase then
            Perform(WM_ERASEBKGND, canvas.handle, 0);

         SaveDC(canvas.handle);
         try
            canvas.Brush.Style := bsClear;
            canvas.Font := Font;
            canvas.Font.Color := FDisabledTextColor;
            canvas.TextOut(1, 1, Text);
         finally
            RestoreDC(canvas.handle, -1);
         end;
      finally
         if callEndPaint then
            EndPaint(handle, ps);
         canvas.free
      end;
   end;
end;
procedure TJvMaskEdit.WMEraseBkGnd(var msg: TWMEraseBkGnd);
var
   canvas           : TCanvas;
begin
   if Enabled then
      inherited
   else
   begin
      Canvas := TCanvas.Create;
      try
         Canvas.Handle := msg.DC;
         SaveDC(msg.DC);
         try
            Canvas.Brush.Color := FDisabledColor;
            Canvas.Brush.Style := bsSolid;
            Canvas.Fillrect(clientrect);
            Msg.result := 1;
         finally
            RestoreDC(msg.DC, -1);
         end;
      finally
         canvas.free
      end;
   end;                                 { Else }
end;

procedure TJvMaskEdit.WMSetFocus(var msg: TMessage);
begin
   inherited;
   FCaret.CreateCaret;
end;

end.

