{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter_Controls.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : adapter unit - converts JvInterpreter calls to delphi calls

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvInterpreter_Controls;

{$I jvcl.inc}

interface

uses
  JvInterpreter;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes,
  {$IFDEF VCL}
  Graphics, Controls, Menus, ImgList,
  JvInterpreter_Windows;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Qt, QGraphics, QControls, QMenus, QImgList,
  JvInterpreter_Types;
  {$ENDIF VisualCLX}

{ TControl }

{ constructor Create(AOwner: TComponent) }

procedure TControl_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TControl.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure BeginDrag(Immediate: Boolean); }

procedure TControl_BeginDrag(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).BeginDrag(Args.Values[0]);
end;

{ procedure BringToFront; }

procedure TControl_BringToFront(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).BringToFront;
end;

{ function ClientToScreen(const Point: TPoint): TPoint; }

procedure TControl_ClientToScreen(var Value: Variant; Args: TJvInterpreterArgs);
begin
  JvInterpreterVarCopy(Value, Point2Var(TControl(Args.Obj).ClientToScreen(Var2Point(Args.Values[0]))));
end;

{ function Dragging: Boolean; }

procedure TControl_Dragging(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TControl(Args.Obj).Dragging;
end;

{ procedure DragDrop(Source: TObject; X, Y: Integer); }

procedure TControl_DragDrop(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).DragDrop(V2O(Args.Values[0]), Args.Values[1], Args.Values[2]);
end;

{ procedure EndDrag(Drop: Boolean); }

procedure TControl_EndDrag(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).EndDrag(Args.Values[0]);
end;

{$IFDEF VCL}

{ function GetTextBuf(Buffer: PChar; BufSize: Integer): Integer; }

procedure TControl_GetTextBuf(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TControl(Args.Obj).GetTextBuf(PChar(string(Args.Values[0])), Args.Values[1]);
end;

{ function GetTextLen: Integer; }

procedure TControl_GetTextLen(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TControl(Args.Obj).GetTextLen;
end;

{$ENDIF VCL}

{ procedure Hide; }

procedure TControl_Hide(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).Hide;
end;

{ procedure Invalidate; }

procedure TControl_Invalidate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).Invalidate;
end;

{ function Perform(Msg: Cardinal; WParam, LParam: Longint): Longint; }

{$IFDEF VCL}
procedure TControl_Perform(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TControl(Args.Obj).Perform(Args.Values[0], Args.Values[1], Args.Values[2]);
end;
{$ENDIF VCL}

{ procedure Refresh; }

procedure TControl_Refresh(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).Refresh;
end;

{ procedure Repaint; }

procedure TControl_Repaint(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).Repaint;
end;

{ function ScreenToClient(const Point: TPoint): TPoint; }

procedure TControl_ScreenToClient(var Value: Variant; Args: TJvInterpreterArgs);
begin
  JvInterpreterVarCopy(Value, Point2Var(TControl(Args.Obj).ScreenToClient(Var2Point(Args.Values[0]))));
end;

{ procedure SendToBack; }

procedure TControl_SendToBack(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).SendToBack;
end;

{ procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); }

procedure TControl_SetBounds(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).SetBounds(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3]);
end;

{ procedure SetTextBuf(Buffer: PChar); }

{$IFDEF VCL}
procedure TControl_SetTextBuf(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).SetTextBuf(PChar(string(Args.Values[0])));
end;
{$ENDIF VCL}

{ procedure Show; }

procedure TControl_Show(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).Show;
end;

{ procedure Update; }

procedure TControl_Update(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).Update;
end;

{ property Read Align: TAlign }

procedure TControl_Read_Align(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TControl(Args.Obj).Align;
end;

{ property Write Align(Value: TAlign) }

procedure TControl_Write_Align(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).Align := Value;
end;

{ property Read BoundsRect: TRect }

procedure TControl_Read_BoundsRect(var Value: Variant; Args: TJvInterpreterArgs);
begin
  JvInterpreterVarCopy(Value, Rect2Var(TControl(Args.Obj).BoundsRect));
end;

{ property Write BoundsRect(Value: TRect) }

procedure TControl_Write_BoundsRect(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).BoundsRect := Var2Rect(Value);
end;

{ property Read ClientHeight: Integer }

procedure TControl_Read_ClientHeight(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TControl(Args.Obj).ClientHeight;
end;

{ property Write ClientHeight(Value: Integer) }

procedure TControl_Write_ClientHeight(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).ClientHeight := Value;
end;

{ property Read ClientOrigin: TPoint }

procedure TControl_Read_ClientOrigin(var Value: Variant; Args: TJvInterpreterArgs);
begin
  JvInterpreterVarCopy(Value, Point2Var(TControl(Args.Obj).ClientOrigin));
end;

{ property Read ClientRect: TRect }

procedure TControl_Read_ClientRect(var Value: Variant; Args: TJvInterpreterArgs);
begin
//  Value := TControl(Args.Obj).ClientRect;
  NotImplemented('TControl.ClientRect');
end;

{ property Read ClientWidth: Integer }

procedure TControl_Read_ClientWidth(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TControl(Args.Obj).ClientWidth;
end;

{ property Write ClientWidth(Value: Integer) }

procedure TControl_Write_ClientWidth(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).ClientWidth := Value;
end;

{ property Read ControlState: TControlState }

procedure TControl_Read_ControlState(var Value: Variant; Args: TJvInterpreterArgs);
begin
//  Value := TControl(Args.Obj).ControlState;
  NotImplemented('TControl.ControlState');
end;

{ property Write ControlState(Value: TControlState) }

procedure TControl_Write_ControlState(const Value: Variant; Args: TJvInterpreterArgs);
begin
//  TControl(Args.Obj).ControlState := Value;
  NotImplemented('TControl.ControlState');
end;

{ property Read ControlStyle: TControlStyle }

procedure TControl_Read_ControlStyle(var Value: Variant; Args: TJvInterpreterArgs);
begin
//  Value := TControl(Args.Obj).ControlStyle;
  NotImplemented('TControl.ControlState');
end;

{ property Write ControlStyle(Value: TControlStyle) }

procedure TControl_Write_ControlStyle(const Value: Variant; Args: TJvInterpreterArgs);
begin
//  TControl(Args.Obj).ControlStyle := Value;
  NotImplemented('TControl.ControlStyle');
end;

{ property Read Parent: TWinControl }

procedure TControl_Read_Parent(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TControl(Args.Obj).Parent);
end;

{ property Write Parent(Value: TWinControl) }

procedure TControl_Write_Parent(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).Parent := V2O(Value) as TWinControl;
end;

{ property Read ShowHint: Boolean }

procedure TControl_Read_ShowHint(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TControl(Args.Obj).ShowHint;
end;

{ property Write ShowHint(Value: Boolean) }

procedure TControl_Write_ShowHint(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).ShowHint := Value;
end;

{ property Read Visible: Boolean }

procedure TControl_Read_Visible(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TControl(Args.Obj).Visible;
end;

{ property Write Visible(Value: Boolean) }

procedure TControl_Write_Visible(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).Visible := Value;
end;

{ property Read Enabled: Boolean }

procedure TControl_Read_Enabled(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TControl(Args.Obj).Enabled;
end;

{ property Write Enabled(Value: Boolean) }

procedure TControl_Write_Enabled(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).Enabled := Value;
end;

{ property Read WindowProc: TWndMethod }

procedure TControl_Read_WindowProc(var Value: Variant; Args: TJvInterpreterArgs);
begin
//  Value := TControl(Args.Obj).WindowProc;
  NotImplemented('TControl.WindowProc');
end;

{ property Write WindowProc(Value: TWndMethod) }

procedure TControl_Write_WindowProc(const Value: Variant; Args: TJvInterpreterArgs);
begin
//  TControl(Args.Obj).WindowProc := Value;
  NotImplemented('TControl.WindowProc');
end;

{ property Read Left: Integer }

procedure TControl_Read_Left(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TControl(Args.Obj).Left;
end;

{ property Write Left(Value: Integer) }

procedure TControl_Write_Left(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).Left := Value;
end;

{ property Read Top: Integer }

procedure TControl_Read_Top(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TControl(Args.Obj).Top;
end;

{ property Write Top(Value: Integer) }

procedure TControl_Write_Top(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).Top := Value;
end;

{ property Read Width: Integer }

procedure TControl_Read_Width(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TControl(Args.Obj).Width;
end;

{ property Write Width(Value: Integer) }

procedure TControl_Write_Width(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).Width := Value;
end;

{ property Read Height: Integer }

procedure TControl_Read_Height(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TControl(Args.Obj).Height;
end;

{ property Write Height(Value: Integer) }

procedure TControl_Write_Height(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).Height := Value;
end;

{ property Read Cursor: TCursor }

procedure TControl_Read_Cursor(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TControl(Args.Obj).Cursor;
end;

{ property Write Cursor(Value: TCursor) }

procedure TControl_Write_Cursor(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).Cursor := Value;
end;

{ property Read Hint: string }

procedure TControl_Read_Hint(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TControl(Args.Obj).Hint;
end;

{ property Write Hint(Value: string) }

procedure TControl_Write_Hint(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TControl(Args.Obj).Hint := Value;
end;

{ TWinControl }

{ constructor Create(AOwner: TComponent) }

procedure TWinControl_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TWinControl.Create(V2O(Args.Values[0]) as TComponent));
end;

{ constructor CreateParented(ParentWindow: HWnd) }

procedure TWinControl_CreateParented(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF VCL}
  Value := O2V(TWinControl.CreateParented(Args.Values[0]));
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Value := O2V(TWinControl.CreateParented(V2P(Args.Values[0])));
  {$ENDIF VisualCLX}
end;

{ procedure Broadcast(var Message); }

procedure TWinControl_Broadcast(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TWinControl(Args.Obj).Broadcast(Args.Values[0]);
end;

{ function CanFocus: Boolean; }

procedure TWinControl_CanFocus(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TWinControl(Args.Obj).CanFocus;
end;

{ function ContainsControl(Control: TControl): Boolean; }

procedure TWinControl_ContainsControl(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TWinControl(Args.Obj).ContainsControl(V2O(Args.Values[0]) as TControl);
end;

{ function ControlAtPos(const Pos: TPoint; AllowDisabled: Boolean): TControl; }

procedure TWinControl_ControlAtPos(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TWinControl(Args.Obj).ControlAtPos(Var2Point(Args.Values[0]), Args.Values[1]));
end;

{ procedure DisableAlign; }

procedure TWinControl_DisableAlign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TWinControl(Args.Obj).DisableAlign;
end;

{ procedure EnableAlign; }

procedure TWinControl_EnableAlign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TWinControl(Args.Obj).EnableAlign;
end;

{ function Focused: Boolean; }

procedure TWinControl_Focused(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TWinControl(Args.Obj).Focused;
end;

{ procedure GetTabOrderList(List: TList); }

procedure TWinControl_GetTabOrderList(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TWinControl(Args.Obj).GetTabOrderList(V2O(Args.Values[0]) as TList);
end;

{ function HandleAllocated: Boolean; }

procedure TWinControl_HandleAllocated(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TWinControl(Args.Obj).HandleAllocated;
end;

{ procedure HandleNeeded; }

procedure TWinControl_HandleNeeded(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TWinControl(Args.Obj).HandleNeeded;
end;

{ procedure InsertControl(AControl: TControl); }

procedure TWinControl_InsertControl(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TWinControl(Args.Obj).InsertControl(V2O(Args.Values[0]) as TControl);
end;

{ procedure Invalidate; }

procedure TWinControl_Invalidate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TWinControl(Args.Obj).Invalidate;
end;

{ procedure PaintTo(DC: HDC; X, Y: Integer); }

{$IFDEF VCL}
procedure TWinControl_PaintTo(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TWinControl(Args.Obj).PaintTo(Args.Values[0], Args.Values[1], Args.Values[2]);
end;
{$ENDIF VCL}

{ procedure RemoveControl(AControl: TControl); }

procedure TWinControl_RemoveControl(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TWinControl(Args.Obj).RemoveControl(V2O(Args.Values[0]) as TControl);
end;

{ procedure Realign; }

procedure TWinControl_Realign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TWinControl(Args.Obj).Realign;
end;

{ procedure Repaint; }

procedure TWinControl_Repaint(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TWinControl(Args.Obj).Repaint;
end;

{ procedure ScaleBy(M, D: Integer); }

procedure TWinControl_ScaleBy(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TWinControl(Args.Obj).ScaleBy(Args.Values[0], Args.Values[1]);
end;

{ procedure ScrollBy(DeltaX, DeltaY: Integer); }

procedure TWinControl_ScrollBy(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TWinControl(Args.Obj).ScrollBy(Args.Values[0], Args.Values[1]);
end;

{ procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); }

procedure TWinControl_SetBounds(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TWinControl(Args.Obj).SetBounds(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3]);
end;

{  procedure SetFocus; }

procedure TWinControl_SetFocus(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TWinControl(Args.Obj).SetFocus;
end;

{ procedure Update; }

procedure TWinControl_Update(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TWinControl(Args.Obj).Update;
end;

{ procedure UpdateControlState; }

procedure TWinControl_UpdateControlState(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TWinControl(Args.Obj).UpdateControlState;
end;

{ property Read Brush: TBrush }

procedure TWinControl_Read_Brush(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TWinControl(Args.Obj).Brush);
end;

{ property Read Controls[Integer]: TControl }

procedure TWinControl_Read_Controls(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TWinControl(Args.Obj).Controls[Args.Values[0]]);
end;

{ property Read ControlCount: Integer }

procedure TWinControl_Read_ControlCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TWinControl(Args.Obj).ControlCount;
end;

{ property Read Handle: HWnd }

procedure TWinControl_Read_Handle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TWinControl(Args.Obj).Handle);
end;

{ property Read ParentWindow: HWnd }

procedure TWinControl_Read_ParentWindow(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF VCL}
  Value := Integer(TWinControl(Args.Obj).ParentWindow);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Value := Integer(TWinControl(Args.Obj).ParentWidget);
  {$ENDIF VisualCLX}
end;

{ property Write ParentWindow(Value: HWnd) }

procedure TWinControl_Write_ParentWindow(const Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF VCL}
  TWinControl(Args.Obj).ParentWindow := Value;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  TWinControl(Args.Obj).ParentWidget := V2P(Value);
  {$ENDIF VisualCLX}
end;

{ property Read Showing: Boolean }

procedure TWinControl_Read_Showing(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TWinControl(Args.Obj).Showing;
end;

{ property Read TabOrder: TTabOrder }

procedure TWinControl_Read_TabOrder(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TWinControl(Args.Obj).TabOrder;
end;

{ property Write TabOrder(Value: TTabOrder) }

procedure TWinControl_Write_TabOrder(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TWinControl(Args.Obj).TabOrder := Value;
end;

{ property Read TabStop: Boolean }

procedure TWinControl_Read_TabStop(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TWinControl(Args.Obj).TabStop;
end;

{ property Write TabStop(Value: Boolean) }

procedure TWinControl_Write_TabStop(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TWinControl(Args.Obj).TabStop := Value;
end;

{ property Read HelpContext: THelpContext }

procedure TWinControl_Read_HelpContext(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TWinControl(Args.Obj).HelpContext;
end;

{ property Write HelpContext(Value: THelpContext) }

procedure TWinControl_Write_HelpContext(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TWinControl(Args.Obj).HelpContext := Value;
end;

{ TGraphicControl }

{ constructor Create(AOwner: TComponent) }

procedure TGraphicControl_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TGraphicControl.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TCustomControl }

{ constructor Create(AOwner: TComponent) }

procedure TCustomControl_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomControl.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TCustomImageList }

{ constructor Create(AOwner: TComponent) }

procedure TCustomImageList_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomImageList.Create(V2O(Args.Values[0]) as TComponent));
end;

{ constructor CreateSize(AWidth: Integer; AHeight: Integer) }

procedure TCustomImageList_CreateSize(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomImageList.CreateSize(Args.Values[0], Args.Values[1]));
end;

{ procedure Assign(Source: TPersistent); }

procedure TCustomImageList_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomImageList(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ function Add(Image, Mask: TBitmap): Integer; }

procedure TCustomImageList_Add(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomImageList(Args.Obj).Add(V2O(Args.Values[0]) as TBitmap, V2O(Args.Values[1]) as TBitmap);
end;

{ function AddIcon(Image: TIcon): Integer; }

{$IFDEF VCL}
procedure TCustomImageList_AddIcon(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomImageList(Args.Obj).AddIcon(V2O(Args.Values[0]) as TIcon);
end;
{$ENDIF VCL}

{ procedure AddImages(Value: TCustomImageList); }

procedure TCustomImageList_AddImages(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomImageList(Args.Obj).AddImages(V2O(Args.Values[0]) as TCustomImageList);
end;

{ function AddMasked(Image: TBitmap; MaskColor: TColor): Integer; }

procedure TCustomImageList_AddMasked(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomImageList(Args.Obj).AddMasked(V2O(Args.Values[0]) as TBitmap, Args.Values[1]);
end;

{ procedure Clear; }

procedure TCustomImageList_Clear(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomImageList(Args.Obj).Clear;
end;

{ procedure Delete(Index: Integer); }

procedure TCustomImageList_Delete(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomImageList(Args.Obj).Delete(Args.Values[0]);
end;

{ procedure Draw(Canvas: TCanvas; X, Y, Index: Integer); }

procedure TCustomImageList_Draw(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomImageList(Args.Obj).Draw(V2O(Args.Values[0]) as TCanvas, Args.Values[1], Args.Values[2], Args.Values[3]);
end;

{$IFDEF VCL}

{ procedure DrawOverlay(Canvas: TCanvas; X, Y: Integer; ImageIndex: Integer; Overlay: TOverlay); }

procedure TCustomImageList_DrawOverlay(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomImageList(Args.Obj).DrawOverlay(V2O(Args.Values[0]) as TCanvas, Args.Values[1], Args.Values[2], Args.Values[3],
    Args.Values[4]);
end;

{ function FileLoad(ResType: TResType; Name: string; MaskColor: TColor): Boolean; }

procedure TCustomImageList_FileLoad(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomImageList(Args.Obj).FileLoad(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{$ENDIF VCL}

{ procedure GetBitmap(Index: Integer; Image: TBitmap); }

procedure TCustomImageList_GetBitmap(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomImageList(Args.Obj).GetBitmap(Args.Values[0], V2O(Args.Values[1]) as TBitmap);
end;

{$IFDEF VCL}

{ function GetHotSpot: TPoint; }

procedure TCustomImageList_GetHotSpot(var Value: Variant; Args: TJvInterpreterArgs);
begin
  JvInterpreterVarCopy(Value, Point2Var(TCustomImageList(Args.Obj).GetHotSpot));
end;

{ procedure GetIcon(Index: Integer; Image: TIcon); }

procedure TCustomImageList_GetIcon(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomImageList(Args.Obj).GetIcon(Args.Values[0], V2O(Args.Values[1]) as TIcon);
end;

{ function GetImageBitmap: HBITMAP; }

procedure TCustomImageList_GetImageBitmap(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TCustomImageList(Args.Obj).GetImageBitmap);
end;

{ function GetMaskBitmap: HBITMAP; }

procedure TCustomImageList_GetMaskBitmap(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TCustomImageList(Args.Obj).GetMaskBitmap);
end;

{$ENDIF VCL}

{ function GetResource(ResType: TResType; Name: string; Width: Integer; LoadFlags: TLoadResources; MaskColor: TColor): Boolean; }

procedure TCustomImageList_GetResource(var Value: Variant; Args: TJvInterpreterArgs);
begin
//  Value := TCustomImageList(Args.Obj).GetResource(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3], Args.Values[4]);
end;

{ function GetInstRes(Instance: THandle; ResType: TResType; Name: string; Width: Integer; LoadFlags: TLoadResources; MaskColor: TColor): Boolean; }

procedure TCustomImageList_GetInstRes(var Value: Variant; Args: TJvInterpreterArgs);
begin
//  Value := TCustomImageList(Args.Obj).GetInstRes(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3], Args.Values[4], Args.Values[5]);
end;

{  function HandleAllocated: Boolean; }

{$IFDEF VCL}
procedure TCustomImageList_HandleAllocated(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomImageList(Args.Obj).HandleAllocated;
end;
{$ENDIF VCL}

{ procedure Insert(Index: Integer; Image, Mask: TBitmap); }

procedure TCustomImageList_Insert(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomImageList(Args.Obj).Insert(Args.Values[0], V2O(Args.Values[1]) as TBitmap, V2O(Args.Values[2]) as TBitmap);
end;

{ procedure InsertIcon(Index: Integer; Image: TIcon); }

{$IFDEF VCL}
procedure TCustomImageList_InsertIcon(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomImageList(Args.Obj).InsertIcon(Args.Values[0], V2O(Args.Values[1]) as TIcon);
end;
{$ENDIF VCL}

{ procedure InsertMasked(Index: Integer; Image: TBitmap; MaskColor: TColor); }

procedure TCustomImageList_InsertMasked(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomImageList(Args.Obj).InsertMasked(Args.Values[0], V2O(Args.Values[1]) as TBitmap, Args.Values[2]);
end;

{ procedure Move(CurIndex, NewIndex: Integer); }

procedure TCustomImageList_Move(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomImageList(Args.Obj).Move(Args.Values[0], Args.Values[1]);
end;

{ function Overlay(ImageIndex: Integer; Overlay: TOverlay): Boolean; }

{$IFDEF VCL}
procedure TCustomImageList_Overlay(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomImageList(Args.Obj).Overlay(Args.Values[0], Args.Values[1]);
end;
{$ENDIF VCL}

{ procedure RegisterChanges(Value: TChangeLink); }

procedure TCustomImageList_RegisterChanges(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomImageList(Args.Obj).RegisterChanges(V2O(Args.Values[0]) as TChangeLink);
end;

{$IFDEF VCL}

{ function ResourceLoad(ResType: TResType; Name: string; MaskColor: TColor): Boolean; }

procedure TCustomImageList_ResourceLoad(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomImageList(Args.Obj).ResourceLoad(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function ResInstLoad(Instance: THandle; ResType: TResType; Name: string; MaskColor: TColor): Boolean; }

procedure TCustomImageList_ResInstLoad(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomImageList(Args.Obj).ResInstLoad(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3]);
end;

{$ENDIF VCL}

{ procedure Replace(Index: Integer; Image, Mask: TBitmap); }

procedure TCustomImageList_Replace(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomImageList(Args.Obj).Replace(Args.Values[0], V2O(Args.Values[1]) as TBitmap, V2O(Args.Values[2]) as TBitmap);
end;

{ procedure ReplaceIcon(Index: Integer; Image: TIcon); }

{$IFDEF VCL}
procedure TCustomImageList_ReplaceIcon(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomImageList(Args.Obj).ReplaceIcon(Args.Values[0], V2O(Args.Values[1]) as TIcon);
end;
{$ENDIF VCL}

{ procedure ReplaceMasked(Index: Integer; NewImage: TBitmap; MaskColor: TColor); }

procedure TCustomImageList_ReplaceMasked(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomImageList(Args.Obj).ReplaceMasked(Args.Values[0], V2O(Args.Values[1]) as TBitmap, Args.Values[2]);
end;

{ procedure UnRegisterChanges(Value: TChangeLink); }

procedure TCustomImageList_UnRegisterChanges(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomImageList(Args.Obj).UnRegisterChanges(V2O(Args.Values[0]) as TChangeLink);
end;

{ property Read Count: Integer }

procedure TCustomImageList_Read_Count(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomImageList(Args.Obj).Count;
end;

{$IFDEF VCL}

{ property Read Handle: HImageList }

procedure TCustomImageList_Read_Handle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TCustomImageList(Args.Obj).Handle);
end;

{ property Write Handle(Value: HImageList) }

procedure TCustomImageList_Write_Handle(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomImageList(Args.Obj).Handle := Value;
end;

{$ENDIF VCL}

{$IFDEF VCL}

{ property Read DragCursor: TCursor }

procedure TCustomImageList_Read_DragCursor(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TImageList(Args.Obj).DragCursor;
end;

{ property Write DragCursor(Value: TCursor) }

procedure TCustomImageList_Write_DragCursor(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TImageList(Args.Obj).DragCursor := Value;
end;

{ property Read Dragging: Boolean }

procedure TCustomImageList_Read_Dragging(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TImageList(Args.Obj).Dragging;
end;

{ function BeginDrag(Window: HWND; X, Y: Integer): Boolean; }

procedure TCustomImageList_BeginDrag(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TImageList(Args.Obj).BeginDrag(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function DragLock(Window: HWND; XPos, YPos: Integer): Boolean; }

procedure TCustomImageList_DragLock(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TImageList(Args.Obj).DragLock(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function DragMove(X, Y: Integer): Boolean; }

procedure TCustomImageList_DragMove(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TImageList(Args.Obj).DragMove(Args.Values[0], Args.Values[1]);
end;

{ procedure DragUnlock; }

procedure TCustomImageList_DragUnlock(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TImageList(Args.Obj).DragUnlock;
end;

{ function EndDrag: Boolean; }

procedure TCustomImageList_EndDrag(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TImageList(Args.Obj).EndDrag;
end;

{ procedure HideDragImage; }

procedure TCustomImageList_HideDragImage(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TImageList(Args.Obj).HideDragImage;
end;

{ function SetDragImage(Index, HotSpotX, HotSpotY: Integer): Boolean; }

procedure TCustomImageList_SetDragImage(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TImageList(Args.Obj).SetDragImage(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ procedure ShowDragImage; }

procedure TCustomImageList_ShowDragImage(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TImageList(Args.Obj).ShowDragImage;
end;

{$ENDIF VCL}

{ hack section }

type
  THackControl = class(TControl);
  THackWinControl = class(TWinControl);
  THackCustomControl = class(TCustomControl);

{ THackControl }

{ property Read Caption: TCaption }

procedure THackControl_Read_Caption(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := THackControl(Args.Obj).Caption;
end;

{ property Write Caption(Value: TCaption) }

procedure THackControl_Write_Caption(const Value: Variant; Args: TJvInterpreterArgs);
begin
  THackControl(Args.Obj).Caption := Value;
end;

{ property Read Color: TColor }

procedure THackControl_Read_Color(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := THackControl(Args.Obj).Color;
end;

{ property Write Color(Value: TColor) }

procedure THackControl_Write_Color(const Value: Variant; Args: TJvInterpreterArgs);
begin
  THackControl(Args.Obj).Color := Value;
end;

{$IFDEF VCL}

{ property Read DesktopFont: Boolean }

procedure THackControl_Read_DesktopFont(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := THackControl(Args.Obj).DesktopFont;
end;

{ property Write DesktopFont(Value: Boolean) }

procedure THackControl_Write_DesktopFont(const Value: Variant; Args: TJvInterpreterArgs);
begin
  THackControl(Args.Obj).DesktopFont := Value;
end;

{ property Read DragCursor: TCursor }

procedure THackControl_Read_DragCursor(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := THackControl(Args.Obj).DragCursor;
end;

{ property Write DragCursor(Value: TCursor) }

procedure THackControl_Write_DragCursor(const Value: Variant; Args: TJvInterpreterArgs);
begin
  THackControl(Args.Obj).DragCursor := Value;
end;

{$ENDIF VCL}

{ property Read DragMode: TDragMode }

procedure THackControl_Read_DragMode(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := THackControl(Args.Obj).DragMode;
end;

{ property Write DragMode(Value: TDragMode) }

procedure THackControl_Write_DragMode(const Value: Variant; Args: TJvInterpreterArgs);
begin
  THackControl(Args.Obj).DragMode := Value;
end;

{ property Read Font: TFont }

procedure THackControl_Read_Font(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(THackControl(Args.Obj).Font);
end;

{ property Write Font(Value: TFont) }

procedure THackControl_Write_Font(const Value: Variant; Args: TJvInterpreterArgs);
begin
  THackControl(Args.Obj).Font := V2O(Value) as TFont;
end;

{$IFDEF VCL}

{ property Read IsControl: Boolean }

procedure THackControl_Read_IsControl(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := THackControl(Args.Obj).IsControl;
end;

{ property Write IsControl(Value: Boolean) }

procedure THackControl_Write_IsControl(const Value: Variant; Args: TJvInterpreterArgs);
begin
  THackControl(Args.Obj).IsControl := Value;
end;

{$ENDIF VCL}

{ property Read MouseCapture: Boolean }

procedure THackControl_Read_MouseCapture(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := THackControl(Args.Obj).MouseCapture;
end;

{ property Write MouseCapture(Value: Boolean) }

procedure THackControl_Write_MouseCapture(const Value: Variant; Args: TJvInterpreterArgs);
begin
  THackControl(Args.Obj).MouseCapture := Value;
end;

{ property Read ParentColor: Boolean }

procedure THackControl_Read_ParentColor(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := THackControl(Args.Obj).ParentColor;
end;

{ property Write ParentColor(Value: Boolean) }

procedure THackControl_Write_ParentColor(const Value: Variant; Args: TJvInterpreterArgs);
begin
  THackControl(Args.Obj).ParentColor := Value;
end;

{ property Read ParentFont: Boolean }

procedure THackControl_Read_ParentFont(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := THackControl(Args.Obj).ParentFont;
end;

{ property Write ParentFont(Value: Boolean) }

procedure THackControl_Write_ParentFont(const Value: Variant; Args: TJvInterpreterArgs);
begin
  THackControl(Args.Obj).ParentFont := Value;
end;

{ property Read ParentShowHint: Boolean }

procedure THackControl_Read_ParentShowHint(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := THackControl(Args.Obj).ParentShowHint;
end;

{ property Write ParentShowHint(Value: Boolean) }

procedure THackControl_Write_ParentShowHint(const Value: Variant; Args: TJvInterpreterArgs);
begin
  THackControl(Args.Obj).ParentShowHint := Value;
end;

{ property Read PopupMenu: TPopupMenu }

procedure THackControl_Read_PopupMenu(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(THackControl(Args.Obj).PopupMenu);
end;

{ property Write PopupMenu(Value: TPopupMenu) }

procedure THackControl_Write_PopupMenu(const Value: Variant; Args: TJvInterpreterArgs);
begin
  THackControl(Args.Obj).PopupMenu := V2O(Value) as TPopupMenu;
end;

{ property Read Text: TCaption }

procedure THackControl_Read_Text(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := THackControl(Args.Obj).Text;
end;

{ property Write Text(Value: TCaption) }

procedure THackControl_Write_Text(const Value: Variant; Args: TJvInterpreterArgs);
begin
  THackControl(Args.Obj).Text := Value;
end;

{$IFDEF VCL}

{ property Read WindowText: PChar }

procedure THackControl_Read_WindowText(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := string(THackControl(Args.Obj).WindowText);
end;

{ property Write WindowText(Value: PChar) }

procedure THackControl_Write_WindowText(const Value: Variant; Args: TJvInterpreterArgs);
begin
  THackControl(Args.Obj).WindowText := PChar(string(Value));
end;

{$ENDIF VCL}

{ THackWinControl }

{$IFDEF VCL}

{ property Read Ctl3D: Boolean }

procedure THackWinControl_Read_Ctl3D(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := THackWinControl(Args.Obj).Ctl3D;
end;

{ property Write Ctl3D(Value: Boolean) }

procedure THackWinControl_Write_Ctl3D(const Value: Variant; Args: TJvInterpreterArgs);
begin
  THackWinControl(Args.Obj).Ctl3D := Value;
end;

{ property Read DefWndProc: Pointer }

procedure THackWinControl_Read_DefWndProc(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := P2V(THackWinControl(Args.Obj).DefWndProc);
end;

{ property Write DefWndProc(Value: Pointer) }

procedure THackWinControl_Write_DefWndProc(const Value: Variant; Args: TJvInterpreterArgs);
begin
  THackWinControl(Args.Obj).DefWndProc := V2P(Value);
end;

{$ENDIF VCL}

{$IFDEF VCL}
{ property Read ImeMode: TImeMode }

procedure THackWinControl_Read_ImeMode(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := THackWinControl(Args.Obj).ImeMode;
end;

{ property Write ImeMode(Value: TImeMode) }

procedure THackWinControl_Write_ImeMode(const Value: Variant; Args: TJvInterpreterArgs);
begin
  THackWinControl(Args.Obj).ImeMode := Value;
end;

{ property Read ImeName: TImeName }

procedure THackWinControl_Read_ImeName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := THackWinControl(Args.Obj).ImeName;
end;

{ property Write ImeName(Value: TImeName) }

procedure THackWinControl_Write_ImeName(const Value: Variant; Args: TJvInterpreterArgs);
begin
  THackWinControl(Args.Obj).ImeName := Value;
end;

{ property Read ParentCtl3D: Boolean }

procedure THackWinControl_Read_ParentCtl3D(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := THackWinControl(Args.Obj).ParentCtl3D;
end;

{ property Write ParentCtl3D(Value: Boolean) }

procedure THackWinControl_Write_ParentCtl3D(const Value: Variant; Args: TJvInterpreterArgs);
begin
  THackWinControl(Args.Obj).ParentCtl3D := Value;
end;

{ property Read WindowHandle: HWnd }

procedure THackWinControl_Read_WindowHandle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(THackWinControl(Args.Obj).WindowHandle);
end;

{ property Write WindowHandle(Value: HWnd) }

procedure THackWinControl_Write_WindowHandle(const Value: Variant; Args: TJvInterpreterArgs);
begin
  THackWinControl(Args.Obj).WindowHandle := Value;
end;

{$ENDIF VCL}

{ THackCustomControl }

{ procedure Paint; }

procedure THackCustomControl_Paint(var Value: Variant; Args: TJvInterpreterArgs);
begin
  THackCustomControl(Args.Obj).Paint;
end;

{ procedure PaintWindow(DC: HDC); }

{$IFDEF VCL}
procedure THackCustomControl_PaintWindow(var Value: Variant; Args: TJvInterpreterArgs);
begin
  THackCustomControl(Args.Obj).PaintWindow(Args.Values[0]);
end;
{$ENDIF VCL}

{ property Read Canvas: TCanvas }

procedure THackCustomControl_Read_Canvas(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(THackCustomControl(Args.Obj).Canvas);
end;

{ functions }

{ function IsDragObject(Sender: TObject): Boolean; }

{$IFDEF VCL}
procedure JvInterpreter_IsDragObject(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := IsDragObject(V2O(Args.Values[0]));
end;
{$ENDIF VCL}

{ function FindControl(Handle: HWnd): TWinControl; }

procedure JvInterpreter_FindControl(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF VCL}
  Value := O2V(FindControl(Args.Values[0]));
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Value := O2V(FindControl(QWidgetH(V2O(Args.Values[0]))));
  {$ENDIF VisualCLX}
end;

{ function FindVCLWindow(const Pos: TPoint): TWinControl; }

{$IFDEF VCL}
procedure JvInterpreter_FindVCLWindow(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(FindVCLWindow(Var2Point(Args.Values[0])));
end;
{$ENDIF VCL}

{ function FindDragTarget(const Pos: TPoint; AllowDisabled: Boolean): TControl; }

procedure JvInterpreter_FindDragTarget(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(FindDragTarget(Var2Point(Args.Values[0]), Args.Values[1]));
end;

{ function GetCaptureControl: TControl; }

procedure JvInterpreter_GetCaptureControl(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(GetCaptureControl);
end;

{ procedure SetCaptureControl(Control: TControl); }

procedure JvInterpreter_SetCaptureControl(var Value: Variant; Args: TJvInterpreterArgs);
begin
  SetCaptureControl(V2O(Args.Values[0]) as TControl);
end;

{ procedure CancelDrag; }

{$IFDEF VCL}
procedure JvInterpreter_CancelDrag(var Value: Variant; Args: TJvInterpreterArgs);
begin
  CancelDrag;
end;
{$ENDIF VCL}

{ function CursorToString(Cursor: TCursor): string; }

procedure JvInterpreter_CursorToString(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := CursorToString(Args.Values[0]);
end;

{ function StringToCursor(const S: string): TCursor; }

procedure JvInterpreter_StringToCursor(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := StringToCursor(Args.Values[0]);
end;

{ function CursorToIdent(Cursor: Longint; var Ident: string): Boolean; }

procedure JvInterpreter_CursorToIdent(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := CursorToIdent(Args.Values[0], string(TVarData(Args.Values[1]).vString));
end;

{ function IdentToCursor(const Ident: string; var Cursor: Longint): Boolean; }

procedure JvInterpreter_IdentToCursor(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := IdentToCursor(Args.Values[0], longint(TVarData(Args.Values[1]).vInteger));
end;

{ function GetShortHint(const Hint: string): string; }

procedure JvInterpreter_GetShortHint(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := GetShortHint(Args.Values[0]);
end;

{ function GetLongHint(const Hint: string): string; }

procedure JvInterpreter_GetLongHint(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := GetLongHint(Args.Values[0]);
end;

{$IFDEF VCL}

{ function InitWndProc(HWindow: HWnd; Message, WParam: Longint; LParam: Longint): Longint; }

procedure JvInterpreter_InitWndProc(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := InitWndProc(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3]);
end;

{ function SendAppMessage(Msg: Cardinal; WParam, LParam: Longint): Longint; }

procedure JvInterpreter_SendAppMessage(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := SendAppMessage(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ procedure MoveWindowOrg(DC: HDC; DX, DY: Integer); }

procedure JvInterpreter_MoveWindowOrg(var Value: Variant; Args: TJvInterpreterArgs);
begin
  MoveWindowOrg(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{$ENDIF VCL}

type
  TJvInterpreterControlsEvent = class(TJvInterpreterEvent)
  private
    procedure MouseEvent(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMoveEvent(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure KeyEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure DragOverEvent(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure DragDropEvent(Sender, Source: TObject; X, Y: Integer);
    procedure StartDragEvent(Sender: TObject; var DragObject: TDragObject);
    procedure EndDragEvent(Sender, Target: TObject; X, Y: Integer);
  end;

procedure TJvInterpreterControlsEvent.MouseEvent(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  CallFunction(nil, [O2V(Sender), Button, S2V(Byte(Shift)), X, Y]);
end;

procedure TJvInterpreterControlsEvent.MouseMoveEvent(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  CallFunction(nil, [O2V(Sender), S2V(Byte(Shift)), X, Y]);
end;

procedure TJvInterpreterControlsEvent.KeyEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  CallFunction(nil, [O2V(Sender), Key, S2V(Byte(Shift))]);
  Key := Args.Values[1];
end;

procedure TJvInterpreterControlsEvent.KeyPressEvent(Sender: TObject; var Key: Char);
begin
  CallFunction(nil, [O2V(Sender), Key]);
  Key := string(Args.Values[1])[1];
end;

procedure TJvInterpreterControlsEvent.DragOverEvent(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  CallFunction(nil, [O2V(Sender), O2V(Source), X, Y, S2V(Byte(State)), Accept]);
  Accept := Args.Values[5];
end;

procedure TJvInterpreterControlsEvent.DragDropEvent(Sender, Source: TObject; X, Y: Integer);
begin
  CallFunction(nil, [O2V(Sender), O2V(Source), X, Y]);
end;

procedure TJvInterpreterControlsEvent.StartDragEvent(Sender: TObject; var DragObject: TDragObject);
begin
  CallFunction(nil, [O2V(Sender), O2V(DragObject)]);
  DragObject := V2O(Args.Values[1]) as TDragObject;
end;

procedure TJvInterpreterControlsEvent.EndDragEvent(Sender, Target: TObject; X, Y: Integer);
begin
  CallFunction(nil, [O2V(Sender), O2V(Target), X, Y]);
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  cControls = 'Controls';
begin
  with JvInterpreterAdapter do
  begin
    {$IFDEF VCL}
    { TDragMessage }
    AddConst(cControls, 'dmDragEnter', Ord(dmDragEnter));
    AddConst(cControls, 'dmDragLeave', Ord(dmDragLeave));
    AddConst(cControls, 'dmDragMove', Ord(dmDragMove));
    AddConst(cControls, 'dmDragDrop', Ord(dmDragDrop));
    AddConst(cControls, 'dmDragCancel', Ord(dmDragCancel));
    AddConst(cControls, 'dmFindTarget', Ord(dmFindTarget));
    {$ENDIF VCL}
    { TAlign }
    AddConst(cControls, 'alNone', Ord(alNone));
    AddConst(cControls, 'alTop', Ord(alTop));
    AddConst(cControls, 'alBottom', Ord(alBottom));
    AddConst(cControls, 'alLeft', Ord(alLeft));
    AddConst(cControls, 'alRight', Ord(alRight));
    AddConst(cControls, 'alClient', Ord(alClient));
    { TControlState }
    AddConst(cControls, 'csLButtonDown', Ord(csLButtonDown));
    AddConst(cControls, 'csClicked', Ord(csClicked));
    AddConst(cControls, 'csPalette', Ord(csPalette));
    AddConst(cControls, 'csReadingState', Ord(csReadingState));
    AddConst(cControls, 'csAlignmentNeeded', Ord(csAlignmentNeeded));
    AddConst(cControls, 'csFocusing', Ord(csFocusing));
    AddConst(cControls, 'csCreating', Ord(csCreating));
    AddConst(cControls, 'csPaintCopy', Ord(csPaintCopy));
    { TControlStyle }
    AddConst(cControls, 'csAcceptsControls', Ord(csAcceptsControls));
    AddConst(cControls, 'csCaptureMouse', Ord(csCaptureMouse));
    AddConst(cControls, 'csDesignInteractive', Ord(csDesignInteractive));
    AddConst(cControls, 'csClickEvents', Ord(csClickEvents));
    AddConst(cControls, 'csFramed', Ord(csFramed));
    AddConst(cControls, 'csSetCaption', Ord(csSetCaption));
    AddConst(cControls, 'csOpaque', Ord(csOpaque));
    AddConst(cControls, 'csDoubleClicks', Ord(csDoubleClicks));
    AddConst(cControls, 'csFixedWidth', Ord(csFixedWidth));
    AddConst(cControls, 'csFixedHeight', Ord(csFixedHeight));
    AddConst(cControls, 'csNoDesignVisible', Ord(csNoDesignVisible));
    AddConst(cControls, 'csReplicatable', Ord(csReplicatable));
    AddConst(cControls, 'csNoStdEvents', Ord(csNoStdEvents));
    AddConst(cControls, 'csDisplayDragImage', Ord(csDisplayDragImage));
    {$IFDEF VCL}
    AddConst(cControls, 'csReflector', Ord(csReflector));
    {$ENDIF VCL}
    AddConst(cControls, 'csActionClient', Ord(csActionClient));
    AddConst(cControls, 'csMenuEvents', Ord(csMenuEvents));
    { TMouseButton }
    AddConst(cControls, 'mbLeft', Ord(mbLeft));
    AddConst(cControls, 'mbRight', Ord(mbRight));
    AddConst(cControls, 'mbMiddle', Ord(mbMiddle));
    { TDragMode }
    AddConst(cControls, 'dmManual', Ord(dmManual));
    AddConst(cControls, 'dmAutomatic', Ord(dmAutomatic));
    { TDragState }
    AddConst(cControls, 'dsDragEnter', Ord(dsDragEnter));
    AddConst(cControls, 'dsDragLeave', Ord(dsDragLeave));
    AddConst(cControls, 'dsDragMove', Ord(dsDragMove));
    { TScalingFlags }
    AddConst(cControls, 'sfLeft', Ord(sfLeft));
    AddConst(cControls, 'sfTop', Ord(sfTop));
    AddConst(cControls, 'sfWidth', Ord(sfWidth));
    AddConst(cControls, 'sfHeight', Ord(sfHeight));
    AddConst(cControls, 'sfFont', Ord(sfFont));
    { TControl }
    AddClass(cControls, TControl, 'TControl');
    AddGet(TControl, 'Create', TControl_Create, 1, [varEmpty], varEmpty);
    AddGet(TControl, 'BeginDrag', TControl_BeginDrag, 1, [varEmpty], varEmpty);
    AddGet(TControl, 'BringToFront', TControl_BringToFront, 0, [varEmpty], varEmpty);
    AddGet(TControl, 'ClientToScreen', TControl_ClientToScreen, 1, [varEmpty], varEmpty);
    AddGet(TControl, 'Dragging', TControl_Dragging, 0, [varEmpty], varEmpty);
    AddGet(TControl, 'DragDrop', TControl_DragDrop, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TControl, 'EndDrag', TControl_EndDrag, 1, [varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TControl, 'GetTextBuf', TControl_GetTextBuf, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TControl, 'GetTextLen', TControl_GetTextLen, 0, [varEmpty], varEmpty);
    {$ENDIF VCL}
    AddGet(TControl, 'Hide', TControl_Hide, 0, [varEmpty], varEmpty);
    AddGet(TControl, 'Invalidate', TControl_Invalidate, 0, [varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TControl, 'Perform', TControl_Perform, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    {$ENDIF VCL}
    AddGet(TControl, 'Refresh', TControl_Refresh, 0, [varEmpty], varEmpty);
    AddGet(TControl, 'Repaint', TControl_Repaint, 0, [varEmpty], varEmpty);
    AddGet(TControl, 'ScreenToClient', TControl_ScreenToClient, 1, [varEmpty], varEmpty);
    AddGet(TControl, 'SendToBack', TControl_SendToBack, 0, [varEmpty], varEmpty);
    AddGet(TControl, 'SetBounds', TControl_SetBounds, 4, [varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TControl, 'SetTextBuf', TControl_SetTextBuf, 1, [varEmpty], varEmpty);
    {$ENDIF VCL}
    AddGet(TControl, 'Show', TControl_Show, 0, [varEmpty], varEmpty);
    AddGet(TControl, 'Update', TControl_Update, 0, [varEmpty], varEmpty);
    AddGet(TControl, 'Align', TControl_Read_Align, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'Align', TControl_Write_Align, 0, [varEmpty]);
    AddGet(TControl, 'BoundsRect', TControl_Read_BoundsRect, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'BoundsRect', TControl_Write_BoundsRect, 0, [varEmpty]);
    AddGet(TControl, 'ClientHeight', TControl_Read_ClientHeight, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'ClientHeight', TControl_Write_ClientHeight, 0, [varEmpty]);
    AddGet(TControl, 'ClientOrigin', TControl_Read_ClientOrigin, 0, [varEmpty], varEmpty);
    AddGet(TControl, 'ClientRect', TControl_Read_ClientRect, 0, [varEmpty], varEmpty);
    AddGet(TControl, 'ClientWidth', TControl_Read_ClientWidth, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'ClientWidth', TControl_Write_ClientWidth, 0, [varEmpty]);
    AddGet(TControl, 'ControlState', TControl_Read_ControlState, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'ControlState', TControl_Write_ControlState, 0, [varEmpty]);
    AddGet(TControl, 'ControlStyle', TControl_Read_ControlStyle, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'ControlStyle', TControl_Write_ControlStyle, 0, [varEmpty]);
    AddGet(TControl, 'Parent', TControl_Read_Parent, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'Parent', TControl_Write_Parent, 0, [varEmpty]);
    AddGet(TControl, 'ShowHint', TControl_Read_ShowHint, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'ShowHint', TControl_Write_ShowHint, 0, [varEmpty]);
    AddGet(TControl, 'Visible', TControl_Read_Visible, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'Visible', TControl_Write_Visible, 0, [varEmpty]);
    AddGet(TControl, 'Enabled', TControl_Read_Enabled, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'Enabled', TControl_Write_Enabled, 0, [varEmpty]);
    AddGet(TControl, 'WindowProc', TControl_Read_WindowProc, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'WindowProc', TControl_Write_WindowProc, 0, [varEmpty]);
    AddGet(TControl, 'Left', TControl_Read_Left, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'Left', TControl_Write_Left, 0, [varEmpty]);
    AddGet(TControl, 'Top', TControl_Read_Top, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'Top', TControl_Write_Top, 0, [varEmpty]);
    AddGet(TControl, 'Width', TControl_Read_Width, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'Width', TControl_Write_Width, 0, [varEmpty]);
    AddGet(TControl, 'Height', TControl_Read_Height, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'Height', TControl_Write_Height, 0, [varEmpty]);
    AddGet(TControl, 'Cursor', TControl_Read_Cursor, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'Cursor', TControl_Write_Cursor, 0, [varEmpty]);
    AddGet(TControl, 'Hint', TControl_Read_Hint, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'Hint', TControl_Write_Hint, 0, [varEmpty]);
    {$IFDEF VCL}
    { TImeMode }
    AddConst(cControls, 'imDisable', Ord(imDisable));
    AddConst(cControls, 'imClose', Ord(imClose));
    AddConst(cControls, 'imOpen', Ord(imOpen));
    AddConst(cControls, 'imDontCare', Ord(imDontCare));
    AddConst(cControls, 'imSAlpha', Ord(imSAlpha));
    AddConst(cControls, 'imAlpha', Ord(imAlpha));
    AddConst(cControls, 'imHira', Ord(imHira));
    AddConst(cControls, 'imSKata', Ord(imSKata));
    AddConst(cControls, 'imKata', Ord(imKata));
    AddConst(cControls, 'imChinese', Ord(imChinese));
    AddConst(cControls, 'imSHanguel', Ord(imSHanguel));
    AddConst(cControls, 'imHanguel', Ord(imHanguel));
    {$ENDIF VCL}
    { TWinControl }
    AddClass(cControls, TWinControl, 'TWinControl');
    AddGet(TWinControl, 'Create', TWinControl_Create, 1, [varEmpty], varEmpty);
    AddGet(TWinControl, 'CreateParented', TWinControl_CreateParented, 1, [varEmpty], varEmpty);
    AddGet(TWinControl, 'Broadcast', TWinControl_Broadcast, 1, [varByRef], varEmpty);
    AddGet(TWinControl, 'CanFocus', TWinControl_CanFocus, 0, [varEmpty], varEmpty);
    AddGet(TWinControl, 'ContainsControl', TWinControl_ContainsControl, 1, [varEmpty], varEmpty);
    AddGet(TWinControl, 'ControlAtPos', TWinControl_ControlAtPos, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TWinControl, 'DisableAlign', TWinControl_DisableAlign, 0, [varEmpty], varEmpty);
    AddGet(TWinControl, 'EnableAlign', TWinControl_EnableAlign, 0, [varEmpty], varEmpty);
    AddGet(TWinControl, 'Focused', TWinControl_Focused, 0, [varEmpty], varEmpty);
    AddGet(TWinControl, 'GetTabOrderList', TWinControl_GetTabOrderList, 1, [varEmpty], varEmpty);
    AddGet(TWinControl, 'HandleAllocated', TWinControl_HandleAllocated, 0, [varEmpty], varEmpty);
    AddGet(TWinControl, 'HandleNeeded', TWinControl_HandleNeeded, 0, [varEmpty], varEmpty);
    AddGet(TWinControl, 'InsertControl', TWinControl_InsertControl, 1, [varEmpty], varEmpty);
    AddGet(TWinControl, 'Invalidate', TWinControl_Invalidate, 0, [varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TWinControl, 'PaintTo', TWinControl_PaintTo, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    {$ENDIF VCL}
    AddGet(TWinControl, 'RemoveControl', TWinControl_RemoveControl, 1, [varEmpty], varEmpty);
    AddGet(TWinControl, 'Realign', TWinControl_Realign, 0, [varEmpty], varEmpty);
    AddGet(TWinControl, 'Repaint', TWinControl_Repaint, 0, [varEmpty], varEmpty);
    AddGet(TWinControl, 'ScaleBy', TWinControl_ScaleBy, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TWinControl, 'ScrollBy', TWinControl_ScrollBy, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TWinControl, 'SetBounds', TWinControl_SetBounds, 4, [varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TWinControl, 'SetFocus', TWinControl_SetFocus, 0, [varEmpty], varEmpty);
    AddGet(TWinControl, 'Update', TWinControl_Update, 0, [varEmpty], varEmpty);
    AddGet(TWinControl, 'UpdateControlState', TWinControl_UpdateControlState, 0, [varEmpty], varEmpty);
    AddGet(TWinControl, 'Brush', TWinControl_Read_Brush, 0, [varEmpty], varEmpty);
    AddIGet(TWinControl, 'Controls', TWinControl_Read_Controls, 1, [varEmpty], varEmpty);
    AddGet(TWinControl, 'ControlCount', TWinControl_Read_ControlCount, 0, [varEmpty], varEmpty);
    AddGet(TWinControl, 'Handle', TWinControl_Read_Handle, 0, [varEmpty], varEmpty);
    AddGet(TWinControl, 'ParentWindow', TWinControl_Read_ParentWindow, 0, [varEmpty], varEmpty);
    AddSet(TWinControl, 'ParentWindow', TWinControl_Write_ParentWindow, 0, [varEmpty]);
    AddGet(TWinControl, 'ParentWidget', TWinControl_Read_ParentWindow, 0, [varEmpty], varEmpty);
    AddSet(TWinControl, 'ParentWidget', TWinControl_Write_ParentWindow, 0, [varEmpty]);
    AddGet(TWinControl, 'Showing', TWinControl_Read_Showing, 0, [varEmpty], varEmpty);
    AddGet(TWinControl, 'TabOrder', TWinControl_Read_TabOrder, 0, [varEmpty], varEmpty);
    AddSet(TWinControl, 'TabOrder', TWinControl_Write_TabOrder, 0, [varEmpty]);
    AddGet(TWinControl, 'TabStop', TWinControl_Read_TabStop, 0, [varEmpty], varEmpty);
    AddSet(TWinControl, 'TabStop', TWinControl_Write_TabStop, 0, [varEmpty]);
    AddGet(TWinControl, 'HelpContext', TWinControl_Read_HelpContext, 0, [varEmpty], varEmpty);
    AddSet(TWinControl, 'HelpContext', TWinControl_Write_HelpContext, 0, [varEmpty]);
    { TGraphicControl }
    AddClass(cControls, TGraphicControl, 'TGraphicControl');
    AddGet(TGraphicControl, 'Create', TGraphicControl_Create, 1, [varEmpty], varEmpty);
    { TCustomControl }
    AddClass(cControls, TCustomControl, 'TCustomControl');
    AddGet(TCustomControl, 'Create', TCustomControl_Create, 1, [varEmpty], varEmpty);

    AddGet(TControl, 'Caption', THackControl_Read_Caption, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'Caption', THackControl_Write_Caption, 0, [varEmpty]);
    AddGet(TControl, 'Color', THackControl_Read_Color, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'Color', THackControl_Write_Color, 0, [varEmpty]);
    {$IFDEF VCL}
    AddGet(TControl, 'DesktopFont', THackControl_Read_DesktopFont, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'DesktopFont', THackControl_Write_DesktopFont, 0, [varEmpty]);
    AddGet(TControl, 'DragCursor', THackControl_Read_DragCursor, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'DragCursor', THackControl_Write_DragCursor, 0, [varEmpty]);
    {$ENDIF VCL}
    AddGet(TControl, 'DragMode', THackControl_Read_DragMode, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'DragMode', THackControl_Write_DragMode, 0, [varEmpty]);
    AddGet(TControl, 'Font', THackControl_Read_Font, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'Font', THackControl_Write_Font, 0, [varEmpty]);
    {$IFDEF VCL}
    AddGet(TControl, 'IsControl', THackControl_Read_IsControl, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'IsControl', THackControl_Write_IsControl, 0, [varEmpty]);
    {$ENDIF VCL}
    AddGet(TControl, 'MouseCapture', THackControl_Read_MouseCapture, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'MouseCapture', THackControl_Write_MouseCapture, 0, [varEmpty]);
    AddGet(TControl, 'ParentColor', THackControl_Read_ParentColor, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'ParentColor', THackControl_Write_ParentColor, 0, [varEmpty]);
    AddGet(TControl, 'ParentFont', THackControl_Read_ParentFont, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'ParentFont', THackControl_Write_ParentFont, 0, [varEmpty]);
    AddGet(TControl, 'ParentShowHint', THackControl_Read_ParentShowHint, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'ParentShowHint', THackControl_Write_ParentShowHint, 0, [varEmpty]);
    AddGet(TControl, 'PopupMenu', THackControl_Read_PopupMenu, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'PopupMenu', THackControl_Write_PopupMenu, 0, [varEmpty]);
    AddGet(TControl, 'Text', THackControl_Read_Text, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'Text', THackControl_Write_Text, 0, [varEmpty]);
    {$IFDEF VCL}
    AddGet(TControl, 'WindowText', THackControl_Read_WindowText, 0, [varEmpty], varEmpty);
    AddSet(TControl, 'WindowText', THackControl_Write_WindowText, 0, [varEmpty]);
    AddGet(TWinControl, 'Ctl3D', THackWinControl_Read_Ctl3D, 0, [varEmpty], varEmpty);
    AddSet(TWinControl, 'Ctl3D', THackWinControl_Write_Ctl3D, 0, [varEmpty]);
    AddGet(TWinControl, 'DefWndProc', THackWinControl_Read_DefWndProc, 0, [varEmpty], varEmpty);
    AddSet(TWinControl, 'DefWndProc', THackWinControl_Write_DefWndProc, 0, [varEmpty]);
    AddGet(TWinControl, 'ImeMode', THackWinControl_Read_ImeMode, 0, [varEmpty], varEmpty);
    AddSet(TWinControl, 'ImeMode', THackWinControl_Write_ImeMode, 0, [varEmpty]);
    AddGet(TWinControl, 'ImeName', THackWinControl_Read_ImeName, 0, [varEmpty], varEmpty);
    AddSet(TWinControl, 'ImeName', THackWinControl_Write_ImeName, 0, [varEmpty]);
    AddGet(TWinControl, 'ParentCtl3D', THackWinControl_Read_ParentCtl3D, 0, [varEmpty], varEmpty);
    AddSet(TWinControl, 'ParentCtl3D', THackWinControl_Write_ParentCtl3D, 0, [varEmpty]);
    AddGet(TWinControl, 'WindowHandle', THackWinControl_Read_WindowHandle, 0, [varEmpty], varEmpty);
    AddSet(TWinControl, 'WindowHandle', THackWinControl_Write_WindowHandle, 0, [varEmpty]);
    {$ENDIF VCL}
    { TCustomControl }
    AddClass('IH_Controls', THackCustomControl, 'THackCustomControl');
    AddGet(TCustomControl, 'Paint', THackCustomControl_Paint, 0, [varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TCustomControl, 'PaintWindow', THackCustomControl_PaintWindow, 1, [varEmpty], varEmpty);
    {$ENDIF VCL}
    AddGet(TCustomControl, 'Canvas', THackCustomControl_Read_Canvas, 0, [varEmpty], varEmpty);

    {$IFDEF VCL}
    { TDrawingStyle }
    AddConst(cControls, 'dsFocus', Ord(dsFocus));
    AddConst(cControls, 'dsSelected', Ord(dsSelected));
    AddConst(cControls, 'dsNormal', Ord(dsNormal));
    AddConst(cControls, 'dsTransparent', Ord(dsTransparent));
    {$ENDIF VCL}
    { TImageType }
    AddConst(cControls, 'itImage', Ord(itImage));
    AddConst(cControls, 'itMask', Ord(itMask));
    {$IFDEF VCL}
    { TResType }
    AddConst(cControls, 'rtBitmap', Ord(rtBitmap));
    AddConst(cControls, 'rtCursor', Ord(rtCursor));
    AddConst(cControls, 'rtIcon', Ord(rtIcon));
    { TLoadResource }
    AddConst(cControls, 'lrDefaultColor', Ord(lrDefaultColor));
    AddConst(cControls, 'lrDefaultSize', Ord(lrDefaultSize));
    AddConst(cControls, 'lrFromFile', Ord(lrFromFile));
    AddConst(cControls, 'lrMap3DColors', Ord(lrMap3DColors));
    AddConst(cControls, 'lrTransparent', Ord(lrTransparent));
    AddConst(cControls, 'lrMonoChrome', Ord(lrMonoChrome));
    {$ENDIF VCL}
    { TCustomImageList }
    AddClass(cControls, TCustomImageList, 'TCustomImageList');
    AddGet(TCustomImageList, 'Create', TCustomImageList_Create, 1, [varEmpty], varEmpty);
    AddGet(TCustomImageList, 'CreateSize', TCustomImageList_CreateSize, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TCustomImageList, 'Assign', TCustomImageList_Assign, 1, [varEmpty], varEmpty);
    AddGet(TCustomImageList, 'Add', TCustomImageList_Add, 2, [varEmpty, varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TCustomImageList, 'AddIcon', TCustomImageList_AddIcon, 1, [varEmpty], varEmpty);
    {$ENDIF VCL}
    AddGet(TCustomImageList, 'AddImages', TCustomImageList_AddImages, 1, [varEmpty], varEmpty);
    AddGet(TCustomImageList, 'AddMasked', TCustomImageList_AddMasked, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TCustomImageList, 'Clear', TCustomImageList_Clear, 0, [varEmpty], varEmpty);
    AddGet(TCustomImageList, 'Delete', TCustomImageList_Delete, 1, [varEmpty], varEmpty);
    AddGet(TCustomImageList, 'Draw', TCustomImageList_Draw, 4, [varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TCustomImageList, 'DrawOverlay', TCustomImageList_DrawOverlay, 5, [varEmpty, varEmpty, varEmpty, varEmpty,
      varEmpty], varEmpty);
    AddGet(TCustomImageList, 'FileLoad', TCustomImageList_FileLoad, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    {$ENDIF VCL}
    AddGet(TCustomImageList, 'GetBitmap', TCustomImageList_GetBitmap, 2, [varEmpty, varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TCustomImageList, 'GetHotSpot', TCustomImageList_GetHotSpot, 0, [varEmpty], varEmpty);
    AddGet(TCustomImageList, 'GetIcon', TCustomImageList_GetIcon, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TCustomImageList, 'GetImageBitmap', TCustomImageList_GetImageBitmap, 0, [varEmpty], varEmpty);
    AddGet(TCustomImageList, 'GetMaskBitmap', TCustomImageList_GetMaskBitmap, 0, [varEmpty], varEmpty);
    {$ENDIF VCL}
    AddGet(TCustomImageList, 'GetResource', TCustomImageList_GetResource, 5, [varEmpty, varEmpty, varEmpty, varEmpty,
      varEmpty], varEmpty);
    AddGet(TCustomImageList, 'GetInstRes', TCustomImageList_GetInstRes, 6, [varEmpty, varEmpty, varEmpty, varEmpty,
      varEmpty, varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TCustomImageList, 'HandleAllocated', TCustomImageList_HandleAllocated, 0, [varEmpty], varEmpty);
    {$ENDIF VCL}
    AddGet(TCustomImageList, 'Insert', TCustomImageList_Insert, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TCustomImageList, 'InsertIcon', TCustomImageList_InsertIcon, 2, [varEmpty, varEmpty], varEmpty);
    {$ENDIF VCL}
    AddGet(TCustomImageList, 'InsertMasked', TCustomImageList_InsertMasked, 3, [varEmpty, varEmpty, varEmpty],
      varEmpty);
    AddGet(TCustomImageList, 'Move', TCustomImageList_Move, 2, [varEmpty, varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TCustomImageList, 'Overlay', TCustomImageList_Overlay, 2, [varEmpty, varEmpty], varEmpty);
    {$ENDIF VCL}
    AddGet(TCustomImageList, 'RegisterChanges', TCustomImageList_RegisterChanges, 1, [varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TCustomImageList, 'ResourceLoad', TCustomImageList_ResourceLoad, 3, [varEmpty, varEmpty, varEmpty],
      varEmpty);
    {$ENDIF VCL}
    {$IFDEF VCL}
    AddGet(TCustomImageList, 'ResInstLoad', TCustomImageList_ResInstLoad, 4, [varEmpty, varEmpty, varEmpty, varEmpty],
      varEmpty);
    {$ENDIF VCL}
    AddGet(TCustomImageList, 'Replace', TCustomImageList_Replace, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TCustomImageList, 'ReplaceIcon', TCustomImageList_ReplaceIcon, 2, [varEmpty, varEmpty], varEmpty);
    {$ENDIF VCL}
    AddGet(TCustomImageList, 'ReplaceMasked', TCustomImageList_ReplaceMasked, 3, [varEmpty, varEmpty, varEmpty],
      varEmpty);
    AddGet(TCustomImageList, 'UnRegisterChanges', TCustomImageList_UnRegisterChanges, 1, [varEmpty], varEmpty);
    AddGet(TCustomImageList, 'Count', TCustomImageList_Read_Count, 0, [varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TCustomImageList, 'Handle', TCustomImageList_Read_Handle, 0, [varEmpty], varEmpty);
    AddSet(TCustomImageList, 'Handle', TCustomImageList_Write_Handle, 0, [varEmpty]);
    {$ENDIF VCL}
    AddGet(TCustomImageList, 'HideDragImage', TCustomImageList_HideDragImage, 0, [varEmpty], varEmpty);
    AddGet(TCustomImageList, 'SetDragImage', TCustomImageList_SetDragImage, 3, [varEmpty, varEmpty, varEmpty],
      varEmpty);
    AddGet(TCustomImageList, 'ShowDragImage', TCustomImageList_ShowDragImage, 0, [varEmpty], varEmpty);
    AddGet(TCustomImageList, 'DragCursor', TCustomImageList_Read_DragCursor, 0, [varEmpty], varEmpty);
    AddSet(TCustomImageList, 'DragCursor', TCustomImageList_Write_DragCursor, 0, [varEmpty]);
    AddGet(TCustomImageList, 'Dragging', TCustomImageList_Read_Dragging, 0, [varEmpty], varEmpty);
    AddGet(TCustomImageList, 'BeginDrag', TCustomImageList_BeginDrag, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TCustomImageList, 'DragLock', TCustomImageList_DragLock, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TCustomImageList, 'DragMove', TCustomImageList_DragMove, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TCustomImageList, 'DragUnlock', TCustomImageList_DragUnlock, 0, [varEmpty], varEmpty);
    AddGet(TCustomImageList, 'EndDrag', TCustomImageList_EndDrag, 0, [varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TDragImageList, 'HideDragImage', TCustomImageList_HideDragImage, 0, [varEmpty], varEmpty);
    AddGet(TDragImageList, 'SetDragImage', TCustomImageList_SetDragImage, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TDragImageList, 'ShowDragImage', TCustomImageList_ShowDragImage, 0, [varEmpty], varEmpty);
    AddGet(TDragImageList, 'DragCursor', TCustomImageList_Read_DragCursor, 0, [varEmpty], varEmpty);
    AddSet(TDragImageList, 'DragCursor', TCustomImageList_Write_DragCursor, 0, [varEmpty]);
    AddGet(TDragImageList, 'Dragging', TCustomImageList_Read_Dragging, 0, [varEmpty], varEmpty);
    AddGet(TDragImageList, 'BeginDrag', TCustomImageList_BeginDrag, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TDragImageList, 'DragLock', TCustomImageList_DragLock, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TDragImageList, 'DragMove', TCustomImageList_DragMove, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TDragImageList, 'DragUnlock', TCustomImageList_DragUnlock, 0, [varEmpty], varEmpty);
    AddGet(TDragImageList, 'EndDrag', TCustomImageList_EndDrag, 0, [varEmpty], varEmpty);
    {$ENDIF VCL}
    { TImageList }
    AddClass(cControls, TImageList, 'TImageList');
    {$IFDEF VCL}
    AddFunction(cControls, 'IsDragObject', JvInterpreter_IsDragObject, 1, [varEmpty], varEmpty);
    {$ENDIF VCL}
    AddFunction(cControls, 'FindControl', JvInterpreter_FindControl, 1, [varEmpty], varEmpty);
    {$IFDEF VCL}
    AddFunction(cControls, 'FindVCLWindow', JvInterpreter_FindVCLWindow, 1, [varEmpty], varEmpty);
    {$ENDIF VCL}
    AddFunction(cControls, 'FindDragTarget', JvInterpreter_FindDragTarget, 2, [varEmpty, varEmpty], varEmpty);
    AddFunction(cControls, 'GetCaptureControl', JvInterpreter_GetCaptureControl, 0, [varEmpty], varEmpty);
    AddFunction(cControls, 'SetCaptureControl', JvInterpreter_SetCaptureControl, 1, [varEmpty], varEmpty);
    {$IFDEF VCL}
    AddFunction(cControls, 'CancelDrag', JvInterpreter_CancelDrag, 0, [varEmpty], varEmpty);
    {$ENDIF VCL}
    AddFunction(cControls, 'CursorToString', JvInterpreter_CursorToString, 1, [varEmpty], varEmpty);
    AddFunction(cControls, 'StringToCursor', JvInterpreter_StringToCursor, 1, [varEmpty], varEmpty);
    AddFunction(cControls, 'CursorToIdent', JvInterpreter_CursorToIdent, 2, [varEmpty, varByRef], varEmpty);
    AddFunction(cControls, 'IdentToCursor', JvInterpreter_IdentToCursor, 2, [varEmpty, varByRef], varEmpty);
    AddFunction(cControls, 'GetShortHint', JvInterpreter_GetShortHint, 1, [varEmpty], varEmpty);
    AddFunction(cControls, 'GetLongHint', JvInterpreter_GetLongHint, 1, [varEmpty], varEmpty);
    {$IFDEF VCL}
    AddFunction(cControls, 'InitWndProc', JvInterpreter_InitWndProc, 4, [varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
    AddFunction(cControls, 'SendAppMessage', JvInterpreter_SendAppMessage, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFunction(cControls, 'MoveWindowOrg', JvInterpreter_MoveWindowOrg, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    {$ENDIF VCL}
    AddHandler(cControls, 'TMouseEvent', TJvInterpreterControlsEvent, @TJvInterpreterControlsEvent.MouseEvent);
    AddHandler(cControls, 'TMouseMoveEvent', TJvInterpreterControlsEvent, @TJvInterpreterControlsEvent.MouseMoveEvent);
    AddHandler(cControls, 'TKeyEvent', TJvInterpreterControlsEvent, @TJvInterpreterControlsEvent.KeyEvent);
    AddHandler(cControls, 'TKeyPressEvent', TJvInterpreterControlsEvent, @TJvInterpreterControlsEvent.KeyPressEvent);
    AddHandler(cControls, 'TDragOverEvent', TJvInterpreterControlsEvent, @TJvInterpreterControlsEvent.DragOverEvent);
    AddHandler(cControls, 'TDragDropEvent', TJvInterpreterControlsEvent, @TJvInterpreterControlsEvent.DragDropEvent);
    AddHandler(cControls, 'TStartDragEvent', TJvInterpreterControlsEvent, @TJvInterpreterControlsEvent.StartDragEvent);
    AddHandler(cControls, 'TEndDragEvent', TJvInterpreterControlsEvent, @TJvInterpreterControlsEvent.EndDragEvent);
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

