{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvJvBoxProcs.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvBoxProcs;

interface

uses
  {$IFDEF VCL}
  Windows, Graphics, Controls, StdCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QStdCtrls, Types, QWindows,
  {$ENDIF VisualCLX}
  Classes;

procedure BoxMoveSelectedItems(SrcList, DstList: TWinControl);
procedure BoxMoveAllItems(SrcList, DstList: TWinControl);
procedure BoxDragOver(List: TWinControl; Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean; Sorted: Boolean);
procedure BoxMoveFocusedItem(List: TWinControl; DstIndex: Integer);

procedure BoxMoveSelected(List: TWinControl; Items: TStrings);
procedure BoxSetItem(List: TWinControl; Index: Integer);
function BoxGetFirstSelection(List: TWinControl): Integer;
function BoxCanDropItem(List: TWinControl; X, Y: Integer;
  var DragIndex: Integer): Boolean;

implementation

{$IFDEF VCL}
uses
  JvxCheckListBox;
{$ENDIF VCL}

{$IFDEF VisualCLX}
const
  LB_ERR = -1;
{$ENDIF VisualCLX}

function BoxItems(List: TWinControl): TStrings;
begin
  if List is TCustomListBox then
    Result := TCustomListBox(List).Items
  {$IFDEF VCL}
  else
  if List is TJvxCustomListBox then
    Result := TJvxCustomListBox(List).Items
  {$ENDIF VCL}
  else
    Result := nil;
end;

function BoxGetSelected(List: TWinControl; Index: Integer): Boolean;
begin
  if List is TCustomListBox then
    Result := TCustomListBox(List).Selected[Index]
  {$IFDEF VCL}
  else
  if List is TJvxCustomListBox then
    Result := TJvxCustomListBox(List).Selected[Index]
  {$ENDIF VCL}
  else
    Result := False;
end;

procedure BoxSetSelected(List: TWinControl; Index: Integer; Value: Boolean);
begin
  if List is TCustomListBox then
    TCustomListBox(List).Selected[Index] := Value
  {$IFDEF VCL}
  else
  if List is TJvxCustomListBox then
    TJvxCustomListBox(List).Selected[Index] := Value;
  {$ENDIF VCL}
end;

function BoxGetItemIndex(List: TWinControl): Integer;
begin
  if List is TCustomListBox then
    Result := TCustomListBox(List).ItemIndex
  {$IFDEF VCL}
  else
  if List is TJvxCustomListBox then
    Result := TJvxCustomListBox(List).ItemIndex
  {$ENDIF VCL}
  else
    Result := LB_ERR;
end;

procedure BoxSetItemIndex(List: TWinControl; Index: Integer);
begin
  if List is TCustomListBox then
    TCustomListBox(List).ItemIndex := Index
  {$IFDEF VCL}
  else
  if List is TJvxCustomListBox then
    TJvxCustomListBox(List).ItemIndex := Index;
  {$ENDIF VCL}
end;

function BoxMultiSelect(List: TWinControl): Boolean;
begin
  if List is TCustomListBox then
    Result := TListBox(List).MultiSelect
  {$IFDEF VCL}
  else
  if List is TJvxCustomListBox then
    Result := TJvxCheckListBox(List).MultiSelect
  {$ENDIF VCL}
  else
    Result := False;
end;

function BoxSelCount(List: TWinControl): Integer;
begin
  if List is TCustomListBox then
    Result := TCustomListBox(List).SelCount
  {$IFDEF VCL}
  else
  if List is TJvxCustomListBox then
    Result := TJvxCustomListBox(List).SelCount
  {$ENDIF VCL}
  else
    Result := 0;
end;

function BoxItemAtPos(List: TWinControl; Pos: TPoint;
  Existing: Boolean): Integer;
begin
  if List is TCustomListBox then
    Result := TCustomListBox(List).ItemAtPos(Pos, Existing)
  {$IFDEF VCL}
  else
  if List is TJvxCustomListBox then
    Result := TJvxCustomListBox(List).ItemAtPos(Pos, Existing)
  {$ENDIF VCL}
  else
    Result := LB_ERR;
end;

function BoxItemRect(List: TWinControl; Index: Integer): TRect;
begin
  if List is TCustomListBox then
    Result := TCustomListBox(List).ItemRect(Index)
  {$IFDEF VCL}
  else
  if List is TJvxCustomListBox then
    Result := TJvxCustomListBox(List).ItemRect(Index)
  {$ENDIF VCL}
  else
    FillChar(Result, SizeOf(Result), 0);
end;

procedure BoxMoveSelected(List: TWinControl; Items: TStrings);
var
  I: Integer;
begin
  if BoxItems(List) = nil then
    Exit;
  I := 0;
  while I < BoxItems(List).Count do
  begin
    if BoxGetSelected(List, I) then
    begin
      Items.AddObject(BoxItems(List).Strings[I], BoxItems(List).Objects[I]);
      BoxItems(List).Delete(I);
    end
    else
      Inc(I);
  end;
end;

function BoxGetFirstSelection(List: TWinControl): Integer;
var
  I: Integer;
begin
  Result := LB_ERR;
  if BoxItems(List) = nil then
    Exit;
  for I := 0 to BoxItems(List).Count - 1 do
  begin
    if BoxGetSelected(List, I) then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := LB_ERR;
end;

procedure BoxSetItem(List: TWinControl; Index: Integer);
var
  MaxIndex: Integer;
begin
  if BoxItems(List) = nil then
    Exit;
  with List do
  begin
    if CanFocus then
      SetFocus;
    MaxIndex := BoxItems(List).Count - 1;
    if Index = LB_ERR then
      Index := 0
    else
    if Index > MaxIndex then
      Index := MaxIndex;
    if Index >= 0 then
      if BoxMultiSelect(List) then
        BoxSetSelected(List, Index, True)
      else
        BoxSetItemIndex(List, Index);
  end;
end;

procedure BoxMoveSelectedItems(SrcList, DstList: TWinControl);
var
  Index, I: Integer;
  {$IFDEF VCL}
  NewIndex: Integer;
  {$ENDIF VCL}
begin
  Index := BoxGetFirstSelection(SrcList);
  if Index <> LB_ERR then
  begin
    BoxItems(SrcList).BeginUpdate;
    BoxItems(DstList).BeginUpdate;
    try
      I := 0;
      while I < BoxItems(SrcList).Count do
        if BoxGetSelected(SrcList, I) then
        begin
          {$IFDEF VCL}
          NewIndex := BoxItems(DstList).AddObject(BoxItems(SrcList).Strings[I],
            BoxItems(SrcList).Objects[I]);
          if (SrcList is TJvxCheckListBox) and (DstList is TJvxCheckListBox) then
            TJvxCheckListBox(DstList).State[NewIndex] :=
              TJvxCheckListBox(SrcList).State[I];
          {$ENDIF VCL}
          {$IFDEF VisualCLX}
          BoxItems(DstList).AddObject(BoxItems(SrcList).Strings[I],
            BoxItems(SrcList).Objects[I]);
          {$ENDIF VisualCLX}
          BoxItems(SrcList).Delete(I);
        end
        else
          Inc(I);
      BoxSetItem(SrcList, Index);
    finally
      BoxItems(SrcList).EndUpdate;
      BoxItems(DstList).EndUpdate;
    end;
  end;
end;

procedure BoxMoveAllItems(SrcList, DstList: TWinControl);
var
  I: Integer;
  {$IFDEF VCL}
  NewIndex: Integer;
  {$ENDIF VCL}
begin
  for I := 0 to BoxItems(SrcList).Count - 1 do
  begin
    {$IFDEF VCL}
    NewIndex := BoxItems(DstList).AddObject(BoxItems(SrcList)[I],
      BoxItems(SrcList).Objects[I]);
    if (SrcList is TJvxCheckListBox) and (DstList is TJvxCheckListBox) then
      TJvxCheckListBox(DstList).State[NewIndex] :=
        TJvxCheckListBox(SrcList).State[I];
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    BoxItems(DstList).AddObject(BoxItems(SrcList)[I],
      BoxItems(SrcList).Objects[I]);
    {$ENDIF VisualCLX}
  end;
  BoxItems(SrcList).Clear;
  BoxSetItem(SrcList, 0);
end;

function BoxCanDropItem(List: TWinControl; X, Y: Integer;
  var DragIndex: Integer): Boolean;
var
  Focused: Integer;
begin
  Result := False;
  if (BoxSelCount(List) = 1) or (not BoxMultiSelect(List)) then
  begin
    Focused := BoxGetItemIndex(List);
    if Focused <> LB_ERR then
    begin
      DragIndex := BoxItemAtPos(List, Point(X, Y), True);
      if (DragIndex >= 0) and (DragIndex <> Focused) then
        Result := True;
    end;
  end;
end;

procedure BoxDragOver(List: TWinControl; Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean; Sorted: Boolean);
var
  DragIndex: Integer;
  R: TRect;

  procedure DrawItemFocusRect(Idx: Integer);
  var
    P: TPoint;
    DC: HDC;
  begin
    R := BoxItemRect(List, Idx);
    P := List.ClientToScreen(R.TopLeft);
    R := Bounds(P.X, P.Y, R.Right - R.Left, R.Bottom - R.Top);
    DC := GetDC(0);
    DrawFocusRect(DC, R);
    ReleaseDC(0, DC);
  end;

begin
  if Source <> List then
    Accept := (Source is TWinControl)
      {$IFDEF VCL} or (Source is TJvxCustomListBox) {$ENDIF}
  else
  begin
    if Sorted then
      Accept := False
    else
    begin
      Accept := BoxCanDropItem(List, X, Y, DragIndex);
      if ((List.Tag - 1) = DragIndex) and (DragIndex >= 0) then
      begin
        if State = dsDragLeave then
        begin
          DrawItemFocusRect(List.Tag - 1);
          List.Tag := 0;
        end;
      end
      else
      begin
        if List.Tag > 0 then
          DrawItemFocusRect(List.Tag - 1);
        if DragIndex >= 0 then
          DrawItemFocusRect(DragIndex);
        List.Tag := DragIndex + 1;
      end;
    end;
  end;
end;

procedure BoxMoveFocusedItem(List: TWinControl; DstIndex: Integer);
begin
  if (DstIndex >= 0) and (DstIndex < BoxItems(List).Count) then
    if (DstIndex <> BoxGetItemIndex(List)) then
    begin
      BoxItems(List).Move(BoxGetItemIndex(List), DstIndex);
      BoxSetItem(List, DstIndex);
    end;
end;

end.

