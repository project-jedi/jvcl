{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter_Types.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : adapter unit - converts JvInterpreter calls to delphi calls

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvInterpreter_Types;

interface

uses
  Types, Variants,
  JvInterpreter;

function Point2Var(const Point: TPoint): Variant;
function Var2Point(const Point: Variant): TPoint;
function Rect2Var(const Rect: TRect): Variant;
function Var2Rect(const Rect: Variant): TRect;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

{ TPoint }

function Point2Var(const Point: TPoint): Variant;
var
  Rec: ^TPoint;
begin
  New(Rec);
  Rec^ := Point;
  Result := R2V('TPoint', Rec);
end;

function Var2Point(const Point: Variant): TPoint;
begin
  Result := TPoint(V2R(Point)^);
end;

procedure JvInterpreter_Point(var Value: Variant; Args: TArgs);
begin
  Value := Point2Var(Point(Args.Values[0], Args.Values[1]));
end;

{ TRect }

function Rect2Var(const Rect: TRect): Variant;
var
  Rec: ^TRect;
begin
  New(Rec);
  Rec^ := Rect;
  Result := R2V('TRect', Rec);
end;

function Var2Rect(const Rect: Variant): TRect;
begin
  Result := TRect(V2R(Rect)^);
end;

procedure JvInterpreter_Rect(var Value: Variant; Args: TArgs);
begin
  Value := Rect2Var(Rect(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3]));
end;

procedure JvInterpreter_Bounds(var Value: Variant; Args: TArgs);
begin
  Value := Rect2Var(Bounds(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3]));
end;

{ Read Field TopLeft: Integer; }

procedure TRect_Read_TopLeft(var Value: Variant; Args: TArgs);
begin
  Value := Point2Var(TRect(P2R(Args.Obj)^).TopLeft);
end;

{ Write Field TopLeft: Integer; }

procedure TRect_Write_TopLeft(const Value: Variant; Args: TArgs);
begin
  TRect(P2R(Args.Obj)^).TopLeft := Var2Point(Value);
end;

{ Read Field BottomRight: Integer; }

procedure TRect_Read_BottomRight(var Value: Variant; Args: TArgs);
begin
  Value := Point2Var(TRect(P2R(Args.Obj)^).BottomRight);
end;

{ Write Field Right: Integer; }

procedure TRect_Write_BottomRight(const Value: Variant; Args: TArgs);
begin
  TRect(P2R(Args.Obj)^).BottomRight := Var2Point(Value);
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  cTypes = 'Types';
begin
  with JvInterpreterAdapter do
  begin
    AddExtUnit(cTypes);
    { TPoint }
    AddRec(cTypes, 'TPoint', sizeof(TPoint), [RFD('X', 0, varInteger), RFD('Y', 4, varInteger)], nil, nil, nil);
    AddFun(cTypes, 'Point', JvInterpreter_Point, 2, [varInteger, varInteger], varRecord);
    { TRect }
    AddRec(cTypes, 'TRect', sizeof(TRect), [RFD('Left', 0, varInteger), RFD('Top', 4, varInteger), RFD('Right', 8,
      varInteger), RFD('Bottom', 12, varInteger)], nil, nil, nil);
    AddFun(cTypes, 'Rect', JvInterpreter_Rect, 4, [varInteger, varInteger, varInteger, varInteger], varRecord);
    AddFun(cTypes, 'Bounds', JvInterpreter_Bounds, 4, [varInteger, varInteger, varInteger, varInteger], varRecord);
    AddRecGet(cTypes, 'TRect', 'TopLeft', TRect_Read_TopLeft, 0, [0], varRecord);
    AddRecSet(cTypes, 'TRect', 'TopLeft', TRect_Write_TopLeft, 0, [0]);
    AddRecGet(cTypes, 'TRect', 'BottomRight', TRect_Read_BottomRight, 0, [0], varRecord);
    AddRecSet(cTypes, 'TRect', 'BottomRight', TRect_Write_BottomRight, 0, [0]);
  end;
end;

end.

