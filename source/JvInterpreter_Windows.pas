{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter_Windows.PAS, released on 2002-07-04.

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

unit JvInterpreter_Windows;

interface

uses
  Windows, JvInterpreter;

function Point2Var(const Point: TPoint): Variant;
function Var2Point(const Point: Variant): TPoint;
function Rect2Var(const Rect: TRect): Variant;
function Var2Rect(const Rect: Variant): TRect;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses
  Classes;

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

procedure JvInterpreter_Point(var Value: Variant; Args: TJvInterpreterArgs);
begin
  JvInterpreterVarCopy(Value, Point2Var(Point(Args.Values[0], Args.Values[1])));
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

procedure JvInterpreter_Rect(var Value: Variant; Args: TJvInterpreterArgs);
begin
  JvInterpreterVarCopy(Value, Rect2Var(Rect(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3])));
end;

procedure JvInterpreter_Bounds(var Value: Variant; Args: TJvInterpreterArgs);
begin
  JvInterpreterVarCopy(Value, Rect2Var(Bounds(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3])));
end;

{ Read Field TopLeft: Integer; }

procedure TRect_Read_TopLeft(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Point2Var(TRect(P2R(Args.Obj)^).TopLeft);
end;

{ Write Field TopLeft: Integer; }

procedure TRect_Write_TopLeft(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TRect(P2R(Args.Obj)^).TopLeft := Var2Point(Value);
end;

{ Read Field BottomRight: Integer; }

procedure TRect_Read_BottomRight(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Point2Var(TRect(P2R(Args.Obj)^).BottomRight);
end;

{ Write Field Right: Integer; }

procedure TRect_Write_BottomRight(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TRect(P2R(Args.Obj)^).BottomRight := Var2Point(Value);
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  cWindows = 'Windows';
begin
  with JvInterpreterAdapter do
  begin
    AddExtUnit(cWindows);
    { TPoint }
    AddRec(cWindows, 'TPoint', sizeof(TPoint), [RFD('X', 0, varInteger), RFD('Y', 4, varInteger)], nil, nil, nil);
    AddFun(cWindows, 'Point', JvInterpreter_Point, 2, [varInteger, varInteger], varRecord);
    { TRect }
    AddRec(cWindows, 'TRect', sizeof(TRect), [RFD('Left', 0, varInteger), RFD('Top', 4, varInteger), RFD('Right', 8,
      varInteger), RFD('Bottom', 12, varInteger)], nil, nil, nil);
    AddFun(cWindows, 'Rect', JvInterpreter_Rect, 4, [varInteger, varInteger, varInteger, varInteger], varRecord);
    AddFun(cWindows, 'Bounds', JvInterpreter_Bounds, 4, [varInteger, varInteger, varInteger, varInteger], varRecord);
    AddRecGet(cWindows, 'TRect', 'TopLeft', TRect_Read_TopLeft, 0, [0], varRecord);
    AddRecSet(cWindows, 'TRect', 'TopLeft', TRect_Write_TopLeft, 0, [0]);
    AddRecGet(cWindows, 'TRect', 'BottomRight', TRect_Read_BottomRight, 0, [0], varRecord);
    AddRecSet(cWindows, 'TRect', 'BottomRight', TRect_Write_BottomRight, 0, [0]);

    AddExtFun(cWindows, 'MessageBox', 0, user32, 'MessageBoxA', -1, 4, [varInteger, varString, varString, varInteger],
      varInteger);
    { MessageBox(, nil) Flags }
    AddConst(cWindows, 'MB_OK', $00000000);
    AddConst(cWindows, 'MB_OKCANCEL', $00000001);
    AddConst(cWindows, 'MB_ABORTRETRYIGNORE', $00000002);
    AddConst(cWindows, 'MB_YESNOCANCEL', $00000003);
    AddConst(cWindows, 'MB_YESNO', $00000004);
    AddConst(cWindows, 'MB_RETRYCANCEL', $00000005);
    AddConst(cWindows, 'MB_ICONHAND', $00000010);
    AddConst(cWindows, 'MB_ICONQUESTION', $00000020);
    AddConst(cWindows, 'MB_ICONEXCLAMATION', $00000030);
    AddConst(cWindows, 'MB_ICONASTERISK', $00000040);
    AddConst(cWindows, 'MB_USERICON', $00000080);
    AddConst(cWindows, 'MB_ICONWARNING', MB_ICONEXCLAMATION);
    AddConst(cWindows, 'MB_ICONERROR', MB_ICONHAND);
    AddConst(cWindows, 'MB_ICONINFORMATION', MB_ICONASTERISK);
    AddConst(cWindows, 'MB_ICONSTOP', MB_ICONHAND);
    AddConst(cWindows, 'MB_DEFBUTTON1', $00000000);
    AddConst(cWindows, 'MB_DEFBUTTON2', $00000100);
    AddConst(cWindows, 'MB_DEFBUTTON3', $00000200);
    AddConst(cWindows, 'MB_DEFBUTTON4', $00000300);
    AddConst(cWindows, 'MB_APPLMODAL', $00000000);
    AddConst(cWindows, 'MB_SYSTEMMODAL', $00001000);
    AddConst(cWindows, 'MB_TASKMODAL', $00002000);
    AddConst(cWindows, 'MB_HELP', $00004000);
    AddConst(cWindows, 'MB_NOFOCUS', $00008000);
    AddConst(cWindows, 'MB_SETFOREGROUND', $00010000);
    AddConst(cWindows, 'MB_DEFAULT_DESKTOP_ONLY', $00020000);
    AddConst(cWindows, 'MB_TOPMOST', $00040000);
    AddConst(cWindows, 'MB_RIGHT', $00080000);
    AddConst(cWindows, 'MB_RTLREADING', $00100000);
    AddConst(cWindows, 'MB_SERVICE_NOTIFICATION', $00200000);
    AddConst(cWindows, 'MB_SERVICE_NOTIFICATION_NT3X', $00040000);
    AddConst(cWindows, 'MB_TYPEMASK', $0000000F);
    AddConst(cWindows, 'MB_ICONMASK', $000000F0);
    AddConst(cWindows, 'MB_DEFMASK', $00000F00);
    AddConst(cWindows, 'MB_MODEMASK', $00003000);
    AddConst(cWindows, 'MB_MISCMASK', $0000C000);

    { Virtual Keys, Standard Set }
    AddConst(cWindows, 'VK_LBUTTON', VK_LBUTTON);
    AddConst(cWindows, 'VK_RBUTTON', VK_RBUTTON);
    AddConst(cWindows, 'VK_CANCEL', VK_CANCEL);
    AddConst(cWindows, 'VK_MBUTTON', VK_MBUTTON);
    AddConst(cWindows, 'VK_BACK', VK_BACK);
    AddConst(cWindows, 'VK_TAB', VK_TAB);
    AddConst(cWindows, 'VK_CLEAR', VK_CLEAR);
    AddConst(cWindows, 'VK_RETURN', VK_RETURN);
    AddConst(cWindows, 'VK_SHIFT', VK_SHIFT);
    AddConst(cWindows, 'VK_CONTROL', VK_CONTROL);
    AddConst(cWindows, 'VK_MENU', VK_MENU);
    AddConst(cWindows, 'VK_PAUSE', VK_PAUSE);
    AddConst(cWindows, 'VK_CAPITAL', VK_CAPITAL);
    {$IFDEF COMPILER3_UP}
    AddConst(cWindows, 'VK_KANA', VK_KANA);
    AddConst(cWindows, 'VK_HANGUL', VK_HANGUL);
    AddConst(cWindows, 'VK_JUNJA', VK_JUNJA);
    AddConst(cWindows, 'VK_FINAL', VK_FINAL);
    AddConst(cWindows, 'VK_HANJA', VK_HANJA);
    AddConst(cWindows, 'VK_KANJI', VK_KANJI);
    AddConst(cWindows, 'VK_CONVERT', VK_CONVERT);
    AddConst(cWindows, 'VK_NONCONVERT', VK_NONCONVERT);
    AddConst(cWindows, 'VK_ACCEPT', VK_ACCEPT);
    AddConst(cWindows, 'VK_MODECHANGE', VK_MODECHANGE);
    {$ENDIF COMPILER3_UP}
    AddConst(cWindows, 'VK_ESCAPE', VK_ESCAPE);
    AddConst(cWindows, 'VK_SPACE', VK_SPACE);
    AddConst(cWindows, 'VK_PRIOR', VK_PRIOR);
    AddConst(cWindows, 'VK_NEXT', VK_NEXT);
    AddConst(cWindows, 'VK_END', VK_END);
    AddConst(cWindows, 'VK_HOME', VK_HOME);
    AddConst(cWindows, 'VK_LEFT', VK_LEFT);
    AddConst(cWindows, 'VK_UP', VK_UP);
    AddConst(cWindows, 'VK_RIGHT', VK_RIGHT);
    AddConst(cWindows, 'VK_DOWN', VK_DOWN);
    AddConst(cWindows, 'VK_SELECT', VK_SELECT);
    AddConst(cWindows, 'VK_PRINT', VK_PRINT);
    AddConst(cWindows, 'VK_EXECUTE', VK_EXECUTE);
    AddConst(cWindows, 'VK_SNAPSHOT', VK_SNAPSHOT);
    AddConst(cWindows, 'VK_INSERT', VK_INSERT);
    AddConst(cWindows, 'VK_DELETE', VK_DELETE);
    AddConst(cWindows, 'VK_HELP', VK_HELP);
    AddConst(cWindows, 'VK_LWIN', VK_LWIN);
    AddConst(cWindows, 'VK_RWIN', VK_RWIN);
    AddConst(cWindows, 'VK_APPS', VK_APPS);
    AddConst(cWindows, 'VK_NUMPAD0', VK_NUMPAD0);
    AddConst(cWindows, 'VK_NUMPAD1', VK_NUMPAD1);
    AddConst(cWindows, 'VK_NUMPAD2', VK_NUMPAD2);
    AddConst(cWindows, 'VK_NUMPAD3', VK_NUMPAD3);
    AddConst(cWindows, 'VK_NUMPAD4', VK_NUMPAD4);
    AddConst(cWindows, 'VK_NUMPAD5', VK_NUMPAD5);
    AddConst(cWindows, 'VK_NUMPAD6', VK_NUMPAD6);
    AddConst(cWindows, 'VK_NUMPAD7', VK_NUMPAD7);
    AddConst(cWindows, 'VK_NUMPAD8', VK_NUMPAD8);
    AddConst(cWindows, 'VK_NUMPAD9', VK_NUMPAD9);
    AddConst(cWindows, 'VK_MULTIPLY', VK_MULTIPLY);
    AddConst(cWindows, 'VK_ADD', VK_ADD);
    AddConst(cWindows, 'VK_SEPARATOR', VK_SEPARATOR);
    AddConst(cWindows, 'VK_SUBTRACT', VK_SUBTRACT);
    AddConst(cWindows, 'VK_DECIMAL', VK_DECIMAL);
    AddConst(cWindows, 'VK_DIVIDE', VK_DIVIDE);
    AddConst(cWindows, 'VK_F1', VK_F1);
    AddConst(cWindows, 'VK_F2', VK_F2);
    AddConst(cWindows, 'VK_F3', VK_F3);
    AddConst(cWindows, 'VK_F4', VK_F4);
    AddConst(cWindows, 'VK_F5', VK_F5);
    AddConst(cWindows, 'VK_F6', VK_F6);
    AddConst(cWindows, 'VK_F7', VK_F7);
    AddConst(cWindows, 'VK_F8', VK_F8);
    AddConst(cWindows, 'VK_F9', VK_F9);
    AddConst(cWindows, 'VK_F10', VK_F10);
    AddConst(cWindows, 'VK_F11', VK_F11);
    AddConst(cWindows, 'VK_F12', VK_F12);
    AddConst(cWindows, 'VK_F13', VK_F13);
    AddConst(cWindows, 'VK_F14', VK_F14);
    AddConst(cWindows, 'VK_F15', VK_F15);
    AddConst(cWindows, 'VK_F16', VK_F16);
    AddConst(cWindows, 'VK_F17', VK_F17);
    AddConst(cWindows, 'VK_F18', VK_F18);
    AddConst(cWindows, 'VK_F19', VK_F19);
    AddConst(cWindows, 'VK_F20', VK_F20);
    AddConst(cWindows, 'VK_F21', VK_F21);
    AddConst(cWindows, 'VK_F22', VK_F22);
    AddConst(cWindows, 'VK_F23', VK_F23);
    AddConst(cWindows, 'VK_F24', VK_F24);
    AddConst(cWindows, 'VK_NUMLOCK', VK_NUMLOCK);
    AddConst(cWindows, 'VK_SCROLL', VK_SCROLL);
    AddConst(cWindows, 'VK_LSHIFT', VK_LSHIFT);
    AddConst(cWindows, 'VK_RSHIFT', VK_RSHIFT);
    AddConst(cWindows, 'VK_LCONTROL', VK_LCONTROL);
    AddConst(cWindows, 'VK_RCONTROL', VK_RCONTROL);
    AddConst(cWindows, 'VK_LMENU', VK_LMENU);
    AddConst(cWindows, 'VK_RMENU', VK_RMENU);
    AddConst(cWindows, 'VK_PROCESSKEY', VK_PROCESSKEY);
    AddConst(cWindows, 'VK_ATTN', VK_ATTN);
    AddConst(cWindows, 'VK_CRSEL', VK_CRSEL);
    AddConst(cWindows, 'VK_EXSEL', VK_EXSEL);
    AddConst(cWindows, 'VK_EREOF', VK_EREOF);
    AddConst(cWindows, 'VK_PLAY', VK_PLAY);
    AddConst(cWindows, 'VK_ZOOM', VK_ZOOM);
    AddConst(cWindows, 'VK_NONAME', VK_NONAME);
    AddConst(cWindows, 'VK_PA1', VK_PA1);
    AddConst(cWindows, 'VK_OEM_CLEAR', VK_OEM_CLEAR);
  end;
end;

end.

