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

uses Windows, JvInterpreter;

  function Point2Var(const Point: TPoint): Variant;
  function Var2Point(const Point: Variant): TPoint;
  function Rect2Var(const Rect: TRect): Variant;
  function Var2Rect(const Rect: Variant): TRect;

  procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses Classes;


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

procedure JvInterpreter_Rect(var Value: Variant; Args: TArgs);
begin
  JvInterpreterVarCopy(Value, Rect2Var(Rect(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3])));
end;

procedure JvInterpreter_Bounds(var Value: Variant; Args: TArgs);
begin
  JvInterpreterVarCopy(Value, Rect2Var(Bounds(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3])));
end;

{  Read Field TopLeft: Integer; }
procedure TRect_Read_TopLeft(var Value: Variant; Args: TArgs);
begin
  Value := Point2Var(TRect(P2R(Args.Obj)^).TopLeft);
end;

{  Write Field TopLeft: Integer; }
procedure TRect_Write_TopLeft(const Value: Variant; Args: TArgs);
begin
  TRect(P2R(Args.Obj)^).TopLeft := Var2Point(Value);
end;

{  Read Field BottomRight: Integer; }
procedure TRect_Read_BottomRight(var Value: Variant; Args: TArgs);
begin
  Value := Point2Var(TRect(P2R(Args.Obj)^).BottomRight);
end;

{  Write Field Right: Integer; }
procedure TRect_Write_BottomRight(const Value: Variant; Args: TArgs);
begin
  TRect(P2R(Args.Obj)^).BottomRight := Var2Point(Value);
end;


procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
begin
  with JvInterpreterAdapter do
  begin
    AddExtUnit('Windows');
   { TPoint }
    AddRec('Windows', 'TPoint', sizeof(TPoint), [RFD('X', 0, varInteger), RFD('Y', 4, varInteger)], nil, nil, nil);
    AddFun('Windows', 'Point', JvInterpreter_Point, 2, [varInteger, varInteger], varRecord);
   { TRect }
    AddRec('Windows', 'TRect', sizeof(TRect), [RFD('Left', 0, varInteger), RFD('Top', 4, varInteger), RFD('Right', 8, varInteger), RFD('Bottom', 12, varInteger)], nil, nil, nil);
    AddFun('Windows', 'Rect', JvInterpreter_Rect, 4, [varInteger, varInteger, varInteger, varInteger], varRecord);
    AddFun('Windows', 'Bounds', JvInterpreter_Bounds, 4, [varInteger, varInteger, varInteger, varInteger], varRecord);
    AddRecGet('Windows', 'TRect', 'TopLeft', TRect_Read_TopLeft, 0, [0], varRecord);
    AddRecSet('Windows', 'TRect', 'TopLeft', TRect_Write_TopLeft, 0, [0]);
    AddRecGet('Windows', 'TRect', 'BottomRight', TRect_Read_BottomRight, 0, [0], varRecord);
    AddRecSet('Windows', 'TRect', 'BottomRight', TRect_Write_BottomRight, 0, [0]);

    AddExtFun('Windows', 'MessageBox', 0, user32, 'MessageBoxA', -1, 4, [varInteger, varString, varString, varInteger], varInteger);
  { MessageBox(, nil) Flags }
    AddConst('Windows', 'MB_OK', $00000000);
    AddConst('Windows', 'MB_OKCANCEL', $00000001);
    AddConst('Windows', 'MB_ABORTRETRYIGNORE', $00000002);
    AddConst('Windows', 'MB_YESNOCANCEL', $00000003);
    AddConst('Windows', 'MB_YESNO', $00000004);
    AddConst('Windows', 'MB_RETRYCANCEL', $00000005);
    AddConst('Windows', 'MB_ICONHAND', $00000010);
    AddConst('Windows', 'MB_ICONQUESTION', $00000020);
    AddConst('Windows', 'MB_ICONEXCLAMATION', $00000030);
    AddConst('Windows', 'MB_ICONASTERISK', $00000040);
    AddConst('Windows', 'MB_USERICON', $00000080);
    AddConst('Windows', 'MB_ICONWARNING',  MB_ICONEXCLAMATION);
    AddConst('Windows', 'MB_ICONERROR', MB_ICONHAND);
    AddConst('Windows', 'MB_ICONINFORMATION', MB_ICONASTERISK);
    AddConst('Windows', 'MB_ICONSTOP', MB_ICONHAND);
    AddConst('Windows', 'MB_DEFBUTTON1', $00000000);
    AddConst('Windows', 'MB_DEFBUTTON2', $00000100);
    AddConst('Windows', 'MB_DEFBUTTON3', $00000200);
    AddConst('Windows', 'MB_DEFBUTTON4', $00000300);
    AddConst('Windows', 'MB_APPLMODAL', $00000000);
    AddConst('Windows', 'MB_SYSTEMMODAL', $00001000);
    AddConst('Windows', 'MB_TASKMODAL', $00002000);
    AddConst('Windows', 'MB_HELP', $00004000);
    AddConst('Windows', 'MB_NOFOCUS', $00008000);
    AddConst('Windows', 'MB_SETFOREGROUND', $00010000);
    AddConst('Windows', 'MB_DEFAULT_DESKTOP_ONLY', $00020000);
    AddConst('Windows', 'MB_TOPMOST', $00040000);
    AddConst('Windows', 'MB_RIGHT', $00080000);
    AddConst('Windows', 'MB_RTLREADING', $00100000);
    AddConst('Windows', 'MB_SERVICE_NOTIFICATION', $00200000);
    AddConst('Windows', 'MB_SERVICE_NOTIFICATION_NT3X', $00040000);
    AddConst('Windows', 'MB_TYPEMASK', $0000000F);
    AddConst('Windows', 'MB_ICONMASK', $000000F0);
    AddConst('Windows', 'MB_DEFMASK', $00000F00);
    AddConst('Windows', 'MB_MODEMASK', $00003000);
    AddConst('Windows', 'MB_MISCMASK', $0000C000);

  { Virtual Keys, Standard Set }
    AddConst('Windows', 'VK_LBUTTON', VK_LBUTTON);
    AddConst('Windows', 'VK_RBUTTON', VK_RBUTTON);
    AddConst('Windows', 'VK_CANCEL', VK_CANCEL);
    AddConst('Windows', 'VK_MBUTTON', VK_MBUTTON);
    AddConst('Windows', 'VK_BACK', VK_BACK);
    AddConst('Windows', 'VK_TAB', VK_TAB);
    AddConst('Windows', 'VK_CLEAR', VK_CLEAR);
    AddConst('Windows', 'VK_RETURN', VK_RETURN);
    AddConst('Windows', 'VK_SHIFT', VK_SHIFT);
    AddConst('Windows', 'VK_CONTROL', VK_CONTROL);
    AddConst('Windows', 'VK_MENU', VK_MENU);
    AddConst('Windows', 'VK_PAUSE', VK_PAUSE);
    AddConst('Windows', 'VK_CAPITAL', VK_CAPITAL);
   {$IFDEF COMPILER3_UP}
    AddConst('Windows', 'VK_KANA', VK_KANA);
    AddConst('Windows', 'VK_HANGUL', VK_HANGUL);
    AddConst('Windows', 'VK_JUNJA', VK_JUNJA);
    AddConst('Windows', 'VK_FINAL', VK_FINAL);
    AddConst('Windows', 'VK_HANJA', VK_HANJA);
    AddConst('Windows', 'VK_KANJI', VK_KANJI);
    AddConst('Windows', 'VK_CONVERT', VK_CONVERT);
    AddConst('Windows', 'VK_NONCONVERT', VK_NONCONVERT);
    AddConst('Windows', 'VK_ACCEPT', VK_ACCEPT);
    AddConst('Windows', 'VK_MODECHANGE', VK_MODECHANGE);
   {$ENDIF COMPILER3_UP}
    AddConst('Windows', 'VK_ESCAPE', VK_ESCAPE);
    AddConst('Windows', 'VK_SPACE', VK_SPACE);
    AddConst('Windows', 'VK_PRIOR', VK_PRIOR);
    AddConst('Windows', 'VK_NEXT', VK_NEXT);
    AddConst('Windows', 'VK_END', VK_END);
    AddConst('Windows', 'VK_HOME', VK_HOME);
    AddConst('Windows', 'VK_LEFT', VK_LEFT);
    AddConst('Windows', 'VK_UP', VK_UP);
    AddConst('Windows', 'VK_RIGHT', VK_RIGHT);
    AddConst('Windows', 'VK_DOWN', VK_DOWN);
    AddConst('Windows', 'VK_SELECT', VK_SELECT);
    AddConst('Windows', 'VK_PRINT', VK_PRINT);
    AddConst('Windows', 'VK_EXECUTE', VK_EXECUTE);
    AddConst('Windows', 'VK_SNAPSHOT', VK_SNAPSHOT);
    AddConst('Windows', 'VK_INSERT', VK_INSERT);
    AddConst('Windows', 'VK_DELETE', VK_DELETE);
    AddConst('Windows', 'VK_HELP', VK_HELP);
    AddConst('Windows', 'VK_LWIN', VK_LWIN);
    AddConst('Windows', 'VK_RWIN', VK_RWIN);
    AddConst('Windows', 'VK_APPS', VK_APPS);
    AddConst('Windows', 'VK_NUMPAD0', VK_NUMPAD0);
    AddConst('Windows', 'VK_NUMPAD1', VK_NUMPAD1);
    AddConst('Windows', 'VK_NUMPAD2', VK_NUMPAD2);
    AddConst('Windows', 'VK_NUMPAD3', VK_NUMPAD3);
    AddConst('Windows', 'VK_NUMPAD4', VK_NUMPAD4);
    AddConst('Windows', 'VK_NUMPAD5', VK_NUMPAD5);
    AddConst('Windows', 'VK_NUMPAD6', VK_NUMPAD6);
    AddConst('Windows', 'VK_NUMPAD7', VK_NUMPAD7);
    AddConst('Windows', 'VK_NUMPAD8', VK_NUMPAD8);
    AddConst('Windows', 'VK_NUMPAD9', VK_NUMPAD9);
    AddConst('Windows', 'VK_MULTIPLY', VK_MULTIPLY);
    AddConst('Windows', 'VK_ADD', VK_ADD);
    AddConst('Windows', 'VK_SEPARATOR', VK_SEPARATOR);
    AddConst('Windows', 'VK_SUBTRACT', VK_SUBTRACT);
    AddConst('Windows', 'VK_DECIMAL', VK_DECIMAL);
    AddConst('Windows', 'VK_DIVIDE', VK_DIVIDE);
    AddConst('Windows', 'VK_F1', VK_F1);
    AddConst('Windows', 'VK_F2', VK_F2);
    AddConst('Windows', 'VK_F3', VK_F3);
    AddConst('Windows', 'VK_F4', VK_F4);
    AddConst('Windows', 'VK_F5', VK_F5);
    AddConst('Windows', 'VK_F6', VK_F6);
    AddConst('Windows', 'VK_F7', VK_F7);
    AddConst('Windows', 'VK_F8', VK_F8);
    AddConst('Windows', 'VK_F9', VK_F9);
    AddConst('Windows', 'VK_F10', VK_F10);
    AddConst('Windows', 'VK_F11', VK_F11);
    AddConst('Windows', 'VK_F12', VK_F12);
    AddConst('Windows', 'VK_F13', VK_F13);
    AddConst('Windows', 'VK_F14', VK_F14);
    AddConst('Windows', 'VK_F15', VK_F15);
    AddConst('Windows', 'VK_F16', VK_F16);
    AddConst('Windows', 'VK_F17', VK_F17);
    AddConst('Windows', 'VK_F18', VK_F18);
    AddConst('Windows', 'VK_F19', VK_F19);
    AddConst('Windows', 'VK_F20', VK_F20);
    AddConst('Windows', 'VK_F21', VK_F21);
    AddConst('Windows', 'VK_F22', VK_F22);
    AddConst('Windows', 'VK_F23', VK_F23);
    AddConst('Windows', 'VK_F24', VK_F24);
    AddConst('Windows', 'VK_NUMLOCK', VK_NUMLOCK);
    AddConst('Windows', 'VK_SCROLL', VK_SCROLL);
    AddConst('Windows', 'VK_LSHIFT', VK_LSHIFT);
    AddConst('Windows', 'VK_RSHIFT', VK_RSHIFT);
    AddConst('Windows', 'VK_LCONTROL', VK_LCONTROL);
    AddConst('Windows', 'VK_RCONTROL', VK_RCONTROL);
    AddConst('Windows', 'VK_LMENU', VK_LMENU);
    AddConst('Windows', 'VK_RMENU', VK_RMENU);
    AddConst('Windows', 'VK_PROCESSKEY', VK_PROCESSKEY);
    AddConst('Windows', 'VK_ATTN', VK_ATTN);
    AddConst('Windows', 'VK_CRSEL', VK_CRSEL);
    AddConst('Windows', 'VK_EXSEL', VK_EXSEL);
    AddConst('Windows', 'VK_EREOF', VK_EREOF);
    AddConst('Windows', 'VK_PLAY', VK_PLAY);
    AddConst('Windows', 'VK_ZOOM', VK_ZOOM);
    AddConst('Windows', 'VK_NONAME', VK_NONAME);
    AddConst('Windows', 'VK_PA1', VK_PA1);
    AddConst('Windows', 'VK_OEM_CLEAR', VK_OEM_CLEAR);
  end;    { with }
end;    { RegisterJvInterpreterAdapter }

end.
