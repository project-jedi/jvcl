{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvEasterEgg.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvEasterEgg;

interface

uses
  Windows,
  {$IFDEF VCL}
  Messages,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Qt, Types,
  {$ENDIF VisualCLX}
  SysUtils, Classes, Controls, Forms,
  JvComponent;

type
  TJvEasterEgg = class(TJvComponent)
  private
    FActive: Boolean;
    FOnEggFound: TNotifyEvent;
    FControlKeys: TShiftState;
    FEgg: string;
    FForm: TCustomForm;
    FCurString: string;
    {$IFDEF VCL}
    function NewWndProc(var Msg: TMessage): Boolean;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    function NewEventFilter(Sender: QObjectH; Event: QEventH): Boolean;
    {$ENDIF VisualCLX}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read FActive write FActive default True;
    property Egg: string read FEgg write FEgg;
    property ControlKeys: TShiftState read FControlKeys write FControlKeys default [ssAlt];
    property OnEggFound: TNotifyEvent read FOnEggFound write FOnEggFound;
  end;

implementation

{$IFDEF VCL}
uses
  JvWndProcHook;
{$ENDIF VCL}

function DownCase(Ch: Char): Char;
begin
  Result := Ch;
  case Result of
    'A'..'Z':
      Inc(Result, Ord('a') - Ord('A'));
  end;
end;

constructor TJvEasterEgg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := True;
  FControlKeys := [ssAlt];
  FForm := GetParentForm(TControl(AOwner));
  if (FForm <> nil) and not (csDesigning in ComponentState) then
    {$IFDEF VCL}
    RegisterWndProcHook(FForm, NewWndProc, hoAfterMsg);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    InstallApplicationHook(NewEventFilter);
    {$ENDIF VisualCLX}
end;

destructor TJvEasterEgg.Destroy;
begin
  if (FForm <> nil) and not (csDesigning in ComponentState) then
    {$IFDEF VCL}
    UnregisterWndProcHook(FForm, NewWndProc, hoAfterMsg);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    UninstallApplicationHook(NewEventFilter);
    {$ENDIF VisualCLX}
  inherited Destroy;
end;

{$IFDEF VCL}
function TJvEasterEgg.NewWndProc(var Msg: TMessage): Boolean;
var
  Shift: TShiftState;
  KeyState: TKeyBoardState;
begin
  Result := False;
  with Msg do
  begin
    if FActive and (FEgg <> '') then
      case Msg of
        WM_KEYUP, WM_SYSKEYUP:
          begin
            GetKeyboardState(KeyState);
            Shift := KeyboardStateToShiftState(KeyState);
            if Shift = FControlKeys then
            begin
              if ssShift in Shift then
                FCurString := FCurString + UpCase(Char(WParam))
              else
                FCurString := FCurString + DownCase(Char(WParam));
              if FCurString = FEgg then
              begin
                if Assigned(FOnEggFound) then
                  FOnEggFound(Self);
                FCurString := '';
              end
              else
              if Length(FCurString) >= Length(FEgg) then
                FCurString := Copy(FCurString, 2, Length(FEgg));
            end;
          end;
      end;
  end;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
function TJvEasterEgg.NewEventFilter(Sender: QObjectH; Event: QEventH): Boolean;
var
  Shift: TShiftState;
  KeyCode: Word;
  KeyChar: Char;
begin
  Result := False;
  if Active and (FEgg <> '') and (QEvent_type(Event) = QEventType_KeyRelease) then
  begin
    KeyCode := QKeyEvent_key(QKeyEventH(Event));
    KeyChar := Char(QKeyEvent_ascii(QKeyEventH(Event)));
    Shift := ButtonStateToShiftState(QKeyEvent_state(QKeyEventH(Event)));
    if (KeyCode = Key_Shift) then
      Include(Shift, ssShift);
    if (KeyCode = Key_Control) then
      Include(Shift, ssCtrl);
    if (KeyCode = Key_Alt) then
      Include(Shift, ssAlt);

    if Shift = FControlKeys then
    begin
      if ssShift in Shift then
        FCurString := FCurString + UpCase(KeyChar)
      else
        FCurString := FCurString + DownCase(KeyChar);
      if FCurString = Egg then
      begin
        if Assigned(FOnEggFound) then
          FOnEggFound(Self);
        FCurString := '';
      end
      else
      if Length(FCurString) >= Length(Egg) then
        FCurString := Copy(FCurString, 2, Length(Egg));
    end;
  end;
end;
{$ENDIF VisualCLX}

end.

