{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvEasterEgg.PAS, released on 2001-02-28.

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

unit JvEasterEgg;

interface

uses
  Messages, SysUtils, Classes, Controls, Forms, Windows,
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
    function NewWndProc(var Msg: TMessage): Boolean;
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

uses
  JvWndProcHook;

// (rom) added by me. Should go to JCL

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
    RegisterWndProcHook(FForm, NewWndProc, hoAfterMsg);
end;

destructor TJvEasterEgg.Destroy;
begin
  if (FForm <> nil) and not (csDesigning in ComponentState) then
    UnregisterWndProcHook(FForm, NewWndProc, hoAfterMsg);
  inherited Destroy;
end;

function TJvEasterEgg.NewWndProc(var Msg: TMessage): Boolean;
var
  Shift: TShiftState;
  KeyState: TKeyBoardState;
begin
  Result := False;
  with Msg do
  begin
    // (rom) simplified
//    Result := CallWindowProc(FOldWndProc, FForm.Handle, Msg, WParam, LParam);
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

end.

