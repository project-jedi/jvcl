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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus, JvTypes, JvComponent;

type
  TJvEasterEgg = class(TJvComponent)
  private
    FActive: Boolean;
    FOnEgg: TNotifyEvent;
    FShiftState: TShiftState;
    FEggs: string;
    FForm: TCustomForm;
    FOldWndProc: Pointer;
    FCurstring: string;
    procedure NewWndProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read FActive write FActive default True;
    property Egg: string read FEggs write FEggs;
    property ControlKeys: TShiftState read FShiftState write FShiftState default [ssAlt];
    property OnEggFound: TNotifyEvent read FOnEgg write FOnEgg;
  end;

implementation

{**************************************************}

constructor TJvEasterEgg.Create(AOwner: TComponent);
var
  ptr: Pointer;
begin
  inherited;
  FOldWndProc := nil;
  FForm := GetParentForm(TControl(AOwner));
  FActive := True;
  FShiftState := [ssAlt];

  FOldWndProc := Pointer(GetWindowLong(FForm.Handle, GWL_WNDPROC));
  ptr := {$IFDEF COMPILER6_UP}Classes.{$ENDIF}MakeObjectInstance(NewWndProc);
  SetWindowLong(FForm.Handle, GWL_WNDPROC, Longint(ptr));
end;

{**************************************************}

destructor TJvEasterEgg.Destroy;
begin
  if not (csDestroying in FForm.ComponentState) then
    SetWindowLong(FForm.Handle, GWL_WNDPROC, LongInt(FOldWndProc));
  inherited;
end;

{**************************************************}

procedure TJvEasterEgg.NewWndProc(var Msg: TMessage);
var
  shift: TShiftState;
begin
  with Msg do
  begin
    // (rom) simplified
    Result := CallWindowProc(FOldWndProc, FForm.Handle, Msg, WParam, LParam);
    if FActive and (FEggs <> '') then
      case Msg of
        WM_KEYUP, WM_SYSKEYUP:
          begin
            shift := KeyDataToShiftState(lParam);
            if shift = FShiftState then
            begin
              if ssShift in shift then
                FCurstring := FCurstring + UpCase(Char(wParam))
              else
                FCurstring := FCurstring + LowerCase(Char(wParam))[1];
              if FCurstring = FEggs then
              begin
                if Assigned(FOnEgg) then
                  FOnEgg(Self);
                FCurstring := '';
              end
              else if Length(FCurstring) >= Length(FEggs) then
                FCurstring := Copy(FCurstring, 2, Length(FEggs));
            end;
          end;
      end;
  end;
end;

end.
