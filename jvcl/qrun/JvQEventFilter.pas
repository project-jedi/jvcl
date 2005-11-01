{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http:{www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMessageControl.pas, released on 2004-10-10

The Initial Developer of the Original Code is André Snepvangers [ASnepvangers att users.sourceforge.net]
Portions created by André Snepvangers are Copyright (C) 2004 André Snepvangers.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http:{jvcl.sourceforge.net

Known Issues:
  It is still possible to move the component in IDE outside the parent.
  It could also be called as a feature. Object Treeview shows the
  correct parent.
-----------------------------------------------------------------------------}
// $Id$

unit JvQEventFilter;

interface

uses
  SysUtils, Classes, Qt, QWindows, QMessages, QControls, QForms,
  JvQControlComponent;

type
  TJvEventFilter = class(TJvCustomControlComponent)
  private
    FOnEvent: TEventEvent;
  protected
    FHook: QObject_hookH;
    procedure SetParent(const Value: TWinControl); override;
    function EventFilter(Receiver: QObjectH; Event: QEventH): Boolean; cdecl;
  public
    destructor Destroy; override;
  published
    property Active;
    property Parent;
    property OnEvent: TEventEvent read FOnEvent write FOnEvent;
  end;

  TJvAppEventFilter = class(TComponent)
  private
    FHook: QApplication_hookH;
  protected
    function EventFilter(Receiver: QObjectH; Event: QEventH): Boolean; virtual; cdecl; abstract; //cdecl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

procedure TJvEventFilter.SetParent(const Value: TWinControl);
var
  WasActive: Boolean;
  Method: TMethod;
begin
  if Value <> Parent then
  begin
    WasActive := Active;
    Active := false;
    if Assigned(FHook) then
      QObject_hook_destroy(FHook);
    FHook := nil;
    inherited SetParent(Value);
    if Assigned(Parent) then
    begin
      FHook := QObject_hook_create(Parent.Handle);
      TEventFilterMethod(Method) := EventFilter;
      Qt_hook_hook_events(FHook, Method);
    end;
    Active := WasActive;
  end;
end;

destructor TJvEventFilter.Destroy;
begin
  if Assigned(FHook) then
    QObject_hook_destroy(FHook);
  inherited Destroy;
end;

function TJvEventFilter.EventFilter(Receiver: QObjectH; Event: QEventH): Boolean;
begin
  Result := False;
  if Assigned(FOnEvent) and Active then
    FOnEvent(Receiver, Event, Result);
end;

constructor TJvAppEventFilter.Create(AOwner: TComponent);
var
  Method: TMethod;
begin
  inherited Create(AOwner);
  FHook := QApplication_hook_create(Application.Handle);
  TEventFilterMethod(Method) := EventFilter;
  Qt_hook_hook_events(FHook, Method);
end;

destructor TJvAppEventFilter.Destroy;
begin
  if Assigned(FHook) then
  begin
    QApplication_hook_destroy(FHook);
  end;
  inherited Destroy;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\qrun'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
