{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDsgnIntf.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : interface to design-time routines

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQDsgnIntf;

{$I jvcl.inc}

interface

uses
  QWindows, Classes, QGraphics;

{ DrawDesignFrame draws frame on the rect, Rect.
  JVCL uses this function to drawing frame around controls at design-time }

procedure DrawDesignFrame(Canvas: TCanvas; Rect: TRect);

procedure DesignerNotify(ASelf, Item: TComponent; Operation: TOperation);
procedure DesignerModified(ASelf: TComponent);
procedure DesignerSelectComponent(ASelf: TComponent);

var
  DrawDesignFrameProc: procedure(Canvas: TCanvas; Rect: TRect);
  DesignerNotifyProc: procedure(ASelf, Item: TComponent; Operation: TOperation);
  DesignerModifiedProc: procedure(ASelf: TComponent);
  DesignerSelectComponentProc: procedure(ASelf: TComponent);

type
  TGetProjectNameProc = function: string;

var
  GetProjectNameProc: TGetProjectNameProc = nil;

implementation

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

procedure DrawDesignFrame(Canvas: TCanvas; Rect: TRect);
begin
  if Assigned(DrawDesignFrameProc) then
    DrawDesignFrameProc(Canvas, Rect);
end;

procedure DesignerNotify(ASelf, Item: TComponent; Operation: TOperation);
begin
  if Assigned(DesignerNotifyProc) then
    DesignerNotifyProc(ASelf, Item, Operation);
end;

procedure DesignerModified(ASelf: TComponent);
begin
  if Assigned(DesignerModifiedProc) then
    DesignerModifiedProc(ASelf);
end;

procedure DesignerSelectComponent(ASelf: TComponent);
begin
  if Assigned(DesignerSelectComponentProc) then
    DesignerSelectComponentProc(ASelf);
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

