{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDsgnIntf.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s): 

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : interface to design-time routines

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvDsgnIntf;

interface

uses
{$IFDEF COMPLIB_VCL}
  Windows, SysUtils, Classes, Graphics;
{$ENDIF COMPLIB_VCL}
{$IFDEF COMPLIB_CLX}
  SysUtils, Types, Classes, QGraphics;
{$ENDIF COMPLIB_CLX}

  { DrawDesignFrame draws frame on the rect, Rect.
    JVCL uses this function to drawing frame around controls at design-time }

  procedure DrawDesignFrame(Canvas : TCanvas; Rect : TRect);


  procedure DesignerNotify(Self, Item: TComponent; Operation: TOperation);
  procedure DesignerModified(Self : TComponent);
  procedure DesignerSelectComponent(Self : TComponent);


var
  DrawDesignFrameProc: procedure (Canvas : TCanvas; Rect : TRect);
  DesignerNotifyProc: procedure (Self, Item: TComponent; Operation: TOperation);
  DesignerModifiedProc: procedure (Self : TComponent);
  DesignerSelectComponentProc: procedure (Self : TComponent);

type
  TGetProjectNameProc = function: string;
  
var
  GetProjectNameProc: TGetProjectNameProc = nil;

implementation

procedure DrawDesignFrame(Canvas : TCanvas; Rect : TRect);
begin
  if Assigned(DrawDesignFrameProc) then
    DrawDesignFrameProc(Canvas, Rect);
end;

procedure DesignerNotify(Self, Item: TComponent; Operation: TOperation);
begin
  if Assigned(DesignerNotifyProc) then
    DesignerNotifyProc(Self, Item, Operation);
end;

procedure DesignerModified(Self : TComponent);
begin
  if Assigned(DesignerModifiedProc) then
    DesignerModifiedProc(Self);
end;

procedure DesignerSelectComponent(Self : TComponent);
begin
  if Assigned(DesignerSelectComponentProc) then
    DesignerSelectComponentProc(Self);
end;

end.
