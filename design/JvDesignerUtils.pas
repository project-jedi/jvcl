{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvaDsgn.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s): 

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : routines for design-time

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDesignerUtils;

{$I jvcl.inc}

interface

uses
  {$IFDEF VCL}
  Windows, Graphics,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Types, QGraphics,
  {$ENDIF VisualCLX}
  SysUtils, Classes,
  {$IFDEF COMPILER6_UP}
  DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvDsgnIntf, JvDsgnTypes;

implementation

procedure DrawDesignFrame(Canvas: TCanvas; Rect: TRect);
begin
  if Integer(Canvas.Handle) <> 0 then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := clGray;
    Canvas.Pen.Style := psDot;
    Canvas.Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
  end;
end;

type
  TMyComponent = class(TComponent);

procedure GetDesigner(ASelf: TPersistent; out Result: IDesignerNotify);
var
  Temp: TPersistent;
begin
  Result := nil;
  if ASelf = nil then
    Exit;
  Temp := TMyComponent(ASelf).GetOwner;
  if Temp = nil then
  begin
    if (ASelf is TComponent) and (csDesigning in TComponent(ASelf).ComponentState) then
      TMyComponent(ASelf).QueryInterface(IDesignerNotify, Result);
  end
  else
  begin
    if (ASelf is TComponent) and not (csDesigning in TComponent(ASelf).ComponentState) then
      Exit;
    GetDesigner(Temp, Result);
  end;
end;

procedure DesignerNotify(ASelf, Item: TComponent; Operation: TOperation);
var
  Designer: IDesignerNotify;
begin
  if csDesigning in ASelf.ComponentState then
  begin
    GetDesigner(ASelf, Designer);
    if Designer <> nil then
      Designer.Notification(Item, Operation);
  end;
end;

procedure DesignerModified(ASelf: TComponent);
var
  Designer: IDesignerNotify;
begin
  if csDesigning in ASelf.ComponentState then
  begin
    GetDesigner(ASelf, Designer);
    if Designer <> nil then
      Designer.Modified;
  end;
end;

procedure DesignerSelectComponent(ASelf: TComponent);
var
  Designer: IDesignerNotify;
  Designer1: IJvFormDesigner;
begin
  if csDesigning in ASelf.ComponentState then
  begin
    GetDesigner(ASelf, Designer);
    if Designer <> nil then
    begin
      Designer.QueryInterface(IJvFormDesigner, Designer1);
      if Designer1 <> nil then
        Designer1.SelectComponent(ASelf);
    end;
  end;
end;

initialization
  DrawDesignFrameProc := DrawDesignFrame;
  DesignerNotifyProc := DesignerNotify;
  DesignerModifiedProc := DesignerModified;
  DesignerSelectComponentProc := DesignerSelectComponent;

end.
