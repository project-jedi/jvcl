{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: RAFDAlignPalette.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s): 

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : routines for design-time

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvaDsgn;

interface

uses
 {$IFDEF COMPLIB_VCL}
  Windows, Graphics, 
 {$ENDIF COMPLIB_VCL}
 {$IFDEF COMPLIB_CLX}
  Types, QGraphics,
 {$ENDIF COMPLIB_CLX}
  SysUtils, Classes,
 {$IFDEF COMPILER6_UP}
  DesignIntf
 {$ELSE}
  DsgnIntf
 {$ENDIF COMPILER6_UP},
 {$IFNDEF COMPILER4_UP}
  Forms,
 {$ENDIF}
  JvDsgnIntf;


implementation


procedure DrawDesignFrame(Canvas : TCanvas; Rect : TRect);
begin
  if Integer(Canvas.Handle) <> 0 then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := clGray;
    Canvas.Pen.Style := psDot;
    Canvas.Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
  end;
end;

{$IFDEF COMPILER4_UP}
type
  TMyComponent = class(TComponent);

procedure GetDesigner(Self: TPersistent; out Result: IDesignerNotify);
var
  Temp: TPersistent;
begin
  Result := nil;
  if Self = nil then Exit;
  Temp := TMyComponent(Self).GetOwner;
  if Temp = nil then
  begin
    if (Self is TComponent) and (csDesigning in TComponent(Self).ComponentState) then
      TMyComponent(Self).QueryInterface(IDesignerNotify, Result);
  end
  else
  begin
    if (Self is TComponent) and
      not (csDesigning in TComponent(Self).ComponentState) then Exit;
    GetDesigner(Temp, Result);
  end;
end;

procedure DesignerNotify(Self, Item: TComponent; Operation: TOperation);
var
  Designer: IDesignerNotify;
begin
  if csDesigning in Self.ComponentState then
  begin
    GetDesigner(Self, Designer);
    if Designer <> nil then
      Designer.Notification(Item, Operation);
  end;    
end;

procedure DesignerModified(Self : TComponent);
var
  Designer: IDesignerNotify;
begin
  if csDesigning in Self.ComponentState then
  begin
    GetDesigner(Self, Designer);
    if Designer <> nil then
      Designer.Modified;
  end;
end;

{$IFDEF COMPILER6_UP}
procedure DesignerSelectComponent(Self : TComponent);
var
  Designer: IDesignerNotify;
  Designer1: IDesigner;
begin
  if csDesigning in Self.ComponentState then
  begin
    GetDesigner(Self, Designer);
    if (Designer <> nil) then
    begin
      Designer.QueryInterface(IDesigner, Designer1);
      if Designer1 <> nil then
        Designer1.SelectComponent(Self);
    end;
  end;
end;

{$ELSE}
 
procedure DesignerSelectComponent(Self : TComponent);
var
  Designer: IDesignerNotify;
  Designer1: IFormDesigner;
begin
  if csDesigning in Self.ComponentState then
  begin
    GetDesigner(Self, Designer);
    if (Designer <> nil) then
    begin
      Designer.QueryInterface(IFormDesigner, Designer1);
      if Designer1 <> nil then
        Designer1.SelectComponent(Self);
    end;
  end;
end;
{$ENDIF COMPILER6_UP}

{$ELSE}

function GetDesigner(Self : TComponent) : TDesigner;
begin
  Result := nil;
  while (Self <> nil) and not (Self is TForm) and (Self.Owner <> nil) do
    Self := Self.Owner;
  if Self is TForm then
    Result := (Self as TForm).Designer;
end;

procedure DesignerNotify(Self, Item: TComponent; Operation: TOperation);
var
  Designer: TDesigner;
begin
  if csDesigning in Self.ComponentState then
  begin
    Designer := GetDesigner(Self);
    if Designer <> nil then
      Designer.Notification(Item, Operation);
  end;
end;

procedure DesignerModified(Self : TComponent);
var
  Designer : TDesigner;
begin
  if csDesigning in Self.ComponentState then
  begin
    Designer := GetDesigner(Self);
    if Designer <> nil then Designer.Modified;
  end;
end;

procedure DesignerSelectComponent(Self : TComponent);
var
  Designer : TDesigner;
begin
  if csDesigning in Self.ComponentState then
  begin
    Designer := GetDesigner(Self);
    if (Designer is TFormDesigner) then
      (Designer as TFormDesigner).SelectComponent(Self);
  end;
end;

{$ENDIF}


initialization
  DrawDesignFrameProc := DrawDesignFrame;
  DesignerNotifyProc := DesignerNotify;
  DesignerModifiedProc := DesignerModified;
  DesignerSelectComponentProc := DesignerSelectComponent;
end.
