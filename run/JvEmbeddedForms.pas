{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvEmbeddedForms.PAS, released on 2004-06-11.

The Initial Developer of the Original Code is "rossen".
Portions created by "rossen" are Copyright (C) 2004 "rossen".
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvEmbeddedForms;

interface

uses
  Windows, Forms, Messages, Classes,
  SysUtils, ExtCtrls, Graphics, Controls,
  JvComponent;

type
  TJvEmbeddedFormLink = class(TJvComponent)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TJvEmbeddedPaintProcedure = procedure of object;

  TJvEmbeddedFormPanel = class(TJvCustomControl)
  private
    FLink: TJvEmbeddedFormLink;
    FLinkedForm: TForm;
    FAlwaysVisible: Boolean;
    FPaintProcedure: TJvEmbeddedPaintProcedure;

    procedure DrawFormImage;
    procedure SetLinkedForm;
    procedure UpdateLinkedForm;
    procedure DefaultPaint;
    procedure SetFormLink(const Value: TJvEmbeddedFormLink);
  protected
    procedure Paint; override;

    procedure InitLinkedForm; virtual;
    procedure ClearLinkedForm; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    procedure DockLinkedForm; virtual;
    procedure UndockLinkedForm(ABorderStyle: TFormBorderStyle; APosition: TPosition); virtual;
    function IsLinkedFormDocked:Boolean;
    property LinkedForm: TForm read FLinkedForm;

  published
    property AlwaysVisible: Boolean read FAlwaysVisible write FAlwaysVisible;

    property FormLink: TJvEmbeddedFormLink read FLink write SetFormLink;
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property Enabled;
    property ParentColor;
    property ParentShowHint;
    property ShowHint;
    property Visible;



    property OnClick;
    property OnDblClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property BorderWidth;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property DockManager;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property UseDockManager;
  end;

  TJvEmbeddedInstanceFormPanel = class(TJvEmbeddedFormPanel)
  private
    FFormClass: TFormClass;
    procedure CreateFormInstance;
  protected
    procedure InitLinkedForm; override;
    procedure ClearLinkedForm; override;
  end;

implementation

resourcestring
  RsEFormLinkSingleInstanceOnly = 'You only need one form link per form.';
  RsELinkCircularRef = 'Circular references not allowed.';

//=== { TJvEmbeddedFormLink  } =================================================

constructor TJvEmbeddedFormLink.Create(AOwner: TComponent);
var
  I: Integer;
begin
  if (AOwner <> nil) and (csDesigning in AOwner.ComponentState) then
    for I := 0 to AOwner.ComponentCount - 1 do
      if AOwner.Components[I] is TJvEmbeddedFormLink then
        raise Exception.Create(RsEFormLinkSingleInstanceOnly);
  inherited Create(AOwner);
end;

//=== { TJvEmbeddedFormPanel  } ================================================

procedure TJvEmbeddedFormPanel.Paint;
begin
//  inherited;
  if Assigned(FPaintProcedure) then
    FPaintProcedure;
end;

procedure TJvEmbeddedFormPanel.DrawFormImage;
var
  FBitmap: TBitmap;
  R: TRect;
begin
  FBitmap := FLinkedForm.GetFormImage;
  try
    DefaultPaint;
    Canvas.Draw(0, 0, FBitmap);
    if csDesigning in ComponentState then
    begin
      R := FLinkedForm.ClientRect;
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Style := psDot;
      Canvas.Rectangle(R);
      Canvas.Brush.Style := bsBDiagonal;
      Canvas.Rectangle(R);
    end;
  finally
    FBitmap.Free;
  end;
end;

procedure TJvEmbeddedFormPanel.SetLinkedForm;
begin
  with FLinkedForm do
  begin
    Parent := Self;
    Align := alClient;
    BorderStyle := bsNone;
    Show;
  end;
end;

procedure TJvEmbeddedFormPanel.UpdateLinkedForm;

  function IsOwnerFormActive: Boolean;
  var
    FParent: TWinControl;
  begin
    FParent := Parent;
    while (FParent <> nil) and (FParent.Parent <> nil) do
      FParent := FParent.Parent;
    Result := (FParent is TForm) and TForm(FParent).Active;
  end;
begin
  if (FLinkedForm.Parent <> Self) and (FLinkedForm.Parent <> nil) and IsOwnerFormActive then
    SetLinkedForm
  else if AlwaysVisible then
    DrawFormImage;
end;

procedure TJvEmbeddedFormPanel.InitLinkedForm;
begin
  FLinkedForm := FLink.Owner as TForm;
  if FLinkedForm <> nil then
  begin
    FLinkedForm.FreeNotification(Self);
//    ClientWidth := FLinkedForm.ClientWidth;
//    ClientHeight := FLinkedForm.ClientHeight;
//    Color := FLinkedForm.Color;

    if csDesigning in ComponentState then
    begin
      DrawFormImage;
      FPaintProcedure := DrawFormImage;
    end
    else
    begin
      SetLinkedForm;
      FPaintProcedure := UpdateLinkedForm;
    end;
  end
  else
    FPaintProcedure := DefaultPaint;
  Invalidate;
end;

procedure TJvEmbeddedFormPanel.ClearLinkedForm;
begin
  FLink := nil;
  FLinkedForm := nil;
  FPaintProcedure := DefaultPaint;
  Invalidate;
end;

procedure TJvEmbeddedFormPanel.SetFormLink(const Value: TJvEmbeddedFormLink);
begin
  if Value <> FLink then
  begin
    if Value = nil then
      ClearLinkedForm
    else if Value.Owner = Owner then
      raise Exception.Create(RsELinkCircularRef)
    else
    begin
      FLink := Value;
      FLink.FreeNotification(Self);
      InitLinkedForm;
    end;
  end;
end;

procedure TJvEmbeddedFormPanel.DefaultPaint;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
end;

procedure TJvEmbeddedFormPanel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if AComponent = FLinkedForm then
      FLinkedForm := nil;
    if AComponent = FormLink then
      FormLink := nil;
  end;
end;

//=== { TJvEmbeddedInstanceFormPanel  } ========================================

procedure TJvEmbeddedInstanceFormPanel.CreateFormInstance;
begin
  FLinkedForm := FFormClass.Create(Self);
  FFormClass := nil;
  FPaintProcedure := DefaultPaint;
  SetLinkedForm;
end;

procedure TJvEmbeddedInstanceFormPanel.ClearLinkedForm;
begin
  if not (csDesigning in ComponentState) then
    FLinkedForm.Free;

  FFormClass := nil;
  inherited ClearLinkedForm;
end;

procedure TJvEmbeddedInstanceFormPanel.InitLinkedForm;
begin
  if csDesigning in ComponentState then
    inherited InitLinkedForm
  else
  begin
    FreeAndNil(FLinkedForm);
    FFormClass := nil;
    FPaintProcedure := DefaultPaint;
    FFormClass := TFormClass((FLink.Owner as TForm).ClassType);
    FPaintProcedure := CreateFormInstance;
  end;
end;

procedure TJvEmbeddedFormPanel.DockLinkedForm;
begin
  if (FLinkedForm <> nil) and (FLinkedForm.Parent <> Self) then
    with FLinkedForm do
    begin
      Hide;
      BorderStyle := bsNone;
      Parent := Self;
      Align := alClient;
      Show;
    end;
end;

procedure TJvEmbeddedFormPanel.UndockLinkedForm(ABorderStyle: TFormBorderStyle; APosition: TPosition);
var B:Boolean;
begin
  if (FLinkedForm <> nil) and (FLinkedForm.Parent = Self) then
    with FLinkedForm do
    begin
      B := AutoScroll;
      Hide;
      Align := alNone;
      Parent := nil;
    // IMPORTANT!!! Don't set BorderStyle unless Parent = nil!!!
      BorderStyle := ABorderStyle;
      Position := APosition;
      AutoScroll := B;
      Show;
    end;
end;

function TJvEmbeddedFormPanel.IsLinkedFormDocked: Boolean;
begin
  Result := (FLinkedForm <> nil) and (FLinkedForm.Parent = Self);
end;




end.

