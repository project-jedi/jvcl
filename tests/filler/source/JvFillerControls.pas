{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFillerControls.Pas, released on --.

The Initial Developers of the Original Code are Peter Thörnqvist and Remko Bonte
Portions created by these individuals are Copyright (C) 2002 - 2003 Peter Thörnqvist
and Remko Bonte.
All Rights Reserved.

Contributor(s):
  Marcel Bestebroer

Last Modified: 2003-04-24

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvFillerControls;

interface

uses
  Windows, Classes, StdCtrls, Controls, Graphics,
  JvFillIntf, JvFillBasicImpl, JvListBox, JvLabel;

type
  TJvFillListBox = class(TJvCustomListBox, IFillerNotify)
  private
    FFiller: IFiller;
    procedure FillerChanging(const AFiller: IFiller; AReason: TJvFillerChangeReason);
    procedure FillerChanged(const AFiller: IFiller; AReason: TJvFillerChangeReason);
    procedure SetFiller(const Value: iFiller);
    {$IFNDEF COMPILER6_UP}
    function GetFillerComp: TComponent;
    procedure SetFillerComp(Value: TComponent);
    {$ENDIF COMPILER6_UP}
  protected
    { Used to emulate owner draw, creates a list of empty items (low overhead for the items) and
      thus maintain the ability to use lbOwnerDrawVariable as drawing style. }
    function GetCount: Integer; {$IFNDEF COMPILER6_UP}virtual;{$ELSE}override;{$ENDIF COMPILER6_UP}
    procedure SetCount(Value: Integer); virtual;
    { Direct link to actual filler interface. This is done to aid in the implementation (less
      IFDEF's in the code; always refer to FillerIntf and it's working in all Delphi versions). }
    function FillerIntf: IFiller;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer);override;
    property Count: Integer read GetCount write SetCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);override;
  published
    {$IFDEF COMPILER6_UP}
    property Filler: IFiller read FFiller write setFiller;
    {$ELSE}
    property Filler: TComponent read GetFillerComp write SetFillerComp;
    {$ENDIF COMPILER6_UP}
    {$IFDEF COMPILER6_UP}
    property AutoComplete;
    {$ENDIF COMPILER6_UP}
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Columns;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property MultiSelect;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    {$IFDEF COMPILER6_UP}
    property ScrollWidth;
    {$ENDIF COMPILER6_UP}
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TJvFillLabel = class(TJvLabel, IFillerNotify)
  private
    FFiller: IFiller;
    FIndex: Integer;
    procedure SetFiller(const Value: IFiller);
    {$IFNDEF COMPILER6_UP}
    function GetFillerComp: TComponent;
    procedure SetFillerComp(Value: TComponent);
    {$ENDIF COMPILER6_UP}
    procedure SetIndex(const Value: Integer);
  protected
    { Direct link to actual filler interface. This is done to aid in the implementation (less
      IFDEF's in the code; always refer to FillerIntf and it's working in all Delphi versions). }
    function FillerIntf: IFiller;
    procedure FillerChanging(const AFiller: IFiller; AReason: TJvFillerChangeReason);
    procedure FillerChanged(const AFiller: IFiller; AReason: TJvFillerChangeReason);
    procedure UpdateCaption;
    function GetLabelCaption: string; override;
    procedure DoDrawText(var Rect: TRect; Flags: Word); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    {$IFDEF COMPILER6_UP}
    property Filler: IFiller read FFiller write setFiller;
    {$ELSE}
    property Filler: TComponent read GetFillerComp write SetFillerComp;
    {$ENDIF COMPILER6_UP}
    property Index: Integer read FIndex write SetIndex;

    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF COMPILER6_UP}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF COMPILER6_UP}
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  Math, SysUtils,
  JclStrings,
  JvTypes;

{ TJvFillListBox }

constructor TJvFillListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OwnerData := False;
end;

destructor TJvFillListBox.Destroy;
begin
  Filler := nil;
  inherited Destroy;
end;

function TJvFillListBox.GetCount: Integer;
begin
  Result := Items.Count;
end;

procedure TJvFillListBox.SetCount(Value: Integer);
begin
  Items.Text := StrRepeat(#13#10, Value);
end;

function TJvFillListBox.FillerIntf: IFiller;
begin
  Result := FFiller;
end;

procedure TJvFillListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  ItemsRenderer: IFillerItemsRenderer;
  Item: IFillerItem;
  ItemRenderer: IFillerItemRenderer;
  ItemText: IFillerItemText;
begin
  if (FillerIntf <> nil) and Supports(FillerIntf, IFillerItemsRenderer, ItemsRenderer) then
  begin
    Canvas.Font := Font;
    if odSelected in State then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.Font.Color  := clHighlightText;
    end
    else
      Canvas.Brush.Color := Color;
    ItemsRenderer.DrawItemByIndex(Canvas, Rect, Index, State);
  end
  else if FillerIntf <> nil then
  begin
    Item := (FillerIntf as IFillerItems).GetItem(Index);
    if Supports(Item, IFillerItemRenderer, ItemRenderer) then
    begin
      Canvas.Font := Font;
      if odSelected in State then
      begin
        Canvas.Brush.Color := clHighlight;
        Canvas.Font.Color  := clHighlightText;
      end
      else
        Canvas.Brush.Color := Color;
      ItemRenderer.Draw(Canvas, Rect, State);
    end
    else if Supports(Item, IFillerItemText, ItemText) then
    begin
      Canvas.Font := Font;
      if odSelected in State then
      begin
        Canvas.Brush.Color := clHighlight;
        Canvas.Font.Color  := clHighlightText;
      end
      else
        Canvas.Brush.Color := Color;
      Canvas.TextRect(Rect, Rect.Left, Rect.Top, ItemText.Caption);
    end
    else
      inherited DrawItem(Index, Rect, State);
  end
  else
    inherited DrawItem(Index, Rect, State);
end;

procedure TJvFillListBox.FillerChanged(const AFiller: IFiller; AReason: TJvFillerChangeReason);
begin
  case AReason of
    frDestroy: // Should never occur in this method.
      begin
        if HasParent then
          Count := 0;
        FFiller := nil;
      end;
    else
      begin
        if HasParent then
          Count := (FillerIntf as IFillerItems).Count;
      end;
  end;
  Invalidate;
end;

procedure TJvFillListBox.FillerChanging(const AFiller: IFiller; AReason: TJvFillerChangeReason);
begin
  case AReason of
    frDestroy:
      begin
        if HasParent then
          Count := 0;
        FFiller := nil;
        Invalidate;
      end;
  end;
end;

procedure TJvFillListBox.MeasureItem(Index: Integer; var Height: Integer);
var
  aSize: TSize;
  ItemsRenderer: IFillerItemsRenderer;
begin
  if (FillerIntf <> nil) and Supports(FillerIntf, IFillerItemsRenderer, ItemsRenderer) then
  begin
    aSize.cy := ItemHeight;
    aSize := ItemsRenderer.MeasureItemByIndex(Canvas, Index);
    if aSize.cy <> 0 then
      Height := aSize.cy;
  end
  else
    inherited MeasureItem(Index, Height);
end;

procedure TJvFillListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Style = lbStandard then
    Params.Style := Params.Style or LBS_OWNERDRAWFIXED;
end;

procedure TJvFillListBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  Invalidate;
end;

procedure TJvFillListBox.SetFiller(const Value: IFiller);
begin
  if FFiller <> Value then
  begin
    if FFiller <> nil then
    begin
      FFiller.UnregisterChangeNotify(self);
      if HasParent then
        Count := 0;
    end;
    FFiller := Value;
    if FFiller <> nil then
    begin
      FFiller.RegisterChangeNotify(self);
      if HasParent then
        Count := (FillerIntf as IFillerItems).Count;
    end;
    Invalidate;
  end;
end;

{$IFNDEF COMPILER6_UP}
function TJvFillListBox.GetFillerComp: TComponent;
var
  CompRef: IInterfaceComponentReference;
begin
  if FFiller = nil then
    Result := nil
  else
  begin
    if Succeeded(FFiller.QueryInterface(IInterfaceComponentReference, CompRef)) then
      Result := CompRef.GetComponent as TComponent
    else
      Result := nil;
  end;
end;

procedure TJvFillListBox.SetFillerComp(Value: TComponent);
var
  CompRef: IInterfaceComponentReference;
  FillerRef: IFiller;
begin
  if Value = nil then
    SetFiller(nil)
  else
  begin
    if Value.GetInterface(IInterfaceComponentReference, CompRef) then
    begin
      if Value.GetInterface(IFiller, FillerRef) then
        SetFiller(FillerRef)
      else
        raise EJVCLException.Create('Component does not support the IFiller interface.');
    end
    else
      raise EJVCLException.Create('Component does not support the IInterfaceComponentReference interface.');
  end;
end;
{$ENDIF COMPILER6_UP}

{ TJvFillLabel }

constructor TJvFillLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIndex := -1;
end;

destructor TJvFillLabel.Destroy;
begin
  Filler := nil;
  inherited Destroy;
end;

{$IFNDEF COMPILER6_UP}
function TJvFillLabel.GetFillerComp: TComponent;
var
  CompRef: IInterfaceComponentReference;
begin
  if FFiller = nil then
    Result := nil
  else
  begin
    if Succeeded(FFiller.QueryInterface(IInterfaceComponentReference, CompRef)) then
      Result := CompRef.GetComponent as TComponent
    else
      Result := nil;
  end;
end;

procedure TJvFillLabel.SetFillerComp(Value: TComponent);
var
  CompRef: IInterfaceComponentReference;
  FillerRef: IFiller;
begin
  if Value = nil then
    SetFiller(nil)
  else
  begin
    if Value.GetInterface(IInterfaceComponentReference, CompRef) then
    begin
      if Value.GetInterface(IFiller, FillerRef) then
        SetFiller(FillerRef)
      else
        raise EJVCLException.Create('Component does not support the IFiller interface.');
    end
    else
      raise EJVCLException.Create('Component does not support the IInterfaceComponentReference interface.');
  end;
end;
{$ENDIF COMPILER6_UP}

procedure TJvFillLabel.DoDrawText(var Rect: TRect; Flags: Word);
var
  Tmp: TSize;
  Renderer: IFillerItemsRenderer;
begin
  if (FillerIntf <> nil) and Supports(FillerIntf, IFillerItemsRenderer, Renderer) and
    (FIndex >= 0) and (FIndex < (FillerIntf as IFillerItems).Count) then
  begin
    Canvas.Brush.Color := Color;
    Canvas.Font := Font;
    if (Flags and DT_CALCRECT <> 0) then
    begin
      Tmp := Renderer.MeasureItemByIndex(Canvas, FIndex);
      Rect.Right := Tmp.cx;
      Rect.Bottom := Tmp.cy;
    end
    else
      Renderer.DrawItemByIndex(Canvas, Rect, FIndex, []);
  end
  else
    inherited DoDrawText(Rect, Flags);
end;

function TJvFillLabel.FillerIntf: IFiller;
begin
  Result := FFiller;
end;

procedure TJvFillLabel.FillerChanged(const AFiller: IFiller;
  AReason: TJvFillerChangeReason);
begin
  UpdateCaption;
end;

procedure TJvFillLabel.FillerChanging(const AFiller: IFiller;
  AReason: TJvFillerChangeReason);
begin
  case AReason of
    frDestroy:
      Filler := nil;
  end;
end;

function TJvFillLabel.GetLabelCaption: string;
begin
  if (FillerIntf <> nil) and (fsText in FillerIntf.GetSupports) and
      (FIndex >= 0) and (FIndex < (FillerIntf as IFillerItems).Count) then
    Result := ((FillerIntf as IFillerItems).Items[FIndex] as IFillerItemText).Caption
  else
    Result := inherited GetLabelCaption;
end;

procedure TJvFillLabel.SetFiller(const Value: IFiller);
begin
  if FFiller = Value then
    Exit;

  if Assigned(FFiller) then
  begin
    FFiller.UnregisterChangeNotify(Self);
    FIndex := -1;
  end;

  FFiller := Value;

  if Assigned(FFiller) then
    FFiller.RegisterChangeNotify(Self);
  UpdateCaption;
end;

procedure TJvFillLabel.SetIndex(const Value: Integer);
begin
  if Value <> FIndex then
  begin
    FIndex := Value;
    UpdateCaption;
  end;
end;

procedure TJvFillLabel.UpdateCaption;
var
  tmp: TSize;
  Renderer: IFillerItemsRenderer;
begin
  if AutoSize and (FillerIntf <> nil) and
    Supports(FillerIntf, IFillerItemsRenderer, Renderer) then
  begin
    tmp := Renderer.MeasureItemByIndex(Canvas, Index);
    if (tmp.cy <> 0)  then
      Height := tmp.cy;
    if tmp.cx <> 0 then
      Width := tmp.cx;
  end;
  Perform(CM_TEXTCHANGED, 0, 0);
end;

end.

