{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAlignListbox.PAS, released on 2000-11-22.

The Initial Developer of the Original Code is Peter Below <100113.1101@compuserve.com>
Portions created by Peter Below are Copyright (C) 2000 Peter Below.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2000-mm-dd

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvAlignListbox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, JVCLVer;

type
  TJvAlignListbox = class(TCustomListbox)
  private
    { Private declarations }
    fAlignment: TAlignment;
    FAboutJVCL: TJVCLAboutInfo;
    procedure SetAlignment(const Value: TAlignment);
  protected
    { Protected declarations }
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
  public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
  published
    { Published declarations }
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Alignment: TAlignment read fAlignment write SetAlignment
      default taLeftJustify;
    { publish all properties of TCustomlistbox with exception of Style }
    property Align;
    property Anchors;
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
    property Items;
    property MultiSelect;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    // property Style;
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

implementation

{ TJvAlignListbox }

constructor TJvAlignListbox.Create(aOwner: TComponent);
begin
  inherited;
//  Style := lbOwnerDrawFixed;
end;

procedure TJvAlignListbox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
const
  alignflags: array[TAlignment] of DWORD =
  (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  //  flags: DWORD;
  S: string;
begin
  if Assigned(OnDrawItem) then
    inherited
  else
  begin
    Canvas.FillRect(Rect);
    if index >= 0 then
    begin
      S := Items[index];
      if Length(S) > 0 then
        DrawText(Canvas.handle, PChar(S), Length(S), Rect,
          DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX or
          alignflags[FAlignment]);
    end;
  end;
end;

procedure TJvAlignListbox.SetAlignment(const Value: TAlignment);
begin
  if fAlignment <> Value then
  begin
    FAlignment := Value;
    if FAlignment <> taLeftJustify then
      Style := lbOwnerDrawFixed
    else
      Style := lbStandard;
    Invalidate;
  end;
end;

end.
