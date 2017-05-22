{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvOwnerDrawViewer.PAS, released on 2003-12-01.

The Initial Developer of the Original Code is: Peter Th�rnqvist
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvOwnerDrawViewer;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes, Graphics,
  JvCustomItemViewer;

type
  TJvOwnerDrawViewerOptions = class(TJvCustomItemViewerOptions)
  published
    property Alignment;
    property AutoCenter;
    property BrushPattern;
    property DragAutoScroll;
    property Height;
    property HorzSpacing;
    property HotTrack;
    property Layout;
    property LazyRead;
    property MultiSelect;
    property RightClickSelect;
    property ScrollBar;
    property ShowCaptions;
    property Smooth;
    property Tracking;
    property VertSpacing;
    property Width;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvOwnerDrawViewer = class(TJvCustomItemViewer)
  private
    function GetOptions: TJvOwnerDrawViewerOptions;
    procedure SetOptions(const Value: TJvOwnerDrawViewerOptions);
  protected
    function GetOptionsClass: TJvItemViewerOptionsClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Count;
    property Items;
  published
    property Options: TJvOwnerDrawViewerOptions read GetOptions write SetOptions;
    property SelectedIndex;
    property OnDrawItem;
    property OnOptionsChanged;
    property OnItemChanging;
    property OnItemChanged;
    property OnItemHint;

    property Align;
    property Anchors;
    //    property BiDiMode;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    //    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDockDrop;
    property OnDockOver;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation


constructor TJvOwnerDrawViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clWindow;
end;

function TJvOwnerDrawViewer.GetOptions: TJvOwnerDrawViewerOptions;
begin
  Result := TJvOwnerDrawViewerOptions(inherited Options);
end;

function TJvOwnerDrawViewer.GetOptionsClass: TJvItemViewerOptionsClass;
begin
  Result := TJvOwnerDrawViewerOptions;
end;

procedure TJvOwnerDrawViewer.SetOptions(const Value: TJvOwnerDrawViewerOptions);
begin
  inherited Options := Value;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
