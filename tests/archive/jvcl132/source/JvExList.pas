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

unit JvExList;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, JVCLVer;

type
  TScrollNotification = procedure(sender: TObject; const Msg: TWMScroll; var dontScroll: Boolean) of object;

  TJvExListbox = class(TCustomListbox)
  private
    { Private declarations }
    FOnVScroll, FOnHScroll: TScrollNotification;
    FAboutJVCL: TJVCLAboutInfo;
  protected
    { Protected declarations }
    procedure WMVScroll(var msg: TWMScroll); message WM_VSCROLL;
    procedure WMHScroll(var msg: TWMScroll); message WM_HSCROLL;
  public
    { Public declarations }
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;

    property Align;
    property BorderStyle;
    property Color;
    property Columns;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property IntegralHeight;
    property ItemHeight;
    property Items;
    property MultiSelect;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
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
    property OnStartDrag;

    property OnVScroll: TScrollNotification read FOnVScroll write FOnVScroll;
    property OnHScroll: TScrollNotification read FOnHScroll write FOnHScroll;

  end;

implementation

procedure TJvExListbox.WMVScroll(var msg: TWMScroll);
var
  dontScroll: Boolean;
begin
  if Assigned(FOnVScroll) then
  begin
    dontScroll := False;
    FOnVScroll(self, msg, dontScroll);
    if dontScroll then
      Exit;
  end;
  inherited;
end;

procedure TJvExListbox.WMHScroll(var msg: TWMScroll);
var
  dontScroll: Boolean;
begin
  if Assigned(FOnHScroll) then
  begin
    dontScroll := False;
    FOnHScroll(self, msg, dontScroll);
    if dontScroll then
      Exit;
  end;
  inherited;
end;

end.
