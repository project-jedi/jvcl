{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvLabel.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].
Peter Thornqvist [peter3@peter3.com]

Last Modified: 2003-08-17

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Changes:
2003-08-17:
  * Implementation moved to TJvCustomLabel. TJvLabel now only publishes properties and events.
2003-03-24:
  * JvHotLink merged into JvLabel:
    To simulate JvHotlink, set AutoOpenURL to true, modify HotTrackFont to fit and assign
    a URL (or file-path) to the URL property.
  * JvAngleLabel merged into JvLabel: set Angle > 0 and font to a TrueTrype font to rotate the text // peter3
Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvLabel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  JVCLVer, JvxCtrls;

type
  TJvLabel = class(TJvCustomLabel)
  published
    property Align;
    property Alignment;
    property AutoSize;
    property Caption;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property Layout;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShadowColor;
    property ShadowSize;
    property ShadowPos;
    property ShowAccelChar;
    property ShowFocus;
    property ShowHint;
    property Transparent;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDrag;
    property OnContextPopup;
    property OnEndDock;
    property OnStartDock;

    property AboutJVCL;
    property Angle;
    property AutoOpenURL;
    property HintColor;
    property HotTrack;
    property HotTrackFont;
    property Images;
    property ImageIndex;
    property Spacing;
    property URL;
    property OnCtl3DChanged;
    property OnParentColorChange;
  end;

implementation


end.

