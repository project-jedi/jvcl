{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http:{www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvQDoubleBuffering.pas, released on 2004-09-19

The Initial Developer of the Original Code is André Snepvangers [ASnepvangers att users.sourceforge.net]
Portions created by André Snepvangers are Copyright (C) 2004 André Snepvangers.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http:{jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQDoubleBuffering;

interface

uses
  Classes, SysUtils,
  Qt, QWindows, QMessages, QControls, QForms;

type
  TJvDoubleBuffered = class(TJvEventFilter)
  private
    FDoubleBuffered: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; override;
    property DoubleBuffered: Boolean read FDoubleBuffered write FDoubleBuffered default true;
  end;

implementation

type
  THackedWidgetControl = class(TWidgetControl);

constructor TJvDoubleBuffered.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDoubleBuffered := true;
end;

function TJvDoubleBuffered.EventFilter(Sender: QObjectH; Event: QEventH): Boolean;
var
  Instance: TObject;
  PixMap: QPixmapH;
  R: TRect;
  PaintEvent: QPaintEventH;
begin
  Result := false;

  if (QEvent_type(Event) <> QEventType_Paint) then
    exit;
  // only widgetcontrols receive a paintevent
  Instance := TWidgetControl(FindObject(Sender));
  with Instance as TWidgetControl do
  begin

    if (csDestroying in ComponentState) or
       ([csCreating, csRecreating]* Instance.ControlState <> []) then
    begin
      // (Re)Post the event in case of csCreating, csRecreating ?
      Result := True;
      exit;
    end;

    if csWidgetPainting in ControlState then
    begin
      Result := SendMessage(Handle, WM_ERASEBKGND, 0, 0) <> 0;
      exit;
    end;

    if (csPaintCopy in ControlState) or not FDoubleBuffered then
      exit
    else
    begin
      QRegion_boundingRect(QPaintEvent_region(QPaintEventH(Event)), @R);
      Pixmap := QPixmap_create ;
      try
        ControlState := ControlState + [csPaintCopy];
        QPixmap_grabWidget(PixMap, Handle, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
        Qt.BitBlt(THackedWidgetControl(Instance).GetPaintDevice, R.Left, R.Top, PixMap, 0, 0, R.Right - R.Left, R.Bottom - R.Top, RasterOp_CopyROP, False);
        Result := True;
      finally
        ControlState := ControlState - [csPaintCopy];
        QPixMap_destroy(PixMap);
      end;
    end;
  end;
end;

end.
