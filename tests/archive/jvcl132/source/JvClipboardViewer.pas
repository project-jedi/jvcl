{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvClipboardViewer.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvClipboardViewer;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ClipBrd, JvTypes, JvComponent;

type
  TJvClipboardViewer = class(TJvComponent)
  private
    FHandle: THandle;
    FNextCB: HWND;
    FOnText: TOnText;
    FOnImage: TOnImage;
    procedure UpdateClip(var Msg: TWMDrawClipBoard); message WM_DRAWCLIPBOARD;
    procedure WndProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    procedure EmptyClipboard;
    property OnImage: TOnImage read FOnImage write FOnImage;
    property OnText: TOnText read FOnText write FOnText;
  end;

implementation

{**************************************************}

constructor TJvClipboardViewer.Create(AOwner: TComponent);
begin
  inherited;
  FHandle :=  {$IFDEF Delphi6_Up}Classes.{$ENDIF}AllocateHWND(WndProc);
  FNextCB := SetClipboardViewer(FHandle);
  // (rom) removed a SetClipboardViewer line her
end;

{**************************************************}

destructor TJvClipboardViewer.Destroy;
begin
   {$IFDEF Delphi6_Up}Classes. {$ENDIF}DeallocateHWnd(FHandle);
  ChangeClipboardChain(FHandle, FNextCB);
  inherited;
end;

{**************************************************}

procedure TJvClipboardViewer.EmptyClipboard;
begin
  OpenClipboard(Application.Handle);
  // (rom) added Windows. to avoid recursion
  Windows.EmptyClipboard;
  CloseClipboard;
end;

{**************************************************}

procedure TJvClipboardViewer.UpdateClip(var Msg: TWMDrawClipBoard);
var
  Bitmap: TBitmap;
begin
  inherited;
  if Clipboard.HasFormat(CF_BITMAP) then
  begin
    Bitmap := nil;
    try
      Bitmap := TBitmap.Create;
      Bitmap.Assign(Clipboard);
      if Assigned(FOnImage) then
        FOnImage(Self, Bitmap);
    finally
      Bitmap.Free;
    end;
  end
  else if Assigned(FOnText) then
    FOnText(Self, ClipBoard.AsText);
  Msg.Result := 0;
end;

{**************************************************}

procedure TJvClipboardViewer.WndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_DRAWCLIPBOARD then
    UpdateClip(TWMDrawClipBoard(Msg))
  else
    Msg.Result := DefWindowProc(FHandle, Msg.Msg, Msg.wParam, Msg.lParam);
end;

end.
