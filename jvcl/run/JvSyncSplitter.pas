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

{$I jvcl.inc}

unit JvSyncSplitter;

interface

uses
  Messages, SysUtils, Classes, Controls, ExtCtrls,
  JvSplitter;

type
  TJvSyncSplitter = class(TJvSplitter)
  private
    FPartner: TJvSyncSplitter;
    FForcedSize: Boolean;
    procedure SetPartner(const Value: TJvSyncSplitter);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WndProc(var Msg: TMessage); override;
  published
    property Partner: TJvSyncSplitter read FPartner write SetPartner;
  end;

implementation

uses
  JvTypes, JvResources;

procedure TJvSyncSplitter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = Partner) then
    Partner := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TJvSyncSplitter.SetPartner(const Value: TJvSyncSplitter);
begin
  if Value <> Self then
    FPartner := Value
  else
    raise EJVCLException.Create(RsEInvalidPartner);
end;

procedure TJvSyncSplitter.WndProc(var Msg: TMessage);
begin
  if Assigned(FPartner) and not FForcedSize then
    case Msg.Msg of
      WM_MOUSEFIRST..WM_MOUSELAST:
        begin
          Partner.FForcedSize := True;
          try
            Partner.Perform(Msg.Msg, Msg.WParam, Msg.LParam);
          finally
            Partner.FForcedSize := False;
          end;
        end;
    end;
  inherited WndProc(Msg);
end;

end.

