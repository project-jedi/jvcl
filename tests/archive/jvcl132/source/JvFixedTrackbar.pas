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

unit JvFixedTrackbar;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls;

// *** Important to include this constant showing your version #
const
  TJvComponent_VERSION = '5.01';

type
  TJvFixedTrackbar = class(TTrackbar)
  private
    { Private declarations }
    procedure CNHScroll(var Message: TWMHScroll); message CN_HSCROLL;
    procedure CNVScroll(var Message: TWMVScroll); message CN_VSCROLL;
    function GetAboutMe: string;
    procedure SetAboutMe(const Value: string);
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
    property AboutMe: string read GetAboutMe write SetAboutMe stored False;
  end;

implementation

{ TJvFixedTrackbar }

procedure TJvFixedTrackbar.CNHScroll(var Message: TWMHScroll);
begin
  if Message.ScrollCode <> SB_ENDSCROLL then
    inherited;
end;

procedure TJvFixedTrackbar.CNVScroll(var Message: TWMVScroll);
begin
  if Message.ScrollCode <> SB_ENDSCROLL then
    inherited;
end;

function TJvFixedTrackbar.GetAboutMe: string;
begin
  Result := 'Version: ' + TJvComponent_VERSION;
end;

procedure TJvFixedTrackbar.SetAboutMe(const Value: string);
begin

end;

end.
